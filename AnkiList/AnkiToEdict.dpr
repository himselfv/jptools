program AnkiToEdict;
{
Parses a tab-separated Anki export, strips HTML tags according to a set of rules
and converts to EDICT-compatible dictionary.
  base deriv deriv "comment" deriv "comment"

Note that while compatible, this does not give you the full extent of EDICT
features (grammar markers etc).

TODO: <ruby>expr<rt>read, expr[read] etc.
TODO: "~" etc.
TODO: <br /> without ; before it => remove <br>, insert ;
TODO: multiple <divs> => separate entries
TODO: ~kana in meaning => separate entries
TODO: "text; ~kana text" => separate entries
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, Generics.Collections, Generics.Defaults, ConsoleToolbox,
  JWBIO, StrUtils, UniStrUtils, FilenameUtils, EdictWriter, RegularExpressions;

type
  TExpressionCard = record
    base: string;
    deriv: string;
    procedure Reset;
  end;

  TAnkiToEdict = class(TCommandLineApp)
  protected
    Files: TFilenameArray;
    ExprColumn: integer;
    ReadColumn: integer;
    MeaningColumn: integer;
    ExprSep: char;
    ReadSep: char;
  protected
    stripHtml: TRegEx;
  protected
    OutputFile: string;
    Output: TStreamEncoder;
    Words: TDictionary<string, TExpressionCard>;
    edict1, edict2, jmdict: TArticleWriter;
    procedure Init; override;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

procedure TAnkiToEdict.ShowUsage;
begin
  writeln('Parses a tab-separated Anki export, strips HTML tags according to a '
    +'set of rules and converts to EDICT-compatible dictionary.');
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('');
  writeln('Input:');
  writeln('  -te <column>      zero-based. Take expressions from this column (default is 0)');
  writeln('  -tr <column>      Take readings from this column (default is 1, if none available set to -1)');
  writeln('  -tm <column>      Take meanings (content) from this column (default is 2, if none available set to -1)');
  writeln('  -es <text>        Expression separator. If specified, multiple ways of writing an expression can be listed.');
  writeln('  -rs <text>        Reading separator. By default expression separator is used.');
  writeln('');
  writeln('Output:');
  writeln('  -o output.file    specify output file (otherwise console, EDICT2 only)');
end;

procedure TAnkiToEdict.Init;
begin
  ExprColumn := 0;
  ReadColumn := 1;
  MeaningColumn := 2;
  ExprSep := '、';
  ReadSep := #00; //same as Expression
end;

function TAnkiToEdict.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-te' then begin
    if i>=ParamCount then BadUsage('-te needs column number');
    Inc(i);
    ExprColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-tr' then begin
    if i>=ParamCount then BadUsage('-tr needs column number');
    Inc(i);
    ReadColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-tm' then begin
    if i>=ParamCount then BadUsage('-tm needs column number');
    Inc(i);
    MeaningColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-es' then begin
    if i>=ParamCount then BadUsage('-es needs separator value');
    Inc(i);
    ExprSep := ParamStr(i)[1];
    Result := true
  end else
  if s='-er' then begin
    if i>=ParamCount then BadUsage('-er needs separator value');
    Inc(i);
    ReadSep := ParamStr(i)[1];
    Result := true
  end else

  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
    Result := inherited;
end;

function TAnkiToEdict.HandleParam(const s: string; var i: integer): boolean;
begin
  AddFile(Files, s);
  Result := true;
end;


procedure TExpressionCard.Reset;
begin
  Self.base := '';
  Self.deriv := '';
end;

function _compareString(const Left, Right: string): Boolean;
begin
  Result := CompareStr(Left, Right)=0;
end;

function _hashString(const Value: string): Integer;
begin
  if Value='' then
    Result := 0
  else
    Result := BobJenkinsHash(Value[1], Length(Value)*SizeOf(Value[1]), 1234567);
end;

procedure TAnkiToEdict.Run;
var i: integer;
begin
  if ParamCount<1 then BadUsage();
  if Length(Files)<1 then BadUsage('Input files not specified');
  if ReadSep=#00 then ReadSep := ExprSep;

  stripHtml := TRegex.Create('<[^>]*>');

  if OutputFile<>'' then begin
    edict1 := TEdict1Writer.Create(OutputFile+'.edict1');
    edict2 := TEdict2Writer.Create(OutputFile+'.edict2');
    jmdict := TJmDictWriter.Create(OutputFile+'.jmdict');
  end else begin
    edict1 := nil;
    edict2 := TEdict2Writer.Create(ConsoleWriter);
    jmdict := nil;
  end;

  Files := ExpandFileMasks(Files, true);
  for i := 0 to Length(Files)-1 do
    ParseFile(Files[i]);

  FreeAndNil(stripHtml);
  FreeAndNil(Output);
end;

procedure TAnkiToEdict.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  ln, expr, read, mean: string;
  p_expr, p_read: TStringArray;
  parts: TStringArray;
  p_mean: TStringArray;
  ed: TEdictArticle;
  sense: PEdictSenseEntry;
  i: integer;
begin
  inp := OpenTextFile(AFilename);
  try
    while inp.ReadLn(ln) do begin
      parts := StrSplit(PWideChar(Trim(ln)),#09);
      if Length(parts)<=0 then continue;

      parts := StrSplit(PWideChar(ln),#09);
      if ExprColumn>=Length(parts) then
        continue;
      expr := Trim(parts[ExprColumn]);
      if (ReadColumn<0) or (ReadColumn=ExprColumn) or (ReadColumn>=Length(parts)) then
        read := ''
      else
        read := Trim(parts[ReadColumn]);

      p_expr := StrSplit(PChar(expr), ExprSep); //OK if it's #00
      p_read := StrSplit(PChar(read), ReadSep); //OK if it's #00

      ed.Reset;
      for expr in p_expr do
        ed.AddKanji^.k := expr;
      for read in p_read do
        with ed.AddKana^ do begin
          k := read;
          AllKanji := true;
        end;

      if MeaningColumn>=Length(parts) then
        continue;
      mean := parts[MeaningColumn];
      mean := stripHtml.Replace(mean, '');

     //TODO: mind brackets, quotes
      p_mean := StrSplit(PChar(mean), ';');
      for i := 0 to Length(p_mean)-1 do begin
        sense := ed.AddSense;
        sense.AddGloss(Trim(p_mean[i]).Replace('/', '\')
          .Replace(', ', '/').Replace(',', '/'));
      end;

      if edict1<>nil then
        edict1.Print(@ed);
      if edict2<>nil then
        edict2.Print(@ed);
      if jmdict<>nil then
        jmdict.Print(@ed);
    end;

  finally
    FreeAndNil(inp);
  end;
end;


begin
  RunApp(TAnkiToEdict);
end.
