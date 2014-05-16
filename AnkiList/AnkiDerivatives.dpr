program AnkiDerivatives;
{
Parses a file listing derivative links between words:
  base deriv deriv "comment" deriv "comment"

And produces a tab-separated file with comments about these relations between
entries, to be imported to Anki.

E.g.:
  a b c d "comment"
  a e
  b e
=>
  a: Derivatives: b, c, d (comment).
  b: From: a (comment); derivatives: e.
  c, d: From: a (comment).
  e: From: a (comment); b.

TODO: <ruby>expr<rt>read, expr[read] etc.
TODO: "~" etc.
TODO: XSLT support for expressions, like in WordList
TODO: Allow matches from up to N dictionaries (ASD -- edict; BSD -- warodai)
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, Generics.Collections, Generics.Defaults, ConsoleToolbox,
  JWBIO, UniStrUtils, FilenameUtils, Edict, AnkiEdictToText;

type
  TExpressionCard = record
    base: string;
    deriv: string;
    procedure Reset;
  end;

  TAnkiDerivatives = class(TCommandLineApp)
  protected
    Files: TFilenameArray;
    DictFiles: TFilenameArray;
    OutputFile: string;
    Output: TStreamEncoder;
    Dicts: TObjectList<TEdict>;
    Words: TDictionary<string, TExpressionCard>;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    function FormatEdictEntry(const expr: string; ed: PEdictEntry): string;
    function FormatEntry(const expr: string): string;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

procedure TAnkiDerivatives.ShowUsage;
begin
  writeln('Generates derivate cross references for an Anki tag from derivative '
    +'list files.');
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -d dictionary     specify dictionary file (multiple -d is allowed,'
    +' in the priority order)');
  writeln('  -o output.file    specify output file (otherwise console)');
end;

function TAnkiDerivatives.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-d' then begin
    if i>=ParamCount then BadUsage('-d requires dictionary file name');
    Inc(i);
    AddFile(DictFiles, ParamStr(i));
    Result := true;
  end else
    Result := inherited;
end;

function TAnkiDerivatives.HandleParam(const s: string; var i: integer): boolean;
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

procedure TAnkiDerivatives.Run;
var i: integer;
  dic: TEdict;
  pair: TPair<string, TExpressionCard>;
  ln: string;
begin
  if ParamCount<1 then BadUsage();
  if Length(Files)<1 then BadUsage('Input files not specified');

  Words := TDictionary<string, TExpressionCard>.Create(
    TDelegatedEqualityComparer<string>.Create(
      _compareString,
      _hashString
    )
  );

  DictFiles := ExpandFileMasks(DictFiles, true);
  Dicts := TObjectList<TEdict>.Create({OwnsObjects=}true);
  for i := 0 to Length(DictFiles)-1 do begin
    dic := TEdict.Create;
    dic.LoadFromFile(DictFiles[i]);
    Dicts.Add(dic);
  end;

  Files := ExpandFileMasks(Files, true);
  for i := 0 to Length(Files)-1 do
    ParseFile(Files[i]);

  if OutputFile<>'' then
    Output := UnicodeFileWriter(OutputFile)
  else
    Output := ConsoleWriter;
  Output.WriteBom;

  for pair in Words do begin
    Output.Write(pair.Key+#09);
    if pair.Value.base<>'' then
      ln := 'От '+pair.Value.base
    else
      ln := '';
    if pair.Value.deriv<>'' then begin
      if ln<>'' then
        ln := ln+'; производные: '
      else
        ln := 'Производные: ';
      ln := ln + pair.Value.deriv;
    end;
    if ln<>'' then
      ln := ln + '.';

    Output.WriteLn(ln);
  end;

  FreeAndNil(Output);
  FreeAndNil(Dicts);
  FreeAndNil(Words);
end;

{ Parses one line from a derivative definition file, minding quotes.
  Does not remove quotes from quoted parts so that you can determine what's
  what later. }
function SplitDerivLine(ln: string): TStringArray;
var beg_i, i: integer;
  flag_escape: boolean;
  quote_char: char;

  procedure Commit;
  var chunk: string;
  begin
    if beg_i>=i then exit;
    chunk := Trim(copy(ln, beg_i, i-beg_i));
    if chunk<>'' then begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := chunk;
    end;
    beg_i := i+1;
  end;

begin
  SetLength(Result, 0);
  if ln='' then exit;

  flag_escape := false;
  quote_char := #00;

  beg_i := 1;
  i := 1;
  while i<=Length(ln) do begin
    if flag_escape then
      flag_escape := false
    else
    if ln[i]='\' then
      flag_escape := true
    else
    if (quote_char='"') and (ln[i]='"') then
      quote_char := #00
    else
    if (quote_char=''') and (ln[i]=''') then
      quote_char := #00
    else
    if quote_char<>#00 then begin
     //Nothing
    end else
    if (ln[i]='"') or (ln[i]='''') then
      quote_char := ln[i]
    else
    if ln[i]=' ' then
      Commit;
   //else just a normal char
   //in any case:
    Inc(i);
  end;

  Commit;
end;

function IsComment(const part: string): boolean;
begin
  Result := (Length(part)>=2) and(
      ((part[1]='"') and (part[Length(part)]='"'))
   or ((part[1]='''') and (part[Length(part)]=''''))
  );
end;

function FormatComment(const part: string): string;
begin
  Result := '('+copy(part, 2, Length(part)-2)+')';
end;

function FindNextComment(const parts: TStringArray; idx: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := idx to Length(parts)-1 do
    if IsComment(parts[i]) then begin
      Result := i;
      break;
    end;
end;

function TAnkiDerivatives.FormatEdictEntry(const expr: string; ed: PEdictEntry): string;
var read: string;
  i: integer;
begin
 //We need reading, but not any reading but matching readings.
 //TODO: If reading is provided in expr, use that.
  read := '';
  if ed.GetKanaIndex(expr)<0 then //else no need for reading since it's kana already
    for i := 0 to Length(ed.kana)-1 do
      if ed.kana[i].MatchesKanji(expr) then begin
        if read<>'' then read := read + '、';
        read := read + ed.kana[i].kana;
      end;

  if read<>'' then
    Result := expr + ' [' + read + '] ' + EdictSensesToText(ed)
  else
    Result := expr + ' ' + EdictSensesToText(ed);
end;

function TAnkiDerivatives.FormatEntry(const expr: string): string;
var i: integer;
  ed: PEdictEntry;
begin
  Result := '';
  for i := 0 to Dicts.Count-1 do begin
    ed := Dicts[i].FindEntry(expr); //TODO: If reading is provided, mind it in lookup
    if ed<>nil then begin
      Result := FormatEdictEntry(expr, ed);
      break;
    end;
  end;

  if Result='' then
    Result := expr;
end;

procedure TAnkiDerivatives.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  ln: string;
  i: integer;
  parts: TStringArray;
  card: TExpressionCard;
  next_comment: integer;
begin
  inp := OpenTextFile(AFilename);
  try
    while inp.ReadLn(ln) do begin
      parts := SplitDerivLine(Trim(ln));
      if Length(parts)<=0 then continue;

     //Base word
      if not Words.TryGetValue(parts[0], card) then
        card.Reset;
      for i := 1 to Length(parts)-1 do begin
        if IsComment(parts[i]) then begin
          if card.deriv <> '' then
            if i=1 then
              card.deriv := card.deriv + '; '
            else
              card.deriv := card.deriv + ' ';
          card.deriv := card.deriv + FormatComment(parts[i]);
        end else begin
          if card.deriv <> '' then
            if i=1 then
              card.deriv := card.deriv + '; '
            else
              card.deriv := card.deriv + ', ';
          card.deriv := card.deriv + FormatEntry(parts[i]);
        end;
        card.deriv := card.deriv + ''
      end;
      Words.AddOrSetValue(parts[0], card);

     //Derivatives
      next_comment := FindNextComment(parts, 1);
      for i := 1 to Length(parts)-1 do begin
        if IsComment(parts[i]) then begin
          next_comment := FindNextComment(parts, i+1);
          continue;
        end;

        if not Words.TryGetValue(parts[i], card) then
          card.Reset;
        if card.base <> '' then
          card.base := card.base + '; ';
        card.base := card.base + FormatEntry(parts[0]);
        if next_comment>=0 then
          card.base := card.base + ' ' + FormatComment(parts[next_comment]);
        Words.AddOrSetValue(parts[i], card);
      end;

    end;

  finally
    FreeAndNil(inp);
  end;
end;


begin
  RunApp(TAnkiDerivatives);
end.
