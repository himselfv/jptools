program AnkiWordList;
{$APPTYPE CONSOLE}
{
 Parses a word/expression list - one expression at a line, possibly
 tab-separated table with other unrelated fields.

 Builds a card list in a format suitable for importing to Anki:
   expression      translations

 Intended usage:
   1. Make a word list for studying either manually or by other means.
   2. Generate Anki cards with this tool.
   3. Import to Anki.

 To update existing deck with translations from another dictionary:
   1. Export the notes into tab-separated text file.
   2. Run this tool and generate another tab-separated file with translations.
   3. Create additional note field in Anki.
   4. Import new file, pasting translations into the new field.

 Requires any EDICT-compatible dictionary.
}
uses
  SysUtils, Classes, StrUtils, UniStrUtils, ConsoleToolbox, JWBIO,
  JWBEdictReader, JWBEdict;

type
  TAnkiWordList = class(TCommandLineApp)
  protected
    Files: array of string;
    EdictFile: string;
    Edict: TEdict;
    OutputFile: string;
    TabNumber: integer;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string; outp: TStreamEncoder);
  end;

procedure TAnkiWordList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -e EDICT          specify EDICT file (otherwise EDICT)');
  writeln('  -tn column        zero-based. Take expressions from this column, if tab-separated');
end;

function TAnkiWordList.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-e' then begin
    if i>=ParamCount then BadUsage('-e needs dictionary name');
    Inc(i);
    EdictFile := ParamStr(i);
    Result := true
  end else
  if s='-tn' then begin
    if i>=ParamCount then BadUsage('-tn needs column number');
    Inc(i);
    TabNumber := StrToInt(ParamStr(i));
    Result := true
  end else
    Result := inherited;
end;

function TAnkiWordList.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;

procedure TAnkiWordList.Run;
var outp: TStreamEncoder;
  i: integer;
begin
  if Length(Files)<=0 then BadUsage('Specify input files.');

  writeln(ErrOutput, 'Loading dictionary');
  Edict := TEdict.Create;
  if EdictFile='' then begin
    if FileExists('EDICT2') then
      Edict.LoadFromFile('EDICT2')
    else
      Edict.LoadFromFile('EDICT');
  end else
    Edict.LoadFromFile(EdictFile);

  writeln(ErrOutput, IntToStr(edict.EntryCount)+' entries loaded.');

  if OutputFile<>'' then
    outp := UnicodeFileWriter(OutputFile)
  else
    outp := ConsoleWriter;
  outp.WriteBom;
  for i := 0 to Length(Files)-1 do
    ParseFile(Files[i], outp);
  FreeAndNil(outp);
end;

//inp is destroyed on exit
procedure TAnkiWordList.ParseFile(const AFilename: string; outp: TStreamEncoder);
var inp: TStreamDecoder;
  ln: string;
  parts: TStringArray;
  i: integer;
  line_c, expr_c, outp_c: integer;
  entry: PEdictEntry;
begin
  writeln(ErrOutput, 'Parsing '+AFilename+'...');
  inp := OpenTextFile(AFilename);
  try
    line_c := 0;
    expr_c := 0;
    outp_c := 0;

    while inp.ReadLn(ln) do begin
      Inc(line_c);

      if TabNumber<=0 then begin //common case faster
        i := pos(#09, ln);
        if i>0 then
          delete(ln, i, MaxInt);
      end else begin
        parts := StrSplit(PWideChar(ln),#09);
        if TabNumber>=Length(parts) then
          continue;
        ln := parts[TabNumber];
      end;

      Inc(expr_c);

      ln := Trim(ln);
      entry := Edict.FindEntry(ln);
      if entry=nil then continue;

      Inc(outp_c);
      outp.WriteLn(ln+#09+UniReplaceStr(entry.AllSenses,'/',', '));
    end;
  finally
    FreeAndNil(inp);
  end;

  writeln(AFilename+' stats: '+IntToStr(line_c)+' lines, '
    +IntToStr(expr_c)+' entries, '+IntToStr(outp_c)+' found.');
end;

begin
  RunApp(TAnkiWordList);
end.
