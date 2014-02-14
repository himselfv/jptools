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
  JWBEdictReader, JWBEdictMarkers, Edict, ActiveX, XmlDoc, XmlIntf,
  FastArray;

type
  TOutputEntry = record
    key: string;
    expr: string;
    read: string;
    text: string;
    procedure Reset(const AKey: string);
  end;
  POutputEntry = ^TOutputEntry;

  TAnkiWordList = class(TCommandLineApp)
  protected
    Files: array of string;
    EdictFile: string;
    Edict: TEdict;
    OutputFile: string;
    OutputXml: boolean;
    XsltFilename: string;
    ExprColumn: integer;
    ReadingColumn: integer;
    iInp, iXsl: IXMLDocument;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    function FindEntries(const AExpr, ARead: string): TArray<PEdictEntry>;
    function XsltTransform(const s: UnicodeString): WideString;
    procedure AddToOutput(var AOutput: TOutputEntry; const AEntry: PEdictEntry);
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string; outp: TStreamEncoder);
  end;

procedure TAnkiWordList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -e EDICT          specify EDICT file (otherwise EDICT)');
  writeln('');
  writeln('Input:');
  writeln('  -tn column        zero-based. Take expressions from this column, if tab-separated (default is 0)');
  writeln('  -tr column        Take readings from this column, if tab-separated (default is 1, unless -tn is overriden, then no default)');
  writeln('');
  writeln('Output:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -xml              output xml instead of the default plaintext');
  writeln('  -xslt <filename>  output xml converted by this XSLT schema');
  writeln('  -or               add both expression and reading for each entry');
  writeln('  -match=<mode>     if there are multiple matches:');
  writeln('    best            output ONE best match');
  writeln('    multiple        output all matches as a single result');
  writeln('    split           output multiple matches as multiple results');

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
    ExprColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-tr' then begin
    if i>=ParamCount then BadUsage('-tr needs column number');
    Inc(i);
    ReadingColumn := StrToInt(ParamStr(i));
    Result := true
  end else
  if s='-xml' then begin
    OutputXml := true;
    Result := true
  end else
  if s='-xslt' then begin
    if i>=ParamCount then BadUsage('-xslt needs XSLT schema filename');
    Inc(i);
    OutputXml := true;
    XsltFilename := ParamStr(i);
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

  if XsltFilename<>'' then begin
    CoInitialize(nil);
    iInp := TXMLDocument.Create(nil);
    iXsl := LoadXMLDocument(XsltFilename);
  end;

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

 //Release
  iXsl := nil;
  iInp := nil;
end;

function TAnkiWordList.FindEntries(const AExpr, ARead: string): TArray<PEdictEntry>;
var entry: PEdictEntry;
begin
 //TODO
  Result.Reset;
  entry := Edict.FindEntry(AExpr);
  if entry<>nil then
    Result.Add(entry);
end;

function GenerateXml(entry: PEdictEntry): string;
var i, j: integer;
  parts: TStringArray;
  kanji: PKanjiEntry;
  kana: PKanaEntry;
  sense: PSenseEntry;
begin
 //Note: Everything must be escaped. "&" often appears in fields.

  Result := '<entry>'
    +'<ref>'+HtmlEscape(entry.ref)+'</ref>';
  for i := 0 to Length(entry.kanji)-1 do begin
    kanji := @entry.kanji[i];
    Result := Result+'<expr>'+HtmlEscape(kanji.kanji);
    for j := 1 to Length(kanji.markers) do
      Result := Result + '<mark>' + HtmlEscape(GetMarkEdict(kanji.markers[j]))+'</mark>';
    Result := Result + '</expr>';
  end;

  for i := 0 to Length(entry.kana)-1 do begin
    kana := @entry.kana[i];
    Result := Result+'<read>'+HtmlEscape(kana.kana);
    for j := 1 to Length(kana.markers) do
      Result := Result + '<mark>' + HtmlEscape(GetMarkEdict(kana.markers[j]))+'</mark>';
    Result := Result+'</read>';
  end;

  for i := 0 to Length(entry.senses)-1 do begin
    sense := @entry.senses[i];
    Result := Result+'<sense>';
    parts := StrSplit(PChar(sense.text), '/');
    for j := 0 to Length(parts)-1 do
      Result := Result+'<gloss>'+HtmlEscape(Trim(parts[j]))+'</gloss>';
    for j := 1 to Length(sense.pos) do
      Result := Result+'<pos>'+HtmlEscape(GetMarkEdict(sense.pos[j]))+'</pos>';
    for j := 1 to Length(sense.markers) do
      Result := Result+'<mark>'+HtmlEscape(GetMarkEdict(sense.markers[j]))+'</mark>';
    Result := Result+'</sense>';
  end;

  if entry.pop then
    Result := Result + '<pop />';

  Result := Result + '</entry>';
end;

function TAnkiWordList.XsltTransform(const s: UnicodeString): WideString;
begin
  iInp.LoadFromXML(s);
  iInp.Node.TransformNode(iXsl.Node,Result);
end;

procedure TOutputEntry.Reset(const AKey: string);
begin
  key := AKey;
  expr := '';
  read := '';
  text := '';
end;

procedure TryAddUnique(var str: string; const part, sep: string);
begin
  if pos(sep+part+sep, sep+str+sep)<=0 then
    if str<>'' then
      str := str + sep +  part
    else
      str := part;
end;

procedure TAnkiWordList.AddToOutput(var AOutput: TOutputEntry; const AEntry: PEdictEntry);
var i: integer;
  entry_text: string;
begin
  for i := 0 to Length(AEntry.kanji)-1 do
    TryAddUnique(AOutput.expr, AEntry.kanji[i].kanji, '、');

  for i := 0 to Length(AEntry.kana)-1 do
    TryAddUnique(AOutput.read, AEntry.kana[i].kana, '、');

  if not OutputXml then begin
    entry_text := UniReplaceStr(AEntry.AllSenses,'/',', ');
    if AOutput.text<>'' then
      AOutput.text := AOutput.text + '; ' + entry_text;
  end else begin
    entry_text := GenerateXml(AEntry);
    AOutput.text := AOutput.text + entry_text;
   //will call xslt at the end
  end;
end;

//inp is destroyed on exit
procedure TAnkiWordList.ParseFile(const AFilename: string; outp: TStreamEncoder);
var inp: TStreamDecoder;
  ln, expr, read: string;
  parts: TStringArray;
  i: integer;
  line_c, expr_c, outp_c: integer;
  outp_text: string;
  EdictEntries: TArray<PEdictEntry>;
  OutputEntries: TArray<TOutputEntry>;
  outp_e: POutputEntry;
begin
  writeln(ErrOutput, 'Parsing '+AFilename+'...');
  inp := OpenTextFile(AFilename);
  try
    line_c := 0;
    expr_c := 0;
    outp_c := 0;

    while inp.ReadLn(ln) do begin
      Inc(line_c);

      if (ExprColumn<=0) and (ReadingColumn=0) then begin //common case faster
        i := pos(#09, ln);
        if i>0 then
          delete(ln, i, MaxInt);
        expr := ln;
      end else begin
        parts := StrSplit(PWideChar(ln),#09);
        if ExprColumn>=Length(parts) then
          continue;
        expr := parts[ExprColumn];
      end;

      Inc(expr_c);

      expr := Trim(expr);
      EdictEntries := FindEntries(expr, read);
      if EdictEntries.Count<=0 then continue;

     //Split output as configured
      OutputEntries.Reset;
      outp_e := POutputEntry(OutputEntries.AddNew);
      outp_e.Reset(expr);
      AddToOutput(outp_e^, EdictEntries[0]);

     //Output
      for i := 0 to OutputEntries.Count-1 do begin
        OutputEntries.GetPointer(i).text := XsltTransform(OutputEntries[i].text);
        outp_text := OutputEntries[i].key + #09 //key
         //TODO: additional expr/read if requested
          + OutputEntries[i].text;
        outp.WriteLn(outp_text);
        Inc(outp_c);
      end;

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
