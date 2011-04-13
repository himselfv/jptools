program AnkiList;
{$APPTYPE CONSOLE}
{
 Parses kanji list and builds a card list in a format suitable for importing to Anki:
   kanji    on    kun    meaning    flags

 Intended usage: 
   1. Make a kanji list for studying either manually, in Wakan or by other means.
   2. Import cards to Anki with the help of this tool.

 Requires kanjidic.
}

uses
  SysUtils, Classes, UniStrUtils, StreamUtils;

type
  TKanjidicRecord = record
    Kanji: UniChar;
    OnRead: UniString;
    KunRead: UniString;
    Meaning: UniString;
    JlptLevel: integer;
    JouyouGrade: integer;
  end;
  PKanjidicRecord = ^TKanjiDicRecord;

var
  KDic: array of TKanjidicRecord;
  KDicLen: integer;
  Output: TCharWriter;

function NewKDicCell: PKanjidicRecord;
begin
  if KDicLen >= Length(KDic) then
    SetLength(KDic, KDicLen*2 + 20);
  Result := @KDic[KDicLen];
  Inc(KDicLen);
end;

procedure ParseStr(s: UniString);
var Cell: PKanjidicRecord;
  Mean: PUniChar;
  i: integer;
  parts: TUniStringArray;
begin
  if (Length(s)<=0) or (s[1]='#') then
    exit; //nothing to parse

  Cell := NewKDicCell;

  Mean := StrScan(PUniChar(s), '{');
  if Mean <> nil then begin
    Cell.Meaning := Mean;
    for i := 1 to Length(Cell.Meaning) do begin
      if Cell.Meaning[i]='}' then
        Cell.Meaning[i] := ','
      else
      if Cell.Meaning[i]='{' then
        Cell.Meaning[i] := ' ';
    end;

    SetLength(s, CharLenW(PWideChar(s), mean));
  end;

  parts := StrSplitW(PWideChar(s), ' ');
  if (Length(parts) < 1) or (Length(parts[1]) < 1) then
    raise Exception.Create('Invalid line in the input file');

  Cell.Kanji := parts[0][1];
  Cell.OnRead := '';
  Cell.KunRead := '';
  Cell.JlptLevel := -1; //not defined
  Cell.JouyouGrade := -1; //not defined

  for i := 1 to Length(parts) - 1 do
    if Length(parts[i]) <= 0 then begin
    end else
    if parts[i][1]='G' then begin //JouyouGrade
      if not TryStrToInt(UniString(@parts[i][2]), Cell.JouyouGrade) then
        raise Exception.Create('Wrong Jouyou grade tag format');
    end else
    if parts[i][1]='J' then begin //Jlpt Level
      if not TryStrToInt(UniString(@parts[i][2]), Cell.JlptLevel) then
        raise Exception.Create('Wrong Jlpt level tag format');
    end else
    if IsKatakana(parts[i][1]) then
      Cell.OnRead := Cell.OnRead + ' ' + parts[i]
    else
    if IsHiragana(parts[i][1]) then
      Cell.KunRead := Cell.KunRead + ' ' + parts[i];
end;

procedure LoadKanjidic(filename: string);
var f: TCharReader;
  s: UniString;
begin
  writeln('Loading '+filename+'...');
  SetLength(KDic, 0);
  KDicLen := 0;
  f := TCharReader.Create(
    TFileStream.Create(filename, fmOpenRead), true);
  try
    while f.ReadLine(s) do
      ParseStr(s);
  finally
    FreeAndNil(f);
  end;
end;

procedure PrintKanji(var k: UniChar);
var i: integer;
  flags: UniString;

  procedure AddFlag(flag: UniString);
  begin
    if flags='' then
      flags := flag
    else
      flags := flags + ' ' + flag;
  end;

begin
  for i := 0 to KDicLen - 1 do
    if KDic[i].Kanji=k then begin
      flags := '';
      if KDic[i].JlptLevel > 0 then
        AddFlag('jlpt'+IntToStr(KDic[i].JlptLevel));
      if KDic[i].JouyouGrade > 0 then
        AddFlag('jouyou'+IntToStr(KDic[i].JouyouGrade));

      Output.WriteString(k + #09 + KDic[i].OnRead + #09
        + KDic[i].KunRead + #09 + KDic[i].Meaning + #09
        + flags + #13#10);
      break;
    end;
end;

const
  sUtf16Bom: UniChar = #65520;

procedure PrintKanjisFromFile(filename: string);
var f: TFileStream;
  c: char;
begin
  writeln('Parsing '+filename+'...');
  f := TFileStream.Create(filename, fmOpenRead);
  try
    if f.Read(c, SizeOf(c)) <> SizeOf(c) then exit;
    if c = sUtf16Bom then begin
      if f.Read(c, SizeOf(c)) <> SizeOf(c) then exit;
    end;

    repeat
      PrintKanji(c);
    until f.Read(c, sizeof(c)) < sizeof(c);
  finally
    FreeAndNil(f);
  end;
end;

var i: integer;
  OutputFile: WideString;
begin
  try
    if paramcount < 1 then begin
      writeln('Usage: '+ExtractFilename(paramstr(0))+' <file1> [file2] ... -o output.file');
      exit;
    end;

    LoadKanjidic('kanjidic-u');

    for i := 1 to ParamCount-1 {sic!} do
      if paramstr(i)='-o' then begin
        if OutputFile<>'' then
          raise Exception.Create('Output file specified more than once');
        OutputFile := ParamStr(i+1);
      end;
    if OutputFile='' then
      raise Exception.Create('Output file not specified');

    Output := TCharWriter.Create(
      TFileStream.Create(OutputFile, fmCreate, fmShareExclusive), csUtf16Le, true);
    Output.WriteBom;

    i := 1;
    while i <= ParamCount do
      if paramstr(i)='-o' then
        Inc(i, 2)
      else begin
        PrintKanjisFromFile(paramstr(i));
        Inc(i);
      end;

    FreeAndNil(Output);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
