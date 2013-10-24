program AnkiKanjiList;
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
  SysUtils, Classes, UniStrUtils, JWBIO, JWBKanjiDicReader;

type
  TKanjidicRecord = record
    Kanji: UniChar;
    OnT0, OnT1: UniString;
    KunT0, KunT1: UniString;
    Meaning: UniString;
    JlptLevel: integer;
    JouyouGrade: integer;
  end;
  PKanjidicRecord = ^TKanjiDicRecord;

var
  KDic: array of TKanjidicRecord;
  KDicLen: integer;
  Output: TStreamEncoder;

function NewKDicCell: PKanjidicRecord;
begin
  if KDicLen >= Length(KDic) then
    SetLength(KDic, KDicLen*2 + 20);
  Result := @KDic[KDicLen];
  Inc(KDicLen);
end;

procedure AddEntry(entry: PKanjidicEntry);
var cell: PKanjidicRecord;
begin
  cell := NewKDicCell;
  cell.Kanji := entry.kanji[1];

  cell.OnT0 := entry.readings[0].JoinOns(' ');
  cell.OnT1 := entry.readings[1].JoinOns(' ');
  cell.KunT0 := entry.readings[0].JoinKuns(' ');
  cell.KunT1 := entry.readings[1].JoinKuns(' ');
  cell.Meaning := entry.JoinMeanings(', ');

  if not entry.TryGetIntValue('G', cell.JouyouGrade) then
    cell.JouyouGrade := -1; //not defined
  if not entry.TryGetIntValue('J', cell.JlptLevel) then
    cell.JouyouGrade := -1; //not defined
end;

procedure LoadKanjidic(filename: string);
var f: TStreamDecoder;
  entry: TKanjidicEntry;
  s: UniString;
begin
  writeln('Loading '+filename+'...');
  SetLength(KDic, 0);
  KDicLen := 0;
  f := OpenTextFile(filename, TUTF8Encoding);
  try
    while f.ReadLn(s) do begin
      if (Length(s)<=0) or (s[1]='#') then
        continue;
      ParseKanjidicLine(s, @entry);
      AddEntry(@entry);
    end;
  finally
    FreeAndNil(f);
  end;
end;

procedure PrintKanji(var k: UniChar);
var i: integer;
  flags: UniString;
  s_on, s_kun: UnicodeString;

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

      s_on := KDic[i].OnT0;
      if KDic[i].OnT1<>'' then begin
        if s_on<>'' then s_on := s_on + ' ';
        s_on := s_on + '<s>'+KDic[i].OnT1+'</s>';
      end;

      s_kun := KDic[i].KunT0;
      if KDic[i].KunT1<>'' then begin
        if s_kun<>'' then s_kun := s_kun + ' ';
        s_kun := s_kun + '<s>'+KDic[i].KunT1+'</s>';
      end;

      Output.Write(k + #09 + s_on + #09 + s_kun + #09
        + KDic[i].Meaning + #09 + flags + #13#10);
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

    Output := UnicodeFileWriter(OutputFile);
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
