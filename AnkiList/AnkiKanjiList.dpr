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
  SysUtils, Classes, UniStrUtils, ConsoleToolbox, JWBIO, FastArray,
  KanjiDicReader;

type
  TKanjidicRecord = record
    Kanji: UniChar;
    Ons: array[0..2] of UnicodeString;
    Kuns: array[0..2] of UnicodeString;
    Meaning: UniString;
    JlptLevel: integer;
    JouyouGrade: integer;
  end;
  PKanjidicRecord = ^TKanjiDicRecord;

  TAnkiKanjiList = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputFile: UnicodeString;
    Output: TStreamEncoder;
    KanjidicFile: UnicodeString;
    ReadingLevel: integer;
    WrapRareReadings: boolean;
    KDic: array of TKanjidicRecord;
    KDicLen: integer;
    function NewKDicCell: PKanjidicRecord;
    procedure AddEntry(entry: PKanjidicEntry);
    procedure LoadKanjidic(filename: string);
    procedure PrintKanji(var k: UniChar);
    procedure PrintKanjisFromFile(filename: string);
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
  end;

procedure TAnkiKanjiList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -k kanjidic.file  specify kanjidic file (otherwise KANJIDIC)');
  writeln('  -t 0/1/2          paste readings up to this T level (see KANJIDIC docs, default is 0)');
  writeln('  -ts               use html <s> tag for rare readings');
end;

function TAnkiKanjiList.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-k' then begin
    if i>=ParamCount then BadUsage('-k requires file name');
    Inc(i);
    KanjidicFile := ParamStr(i);
    Result := true;
  end else
  if s='-t' then begin
    if i>=ParamCount then BadUsage('-t requires rarity level');
    Inc(i);
    ReadingLevel := StrToInt(ParamStr(i));
    Result := true;
  end else
  if s='-ts' then begin
    WrapRareReadings := true;
    Result := true;
  end else
    Result := inherited;
end;

function TAnkiKanjiList.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := ParamStr(i);
  Result := true;
end;

function TAnkiKanjiList.NewKDicCell: PKanjidicRecord;
begin
  if KDicLen >= Length(KDic) then
    SetLength(KDic, KDicLen*2 + 20);
  Result := @KDic[KDicLen];
  Inc(KDicLen);
end;

procedure TAnkiKanjiList.AddEntry(entry: PKanjidicEntry);
var cell: PKanjidicRecord;
  i: integer;
begin
  cell := NewKDicCell;
  cell.Kanji := entry.kanji[1];

  for i := 0 to ReadingLevel do begin
    cell.Ons[i] := entry.readings[i].JoinOns(' ');
    cell.Kuns[i] := entry.readings[i].JoinKuns(' ');
  end;
  cell.Meaning := entry.JoinMeanings(', ');

  if not entry.TryGetIntValue('G', cell.JouyouGrade) then
    cell.JouyouGrade := -1; //not defined
  if not entry.TryGetIntValue('J', cell.JlptLevel) then
    cell.JouyouGrade := -1; //not defined
end;

procedure TAnkiKanjiList.LoadKanjidic(filename: string);
var f: TStreamDecoder;
  entry: TKanjidicEntry;
  s: UniString;
begin
  writeln(ErrOutput, 'Loading '+filename+'...');
  SetLength(KDic, 0);
  KDicLen := 0;
  f := OpenTextFile(filename); //kanjidic is normally EUC but lets guess it
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

procedure TAnkiKanjiList.PrintKanji(var k: UniChar);
var i, j: integer;
  flags: UniString;
  s_on, s_kun: UnicodeString;

  procedure AddFlag(flag: UniString);
  begin
    if flags='' then
      flags := flag
    else
      flags := flags + ' ' + flag;
  end;

 //Wraps in <s></s>, or doesnt
  function Wrap(const s: string; lvl: integer): string;
  begin
    if (lvl=0) or not WrapRareReadings then
      Result := s
    else
    if lvl=1 then
      Result := '<s>'+s+'</s>' //without a class, to keep data smaller. No class==t1
    else
      Result := '<s class="t2">'+s+'</s>';
  end;

begin
  for i := 0 to KDicLen - 1 do
    if KDic[i].Kanji=k then begin
      flags := '';
      if KDic[i].JlptLevel > 0 then
        AddFlag('jlpt'+IntToStr(KDic[i].JlptLevel));
      if KDic[i].JouyouGrade > 0 then
        AddFlag('jouyou'+IntToStr(KDic[i].JouyouGrade));

      s_on := '';
      s_kun := '';
      for j := 0 to ReadingLevel do begin
        if KDic[i].Ons[j]<>'' then
          s_on := s_on + Wrap(KDic[i].Ons[j], j)+' ';
        if KDic[i].Kuns[j]<>'' then
          s_kun := s_kun + Wrap(KDic[i].Kuns[j], j)+' ';
      end;
      SetLength(s_on, Length(s_on)-1);
      SetLength(s_on, Length(s_on)-1);

      Output.Write(k + #09 + s_on + #09 + s_kun + #09
        + KDic[i].Meaning + #09 + flags + #13#10);
      break;
    end;
end;

const
  sUtf16Bom: UniChar = #65520;

procedure TAnkiKanjiList.PrintKanjisFromFile(filename: string);
var f: TStreamDecoder;
  c: char;
begin
  writeln(ErrOutput, 'Parsing '+filename+'...');
  f := OpenTextFile(filename);
  try
    while f.ReadChar(c) do
      PrintKanji(c);
  finally
    FreeAndNil(f);
  end;
end;

procedure TAnkiKanjiList.Run;
var i: integer;
begin
  if ParamCount<1 then BadUsage();
  if Length(Files)<1 then BadUsage('Input files not specified');
  if OutputFile='' then BadUsage('Output file not specified');

  if KanjidicFile<>'' then
    LoadKanjidic(KanjidicFile)
  else
    if FileExists('kanjidic') then
      LoadKanjidic('kanjidic')
    else
      LoadKanjidic(ProgramFolder+'\kanjidic');

  if OutputFile<>'' then
    Output := UnicodeFileWriter(OutputFile)
  else
    Output := ConsoleWriter;
  Output.WriteBom;
  for i := 0 to Length(Files)-1 do
    PrintKanjisFromFile(Files[i]);
  FreeAndNil(Output);
end;

begin
  RunApp(TAnkiKanjiList);
end.
