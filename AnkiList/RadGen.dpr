program RadGen;
{ Parses a list of kanji and lists all parts of characters used in each.
 Requires:
   radkfile       for RaineRadicals
   sqlite3.dll    for Yarxi, but statically linked, sorry
 And either:
   wakan.chr        for radical names
   wakan-types.cfg  for property type details in wakan.chr
 Or:
   yarxi.db       for radical names
   yarxi.kcs      for kana conversion
 Or:
   kanjidic       for kanjidic
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, ConsoleToolbox, JwbStrings, JwbIo, JwbCharData, RaineRadicals,
  TextTable, Yarxi, YarxiFmt, KanjiDic;

type
  TRadGen = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputFile: UnicodeString;
    WakanDescCount: integer;
    YarxiDescCount: integer;
    MaxDescCount: integer;
    Output: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;

  protected //Raine
    RaineRadicals: TRaineRadicals;

  protected //Kanjidic
    Kanjidic: TKanjidic;
    procedure KanjidicInit;
    procedure KanjidicFree;

  protected //Wakan.chr
    Chars: TTextTableCursor;
    CharProps: TCharPropertyCursor;
    procedure WakanInit;
    procedure WakanFree;
    procedure WakanLoadTypes(const AFilename: string);
    function WakanGetDesc(const AChar: char): TStringArray;

  protected //Yarxi
    Yarxi: TYarxiDb;
    procedure YarxiInit;
    procedure YarxiFree;
    function YarxiGetDesc(const AChar: char): TStringArray;

  public
    procedure Init; override;
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);
  end;

procedure TRadGen.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -y 0/1/2          paste up to this number of descriptions from Yarxi');
  writeln('  -w 0/1/2          paste up to this number of descriptions from Wakan');
end;

procedure TRadGen.Init;
begin
 //Enable the sources which are available, by default
  if FileExists('yarxi.db') and FileExists('yarxi.kcs') then
    YarxiDescCount := 1
  else
    YarxiDescCount := 0;
  if FileExists('wakan.chr') and FileExists('wakan-types.cfg') then
    WakanDescCount := 1
  else
    WakanDescCount := 0;
end;

function TRadGen.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-w' then begin
    if i>=ParamCount then BadUsage('-w requires description count');
    Inc(i);
    WakanDescCount := StrToInt(ParamStr(i));
    Result := true;
  end else
  if s='-y' then begin
    if i>=ParamCount then BadUsage('-y requires description count');
    Inc(i);
    YarxiDescCount := StrToInt(ParamStr(i));
    Result := true;
  end else
    Result := inherited;
end;

function TRadGen.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := ParamStr(i);
  Result := true;
end;

procedure TRadGen.Run;
var AFile: string;
  i: integer;
  tm: cardinal;
begin
  RaineRadicals := TRaineRadicals.Create;
  RaineRadicals.LoadFromRadKFile('RADKFILE');
  tm := GetTickCount;
  for i := 0 to 20 do begin
    KanjidicInit;
    KanjidicFree;
  end;
  tm := GetTickCount-tm;
  writeln('Time: '+IntToStr(tm));

  if WakanDescCount>0 then
    WakanInit;
  if YarxiDescCount>0 then
    YarxiInit;

  if OutputFile<>'' then
    Output := CreateTextFile(OutputFile, TUTF8Encoding)
  else
    Output := ConsoleWriter;
  Output.WriteBom;

  for AFile in Files do
    ParseFile(AFile);

  FreeAndNil(Output);
  YarxiFree;
  WakanFree;
  KanjidicFree;
end;

procedure TRadGen.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  i: integer;
  ch: char;
  rads: string; //radical list
  expl: string; //radicals with explanations
  desc, tmp: TStringArray;
begin
  inp := FileReader(AFilename);
  while inp.ReadChar(ch) do begin
    rads := RaineRadicals.GetCharRadicals(ch);

    expl := '';
    for i := 1 to Length(rads) do begin

      SetLength(desc, 0);
      if YarxiDescCount>0 then begin
        tmp := YarxiGetDesc(rads[i]);
        if Length(tmp)>YarxiDescCount then
          SetLength(tmp, YarxiDescCount);
        Append(desc, tmp);
      end;

      if WakanDescCount>0 then begin
        tmp := WakanGetDesc(rads[i]);
        if Length(tmp)>WakanDescCount then
          SetLength(tmp, WakanDescCount);
        Append(desc, tmp);
      end;

      if Length(desc)<=0 then
        expl := expl + rads[i] + ', '
      else
        expl := expl + rads[i] + ' (' + JoinStr(desc,', ')+ '), ';
    end;
    SetLength(expl, Length(expl)-2);

    Output.WriteLn(ch+#09+expl);
  end;
  FreeAndNil(inp);
end;

procedure TRadGen.KanjidicInit;
begin
  Kanjidic := TKanjidic.Create;
  Kanjidic.LoadFromFile('kanjidic');
end;

procedure TRadGen.KanjidicFree;
begin
  FreeAndNil(Kanjidic);
end;

procedure TRadGen.WakanInit;
begin
  LoadCharData('wakan.chr');
  WakanLoadTypes('wakan-types.cfg');
  Chars := TTextTableCursor.Create(TChar);
  CharProps := TCharPropertyCursor.Create(TCharProp);
end;

procedure TRadGen.WakanFree;
begin
  FreeAndNil(CharProps);
  FreeAndNil(Chars);
end;

procedure TRadGen.WakanLoadTypes(const AFilename: string);
var sl: TStringList;
  line: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    for line in sl do
      AddCharPropType(line);
  finally
    FreeAndNil(sl);
  end;
end;

function TRadGen.WakanGetDesc(const AChar: char): TStringArray;
var tmp: string;
begin
  if not Chars.Locate('Unicode',AChar) then begin
    SetLength(Result,0);
    exit;
  end;

  tmp := CharProps.GetJapaneseDefinitions(Chars.Int(TCharIndex), #09);
  if tmp='' then
    SetLength(Result,0)
  else
    Result := SplitStr(tmp,#09);
end;

procedure TRadGen.YarxiInit;
begin
  Yarxi := TYarxiDB.Create('yarxi.db');
  Yarxi.KanaTran.LoadFromFile('yarxi.kcs');
  Yarxi.ParseKanji;
end;

procedure TRadGen.YarxiFree;
begin
  FreeAndNil(Yarxi);
end;

function TRadGen.YarxiGetDesc(const AChar: char): TStringArray;
var kr: PKanjiRecord;
begin
  kr := Yarxi.FindKanji(AChar);
  if kr=nil then
    SetLength(Result, 0)
  else begin
    SetLength(Result, 1);
    Result[0] := StripRusNickFormatting(kr.RusNicks[0]);
    if Result[0]='' then //nickname is empty, don't confuse callers
      SetLength(Result, 0);
  end;
end;

begin
  RunApp(TRadGen);
end.
