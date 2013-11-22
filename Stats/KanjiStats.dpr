program KanjiStats;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, ConsoleToolbox, UniStrUtils, StreamUtils, SearchSort, JWBIO;

type
  TKanjiStatEntry = record
    Kanji: char;
    Count: integer;
  end;
  PKanjiStatEntry = ^TKanjiStatEntry;

  TKanjiStatList = array of TKanjiStatEntry;
  PKanjiStatList = ^TKanjiStatList;

  TKanjiStats = class(TCommandLineApp)
  protected
    InputFiles: array of string;
    KanjiFiles: array of string;
    OutputFile: string;
    VerboseOutput: boolean;
    OutputStream: TStreamEncoder;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    procedure OutputChar(const ks: TKanjiStatEntry);
    procedure OutputResults;
  protected
    KnownKanjis: array of char;
    function FindKnownKanji(c: char): integer;
    procedure AddKnownKanji(c: char);
    procedure LoadKnownKanjiFile(filename: string);
  protected
    Stats: TKanjiStatList;
    function GetKanjiStats(c: char): PKanjiStatEntry;
    procedure ParseFile(filename: string);
    procedure SortResults;
  public
    procedure ShowUsage; override;
    procedure Run; override;
  end;


procedure TKanjiStats.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -k known_kanji    ');
  writeln('  -o output file    (otherwise console)');
  writeln('  -v                verbose output (kanji=count)');
end;

function TKanjiStats.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := false;

  if SameText(s, '-k') then begin
    if i=ParamCount then
      BadUsage('After -k you have to specify a kanji list file.');
    Inc(i);
    SetLength(KanjiFiles, Length(KanjiFiles)+1);
    KanjiFiles[Length(KanjiFiles)-1] := ParamStr(i);
    Result := true;
  end else

  if SameText(s, '-v') then begin
    VerboseOutput := true;
    Result := true;
  end else

  if SameText(s, '-o') then begin
    if i=ParamCount then
      BadUsage('After -o you have to specify an output file.');
    Inc(i);
    if OutputFile<>'' then
      BadUsage('Output file already set to '+OutputFile+', '
        +'cannot set to '+ParamStr(i)+'.');
    OutputFile := ParamStr(i);
    Result := true;
  end;
end;

function TKanjiStats.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(InputFiles, Length(InputFiles)+1);
  InputFiles[Length(InputFiles)-1] := s;
  Result := true;
end;

procedure TKanjiStats.Run;
var i: integer;
begin
  if Length(InputFiles)<1 then
    BadUsage('You have to specify an input file');

  SetLength(KnownKanjis, 0);
  SetLength(Stats, 0);
  for i := 0 to Length(KanjiFiles) - 1 do
    LoadKnownKanjiFile(KanjiFiles[i]);
  for I := 0 to Length(InputFiles) - 1 do
    ParseFile(InputFiles[i]);
  SortResults;
  OutputResults;
end;

function TKanjiStats.FindKnownKanji(c: char): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(KnownKanjis) - 1 do
    if KnownKanjis[i]=c then begin
      Result := i;
      break;
    end;
end;

procedure TKanjiStats.AddKnownKanji(c: char);
begin
  SetLength(KnownKanjis, Length(KnownKanjis)+1);
  KnownKanjis[Length(KnownKanjis)-1] := c;
end;

procedure TKanjiStats.LoadKnownKanjiFile(filename: string);
var r: TStreamDecoder;
  c: char;
begin
  r := OpenTextFile(filename, TUTF16Encoding);
  try
    while r.ReadChar(c) do
      if IsKanji(c) and (FindKnownKanji(c)<0) then
        AddKnownKanji(c);
  finally
    FreeAndNil(r);
  end;
end;

function TKanjiStats.GetKanjiStats(c: char): PKanjiStatEntry;
var i: integer;
begin
  for i := 0 to Length(Stats) - 1 do
    if Stats[i].Kanji=c then begin
      Result := @Stats[i];
      exit;
    end;

  SetLength(Stats, Length(Stats)+1);
  Stats[Length(Stats)-1].Kanji := c;
  Stats[Length(Stats)-1].Count := 0;
  Result := @Stats[Length(Stats)-1];
end;

//Kanjis have been loaded, dict initialized, we're parsing files one by one
procedure TKanjiStats.ParseFile(filename: string);
var r: TStreamDecoder;
  c: char;
begin
  r := OpenTextFile(filename, TUTF16Encoding);
  try
    while r.ReadChar(c) do
      if IsKanji(c) and (FindKnownKanji(c)<0) then
        with GetKanjiStats(c)^ do
          Count := Count + 1;
  finally
    FreeAndNil(r);
  end;
end;

//Returns >0 if item 1 is greater than item 2. Zero if they're equal.
function StatsCmp(data: pointer; i1, i2: integer): integer;
var list: PKanjiStatList absolute data;
begin
  Result := list^[i2].Count-list^[i1].Count;
end;

//Moves item I to cell NewI, shifting the rest downwards. I is always >= NewI.
procedure StatsMov(data: pointer; I, NewI: integer);
var list: PKanjiStatList absolute data;
  ks: TKanjiStatEntry;
begin
  ks := list^[i];
  while i > newi do begin
    list^[i] := list^[i-1];
    Dec(i);
  end;
  list^[i] := ks;
end;

procedure TKanjiStats.SortResults;
begin
  BubbleSort(@Stats, Length(Stats), StatsCmp, StatsMov);
end;

procedure TKanjiStats.OutputChar(const ks: TKanjiStatEntry);
var str: string;
begin
  if VerboseOutput then
    str := ks.Kanji + '=' + IntToStr(ks.Count) + #13#10
  else
    str := ks.Kanji;
  OutputStream.Write(str);
end;

procedure TKanjiStats.OutputResults;
var i: integer;
begin
  if OutputFile <> '' then
    OutputStream := CreateTextFile(OutputFile, TUTF16Encoding)
  else
    OutputStream := ConsoleWriter();
  try
    OutputStream.WriteBom;
    for i := 0 to Length(Stats) - 1 do
      OutputChar(Stats[i]);
  finally
    FreeAndNil(OutputStream);
  end;
end;

begin
  RunApp(TKanjiStats);
end.
