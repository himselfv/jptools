program KanjiStats;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, ConsoleToolbox, UniStrUtils, StreamUtils, SearchSort, JWBIO,
  FilenameUtils;

type
  TKanjiStatEntry = record
    Kanji: char;
    Count: integer;
  end;
  PKanjiStatEntry = ^TKanjiStatEntry;

  TKanjiStatList = array of TKanjiStatEntry;
  PKanjiStatList = ^TKanjiStatList;

  TFileStats = record
    TotalChars: integer;
    TotalKanji: integer;
  end;

  TKanjiStats = class(TCommandLineApp)
  protected
    InputFiles: TFilenameArray;
    ScanSubdirs: boolean;
    KanjiFiles: array of string;
    OutputFile: string;
    VerboseOutput: boolean;
    OutputStream: TStreamEncoder;
    DefaultEncodingName: string;
    DefaultEncoding: CEncoding;
    DisplayProgress: boolean;
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
    procedure ParseFile(filename: string; out AFileStats: TFileStats);
    procedure SortResults;
  public
    procedure ShowUsage; override;
    procedure Run; override;
  end;


procedure TKanjiStats.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('  <file1>           Files or file masks to parse');
  writeln('Flags:');
  writeln('  -s                Scan subdirectories too');
  writeln('  -k known_kanji    List of characters to ignore (lets you scan for e.g. only characters yet unlearned)');
  writeln('  -o output file    (otherwise console)');
  writeln('  -e <encoding>     Assume this encoding (otherwise guess)');
  writeln('  -p                Display progress on error console');
  writeln('  -v                Verbose output (kanji=count)');
end;

function TKanjiStats.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := false;

  if SameText(s, '-s') then begin
    ScanSubdirs := true;
    Result := true;
  end else

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
  end else

  if SameText(s, '-e') then begin
    if i=ParamCount then
      BadUsage('-e requires encoding name');
    Inc(i);
    DefaultEncodingName := ParamStr(i);
    Result := true;
  end else

  if SameText(s, '-p') then begin
    DisplayProgress := true;
    Result := true;
  end;
end;

function TKanjiStats.HandleParam(const s: string; var i: integer): boolean;
begin
  AddFile(InputFiles, s);
  Result := true;
end;

procedure TKanjiStats.Run;
var i, total: integer;
  fstats: TFileStats;
begin
  if Length(InputFiles)<1 then
    BadUsage('You have to specify an input file');

  if DefaultEncodingName <> '' then begin
    DefaultEncoding := FindEncodingByName(DefaultEncodingName);
    if DefaultEncoding = nil then
      BadUsage('Unrecognized encoding: '+DefaultEncodingName);
  end;

  if DisplayProgress then
    writeln(ErrOutput, 'Scanning for files...');

  InputFiles := ExpandFileMasks(InputFiles, ScanSubdirs);

  if DisplayProgress then
    writeln(ErrOutput, IntToStr(Length(InputFiles))+' files total.');
  total := Length(InputFiles);

  SetLength(KnownKanjis, 0);
  SetLength(Stats, 0);
  for i := 0 to Length(KanjiFiles) - 1 do
    LoadKnownKanjiFile(KanjiFiles[i]);
  for i := 0 to Length(InputFiles) - 1 do begin
    if DisplayProgress then
      write(ErrOutput, CurrToStr(100 * i / total)+'%: '+InputFiles[i]+'... ');
    ParseFile(InputFiles[i], fstats);
    if DisplayProgress then
      writeln(ErrOutput, IntToStr(fstats.TotalChars)+' ch / '+IntToStr(fstats.TotalKanji)+' kj');
  end;
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

{$DEFINE BINSEARCH}

{$IFDEF BINSEARCH}
function Cmp_KanjiStats(Data: pointer; I: integer; Item: pointer): integer;
begin
  Result := integer(TKanjiStatList(Data)[I].Kanji) - integer(Item);
end;
{$ENDIF}

function TKanjiStats.GetKanjiStats(c: char): PKanjiStatEntry;
var i: integer;
begin
{$IFDEF BINSEARCH}
  if not BinSearch(Pointer(Stats), Length(Stats), Pointer(c), Cmp_KanjiStats, i) then begin
    SetLength(Stats, Length(Stats)+1);
    Move(Stats[i], Stats[i+1], (Length(Stats)-i-1)*SizeOf(Stats[i]));
    Stats[i].Kanji := c;
    Stats[i].Count := 0;
  end;
  Result := @Stats[i]
{$ELSE}
  for i := 0 to Length(Stats) - 1 do
    if Stats[i].Kanji=c then begin
      Result := @Stats[i];
      exit;
    end;

  SetLength(Stats, Length(Stats)+1);
  Stats[Length(Stats)-1].Kanji := c;
  Stats[Length(Stats)-1].Count := 0;
  Result := @Stats[Length(Stats)-1];
{$ENDIF}
end;

//Kanjis have been loaded, dict initialized, we're parsing files one by one
procedure TKanjiStats.ParseFile(filename: string; out AFileStats: TFileStats);
var r: TStreamDecoder;
  c: char;
begin
  FillChar(AFileStats, SizeOf(AFileStats), 0);
  r := OpenTextFile(filename, DefaultEncoding);
  try
    while r.ReadChar(c) do begin
      Inc(AFileStats.TotalChars);
      if IsKanji(c) then begin
        if FindKnownKanji(c) < 0 then
          with GetKanjiStats(c)^ do
            Count := Count + 1;
        Inc(AFileStats.TotalKanji);
      end;
    end;
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
