program KanjiStats;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, UniStrUtils, StreamUtils, SearchSort;

type
  EBadUsage = class(Exception)
  end;

procedure BadUsage(msg: UniString);
begin
  raise EBadUsage.Create(msg);
end;

procedure PrintUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<file1> [file2] '
    +'[-k known_kanji] [-o output file] [-v]');
  writeln('If no output specified, console will be used.');
  writeln('-v enables verbose output (kanji=count)');
end;

var
  InputFiles: array of UniString;
  KanjiFiles: array of UniString;
  OutputFile: UniString; {пустой - значит, консоль}
  VerboseOutput: boolean;

procedure ParseCommandLine;
var i: integer;
  s: UniString;
begin
  SetLength(InputFiles, 0);
  SetLength(KanjiFiles, 0);
  OutputFile := '';
  VerboseOutput := false;

  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if Length(s)=0 then begin
      Inc(i);
      continue;
    end;
    if s[1]<>'-' then begin
      SetLength(InputFiles, Length(InputFiles)+1);
      InputFiles[Length(InputFiles)-1] := s;
      Inc(i);
      continue;
    end;

    if SameText(s, '-k') then begin
      if i=ParamCount then
        BadUsage('After -k you have to specify a kanji list file.');
      Inc(i);
      SetLength(KanjiFiles, Length(KanjiFiles)+1);
      KanjiFiles[Length(KanjiFiles)-1] := ParamStr(i);
      Inc(i);
      continue;
    end;

    if SameText(s, '-v') then begin
      VerboseOutput := true;
      Inc(i);
      continue;
    end;

    if SameText(s, '-o') then begin
      if i=ParamCount then
        BadUsage('After -o you have to specify an output file.');
      Inc(i);
      if OutputFile<>'' then
        BadUsage('Output file already set to '+OutputFile+', '
          +'cannot set to '+ParamStr(i)+'.');
      OutputFile := ParamStr(i);
      Inc(i);
      continue;
    end;

    BadUsage('Unrecognized switch: '+s);
  end;

  if Length(InputFiles)<1 then
    BadUsage('You have to specify an input file');
end;


var
  KnownKanjis: array of UniChar;

function FindKnownKanji(c: UniChar): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(KnownKanjis) - 1 do
    if KnownKanjis[i]=c then begin
      Result := i;
      break;
    end;
end;

procedure AddKnownKanji(c: UniChar);
begin
  SetLength(KnownKanjis, Length(KnownKanjis)+1);
  KnownKanjis[Length(KnownKanjis)-1] := c;
end;

procedure LoadKnownKanjiFile(filename: UniString);
var r: TCharReader;
  c: UniChar;
begin
  r := TCharReader.Create(
    TFileStream.Create(filename, fmOpenRead), {OwnsStream=}true);
  try
    while r.ReadChar(c) do
      if IsKanji(c) and (FindKnownKanji(c)<0) then
        AddKnownKanji(c);
  finally
    FreeAndNil(r);
  end;
end;


type
  TKanjiStats = record
    Kanji: UniChar;
    Count: integer;
  end;
  PKanjiStats = ^TKanjiStats;

  TKanjiStatList = array of TKanjiStats;
  PKanjiStatList = ^TKanjiStatList;

var
  Stats: TKanjiStatList;

function GetKanjiStats(c: UniChar): PKanjiStats;
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
procedure ParseFile(filename: UniString);
var r: TCharReader;
  c: UniChar;
begin
  r := TCharReader.Create(
    TFileStream.Create(filename, fmOpenRead), {OwnsStream=}true);
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
  ks: TKanjiStats;
begin
  ks := list^[i];
  while i > newi do begin
    list^[i] := list^[i-1];
    Dec(i);
  end;
  list^[i] := ks;
end;


procedure SortResults;
begin
  BubbleSort(@Stats, Length(Stats), StatsCmp, StatsMov);
end;

var
  OutputStream: TStream;

procedure OutputChar(const ks: TKanjiStats);
var str: UniString;
begin
  if VerboseOutput then
    str := ks.Kanji + '=' + IntToStr(ks.Count) + #13#10
  else
    str := ks.Kanji;
  if OutputStream<>nil then
    OutputStream.Write(str[1], Length(str)*SizeOf(UniChar))
  else
    write(str);
end;

procedure OutputResults;
var i: integer;
 bom: char;
begin
  if OutputFile <> '' then
    OutputStream := TFileStream.Create(OutputFile, fmCreate)
//    OutputStream := TStreamWriter.Create(
//      TFileStream.Create(OutputFile, fmCreate), {OwnsStream=}true)
  else
    OutputStream := nil;
  try
    bom := BOM_UTF16LE;
    if OutputStream<>nil then
      OutputStream.Write(bom, SizeOf(bom));
    for i := 0 to Length(Stats) - 1 do
      OutputChar(Stats[i]);
  finally
    if OutputStream<>nil then
      FreeAndNil(OutputStream);
  end;
end;

//Settings have been loaded already
procedure Run;
var i: integer;
begin
  SetLength(KnownKanjis, 0);
  SetLength(Stats, 0);
  for i := 0 to Length(KanjiFiles) - 1 do
    LoadKnownKanjiFile(KanjiFiles[i]);
  for I := 0 to Length(InputFiles) - 1 do
    ParseFile(InputFiles[i]);
  SortResults;
  OutputResults;
end;

begin
  if ParamCount=0 then begin
    PrintUsage;
    exit;
  end;

  try
    ParseCommandLine;
    Run;
  except
    on E: EBadUsage do begin
      writeln('Bad usage. ');
      writeln('  '+E.Message);
      PrintUsage;
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
