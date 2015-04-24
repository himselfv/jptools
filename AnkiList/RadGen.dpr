program RadGen;
{ Parses a list of kanji and lists all parts of characters used in each.
 Requires:
   radkfile       for RaineRadicals
   sqlite3.dll    for Yarxi, but statically linked, sorry
 And either:
   kanjidic       for kanjidic
 Or:
   yarxi.db       for radical names
   yarxi.kcs      for kana conversion
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, Classes, ConsoleToolbox, JwbStrings, JwbIo, RaineRadicals,
  FastArray, Yarxi, YarxiCore, KanjidicReader, Kanjidic;

type
  TRadGen = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputFile: UnicodeString;
    RadkfileFile: string;
    KanjidicFile: string;
    KanjidicDescCount: integer;
    YarxiFile: string;
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
    function KanjidicGetDesc(const AChar: char): TStringArray;

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
  writeln('  -rf radkfile      specify radkfile file (otherwise RADKFILE)');
  writeln('  -kf kanjidic      specify kanjidic file (otherwise Kanjidic)');
  writeln('  -k 0/1/2          paste up to this number of descriptions from Kanjidic');
  writeln('  -yf yarxi.db      specify yarxi database (otherwise yarxi.db)');
  writeln('  -y 0/1/2          paste up to this number of descriptions from Yarxi');
  writeln('  -ys               keep yarxi parser silent, ignore warnings');
end;

procedure TRadGen.Init;
begin
  RadkfileFile := '';
  KanjidicFile := '';
  YarxiFile := '';
 //Enable the sources which are available, by default
  if FileExists('yarxi.db') or FileExists(ProgramFolder+'\yarxi.db') then
    YarxiDescCount := 1
  else
    YarxiDescCount := 0;
  if FileExists('kanjidic') or FileExists(ProgramFolder+'\kanjidic') then
    KanjidicDescCount := 1
  else
    KanjidicDescCount := 0;
end;

function TRadGen.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-rf' then begin
    if i>=ParamCount then BadUsage('-rf requires file name');
    Inc(i);
    RadkfileFile := ParamStr(i);
    Result := true;
  end else
  if s='-kf' then begin
    if i>=ParamCount then BadUsage('-kf requires file name');
    Inc(i);
    KanjidicFile := ParamStr(i);
    Result := true;
  end else
  if s='-k' then begin
    if i>=ParamCount then BadUsage('-k requires description count');
    Inc(i);
    KanjidicDescCount := StrToInt(ParamStr(i));
    Result := true;
  end else
  if s='-yf' then begin
    if i>=ParamCount then BadUsage('-yf requires file name');
    Inc(i);
    YarxiFile := ParamStr(i);
    Result := true;
  end else
  if s='-y' then begin
    if i>=ParamCount then BadUsage('-y requires description count');
    Inc(i);
    YarxiDescCount := StrToInt(ParamStr(i));
    Result := true;
  end else
  if s='-ys' then begin
    YarxiSilent := true;
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
begin
  if Length(Files)<=0 then
    BadUsage('No files given.');

  RaineRadicals := TRaineRadicals.Create;
  if RadkfileFile <> '' then
    RaineRadicals.LoadFromRadKFile(RadkfileFile)
  else
  if FileExists('radkfile') then
    RaineRadicals.LoadFromRadKFile('radkfile')
  else
    RaineRadicals.LoadFromRadKFile(ProgramFolder+'\radkfile');

  if YarxiDescCount>0 then
    YarxiInit;
  if KanjidicDescCount>0 then
    KanjidicInit;

  if OutputFile<>'' then
    Output := CreateTextFile(OutputFile, TUTF8Encoding)
  else
    Output := ConsoleWriter;
  Output.WriteBom;

  for AFile in Files do
    ParseFile(AFile);

  FreeAndNil(Output);
  YarxiFree;
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

      if KanjidicDescCount>0 then begin
        tmp := KanjidicGetDesc(rads[i]);
        if Length(tmp)>KanjidicDescCount then
          SetLength(tmp, KanjidicDescCount);
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
  if KanjidicFile <> '' then
    Kanjidic.LoadFromFile(KanjidicFile)
  else
  if FileExists('kanjidic') then
    Kanjidic.LoadFromFile('kanjidic')
  else
    Kanjidic.LoadFromFile(ProgramFolder+'\kanjidic');
end;

procedure TRadGen.KanjidicFree;
begin
  FreeAndNil(Kanjidic);
end;

function TRadGen.KanjidicGetDesc(const AChar: char): TStringArray;
var entry: PKanjiDicEntry;
  i: integer;
begin
  entry := Kanjidic.FindEntry(AChar);
  if entry=nil then begin
    SetLength(Result, 0);
    exit;
  end;

  SetLength(Result, entry.meanings.Length);
  for i := 0 to Length(Result)-1 do
    Result[i] := entry.meanings[i];
end;

procedure TRadGen.YarxiInit;
begin
  if YarxiFile <> '' then
    Yarxi := TYarxiDB.Create(YarxiFile)
  else
  if FileExists('yarxi.db') then
    Yarxi := TYarxiDB.Create('yarxi.db')
  else
    Yarxi := TYarxiDB.Create(ProgramFolder+'\yarxi.db');

  if FileExists('Hepburn-Yarxi.roma') then
    KanaTran.LoadFromFile('Hepburn-Yarxi.roma')
  else
    KanaTran.LoadFromFile(ProgramFolder+'\Hepburn-Yarxi.roma');
end;

procedure TRadGen.YarxiFree;
begin
  FreeAndNil(Yarxi);
end;

function TRadGen.YarxiGetDesc(const AChar: char): TStringArray;
var kr: TKanjiRecord;
begin
  if not Yarxi.GetKanji(AChar, kr)
  or (kr.RusNicks.Length<=0) then
    SetLength(Result, 0)
  else begin
    SetLength(Result, 1);
    Result[0] := kr.RusNicks[0];
    if Result[0]='' then //nickname is empty, don't confuse callers
      SetLength(Result, 0);
  end;
end;

begin
  RunApp(TRadGen);
end.
