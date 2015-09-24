program YarxiKanjiInfo;
{ Similar to AnkiKanjiList. Takes a kanji char blob, produces tab-separated
 file listing Yarxi properties for each kanji, suitable for importing to Anki. }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Classes, SysUtils, ConsoleToolbox, JwbIo, FastArray, Yarxi, YarxiKanji,
  YarxiCore;

type
  TYarxiKanjiInfo = class(TCommandLineApp)
  protected
    Files: array of string;
    OutputFile: UnicodeString;
    Output: TStreamEncoder;
    YarxiFile: string;
    ReadingLevel: integer;
    WrapRareReadings: boolean;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
    function MergeOnYomi(const AOnYomi: TOnYomiEntries): string;

  protected //Yarxi
    Yarxi: TYarxiDb;
    procedure YarxiInit;
    procedure YarxiFree;

  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(const AFilename: string);

  end;

procedure TYarxiKanjiInfo.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -yf yarxi.db      specify yarxi database (otherwise yarxi.db)');
  writeln('  -ys               keep yarxi parser silent, ignore warnings');
  writeln('  -t 0/1/2          paste readings up to this rarity (default is 0)');
  writeln('  -ts               use html <s> tag for rare readings');
end;

function TYarxiKanjiInfo.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-yf' then begin
    if i>=ParamCount then BadUsage('-yf requires file name');
    Inc(i);
    YarxiFile := ParamStr(i);
    Result := true;
  end else
  if s='-ys' then begin
    YarxiSilent := true;
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

function TYarxiKanjiInfo.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := ParamStr(i);
  Result := true;
end;

procedure TYarxiKanjiInfo.Run;
var AFile: string;
begin
  if Length(Files)<=0 then
    BadUsage('No files given.');

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
end;

procedure TYarxiKanjiInfo.ParseFile(const AFilename: string);
var inp: TStreamDecoder;
  ch: char;
  ln: string;
  kj: TKanjiRecord;
begin
  inp := OpenTextFile(AFilename);
  while inp.ReadChar(ch) do begin
    if not Yarxi.GetKanji(ch, kj) then continue; //no info for kanji

    ln := ch+#09
      +IntToStr(kj.Nomer)+#09
      +FastArray.Join(kj.RusNicks,', ')+#09
      +MergeOnYomi(kj.OnYomi)
     //��������� ���� ������� �������������
     ;

    Output.WriteLn(ln);
  end;
  FreeAndNil(inp);
end;

function TYarxiKanjiInfo.MergeOnYomi(const AOnYomi: TOnYomiEntries): string;
const ASep: string = ', ';
var i: integer;
  RareOnYomi: string;
begin
 //�.�. rare � �� rare ����� ���� ���������� (������������..), �������� ������
  Result := '';
  for i := 0 to Length(AOnYomi)-1 do
    if not AOnYomi[i].rare then
      Result := Result + AOnYomi[i].kana + ASep;
  SetLength(Result, Length(Result)-Length(ASep)); //last separator

  if ReadingLevel<1 then exit;

  RareOnYomi := '';
  for i := 0 to Length(AOnYomi)-1 do
    if AOnYomi[i].rare then
      RareOnYomi := RareOnYomi + AOnYomi[i].kana + ASep;
  SetLength(RareOnYomi, Length(RareOnYomi)-Length(ASep)); //last separator

  if RareOnYomi='' then exit;

  if WrapRareReadings then
    RareOnYomi := '<s>'+RareOnYomi+'</s>';

  if Result<>'' then
    Result := Result + ASep + RareOnYomi
  else
    Result := RareOnYomi;
end;

procedure TYarxiKanjiInfo.YarxiInit();
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

procedure TYarxiKanjiInfo.YarxiFree;
begin
  FreeAndNil(Yarxi);
end;

begin
  RunApp(TYarxiKanjiInfo);
end.
