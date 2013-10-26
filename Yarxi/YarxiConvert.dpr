program YarxiConvert;
{$APPTYPE CONSOLE}
{ Reads Yarxi database format and generates various things out of it.
 Requires sqlite3.dll in the same folder. }

uses
  SysUtils, UniStrUtils, ConsoleToolbox, sqlite3, sqlite3ds, uDataSetHelper,
  JWBIO, JWBKanaConv, YarxiFmt;

type
  TKanjiRecord = record
    Kanji: string;
    RusNick: string;
    RusNicks: TStringArray;
    OnYomi: TOnYomiEntries;
    KunYomi: string;
    Russian: string;
    Compounds: string;
    function JoinOns(const sep: string=' '): string;
  end;
  PKanjiRecord = ^TKanjiRecord;

  TTangoRecord = record
    K1, K2, K3, K4: word;
    Kana: string;
    Reading: string;
    Russian: string;
  end;
  PTangoRecord = ^TTangoRecord;

  TYarxiConvert = class(TCommandLineApp)
  protected
    Command: string;
    Yarxi: TSqliteDb;
    KanaTran: TRomajiTranslator;
    Output: TStreamEncoder;
    Kanji: array of TKanjiRecord;
    Tango: array of TTangoRecord;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure RunKanji;
    procedure RunTango;
  end;

function TKanjiRecord.JoinOns(const sep: string=' '): string;
var i: integer;
begin
  if Length(OnYomi)<=0 then begin
    Result := '';
    exit;
  end;
  Result := OnYomi[0].kana;
  for i := 1 to Length(OnYomi)-1 do
    Result := Result + sep + OnYomi[i].kana;
end;

procedure TYarxiConvert.ShowUsage;
begin
  writeln('Usage: '+ProgramName);
end;

function TYarxiConvert.HandleSwitch(const s: string; var i: integer): boolean;
begin
  Result := inherited;
end;

function TYarxiConvert.HandleParam(const s: string; var i: integer): boolean;
begin
  Command := s;
  Result := true;
end;

procedure TYarxiConvert.Run;
begin
  Yarxi := TSqLiteDb.Create('yarxi.db');
  KanaTran := TRomajiTranslator.Create;
  KanaTran.LoadFromFile('yarxi.kcs');
  Output := ConsoleWriter();

  if Command='kanji' then
    RunKanji
  else
  if (Command='words') or (Command='tango') then
    RunTango
  else
    BadUsage('Invalid command: '+Command);

  FreeAndNil(Output);
  FreeAndNil(KanaTran);
  FreeAndNil(Yarxi);
end;

procedure TYarxiConvert.RunKanji;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
  k: TKanjiRecord;
begin
  ds := Yarxi.Query('SELECT * FROM Kanji');
  SetLength(Kanji, 7000); //should be enough
  RecCount := 0;
  for rec in ds do begin
    k.Kanji := WideChar(word(rec.Uncd));
    k.RusNick := DecodeRussian(rec.RusNick);
    k.RusNicks := DecodeKanjiRusNick(k.RusNick);
    k.RusNick := StripAlternativeRusNicks(k.RusNick);
    k.OnYomi := SplitOnYomi(rec.OnYomi);
    k.KunYomi := rec.KunYomi;
    k.Russian := DecodeRussian(rec.Russian);
    k.Compounds := rec.Compounds;
    Kanji[RecCount] := k;
    Inc(RecCount);
  end;
  SetLength(Kanji, RecCount); //trim

  writeln(ErrOutput, IntToStr(RecCount)+' kanji read.');

  for k in Kanji do
    Output.WriteLn(
      SepJoin(k.RusNicks, '/') + #09
        + k.JoinOns + #09 + KanaTran.RomajiToKana('K'+k.JoinOns(' '), 1, []) + #09
        + k.KunYomi + #09 + KanaTran.RomajiToKana('H'+k.KunYomi, 1, []) + #09
        + k.Russian
    );
end;

procedure TYarxiConvert.RunTango;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
  k: TTangoRecord;
begin
  ds := Yarxi.Query('SELECT * FROM Tango');
  SetLength(Tango, 60000); //should be enough
  RecCount := 0;
  for rec in ds do begin
    k.Kana := rec.Kana;
    k.Reading := rec.Reading;
    k.Russian := DecodeRussian(rec.Russian);
    Tango[RecCount] := k;
    Inc(RecCount);
  end;
  SetLength(Kanji, RecCount); //trim

  writeln(ErrOutput, IntToStr(RecCount)+' tango read.');

  for k in Tango do
    Output.WriteLn(k.Kana + #09 + k.Reading + #09 + k.Russian);
end;

begin
  RunApp(TYarxiConvert);
end.
