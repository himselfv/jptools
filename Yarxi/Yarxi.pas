unit Yarxi;
{
Usage:
  Yarxi := TYarxiDB.Create('yarxi.db');
  Yarxi.KanaTran.LoadFromFile('yarxi.kcs');
}


interface
uses SysUtils, sqlite3, sqlite3ds, uDataSetHelper, UniStrUtils, JWBKanaConv,
  FastArray, YarxiFmt;

type
  TKanjiRecord = record
    Nomer: integer;
    Kanji: string;
    RusNick: string;
    RusNicks: TRusNicks;
    RawRusNick: string;
    OnYomi: TOnYomiEntries;
    KunYomi: TKanjiReadings;
    RawKunYomi: string;
    RawRussian: string;
    Russian: TRussianMeanings;
    Compounds: TCompoundEntries;
    RawCompounds: string;
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

  TYarxiDB = class
  protected
    Db: TSqliteDb;
    function ParseKanji(const Rec: variant): TKanjiRecord;
    function ParseTango(const Rec: variant): TTangoRecord;

  protected
    procedure LoadKanjiIndex;

  protected
    procedure LoadKanji;
    procedure LoadTango;
    function FindKanjiIndex(const AChar: string): integer; //do not expose
  public
    Kanji: array of TKanjiRecord;
    Tango: array of TTangoRecord;

  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function GetKanji(const AChar: string; out ARec: TKanjiRecord): boolean;
    function KanjiCount: integer;
    function TangoCount: integer;

  end;

function Join(const AArray: TStringArray; const sep: string=', '): string; overload;

implementation
uses StrUtils, YarxiRefs, YarxiCore;

function Join(const AArray: TStringArray; const sep: string=', '): string;
var i: integer;
begin
  if Length(AArray)<=0 then begin
    Result := '';
    exit;
  end;
  Result := AArray[0];
  for i := 1 to Length(AArray)-1 do
    Result := Result + sep + AArray[i];
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

constructor TYarxiDB.Create(const AFilename: string);
begin
  inherited Create;
  Db := TSqLiteDb.Create(AFilename);
end;

destructor TYarxiDB.Destroy;
begin
  FreeAndNil(Db);
  inherited;
end;

function TYarxiDB.ParseKanji(const Rec: variant): TKanjiRecord;
var i: integer;
begin
  Result.Nomer := rec.Nomer;
  if (Result.Nomer=216) or (Result.Nomer=219) or (Result.Nomer=1385)
  or (Result.Nomer=1528) or (Result.Nomer=1617) or (Result.Nomer=2403)
  or (Result.Nomer=2664) then begin //некоторые кандзи просто в полной беде
    FillChar(Result, SizeOf(Result), 0);
    exit; //откройте базу, полюбуйтесь на них
  end;

  PushComplainContext('#'+IntToStr(Result.Nomer));
  try
    Result.Kanji := WideChar(word(rec.Uncd));
    Result.RawRusNick := DecodeRussian(rec.RusNick);
    Result.RusNicks := ParseKanjiRusNick(Result.RawRusNick);
    if Result.RusNicks.Length>0 then
      Result.RusNick := Result.RusNicks[0]
    else
      Result.RusNick := '';
    Result.OnYomi := ParseOnYomi(rec.OnYomi);
   //Преобразуем после сплита, т.к. может затронуть разметочные символы типа тире
    for i := 0 to Length(Result.OnYomi)-1 do
      Result.OnYomi[i].kana := KanaTran.RomajiToKana(Result.OnYomi[i].kana, []);
    Result.RawKunYomi := rec.KunYomi;
    Result.KunYomi := ParseKanjiKunYomi(Result.RawKunYomi);
    Result.RawRussian := repl(DecodeRussian(rec.Russian),'\','');
    Result.Russian := ParseKanjiRussian(Result.RawRussian);
    Result.RawCompounds := rec.Compounds;
    Result.Compounds := ParseKanjiCompounds(Result.RawCompounds);
  finally
    PopComplainContext;
  end;
end;

function TYarxiDB.ParseTango(const Rec: variant): TTangoRecord;
begin
  Result.Kana := rec.Kana;
  Result.Reading := rec.Reading;
  Result.Russian := DecodeRussian(rec.Russian);
end;

procedure TYarxiDB.LoadKanjiIndex;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
begin
  ds := Db.Query('SELECT * FROM Kanji');
  RecCount := 1;
  for rec in ds do begin
   //Вообще-то лучше брать rec.Nomer, но пока он везде совпадает
    KanjiRefs.Add(RecCount, WideChar(word(rec.Uncd)));
    Inc(RecCount);
  end;
end;


procedure TYarxiDB.LoadKanji;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
begin
  if Length(Kanji)>0 then exit; //already parsed;
  LoadKanjiIndex;
  ds := Db.Query('SELECT * FROM Kanji');
  SetLength(Kanji, 7000); //should be enough
  RecCount := 0;
  for rec in ds do begin
    Kanji[RecCount] := ParseKanji(rec);
    Inc(RecCount);
  end;
  SetLength(Kanji, RecCount); //trim
end;

procedure TYarxiDB.LoadTango;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
begin
  if Length(Tango)>0 then exit; //already parsed;
  ds := Db.Query('SELECT * FROM Tango');
  SetLength(Tango, 60000); //should be enough
  RecCount := 0;
  for rec in ds do begin
    Tango[RecCount] := ParseTango(rec);
    Inc(RecCount);
  end;
  SetLength(Kanji, RecCount); //trim
end;

function TYarxiDB.FindKanjiIndex(const AChar: string): integer;
var i: integer;
begin
  LoadKanji;
 //Stupid for now, may have to
 //  1. Use internal BTrees, or
 //  2. Query DB and parse dynamically
  Result := -1;
  for i := 0 to Length(Kanji)-1 do
    if Kanji[i].Kanji=AChar then begin
      Result := i;
      break;
    end;
end;

function TYarxiDB.GetKanji(const AChar: string; out ARec: TKanjiRecord): boolean;
var idx: integer;
begin
  idx := FindKanjiIndex(AChar);
  if idx<0 then begin
    Result := false;
    exit;
  end else begin
    ARec := Kanji[idx];
    Result := true;
  end;
end;

function TYarxiDB.KanjiCount: integer;
begin
  LoadKanji;
  Result := Length(Kanji);
end;

function TYarxiDB.TangoCount: integer;
begin
  LoadTango;
  Result := Length(Tango);
end;

end.
