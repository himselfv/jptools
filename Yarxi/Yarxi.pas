unit Yarxi;
{
Usage:
  Yarxi := TYarxiDB.Create('yarxi.db');
  Yarxi.KanaTran.LoadFromFile('yarxi.kcs');
}

interface
uses SysUtils, sqlite3, sqlite3ds, uDataSetHelper, UniStrUtils, JWBKanaConv, YarxiFmt;

type
  TKanjiRecord = record
    Kanji: string;
    RusNick: string;
    RusNicks: TStringArray;
    RawRusNick: string;
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

  TYarxiDB = class
  protected
    Db: TSqliteDb;
  public
    KanaTran: TRomajiTranslator;
    Kanji: array of TKanjiRecord;
    Tango: array of TTangoRecord;
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    procedure ParseKanji;
    procedure ParseTango;
    function FindKanjiIndex(const AChar: string): integer;
    function FindKanji(const AChar: string): PKanjiRecord;


  end;

implementation

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
  KanaTran := TKanaTranslator.Create;
end;

destructor TYarxiDB.Destroy;
begin
  FreeAndNil(KanaTran);
  FreeAndNil(Db);
  inherited;
end;

procedure TYarxiDB.ParseKanji;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
  k: TKanjiRecord;
  i: integer;
begin
  ds := Db.Query('SELECT * FROM Kanji');
  SetLength(Kanji, 7000); //should be enough
  RecCount := 0;
  for rec in ds do begin
    k.Kanji := WideChar(word(rec.Uncd));
    k.RawRusNick := DecodeRussian(rec.RusNick);
    k.RusNicks := DecodeKanjiRusNick(k.RawRusNick);
    k.RusNick := StripAlternativeRusNicks(k.RawRusNick);
    k.OnYomi := SplitOnYomi(rec.OnYomi);
   //ѕреобразуем после сплита, т.к. может затронуть разметочные символы типа тире
    for i := 0 to Length(k.OnYomi)-1 do
      k.OnYomi[i].kana := KanaTran.RomajiToKana(k.OnYomi[i].kana, []);
    k.KunYomi := rec.KunYomi;
    k.Russian := DecodeRussian(rec.Russian);
    k.Compounds := rec.Compounds;
    Kanji[RecCount] := k;
    Inc(RecCount);
  end;
  SetLength(Kanji, RecCount); //trim
end;

procedure TYarxiDB.ParseTango;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
  k: TTangoRecord;
begin
  ds := Db.Query('SELECT * FROM Tango');
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
end;

function TYarxiDB.FindKanjiIndex(const AChar: string): integer;
var i: integer;
begin
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

function TYarxiDB.FindKanji(const AChar: string): PKanjiRecord;
var idx: integer;
begin
  idx := FindKanjiIndex(AChar);
  if idx<0 then begin
    Result := nil;
    exit;
  end else
    Result := @Kanji[idx];
end;

end.
