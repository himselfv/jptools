unit Yarxi;
{
Usage:
  Yarxi := TYarxiDB.Create('yarxi.db');
  Yarxi.KanaTran.LoadFromFile('yarxi.kcs');
}

//{$DEFINE USE_DB}
{ Do not preload data, just query the DB. Better for small number of queries,
 but if you're doing 1000+ lookups, it'd be faster to preload. }


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
    function ParseKanji(const Rec: variant): TKanjiRecord;
    function ParseTango(const Rec: variant): TTangoRecord;

 {$IFNDEF USE_DB}
  protected
    Kanji: array of TKanjiRecord;
    Tango: array of TTangoRecord;
    procedure LoadKanji;
    procedure LoadTango;
    function FindKanjiIndex(const AChar: string): integer; //do not expose
 {$ENDIF}

  public
    KanaTran: TRomajiTranslator;
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function GetKanji(const AChar: string; out ARec: TKanjiRecord): boolean;

  end;

function Join(const AArray: TStringArray; const sep: string=', '): string; overload;

implementation

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
  KanaTran := TKanaTranslator.Create;
end;

destructor TYarxiDB.Destroy;
begin
  FreeAndNil(KanaTran);
  FreeAndNil(Db);
  inherited;
end;

function TYarxiDB.ParseKanji(const Rec: variant): TKanjiRecord;
var i: integer;
begin
  Result.Nomer := rec.Nomer;
  Result.Kanji := WideChar(word(rec.Uncd));
  Result.RawRusNick := DecodeRussian(rec.RusNick);
  Result.RusNicks := DecodeKanjiRusNick(Result.RawRusNick);
  if Result.RusNicks.Length>0 then
    Result.RusNick := Result.RusNicks[i]
  else
    Result.RusNick := '';
  Result.OnYomi := SplitOnYomi(rec.OnYomi);
 //ѕреобразуем после сплита, т.к. может затронуть разметочные символы типа тире
  for i := 0 to Length(Result.OnYomi)-1 do
    Result.OnYomi[i].kana := KanaTran.RomajiToKana(Result.OnYomi[i].kana, []);
  Result.KunYomi := rec.KunYomi;
  Result.Russian := DecodeRussian(rec.Russian);
  Result.Compounds := rec.Compounds;
end;

function TYarxiDB.ParseTango(const Rec: variant): TTangoRecord;
begin
  Result.Kana := rec.Kana;
  Result.Reading := rec.Reading;
  Result.Russian := DecodeRussian(rec.Russian);
end;

{$IFNDEF USE_DB}
procedure TYarxiDB.LoadKanji;
var ds: TSqliteDataset;
  RecCount: integer;
  rec: variant;
begin
  if Length(Kanji)>0 then exit; //already parsed;
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
{$ENDIF}

function TYarxiDB.GetKanji(const AChar: string; out ARec: TKanjiRecord): boolean;
{$IFDEF USE_DB}
var ds: TSqliteDataset;
  charval: integer;
begin
  case Length(AChar) of
    1: charval := Word(AChar[1]);
    2: charval := Word(AChar[1]) + Word(AChar[2]) shl 16;
  else //0 and 2+ are not valid
    Result := false;
    exit;
  end;

  ds := Db.Query('SELECT * FROM Kanji WHERE Kanji.Uncd='+IntToStr(charval));
  if ds.EOF then
    Result := false
  else begin
    ARec := ParseKanji(ds.CurrentRec);
    Result := true;
  end;
end;
{$ELSE}
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
{$ENDIF}

end.
