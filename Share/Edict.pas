unit Edict;
{ Basic in-memory EDICT representation. Quite slow in loading and heavy
 in keeping around, but fast and works well if you just need a simple task done.
 For daily use, consider converting EDICT to some sort of database format.
 No deflexion support. }

//{$DEFINE NO_KANA_KANJI_LINK}
{ Speeds up load and simplifies things, but ignores the info on which kana can
 be used with which kanji.
 I'll maybe delete this option when I figure out if it really slows things down
 or adds to memory, and how to mitigate that. }

interface
uses SysUtils, Classes, UniStrUtils, JWBIO, EdictReader, BalancedTree;

type
 { We can't use the same entries as EdictReader as those are optimized for reuse
  and very memory heavy }
  TKanjiEntry = EdictReader.TKanjiEntry; //this one is ok
  PKanjiEntry = EdictReader.PKanjiEntry;
  TKanaEntry = record
    kana: UnicodeString;
   {$IFNDEF NO_KANA_KANJI_LINK}
    kanji: array of PKanjiEntry;
   {$ENDIF}
    markers: TMarkers;
   {$IFNDEF NO_KANA_KANJI_LINK}
    function GetKanjiIndex(const AKanji: string): integer;
    function MatchesKanji(const AKanji: string): boolean;
   {$ENDIF}
  end;
  PKanaEntry = ^TKanaEntry;
  TSenseEntry = EdictReader.TSenseEntry;
  PSenseEntry = EdictReader.PSenseEntry;

  TEdictEntry = record
    ref: string;
    kanji: array of TKanjiEntry;
    kana: array of TKanaEntry;
    senses: array of TSenseEntry;
    pop: boolean;
    function GetKanjiIndex(const AKanji: UnicodeString): integer;
    function FindKanjiEntry(const AKanji: UnicodeString): PKanjiEntry;
    function GetKanaIndex(const AKana: UnicodeString): integer;
    function AllSenses: string;
  end;
  PEdictEntry = ^TEdictEntry;

  TEdictEntries = array of PEdictEntry;

 { FExpr must not be nil. }
  TExprItem = class(TBinTreeItem)
  protected
    FEntries: TEdictEntries;
    FExpr: string; //hopefully shared with KanjiEntry
  public
    constructor Create(AEntry: PEdictEntry; const AExpr: string);
    function CompareData(const a):Integer; override;
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
    function GetEntryIndex(const AEntry: PEdictEntry): integer;
    procedure AddEntry(const AEntry: PEdictEntry);
  end;

  TSourceFormat = (sfEdict, sfCEdict);
  TEdict = class
  protected
    FEntries: array of TEdictEntry;
    FEntryCount: integer;
    FExprTree: TBinTree;
    function AllocEntry: PEdictEntry;
    procedure RegisterEntry(const AEntry: PEdictEntry);
    procedure AddOrSearch(const AEntry: PEdictEntry; AKey: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const AFilename: string;
      ASourceFormat: TSourceFormat = sfEdict);
    procedure LoadFromStream(const AStream: TStream;
      ASourceFormat: TSourceFormat = sfEdict);
    procedure Load(AInput: TStreamDecoder; ASourceFormat: TSourceFormat);
    function FindEntry(const expr: UnicodeString): PEdictEntry; overload;
    function FindEntry(const expr, read: UnicodeString): PEdictEntry; overload;
    function FindEntries(const expr: UnicodeString): TEdictEntries; overload;
    function FindEntries(const expr: TStringArray): TEdictEntries; overload;
    property EntryCount: integer read FEntryCount;
  end;

procedure MergeEntries(var Entries: TEdictEntries; const NewEntries: TEdictEntries);

implementation
uses WideStrUtils;

{$IFNDEF NO_KANA_KANJI_LINK}
function TKanaEntry.GetKanjiIndex(const AKanji: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(kanji)-1 do
    if kanji[i].kanji=AKanji then begin
      Result := i;
      break;
    end;
end;

function TKanaEntry.MatchesKanji(const AKanji: string): boolean;
begin
  Result := (Length(kanji)<=0) or (GetKanjiIndex(AKanji)>=0);
end;
{$ENDIF}

function TEdictEntry.GetKanjiIndex(const AKanji: UnicodeString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(kanji)-1 do
    if kanji[i].kanji=AKanji then begin
      Result := i;
      break;
    end;
end;

function TEdictEntry.FindKanjiEntry(const AKanji: UnicodeString): PKanjiEntry;
var i: integer;
begin
  Result := nil;
  for i := 0 to Length(kanji)-1 do
    if kanji[i].kanji=AKanji then begin
      Result := @kanji[i];
      break;
    end;
end;

function TEdictEntry.GetKanaIndex(const AKana: UnicodeString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(kana)-1 do
    if kana[i].kana=AKana then begin
      Result := i;
      break;
    end;
end;

function TEdictEntry.AllSenses: string;
var i: integer;
begin
  if Length(Self.senses)<=0 then
    Result := ''
  else
  if Length(Self.senses)=1 then
    Result := Self.senses[0].text //no numbering
  else begin
    Result := '';
    for i := 0 to Length(Self.senses)-1 do
      Result := Result + '('+IntToStr(i+1)+') '+self.Senses[i].text+'; ';
    SetLength(Result, Length(Result)-2);
  end;
end;

constructor TExprItem.Create(AEntry: PEdictEntry; const AExpr: string);
begin
  inherited Create;
  SetLength(Self.FEntries, 1);
  Self.FEntries[0] := AEntry;
  Self.FExpr := AExpr;
end;

//Only PWideChar must be passed to CompareData.
function TExprItem.CompareData(const a):Integer;
begin
  Result := WStrComp(PWideChar(Self.FExpr), PWideChar(a));
end;

function TExprItem.Compare(a:TBinTreeItem):Integer;
begin
  Result := CompareData(TExprItem(a).FExpr);
end;

procedure TExprItem.Copy(ToA:TBinTreeItem);
begin
  TExprItem(ToA).FEntries := System.Copy(Self.FEntries);
  TExprItem(ToA).FExpr := Self.FExpr;
end;

function TExprItem.GetEntryIndex(const AEntry: PEdictEntry): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(FEntries)-1 do
    if FEntries[i]=AEntry then begin
      Result := i;
      break;
    end;
end;

procedure TExprItem.AddEntry(const AEntry: PEdictEntry);
begin
  if GetEntryIndex(AEntry)>=0 then exit;
  SetLength(FEntries, Length(FEntries)+1);
  FEntries[Length(FEntries)-1] := AEntry;
end;

constructor TEdict.Create;
begin
  inherited;
  FExprTree := TBinTree.Create;
end;

destructor TEdict.Destroy;
begin
  FreeAndNil(FExprTree);
  inherited;
end;

procedure TEdict.Clear;
begin
  FExprTree.Clear;
  SetLength(FEntries, 0);
  FEntryCount := 0;
end;

procedure TEdict.LoadFromFile(const AFilename: string;
  ASourceFormat: TSourceFormat = sfEdict);
var inp: TStreamDecoder;
begin
  inp := OpenTextFile(AFilename);
  try
    Load(inp, ASourceFormat);
  finally
    FreeAndNil(inp);
  end;
end;

procedure TEdict.LoadFromStream(const AStream: TStream;
  ASourceFormat: TSourceFormat = sfEdict);
var inp: TStreamDecoder;
begin
  inp := OpenStream(AStream, {owns=}false);
  try
    Load(inp, ASourceFormat);
  finally
    FreeAndNil(inp);
  end;
end;

procedure TEdict.Load(AInput: TStreamDecoder; ASourceFormat: TSourceFormat);
var ed: TEdictArticle;
  entry: PEdictEntry;
  rec_count: integer;
  c: char;
  i: integer;
 {$IFNDEF NO_KANA_KANJI_LINK}
  j: integer;
 {$ENDIF}
  ln: string;
begin
  Clear;

 { We keep a list of records directly, so every reallocation will break pointers.
  Let's keep things simple and just allocate memory once.
  For that, count the (maximal possible) number of records. }
  rec_count := 0;
  while AInput.ReadChar(c) do
    if c=#$0A then Inc(rec_count);
  Inc(rec_count,10); //we're generous
  SetLength(FEntries, rec_count);

  AInput.Rewind();
  while AInput.ReadLn(ln) do begin
    ln := Trim(ln);
    if ln='' then continue;

    case ASourceFormat of
      sfEdict: ParseEdict2Line(ln, @ed);
      sfCEdict: ParseCCEdictLine(ln, @ed);
    end;
    entry := AllocEntry;

   //Copy entry
    entry.ref := ed.ref;
    SetLength(entry.kanji, ed.kanji_used);
    for i := 0 to ed.kanji_used-1 do
      entry.kanji[i] := ed.kanji[i];
    SetLength(entry.kana, ed.kana_used);
    for i := 0 to ed.kana_used-1 do begin
      entry.kana[i].kana := ed.kana[i].kana;
      entry.kana[i].markers := ed.kana[i].markers;
     {$IFNDEF NO_KANA_KANJI_LINK}
      SetLength(entry.kana[i].kanji, ed.kana[i].kanji_used);
      for j := 0 to ed.kana[i].kanji_used-1 do
        entry.kana[i].kanji[j] := entry.FindKanjiEntry(ed.kana[i].kanji[j]);
       //TODO: Handle the case where it's nil
     {$ENDIF}
    end;
    SetLength(entry.senses, ed.senses_used);
    for i := 0 to ed.senses_used-1 do
      entry.senses[i] := ed.senses[i];
    entry.pop := ed.pop;

    RegisterEntry(entry);
  end;
end;

function TEdict.AllocEntry: PEdictEntry;
begin
 { Someday we might want to do an automatic reallocation here in AllocEntry,
  but this is not that day -- see comments in Load() }
  if FEntryCount>=Length(FEntries)-1 then
    raise Exception.Create('Cannot allocate one more entry.');
  Result := @FEntries[FEntryCount];
  Inc(FEntryCount);
end;

{ Adds entry to various indexes }
procedure TEdict.RegisterEntry(const AEntry: PEdictEntry);
var i: integer;
begin
  for i := 0 to Length(AEntry.kanji)-1 do
    AddOrSearch(AEntry, AEntry.kanji[i].kanji);
  for i := 0 to Length(AEntry.kana)-1 do
    AddOrSearch(AEntry, AEntry.kana[i].kana);
end;

procedure TEdict.AddOrSearch(const AEntry: PEdictEntry; AKey: string);
var item, exitem: TExprItem;
begin
  item := TExprItem.Create(AEntry, AKey);
  exitem := TExprItem(FExprTree.AddOrSearch(item));
  if exitem<>item then begin
    FreeAndNil(item);
    exitem.AddEntry(AEntry);
  end;
end;

{ Searches for the first entry matching expr either as kanji or kana }
function TEdict.FindEntry(const expr: UnicodeString): PEdictEntry;
var item: TBinTreeItem;
begin
  item := FExprTree.SearchData(expr);
  if item=nil then
    Result := nil
  else
    Result := TExprItem(item).FEntries[0]; //guaranteed to have at least one
end;

{ Searches for the first entry matching exactly this expression and reading }
function TEdict.FindEntry(const expr, read: UnicodeString): PEdictEntry;
var item: TExprItem;
  i, ki: integer;
begin
  item := TExprItem(FExprTree.SearchData(expr));
  if item=nil then begin
    Result := nil;
    exit;
  end;

  Result := nil;
  for i := 0 to Length(item.FEntries)-1 do begin
    if item.FEntries[i].GetKanjiIndex(expr)<0 then continue; //no kanji
    ki := item.FEntries[i].GetKanaIndex(read);
    if ki<0 then continue; //no kana
    if not item.FEntries[i].kana[ki].MatchesKanji(expr) then
      continue; //no kanji-kana match, although this is rare I guess
    Result := item.FEntries[i];
    break;
  end;
end;

{ Searches for all entries matching expr either as kanji or kana }
function TEdict.FindEntries(const expr: UnicodeString): TEdictEntries;
var item: TBinTreeItem;
begin
  item := FExprTree.SearchData(expr);
  if item=nil then begin
    SetLength(Result, 0);
    exit;
  end;

  Result := System.Copy(TExprItem(item).FEntries);
end;

{ Locates given entry index in an array of entries }
function GetEntryIndex(const entries: TEdictEntries; const entry: PEdictEntry): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(entries)-1 do
    if entries[i]=entry then begin
      Result := i;
      break;
    end;
end;

procedure MergeEntries(var Entries: TEdictEntries; const NewEntries: TEdictEntries);
var i: integer;
begin
  for i := 0 to Length(NewEntries)-1 do begin
    if GetEntryIndex(Entries, NewEntries[i])>=0 then
      continue;
    SetLength(Entries, Length(Entries)+1);
    Entries[Length(Entries)-1] := NewEntries[i];
  end;
end;

{ Returns all unique entries which match given expression }
function TEdict.FindEntries(const expr: TStringArray): TEdictEntries;
var i: integer;
  entries: TEdictEntries;
begin
  SetLength(Result, 0);
  for i := 0 to Length(expr)-1 do begin
    entries := FindEntries(expr[i]);
    MergeEntries(Result, entries);
  end;
end;

end.
