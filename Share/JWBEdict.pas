unit JWBEdict;
{ Basic in-memory EDICT representation. Quite slow in loading and quite heavy
 in keeping around, but fast and works well if you just need a simple task done.
 For daily use, consider converting EDICT to some sort of database format.
 No deflexion support. }

{$DEFINE NO_KANA_KANJI_LINK}
{ Speeds up load and simplifies things, but ignores the info on which kana can
 be used with which kanji.
 I'll maybe delete this option when I figure out if it really slows things down
 or adds to memory, and how to mitigate that. }

interface
uses SysUtils, Classes, JWBIO, JWBEdictReader, JWBEdictMarkers, BalancedTree;

type
 { We can't use the same entries as EdictReader as those are optimized for reuse
  and very memory heavy }
  TKanjiEntry = JWBEdictReader.TKanjiEntry; //this one is ok
  PKanjiEntry = JWBEdictReader.PKanjiEntry;
  TKanaEntry = record
    kana: UnicodeString;
   {$IFNDEF NO_KANA_KANJI_LINK}
    kanji: array of PKanjiEntry;
   {$ENDIF}
    markers: TMarkers;
  end;
  TSenseEntry = JWBEdictReader.TSenseEntry;
  PSenseEntry = JWBEdictReader.PSenseEntry;

  TEdictEntry = record
    ref: string;
    kanji: array of TKanjiEntry;
    kana: array of TKanaEntry;
    senses: array of TSenseEntry;
    pop: boolean;
    function FindKanjiEntry(const AKanji: UnicodeString): PKanjiEntry;
    function AllSenses: string;
  end;
  PEdictEntry = ^TEdictEntry;

 { Only TExprItems are allowed to be in the tree. FExpr must not be nil.
  Only PWideChar must be passed to CompareData. }
  TExprItem = class(TBinTreeItem)
  protected
    FEntry: PEdictEntry;
    FExpr: string; //hopefully shared with KanjiEntry
  public
    constructor Create(AEntry: PEdictEntry; const AExpr: string);
    function CompareData(const a):Integer; override;
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
    procedure List; override;
  end;

  TSourceFormat = (sfEdict, sfCEdict);
  TEdict = class
  protected
    FEntries: array of TEdictEntry;
    FEntryCount: integer;
    FExprTree: TBinTree;
    function AllocEntry: PEdictEntry;
    procedure RegisterEntry(const AEntry: PEdictEntry);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const AFilename: string;
      ASourceFormat: TSourceFormat = sfEdict);
    procedure LoadFromStream(const AStream: TStream;
      ASourceFormat: TSourceFormat = sfEdict);
    procedure Load(AInput: TStreamDecoder; ASourceFormat: TSourceFormat);
    function FindEntry(const expr: UnicodeString): PEdictEntry;
    property EntryCount: integer read FEntryCount;
  end;

implementation
uses WideStrUtils;

function TEdictEntry.FindKanjiEntry(const AKanji: UnicodeString): PKanjiEntry;
var i: integer;
begin
  Result := nil;
  for i := 0 to Length(kanji) do
    if kanji[i].kanji=AKanji then begin
      Result := @kanji[i];
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
  Self.FEntry := AEntry;
  Self.FExpr := AExpr;
end;

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
  TExprItem(ToA).FEntry := Self.FEntry;
  TExprItem(ToA).FExpr := Self.FExpr;
end;

procedure TExprItem.List;
begin
 //we don't support no listing
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
    FExprTree.Add(TExprItem.Create(AEntry, AEntry.kanji[i].kanji));
  for i := 0 to Length(AEntry.kana)-1 do
    FExprTree.Add(TExprItem.Create(AEntry, AEntry.kana[i].kana));
end;

{ Searches for entry through kanji and kana indexes }
function TEdict.FindEntry(const expr: UnicodeString): PEdictEntry;
var item: TBinTreeItem;
begin
  item := FExprTree.SearchData(expr);
  if item=nil then
    Result := nil
  else
    Result := TExprItem(item).FEntry;
end;

end.
