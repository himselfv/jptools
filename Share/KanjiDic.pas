unit KanjiDic;
{ Basic in-memory KanjiDic representation, heavy and slow. If you need anything
 fancy, write your own engine and use KanjiDicReader to load data.

 Usage:
   kd := TKanjidic.Create;
   kd.LoadFromFile('kanjidic');
   common_ons := kd.FindEntry(char).readings[0].ons.Join(', ');
 }

//{$DEFINE STATIC_ENTRIES}
{ If set, preallocates space for all entries in the dictionary and does not
 allow adding entries dynamically -- faster. }

interface
uses SysUtils, Classes, JWBIO, BalancedTree, FastArray, KanjiDicReader;

type
 { Char lookups are indexed. We may add other indexes as the need arises }
  TCharItem = class(TBinTreeItem)
  protected
    FEntry: PKanjiDicEntry;
    FChar: UnicodeString; //hopefully shared with the entry
  public
    constructor Create(AEntry: PKanjiDicEntry; const AChar: UnicodeString);
    function CompareData(const a):Integer; override;
    function Compare(a:TBinTreeItem):Integer; override;
    procedure Copy(ToA:TBinTreeItem); override;
  end;

  TKanjiDic = class
  protected
   {$IFDEF STATIC_ENTRIES}
    FEntries: TArray<TKanjiDicEntry>;
   {$ELSE}
    FEntries: TArray<PKanjiDicEntry>;
   {$ENDIF}
    FCharTree: TBinTree;
    function AllocEntry: PKanjiDicEntry;
    procedure RegisterEntry(const AEntry: PKanjiDicEntry);
    function GetEntryCount: integer; inline;
    function GetEntry(const AIndex: integer): PKanjiDicEntry;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(const AFilename: string);
    procedure LoadFromStream(const AStream: TStream);
    procedure Load(AInput: TStreamDecoder);
    function FindEntry(const AChar: UnicodeString): PKanjiDicEntry;
    property EntryCount: integer read GetEntryCount;
    property Entries[const Index: integer]: PKanjiDicEntry read GetEntry;
  end;

implementation
uses WideStrUtils;

constructor TCharItem.Create(AEntry: PKanjiDicEntry; const AChar: UnicodeString);
begin
  inherited Create;
  Self.FEntry := AEntry;
  Self.FChar := AChar;
end;

//Accepts only PWideChar
function TCharItem.CompareData(const a):Integer;
begin
  Result := WStrComp(PWideChar(Self.FChar), PWideChar(a));
end;

function TCharItem.Compare(a:TBinTreeItem):Integer;
begin
  Result := CompareData(TCharItem(a).FChar);
end;

procedure TCharItem.Copy(ToA:TBinTreeItem);
begin
  TCharItem(ToA).FEntry := Self.FEntry;
  TCharItem(ToA).FChar := Self.FChar;
end;

constructor TKanjiDic.Create;
begin
  inherited;
  FCharTree := TBinTree.Create;
end;

destructor TKanjiDic.Destroy;
begin
  FreeAndNil(FCharTree);
  inherited;
end;

procedure TKanjiDic.Clear;
var i: integer;
  ptr: PKanjiDicEntry;
begin
  FCharTree.Clear;
 {$IFNDEF STATIC_ENTRIES}
  for i := FEntries.Length-1 downto 0 do begin
    ptr := PKanjiDicEntry(FEntries.GetPointer(i));
    Dispose(ptr);
  end;
 {$ENDIF}
  FEntries.Length := 0;
end;

procedure TKanjiDic.LoadFromFile(const AFilename: string);
var inp: TStreamDecoder;
begin
  inp := OpenTextFile(AFilename);
  try
    Load(inp);
  finally
    FreeAndNil(inp);
  end;
end;

procedure TKanjiDic.LoadFromStream(const AStream: TStream);
var inp: TStreamDecoder;
begin
  inp := OpenStream(AStream, {owns=}false);
  try
    Load(inp);
  finally
    FreeAndNil(inp);
  end;
end;

procedure TKanjiDic.Load(AInput: TStreamDecoder);
var ed: PKanjiDicEntry;
  ln: string;
 {$IFDEF STATIC_ENTRIES}
  rec_count: integer;
  c: char;
 {$ENDIF}
begin
  Clear;

 {$IFDEF STATIC_ENTRIES}
 { We keep a list of records directly, so every reallocation will break pointers.
  Let's keep things simple and just allocate memory once.
  For that, count the (maximal possible) number of records. }
  rec_count := 0;
  while AInput.ReadChar(c) do
    if c=#$0A then Inc(rec_count);
  Inc(rec_count,10); //we're generous
  FEntries.Prealloc(rec_count);
 {$ELSE}
 //With dynamic allocation there's no need to scan the file.
 {$ENDIF}

  AInput.Rewind();
  while AInput.ReadLn(ln) do begin
    ln := Trim(ln);
    if ln='' then continue;
    if IsKanjidicComment(ln) then
      continue;
    ed := AllocEntry;
    ParseKanjiDicLine(ln, ed);
    RegisterEntry(ed);
  end;
end;

function TKanjiDic.AllocEntry: PKanjiDicEntry;
begin
 {$IFDEF STATIC_ENTRIES}
 { Entries are preallocated in Load() and must not be allocated on-the-fly,
  or pointers will break everywhere. }
  if FEntries.Length>=FEntries.PreallocatedLength then
    raise Exception.Create('Cannot allocate one more entry.');
  Result := PKanjiDicEntry(FEntries.AddNew);
 {$ELSE}
  New(Result);
  FEntries.Add(Result);
 {$ENDIF}
end;

procedure TKanjiDic.RegisterEntry(const AEntry: PKanjiDicEntry);
begin
  FCharTree.Add(TCharItem.Create(AEntry, AEntry.kanji));
end;

function TKanjiDic.GetEntryCount: integer;
begin
  Result := FEntries.Length;
end;

function TKanjiDic.GetEntry(const AIndex: integer): PKanjiDicEntry;
begin
 {$IFDEF STATIC_ENTRIES}
  Result := FEntries.P[AIndex];
 {$ELSE}
  Result := FEntries[AIndex];
 {$ENDIF}
end;

{ Searches for a given entry }
function TKanjiDic.FindEntry(const AChar: UnicodeString): PKanjiDicEntry;
var item: TBinTreeItem;
begin
  item := FCharTree.SearchData(AChar);
  if item=nil then
    Result := nil
  else
    Result := TCharItem(item).FEntry;
end;

end.
