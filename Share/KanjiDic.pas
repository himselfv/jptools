unit KanjiDic;
{ Basic in-memory KanjiDic representation, heavy and slow. If you need anything
 fancy, write your own engine and use KanjiDicReader to load data.

 Usage:
   kd := TKanjidic.Create;
   kd.LoadFromFile('kanjidic');
   common_ons := kd.FindEntry(char).readings[0].ons.Join(', ');
 }

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
    FEntries: TArray<PKanjiDicEntry>;
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
  for i := FEntries.Length-1 downto 0 do begin
    ptr := PKanjiDicEntry(FEntries.GetPointer(i));
    Dispose(ptr);
  end;
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
begin
  Clear;

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
  New(Result);
  FEntries.Add(Result);
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
  Result := FEntries[AIndex];
end;

{ Searches for a given entry }
function TKanjiDic.FindEntry(const AChar: UnicodeString): PKanjiDicEntry;
var item: TBinTreeItem;
begin
  if AChar='' then begin
    Result := nil;
    exit;
  end;
  item := FCharTree.SearchData(AChar);
  if item=nil then
    Result := nil
  else
    Result := TCharItem(item).FEntry;
end;

end.
