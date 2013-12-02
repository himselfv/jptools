unit FastArray;
{ Array wrapper to support predictable memory preallocation.
 Used in various format readers.

 Do not use direct arrays (TArray<TSomeRecord>) if your structure is sufficiently
 complicated.
 1. TArray<T> will realloc all the data => slow.
 2. Any internal links between parts of the structure will be broken,
   resulting in hard-to-track bugs.
 Use pointer arrays (TArray<PSomeRecord>) instead everywhere except for
 the bottom level (strings, simple records). }

interface

type
  TPredicate<T> = reference to function(const item: T): boolean;
  TComparison<T> = function(const a,b: T): integer;

  TArray<T> = record
  type
    PT = ^T;
  public
    FItems: array of T;
    FItemCount: integer;
    FComparison: TComparison<T>;
    procedure Clear; inline;
    procedure Reset; inline; //same as Clear
    function GetItem(const AIndex: integer): T; inline;
    procedure SetItem(const AIndex: integer; const AItem: T); inline;
    function GetPointer(const AIndex: integer): PT; inline;
    procedure SetPointer(const AIndex: integer; const AValue: PT); inline;
    procedure Add(const AItem: T);
    function AddNew: PT;
    procedure AddUnique(const item: T);
    function Find(const item: T): integer; overload;
    function Find(pred: TPredicate<T>): integer; overload;
    function Find(pred: TPredicate<PT>): integer; overload;
    procedure SetLength(const ALength: integer);
    procedure Prealloc(const ACount: integer);
    function GetPreallocatedLength: integer; inline;
    property Comparison: TComparison<T> read FComparison write FComparison;
    property Length: integer read FItemCount write SetLength;
    property Count: integer read FItemCount write SetLength; //same
    property PreallocatedLength: integer read GetPreallocatedLength;
    property Items[const AIndex: integer]: T read GetItem write SetItem; default;
    property P[const AIndex: integer]: PT read GetPointer write SetPointer;
  end;

function Join(const AArray: TArray<string>; const ASep: string): string;

implementation

procedure TArray<T>.Clear;
begin
  FItemCount := 0;
end;

procedure TArray<T>.Reset;
begin
  Clear;
end;

function TArray<T>.GetItem(const AIndex: integer): T;
begin
  Result := FItems[AIndex];
end;

procedure TArray<T>.SetItem(const AIndex: integer; const AItem: T);
begin
  FItems[AIndex] := AItem;
end;

function TArray<T>.GetPointer(const AIndex: integer): PT;
begin
  Result := @FItems[AIndex];
end;

procedure TArray<T>.SetPointer(const AIndex: integer; const AValue: PT);
begin
  FItems[AIndex] := AValue^;
end;

procedure TArray<T>.Add(const AItem: T);
begin
  Self.Length := Self.Length + 1;
  Self.FItems[Self.Length-1] := AItem;
end;

function TArray<T>.AddNew: PT;
begin
  Self.Length := Self.Length + 1;
  Result := @FItems[Self.Length-1];
end;

procedure TArray<T>.AddUnique(const item: T);
begin
  if Find(item)<0 then Add(item);
end;

procedure TArray<T>.SetLength(const ALength: integer);
begin
  FItemCount := ALength;
  if FItemCount=0 then exit; //do not preallocate on SetLength(0)
  if FItemCount>System.Length(FItems) then
    System.SetLength(FItems, FItemCount*2+5);
end;

procedure TArray<T>.Prealloc(const ACount: integer);
begin
  if ACount>System.Length(FItems) then
    System.SetLength(FItems, ACount);
end;

function TArray<T>.GetPreallocatedLength: integer;
begin
  Result := System.Length(FItems);
end;

function TArray<T>.Find(const item: T): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Length - 1 do
    if Comparison(item,items[i])=0 then begin
      Result := i;
      exit;
    end;
end;

function TArray<T>.Find(pred: TPredicate<T>): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Length - 1 do
    if pred(items[i]) then begin
      Result := i;
      exit;
    end;
end;

function TArray<T>.Find(pred: TPredicate<PT>): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Self.Length - 1 do
    if pred(@FItems[i]) then begin
      Result := i;
      exit;
    end;
end;

//Adds the items together, interleaving the additions with ASep
function Join(const AArray: TArray<string>; const ASep: string): string;
var i: integer;
begin
  case AArray.Length of
   0: Result := Default(string);
   1: Result := AArray.FItems[0];
  else
    Result := AArray.FItems[0];
    for i := 1 to AArray.Length-1 do
      Result := Result + ASep + AArray.FItems[i];
  end;
end;

end.
