unit WcUtils;

interface

type
  TPredicate<T> = reference to function(const item: T): boolean;
  TComparison<T> = function(const a,b: T): integer;

  TList<T> = record
  type
    PT = ^T;
  public
    items: array of T;
    used: integer;
    Comparison: TComparison<T>;
    procedure Reset;
    function GetPtr(Index: integer): PT;
    function Add(const item: T): PT;
    procedure AddUnique(const item: T);
    function Find(const item: T): integer; overload;
    function Find(pred: TPredicate<T>): integer; overload;
    function Find(pred: TPredicate<PT>): integer; overload;
    property Count: integer read used;
  end;

function repl(const s:string;const sub,repl:string):string;
function join(const a:array of string; const sep: string): string;

implementation

procedure TList<T>.Reset;
begin
  used := 0;
end;

function TList<T>.GetPtr(Index: integer): PT;
begin
  Result := @items[Index];
end;

function TList<T>.Add(const item: T): PT;
begin
  if used>=Length(items) then
    SetLength(items, Length(items)+Trunc(Length(items)*0.5)+4);
  Result := @items[used];
  Result^ := item;
  Inc(used);
end;

procedure TList<T>.AddUnique(const item: T);
begin
  if Find(item)<0 then Add(item);
end;

function TList<T>.Find(const item: T): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Comparison(item,items[i])=0 then begin
      Result := i;
      exit;
    end;
end;

function TList<T>.Find(pred: TPredicate<T>): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if pred(items[i]) then begin
      Result := i;
      exit;
    end;
end;

function TList<T>.Find(pred: TPredicate<PT>): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if pred(@items[i]) then begin
      Result := i;
      exit;
    end;
end;



function repl(const s:string;const sub,repl:string):string;
begin
  Result := s;
  while pos(sub,Result)>0 do
    Result:=copy(Result,1,pos(sub,Result)-1)+repl+copy(Result,pos(sub,Result)+length(sub),length(Result)-pos(sub,Result)+1-length(sub));
end;

function join(const a:array of string; const sep: string): string;
var i: integer;
begin
  if Length(a)<=0 then
    Result := ''
  else
    Result := a[0];
  for i := 1 to Length(a) - 1 do
    Result := Result + sep + a[i];
end;


end.
