unit WcUtils;

interface

function repl(const s:string;const sub,repl:string):string;
function join(const a:array of string; const sep: string): string;
function countc(const s:string;const c:char): integer;

implementation

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

//Число вхождений символа в строку
function countc(const s:string;const c:char): integer;
var i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    if s[i]=c then Inc(Result);
end;


end.
