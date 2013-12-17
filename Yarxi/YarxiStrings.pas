unit YarxiStrings;
{ Yarxi string utils. Sometime later should probably be integrated into
 common UniStrUtils -- most of them anyway }

interface
uses UniStrUtils;

function spanlen(ps, pe: PChar): integer;
function spansize(ps, pe: PChar): integer;
function spancopy(ps, pe: PChar): string;

function pop(var s: string; const sep: char): string; overload;
function pop(var pc: PChar; const sep: char): string; overload;
function trypop(var s: string; const sep: char): string; overload;
function trypop(var pc: PChar; const sep: char): string; overload;

function leq(pc: PChar; const match: string): boolean;
function eat(var pc: PChar; const match: string): boolean; overload;
function eat(var pc: PChar; const matches: array of string): integer; overload;

function repl(const s: string; const AFrom, ATo: string): string; inline;
function killc(const s: string; const chars: string): string;

function Split(const s: string; const sep: char): TStringArray; inline;

function Unquote(const s: string; op, ed: char): string;
function TryUnquote(var s: string; op, ed: char): boolean;

function IsLatin(const ch: char): boolean; inline;
function IsUpperCaseLatin(const ch: char): boolean; inline;
function IsCyrillic(const ch: char): boolean; inline;
function IsDigit(const ch: char): boolean; inline;
function IsKanji(const ch: char): boolean; inline;

function DigitToInt(const ch: char): byte; inline;

function EatNumber(var pc: PChar): integer; overload;
function EatNumber(var pc: PChar; DigitCount: integer): integer; overload;
function EatLatin(var pc: PChar): string;

function test_char(const ch: char; const chars: string): integer;


implementation
uses SysUtils, StrUtils, WcExceptions;

{ Длина в char-позициях промежутка между символами }
function spanlen(ps, pe: PChar): integer;
begin
  Result := (NativeUInt(pe)-NativeUInt(ps)) div SizeOf(char);
end;

{ Размер в байтах промежутка между символами }
function spansize(ps, pe: PChar): integer;
begin
  Result := NativeUInt(pe)-NativeUInt(ps);
end;

{ Копирует набор символов с ps по pe не включительно }
function spancopy(ps, pe: PChar): string;
var i: integer;
begin
  SetLength(Result, (NativeUInt(pe)-NativeUInt(ps)) div SizeOf(char));
  for i := 1 to Length(Result) do begin
    Result[i] := ps^;
    Inc(ps);
  end;
end;

{ Извлекает начало строки до разделителя; уничтожает разделитель. Если
 разделителя нет, извлекает остаток строки. }
function pop(var s: string; const sep: char): string;
var i: integer;
begin
  i := pos(sep, s);
  if i<=0 then begin
    Result := s;
    s := '';
  end else begin
    Result := copy(s, 1, i-1);
    delete(s, 1, i);
  end;
end;

function pop(var pc: PChar; const sep: char): string;
var ps: PChar;
begin
  ps := pc;
  while (pc^<>#00) and (pc^<>sep) do
    Inc(pc);
  Result := spancopy(ps, pc);
  if pc^=sep then Inc(pc);
end;

{ То же, но когда разделителя нет, ничего не возвращает и оставляет хвост, как
 есть. }
function trypop(var s: string; const sep: char): string;
var i: integer;
begin
  i := pos(sep, s);
  if i<=0 then
    Result := ''
  else begin
    Result := copy(s, 1, i-1);
    delete(s, 1, i);
  end;
end;

function trypop(var pc: PChar; const sep: char): string;
var ps: PChar;
begin
  ps := pc;
  while (pc^<>#00) and (pc^<>sep) do
    Inc(pc);
  if pc^=#00 then
    pc := ps
  else begin
    Result := spancopy(ps, pc);
    if pc^=sep then Inc(pc);
  end;
end;


{ Tests that Pc matches Match at the starting point }
function leq(pc: PChar; const match: string): boolean;
var pm: PChar;
begin
  pm := PChar(match);
  while pc^=pm^ do begin
    Inc(pc);
    Inc(pm);
  end;
  Result := (pm^=#00);
end;

{ Проверяет, что pc начинается с match, увеличивает pc на его длину. Возвращает
 false, если совпадения нет. }
function eat(var pc: PChar; const match: string): boolean;
begin
  Result := leq(pc, match);
  if Result then
    Inc(pc, Length(match));
end;

{ Проверяет, что pc начинается с какого-то из matches, увеличивает pc на его
 длину. Возвращает номер варианта или -1, если совпадения нет.
 Внимание: если варианты перекрывающиеся (напр. ABC, AB), начинайте с более
 длинных. }
function eat(var pc: PChar; const matches: array of string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(matches)-1 do
    if eat(pc, matches[i]) then begin
      Result := i;
      break;
    end;
end;

{ Заменяет подстроку в строке }
function repl(const s: string; const AFrom, ATo: string): string;
begin
  Result := UniReplaceStr(s, AFrom, ATo);
end;

{ Удаляет все вхождения всех указанных символов из строки. Chars не должно быть
 пустым. }
function killc(const s: string; const chars: string): string;
var pc, pk, pt: PChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  SetLength(Result, Length(s)); //больше быть не может
  pt := PChar(Result);
  pc := PChar(s);
  while pc^<>#00 do begin
    pk := PChar(chars);
    while pk^<>#00 do
      if pk^=pc^ then break else Inc(pk);
    if pk^=#00 then begin
      pt^ := pc^;
      Inc(pt);
    end;
    Inc(pc);
  end;

  SetLength(Result, spanlen(PChar(Result), pt)); //обрезаем
end;


{ Чуть более удобная обёртка для функции StrSplit }
function Split(const s: string; const sep: char): TStringArray;
begin
  Result := StrSplit(PChar(s), sep);
end;

{ Удаляет кавычки, если они есть }
function Unquote(const s: string; op, ed: char): string;
begin
  if (Length(s)>=2) and (s[1]=op) and (s[Length(s)]=ed) then
    Result := copy(s,2,Length(s)-2);
end;

function TryUnquote(var s: string; op, ed: char): boolean;
begin
  Result := (Length(s)>=2) and (s[1]=op) and (s[Length(s)]=ed);
  if Result then
    s := copy(s,2,Length(s)-2);
end;



function IsLatin(const ch: char): boolean;
begin
  Result := ((ch>='A') and (ch<='Z')) or ((ch>='a') and (ch<='z'));
end;

function IsUpperCaseLatin(const ch: char): boolean;
begin
  Result := (ch>='A') and (ch<='Z');
end;

function IsCyrillic(const ch: char): boolean;
begin
  Result := ((ch>='А') and (ch<='Я')) or ((ch>='а') and (ch<='я'))
    or (ch='Ё') or (ch='ё');
end;

function IsDigit(const ch: char): boolean;
begin
  Result := (ch>='0') and (ch<='9');
end;

//Вообще-то мы немножко просрали двухпозиционные символы... Эх!
//Слава богу, в Яркси их и близко нет. По-моему.
function IsKanji(const ch: char): boolean;
begin
  Result := ((Word(ch)>=$3400) and (Word(ch)<=$9FFF))
    or ((Word(ch)>=$F900) and (Word(ch)<=$FAFF));
end;

//Если это было не число, пеняйте на себя
function DigitToInt(const ch: char): byte;
begin
  Result := Ord(ch)-Ord('0');
end;

//Reads a positive number (only digits)
function EatNumber(var pc: PChar): integer;
var ps: PChar;
begin
  ps := pc;
  while IsDigit(pc^) do
    Inc(pc);
  Check(pc>ps);
  Result := StrToInt(spancopy(ps,pc));
end;

function EatNumber(var pc: PChar; DigitCount: integer): integer;
var ps: PChar;
begin
  ps := pc;
  while DigitCount>0 do begin
    Check(IsDigit(pc^));
    Inc(pc);
    Dec(DigitCount);
  end;
  Result := StrToInt(spancopy(ps,pc));
end;

function EatLatin(var pc: PChar): string;
var ps: PChar;
begin
  ps := pc;
  while IsLatin(pc^) do
    Inc(pc);
  Check(pc>ps);
  Result := spancopy(ps,pc);
end;

//Возвращает индекс символа в списке или 0
function test_char(const ch: char; const chars: string): integer;
begin
  Result := Length(chars);
  while (Result>0) and (ch<>chars[Result]) do
    Dec(Result);
end;


end.
