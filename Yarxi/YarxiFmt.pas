unit YarxiFmt;
{ Форматы, используемые в базе данных Яркси. Перед чтением убедитесь, что рядом
 нет женщин и детей. }

interface
uses SysUtils, Classes, UniStrUtils, FastArray;

{ Во всех полях используется "обезъяний русский":
    <a   =>   a
    <b   =>   б
    <c   =>   в

Обычно всё пишется маленькими, а заглавная буква первой делается автоматически.
Но если записано имя собственное, то заглавная прописывается явно.
Заглавные получаются из обычных так: <c -> <C. Для специальных букв заглавные
смотри ниже по табличке.
Цифры не кодируются.
}

function DecodeRussian(const inp: string): string;


{
Поле Kanji.RusNick:
1. Несколько вариантов:
  человеколюбие*косточка*
2. Альтернативные (скрытые) записи: *#*
  дешёвый*#*дешевый*
 Если вариантов несколько, альтернатива ставится ко всему набору:
  лёгкий*обмен*#*легкий*обмен**  --- две звёздочки в конце
3. Пробелы стандартно:
  вишнёвая берёза*#*вишневая береза*
4. Перенос строк: *_*
  речь*_*различать*_*лепесток*косичка*_*управление*
  По умолчанию Яркси каждый вариант пишет новой строкой, но там, где стоит _,
  строка не переносится.
5. Ударения
  замок*!2  --- ударение на 2-й букве
  ах!  --- просто восклицательный знак
6. Курсив? (для названий радикалов)
  ''капля''
  ''обёртка''
  ''лёд''*#*''лед''  --- совместно с альтернативой
  императорское ''мы''  --- просто кавычки

Ещё встречается:
  -ха-  --- ничего не значит, отображается, как есть
}
type
  TRusNicks = TArray<string>;

function DecodeKanjiRusNick(const inp: string): TRusNicks;

{
Поле Kanji.OnYomi:
Формат: *kana*;*kana**;*kana*
Если в конце две звёздочки, чтение малоупотребляемое.
Строка пустая или стоит тире - кокудзи.
}
type
  TOnYomiEntry = record
    kana: string; //если пустое => кокудзи
    rare: boolean;
  end;
  POnYomiEntry = ^TOnYomiEntry;
  TOnYomiEntries = array of TOnYomiEntry; //пустой => кокудзи

function SplitOnYomi(const inp: string): TOnYomiEntries;

implementation
uses StrUtils;

{ Заменяет весь обезъяний русский в тексте нормальными русскими буквами. }
function DecodeRussian(const inp: string): string;
const
  eng: string = 'abcdefghijklmnopqrstuvwxyz1234567ABCDEFGHIJKLMNOPQRSTUVWXYZ890!?=+';
  rus: string = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
var pc, po: PChar;
  i: integer;
  found: boolean;
begin
  if inp='' then begin
    Result := '';
    exit;
  end;

  Result := '';
  SetLength(Result, Length(inp)); //not going to be bigger than that

  pc := PChar(@inp[1]);
  po := PChar(@Result[1]);
  while pc^<>#00 do begin
    if pc^='<' then begin
      Inc(pc);
      found := false;
      for i:= 1 to Length(eng) do
        if eng[i]=pc^ then begin
          po^ := rus[i];
          found := true;
          break;
        end;
      if not found then begin
        po^ := '<';
        Dec(pc);
      end;
    end else
      po^ := pc^;
    Inc(pc);
    Inc(po);
  end;
  po^ := #00;
  SetLength(Result, StrLen(PChar(Result))); //trim
end;


function KillQuotes(const inp: string): string;
begin
  if (Length(inp)<4) or (inp[1]<>'''') or (inp[2]<>'''')
  or (inp[Length(inp)-1]<>'''') or (inp[Length(inp)]<>'''') then
    Result := inp
  else
    Result := copy(inp, 3, Length(inp)-4);
end;

{ Разбирает строку и составляет список всех названий кандзи.
 Альтернативы выбрасывает. Флаги непереноса строки выбрасывает. Акценты выбрасывает.
 Можно было бы разобрать, но НААААФИИИГ. }
function DecodeKanjiRusNick(const inp: string): TRusNicks;
var tmp: string;
  i_pos: integer;
  i, i_start: integer;
begin
  Result.Clear;
  tmp := inp;

 //Альтернативы нафиг
  i_pos := pos('*#*',tmp);
  if i_pos>0 then
    delete(tmp,i_pos,MaxInt);

  tmp := UniReplaceStr(tmp, '*_*', '*');
  tmp := UniReplaceStr(tmp, '**', '*');

 //Звёздочки в конце
  while (Length(tmp)>0) and (tmp[Length(tmp)]='*') do
    SetLength(tmp, Length(tmp)-1);

  if tmp='' then exit;

  i := 1;
  i_start := 1;
  while i<=Length(tmp) do begin
    if tmp[i]='*' then begin
      if i_start<i then
        Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)));
      i_start := i+1;
    end;
    Inc(i);
  end;

  if i>i_start then
    Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)))
end;


function SplitOnYomi(const inp: string): TOnYomiEntries;
var i_beg, i_pos, cnt: integer;

  procedure PostWord;
  var i_end: integer;
  begin
    if (i_pos-i_beg<=0) or ((i_pos-i_beg=1) and (inp[i_beg]='-')) then begin
      Result[cnt].kana := ''; //kokuji
      Result[cnt].rare := false;
    end else
   { Вообще говоря, судя по формату, каждое он-ёми должно быть обёрнуто в
    *звёздочки*, и исключений быть не может.
    Но они есть. Так что будем очень терпимы. }
    begin
      i_end := i_pos;
      Result[cnt].rare := false;
      if inp[i_beg]='*' then
        Inc(i_beg);
      if inp[i_end-1]='*' then
        Dec(i_end);
      if (i_end-1>=i_beg) and (inp[i_end-1]='*') then begin
        Result[cnt].rare := true;
        Dec(i_end);
      end;
      Result[cnt].kana := copy(inp, i_beg, i_end-i_beg);
    end;
  end;

begin
  if (inp='') or (inp='-') then begin
    SetLength(Result, 0);
    exit;
  end;

 //Считаем число ;,
  cnt := 0;
  for i_pos := 1 to Length(inp) do
    if (inp[i_pos]=';') or (inp[i_pos]=',') then
      Inc(cnt);
  SetLength(Result, cnt+1);

  cnt := 0;
  i_beg := 1;
  i_pos := 1;
  while i_pos<=Length(inp) do begin
    if (inp[i_pos]=';') or (inp[i_pos]=',') then begin
      PostWord;
      i_beg := i_pos+1;
      Inc(cnt);
    end;
    Inc(i_pos);
  end;
  PostWord;
end;

end.
