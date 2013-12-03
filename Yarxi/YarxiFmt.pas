unit YarxiFmt;
{ Форматы, используемые в базе данных Яркси. Перед чтением убедитесь, что рядом
 нет женщин и детей. }

{$DEFINE STRICT}
{ Допускать только те вольности в формате, которые действительно встречались
 в базе Яркси. Рекомендуется.
 Без этого парсер старается быть терпимым к ошибкам. }

interface
uses SysUtils, Classes, UniStrUtils, FastArray, WcExceptions;

{ Полезные функции для работы со строками }

function spancopy(ps, pe: PChar): string;
function pop(var s: string; const sep: char): string; overload;
function pop(var pc: PChar; const sep: char): string; overload;
function trypop(var s: string; const sep: char): string; overload;
function trypop(var pc: PChar; const sep: char): string; overload;

function leq(pc: PChar; const match: string): boolean;
function eat(var pc: PChar; const match: string): boolean; overload;
function eat(var pc: PChar; const matches: array of string): integer; overload;

function repl(const s: string; const AFrom, ATo: string): string; inline;
function Split(const s: string; const sep: char): TStringArray; inline;
function Unquote(const s: string; op, ed: char): string;
function TryUnquote(var s: string; op, ed: char): boolean;

function IsLatin(const ch: char): boolean; inline;
function IsUpperCaseLatin(const ch: char): boolean; inline;
function IsNumeric(const ch: char): boolean; inline;

function EatNumber(var pc: PChar): integer;// inline;
function EatLatin(var pc: PChar): string;// inline;


{ Функции посылают сюда жалобы на жизнь. В дальнейшем надо сделать нормальный
 сборщик жалоб, как в WarodaiConvert. }

var
  ComplainContext: string;
 //добавляется ко всем жалобам. Внешние функции могут записывать сюда
 //номер и/или содержание текущей записи

procedure PushComplainContext(const AData: string);
procedure PopComplainContext;
procedure Complain(const msg: string); inline; overload;
procedure Complain(const msg, data: string); inline; overload;


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
Формат:  человеколюбие*косточка*вишнёвая берёза
*#* - альтернативные записи (ставятся ко всему набору):
  дешёвый*#*дешевый*
  лёгкий*обмен*#*легкий*обмен**  --- две звёздочки в конце
*_* - там, где стоит, строка не переносится:
  речь*_*различать*_*лепесток*косичка*_*управление*
Ударения:
  замок*!2  --- ударение на 2-й букве
  ах!  --- просто восклицательный знак
Курсив? (для названий радикалов):
  ''капля''
  ''лёд''*#*''лед''  --- совместно с альтернативой
  императорское ''мы''  --- просто кавычки

Ещё встречается:
  -ха-  --- ничего не значит, отображается, как есть
}
type
  TRusNicks = TArray<string>;

function ParseKanjiRusNick(const inp: string): TRusNicks;

{
Поле Kanji.OnYomi:
Формат: *kana*;*kana**;*kana*
Могут использоваться ";" и ",".
Флаги:
 *kana**   чтение малоупотребимое
 -         кокудзи (в т.ч. как один из вариантов)
Всё поле пустое => кокудзи.
Встречается такое (сейчас не обрабатывается):
  *cho:*(*ten**)*
  *ryu:*(*ryo:**)*
  -*do:*
}
type
  TOnYomiEntry = record
    kana: string; //если пустое => кокудзи
    rare: boolean;
  end;
  POnYomiEntry = ^TOnYomiEntry;
  TOnYomiEntries = array of TOnYomiEntry; //пустой => кокудзи

function ParseOnYomi(const inp: string): TOnYomiEntries;


{
 Кандзи: чтения в словах.
}

type
  TCharLinkRef = record
   //Одно из двух:
    charref: integer;
    text: string;
  end;
  PCharLinkRef = ^TCharLinkRef;
  TCharLinkRefChain = TArray<TCharLinkRef>;
  TCharLinkPosition = (
    lpDefault = 0, //по-моему, LastLine
    lpFirstLine,
    lpAllClauses,
    lpOneClause,
    lpFromTo,
    lpNewline,
    lpKanji
  );
  TCharLink = record
    _type: char;
    pos: TCharLinkPosition;
    posfrom: byte;
    posto: byte;
    refs: TCharLinkRefChain;
    wordref: integer; //весь набор целиком ссылается на слово. Обычно ноль
  end;
  PCharLink = ^TCharLink;
  TKunReadingFlag = (
    krIgnoreInSearch, //не учитывать при поиске
    krOnReading       //онное чтение в японском слове (напр. АИрасии)
  );
  TKunReadingFlags = set of TKunReadingFlag;
  TKunReading = record
    text: string;
    ipos: TArray<byte>; //места, в к-х в транскрипции вместо й должно стоять и.
    //В дальнейшем нужно скомбинировать эти поля в расширенную транскрипцию типа:
    //  jii'jii'
    //Чтобы парсер мог работать по фиксированным правилам (ii'->ии, ii->ий).
    tpos: TArray<byte>; //места, где надо вставить тире (не меняя индексации остального)
    flags: TKunReadingFlags;
  end;
  PKunReading = ^TKunReading;
  TAdditionalKanji = record
    pos: integer; //точка вставки доп. цепочки кандзи, в кандзи!
    chain: TCharLinkRefChain; //пока что такие же свойства, как в ссылках
    kuri: boolean; //в конце цепочки курикаэси
  end;
  PAdditionalKanji = ^TAdditionalKanji;
  TOptionalSpan = record
    op: byte; //начинается после ...
    ed: byte; //кончается после ...; 0 = до конца транскрипции
  end;
  POptionalSpan = ^TOptionalSpan;
  TUsuallyIn = (
    uiNormal = 0,
    uiHiragana,
    uiKatakana,
    uiKana
  );
  TKunReadingSetFlag = (
    ksHidden,                  //не показывать, но учитывать при поиске
    ksTranscriptionUnderWord,
    ksWithKurikaeshi,
    ksUnchecked
  );
  TKunReadingSetFlags = set of TKunReadingSetFlag;
  TKunReadingSet = record
    items: array of TKunReading;
    prefix_chars: byte; //число символов префикса (до кандзи)
    main_chars: byte; //префикс + число символов, заменяемых кандзи
    kuri_chars: byte; //число символов, указанных до основного курикаэси
    optional_spans: TArray<TOptionalSpan>; //опциональные блоки
    refs: TArray<TCharLink>;
    flags: TKunReadingSetFlags;
    latin_tail: string; //дополнительный хвост вида ~суру
    additional_kanji: TArray<TAdditionalKanji>;
    usually_in: TUsuallyIn;
    tl_usually_in: TArray<TUsuallyIn>; //если даны метки для конкретных подпунктов
  end;
  PKunReadingSet = ^TKunReadingSet;
  TKunReadings = array of TKunReadingSet;

function ParseKanjiKunReadings(inp: string): TKunReadings;
function ParseCharLink(var pc: PChar): TCharLink;
function ParseAdditionalKanjiChain(var pc: PChar): TCharLinkRefChain;

{
  Кандзи: Переводы.
}
type
  TKunyomiMeaning = record
  end;
  TKunyomiMeanings = TArray<TKunyomiMeaning>;

  TCompoundMeaning = record
  end;
  TCompoundMeanings = TArray<TCompoundMeaning>;

  TRussianMeaningFlag = (
    mfUnchecked
  );
  TRussianMeaningFlags = set of TRussianMeaningFlag;
  TRussianMeanings = record
    flags: TRussianMeaningFlags;
    kunyomi: TKunyomiMeanings;
    compound: TCompoundMeanings;
  end;

function ParseKanjiRussian(inp: string): TRussianMeanings;
function ParseKanjiKunyomiMeanings(inp: string): TKunyomiMeanings;
function ParseKanjiCompoundMeanings(inp: string): TCompoundMeanings;


{
  Кандзи: Чтения в сочетаниях.
}
type
  TCompoundReadingType = (ctCommon, ctRare);
  TCompoundReading = record
    _type: TCompoundReadingType;
    text: string;
  end;
  PCompoundReading = ^TCompoundReading;

  TCompoundReadingSet = array of TCompoundReading;
  PCompoundReadingSet = ^TCompoundReadingSet;

  TCompoundReadings = array of TCompoundReadingSet;

function ParseKanjiCompoundReadings(inp: string): TCompoundReadings;
function DumpKanjiCompoundReadings(const AReadings: TCompoundReadings): string;
function DumpKanjiCompoundReadingSet(const AReadingSet: TCompoundReadingSet): string;
function DumpKanjiCompoundReading(const AReading: TCompoundReading): string;


{
  Кандзи: Чтения в именах.
}
type
  TNameReadingType = (
    ntCommon,       //обычные чтения
    ntOccasional,   //"также"
    ntRare,         //"реже"
    ntHidden        //скрытые, только для поиска
  );
  TNameReading = record
    _type: TNameReadingType;
    text: string;
  end;
  PNameReading = ^TNameReading;
  TNameReadings = array of TNameReading;

function ParseKanjiNameReadings(const inp: string): TNameReadings;
function DumpKanjiNameReadings(const AReadings: TNameReadings): string;
function DumpKanjiNameReading(const AReading: TNameReading): string;


{
  Кандзи: Kanji.Kunyomi
}
type
  TKanjiReadings = record
    show_kuns: byte; //показывать n кунов, остальное под кат
    show_tango: byte; //показывать n танго, остальное под кат
    kun: TKunReadings;
    compound: TCompoundReadings;
    name: TNameReadings;
  end;
  PKanjiReadings = ^TKanjiReadings;

function ParseKanjiKunYomi(inp: string): TKanjiReadings;
function DumpKanjiKunYomi(const AReadings: TKanjiReadings): string;


{
  Кандзи: Kanji.Compounds. Работает в сочетании с KunYomi и Russian.
}
type
  TCompoundFlag = (cfIrregularReading, cfIrregularMeaning, cfSingular, cfHashtag);
  TCompoundFlags = set of TCompoundFlag;
  TCompoundEntry = record
    block: integer; //special purpose blocks are negative
    wordref: integer;
    msgref: integer;
    flags: TCompoundFlags;
  end;
  PCompoundEntry = ^TCompoundEntry;
  TCompoundEntries = array of TCompoundEntry;

const //special purpose blocks
  BLOCK_NAMES = -1;

function ParseKanjiCompounds(inp: string): TCompoundEntries;
function DumpKanjiCompounds(const ACompounds: TCompoundEntries): string;
function DumpKanjiCompound(const ACompound: TCompoundEntry): string;


implementation
uses StrUtils;

{ Полезные функции для работы со строками }

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

{ Чуть более удобная обёртка для функции StrSplit }
function Split(const s: string; const sep: char): TStringArray;
begin
  Result := StrSplit(PChar(s), sep);
end;

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

function IsNumeric(const ch: char): boolean;
begin
  Result := (ch>='0') and (ch<='9');
end;

//Reads a positive number (only digits)
function EatNumber(var pc: PChar): integer;
var ps: PChar;
begin
  ps := pc;
  while IsNumeric(pc^) do
    Inc(pc);
  Check(pc>ps);
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



{ Сборщик жалоб }

procedure PushComplainContext(const AData: string);
begin
  if ComplainContext<>'' then
    ComplainContext:=ComplainContext+#09+AData
  else
    ComplainContext:=AData;
end;

procedure PopComplainContext;
var i, j: integer;
begin
  i := 0;
  repeat
    j := i;
    i := pos(#09,ComplainContext,j+1);
  until i<=0;
  if j=0 then //nothing found
    ComplainContext := ''
  else
    ComplainContext := Copy(ComplainContext,1,j-1);
end;

procedure Complain(const msg: string);
begin
  if ComplainContext<>'' then
    Warning(#13#10'  '+repl(ComplainContext,#09,#13#10'  ')+#13#10'  '+msg)
  else
    Warning(msg);
end;

procedure Complain(const msg, data: string);
begin
  if ComplainContext<>'' then
    Warning(#13#10'  '+repl(ComplainContext,#09,#13#10'  ')+#13#10'  '+data)
  else
    Warning(msg+#13#10'  '+data);
end;



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

{ Убирает ''кавычки'' по сторонам RusNick }
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
function ParseKanjiRusNick(const inp: string): TRusNicks;
var tmp: string;
  i_pos: integer;
  i, i_start: integer;
begin
  PushComplainContext(inp);
  try
    Result.Clear;
    tmp := inp;

   //Альтернативы нафиг
    i_pos := pos('*#*',tmp);
    if i_pos>0 then
      delete(tmp,i_pos,MaxInt);

   //Сложные случаи сводим к простым
    tmp := repl(tmp, '*_*', '*');
    tmp := repl(tmp, '**', '*');

   //Звёздочки в конце
    while (Length(tmp)>0) and (tmp[Length(tmp)]='*') do
      SetLength(tmp, Length(tmp)-1);

    if tmp='' then exit;

    i := 1;
    i_start := 1;
    while i<=Length(tmp) do begin
      if tmp[i]='*' then begin
        if (i_start<i)
        and (tmp[i_start]<>'!') then //это было ударение - нафиг
          Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)));
        i_start := i+1;
      end;
      Inc(i);
    end;

    if (i>i_start)
    and (tmp[i_start]<>'!') then
      Result.Add(KillQuotes(copy(tmp, i_start, i-i_start)))
  finally
    PopComplainContext;
  end;
end;


function ParseOnYomi(const inp: string): TOnYomiEntries;
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
  PushComplainContext(inp);
  try
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
  finally
    PopComplainContext;
  end;
end;

{
Поле: Kanji.KunYomi. Работает в сочетании с Russian и Compounds.
Формат: [префиксы][куны]|[чтения в сочетаниях]|[чтения в именах|c|палками]
Префиксы:
  !2!  показывать 2 куна, остальное под кат
  !2?  2 танго, остальное под кат
Любые блоки могут отсутствовать, однако появляются последовательно. Блок может
быть пуст.
}
function ParseKanjiKunYomi(inp: string): TKanjiReadings;
var block: string;
  i: integer;
begin
  PushComplainContext(inp);
  try
    Result.show_kuns := 0;
    Result.show_tango := 0;

   { Вынимаем из начала строки флаги вида !2!, !2?. Несколько флагов подряд мы
    пока не встречали и не поддерживаем. }
    if (Length(inp)>0) and (inp[1]='!') then begin
      i := 2;
      while (i<=Length(inp)) and (inp[i]<>'!') and (inp[i]<>'?') do
        Inc(i);
      if i>Length(inp) then
        raise Exception.Create('ParseKanjiKunYomi: Unterminated leading flag.');
      if inp[i]='!' then
        Result.show_kuns := StrToInt(copy(inp,2,i-2))
      else
        Result.show_tango := StrToInt(copy(inp,2,i-2));
      delete(inp, 1, i);
    end;

    block := pop(inp, '|');
    Result.kun := ParseKanjiKunReadings(block);
    block := pop(inp, '|');
    Result.compound := ParseKanjiCompoundReadings(block);
   //Остальное - имена
    Result.name := ParseKanjiNameReadings(inp);
  finally
    PopComplainContext;
  end;
end;

function DumpKanjiKunYomi(const AReadings: TKanjiReadings): string;
begin
  Result := 'Compounds: '+ DumpKanjiCompoundReadings(AReadings.compound) + '; '
    +'Names: '+DumpKanjiNameReadings(AReadings.name);
end;

{
НЕ ДОПИСАНО ПО-ЧЕЛОВЕЧЕСКИ.
Поле: Kanji.KunYomi, блок KunReadings.
Формат: 334*aware*awareppoi*kanashii
Набор цифр определяет, сколько букв покрывает кандзи в соотв. чтении:
  AWAre*AWAreppoi*KANAshii
0 означает "покрывает всё слово".
^ означает "10 + следующая цифра" (т.к. просто 10 читалось бы как позиции 1 и 0)

Дальше, разделённые *, идут наборы вариантов чтения кунёми. Обычно в наборе одно
чтение, если несколько - разделяются "*/*":
  034*jiji*/*jijii*/*jii*aware*kanashii
Последняя * не ставится.

чтение*Qn*     "и" вместо "й" в русской транскрипции в n-й позиции
ЧТЕНИЕ         онное чтение в яп. слове (напр. АИрасии) - отобр. синим
* чтение *     пробелы вокруг => не искать по этому чтению
*&*набор       не показывать, но учитывать при поиске (иногда &слитно).
               Относится именно к набору, т.к. покрытие относится к набору, а для
               скрытых чтений нет покрытия
*=*чтение      разновидность предыдущего чтения, не показывать
набор*!!*      транскрипция помещается ПОД словом, кол-во транскрипций может быть
               больше одной (#1196)
набор*!R*      то же, что !!, но только для русского словаря
чтение*-n*     вставить тире в n-ю позицию чтения. Похоже, только для красоты.
               Номера букв и длина чтения логически не меняются.
**набор        не отредактировано (набор и его переводы бледным цветом)
набор*#хвост*  латинский хвост к набору (~суру). Иногда пробел *#в конце *
               На длину чтения и его кандзи-отображение не влияет.

Пометки:
набор*^^*      чаще хираганой (иногда слитно^^)
набор*^!^*     то же, разместить текст под кандзи
набор*^@*      чаще катаканой
набор*^#*      чаще каной
набор*^01129*  стандартная ссылка -- см. ParseCharLink (иногда слитно:
               *hiroi^50859* или *ateru*^12060^11250*)
 К сожалению, существует некоторая проблема, следующие вещи все верны:
   ^40000      ссылка на 0000
   ^_240000    ссылка со 2-го пункта на 0000 (см. формат ссылок)
   ^^          чаще хираганой
   ^_2^^       чаще хираганой для 2-го пункта (#124)
   ^_2^#       чаще каной
 Ну и как это, блин, разбирать?

Отображение в кандзи:
набор*+6*      добавить курикаэси (種々) перед хвостом каны. Число задаёт, после
  какой буквы чтения вставить кури. Если оно не равно длине корня, то весь хвост
  отбрасывается:
    4*abcdef     = 種ef
    4*abcdef*+4* = 種々ef
    4*abcdef*+6* = 種々
  На это код братски закладывается, добавляя недостающий хвост как latin_tail.
  Хочется взять и уверстать.

*набор*$1[..]* вставить с указанной позиции кандзи-позиции (в итоговом тексте):
  [1084]       кандзи в скобках
  1084         кандзи по номеру
  ''кана''     кана
От стандартного чтения по-прежнему добавляется хвост после всего. Если был кури,
он добавляется перед хвостом.
Вхождений $ может быть несколько. Кандзи-позиций перед хвостом выделяется столько,
какой макс. номер вхождения + длина. Вхождения могут оставлять дыры ($1..$3..)
или перекрываться ($1''sou''$2''u'' - u в кане накрываются)
  $25445+      вторичный курикаэси, вставляется после вхождения
Вторичный кури допускается только один. После него обрезаются остальные вхождения,
в т.ч. последующие вторичные кури.
Если присутствует вторичный кури, любой канахвост переносится после первого кандзи:
  К1_канахвост_К2_К3_вторкури
Первичный кури вторичному не обязателен. Цифры после + не требуются и игнорируются.

набор*~n*      n символов от начала чтения вынести перед кандзи (для гонорификов)
набор*[3]5*    часть чтения с 4-й буквы по 5-ю опциональна (кв. скобки)
набор*[4[6]6]8 две опциональных части, (4..6] и (6..8]

Игнорируем:
набор*VI*
набор*VT*      неизвестно что, с виду не влияет
набор*L1*      неизвестно, что

После каждого флага тоже ставится *, последняя * не ставится (однако ставится
закрывающая для набора, если требуется).

Для скрытых чтений информация о покрытии отсутствует (напр. #13):
!2!041133* AKU *warui*ashi*ashikarazu*akutareru*^!^*akutare*^^*&*nikui*|AKU,WARU/O/-NIKUI
}
function match_targeted_kana_flag(pc: PChar): boolean; forward;
function ParseKanjiKunReadings(inp: string): TKunReadings;
var lead: string;
  rset: PKunReadingSet;
  rd: PKunReading;
  ps, pc: PChar;
  flag_next_hidden: boolean;
  flag_next_unchecked: boolean;
  flag_slash: boolean;
  req_sep: boolean; //require * or EOF as the next char
  i, tmp_int: integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(inp)<=0 then exit;

  flag_next_hidden := false;
  flag_next_unchecked := false;
  flag_slash := false;
  req_sep := false;

  lead := pop(inp,'*');
  rset := nil;
  rd := nil;
  pc := PChar(inp);
  while pc^<>#00 do begin

    if req_sep then begin
      Check(pc^='*', '* required and missing: '+pc);
      req_sep := false
    end;

   //Двойная звёздочка
    if eat(pc,'**') then begin
      flag_next_unchecked := true;
    end else

   //Одиночная звёздочка
    if pc^='*' then begin
      Inc(pc);
     //просто ничего не делаем
    end else

    if pc^='!' then begin
      Check(eat(pc, ['!!','!R ', '!R', '!$'])>=0);
      req_sep := true;
      Check(rset<>nil, 'No open reading set');
      Check(not (ksTranscriptionUnderWord in rset.flags), 'Duplicate !-sequence' );
      rset.flags := rset.flags + [ksTranscriptionUnderWord];
    end else

    if pc^='Q' then begin
      Inc(pc);
      Check(rd<>nil, 'No open reading');
      rd.ipos.Add(EatNumber(pc));
      req_sep := true;
    end else

    if pc^='V' then begin
     //Непонятные флаги. Просто удаляем
      Check(eat(pc, ['VI','VT','V2'])>=0);
      req_sep := true;
    end else

    if pc^='L' then begin
     //Непонятные флаги
      Check(eat(pc, ['L1'])>=0);
      req_sep := true;
    end else

   //также бывает нормальное тире: *-ni itatte wa. #1108
    if (pc^='-') and IsNumeric((pc+1)^) then begin
      Inc(pc);
      Check(rd<>nil, 'No open reading');
      rd.tpos.Add(EatNumber(pc));
     //req_sep := true;  //#217
    end else

    if pc^='~' then begin
      Inc(pc);
      Check(rset<>nil, 'No open reading set');
      Check(rset.prefix_chars<=0, 'Duplicate prefix char declaration');
      rset.prefix_chars := EatNumber(pc);
     //req_sep := true;  //Nope: #94
    end else

    if pc^='[' then begin
      Inc(pc);
      Check(rset<>nil, 'No open reading set');
      Check(rset.optional_spans.Length=0, 'Duplicate optional_pos declaration');
      with rset.optional_spans.AddNew^ do begin
        op := EatNumber(pc);
        if pc^='[' then begin
          Inc(pc);
         //доп. блок - такой же
          with rset.optional_spans.AddNew^ do begin
            op := EatNumber(pc);
            Check(pc^=']');
            Inc(pc);
            ed := EatNumber(pc); //во внутр. блоке Яркси требует 2-й части, потребуем и мы
          end;
        end;
        Check(pc^=']');
        Inc(pc);
        if IsNumeric(pc^) then //что-то осталось
          ed := EatNumber(pc)
        else
          ed := 0;
      end;
      req_sep := true;
    end else

    if (pc^='&') or (pc^='=') then begin
      Inc(pc);
      Check(not flag_next_hidden, 'Duplicate flag_hidden.');
      flag_next_hidden := true;
     //req_sep := true;  //Nope, может слипаться с последующими
    end else

    if pc^='/' then begin
      Inc(pc);
      rd := nil;
      flag_slash := true;
      req_sep := true;
    end else

    if pc^='^' then begin
      Check(rset<>nil, 'No open reading set');

      if eat(pc,['^^','^!^'])>=0 then begin
        Check(rset.usually_in=uiNormal, 'Duplicate ^^ flag');
        rset.usually_in := uiHiragana;
      end else

      if eat(pc,'^@') then begin
        Check(rset.usually_in=uiNormal, 'Duplicate ^@ flag');
        rset.usually_in := uiKatakana;
      end else

      if eat(pc,['^#','^!#'])>=0 then begin
        Check(rset.usually_in=uiNormal, 'Duplicate ^# flag');
        rset.usually_in := uiKana;
      end else

     //К сожалению, придётся заниматься проституцией
      if match_targeted_kana_flag(pc) then begin
        Inc(pc,2); //^_
        tmp_int := EatNumber(pc);
        if rset.tl_usually_in.Length<tmp_int then
          rset.tl_usually_in.SetLength(tmp_int+1);
        Check(rset.tl_usually_in[tmp_int] = uiNormal);
        if eat(pc,['^^','^!^'])>=0 then begin
          rset.tl_usually_in[tmp_int] := uiHiragana;
        end else
        if eat(pc,'^@') then begin
          rset.tl_usually_in[tmp_int] := uiKatakana;
        end else
        if eat(pc,'^#') then begin
          rset.tl_usually_in[tmp_int] := uiKana;
        end else
          Die('This is goddamn horrible.')
      end else
        rset.refs.Add(ParseCharLink(pc));

     //req_sep := true;  //Nope, бывают цепочки
    end else

    if pc^='#' then begin
      Inc(pc);
      Check(rset<>nil, 'No open reading set');
      ps := pc;
      while IsLatin(pc^) or (pc^='[') or (pc^=']') {#340} do
        Inc(pc);
      Check(pc>ps);
      rset.latin_tail := spancopy(ps, pc);
      while pc^=' ' do Inc(pc); //#34
      req_sep := true;
    end else

    if pc^='+' then begin
      Inc(pc);
      Check(rset<>nil, 'No open reading set');
      Check(not (ksWithKurikaeshi in rset.flags), 'Duplicate +kurikaeshi');
      rset.kuri_chars := EatNumber(pc);
      rset.flags := rset.flags + [ksWithKurikaeshi];
      req_sep := true;
    end else

    if pc^='$' then begin
      Check(rset<>nil, 'No open reading set');
      Inc(pc);
      Check(IsNumeric(pc^), 'invalid additional_kanji_pos');
      with rset.additional_kanji.AddNew^ do begin
        pos := Ord(pc^)-Ord('0');
        Inc(pc);
        chain := ParseAdditionalKanjiChain(pc);
        if pc^='+' then begin
          Inc(pc);
          kuri := true;
          for i := 0 to rset.additional_kanji.Length-2 do
            Assert(not rset.additional_kanji[i].kuri); //допуск.-ся только один доп. кури
        end;
      end;
     //req_sep := true;  //Nope, бывают цепочки
    end else

   //Просто транскрипция
    begin
      ps := pc;
      while IsLatin(pc^) or (pc^=':'){долгота в транскрипциях} or (pc^=''''){после буквы n}
      or (pc^='-') or (pc^=' ') do
        Inc(pc);

     //Мы обязаны хоть что-то прочесть, т.к. все флаги мы уже исключили
      Check(pc>ps, 'Cannot parse this part: '+pc);

      if not flag_slash then begin
        SetLength(Result, Length(Result)+1);
        rset := @Result[Length(Result)-1];
        FillChar(rset^, SizeOf(rset^), 0);

        if flag_next_hidden then begin
         //Ну и что, блин, делать? Откуда брать настоящую информацию о покрытии?
         //Потому, что попадаются чтения типа nikui, которые и явно важны,
         //и покрытие нетривиально.
         //Идиотский формат.
          rset.main_chars := 0; //так делает сам яркси
        end else begin
         //Съедаем одну позицию из lead
          if lead='' then begin
            Complain('No char coverage data for another reading block');
            rset.main_chars := 0;
          end else begin
           //Расширенная позиция: ^7 == 17
            if lead[1]='^' then begin
              rset.main_chars := 10;
              delete(lead,1,1);
              Check(lead<>'', 'Incomplete char coverage ^expansion');
            end else
              rset.main_chars := 0;

            if lead[1]='_' then begin //похоже, это как ноль
              Complain('lead[i]==_, correcting to 0');
              lead[1]:='0';
            end;

           //Остаток позиции
            Check((lead[1]>='0') and (lead[1]<='9'), 'Invalid char coverage position: '+lead[1]+' (digit expected)');
            rset.main_chars := rset.main_chars + Ord(lead[1])-Ord('0');
            delete(lead,1,1);
          end;
        end;

        if flag_next_unchecked then
          rset.flags := rset.flags + [ksUnchecked];
        flag_next_unchecked := false;

        if flag_next_hidden then
          rset.flags := rset.flags + [ksHidden];
        flag_next_hidden := false;
      end else
        flag_slash := false; //слеш работает на одно чтение вперёд

      SetLength(rset.items, Length(rset.items)+1);
      rd := @rset.items[Length(rset.items)-1];
      FillChar(rd^, SizeOf(rd^), 0);

      rd.text := spancopy(ps,pc);
      if rd.text<>'' then begin
       //сначала пробелы
        if (rd.text[1]=' ') or (rd.text[Length(rd.text)]=' ') then begin
          rd.text := Trim(rd.text);
          rd.flags := rd.flags + [krIgnoreInSearch];
        end;

       //теперь заглавные
        if IsUppercaseLatin(rd.text[1]) then begin
          rd.text := LowerCase(rd.text);
          rd.flags := rd.flags + [krOnReading];
        end;
      end;

     //req_sep := true;  //Nope, возможно, что-то осталось
    end;
  end;

 //Контроль
  if lead<>'' then
    Complain('Остались неразобранные позиции числа символов.')
end;

function match_targeted_kana_flag(pc: PChar): boolean;
begin
  Result := false;
  if pc^<>'^' then exit;
  Inc(pc);
  if pc^<>'_' then exit;
  Inc(pc);
  if not IsNumeric(pc^) then exit;
  Inc(pc); //пока допускаем макс. одну цифру
 { Более свободно:
    while IsNumeric(pc^) do Inc(pc);
  Однако так рискуем матчнуть ^_210000^1111. Сейчас не матчнем, т.к. у нас
  дальше требуется ^, @ или #, но если это требование ослабнет... }
  if pc^<>'^' then exit;
  Inc(pc);
  Result := (pc^='^') or (pc^='@') or (pc^='#');
end;

{
Формат ссылок на кандзи: ^[цифра][номер]-''текст''-[номер]=[номер слова]
Все части, кроме ^[цифра][номер] опциональны.
Цифра:
  0 = см.
  1 = ср.
  2 = ?????
  3 = реже
  4 = иначе
  5 = чаще
  6 = синоним
  7 = антоним
  8 = не путать с
  9 = ранее
  r = ранее так же
  t = теперь
  i = иногда так же
  z = как замена
  m = ошибочно вместо
  всё остальное = ??????
^[цифра][номер]-[номер] === несколько кандзи подряд
^[цифра][номер]-''[текст]'' === доп. текст хираганой
^<блок>=[номер слова] === сделать весь блок ссылкой на указанное слово
По умолчанию ссылка присоединяется к последнему варианту перевода
^_[цифра]<блок> === присоединить ссылку к n-му варианту перевода из нескольких
^:<блок> === присоединить ссылку ко всем вариантам перевода
^::[2 цифры]<блок> === присоединить ссылку к n-му и m-му вариантам перевода
  /  палка между кандзи (1172/8654)
^!<блок> === поставить ссылку слева от вариантов перевода (под словом)
^- === присоединить ссылку к первой строчке перевода. Немного отличается от
  ^_цифра, т.к. вариант перевода может быть всего один, нулевой, а строчек
  несколько.
^+ === только для блоков 2+: начать с новой строки, а не продолжать старую.
Также бывает:
  ^4''текст''номер
Тогда в чём смысл тире? Неизвестно, но без тире Яркси с длинными цепочками не
справляется (>2 эл-тов).
}
function ParseCharLink(var pc: PChar): TCharLink;
var i: integer;
  ps: PChar;
begin
  Check(pc^='^', 'Invalid start mark');
  Inc(pc);

  if pc^='+' then begin
    Inc(pc);
    Result.pos := lpNewline;
  end else
  if pc^='-' then begin
    Inc(pc);
    Result.pos := lpFirstLine;
  end else
  if pc^='_' then begin
    Inc(pc);
    Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
    Result.pos := lpOneClause;
    Result.posfrom := Ord(pc^)-Ord('0');
    Inc(pc);
  end else
  if pc^=':' then begin
    Inc(pc);
    if pc^=':' then begin
      Result.pos := lpFromTo;
      Inc(pc);
      Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
      Result.posfrom := Ord(pc^)-Ord('0');
      Inc(pc);
      Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
      Result.posto := Ord(pc^)-Ord('0');
      Inc(pc);
    end else
      Result.pos := lpAllClauses;
  end else
  if pc^='!' then begin //#356
    Inc(pc);
    Result.pos := lpKanji;
  end else
    Result.pos := lpDefault;

  if ((pc^>='0') and (pc^<='9')) or (pc^='r') or (pc^='t') or (pc^='i')
  or (pc^='z') or (pc^='m') then
    Result._type := pc^
  else
    Die('Invalid type: '+pc^);
  Inc(pc);

  Result.refs.Clear;
  while pc^<>#00 do begin
    if pc^='''' then begin
      Check((pc+1)^='''', 'invalid singular '' mark');
      pc := pc+2;
      ps := pc;
      while (pc^<>#00) and (pc^<>'''') do
        Inc(pc);
      Check(pc^<>#00, 'invalid unclosed text element');
      Check((pc+1)^='''', 'invalid singular '' mark');
      with Result.refs.AddNew^ do begin
        charref := 0;
        text := spancopy(ps,pc);
      end;
      pc := pc+2;
    end else
   //Допустимые печатные символы
    if (pc^='/') then begin
      with Result.refs.AddNew^ do begin
        charref := 0;
        text := pc;
      end;
      Inc(pc);
    end else
   //Разделитель. Необязателен, но обычно присутствует. С длинными цепочками
   //без разделителя Яркси не справляется.
    if pc^='-' then begin
     //просто пропускаем
      Inc(pc);
    end else
   //Число
    if IsNumeric(pc^) then  begin
      with Result.refs.AddNew^ do begin
        text := '';
        charref := 0;
        for i := 1 to 4 do begin
          Check((pc^>='0') and (pc^<='9'), 'invalid charref');
          charref := charref * 10 + Ord(pc^)-Ord('0');
          Inc(pc);
        end;
      end;
    end else
   //Что-то другое => конец цепочки
      break;
  end;

 //Последняя часть
  Result.wordref := 0;
  if pc^='=' then begin
    Inc(pc);
    while (pc^>='0') and (pc^<='9') do begin
      Inc(pc);
      Result.wordref := Result.wordref * 10 + Ord(pc^)-Ord('0');
    end;
  end;
end;

{ Цепочка вида номер''текст''[номер]номер, встречающаяся в KunYomi при вставке
 дополнительных кандзи }
function ParseAdditionalKanjiChain(var pc: PChar): TCharLinkRefChain;
var ps: PChar;
begin
 {
  Вообще говоря, цепочка может быть любой, но Яркси поступает по разному в
  зав-ти от того, начинается она с кандзи или текста.
  Если с кандзи то корень слова вышибает в начало канахвоста:
    __К1_текст_К0_канахвост
  Если с текста, то оставляет на месте:
    K0_текст_K1_канахвост
 }
  Result.Clear;
  while pc^<>#00 do begin

   //Текст каной
    if pc^='''' then begin
      Inc(pc);
      Check(pc^='''');
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>'''') do
        Inc(pc);
      Check(pc^<>#00);
      with Result.AddNew^ do begin
        text := spancopy(ps,pc);
        charref := 0;
      end;
      Check(pc^='''');
      Inc(pc);
      Check(pc^='''');
      Inc(pc);
    end else
   //Допустимые символы
    if (pc^='[') or (pc^=']') then begin
      with Result.AddNew^ do begin
        text := pc^;
        charref := 0;
      end;
      Inc(pc);
    end else
   //Число (номер кандзи)
    if IsNumeric(pc^) then begin
      with Result.AddNew^ do begin
        text := '';
        charref := EatNumber(pc); //надеюсь, два подряд не бывает..
      end;
    end else
      break; //неизвестный символ => выходим
  end;
end;



{
Поле: Kanji.Russian.
Формат: KunyomiMeanings|CompoundMeanings
Перед вызовом конвертируйте русские буквы и удалите из строки все "\" (мусор).
}
function ParseKanjiRussian(inp: string): TRussianMeanings;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(inp)<=0 then exit;

  if inp[1]='~' then begin
    Result.flags := Result.flags + [mfUnchecked];
    delete(inp,1,1);
  end;

  Result.kunyomi := ParseKanjiKunyomiMeanings(pop(inp,'|'));
  Result.compound := ParseKanjiCompoundMeanings(inp);
end;

function ParseKanjiKunyomiMeanings(inp: string): TKunyomiMeanings;
begin

end;

function ParseKanjiCompoundMeanings(inp: string): TCompoundMeanings;
begin

end;



{
Поле: Kanji.KunYomi, блок CompoundReadings.
Формат:
  AISO_AISOX/AISO2,ASIO2X_AISO2Y-AISO2Z/AISO3
  => aiso, реже aisox;
     aiso2, aiso2x, реже aiso2y, aiso2z;
     aiso3
  U,HA,HANE
Разделители:
  /   следующий набор
  ,   следующее чтение
  _   следующее редкое чтение
  -   ничего не значит, обычный символ
}
function ParseKanjiCompoundReadings(inp: string): TCompoundReadings;
var ps, pc: PChar;
  rset: PCompoundReadingSet;
  next_type: TCompoundReadingType;

  procedure CommitReading;
  begin
    if pc<=ps then exit;
    if rset=nil then begin
      SetLength(Result, Length(Result)+1);
      rset := @Result[Length(Result)-1];
      SetLength(rset^, 1);
    end else
      SetLength(rset^, Length(rset^)+1);
    rset^[Length(rset^)-1]._type := next_type;
    rset^[Length(rset^)-1].text := spancopy(ps, pc);
  end;

begin
  SetLength(Result, 0);
  if inp='' then exit;

  rset := nil;
  next_type := ctCommon;

  pc := PChar(inp);
  ps := pc;
  while pc^<>#00 do begin

    if pc^='/' then begin
      CommitReading;
      rset := nil;
      next_type := ctCommon;
      ps := pc+1;
    end else

    if pc^=',' then begin
      CommitReading;
      ps := pc+1;
    end else

    if pc^='_' then begin
      CommitReading;
      next_type := ctRare;
      ps := pc+1;
    end else

    begin
      //nothing
    end;

    Inc(pc);
  end;

  CommitReading;
end;

function DumpKanjiCompoundReadings(const AReadings: TCompoundReadings): string;
var i: integer;
begin
  if Length(AReadings)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiCompoundReadingSet(AReadings[0]);
  for i := 1 to Length(AReadings)-1 do
    Result := Result + '; ' + DumpKanjiCompoundReadingSet(AReadings[i]);
end;

function DumpKanjiCompoundReadingSet(const AReadingSet: TCompoundReadingSet): string;
var i: integer;
begin
  if Length(AReadingSet)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiCompoundReading(AReadingSet[0]);
  for i := 1 to Length(AReadingSet)-1 do
    Result := Result + ', ' + DumpKanjiCompoundReading(AReadingSet[i]);
end;

function DumpKanjiCompoundReading(const AReading: TCompoundReading): string;
begin
  case AReading._type of
    ctCommon: Result := 'C:';
    ctRare: Result := 'R:';
  else
    raise Exception.Create('DumpKanjiCompoundReading: Unexpected name reading type');
  end;
  Result := Result + AReading.text;
end;



{
Поле: Kanji.KunYomi, блок NameReadings.
Формат: [в именах]|[также]|[реже]-[скрыто]
Любые блоки могут отсутствовать, но появляются последовательно:
  [в именах]||[реже]
Исключение: || в начале - то же, что |           (#4)
Исключение: -[скрыто] обрывает цепочку.
  [в именах]-[скрыто]|[всё равно скрыто]
  [в именах]-[скрыто]-[скрыто]
Формат блока: $чтение$чтение$
В мягком режиме:
  - деление запятой $чтение,чтение$ (яркси это терпит)
  - отсутствие $рамок$
}
function ParseKanjiNameReadings(const inp: string): TNameReadings;
var blockType: TNameReadingType;
  pc, ps: PChar;
  readingSetOpen: boolean; //видели $ после прошлого | или -

 //ps должен стоять на начальном символе $, pc на конечном.
  procedure CommitReading;
  begin
    if ps>=pc then exit;
    if (ps+1=pc) and (ps^=' ') then
      exit; //Яркси любит такие пустые пробельные позиции, но по сути не нужны
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1]._type := blockType;
    Result[Length(Result)-1].text := spancopy(ps,pc);
  end;

begin
  SetLength(Result, 0);
  if inp='' then exit;

  blockType := ntCommon;
  readingSetOpen := false;
  pc := PChar(inp);
  if pc^='|' then Inc(pc); // иногда лишний | в начале, см. формат
  ps := pc;
  while pc^<>#00 do begin
    if pc^='|' then begin
     {$IFDEF STRICT}
      if ps<pc then
        raise Exception.Create('ParseKanjiNameReadings: No closing tag for a reading');
     {$ELSE}
      CommitReading;
     {$ENDIF}
      case blockType of
        ntCommon: blockType := ntOccasional;
        ntOccasional: blockType := ntRare;
      else //rare or hidden
        raise Exception.Create('ParseKanjiNameReadings: additional | not allowed.');
      end;
      ps := pc+1;
      readingSetOpen := false;
    end else

   //note: могут быть и как часть текста: $asd$-bsd$  $asd-$bsd$
    if (pc^='-') and ((pc+1)^='$') and (ps>=pc) then begin
     {$IFDEF STRICT}
      if ps<pc then
        raise Exception.Create('ParseKanjiNameReadings: No closing tag for a reading');
     {$ELSE}
      CommitReading;
     {$ENDIF}
      blockType := ntHidden;
      ps := pc+1;
      readingSetOpen := false;
    end else

    if pc^='$' then begin
      CommitReading; //если это переключатель
      readingSetOpen := true;
      ps := pc+1;
    end else

    begin
     {$IFDEF STRICT}
      if not ReadingSetOpen then
        raise Exception.Create('ParseKanjiNameReadings: No opening tag for a reading');
     {$ENDIF}
    end;

    Inc(pc);
  end;

  CommitReading;
end;

function DumpKanjiNameReadings(const AReadings: TNameReadings): string;
var i: integer;
begin
  if Length(AReadings)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiNameReading(AReadings[0]);
  for i := 1 to Length(AReadings)-1 do
    Result := Result + ', ' + DumpKanjiNameReading(AReadings[i]);
end;

function DumpKanjiNameReading(const AReading: TNameReading): string;
begin
  case AReading._type of
    ntCommon: Result := 'C:';
    ntOccasional: Result := 'O:';
    ntRare: Result := 'R:';
    ntHidden: Result := 'H:';
  else
    raise Exception.Create('DumpKanjiNameReading: Unexpected name reading type');
  end;
  Result := Result + AReading.text;
end;


{
Поле: Kanji.Compounds. Работает в сочетании с KunYomi и Russian.
Формат: 1:48667,N:8502@,N:2280,N:2279,N:55637,[блок]:[слово][флаги]
Специальный блок N означает "В именах".
Флаги:
 @   по соответствующему чтению (обычно не входящему в общий список) данный
     иероглиф может читаться только в одиночку и никогда в сочетаниях с другими
     знаками (отметить значение флажком 1)
 ^   нестандартное чтение (отметить треугольничком)
 *   нестандартное значение (отметить ромбиком)
 &   нестандартное чтение и значение (отметить и ромбиком, и треугольничком,
     а также отступить немного после предыдущего слова)
 #   с виду никак не влияет, может сочетаться с другими
Фигурные скобки (в комментарии заменил на квадратные) означают одно из нескольких
заранее известных сообщений для конкретных кандзи:
  1:6590,1:6579,1:[27]
Cохраняем в msgref, wordref при этом пуст.
# Решётка в начале списка означает, что список длинный и его нужно показывать
свёрнуто (#54,#67).

Выбросы:
1. В ограниченном числе случаев встречается _!мусор_мусор_1:нормальная,2:статья.
2. Встречается и _какое-то_число_1:нормальная,2:статья.
3. Иногда: "=мусор мусор" вместо статьи (впрочем, статья пустая).
}
function ParseKanjiCompounds(inp: string): TCompoundEntries;
var parts: TStringArray;
  i: integer;
  block_id: string;
  ch: char;
begin
 //Есть ровно ограниченное число случаев, когда по какой-то причине копия KunYomi
 //вываливается в Compounds. Все они начинаются с _ и мусор заканчивается _
  if (Length(inp)>0) and (inp[1]='_') then begin
    Complain('KunYomi leak', inp);
    parts := Split(inp, '_'); //боже помоги
    if Length(parts)>0 then
      inp := parts[Length(parts)-1];
  end;

 //Изредка вместо всей статьи фигня вида "=мусор мусор"
  if (Length(inp)>0) and (inp[1]='=') then begin
    Complain('= operator leak', inp);
    inp := '';
  end;

  parts := Split(inp, ',');
  SetLength(Result, Length(parts));
  if Length(parts)<=0 then exit; //меньше проверок

 //Решётка в начале списка означает, что список длинный и его нужно показывать
 //сжато. Игнорируем.
  if (Length(parts[0])>0) and (parts[0][1]='#') then
    delete(parts[0],1,1);

  for i := 0 to Length(parts)-1 do begin
    block_id := trypop(parts[i], ':');
    if block_id='' then
      raise Exception.Create('ParseKanjiCompounds: no block_id separator.');
   //Блок может быть и двухциферным, 10+
    if block_id='N' then
      Result[i].block := BLOCK_NAMES
    else
      Result[i].block := StrToInt(block_id);

    Result[i].flags := [];
    while Length(parts[i])>0 do begin
      ch := parts[i][Length(parts[i])];
      if ch='@' then
        Result[i].flags := Result[i].flags + [cfSingular]
      else
      if ch='^' then
        Result[i].flags := Result[i].flags + [cfIrregularReading]
      else
      if ch='*' then
        Result[i].flags := Result[i].flags + [cfIrregularMeaning]
      else
      if ch='&' then
        Result[i].flags := Result[i].flags + [cfIrregularReading, cfIrregularMeaning]
      else
      if ch='#' then
        Result[i].flags := Result[i].flags + [cfHashtag]
      else
        break;
      delete(parts[i], Length(parts[i]), 1);
    end;

    if tryunquote(parts[i],'{','}') then
      Result[i].msgref := StrToInt(parts[i])
    else
      Result[i].wordref := StrToInt(parts[i]);
  end;
end;

function DumpKanjiCompounds(const ACompounds: TCompoundEntries): string;
var i: integer;
begin
  if Length(ACompounds)<=0 then begin
    Result := '';
    exit;
  end;
  Result := DumpKanjiCompound(ACompounds[0]);
  for i := 1 to Length(ACompounds)-1 do
    Result := Result + ', ' + DumpKanjiCompound(ACompounds[i]);
end;

function DumpKanjiCompound(const ACompound: TCompoundEntry): string;
begin
  Result := IntToStr(ACompound.block)+':'+IntToStr(ACompound.wordref);
  if ACompound.msgref>0 then
    Result := Result+':'+IntToStr(ACompound.msgref);
  if cfIrregularReading in ACompound.flags then
    Result := Result + '^';
  if cfIrregularMeaning in ACompound.flags then
    Result := Result + '*';
  if cfSingular in ACompound.flags then
    Result := Result + '@';
  if cfHashtag in ACompound.flags then
    Result := Result + '#';
end;

end.
