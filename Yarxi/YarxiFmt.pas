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

function pop(var s: string; const sep: char): string;
function trypop(var s: string; const sep: char): string;
function repl(const s: string; const AFrom, ATo: string): string; inline;
function spancopy(ps, pe: PChar): string;
function Split(const s: string; const sep: char): TStringArray; inline;
function Unquote(const s: string; op, ed: char): string;
function TryUnquote(var s: string; op, ed: char): boolean;

function IsLatin(const ch: char): boolean; inline;
function IsUpperCaseLatin(const ch: char): boolean; inline;

{ Функции посылают сюда жалобы на жизнь. В дальнейшем надо сделать нормальный
 сборщик жалоб, как в WarodaiConvert. }

procedure PushComplainContext(const AData: string);
procedure PopComplainContext;
procedure Complain(const msg: string); overload;
procedure Complain(const msg, data: string); overload;


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
  TCharLinkRefChain = array of TCharLinkRef;
  TCharLink = record
    _type: byte;
    tlvars: array[0..1] of byte; //номер варианта перевода, к к-му приписана ссылка. Обычно ноль
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
    ipos: array of byte; //места, в к-х в транскрипции вместо й должно стоять и.
    //В дальнейшем нужно скомбинировать эти поля в расширенную транскрипцию типа:
    //  jii'jii'
    //Чтобы парсер мог работать по фиксированным правилам (ii'->ии, ii->ий).
    tpos: array of byte; //места, где надо вставить тире (не меняя индексации остального)
    flags: TKunReadingFlags;
  end;
  PKunReading = ^TKunReading;
  TKunReadingSetFlag = (
    ksHidden,                  //не показывать, но учитывать при поиске
    ksTranscriptionUnderWord,
    ksUsuallyInHiragana,
    ksWithKurikaeshi,
    ksUnchecked
  );
  TKunReadingSetFlags = set of TKunReadingSetFlag;
  TKunReadingSet = record
    items: array of TKunReading;
    prefix_chars: byte; //число символов префикса (до кандзи)
    main_chars: byte; //префикс + число символов, заменяемых кандзи
    optional_op: byte; //опциональный блок начинается после ...
    optional_ed: byte; //опциональный блок кончается после ...
    refs: array of TCharLink;
    flags: TKunReadingSetFlags;
    tail: string; //дополнительный хвост вида ~суру
    additional_kanji_pos: integer; //точка вставки доп. цепочки кандзи, обычно 0
    additional_kanji: TCharLinkRefChain; //пока что такие же свойства, как в ссылках
  end;
  PKunReadingSet = ^TKunReadingSet;
  TKunReadings = array of TKunReadingSet;

function ParseKanjiKunReadings(inp: string): TKunReadings;
function ParseCharLink(var pc: PChar): TCharLink;
function ParseAdditionalKanjiChain(var pc: PChar): TCharLinkRefChain;


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

function repl(const s: string; const AFrom, ATo: string): string;
begin
  Result := UniReplaceStr(s, AFrom, ATo);
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


{ Сборщик жалоб }

var
  ComplainContext: string;
 //добавляется ко всем жалобам. Внешние функции могут записывать сюда
 //номер и/или содержание текущей записи

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
чтение*-n*     вставить тире в n-ю позицию чтения. Непонятно, зачем нужно. Иногда без пробела.
набор*!!*      транскрипция помещается ПОД словом, кол-во транскрипций может быть
               больше одной (#1196)
набор*!R*      то же, что !!, но только для русского словаря
набор*~n*      n символов от начале чтения вынести перед кандзи (обычно для гонорификов)
набор*[3]5*    часть чтения с 4-й буквы по 5-ю опциональна (кв. скобки)
набор*^^*      чаще хираганой (иногда слитно^^)
набор*^!^*     то же, разместить текст под кандзи
набор*^01129*  стандартная ссылка -- см. ParseCharLink (иногда слитно:
               *hiroi^50859* или *ateru*^12060^11250*)
набор*+6*      добавить курикаэси (種々). Цифра переопределяет, сколько букв
               покрывают кандзи+кури вместе. Зачем - не знаю.
набор*#хвост*  текстовый хвост к набору (~суру). Иногда пробел *#в конце *
**набор        не отредактировано (набор и его переводы бледным цветом)
*набор*$1[..]* начать с указанной позиции кандзи-позиции (в итоговом тексте):
  1084         кандзи по номеру
  [1084]       кандзи в скобках
  ''кана''     кана
               От стандартного чтения по-прежнему добавляется хвост после всего.
набор*VI*
набор*VT*      неизвестно что, с виду не влияет

После каждого флага тоже ставится *, последняя * не ставится (однако ставится
закрывающая для набора, если требуется).

Для скрытых чтений информация о покрытии отсутствует (напр. #13):
!2!041133* AKU *warui*ashi*ashikarazu*akutareru*^!^*akutare*^^*&*nikui*|AKU,WARU/O/-NIKUI
}
function ParseKanjiKunReadings(inp: string): TKunReadings;
var lead, word: string;
  rset: PKunReadingSet;
  rd: PKunReading;
  pc: PChar;
  flag_next_hidden: boolean;
  flag_next_unchecked: boolean;
  flag_slash: boolean;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(inp)<=0 then exit;

  flag_next_hidden := false;
  flag_next_unchecked := false;
  flag_slash := false;

  lead := pop(inp,'*');
  inp := repl(inp, '**', '*%*'); //чтобы облегчить нам жизнь ниже
  rset := nil;
  rd := nil;
  word := '';
  while (inp<>'') or (word<>'') do begin
    if word='' then
      word := pop(inp,'*');
    if Length(word)<=0 then continue;

    if word[1]='!' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check((word='!!') or (word='!R') or (word='!R '), 'mod "'+word+'": Invalid !-sequence');
      Check(not (ksTranscriptionUnderWord in rset.flags), 'Duplicate !-sequence' );
      rset.flags := rset.flags + [ksTranscriptionUnderWord];
      word := '';
    end else

    if word[1]='Q' then begin
      Check(rd<>nil, 'mod "'+word+'": no open reading');
      delete(word,1,1);
      SetLength(rd.ipos, Length(rd.ipos)+1);
      rd.ipos[Length(rd.ipos)-1] := StrToInt(word);
      word := '';
    end else

    if word[1]='V' then begin
     //Непонятные флаги: VI, VT. Просто удаляем
      delete(word,1,1);
      Check(word<>'');
      Check((word[1]='I')or(word[1]='T'));
      delete(word,1,1);
      Check(word='');
    end else

    if word[1]='-' then begin
      Check(rd<>nil, 'mod "'+word+'": no open reading');
      delete(word,1,1);
      SetLength(rd.tpos, Length(rd.tpos)+1);
      rd.tpos[Length(rd.tpos)-1] := StrToInt(word);
      word := '';
    end else

    if word[1]='~' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check(rset.prefix_chars<=0,
        'Duplicate prefix char declaration in a single reading set');
      delete(word,1,1);
      rset.prefix_chars := StrToInt(word);
      word := '';
    end else

    if word[1]='[' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check((rset.optional_op<=0) and (rset.optional_ed<=0),
        'Duplicate optional_pos declaration in a single reading set');
      delete(word,1,1);
      rset.optional_op := StrToInt(trypop(word,']')); //должно присутствовать => ошибка, если вернёт пустую строку
      if word<>'' then //что-то осталось
        rset.optional_ed := StrToInt(word);
      word := '';
    end else

    if (word[1]='&') or (word[1]='=') then begin
      Check(not flag_next_hidden, 'mod "'+word+'": Duplicate flag_hidden.');
      flag_next_hidden := true;
      delete(word,1,1);
     //может слипаться с последующими
    end else

    if word[1]='/' then begin
      Check(word='/', 'Invalid /-sequence');
      rd := nil;
      flag_slash := true;
      word := '';
    end else

    if ((word[1]='^') and (word[2]='^'))
    or ((word[1]='^') and (word[2]='!') and (word[3]='^')) then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check(not (ksUsuallyInHiragana in rset.flags),
        'Duplicate ^^ flag for a reading set');
      rset.flags := rset.flags + [ksUsuallyInHiragana];
      if word[2]='!' then
        delete(word,1,3)
      else
        delete(word,1,2);
     //возможно, что-то осталось
    end else

    if word[1]='^' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      pc := PChar(word);
      SetLength(rset.refs, Length(rset.refs)+1);
      rset.refs[Length(rset.refs)-1] := ParseCharLink(pc);
      delete(word, 1, pc-PChar(word));
     //возможно, что-то осталось
    end else

    if word[1]='#' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      rset.tail := word;
      word := ''
    end else

    if word[1]='+' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check(not (ksWithKurikaeshi in rset.flags),
        'Duplicate +kurikaeshi flag for a reading set');
      delete(word,1,1);
      rset.main_chars := StrToInt(word);
      rset.flags := rset.flags + [ksWithKurikaeshi];
      word := '';
    end else

    if word='%' then begin //наш специальный символ -- целиком
      flag_next_unchecked := true;
      word := '';
    end else

    if word[1]='$' then begin
      Check(rset<>nil, 'mod "'+word+'": no open reading set');
      Check(rset.additional_kanji_pos<=0, 'duplicate additional kanji');
      pc := PChar(word);

      Inc(pc);
      Check((pc^>='0') and (pc^<='9'), 'invalid additional_kanji_pos');
      rset.additional_kanji_pos := Ord(pc^)-Ord('0');

      Inc(pc);
      rset.additional_kanji := ParseAdditionalKanjiChain(pc);
      delete(word, 1, pc-PChar(word));
     //возможно, что-то осталось
    end else

    begin
      pc := PChar(word);
      while IsLatin(pc^) or (pc^=':'){долгота в транскрипциях}
      or (pc^='-') or (pc^=' ') do
        Inc(pc);

     //Мы обязаны хоть что-то прочесть, т.к. все флаги мы уже исключили
      Check(pc>PChar(word), 'Cannot parse this part: '+pc);

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
          Check(lead<>'', 'No char coverage data for another reading block');
         //Расширенная позиция: ^7 == 17
          if lead[1]='^' then begin
            rset.main_chars := 10;
            delete(lead,1,1);
            Check(lead<>'', 'Incomplete char coverage ^expansion');
          end else
            rset.main_chars := 0;
         //Остаток позиции
          Check((lead[1]>='0') and (lead[1]<='9'), 'Invalid char coverage position: '+lead[1]+' (digit expected)');
          rset.main_chars := rset.main_chars + Ord(lead[1])-Ord('0');
          delete(lead,1,1);
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

      rd.text := spancopy(PChar(word),pc);
      delete(word, 1, pc-PChar(word));

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

     //возможно, что-то осталось в word
    end;
  end;

 //Контроль
  if lead<>'' then
    Complain('Остались неразобранные позиции числа символов.')
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
  всё остальное = ??????
^[цифра][номер]-[номер] === несколько кандзи подряд
^[цифра][номер]-''[текст]'' === доп. текст хираганой
^<блок>=[номер слова] === сделать весь блок ссылкой на указанное слово
^_[цифра]<блок> === присоединить ссылку к n-му варианту перевода из нескольких
^::[2 цифры]<блок> === присоединить ссылку к n-му и m-му вариантам перевода

Не сделано:
  /  палка между кандзи (1172/8654)
}
function ParseCharLink(var pc: PChar): TCharLink;
var i: integer;
  ref: PCharLinkRef;
  ps: PChar;
begin
  Check(pc^='^', 'Invalid start mark');
  Inc(pc);

  if pc^='_' then begin
    Inc(pc);
    Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
    Result.tlvars[0] := Ord(pc^)-Ord('0');
    Inc(pc);
  end else
  if pc^=':' then begin
    Inc(pc);
    Check(pc^=':');
    Inc(pc);
    Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
    Result.tlvars[0] := Ord(pc^)-Ord('0');
    Inc(pc);
    Check((pc^>='0') and (pc^<='9'), 'Invalid tl-variant index');
    Result.tlvars[1] := Ord(pc^)-Ord('0');
    Inc(pc);
  end else
  begin
    Result.tlvars[0] := 0;
    Result.tlvars[0] := 1;
  end;


  Check((pc^>='0') and (pc^<='9'), 'Invalid type');
  Result._type := Ord(pc^)-Ord('0');

  SetLength(Result.refs, 0);

  repeat
    Inc(pc);
    SetLength(Result.refs, Length(Result.refs)+1);
    ref := @Result.refs[Length(Result.refs)-1];
    ref.text := '';
    ref.charref := 0;
    if pc^='''' then begin
      Check((pc+1)^='''', 'invalid singular '' mark');
      pc := pc+2;
      ps := pc;
      while (pc^<>#00) and (pc^<>'''') do
        Inc(pc);
      Check(pc^<>#00, 'invalid unclosed text element');
      Check((pc+1)^='''', 'invalid singular '' mark');
      ref.text := spancopy(ps,pc);
      pc := pc+2;
    end else
   //Допустимые символы
    if (pc^='/') then begin
      ref.text := pc;
      Inc(pc);
    end else
   //Число
    begin
      for i := 1 to 4 do begin
        Check((pc^>='0') and (pc^<='9'), 'invalid charref');
        ref.charref := ref.charref * 10 + Ord(pc^)-Ord('0');
        Inc(pc);
      end;
    end;
  until (pc^<>'-') and (pc^<>'/'); //что может идти следом в рамках того же ^-блока

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

{ Цепочка вида номер''текст''[номер]номер }
function ParseAdditionalKanjiChain(var pc: PChar): TCharLinkRefChain;
var ref: PCharLinkRef;
  ps: PChar;
begin
  SetLength(Result, 0);
  while pc^<>#00 do begin
    SetLength(Result, Length(Result)+1);
    ref := @Result[Length(Result)-1];

    ref.charref := 0;
    ref.text := '';

    if pc^='''' then begin
      Inc(pc);
      Check(pc^='''');
      Inc(pc);
      ps := pc;
      while (pc^<>#00) and (pc^<>'''') do
        Inc(pc);
      Check(pc^<>#00);
      ref.text := spancopy(ps,pc);
      Check(pc^='''');
      Inc(pc);
      Check(pc^='''');
      Inc(pc);
    end else
   //Допустимые символы
    if (pc^='[') or (pc^=']') then begin
      ref.text := pc^;
      Inc(pc);
    end else
   //Число (номер кандзи)
    begin
      ps := pc;
      while (pc^>='0') and (pc^<='9') do
        Inc(pc);
      Check(pc>ps);
    end;
  end;
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
