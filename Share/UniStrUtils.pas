unit UniStrUtils;
{$WEAKPACKAGEUNIT ON}

interface
uses SysUtils, Windows, StrUtils, WideStrUtils;

(*
 В библиотеке используется тип UnicodeString/UniString.
 На старых компиляторах
   UnicodeString = WideString
 На новых он поддерживается сразу.
   UnicodeString = UnicodeString (и == string, если включено UNICODE)

 Все функции существуют в следующих версиях:
   Function: агностическая функция (для типа string)
   AnsiFunction (FunctionA): версия для AnsiString
   WideFunction (FunctionW): версия для WideString
   UniFunction (FunctionU): версия для UnicodeString (оптимальная)

 Библиотека старается наверстать все упущения Delphi, и добавляет недостающие
 функции:
   Агностические, если в Дельфи они забыты (объявлены, как AnsiFunction).
   Подлинные Ansi, если в Дельфи под этим именем агностическая.
   Подлинные Wide, если таких нет в стандартных библиотеках.
   И оптимальные Uni.

 В виде исключения, если подлинных Wide в дельфи нет, они иногда объявляются
 здесь сразу для UnicodeString.

 Таким образом, UniFunction/WideFunction даёт вам поддержку юникода в наилучшем
 возможном виде, а простая Function работает со строками, которые приняты
 по умолчанию на платформе компиляции.

 Следует помнить, что WideChar == UnicodeChar, и PWideChar в любом случае
 ничуть не отличается от PUnicodeChar. Поэтому функции, которые работают
 с PWideChar, не требуют изменений.

 Используются проверки:
   IFDEF UNICODE: для проверки типа string (Ansi или Unicode)
   IF CompilerVersion>=21: для проверки доступности новых типов и функций

 Например:
   IFDEF UNICODE          => string == UnicodeString       (по умолчанию включен юникод)
   IF CompilerVersion>=21 => UniString == UnicodeString    (юникод ДОСТУПЕН В ПРИНЦИПЕ)
*)

(*
Об оптимизации работы со строками.
Обязательно прочтите, если добавляете функции в библиотеку.

I. Приведение к PWideChar
==============================
@s[1] вызывает UniqueStringU
PWideChar(s) вызывает WCharFromUStr

Поэтому если нужно просто получить указатель, делайте:
  PWideChar(pointer(s))+offset*SizeOf(WideChar)

Это самый быстрый способ (в несколько раз быстрее, никаких вызовов).

II. Length
=============
Зачем-то вызывает UniqueStringU.
Если нужно просто проверить на непустоту, используйте:
  pointer(s) <> nil

III. Const string
=====================
Везде, где входной параметр функции - строка, массив или структура, его надо
объявлять с модификатором const. Это делает вызов функции в несколько раз быстрее.
Если внутри функции вы его всё-таки меняете, создайте доп. переменную.

III. String Format Checking
==============================
Во всех Unicode-Enabled Delphi по умолчанию включено "String Format Checking".
Эта опция делает все операции со строками в несколько раз медленней, отключает
оптимизацию const string и скрывает дикие ошибки. См.:
  http://www.micro-isv.asia/2008/10/needless-string-checks-with-ensureunicodestring/
Её надо отключать везде и всегда.
*)

const
  BOM_UTF16BE: AnsiString = #254#255; //FE FF
  BOM_UTF16LE: AnsiString = #255#254; //FF FE
 //должны быть ansi, иначе получится два юникод-символа

type
 //UnicodeString - это наилучший доступный на платформе Unicode-тип.
 //На новых компиляторах он поддерживается нативно, на старых это WideString.
 {$IF CompilerVersion >= 21}
  UniString = UnicodeString;
  PUniString = PUnicodeString;
 {$ELSE}
  UnicodeString = WideString;
  PUnicodeString = PWideString;
  UniString = UnicodeString;
  PUniString = PUnicodeString;
 {$IFEND}

  UniChar = WideChar;
  PUniChar = PWideChar;

  TAnsiStringArray = array of AnsiString;
  TUniStringArray = array of UniString;
  TStringArrayA = TAnsiStringArray;
  TStringArrayU = TUniStringArray;
 {$IFDEF UNICODE}
  TUnicodeStringArray = TUniStringArray;
  TStringArray = TUniStringArray;
 {$ELSE}
  TStringArray = TAnsiStringArray;
 {$ENDIF}

 //С Wide мы не очень хорошо поступили:
 //возможно, кому-то хочется массив именно WideString.
  TWideStringArray = TUniStringArray;

 //Указатели
  PStringArray = ^TStringArray;
  PAnsiStringArray = ^TAnsiStringArray;
  PWideStringArray = ^TWideStringArray;
  PUniStringArray = ^TUniStringArray;

 //Обратная совместимость
  TStringArrayW = TWideStringArray;

 {$IF CompilerVersion < 21}
 //В старых версиях не объявлены, а ими удобно пользоваться
  UCS2Char = WideChar;
  PUCS2Char = PWideChar;
  UCS4Char = type LongWord;
  PUCS4Char = ^UCS4Char;

  TUCS4CharArray = array [0..$effffff] of UCS4Char;
  PUCS4CharArray = ^TUCS4CharArray;

  UCS4String = array of UCS4Char;

 //На старом компиляторе преобразований кодировки для AnsiString не выполняется,
 //и она безопасна для UTF8 и RawByte как есть (в новых нужно указать флаги)
  UTF8String = AnsiString;
  PUTF8String = ^UTF8String;

  RawByteString = AnsiString;
  PRawByteString = ^RawByteString;
 {$IFEND}


{$IFDEF UNICODE}
(*
  В юникод-версиях Дельфи некоторые Ansi-функции объявлены как UnicodeString.
  Например,
    UpperCase - принимает string и работает только с ASCII
    AnsiUpperCase - принимает string и работает со всеми строками
  То есть, Ansi фактически Uni.

  Само по себе это не страшно (главное помнить не использовать UpperCase),
  но при компиляции Ansi-кода возникают дурацкие варнинги.

  Так что здесь представлены "честные" функции для Ansi-строк.
*)

function AnsiPos(const Substr, S: AnsiString): Integer;

function AnsiStringReplace(const S, OldPattern, NewPattern: AnsiString;
  Flags: TReplaceFlags): AnsiString;
function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString; inline;
function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString; inline;

function AnsiUpperCase(const S: AnsiString): AnsiString;
function AnsiLowerCase(const S: AnsiString): AnsiString;
function AnsiCompareStr(const S1, S2: AnsiString): Integer;
function AnsiSameStr(const S1, S2: AnsiString): Boolean; inline;
function AnsiCompareText(const S1, S2: AnsiString): Integer;
function AnsiSameText(const S1, S2: AnsiString): Boolean; inline;
{$ENDIF}


(*
These are present in SysUtils/StrUtils/WideStrUtils
  function WideUpperCase(const S: WideString): WideString;
  function WideLowerCase(const S: WideString): WideString;
  ...etc

But we have unicode versions (always optimal):
*)

function UStrPCopy(Dest: PUniChar; const Source: UnicodeString): PUniChar; inline;
function UStrPLCopy(Dest: PUniChar; const Source: UnicodeString; MaxLen: Cardinal): PUniChar; inline;

function UniLastChar(const S: UnicodeString): PUniChar; inline;
function UniQuotedStr(const S: UnicodeString; Quote: UniChar): UnicodeString; inline;
function UniExtractQuotedStr(var Src: PUniChar; Quote: UniChar): UnicodeString; inline;
function UniDequotedStr(const S: UnicodeString; AQuote: UniChar): UnicodeString; inline;
function UniAdjustLineBreaks(const S: UnicodeString; Style: TTextLineBreakStyle = tlbsCRLF): UnicodeString; inline;

function UniStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString; inline;
function UniReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;
function UniReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString; inline;

function UniUpperCase(const S: UnicodeString): UnicodeString; inline;
function UniLowerCase(const S: UnicodeString): UnicodeString; inline;
function UniCompareStr(const S1, S2: UnicodeString): Integer; inline;
function UniSameStr(const S1, S2: UnicodeString): Boolean; inline;
function UniCompareText(const S1, S2: UnicodeString): Integer; inline;
function UniSameText(const S1, S2: UnicodeString): Boolean; inline;


(*
 Wide-версии стандартных функций.
 На новых компиляторах функции линкуются к системным. На старых реализованы с нуля.
*)

//remember, this returns the BYTE offset
function WidePos(const Substr, S: UnicodeString): Integer;
function WideMidStr(const AText: UnicodeString; const AStart: integer; const ACount: integer): UnicodeString;

//Логика функций сравнения та же, что у Ansi-версий: сравнение лингвистическое,
//с учётом юникод-цепочек, а не бинарное, по байтам.
function WideStrComp(S1, S2: PWideChar): Integer;
function WideStrIComp(S1, S2: PWideChar): Integer;
function WideStartsStr(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
function WideEndsStr(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
function WideContainsStr(const AText: UnicodeString; const ASubText: UnicodeString): boolean;
function WideStartsText(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
function WideEndsText(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
function WideContainsText(const AText: UnicodeString; const ASubText: UnicodeString): boolean;


(*
  Далее идут вспомогательные функции библиотеки, написанные во всех версиях с нуля.
  Юникод-версии представлены как FunctionW или WideFunction.
*)

//Поиск строки в массивах
//Wide-версии нет, поскольку TWideStringArray ==def== TUniStringArray
function AnsiStrInArray(const a: TAnsiStringArray; const s: AnsiString): integer;
function AnsiTextInArray(const a: TAnsiStringArray; const s: AnsiString): integer;
function UniStrInArray(const a: TUniStringArray; const s: UnicodeString): integer;
function UniTextInArray(const a: TUniStringArray; const s: UnicodeString): integer;
function StrInArray(const a: TStringArray; const s: string): integer;
function TextInArray(const a: TStringArray; const s: string): integer;

//Splits a string by a single type of separators. Ansi version.
function StrSplitA(s: PAnsiChar; sep: AnsiChar): TAnsiStringArray;
function StrSplitW(s: PUniChar; sep: UniChar): TUniStringArray;
function StrSplit(s: PChar; sep: char): TStringArray;

//Same, just with Strings
function AnsiSepSplit(const s: AnsiString; sep: AnsiChar): TAnsiStringArray;
function WideSepSplit(const s: UnicodeString; sep: UniChar): TUniStringArray;
function SepSplit(const s: string; sep: char): TStringArray;

//Бьёт строку по нескольким разделителям, с учётом кавычек
function StrSplitExA(s: PAnsiChar; sep: PAnsiChar; quote: AnsiChar): TAnsiStringArray;
function StrSplitExW(s: PUnichar; sep: PUniChar; quote: UniChar): TUniStringArray;
function StrSplitEx(s: PChar; sep: PChar; quote: Char): TStringArray;

//Joins a string array usng the specified separator
function AnsiSepJoin(const s: TAnsiStringArray; sep: AnsiChar): AnsiString; overload;
function WideSepJoin(const s: TUniStringArray; sep: UniChar): UnicodeString; overload;
function SepJoin(const s: TStringArray; sep: Char): string; overload;

//Same, just receives a point to a first string, and their number
function AnsiSepJoin(s: PAnsiString; cnt: integer; sep: AnsiChar): AnsiString; overload;
function WideSepJoin(s: PUniString; cnt: integer; sep: UniChar): UnicodeString; overload;
function SepJoin(s: PString; cnt: integer; sep: Char): string; overload;

//Возвращает в виде WideString строку PWideChar, но не более N символов
//Полезно для чтения всяких буферов фиксированного размера, где не гарантирован ноль.
function AnsiStrFromBuf(s: PAnsiChar; MaxLen: integer): AnsiString;
function WideStrFromBuf(s: PUniChar; MaxLen: integer): UnicodeString;
function StrFromBuf(s: PChar; MaxLen: integer): string;

//Checks if a char is a number
function AnsiCharIsNumber(c: AnsiChar): boolean; inline;
function WideCharIsNumber(c: UniChar): boolean; inline;
function CharIsNumber(c: char): boolean; inline;

//Check if a char is a latin symbol
function AnsiCharIsLatinSymbol(c: AnsiChar): boolean; inline;
function WideCharIsLatinSymbol(c: UniChar): boolean; inline;
function CharIsLatinSymbol(c: char): boolean; inline;

//Check if a string is composed only from numbers and latin symbols
function AnsiStrIsLnOnly(const str: AnsiString): boolean;
function WideStrIsLnOnly(const str: UnicodeString): boolean;
function StrIsLnOnly(const str: string): boolean;

//These have significantly different implementation when working with Ansi code page,
//so they're implemented only in Unicode yet.
function IsDigit(c: UniChar): boolean;
function IsHiragana(c: UniChar): boolean;
function IsKatakana(c: UniChar): boolean;
function IsKana(c: UniChar): boolean;
function IsKanji(c: UniChar): boolean;
function IsCJKSymbolOrPunctuation(c: UniChar): boolean;
function IsFullWidthCharacter(c: UniChar): boolean;
function ContainsKanji(const s: UniString): boolean;

//Возвращает номер символа, на который указывает ptr, в строке str
function AnsiPcharInd(str, ptr: PAnsiChar): integer;
function WidePcharInd(str, ptr: PUniChar): integer;
function PcharInd(str, ptr: PChar): integer;

//Возвращает длину отрезка PChar в символах.
function CharLenA(p1, p2: PAnsiChar): integer;
function CharLenW(p1, p2: PUniChar): integer;
function CharLen(p1, p2: PChar): integer;

//Находит конец строки
function StrEndA(s: PAnsiChar): PAnsiChar;
function StrEndW(s: PUniChar): PUniChar;
function StrEnd(s: PChar): PChar;

//Быстрое обращение к следующему-предыдущему символу
function NextChar(p: PAnsiChar; c: integer = 1): PAnsiChar; overload;
function PrevChar(p: PAnsiChar; c: integer = 1): PAnsiChar; overload;
function NextChar(p: PWideChar; c: integer = 1): PWideChar; overload;
function PrevChar(p: PWideChar; c: integer = 1): PWideChar; overload;

{ Арифметика указателей }
function PwcOff(var a; n: integer): PWideChar; inline;
function PwcCmp(var a; var b): integer; inline; overload;
function PwcCmp(var a; an: integer; var b; bn: integer): integer; inline; overload;

//Возвращает подстроку с заданного места и нужного размера.
function StrSubLA(beg: PAnsiChar; len: integer): AnsiString;
function StrSubLW(beg: PUniChar; len: integer): UnicodeString;
function StrSubL(beg: PChar; len: integer): string;
//Возвращает подстроку с заданного места и до заданного места не включительно.
function StrSubA(beg: PAnsiChar; en: PAnsiChar): AnsiString;
function StrSubW(beg: PUniChar; en: PUniChar): UnicodeString;
function StrSub(beg: PChar; en: PChar): string;
//Обратная совместимость
function SubStrPchA(beg, en: PAnsiChar): AnsiString;
function SubStrPchW(beg, en: PUniChar): UnicodeString;
function SubStrPch(beg, en: pchar): string;


//Scans the specified string for the specfied character. Starts at position <start_at>.
//Ends at <end_at> - 1 symbols. Returns first symbol index in string or -1 if not found any.
function AnsiFindChar(const s: AnsiString; c: AnsiChar; start_at: integer = 1; end_at: integer = -1): integer;
function WideFindChar(const s: UnicodeString; c: UniChar; start_at: integer = 1; end_at: integer = -1): integer;
function FindChar(const s: string; c: char; start_at: integer = 1; end_at: integer = -1): integer; inline;

//Дополнения к стандартной дельфийской StrScan
function StrScanA(str: PAnsiChar; chr: AnsiChar): PAnsiChar;
function StrScanW(str: PUniChar; chr: UniChar): PUniChar;

//Ищет любой из перечисленных символов, иначе возвращает nil.
function StrScanAnyA(str: PAnsiChar; symbols: PAnsiChar): PAnsiChar;
function StrScanAnyW(str: PUniChar; symbols: PUniChar): PUniChar;
function StrScanAny(str: PChar; symbols: PChar): PChar;

//Ищет любой из перечисленных символов, иначе возвращает указатель на конец строки.
function StrScanEndA(str: PAnsiChar; symbols: PAnsiChar): PAnsiChar;
function StrScanEndW(str: PUniChar; symbols: PUniChar): PWideChar;
function StrScanEnd(str: PChar; symbols: PChar): PChar;

(*
  Обратная совместимость:
   1. Функции StrScanAny раньше назывались StrPosAny.
   2. Функции StrScanDef раньше назывались StrScan.
  Оба переименования совершены с целью унификации названий с Дельфи.
  StrScanAny ведёт себя в точности как StrScan, только для нескольких символов.
*)

//Находит первый символ из набора cs, возвращает ссылку на него и кусок текста до него.
//Если такого символа нет, возвращает остаток строки и nil.
function AnsiReadUpToNext(str: PAnsiChar; const cs: AnsiString; out block: AnsiString): PAnsiChar;
function WideReadUpToNext(str: PUniChar; const cs: UnicodeString; out block: UnicodeString): PUniChar;
function ReadUpToNext(str: PChar; const cs: string; out block: string): PChar;

//Сравнивает строки до окончания любой из них.
function StrCmpNext(a, b: PChar): boolean; inline;

//Возвращает длину совпадающего участка c начала строк, в символах, включая нулевой.
function StrMatch(a, b: PChar): integer; inline;

//Пропускает все символы до первого, не входящего в chars (null-term string)
procedure SkipChars(var pc: PChar; chars: PChar);


//Removes quote characters from around the string, if they exist.
//Duplicate: UniDequotedStr, although this one is more powerful
function AnsiStripQuotes(const s: AnsiString; qc1, qc2: AnsiChar): AnsiString;
function WideStripQuotes(const s: UnicodeString; qc1, qc2: UniChar): UnicodeString;
function StripQuotes(const s: string; qc1, qc2: char): string;

//Удаляет пробелы из конца строки - версия для String
function STrimStartA(const s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimStartW(const s: UnicodeString; sep: UniChar = ' '): UnicodeString;
function STrimStart(const s: string; sep: Char = ' '): string;
function STrimEndA(const s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimEndW(const s: UnicodeString; sep: UniChar = ' '): UnicodeString;
function STrimEnd(const s: string; sep: Char = ' '): string;
function STrimA(const s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimW(const s: UnicodeString; sep: UniChar = ' '): UnicodeString;
function STrim(const s: string; sep: Char = ' '): string;

//Удаляет пробелы из конца строки - версия для PChar
function PTrimStartA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimStartW(s: PUniChar; sep: UniChar = ' '): UnicodeString;
function PTrimStart(s: PChar; sep: Char = ' '): string;
function PTrimEndA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimEndW(s: PUniChar; sep: UniChar = ' '): UnicodeString;
function PTrimEnd(s: PChar; sep: Char = ' '): string;
function PTrimA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimW(s: PUniChar; sep: UniChar = ' '): UnicodeString;
function PTrim(s: PChar; sep: Char = ' '): string;

//Удаляет пробелы из начала и конца строки, заданных прямо.
function BETrimA(beg, en: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function BETrimW(beg, en: PUniChar; sep: UniChar = ' '): UnicodeString;
function BETrim(beg, en: PChar; sep: Char = ' '): string;


{ Binary/string conversion }
//Преобразует данные в цепочку hex-кодов.
function BinToHex(ptr: pbyte; sz: integer): AnsiString;
//Преобразует массив байт в цепочку hex-кодов.
function DataToHex(data: array of byte): AnsiString;
//Декодирует один hex-символ в число от 1 до 16
function HexCharValue(c: AnsiChar): byte; inline;
//Декодирует строку из hex-пар в данные. Место под данные должно быть выделено заранее
procedure HexToBin(const s: AnsiString; p: pbyte; size: integer);


{ Codepage utils }
//Превращает один символ в Wide/Ansi
function ToWideChar(c: AnsiChar; cp: cardinal): WideChar;
function ToChar(c: WideChar; cp: cardinal): AnsiChar;

//Превращает строку в Wide/Ansi
function ToWideString(const s: AnsiString; cp: cardinal): WideString;
function ToString(const s: WideString; cp: cardinal): AnsiString;

//Превращает буфер заданной длины в Wide/Ansi
function BufToWideString(s: PAnsiChar; len: integer; cp: cardinal): WideString;
function BufToString(s: PWideChar; len: integer; cp: cardinal): AnsiString;

//Меняет кодировку Ansi-строки
function Convert(const s: AnsiString; cpIn, cpOut: cardinal): AnsiString;

//Меняет кодировку Ansi-строки с системной на консольную и наоборот
function WinToOEM(const s: AnsiString): AnsiString; inline;
function OEMToWin(const s: AnsiString): AnsiString; inline;


type
 (*
   StringBuilder.
   Сбросьте перед работой с помощью Clear. Добавляйте куски с помощью Add.
   В конце либо используйте, как есть (длина в Used), либо обрежьте с помощью Pack.
 *)
  TAnsiStringBuilder = record
    Data: AnsiString;
    Used: integer;
    procedure Clear;
    function Pack: AnsiString;
    procedure Add(c: AnsiChar); overload;
    procedure Add(pc: PAnsiChar); overload;
    procedure Add(const s: AnsiString); overload;
    procedure Pop(SymbolCount: integer = 1);
  end;
  PAnsiStringBuilder = ^TAnsiStringBuilder;

  TUniStringBuilder = record
    Data: UnicodeString;
    Used: integer;
    procedure Clear;
    function Pack: UnicodeString;
    procedure Add(c: UniChar); overload;
    procedure Add(pc: PUniChar); overload;
    procedure Add(const s: UnicodeString); overload;
    procedure Pop(SymbolCount: integer = 1);
  end;
  PUniStringBuilder = ^TUniStringBuilder;

 //Обратная совместимость
  TWideStringBuilder = TUniStringBuilder;
  PWideStringBuilder = PUniStringBuilder;

 {$IFDEF UNICODE}
  TStringBuilder = TUniStringBuilder;
  PStringBuilder = ^TStringBuilder;
 {$ELSE}
  TStringBuilder = TAnsiStringBuilder;
  PStringBuilder = ^TStringBuilder;
 {$ENDIF}

{ Html encoding }

type
  TUrlEncodeOption = (
    ueNoSpacePlus //Encode space as %20, not "+"
  );
  TUrlEncodeOptions = set of TUrlEncodeOption;

function UrlEncode(const s: UnicodeString; options: TUrlEncodeOptions = []): AnsiString;
function HtmlEscape(const s: UnicodeString): UnicodeString;
function HtmlEscapeObvious(const s: UnicodeString): UnicodeString;
function HtmlEscapeToAnsi(const s: UnicodeString): AnsiString;

implementation

////////////////////////////////////////////////////////////////////////////////
{$IFDEF UNICODE}
(*
  Все реализации скопированы у Борланд.
*)

function AnsiPos(const Substr, S: AnsiString): Integer;
var
  P: PAnsiChar;
begin
  Result := 0;
  P := AnsiStrPos(PAnsiChar(S), PAnsiChar(SubStr));
  if P <> nil then
    Result := (IntPtr(P) - IntPtr(PAnsiChar(S))) div SizeOf(AnsiChar) + 1;
end;

function AnsiStringReplace(const S, OldPattern, NewPattern: AnsiString;
  Flags: TReplaceFlags): AnsiString;
var
  SearchStr, Patt, NewStr: AnsiString;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := AnsiUpperCase(S);
    Patt := AnsiUpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := AnsiPos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function AnsiReplaceStr(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := AnsiStringReplace(AText, AFromText, AToText, [rfReplaceAll]);
end;

function AnsiReplaceText(const AText, AFromText, AToText: AnsiString): AnsiString;
begin
  Result := AnsiStringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;

function AnsiUpperCase(const S: AnsiString): AnsiString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then
    CharUpperBuffA(PAnsiChar(Result), Len);
end;

function AnsiLowerCase(const S: AnsiString): AnsiString;
var
  Len: Integer;
begin
  Len := Length(S);
  SetString(Result, PAnsiChar(S), Len);
  if Len > 0 then
    CharLowerBuffA(PAnsiChar(Result), Len);
end;

function AnsiCompareStr(const S1, S2: AnsiString): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, 0, PAnsiChar(S1), Length(S1),
      PAnsiChar(S2), Length(S2)) - CSTR_EQUAL;
end;

function AnsiSameStr(const S1, S2: AnsiString): Boolean;
begin
  Result := AnsiCompareStr(S1, S2) = 0;
end;

function AnsiCompareText(const S1, S2: AnsiString): Integer;
begin
  Result := CompareStringA(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PAnsiChar(S1),
    Length(S1), PAnsiChar(S2), Length(S2)) - CSTR_EQUAL;
end;

function AnsiSameText(const S1, S2: AnsiString): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;
{$ENDIF}


(*
  Unicode versions of WideStrUtils functions.
*)

function UStrPCopy(Dest: PUniChar; const Source: UnicodeString): PUniChar;
begin
 {Copied from WideStrUtils}
  Result := WStrLCopy(Dest, PWideChar(Source), Length(Source));
end;

function UStrPLCopy(Dest: PUniChar; const Source: UnicodeString; MaxLen: Cardinal): PUniChar;
begin
 {Copied from WideStrUtils}
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;

function UniLastChar(const S: UnicodeString): PUniChar;
begin
 {Copied from WideStrUtils for speed}
  if S = '' then
    Result := nil
  else
    Result := @S[Length(S)];
end;

function UniQuotedStr(const S: UnicodeString; Quote: UniChar): UnicodeString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of QuotedStr. This one works for "strings" on Unicode.
  Result := AnsiQuotedStr(S, Quote);
{$ELSE}
  Result := WideQuotedStr(S, Quote);
{$ENDIF}
end;

function UniExtractQuotedStr(var Src: PUniChar; Quote: UniChar): UnicodeString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of ExtractQuotedStr.
  Result := AnsiExtractQuotedStr(Src, Quote);
{$ELSE}
  Result := WideExtractQuotedStr(Src, Quote);
{$ENDIF}
end;

function UniDequotedStr(const S: UnicodeString; AQuote: UniChar): UnicodeString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of DequotedStr.
  Result := AnsiDequotedStr(S, AQuote);
{$ELSE}
  Result := WideDequotedStr(S, AQuote);
{$ENDIF}
end;

function UniAdjustLineBreaks(const S: UnicodeString; Style: TTextLineBreakStyle = tlbsCRLF): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := AdjustLineBreaks(S, Style);
{$ELSE}
  Result := WideAdjustLineBreaks(S, Style);
{$ENDIF}
end;

function UniStringReplace(const S, OldPattern, NewPattern: UnicodeString;
  Flags: TReplaceFlags): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
{$ELSE}
  Result := WideStringReplace(S, OldPattern, NewPattern, Flags);
{$ENDIF}
end;

function UniReplaceStr(const AText, AFromText, AToText: UnicodeString): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := ReplaceStr(AText, AFromText, AToText);
{$ELSE}
  Result := WideReplaceStr(AText, AFromText, AToText);
{$ENDIF}
end;

function UniReplaceText(const AText, AFromText, AToText: UnicodeString): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := ReplaceText(AText, AFromText, AToText);
{$ELSE}
  Result := WideReplaceText(AText, AFromText, AToText);
{$ENDIF}
end;

(*
Note about UpperCase/LowerCase:
  1. SysUtils.UpperCase sucks and works only with dirty ANSI, even on unicode. Don't use it!
  2. SysUtils.AnsiUpperCase works for Unicode on Unicode compilers.
  3. UniStrUtils.AnsiUpperCase works as pure, but proper Ansi/multibyte, everywhere.
*)
function UniUpperCase(const S: UnicodeString): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := SysUtils.AnsiUpperCase(S);
{$ELSE}
  Result := WideUpperCase(S);
{$ENDIF}
end;

function UniLowerCase(const S: UnicodeString): UnicodeString;
begin
{$IFDEF UNICODE}
  Result := SysUtils.AnsiLowerCase(S);
{$ELSE}
  Result := WideLowerCase(S);
{$ENDIF}
end;

function UniCompareStr(const S1, S2: UnicodeString): Integer;
begin
{$IFDEF UNICODE}
  Result := CompareStr(S1, S2);
{$ELSE}
  Result := WideCompareStr(S1, S2);
{$ENDIF}
end;

function UniSameStr(const S1, S2: UnicodeString): Boolean;
begin
{$IFDEF UNICODE}
  Result := SameStr(S1, S2);
{$ELSE}
  Result := WideSameStr(S1, S2);
{$ENDIF}
end;

function UniCompareText(const S1, S2: UnicodeString): Integer;
begin
{$IFDEF UNICODE}
  Result := CompareText(S1, S2);
{$ELSE}
  Result := WideCompareText(S1, S2);
{$ENDIF}
end;

function UniSameText(const S1, S2: UnicodeString): Boolean;
begin
{$IFDEF UNICODE}
  Result := SameText(S1, S2);
{$ELSE}
  Result := WideSameText(S1, S2);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//Wide versions of standard routines.
//На новых компиляторах все функции линкуются к системным. На старых реализованы с нуля.
//Ansi-версии и дефолтные версии присутствовали и присутствуют в хедерах.

function WidePos(const Substr, S: UnicodeString): Integer;
{$IFDEF UNICODE}
begin
  Result := Pos(SubStr, S);
end;
{$ELSE}
var
  P: PWideChar;
begin
  Result := 0;
  P := WStrPos(PWideChar(S), PWideChar(SubStr));
  if P <> nil then
    Result := IntPtr(P) - IntPtr(PWideChar(S)) + 1;
end;
{$ENDIF}

//Returns substring of s, starting at <start> characters and continuing <length> of them.
function WideMidStr(const AText: UnicodeString; const AStart: integer; const ACount: integer): UnicodeString;
{$IFDEF UNICODE}
begin
  Result := StrUtils.MidStr(AText, AStart, ACount);
end;
{$ELSE}
begin
  SetLength(Result, ACount);
  if ACount <= Length(AText) - AStart then
   //If there's enough symbols in s, copy len of them
    Move(AText[AStart], Result[1], ACount*SizeOf(AText[1]))
  else
   //Else just copy everything that we can
    Move(AText[AStart], Result[1], (Length(AText)-AStart)*SizeOf(AText[1]));
end;
{$ENDIF}

function WideStrComp(S1, S2: PWideChar): Integer;
begin
{$IFDEF UNICODE}
 //На Unicode-компиляторах эта функция существует под Ansi-названием в Wide-конфигурации.
  Result := AnsiStrComp(S1, S2);
{$ELSE}
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, S1, -1, S2, -1) - 2;
{$ENDIF}
end;

function WideStrIComp(S1, S2: PWideChar): Integer;
begin
{$IFDEF UNICODE}
 //На Unicode-компиляторах эта функция существует под Ansi-названием в Wide-конфигурации.
  Result := AnsiStrIComp(S1, S2);
{$ELSE}
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, -1,
    S2, -1) - 2;
{$ENDIF}
end;

function WideStartsStr(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
{$IFDEF UNICODE}
begin
  Result := StrUtils.StartsStr(ASubText, AText);
end;
{$ELSE}
begin
  if Length(ASubText) > Length(AText) then
    Result := false
  else
   //Сравниваем только начало
    Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
      PWideChar(ASubText), -1,  PWideChar(AText), Length(ASubText)) = 2;
end;
{$ENDIF}

function WideEndsStr(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
{$IFDEF UNICODE}
begin
  Result := StrUtils.EndsStr(ASubText, AText);
end;
{$ELSE}
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') then
    Result := WideStrComp(Pointer(ASubText), Pointer(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;
{$ENDIF}

//Аргументы в обратном порядке, как и у AnsiContainsStr
function WideContainsStr(const AText: UnicodeString; const ASubText: UnicodeString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.ContainsStr(AText, ASubText);
{$ELSE}
  Result := (WStrPos(PWideChar(AText), PWideChar(ASubText)) <> nil);
{$ENDIF}
end;

function WideStartsText(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.StartsText(ASubText, AText);
{$ELSE}
  Result := WideStartsStr(WideLowerCase(ASubText), WideLowerCase(AText));
{$ENDIF}
end;

function WideEndsText(const ASubText: UnicodeString; const AText: UnicodeString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.EndsText(ASubText, AText);
{$ELSE}
  Result := WideEndsStr(WideLowerCase(ASubText), WideLowerCase(AText));
{$ENDIF}
end;

//Аргументы в обратном порядке, как и у AnsiContainsText
function WideContainsText(const AText: UnicodeString; const ASubText: UnicodeString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.StartsText(AText, ASubText);
{$ELSE}
  Result := WideContainsStr(WideLowerCase(AText), WideLowerCase(ASubText));
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Находит индекс первого появления строки в массиве

function AnsiStrInArray(const a: TAnsiStringArray; const s: AnsiString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(a)-1 do
    if AnsiSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function AnsiTextInArray(const a: TAnsiStringArray; const s: AnsiString): integer;
var i: integer;
  ts: AnsiString;
begin
  Result := -1;
  ts := AnsiUpperCase(s);
  for i := 0 to Length(a)-1 do
    if AnsiSameStr(ts, a[i]) then begin
      Result := i;
      break;
    end;
end;

function UniStrInArray(const a: TUniStringArray; const s: UnicodeString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(a)-1 do
    if UniSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function UniTextInArray(const a: TUniStringArray; const s: UnicodeString): integer;
var i: integer;
  ts: UnicodeString;
begin
  Result := -1;
  ts := UniUpperCase(s);
  for i := 0 to Length(a)-1 do
    if UniSameStr(ts, a[i]) then begin
      Result := i;
      break;
    end;
end;

function StrInArray(const a: TStringArray; const s: string): integer;
begin
{$IFDEF UNICODE}
  Result := UniStrInArray(a, s);
{$ELSE}
  Result := AnsiStrInArray(a, s);
{$ENDIF}
end;

function TextInArray(const a: TStringArray; const s: string): integer;
begin
{$IFDEF UNICODE}
  Result := UniTextInArray(a, s);
{$ELSE}
  Result := AnsiTextInArray(a, s);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
(*
  Splits a string by a single type of separators.
  A,,,B => five items (A,,,B)
*)

function StrSplitA(s: PAnsiChar; sep: AnsiChar): TAnsiStringArray;
var pc: PAnsiChar;
  i: integer;
begin
 //Count the number of separator characters
  i := 1;
  pc := s;
  while pc^ <> #00 do begin
    if pc^=sep then
      Inc(i);
    Inc(pc);
  end;

 //Reserve array
  SetLength(Result, i);

 //Parse
  i := 0;
  pc := s;
  while pc^<>#00 do
    if pc^=sep then begin
      Result[i] := StrSubA(s, pc);
      Inc(i);
      Inc(pc);
      s := pc;
    end else
      Inc(pc);

 //Last time
  Result[i] := StrSubA(s, pc);
end;

function StrSplitW(s: PUniChar; sep: UniChar): TUniStringArray;
var pc: PUniChar;
  i: integer;
begin
 //Count the number of separator characters
  i := 1;
  pc := s;
  while pc^ <> #00 do begin
    if pc^=sep then
      Inc(i);
    Inc(pc);
  end;

 //Reserve array
  SetLength(Result, i);

 //Parse
  i := 0;
  pc := s;
  while pc^<>#00 do
    if pc^=sep then begin
      Result[i] := StrSubW(s, pc);
      Inc(i);
      Inc(pc);
      s := pc;
    end else
      Inc(pc);

 //Last time
  Result[i] := StrSubW(s, pc);
end;

function StrSplit(s: PChar; sep: char): TStringArray;
begin
{$IFDEF UNICODE}
  Result := StrSplitW(PWideChar(s), sep);
{$ELSE}
  Result := StrSplitA(PAnsiChar(s), sep);
{$ENDIF}
end;

function AnsiSepSplit(const s: AnsiString; sep: AnsiChar): TAnsiStringArray;
begin
  Result := StrSplitA(PAnsiChar(s), sep);
end;

function WideSepSplit(const s: UnicodeString; sep: UniChar): TUniStringArray;
begin
  Result := StrSplitW(PWideChar(s), sep);
end;

function SepSplit(const s: string; sep: char): TStringArray;
begin
{$IFDEF UNICODE}
  Result := StrSplitW(PWideChar(s), sep);
{$ELSE}
  Result := StrSplitA(PAnsiChar(s), sep);
{$ENDIF}
end;


function StrSplitExA(s: PAnsiChar; sep: PAnsiChar; quote: AnsiChar): TAnsiStringArray;
var pc: PAnsiChar;
  i: integer;
  in_q: boolean;

  function match_q(c: AnsiChar): boolean;
  var sc: PAnsiChar;
  begin
    sc := sep;
    while (sc^<>#00) and (sc^<>c) do Inc(sc);
    Result := (sc^=c);
  end;

begin
 //Count the number of separator characters not between
  i := 1;
  pc := s;
  in_q := false;
  while pc^ <> #00 do begin
    if pc^=quote then
      in_q := not in_q
    else
    if (not in_q) and match_q(pc^) then
      Inc(i);
    Inc(pc);
  end;

 //Reserve array
  SetLength(Result, i);

 //Parse
  i := 0;
  pc := s;
  in_q := false;
  while pc^<>#00 do begin
    if pc^=quote then begin
      in_q := not in_q;
      Inc(pc);
    end else
    if (not in_q) and match_q(pc^) then begin
      Result[i] := StrSubA(s, pc);
      Inc(i);
      Inc(pc);
      s := pc;
    end else
      Inc(pc);
  end;

 //Last time
  Result[i] := StrSubA(s, pc);
end;

function StrSplitExW(s: PUnichar; sep: PUniChar; quote: UniChar): TUniStringArray;
var pc: PUniChar;
  i: integer;
  in_q: boolean;

  function match_q(c: UniChar): boolean;
  var sc: PUniChar;
  begin
    sc := sep;
    while (sc^<>#00) and (sc^<>c) do Inc(sc);
    Result := (sc^=c);
  end;

begin
 //Count the number of separator characters not between
  i := 1;
  pc := s;
  in_q := false;
  while pc^ <> #00 do begin
    if pc^=quote then
      in_q := not in_q
    else
    if (not in_q) and match_q(pc^) then
      Inc(i);
    Inc(pc);
  end;

 //Reserve array
  SetLength(Result, i);

 //Parse
  i := 0;
  pc := s;
  in_q := false;
  while pc^<>#00 do begin
    if pc^=quote then begin
      in_q := not in_q;
      Inc(pc);
    end else
    if (not in_q) and match_q(pc^) then begin
      Result[i] := StrSubW(s, pc);
      Inc(i);
      Inc(pc);
      s := pc;
    end else
      Inc(pc);
  end;

 //Last time
  Result[i] := StrSubW(s, pc);
end;

function StrSplitEx(s: PChar; sep: PChar; quote: Char): TStringArray;
begin
{$IFDEF UNICODE}
  Result := StrSplitExW(PWideChar(s), PWideChar(sep), quote);
{$ELSE}
  Result := StrSplitExA(PAnsiChar(s), PAnsiChar(sep), quote);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//Joins a string array usng the specified separator.

function AnsiSepJoin(const s: TAnsiStringArray; sep: AnsiChar): AnsiString;
begin
  Result := AnsiSepJoin(@s[1], Length(s), sep);
end;

function WideSepJoin(const s: TUniStringArray; sep: UniChar): UnicodeString;
begin
  Result := WideSepJoin(@s[1], Length(s), sep);
end;

function SepJoin(const s: TStringArray; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := WideSepJoin(@s[1], Length(s), sep);
{$ELSE}
  Result := AnsiSepJoin(@s[1], Length(s), sep);
{$ENDIF}
end;

function AnsiSepJoin(s: PAnsiString; cnt: integer; sep: AnsiChar): AnsiString;
var si: PAnsiString;
  ci: integer;
  len, li: integer;
begin
 //Считаем общий размер
  len := cnt - 1; //число разделителей
  si := s;
  ci := cnt;
  while ci>0 do begin
    len := len + Length(si^);
    Inc(si);
    Dec(ci);
  end;

 //Выделяем память
  SetLength(Result, len);
  li := 1;
  while cnt>1 do begin
    Move(s^[1], Result[li], Length(s^)*SizeOf(AnsiChar));
    li := li + Length(s^);
    Result[li] := sep;
    li := li + 1;
    Inc(s);
    Dec(cnt);
  end;

 //Последний кусок
  if cnt >= 1 then
    Move(s^[1], Result[li], Length(s^)*SizeOf(AnsiChar));
end;

function WideSepJoin(s: PUniString; cnt: integer; sep: UniChar): UnicodeString;
var si: PUniString;
  ci: integer;
  len, li: integer;
begin
 //Считаем общий размер
  len := cnt - 1; //число разделителей
  si := s;
  ci := cnt;
  while ci>0 do begin
    len := len + Length(si^);
    Inc(si);
    Dec(ci);
  end;

 //Выделяем память
  SetLength(Result, len);
  li := 1;
  while cnt>1 do begin
    Move(s^[1], Result[li], Length(s^)*SizeOf(UniChar));
    li := li + Length(s^);
    Result[li] := sep;
    li := li + 1;
    Inc(s);
    Dec(cnt);
  end;

 //Последний кусок
  if cnt >= 1 then
    Move(s^[1], Result[li], Length(s^)*SizeOf(UniChar));
end;

function SepJoin(s: PString; cnt: integer; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := WideSepJoin(s, cnt, sep);
{$ELSE}
  Result := AnsiSepJoin(s, cnt, sep);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
//Возвращает в виде WideString строку PWideChar, но не более N символов
//Полезно для чтения всяких буферов фиксированного размера, где не гарантирован ноль.

function AnsiStrFromBuf(s: PAnsiChar; MaxLen: integer): AnsiString;
var p: PAnsiChar;
begin
  p := s;
  while (p^ <> #00) and (MaxLen > 0) do begin
    Inc(p);
    Dec(MaxLen);
  end;

 //p указывает на символ, копировать который уже не надо
  Result := SubStrPchA(s, p);
end;

function WideStrFromBuf(s: PUniChar; MaxLen: integer): UnicodeString;
var p: PWideChar;
begin
  p := s;
  while (p^ <> #00) and (MaxLen > 0) do begin
    Inc(p);
    Dec(MaxLen);
  end;

 //p указывает на символ, копировать который уже не надо
  Result := SubStrPchW(s, p);
end;

function StrFromBuf(s: PChar; MaxLen: integer): string;
begin
{$IFDEF UNICODE}
  Result := WideStrFromBuf(s, MaxLen);
{$ELSE}
  Result := AnsiStrFromBuf(s, MaxLen);
{$ENDIF}
end;



////////////////////////////////////////////////////////////////////////////////
///   Character checks

//Checks if char is a number
function AnsiCharIsNumber(c: AnsiChar): boolean;
begin
  Result := (Ord(c) >= Ord('0')) and (Ord(c) <= Ord('9'));
end;

function WideCharIsNumber(c: UniChar): boolean;
begin
  Result := (Ord(c) >= Ord('0')) and (Ord(c) <= Ord('9'));
end;

function CharIsNumber(c: char): boolean;
begin
  Result := (Ord(c) >= Ord('0')) and (Ord(c) <= Ord('9'));
end;

//Checks if char is a latin symbol
function AnsiCharIsLatinSymbol(c: AnsiChar): boolean;
begin
  Result := ((Ord(c) >= Ord('A')) and (Ord(c) <= Ord('Z')))
         or ((Ord(c) >= Ord('a')) and (Ord(c) <= Ord('z')));
end;

function WideCharIsLatinSymbol(c: UniChar): boolean;
begin
  Result := ((Ord(c) >= Ord('A')) and (Ord(c) <= Ord('Z')))
         or ((Ord(c) >= Ord('a')) and (Ord(c) <= Ord('z')));
end;

function CharIsLatinSymbol(c: char): boolean;
begin
  Result := ((Ord(c) >= Ord('A')) and (Ord(c) <= Ord('Z')))
         or ((Ord(c) >= Ord('a')) and (Ord(c) <= Ord('z')));
end;


//Check if string is composed only from numbers and latin symbols
function AnsiStrIsLnOnly(const str: AnsiString): boolean;
var i: integer;
begin
  Result := true;
  for i := 1 to Length(str) do
    if not AnsiCharIsNumber(str[i])
    and not AnsiCharIsLatinSymbol(str[i]) then begin
      Result := false;
      exit;
    end;
end;

function WideStrIsLnOnly(const str: UnicodeString): boolean;
var i: integer;
begin
  Result := true;
  for i := 1 to Length(str) do
    if not WideCharIsNumber(str[i])
    and not WideCharIsLatinSymbol(str[i]) then begin
      Result := false;
      exit;
    end;
end;

function StrIsLnOnly(const str: string): boolean;
begin
{$IFDEF UNICODE}
  Result := WideStrIsLnOnly(str);
{$ELSE}
  Result := AnsiStrIsLnOnly(str);
{$ENDIF}
end;


function IsDigit(c: UniChar): boolean;
begin
  Result := (c>='0') and (c<='9');
end;

(*
 Following stuff isn't precise.
 1. We don't include repetition marks etc in Hiragana/Katakana
 2. There are both unique to H/K and shared H-K marks
 3. Unicode includes copies of H/K with effects:
   - Normal [included]
   - Small [included]
   - Circled
   - Other?
 4. Unicode has various special symbols for KIROGURAMU in H/K, for instance. Not included.
 5. Kanjis are included only from Basic Plane.
*)

function IsHiragana(c: UniChar): boolean;
begin
  Result := (Ord(c) >= $3040) and (Ord(c) <= $309F);
end;

function IsKatakana(c: UniChar): boolean;
begin
  Result := ((Ord(c) >= $30A0) and (Ord(c) <= $30FF))
         or ((Ord(c) >= $31F0) and (Ord(c) <= $31FF)); //katakana phonetic extensions
end;

function IsKana(c: UniChar): boolean;
begin
  Result := IsKatakana(c) or IsHiragana(c);
end;

function IsKanji(c: UniChar): boolean;
begin
  Result := ((Ord(c) >= $4E00) and (Ord(c) <= $9FFF))  //CJK Unified Ideographs
         or ((Ord(c) >= $F900) and (Ord(c) <= $FAFF)); //CJK Compatibility Ideographs
end;

//Символ относится к диапазону "CJK Symbols and Punctuation"
function IsCJKSymbolOrPunctuation(c: UniChar): boolean;
begin
  Result := (Ord(c) >= $3000) and (Ord(c) <= $303F);
end;

//Символ относится к диапазонам "Fullwidth Characters"
function IsFullWidthCharacter(c: UniChar): boolean;
begin
  Result := (Ord(c) >= $FF01) and (Ord(c) <= $FF5E)
         or (Ord(c) >= $FFE0) and (Ord(c) <= $FFE6);
end;

function ContainsKanji(const s: UniString): boolean;
var i: integer;
begin
  Result := false;
  for i := 1 to Length(s) do
    if IsKanji(s[i]) then begin
      Result := true;
      break;
    end;
end;

////////////////////////////////////////////////////////////////////////////////
///  Indexing and lengths

// Возвращает номер символа, на который указывает ptr, в строке str
function AnsiPcharInd(str, ptr: PAnsiChar): integer;
begin
  Result := IntPtr(ptr)-IntPtr(str)+1;
end;

function WidePcharInd(str, ptr: PUniChar): integer;
begin
  Result := (IntPtr(ptr)-IntPtr(str)) div SizeOf(UniChar) + 1;
end;

function PcharInd(str, ptr: PChar): integer;
begin
  Result := (IntPtr(ptr)-IntPtr(str)) div SizeOf(char) + 1;
end;


//Возвращает длину отрезка в символах
function CharLenA(p1, p2: PAnsiChar): integer;
begin
  Result := (cardinal(p2) - cardinal(p1)) div SizeOf(AnsiChar);
end;

function CharLenW(p1, p2: PUniChar): integer;
begin
  Result := (cardinal(p2) - cardinal(p1)) div SizeOf(UniChar);
end;

function CharLen(p1, p2: PChar): integer;
begin
  Result := (cardinal(p2) - cardinal(p1)) div SizeOf(Char);
end;


//Находит конец строки
function StrEndA(s: PAnsiChar): PAnsiChar;
begin
  Result := s;
  while Result^ <> #00 do Inc(Result);
end;

function StrEndW(s: PUniChar): PUniChar;
begin
  Result := s;
  while Result^ <> #00 do Inc(Result);
end;

function StrEnd(s: PChar): PChar;
begin
  Result := s;
  while Result^ <> #00 do Inc(Result);
end;

////////////////////////////////////////////////////////////////////////////////
/// Быстрое обращение к следующему-предыдущему символу

function NextChar(p: PAnsiChar; c: integer = 1): PAnsiChar;
begin
  Result := PAnsiChar(IntPtr(p) + SizeOf(AnsiChar)*c);
end;

function PrevChar(p: PAnsiChar; c: integer = 1): PAnsiChar;
begin
  Result := PAnsiChar(IntPtr(p) - SizeOf(AnsiChar)*c);
end;

function NextChar(p: PWideChar; c: integer = 1): PWideChar;
begin
  Result := PWideChar(IntPtr(p) + SizeOf(WideChar)*c);
end;

function PrevChar(p: PWideChar; c: integer = 1): PWideChar;
begin
  Result := PWideChar(IntPtr(p) - SizeOf(WideChar)*c);
end;


////////////////////////////////////////////////////////////////////////////////
///  Арифметика указателей

//Возвращает указатель на n-й знак строки. Быстрее и безопасней, чем дельфийская хрень.
//Отступ считает с единицы.
function PwcOff(var a; n: integer): PWideChar;
begin
  Result := PWideChar(IntPtr(a) + (n-1)*SizeOf(WideChar));
end;

//Сравнивает указатели. Больше нуля, если a>b.
function PwcCmp(var a; var b): integer;
begin
  Result := IntPtr(a)-IntPtr(b);
end;

//Отступ считает с единицы.
function PwcCmp(var a; an: integer; var b; bn: integer): integer;
begin
  Result := IntPtr(a)+(an-1)*SizeOf(WideChar)-IntPtr(b)-(bn-1)*SizeOf(WideChar);
end;


////////////////////////////////////////////////////////////////////////////////
///  Возвращает строку между заданными позициями, или заданной длины

function StrSubLA(beg: PAnsiChar; len: integer): AnsiString;
var i: integer;
begin
  SetLength(Result, len);
  i := 1;
  while i <= len do begin
    Result[i] := beg^;
    Inc(beg);
    Inc(i);
  end;
end;

function StrSubLW(beg: PUniChar; len: integer): UnicodeString;
var i: integer;
begin
  SetLength(Result, len);
  i := 1;
  while i <= len do begin
    Result[i] := beg^;
    Inc(beg);
    Inc(i);
  end;
end;

function StrSubL(beg: PChar; len: integer): string;
begin
{$IFDEF UNICODE}
  Result := StrSubLW(beg, len);
{$ELSE}
  Result := StrSubLA(beg, len);
{$ENDIF}
end;

function StrSubA(beg: PAnsiChar; en: PAnsiChar): AnsiString;
begin
  Result := StrSubLA(beg, (IntPtr(en)-IntPtr(beg)) div SizeOf(AnsiChar));
end;

function StrSubW(beg: PUniChar; en: PUniChar): UnicodeString;
begin
  Result := StrSubLW(beg, (IntPtr(en)-IntPtr(beg)) div SizeOf(WideChar));
end;

function StrSub(beg: PChar; en: PChar): string;
begin
{$IFDEF UNICODE}
  Result := StrSubW(beg, en);
{$ELSE}
  Result := StrSubA(beg, en);
{$ENDIF}
end;

//Обратная совместимость
function SubStrPchA(beg, en: PAnsiChar): AnsiString;
begin
  Result := StrSubA(beg, en);
end;

function SubStrPchW(beg, en: PUniChar): UnicodeString;
begin
  Result := StrSubW(beg, en);
end;

function SubStrPch(beg, en: pchar): string;
begin
//Редиректим сразу на нужные функции, без проводников
{$IFDEF UNICODE}
  Result := StrSubW(beg, en);
{$ELSE}
  Result := StrSubA(beg, en);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Поиск символов в строке

//Scans the specified string for the specfied character. Starts at position <start_at>.
//Ends at <end_at> symbols - 1. Returns first symbol index in string or -1 if not found any.
function AnsiFindChar(const s: AnsiString; c: AnsiChar; start_at: integer; end_at: integer): integer;
var i: integer;
begin
  if end_at=-1 then
    end_at := Length(s);

  Result := -1;
  for i := start_at to end_at - 1 do
    if s[i]=c then begin
      Result := i;
      exit;
    end;
end;

function WideFindChar(const s: UnicodeString; c: UniChar; start_at: integer; end_at: integer): integer;
var i: integer;
begin
  if end_at=-1 then
    end_at := Length(s);

  Result := -1;
  for i := start_at to end_at - 1 do
    if s[i]=c then begin
      Result := i;
      exit;
    end;
end;

function FindChar(const s: string; c: char; start_at: integer; end_at: integer): integer;
begin
{$IFDEF UNICODE}
  Result := WideFindChar(s, c, start_at, end_at);
{$ELSE}
  Result := AnsiFindChar(s, c, start_at, end_at);
{$ENDIF}
end;

//Дополнения к стандартной дельфийской StrScan
function StrScanA(str: PAnsiChar; chr: AnsiChar): PAnsiChar;
begin
  Result := StrScan(str, chr);
end;

function StrScanW(str: PUniChar; chr: UniChar): PUniChar;
begin
{$IFDEF UNICODE}
  Result := StrScan(str, chr); //has unicode version
{$ELSE}
 { Copied from SysUtils }
  Result := Str;
  while Result^ <> #0 do
  begin
    if Result^ = Chr then
      Exit;
    Inc(Result);
  end;
  if Chr <> #0 then
    Result := nil;
{$ENDIF}
end;

//Ищет любой из перечисленных символов, иначе возвращает nil.
function StrScanAnyA(str: PAnsiChar; symbols: PAnsiChar): PAnsiChar;
var smb: PAnsiChar;
begin
  Result := nil;
  while str^ <> #00 do begin
    smb := symbols;
    while smb^ <> #00 do
      if smb^ = str^ then begin
        Result := str;
        exit;
      end else
        Inc(smb);
    Inc(str);
  end;
end;

function StrScanAnyW(str: PUniChar; symbols: PUniChar): PUniChar;
var smb: PWideChar;
begin
  Result := nil;
  while str^ <> #00 do begin
    smb := symbols;
    while smb^ <> #00 do
      if smb^ = str^ then begin
        Result := str;
        exit;
      end else
        Inc(smb);
    Inc(str);
  end;
end;

function StrScanAny(str: PChar; symbols: PChar): PChar;
begin
{$IFDEF UNICODE}
  Result := StrScanAnyW(str, symbols);
{$ELSE}
  Result := StrScanAnyA(str, symbols);
{$ENDIF}
end;

//Ищет любой из перечисленных символов, иначе возвращает указатель на конец строки.
//Для скорости алгоритмы скопированы из StrScanAny
function StrScanEndA(str: PAnsiChar; symbols: PAnsiChar): PAnsiChar;
var smb: PAnsiChar;
begin
  while str^ <> #00 do begin
    smb := symbols;
    while smb^ <> #00 do
      if smb^ = str^ then begin
        Result := str;
        exit;
      end else
        Inc(smb);
    Inc(str);
  end;

  //If nothing is found, return endstr
  Result := str;
end;

function StrScanEndW(str: PUniChar; symbols: PUniChar): PUniChar;
var smb: PWideChar;
begin
  while str^ <> #00 do begin
    smb := symbols;
    while smb^ <> #00 do
      if smb^ = str^ then begin
        Result := str;
        exit;
      end else
        Inc(smb);
    Inc(str);
  end;

  //If nothing is found, return endstr
  Result := str;
end;

function StrScanEnd(str: PChar; symbols: PChar): PChar;
begin
{$IFDEF UNICODE}
  Result := StrScanEndW(str, symbols);
{$ELSE}
  Result := StrScanEndA(str, symbols);
{$ENDIF}
end;


//Находит первый символ из набора cs, возвращает ссылку на него и кусок текста до него.
//Если такого символа нет, возвращает остаток строки и nil.
function AnsiReadUpToNext(str: PAnsiChar; const cs: AnsiString; out block: AnsiString): PAnsiChar;
begin
  Result := StrScanAnyA(str, PAnsiChar(cs));
  if Result <> nil then
    SetLength(block, AnsiPcharInd(str, Result)-1)
  else
    SetLength(block, StrLen(str));
  if Length(block) > 0 then
    StrLCopy(@block[1], str, Length(block)); //null not included
end;

//Находит первый символ из набора cs, возвращает ссылку на него и кусок текста до него.
//Если такого символа нет, возвращает остаток строки и nil.
function WideReadUpToNext(str: PUniChar; const cs: UnicodeString; out block: UnicodeString): PUniChar;
begin
  Result := StrScanAnyW(str, PWideChar(cs));
  if Result <> nil then
    SetLength(block, WidePCharInd(str, Result)-1)
  else
    SetLength(block, WStrLen(str));
  if Length(block) > 0 then
    WStrLCopy(@block[1], str, Length(block)); //null not included
end;

function ReadUpToNext(str: PChar; const cs: string; out block: string): PChar;
begin
{$IFDEF UNICODE}
  Result := WideReadUpToNext(str, cs, block);
{$ELSE}
  Result := AnsiReadUpToNext(str, cs, block);
{$ENDIF}
end;

//Проверяет, что строки совпадают до окончания одной из них
function StrCmpNext(a, b: PChar): boolean;
begin
  while (a^ = b^) and (a^<>#00) do begin //#00 не выедаем даже общий
    Inc(a);
    Inc(b);
  end;
  Result := (a^=#00) or (b^=#00);
end;

//Возвращает длину совпадающего участка c начала строк, в символах, включая нулевой.
function StrMatch(a, b: PChar): integer;
begin
  Result := 0;
  while (a^ = b^) and (a^<>#00) do begin //#00 не выедаем даже общий
    Inc(a);
    Inc(b);
    Inc(Result);
  end;
 //сверяем #00
  if (a^=b^) then Inc(Result);
end;

procedure SkipChars(var pc: PChar; chars: PChar);
var pcc: PChar;
begin
  while pc^<>#00 do begin
    pcc := chars;
    while pcc^<>#00 do
      if pcc^=pc^ then begin
        pcc := nil;
        break;
      end else
        Inc(pcc);
    if pcc=nil then //skip char
      Inc(pc)
    else
      exit; //non-skip char
  end;
end;

////////////////////////////////////////////////////////////////////////////////
///  Удаление лишних символов по краям

//Removes quote characters around the string, if they exist.
function AnsiStripQuotes(const s: AnsiString; qc1, qc2: AnsiChar): AnsiString;
begin
  Result := s;
  if Length(Result) < 2 then exit;

 //Если кавычки есть, убираем их.
  if (Result[1]=qc1) and (Result[Length(Result)-1]=qc2) then begin
    Move(Result[2], Result[1], (Length(Result)-2)*SizeOf(Result[1]));
    SetLength(Result, Length(Result)-2);
  end;
end;

function WideStripQuotes(const s: UnicodeString; qc1, qc2: UniChar): UnicodeString;
begin
  Result := s;
  if Length(Result) < 2 then exit;

 //Если кавычки есть, убираем их.
  if (Result[1]=qc1) and (Result[Length(Result)-1]=qc2) then begin
    Move(Result[2], Result[1], (Length(Result)-2)*SizeOf(Result[1]));
    SetLength(Result, Length(Result)-2);
  end;
end;

function StripQuotes(const s: string; qc1, qc2: char): string;
begin
{$IFDEF UNICODE}
  Result := WideStripQuotes(s, qc1, qc2);
{$ELSE}
  Result := AnsiStripQuotes(s, qc1, qc2);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Тримы - версия для String

function STrimStartA(const s: AnsiString; sep: AnsiChar): AnsiString;
var ps, pe: PAnsiChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  ps := @s[1];
  if ps^<>sep then begin
    Result := s; //короткая версия
    exit;
  end;

 //Длинная версия
  pe := @s[Length(s)];
  while (cardinal(ps) <= cardinal(pe)) and (ps^=sep) do
    Inc(ps);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(AnsiChar) + 1);
  Move(s[1], Result[1], Length(Result)*SizeOf(AnsiChar));
end;

function STrimStartW(const s: UnicodeString; sep: UniChar): UnicodeString;
var ps, pe: PWideChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  ps := @s[1];
  if ps^<>sep then begin
    Result := s; //короткая версия
    exit;
  end;

 //Длинная версия
  pe := @s[Length(s)];
  while (cardinal(ps) <= cardinal(pe)) and (ps^=sep) do
    Inc(ps);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(WideChar) + 1);
  Move(s[1], Result[1], Length(Result)*SizeOf(WideChar));
end;

function STrimStart(const s: string; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := STrimStartW(s, sep);
{$ELSE}
  Result := STrimStartA(s, sep);
{$ENDIF}
end;

function STrimEndA(const s: AnsiString; sep: AnsiChar): AnsiString;
var ps, pe: PAnsiChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  pe := @s[Length(s)];
  if pe^<>sep then begin
    Result := s; //короткая версия
    exit;
  end;

 //Длинная версия
  ps := @s[1];
  while (cardinal(pe) > cardinal(ps)) and (pe^=sep) do
    Dec(pe);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(AnsiChar) + 1);
  Move(s[1], Result[1], Length(Result)*SizeOf(AnsiChar));
end;

function STrimEndW(const s: UnicodeString; sep: UniChar): UnicodeString;
var ps, pe: PWideChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  pe := @s[Length(s)];
  if pe^<>sep then begin
    Result := s; //короткая версия
    exit;
  end;

 //Длинная версия
  ps := @s[1];
  while (cardinal(pe) > cardinal(ps)) and (pe^=sep) do
    Dec(pe);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(WideChar) + 1);
  Move(s[1], Result[1], Length(Result)*SizeOf(WideChar));
end;

function STrimEnd(const s: string; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := STrimEndW(s, sep);
{$ELSE}
  Result := STrimEndA(s, sep);
{$ENDIF}
end;

function STrimA(const s: AnsiString; sep: AnsiChar): AnsiString;
var ps, pe: PAnsiChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  ps := @s[1];
  pe := @s[Length(s)];

  while (cardinal(pe) >= cardinal(ps)) and (pe^ = sep) do
    Dec(pe);

  while (cardinal(ps) <= cardinal(pe)) and (ps^ = sep) do
    Inc(ps);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(AnsiChar) + 1);
  Move(ps^, Result[1], Length(Result)*SizeOf(AnsiChar));
end;

function STrimW(const s: UnicodeString; sep: UniChar): UnicodeString;
var ps, pe: PWideChar;
begin
  if s='' then begin
    Result := '';
    exit;
  end;

  ps := @s[1];
  pe := @s[Length(s)];

  while (cardinal(pe) >= cardinal(ps)) and (pe^ = sep) do
    Dec(pe);

  while (cardinal(ps) <= cardinal(pe)) and (ps^ = sep) do
    Inc(ps);

  if cardinal(ps) > cardinal(pe) then begin
    Result := '';
    exit;
  end;

  SetLength(Result, (cardinal(pe)-cardinal(ps)) div SizeOf(WideChar) + 1);
  Move(ps^, Result[1], Length(Result)*SizeOf(WideChar));
end;

function STrim(const s: string; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := STrimW(s, sep);
{$ELSE}
  Result := STrimA(s, sep);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
///  Тримы - версия для PChar

function PTrimStartA(s: PAnsiChar; sep: AnsiChar): AnsiString;
begin
  while s^=sep do Inc(s);
  Result := s;
end;

function PTrimStartW(s: PUniChar; sep: UniChar): UnicodeString;
begin
  while s^=sep do Inc(s);
  Result := s;
end;

function PTrimStart(s: PChar; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := PTrimStartW(s, sep);
{$ELSE}
  Result := PTrimStartA(s, sep);
{$ENDIF}
end;


function PTrimEndA(s: PAnsiChar; sep: AnsiChar): AnsiString;
var se: PAnsiChar;
begin
 //Конец строки
  se := s;
  while se^<>#00 do Inc(se);

 //Откатываемся назад
  Dec(se);
  while (se^=sep) and (IntPtr(se) > IntPtr(s)) do
    Dec(se);

 //Пустая строка
  if IntPtr(se) <= IntPtr(s) then begin
    Result := '';
    exit;
  end;

  Inc(se);
  Result := StrSubA(s, se);
end;

function PTrimEndW(s: PUniChar; sep: UniChar): UnicodeString;
var se: PWideChar;
begin
 //Конец строки
  se := s;
  while se^<>#00 do Inc(se);

 //Откатываемся назад
  Dec(se);
  while (se^=sep) and (IntPtr(se) > IntPtr(s)) do
    Dec(se);

 //Пустая строка
  if IntPtr(se) < IntPtr(s) then begin
    Result := '';
    exit;
  end;

  Inc(se);
  Result := StrSubW(s, se);
end;

function PTrimEnd(s: PChar; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := PTrimEndW(s, sep);
{$ELSE}
  Result := PTrimEndA(s, sep);
{$ENDIF}
end;

function PTrimA(s: PAnsiChar; sep: AnsiChar): AnsiString;
begin
 //Пробелы в начале
  while s^=sep do Inc(s);
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  Result := PTrimEndA(s, sep);
end;

function PTrimW(s: PUniChar; sep: UniChar): UnicodeString;
begin
 //Пробелы в начале
  while s^=sep do Inc(s);
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  Result := PTrimEndW(s, sep);
end;

function PTrim(s: PChar; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := PTrimW(s, sep);
{$ELSE}
  Result := PTrimA(s, sep);
{$ENDIF}
end;


function BETrimA(beg, en: PAnsiChar; sep: AnsiChar): AnsiString;
begin
 //Trim spaces
  Dec(en);
  while (IntPtr(en) > IntPtr(beg)) and (en^=sep) do
    Dec(en);
  Inc(en);

  while (IntPtr(en) > IntPtr(beg)) and (beg^=sep) do
    Inc(beg);

  Result := StrSubA(beg, en);
end;

function BETrimW(beg, en: PUniChar; sep: UniChar): UnicodeString;
begin
 //Trim spaces
  Dec(en);
  while (IntPtr(en) > IntPtr(beg)) and (en^=sep) do
    Dec(en);
  Inc(en);

  while (IntPtr(en) > IntPtr(beg)) and (beg^=sep) do
    Inc(beg);

  Result := StrSubW(beg, en);
end;

function BETrim(beg, en: PChar; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := BETrimW(beg, en, sep);
{$ELSE}
  Result := BETrimA(beg, en, sep);
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// Binary/string conversions

const
  sHexSymbols: AnsiString = '0123456789ABCDEF';

function ByteToHex(b: byte): AnsiString; inline;
begin
  SetLength(Result, 2);
  Result[1] := sHexSymbols[(b shr 4) + 1];
  Result[2] := sHexSymbols[(b mod 16) + 1];
end;

function BinToHex(ptr: pbyte; sz: integer): AnsiString;
var i: integer;
begin
  SetLength(Result, sz*2);
  i := 0;
  while i < sz do begin
    Result[i*2+1] := sHexSymbols[(ptr^ shr 4) + 1];
    Result[i*2+2] := sHexSymbols[(ptr^ mod 16) + 1];
    Inc(ptr);
    Inc(i);
  end;
end;

function DataToHex(data: array of byte): AnsiString;
begin
  Result := BinToHex(@data[0], Length(data));
end;

function HexCharValue(c: AnsiChar): byte; inline;
begin
  if c in ['0'..'9'] then
    Result := Ord(c) - Ord('0')
  else
    if c in ['a'..'f'] then
      Result := 10 + Ord(c) - Ord('a')
    else
      if c in ['A'..'F'] then
        Result := 10 + Ord(c) - Ord('A')
      else
        raise Exception.Create('Illegal hex symbol: '+c);
end;

//Буфер должен быть выделен заранее.
//Переведено будет лишь столько, сколько влезло в указанный размер.
procedure HexToBin(const s: AnsiString; p: pbyte; size: integer);
var i: integer;
begin
  if size > (Length(s) div 2) then
    size := (Length(s) div 2);

  for i := 0 to size-1 do begin
    p^ := HexCharValue(s[2*i+1]) * 16
        + HexCharValue(s[2*i+2]);
    Inc(p);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
///  Codepage utils

function ToWideChar(c: AnsiChar; cp: cardinal): WideChar;
begin
  if MultiByteToWideChar(cp, 0, @c, 1, @Result, 2) = 0 then
    RaiseLastOsError;
end;

function ToChar(c: WideChar; cp: cardinal): AnsiChar;
begin
  if WideCharToMultiByte(cp, 0, @c, 2, @Result, 1, nil, nil) = 0 then
    RaiseLastOsError;
end;

function ToWideString(const s: AnsiString; cp: cardinal): WideString;
begin
  Result := BufToWideString(PAnsiChar(s), Length(s), cp);
end;

function ToString(const s: WideString; cp: cardinal): AnsiString;
begin
  Result := BufToString(PWideChar(s), Length(s), cp);
end;

function BufToWideString(s: PAnsiChar; len: integer; cp: cardinal): WideString;
var size: integer;
begin
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  size := MultiByteToWideChar(cp, 0, s, len, nil, 0);
  if size=0 then
    RaiseLastOsError;
  SetLength(Result, size);
  if MultiByteToWideChar(cp, 0, s, len, pwidechar(Result), size) = 0 then
    RaiseLastOsError;
end;

function BufToString(s: PWideChar; len: integer; cp: cardinal): AnsiString;
var size: integer;
begin
  if s^=#00 then begin
    Result := '';
    exit;
  end;

  size := WideCharToMultiByte(cp, 0, s, len, nil, 0, nil, nil);
  if size=0 then
    RaiseLastOsError;
  SetLength(Result, size);
  if WideCharToMultiByte(cp, 0, s, len, PAnsiChar(Result), size, nil, nil) = 0 then
    RaiseLastOsError;
end;

function Convert(const s: AnsiString; cpIn, cpOut: cardinal): AnsiString;
begin
  Result := ToString(ToWideString(s, cpIn), cpOut);
end;

//Переводит строку из текущей кодировки системы в текущую кодировку консоли.
function WinToOEM(const s: AnsiString): AnsiString;
begin
  Result := Convert(s, CP_ACP, CP_OEMCP);
end;

//Переводит строку из текущей кодировки консоли в текущую кодировку системы.
function OEMToWin(const s: AnsiString): AnsiString;
begin
  Result := Convert(s, CP_OEMCP, CP_ACP);
end;


////////////////////////////////////////////////////////////////////////////////
///  StringBuilder

procedure TAnsiStringBuilder.Clear;
begin
  Used := 0;
end;

function TAnsiStringBuilder.Pack: AnsiString;
begin
  SetLength(Data, Used);
  Result := Data;
end;

procedure TAnsiStringBuilder.Add(c: AnsiChar);
begin
  if Used >= Length(Data) then
    SetLength(Data, Length(Data)*2 + 20);
  Inc(Used);
  Data[Used] := c;
end;

procedure TAnsiStringBuilder.Add(pc: PAnsiChar);
var len, i: integer;
begin
  len := StrLen(pc);

  if Used+len >= Length(Data) then
    if len > Length(Data)+20 then
      SetLength(Data, Length(Data)+len+20)
    else
      SetLength(Data, Length(Data)*2 + 20);

  for i := 1 to len do begin
    Data[Used+i] := pc^;
    Inc(pc);
  end;
  Inc(Used, len);
end;

procedure TAnsiStringBuilder.Add(const s: AnsiString);
var len, i: integer;
begin
  len := Length(s);

  if Used+len >= Length(Data) then
    if len > Length(Data)+20 then
      SetLength(Data, Length(Data)+len+20)
    else
      SetLength(Data, Length(Data)*2 + 20);

  for i := 1 to len do
    Data[Used+i] := s[i];
  Inc(Used, len);
end;

procedure TAnsiStringBuilder.Pop(SymbolCount: integer = 1);
begin
  if SymbolCount >= Used then
    Used := 0
  else
    Used := Used - SymbolCount;
end;

procedure TUniStringBuilder.Clear;
begin
  Used := 0;
end;

function TUniStringBuilder.Pack: UnicodeString;
begin
  SetLength(Data, Used);
  Result := Data;
end;

procedure TUniStringBuilder.Add(c: UniChar);
begin
  if Used >= Length(Data) then
    SetLength(Data, Length(Data)*2 + 20);
  Inc(Used);
  Data[Used] := c;
end;

procedure TUniStringBuilder.Add(pc: PUniChar);
var len, i: integer;
begin
  len := WStrLen(pc);

  if Used+len >= Length(Data) then
    if len > Length(Data)+20 then
      SetLength(Data, Length(Data)+len+20)
    else
      SetLength(Data, Length(Data)*2 + 20);

  for i := 1 to len do begin
    Data[Used+i] := pc^;
    Inc(pc);
  end;
  Inc(Used, len);
end;

procedure TUniStringBuilder.Add(const s: UnicodeString);
var len, i: integer;
begin
  len := Length(s);

  if Used+len >= Length(Data) then
    if len > Length(Data)+20 then
      SetLength(Data, Length(Data)+len+20)
    else
      SetLength(Data, Length(Data)*2 + 20);

  for i := 1 to len do
    Data[Used+i] := s[i];
  Inc(Used, len);
end;

procedure TUniStringBuilder.Pop(SymbolCount: integer = 1);
begin
  if SymbolCount >= Used then
    Used := 0
  else
    Used := Used - SymbolCount;
end;


{ Функции кодирования в HTML-форматы. Пока сделаны медленно и просто,
 при необходимости можно ускорить. }

//Кодирует строку в URI-форме: "(te)(su)(to) str" -> "%E3%83%86%E3%82%B9%E3%83%88+str"
//Пока сделано медленно и просто, при необходимости можно ускорить
function UrlEncode(const s: UnicodeString; options: TUrlEncodeOptions): AnsiString;
var i, j: integer;
  U: UTF8String;
begin
  Result := '';
  for i := 1 to Length(s) do
    if CharInSet(s[i], ['a'..'z', 'A'..'Z', '1'..'9', '0']) then
      Result := Result + AnsiChar(s[i])
    else
    if s[i]=' ' then
      if ueNoSpacePlus in options then
        Result := Result + '%20'
      else
        Result := Result + '+'
    else begin
     //Вообще говоря, символ в UTF-16 может занимать несколько пар...
     //Но мы здесь это игнорируем.
      U := UTF8String(s[i]); // explicit Unicode->UTF8 conversion
      for j := 1 to Length(U) do
        Result := Result + '%' + AnsiString(IntToHex(Ord(U[j]), 2));
    end;
end;

//Кодирует строку в HTML-форме. Заменяет только символы, которые не могут
//встречаться в правильном HTML.
//Все остальные юникод-символы остаются в нормальной форме.
function HtmlEscape(const s: UnicodeString): UnicodeString;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i]='&' then Result := Result + '&amp;' else
    if s[i]='''' then Result := Result + '&apos;' else
    if s[i]='"' then Result := Result + '&quot;' else
    if s[i]='<' then Result := Result + '&lt;' else
    if s[i]='>' then Result := Result + '&gt;' else
    Result := Result + s[i];
end;

//Кодирует строку в HTML-форме. Неизвестно, была ли строка закодирована до сих пор.
//Пользователь мог закодировать некоторые последовательности, но забыть другие.
//Поэтому кодируются только те символы, которые встречаться в итоговой строке
//не могут никак:
//   "asd &amp; bsd <esd>" --> "asd &amp; bsd &lt;esd&gt;"
function HtmlEscapeObvious(const s: UnicodeString): UnicodeString;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
   //& не кодируем
    if s[i]='''' then Result := Result + '&apos;' else
    if s[i]='"' then Result := Result + '&quot;' else
    if s[i]='<' then Result := Result + '&lt;' else
    if s[i]='>' then Result := Result + '&gt;' else
    Result := Result + s[i];
end;

//Кодирует строку в HTML-форме. Заменяет все символы, не входящие в Ansi-набор.
//  "(te)(su)(to) str" -> "&12486;&12473;&12488; str"
//При необходимости можно сделать флаг "эскейпить в 16-ричные коды: &#x30DB;"
function HtmlEscapeToAnsi(const s: UnicodeString): AnsiString;
var i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i]='&' then Result := Result + '&amp;' else
    if s[i]='''' then Result := Result + '&apos;' else
    if s[i]='"' then Result := Result + '&quot;' else
    if s[i]='<' then Result := Result + '&lt;' else
    if s[i]='>' then Result := Result + '&gt;' else
   //ANSI-символы
    if CharInSet(s[i], ['a'..'z', 'A'..'Z', '1'..'9', '0', ' ']) then
      Result := Result + AnsiChar(s[i])
    else
      Result := Result + '&#'+AnsiString(IntToStr(word(s[i])))+';'
end;



end.
