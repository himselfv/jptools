unit UniStrUtils;
{$WEAKPACKAGEUNIT ON}

interface
uses SysUtils, WideStrUtils;

(*
 В библиотеке введён дополнительный тип: UniString.
 На старых компиляторах
   UniString = WideString
 На новых
   UniString = string (unicode)

 Все функции существуют в следующих версиях:
   Function: агностическая функция (для типа string)
   AnsiFunction (FunctionA): версия для AnsiString
   WideFunction (FunctionW): версия для WideString
   UniFunction (FunctionU): версия для UniString (оптимальная)

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
 по умолчанию на платформе.

 Следует помнить, что WideChar == UnicodeChar, и PWideChar в любом случае
 ничуть не отличается от PUnicodeChar. Поэтому функции, которые работают
 с PWideChar, продолжают работать без изменений.
*)

const
  BOM_UTF16LE = #255#254;
  BOM_UTF16BE = #254#255;

type
 //UniString - это наилучший доступный на платформе Unicode-тип.
 //На старых компиляторах UniString=WideString, на новых UniString=UnicodeString.
 {$IFDEF UNICODE}
  UniString = UnicodeString;
  PUniString = PUnicodeString;
 {$ELSE}
  UniString = WideString;
  PUniString = PWideString;
 {$ENDIF}

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


{$IFDEF UNICODE}
(*
  В юникод-версиях Дельфи некоторые Ansi-функции объявлены как UniString.
  Например,
    UpperCase - принимает string и работает только с ASCII
    AnsiUpperCase - принимает string и работает со всеми строками
  То есть, Ansi фактически Uni.

  Само по себе это не страшно (главное помнить не использовать UpperCase),
  но при компиляции Ansi-кода возникают дурацкие варнинги.

  Так что здесь представлены "честные" функции для Ansi-строк.
*)

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

function UStrPCopy(Dest: PUniChar; const Source: UniString): PUniChar; inline;
function UStrPLCopy(Dest: PUniChar; const Source: UniString; MaxLen: Cardinal): PUniChar; inline;

function UniLastChar(const S: UniString): PUniChar; inline;
function UniQuotedStr(const S: UniString; Quote: UniChar): UniString; inline;
function UniExtractQuotedStr(var Src: PUniChar; Quote: UniChar): UniString; inline;
function UniDequotedStr(const S: UniString; AQuote: UniChar): UniString; inline;
function UniAdjustLineBreaks(const S: UniString; Style: TTextLineBreakStyle = tlbsCRLF): UniString; inline;

function UniStringReplace(const S, OldPattern, NewPattern: UniString;
  Flags: TReplaceFlags): UniString; inline;
function UniReplaceStr(const AText, AFromText, AToText: UniString): UniString; inline;
function UniReplaceText(const AText, AFromText, AToText: UniString): UniString; inline;

function UniUpperCase(const S: UniString): UniString; inline;
function UniLowerCase(const S: UniString): UniString; inline;
function UniCompareStr(const S1, S2: UniString): Integer; inline;
function UniSameStr(const S1, S2: UniString): Boolean; inline;
function UniCompareText(const S1, S2: UniString): Integer; inline;
function UniSameText(const S1, S2: UniString): Boolean; inline;


(*
 Wide-версии стандартных функций.
 На новых компиляторах функции линкуются к системным. На старых реализованы с нуля.
*)

//remember, this returns the BYTE offset
function WidePos(const Substr, S: UniString): Integer;
function WideMidStr(const AText: UniString; const AStart: integer; const ACount: integer): UniString;

//Логика функций сравнения та же, что у Ansi-версий: сравнение лингвистическое,
//с учётом юникод-цепочек, а не бинарное, по байтам.
function WideStrComp(S1, S2: PWideChar): Integer;
function WideStrIComp(S1, S2: PWideChar): Integer;
function WideStartsStr(const ASubText: UniString; const AText: UniString): boolean;
function WideEndsStr(const ASubText: UniString; const AText: UniString): boolean;
function WideContainsStr(const AText: UniString; const ASubText: UniString): boolean;
function WideStartsText(const ASubText: UniString; const AText: UniString): boolean;
function WideEndsText(const ASubText: UniString; const AText: UniString): boolean;
function WideContainsText(const AText: UniString; const ASubText: UniString): boolean;


(*
  Далее идут вспомогательные функции библиотеки, написанные во всех версиях с нуля.
  Юникод-версии представлены как FunctionW или WideFunction.
*)

//Поиск строки в массивах
//Wide-версии нет, поскольку TWideStringArray ==def== TUniStringArray
function AnsiStrInArray(a: TAnsiStringArray; s: AnsiString): integer;
function AnsiTextInArray(a: TAnsiStringArray; s: AnsiString): integer;
function UniStrInArray(a: TUniStringArray; s: UniString): integer;
function UniTextInArray(a: TUniStringArray; s: UniString): integer;
function StrInArray(a: TStringArray; s: string): integer;
function TextInArray(a: TStringArray; s: string): integer;

//Splits a string by a single type of separators. Ansi version.
function StrSplitA(s: PAnsiChar; sep: AnsiChar): TAnsiStringArray;
function StrSplitW(s: PUniChar; sep: UniChar): TUniStringArray;
function StrSplit(s: PChar; sep: char): TStringArray;

//Same, just with Strings
function AnsiSepSplit(s: AnsiString; sep: AnsiChar): TAnsiStringArray;
function WideSepSplit(s: UniString; sep: UniChar): TUniStringArray;
function SepSplit(s: string; sep: char): TStringArray;

//Joins a string array usng the specified separator
function AnsiSepJoin(s: TAnsiStringArray; sep: AnsiChar): AnsiString;
function WideSepJoin(s: TUniStringArray; sep: UniChar): UniString;
function SepJoin(s: TStringArray; sep: Char): string;

//Возвращает в виде WideString строку PWideChar, но не более N символов
//Полезно для чтения всяких буферов фиксированного размера, где не гарантирован ноль.
function AnsiStrFromBuf(s: PAnsiChar; MaxLen: integer): AnsiString;
function WideStrFromBuf(s: PUniChar; MaxLen: integer): UniString;
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
function AnsiStrIsLnOnly(str: AnsiString): boolean;
function WideStrIsLnOnly(str: UniString): boolean;
function StrIsLnOnly(str: string): boolean;

//These have significantly different implementation when working with Ansi code page,
//so they're implemented only in Unicode yet.
function IsHiragana(c: UniChar): boolean;
function IsKatakana(c: UniChar): boolean;
function IsKana(c: UniChar): boolean;
function IsKanji(c: UniChar): boolean;

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

//Возвращает подстроку с заданного места и нужного размера.
function StrSubLA(beg: PAnsiChar; len: integer): AnsiString;
function StrSubLW(beg: PUniChar; len: integer): UniString;
function StrSubL(beg: PChar; len: integer): string;
//Возвращает подстроку с заданного места и до заданного места не включительно.
function StrSubA(beg: PAnsiChar; en: PAnsiChar): AnsiString;
function StrSubW(beg: PUniChar; en: PUniChar): UniString;
function StrSub(beg: PChar; en: PChar): string;
//Обратная совместимость
function SubStrPchA(beg, en: PAnsiChar): AnsiString;
function SubStrPchW(beg, en: PUniChar): UniString;
function SubStrPch(beg, en: pchar): string;


//Scans the specified string for the specfied character. Starts at position <start_at>.
//Continues for <use_len> symbols. Returns first symbol index in string or -1 if not found any.
function AnsiFindChar(s: AnsiString; c: AnsiChar; start_at: integer; use_len: integer): integer;
function WideFindChar(s: UniString; c: UniChar; start_at: integer; use_len: integer): integer;
function FindChar(s: string; c: char; start_at: integer; use_len: integer): integer;

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
function AnsiReadUpToNext(str: PAnsiChar; cs: AnsiString; out block: AnsiString): PAnsiChar;
function WideReadUpToNext(str: PUniChar; cs: UniString; out block: UniString): PUniChar;
function ReadUpToNext(str: PChar; cs: string; out block: string): PChar;


//Removes quote characters from around the string, if they exist.
function AnsiStripQuotes(s: AnsiString; qc1, qc2: AnsiChar): AnsiString;
function WideStripQuotes(s: UniString; qc1, qc2: UniChar): UniString;
function StripQuotes(s: string; qc1, qc2: char): string;

//Удаляет пробелы из конца строки - версия для String
function STrimStartA(s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimStartW(s: UniString; sep: UniChar = ' '): UniString;
function STrimStart(s: string; sep: Char = ' '): string;
function STrimEndA(s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimEndW(s: UniString; sep: UniChar = ' '): UniString;
function STrimEnd(s: string; sep: Char = ' '): string;
function STrimA(s: AnsiString; sep: AnsiChar = ' '): AnsiString;
function STrimW(s: UniString; sep: UniChar = ' '): UniString;
function STrim(s: string; sep: Char = ' '): string;

//Удаляет пробелы из конца строки - версия для PChar
function PTrimStartA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimStartW(s: PUniChar; sep: UniChar = ' '): UniString;
function PTrimStart(s: PChar; sep: Char = ' '): string;
function PTrimEndA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimEndW(s: PUniChar; sep: UniChar = ' '): UniString;
function PTrimEnd(s: PChar; sep: Char = ' '): string;
function PTrimA(s: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function PTrimW(s: PUniChar; sep: UniChar = ' '): UniString;
function PTrim(s: PChar; sep: Char = ' '): string;

//Удаляет пробелы из начала и конца строки, заданных прямо.
function BETrimA(beg, en: PAnsiChar; sep: AnsiChar = ' '): AnsiString;
function BETrimW(beg, en: PUniChar; sep: UniChar = ' '): UniString;
function BETrim(beg, en: PChar; sep: Char = ' '): string;


//Binary/string conversion
//Преобразует данные в цепочку hex-кодов.
function BinToHex(ptr: pbyte; sz: integer): AnsiString;
//Преобразует массив байт в цепочку hex-кодов.
function DataToHex(data: array of byte): AnsiString;
//Декодирует один hex-символ в число от 1 до 16
function HexCharValue(c: AnsiChar): byte; inline;
//Декодирует строку из hex-пар в данные. Место под данные должно быть выделено заранее
procedure HexToBin(s: AnsiString; p: pbyte; size: integer);

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
    procedure Add(s: AnsiString); overload;
    procedure Pop(SymbolCount: integer = 1);
  end;
  PAnsiStringBuilder = ^TAnsiStringBuilder;

  TUniStringBuilder = record
    Data: UniString;
    Used: integer;
    procedure Clear;
    function Pack: UniString;
    procedure Add(c: UniChar); overload;
    procedure Add(pc: PUniChar); overload;
    procedure Add(s: UniString); overload;
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

implementation
uses StrUtils, Windows;

////////////////////////////////////////////////////////////////////////////////
{$IFDEF UNICODE}
(*
  Все реализации скопированы у Борланд.
*)

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

function UStrPCopy(Dest: PUniChar; const Source: UniString): PUniChar;
begin
 {Copied from WideStrUtils}
  Result := WStrLCopy(Dest, PWideChar(Source), Length(Source));
end;

function UStrPLCopy(Dest: PUniChar; const Source: UniString; MaxLen: Cardinal): PUniChar;
begin
 {Copied from WideStrUtils}
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;

function UniLastChar(const S: UniString): PUniChar;
begin
 {Copied from WideStrUtils for speed}
  if S = '' then
    Result := nil
  else
    Result := @S[Length(S)];
end;

function UniQuotedStr(const S: UniString; Quote: UniChar): UniString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of QuotedStr. This one works for "strings" on Unicode.
  Result := AnsiQuotedStr(S, Quote);
{$ELSE}
  Result := WideQuotedStr(S, Quote);
{$ENDIF}
end;

function UniExtractQuotedStr(var Src: PUniChar; Quote: UniChar): UniString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of ExtractQuotedStr.
  Result := AnsiExtractQuotedStr(Src, Quote);
{$ELSE}
  Result := WideExtractQuotedStr(Src, Quote);
{$ENDIF}
end;

function UniDequotedStr(const S: UniString; AQuote: UniChar): UniString;
begin
{$IFDEF UNICODE}
 //There's no "agnostic" version of DequotedStr.
  Result := AnsiDequotedStr(S, AQuote);
{$ELSE}
  Result := WideDequotedStr(S, AQuote);
{$ENDIF}
end;

function UniAdjustLineBreaks(const S: UniString; Style: TTextLineBreakStyle = tlbsCRLF): UniString;
begin
{$IFDEF UNICODE}
  Result := AdjustLineBreaks(S, Style);
{$ELSE}
  Result := WideAdjustLineBreaks(S, Style);
{$ENDIF}
end;

function UniStringReplace(const S, OldPattern, NewPattern: UniString;
  Flags: TReplaceFlags): UniString;
begin
{$IFDEF UNICODE}
  Result := StringReplace(S, OldPattern, NewPattern, Flags);
{$ELSE}
  Result := WideStringReplace(S, OldPattern, NewPattern, Flags);
{$ENDIF}
end;

function UniReplaceStr(const AText, AFromText, AToText: UniString): UniString;
begin
{$IFDEF UNICODE}
  Result := ReplaceStr(AText, AFromText, AToText);
{$ELSE}
  Result := WideReplaceStr(AText, AFromText, AToText);
{$ENDIF}
end;

function UniReplaceText(const AText, AFromText, AToText: UniString): UniString;
begin
{$IFDEF UNICODE}
  Result := ReplaceText(AText, AFromText, AToText);
{$ELSE}
  Result := WideReplaceText(AText, AFromText, AToText);
{$ENDIF}
end;

function UniUpperCase(const S: UniString): UniString;
begin
{$IFDEF UNICODE}
  Result := UpperCase(S);
{$ELSE}
  Result := WideUpperCase(S);
{$ENDIF}
end;

function UniLowerCase(const S: UniString): UniString;
begin
{$IFDEF UNICODE}
  Result := LowerCase(S);
{$ELSE}
  Result := WideLowerCase(S);
{$ENDIF}
end;

function UniCompareStr(const S1, S2: UniString): Integer;
begin
{$IFDEF UNICODE}
  Result := CompareStr(S1, S2);
{$ELSE}
  Result := WideCompareStr(S1, S2);
{$ENDIF}
end;

function UniSameStr(const S1, S2: UniString): Boolean;
begin
{$IFDEF UNICODE}
  Result := SameStr(S1, S2);
{$ELSE}
  Result := WideSameStr(S1, S2);
{$ENDIF}
end;

function UniCompareText(const S1, S2: UniString): Integer;
begin
{$IFDEF UNICODE}
  Result := CompareText(S1, S2);
{$ELSE}
  Result := WideCompareText(S1, S2);
{$ENDIF}
end;

function UniSameText(const S1, S2: UniString): Boolean;
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

function WidePos(const Substr, S: UniString): Integer;
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
    Result := Integer(P) - Integer(PWideChar(S)) + 1;
end;
{$ENDIF}

//Returns substring of s, starting at <start> characters and continuing <length> of them.
function WideMidStr(const AText: UniString; const AStart: integer; const ACount: integer): UniString;
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

function WideStartsStr(const ASubText: UniString; const AText: UniString): boolean;
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

function WideEndsStr(const ASubText: UniString; const AText: UniString): boolean;
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
function WideContainsStr(const AText: UniString; const ASubText: UniString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.ContainsStr(AText, ASubText);
{$ELSE}
  Result := (WStrPos(PWideChar(AText), PWideChar(ASubText)) <> nil);
{$ENDIF}
end;

function WideStartsText(const ASubText: UniString; const AText: UniString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.StartsText(ASubText, AText);
{$ELSE}
  Result := WideStartsStr(WideLowerCase(ASubText), WideLowerCase(AText));
{$ENDIF}
end;

function WideEndsText(const ASubText: UniString; const AText: UniString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.EndsText(ASubText, AText);
{$ELSE}
  Result := WideEndsStr(WideLowerCase(ASubText), WideLowerCase(AText));
{$ENDIF}
end;

//Аргументы в обратном порядке, как и у AnsiContainsText
function WideContainsText(const AText: UniString; const ASubText: UniString): boolean;
begin
{$IFDEF UNICODE}
  Result := StrUtils.StartsText(AText, ASubText);
{$ELSE}
  Result := WideContainsStr(WideLowerCase(AText), WideLowerCase(ASubText));
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Находит индекс первого появления строки в массиве

function AnsiStrInArray(a: TAnsiStringArray; s: AnsiString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(a)-1 do
    if AnsiSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function AnsiTextInArray(a: TAnsiStringArray; s: AnsiString): integer;
var i: integer;
begin
  Result := -1;
  s := AnsiUpperCase(s);
  for i := 0 to Length(a)-1 do
    if AnsiSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function UniStrInArray(a: TUniStringArray; s: UniString): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to Length(a)-1 do
    if UniSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function UniTextInArray(a: TUniStringArray; s: UniString): integer;
var i: integer;
begin
  Result := -1;
  s := UniUpperCase(s);
  for i := 0 to Length(a)-1 do
    if UniSameStr(s, a[i]) then begin
      Result := i;
      break;
    end;
end;

function StrInArray(a: TStringArray; s: string): integer;
begin
{$IFDEF UNICODE}
  Result := UniStrInArray(a, s);
{$ELSE}
  Result := AnsiStrInArray(a, s);
{$ENDIF}
end;

function TextInArray(a: TStringArray; s: string): integer;
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

function AnsiSepSplit(s: AnsiString; sep: AnsiChar): TAnsiStringArray;
begin
  Result := StrSplitA(PAnsiChar(s), sep);
end;

function WideSepSplit(s: UniString; sep: UniChar): TUniStringArray;
begin
  Result := StrSplitW(PWideChar(s), sep);
end;

function SepSplit(s: string; sep: char): TStringArray;
begin
{$IFDEF UNICODE}
  Result := StrSplitW(PWideChar(s), sep);
{$ELSE}
  Result := StrSplitA(PAnsiChar(s), sep);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
//Joins a string array usng the specified separator.

function AnsiSepJoin(s: TAnsiStringArray; sep: AnsiChar): AnsiString;
var len, i, li: integer;
begin
 //Считаем общий размер
  len := Length(s) - 1; //число разделителей
  for i := 0 to Length(s) - 1 do
    len := len + Length(s[i]);

 //Выделяем память
  SetLength(Result, len);
  li := 0;
  for i := 0 to Length(s) - 2 do begin
    Move(s[i][1], Result[li], Length(s[i])*SizeOf(AnsiChar));
    li := li + Length(s[i]);
    Result[li] := sep;
    li := li + 1;
  end;

 //Последний кусок
  i := Length(s) - 1;
  if i >= 0 then
    Move(s[i][1], Result[li], Length(s[i])*SizeOf(AnsiChar));
end;

function WideSepJoin(s: TUniStringArray; sep: UniChar): UniString;
var len, i, li: integer;
begin
 //Считаем общий размер
  len := Length(s) - 1; //число разделителей
  for i := 0 to Length(s) - 1 do
    len := len + Length(s[i]);

 //Выделяем память
  SetLength(Result, len);
  li := 0;
  for i := 0 to Length(s) - 2 do begin
    Move(s[i][1], Result[li], Length(s[i])*SizeOf(UniChar));
    li := li + Length(s[i]);
    Result[li] := sep;
    li := li + 1;
  end;

 //Последний кусок
  i := Length(s) - 1;
  if i >= 0 then
    Move(s[i][1], Result[li], Length(s[i])*SizeOf(UniChar));
end;

function SepJoin(s: TStringArray; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := WideSepJoin(s, sep);
{$ELSE}
  Result := AnsiSepJoin(s, sep);
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

function WideStrFromBuf(s: PUniChar; MaxLen: integer): UniString;
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
function AnsiStrIsLnOnly(str: AnsiString): boolean;
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

function WideStrIsLnOnly(str: UniString): boolean;
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

function StrIsLnOnly(str: string): boolean;
begin
{$IFDEF UNICODE}
  Result := WideStrIsLnOnly(str);
{$ELSE}
  Result := AnsiStrIsLnOnly(str);
{$ENDIF}
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
  Result := (Ord(c) >= $3041) and (Ord(c) <= $3094);
end;

function IsKatakana(c: UniChar): boolean;
begin
  Result := (Ord(c) >= $30A1) and (Ord(c) <= $30FA);
end;

function IsKana(c: UniChar): boolean;
begin
  Result := IsKatakana(c) or IsHiragana(c);
end;

function IsKanji(c: UniChar): boolean;
begin
  Result := (Ord(c) >= $4E00) and (Ord(c) <= $9FA5);
end;

////////////////////////////////////////////////////////////////////////////////
///  Indexing and lengths

// Возвращает номер символа, на который указывает ptr, в строке str
function AnsiPcharInd(str, ptr: PAnsiChar): integer;
begin
  Result := integer(ptr)-integer(str)+1;
end;

function WidePcharInd(str, ptr: PUniChar): integer;
begin
  Result := (integer(ptr)-integer(str)) div SizeOf(UniChar) + 1;
end;

function PcharInd(str, ptr: PChar): integer;
begin
  Result := (integer(ptr)-integer(str)) div SizeOf(char) + 1;
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
  Result := PAnsiChar(integer(p) + SizeOf(AnsiChar)*c);
end;

function PrevChar(p: PAnsiChar; c: integer = 1): PAnsiChar;
begin
  Result := PAnsiChar(integer(p) - SizeOf(AnsiChar)*c);
end;

function NextChar(p: PWideChar; c: integer = 1): PWideChar;
begin
  Result := PWideChar(integer(p) + SizeOf(WideChar)*c);
end;

function PrevChar(p: PWideChar; c: integer = 1): PWideChar;
begin
  Result := PWideChar(integer(p) - SizeOf(WideChar)*c);
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

function StrSubLW(beg: PUniChar; len: integer): UniString;
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
  Result := StrSubLA(beg, (integer(en)-integer(beg)) div SizeOf(AnsiChar));
end;

function StrSubW(beg: PUniChar; en: PUniChar): UniString;
begin
  Result := StrSubLW(beg, (integer(en)-integer(beg)) div SizeOf(WideChar));
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

function SubStrPchW(beg, en: PUniChar): UniString;
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
//Continues for <use_len> symbols. Returns first symbol index in string or -1 if not found any.
function AnsiFindChar(s: AnsiString; c: AnsiChar; start_at: integer; use_len: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := start_at to use_len - 1 do
    if (s[i]=c) then begin
      Result := i;
      exit;
    end;
end;

function WideFindChar(s: UniString; c: UniChar; start_at: integer; use_len: integer): integer;
var i: integer;
begin
  Result := -1;
  for i := start_at to use_len - 1 do
    if (s[i]=c) then begin
      Result := i;
      exit;
    end;
end;

function FindChar(s: string; c: char; start_at: integer; use_len: integer): integer;
begin
{$IFDEF UNICODE}
  Result := WideFindChar(s, c, start_at, use_len);
{$ELSE}
  Result := AnsiFindChar(s, c, start_at, use_len);
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
function AnsiReadUpToNext(str: PAnsiChar; cs: AnsiString; out block: AnsiString): PAnsiChar;
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
function WideReadUpToNext(str: PUniChar; cs: UniString; out block: UniString): PUniChar;
begin
  Result := StrScanAnyW(str, PWideChar(cs));
  if Result <> nil then
    SetLength(block, WidePCharInd(str, Result)-1)
  else
    SetLength(block, WStrLen(str));
  if Length(block) > 0 then
    WStrLCopy(@block[1], str, Length(block)); //null not included
end;

function ReadUpToNext(str: PChar; cs: string; out block: string): PChar;
begin
{$IFDEF UNICODE}
  Result := WideReadUpToNext(str, cs, block);
{$ELSE}
  Result := AnsiReadUpToNext(str, cs, block);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Удаление лишних символов по краям

//Removes quote characters around the string, if they exist.
function AnsiStripQuotes(s: AnsiString; qc1, qc2: AnsiChar): AnsiString;
begin
  Result := s;
  if Length(Result) < 2 then exit;

 //Если кавычки есть, убираем их.
  if (Result[1]=qc1) and (Result[Length(Result)-1]=qc2) then begin
    Move(Result[2], Result[1], (Length(Result)-2)*SizeOf(Result[1]));
    SetLength(Result, Length(Result)-2);
  end;
end;

function WideStripQuotes(s: UniString; qc1, qc2: UniChar): UniString;
begin
  Result := s;
  if Length(Result) < 2 then exit;

 //Если кавычки есть, убираем их.
  if (Result[1]=qc1) and (Result[Length(Result)-1]=qc2) then begin
    Move(Result[2], Result[1], (Length(Result)-2)*SizeOf(Result[1]));
    SetLength(Result, Length(Result)-2);
  end;
end;

function StripQuotes(s: string; qc1, qc2: char): string;
begin
{$IFDEF UNICODE}
  Result := WideStripQuotes(s, qc1, qc2);
{$ELSE}
  Result := AnsiStripQuotes(s, qc1, qc2);
{$ENDIF}
end;


////////////////////////////////////////////////////////////////////////////////
///  Тримы - версия для String

function STrimStartA(s: AnsiString; sep: AnsiChar): AnsiString;
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

function STrimStartW(s: UniString; sep: UniChar): UniString;
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

function STrimStart(s: string; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := STrimStartW(s, sep);
{$ELSE}
  Result := STrimStartA(s, sep);
{$ENDIF}
end;

function STrimEndA(s: AnsiString; sep: AnsiChar): AnsiString;
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

function STrimEndW(s: UniString; sep: UniChar): UniString;
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

function STrimEnd(s: string; sep: Char): string;
begin
{$IFDEF UNICODE}
  Result := STrimEndW(s, sep);
{$ELSE}
  Result := STrimEndA(s, sep);
{$ENDIF}
end;

function STrimA(s: AnsiString; sep: AnsiChar): AnsiString;
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

function STrimW(s: UniString; sep: UniChar): UniString;
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

function STrim(s: string; sep: Char): string;
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

function PTrimStartW(s: PUniChar; sep: UniChar): UniString;
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
  while (se^=sep) and (integer(se) > integer(s)) do
    Dec(se);

 //Пустая строка
  if integer(se) <= integer(s) then begin
    Result := '';
    exit;
  end;

  Inc(se);
  Result := StrSubA(s, se);
end;

function PTrimEndW(s: PUniChar; sep: UniChar): UniString;
var se: PWideChar;
begin
 //Конец строки
  se := s;
  while se^<>#00 do Inc(se);

 //Откатываемся назад
  Dec(se);
  while (se^=sep) and (integer(se) > integer(s)) do
    Dec(se);

 //Пустая строка
  if integer(se) < integer(s) then begin
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

function PTrimW(s: PUniChar; sep: UniChar): UniString;
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
  while (integer(en) > integer(beg)) and (en^=sep) do
    Dec(en);
  Inc(en);

  while (integer(en) > integer(beg)) and (beg^=sep) do
    Inc(beg);

  Result := StrSubA(beg, en);
end;

function BETrimW(beg, en: PUniChar; sep: UniChar): UniString;
begin
 //Trim spaces
  Dec(en);
  while (integer(en) > integer(beg)) and (en^=sep) do
    Dec(en);
  Inc(en);

  while (integer(en) > integer(beg)) and (beg^=sep) do
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
procedure HexToBin(s: AnsiString; p: pbyte; size: integer);
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

procedure TAnsiStringBuilder.Add(s: AnsiString);
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

function TUniStringBuilder.Pack: UniString;
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

procedure TUniStringBuilder.Add(s: UniString);
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

end.
