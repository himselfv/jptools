unit WarodaiXrefs;
{
Матчит различные XRef-ссылки в Вародае.

Общая идея:
Матчим последовательности вида
  "см. <a href>ССЫЛКА 1</a> и ССЫЛКА 2"
Затем разбираем на отдельные ссылки. Каждая ссылка либо отмечена <a href>,
либо определённого формата (см. ниже).
}

interface
uses Warodai, FastArray, WcUtils, EdictWriter, PerlRegEx, PerlRegExUtils;
{$INCLUDE 'Warodai.inc'}

procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);

function HasHrefParts(const ln: UTF8String): boolean;
procedure AssertNoHrefsLeft(const ln: UTF8String);


{
Можно посчитать, какие виды предисловий ко ссылкам встречаются в файле.
Передавайте в MatchHrefs все строки с HTML-разметкой, а в конце проверьте AllHrefTypes;
}

var
  AllHrefTypes: TArray<string>;
  xrefStats: record
    HrefExpr: integer;
    SimpleHref: integer;
  end;

procedure MatchHrefs(const ln: string);

implementation
uses SysUtils, UniStrUtils, WarodaiHeader;

{
Форма ссылки:
  あわ【泡】 (в едикте через точку)
  そとわ【外輪】(の足)
  あいおい【相老】(～する)
  あいおい【相老】(～する) 1
  あいおい【相老2】

Страшные вещи ловим:
  きだおれ(京の)着倒れ
}
const
  pRomanDigit='IVX'; //используются для ссылок на подзначения
  pSomeCJKPunctuation='\x{3000}-\x{3009}\x{3012}-\x{303F}'; //без 【】
  pCJKRefChar = pCJKUnifiedIdeographs + pCJKUnifiedIdeographsExtA
    + pCJKUnifiedIdeographsExtB + pCJKUnifiedIdeographsExtC
    + pHiragana + pKatakana + pSomeCJKPunctuation
    + '\/…';
  pCJKRefStr='['+pCJKRefChar+']+(?:['+pRomanDigit+']+)?'; //слово и, опционально, латинский номер его вариации (см. DropVariantIndicator)

  pRefBase=pCJKRefStr;
  pRefWri1='【[^】]*】'; //расшифровка в формате あわ【泡】 -- матчим побольше, что внутри только CJK - проверим потом
  pRefNo='\s*\d{1,3}'; //опционально 1-2о цифры с пробелом или без -- ссылка на подзначение

  //Хвост ссылки
  pRefRoundBrackets = '\s?\(.*?\)'; //любое число символов - русских или японских - в скобках -- продолжение или пояснение
  pRefFreeTail = '\s?['+pCJKRefChar+']+?'; //некоторое число японских символов без скобок

 //полный хвост ссылки - поставляется во множестве разновидностей
  pRefTail =
    '(?:' //в любом порядке (RefNo может идти после скобок):
      +'(?:'+pRefNo+')?'
      +'('+pRefRoundBrackets+'|)' //#+1
      +'('+pRefFreeTail+'|)'      //#+2
    +'|'
      +'('+pRefRoundBrackets+'|)' //#+3
      +'(?:'+pRefNo+')?'
      +'('+pRefFreeTail+'|)'      //#+4
    +'|'
      +'('+pRefRoundBrackets+'|)' //#+5
      +'('+pRefFreeTail+'|)'      //#+6
      +'(?:'+pRefNo+')?'
    +')';
  gcRefTail=6; //group count

 //одна голая ссылка в нужном формате -> base, writing, [tail groups]
  pSingleTextRef=
     '('+pRefBase+')'
    +'('+pRefWri1+'|)' //любое из пояснений, или ничего -- чтобы число скобок не менялось
    + pRefTail;
  gcSingleTextRef=8;

  //теги
  pHrefOpen='<a href=[^>]*>';
  pHrefClose='</a>';

 //одна href-ссылка в нужном формате  -> base, writing, [tail groups]
  pSingleHref=
      pHrefOpen
    + pSingleTextRef
    + pHrefClose
    + pRefTail;  //хвост может быть как внутри SingleTextRef, так и снаружи. Хвост может быть пустым, так что беды это принести не должно

 //одна ссылка либо в <a href>, либо голая  -> a_href_base, a_href_writing, [a_href_tail], text_base, text_writing, [text_tail]
  pSingleRef=
     '(?:'
     +pSingleHref
    +'|'
     +pSingleTextRef
    +')'
   //когда матчим отдельные ссылки, строго проверяем, чтобы после них шёл разрыв слова -
   //иначе матчится даже палочка в </a>
    +'(?=$|[\p{Z}\p{P}])';
 {
  Группы:
    1, 2 - href\base
    (3, 4), (5, 6), (7, 8) - href\tail\versions
    (9, 10), (11, 12), (13, 14) - href\tail\version
    15, 16 - text\base
    (17, 18), (19, 20), (21, 22) - text\tail\version
 }

 //одна ссылка в a href с любым содержимым
 //в содержимом не должно быть теговых знаков <> - это запрещено HTML (если нарушено - останутся ошмётки <a>, и позже матчнутся в проверке)
  pHyperRef=
      pHrefOpen
    + '([^<>]*)'
    + pHrefClose
    + pRefTail;

 //либо голая ссылка в нужном формате, либо <a href> с ЛЮБЫМ содержимым (мы так хотим: дальше будут проверки)
  pTextOrHyperRef = '(?:'+pSingleTextRef+'|'+pHyperRef+')';

  pRefNames= //case-insensitive, см. pXref
     'уст\.|'
    +'см\. тж\.|см\.|'
    +'ср\. тж\.|ср\.|'
    +'тж\. уст\.|тж\. редко|тж\.|'
    +'ант\.|'
    +'неправ\. вм\.|ошибочно вм\.|вм\.|'
    +'сокр\. см\.|сокр\. от|от сокр\.|сокр\.|'
    +'в отличие от|кн\. опред\. форма от|производное от|от'
    ;

 {
  Одноразовые (фиксите):
    тж. редко,
  Нельзя парсить:
    связ., связ.:, чаще, напр., как
 }

 //название ссылки + некоторое число рефов ("см <a href>СТАТЬЯ1</a> и СТАТЬЯ2")
  pXref=
     '\s*'
    +'(?i)('+pRefNames+')(?-i)\s' //case insensitive
    +'('+pTextOrHyperRef+'(?:\s*[\,\;и]\s+'+pTextOrHyperRef+')*)' //любое число ссылок больше одной, через запятую
    +'\s*';

 //К сожалению, регэкспы не могут матчить повторяющиеся группы (будет заматчена последняя),
 //так что элементы внутри наборы ссылок надо матчить отдельно

var
  preXref: TPerlRegEx;
  preSingleRef: TPerlRegEx;
 //после сложного матча всех ссылок матчим по-простому, чтобы проверить, что мы нигде не ошиблись
  preHrefOpen: TPerlRegEx;
  preHrefClose: TPerlRegEx;


{ Находит в строке все элементы ссылочного типа и регистрирует их в записи Sense }
procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);
var xr0, xr1, xr2, xr3: UnicodeString;
  tmp: UnicodeString;
  xr_off: integer;
begin
  preXref.Subject := UTF8String(ln);
  if not preXref.Match then exit;
  preXref.Replacement := '';
  preSingleRef.Replacement := '';

  repeat
    xr0 := UnicodeString(preXref.Groups[1]); //тип ссылки

   //Может быть несколько: "см. ОДНО, ДРУГОЕ"
   //Матчим все
    preSingleRef.Subject := preXref.Groups[2];
    Assert(preSingleRef.Match); //не может быть чтоб не матчилось
    repeat
     //Вычисляем отступ номера группы матчей
      if preSingleRef.Groups[0][1]='<' then
       //Первая группа -- с <a href>
        xr_off := 0
      else
       //Вторая группа -- просто ссылка
        xr_off :=  gcSingleTextRef + gcRefTail;

     //Читаем базу и расшифровку
      xr1 := DropVariantIndicator(UnicodeString(preSingleRef.Groups[xr_off+1]));
      xr2 := UnicodeString(preSingleRef.Groups[xr_off+2]); //индикатор удалим после снятия скобок

     //Оба поля сразу не должны быть пустыми - если пустые, скорее всего,
     //мы промазали с номерами групп (кто-то менял регэкс)
      if (xr1='') and (xr2='') then
        raise EUnsupportedXref.Create('Both reading and writing fields of xref are empty -- WTF');

     //Удаляем скобки из записи
      if xr2<>'' then begin
        Assert(xr2[1]='【');
        xr2 := copy(xr2,2,Length(xr2)-2);
      end;
      xr2 := DropVariantIndicator(xr2); //после удаления скобкок

     //Проверяем, что внутри 【】 осталось только CJK
      if (xr2<>'') and (EvalChars(xr2) and (EV_CYR or EV_LATIN) <> 0) then
        raise EUnsupportedXref.Create('Illegal symbols in Xref kanji block');

     //Бесскобочные хвосты отлавливаются, но не поддерживаются. Почти не встречаются.
     //Хвостов у нас из-за комбинаций три набора групп
      if (preSingleRef.Groups[xr_off+4]<>'') or (preSingleRef.Groups[xr_off+6]<>'')
      or (preSingleRef.Groups[xr_off+8]<>'') then
        raise EUnsupportedXref.Create('Invalid Xref tail -- non-bracketed tail');
      if xr_off=0 then //доп. хвост для href-версии
        if (preSingleRef.Groups[xr_off+10]<>'') or (preSingleRef.Groups[xr_off+12]<>'')
        or (preSingleRef.Groups[xr_off+14]<>'') then
          raise EUnsupportedXref.Create('Invalid Xref tail -- non-bracketed tail');

     //Проверяем на скобочные продолжения -- そとわ【外輪】(の足)
      xr3 := UnicodeString(preSingleRef.Groups[xr_off+3]);
      if xr3='' then xr3 := UnicodeString(preSingleRef.Groups[xr_off+5]);
      if xr3='' then xr3 := UnicodeString(preSingleRef.Groups[xr_off+7]);
      if xr_off=0 then begin  //доп. хвост для href-версии
        if xr3='' then xr3 := UnicodeString(preSingleRef.Groups[xr_off+9]);
        if xr3='' then xr3 := UnicodeString(preSingleRef.Groups[xr_off+11]);
        if xr3='' then xr3 := UnicodeString(preSingleRef.Groups[xr_off+13]);
      end;
      if xr3<>'' then begin
        xr3 := Trim(xr3);

       //Почему-то поймали в скобочное выражение бесскобочный хвост.
       //Раньше проверка была нужна, теперь оставил только на всякий случай
        if (Length(xr3)<2) or (xr3[1]<>'(') or (xr3[Length(xr3)]<>')') then
          raise EUnsupportedXref.Create('Invalid Xref tail -- non-bracketed bracket WTF');
        xr3 := copy(xr3,2,Length(xr3)-2);

        if EvalChars(xr3) and (EV_CYR or EV_LATIN) <> 0 then
         //たていれ【達入れ】(татэирэ)
          raise EUnsupportedXref.Create('Cyrillic/latin reading in (round brackets) -- format error');

        if xr3[1]='～' then
         //こ【粉】(～にする) -- просто удаляем тильду (у нас для каждого такого шаблона своя статья)
          delete(xr3,1,1);

       //Если у ссылки есть скобочная часть - это продолжение, напр. あな【穴】(を明ける)
       //Продолжение может уже включать в себя написание:
        if (xr2<>'') and (pos(xr2,xr3)>0) then begin
         //くも【雲】(雲の上)
        end else
       //или чтение
        if (pos(xr1,xr3)>0) then begin
        end else begin
         //в остальных случаях считаем, что это чистое продолжение
         //встречались ошибки вроде ぎんこう(眼の銀行), но у нас нет никаких способов их поймать -- и их мало (пара штук)
          if xr2<>'' then
            xr3 := xr2+xr3
          else
            xr3 := xr1+xr3;
        end;

       //Теперь у нас
       //xr1: база (кана/кандзи?)
       //xr2: кандзи?
       //xr3: полное выражение с базой

       //Пробуем сделать и кану
        if xr2<>'' then begin
          tmp := repl(xr3, xr2, xr1);
          if EvalChars(tmp) and EV_KANJI = 0 then begin
           //Ура, получилось!
            xr2 := xr3; //написание
            xr1 := tmp; //чтение
          end else begin
            xr2 := ''; //не получилось чтения
            xr1 := xr3; //написание
          end;
        end else begin
        //Ничего не остаётся, как превратить ссылку в одиночную на выражение целиком, без расшифровки
          xr2 := '';
          xr1 := xr3;
        end;

      end;

      xr1 := FixEllipsis(xr1);
      xr2 := FixEllipsis(xr2);

     //Пока что баним записи с точечками (несколько вариантов написания) - вообще их надо разделять
     //Обратите внимание, что есть ДВЕ разные похожие точки - другая разделяет слова, и является просто буквой
      if (pos('･', xr1)>0) or (pos('･', xr2)>0) then
        raise EUnsupportedXref.Create('dot in xref value');

     //Объединяем
      if xr2<>'' then xr1 := xr1+'・'+xr2;

     //Всё это ненормально
      if pos('[', xr1)>0 then
        raise EUnsupportedXref.Create('[ in xref value');
      if pos('(', xr1)>0 then
        raise EUnsupportedXref.Create('( in xref value');
      if pos('/', xr1)>0 then //этих очень мало -- 3 штуки
        raise EUnsupportedXref.Create('/ in xref value');

      if (xr0='см. тж.') or (xr0='ср. тж.') or (xr0='см.') or (xr0='ср.')
      or (xr0='тж.') then
       //эти xref-ы не требуют пояснений
        xr0 := ''
      else
      if (xr0='тж. уст.') then
        xr0 := 'уст.'
      else
      if (xr0='тж. редко') then
        xr0 := 'редко'
      else
      if (xr0='неправ. вм.') or (xr0='ошибочно вм.') then
        xr0 := 'вм.'
      else
      if (xr0='сокр. см.') or (xr0='сокр. от') or (xr0='от сокр.') then
        xr0 := 'сокр.'
      else
      if (xr0='кн. опред. форма от') or (xr0='производное от') then
        xr0 := 'от';

      if xr0='ант.' then
        sn.AddAnt(xr1)
      else
        sn.AddXref(xr0, xr1);

      preSingleRef.Replace;
    until not preSingleRef.MatchAgain;

   //Проверяем, что ошмёток ссылок не осталось
    AssertNoHrefsLeft(preSingleRef.Subject);

    preXref.Replace;
  until not preXref.MatchAgain;

{
 //Проверяем, что ошмёток ссылок не осталось
 //Здесь это не очень правильно! В строке могли остаться легальные <a href=>
  AssertNoHrefsLeft(preXref.Subject);
}

  ln := UnicodeString(preXref.Subject); //after replacements
end;

{ True, если в строке есть ошмётки <a href> }
function HasHrefParts(const ln: UTF8String): boolean;
begin
  preHrefOpen.Subject := ln;
  preHrefClose.Subject := ln;
  Result := preHrefOpen.Match or preHrefClose.Match;
end;

{ Бросает исключение, если в строке остались ошмётки <a href>
 Полезно вызывать для контроля после разбора какого-то Xref-блока на его остатки.
 Учтите, что в строках могут легально встречаться невынутые href-ссылки:
 не все ссылки должны превращаться в <see also>. }
procedure AssertNoHrefsLeft(const ln: UTF8String);
begin
  if HasHrefParts(ln) then
    raise EUnsupportedXref.Create('Remains of <a href> are left in string -- prob. unsupported href structure');
end;


{
Другой вариант:
Все ссылки в формате <a href="#1-107-1-37">.*</a>, без разбору.

Встречаются такие формы:
  <i>см.</i> <a href="#1-107-1-37">ごぎょう【五行】</a>

Этот вариант мы используем только для того, чтобы составить список всех
существующих вариантов предисловия к оформленной ссылке.
}

const
  pAttributeValue='"[^"]*"|[^\s>]*'; //-> 1 group
  pLink = '<a href=('+pAttributeValue+')[^>]*>([^<]*?)</a>';
  pHrefExpr = '(?<=^|\s)([^\s]+?)(?:\s+)'+pLink;
  pSimpleHref = '<a href=';

var
  preHrefExpr: TPerlRegEx;
  preSimpleHref: TPerlRegEx;

procedure MatchHrefs(const ln: string);
begin
  preHrefExpr.Subject := UTF8String(ln);
  if preHrefExpr.Match then
  repeat
    Inc(xrefStats.HrefExpr);
    AllHrefTypes.AddUnique(UnicodeString(preHrefExpr.Groups[1]));
  until not preHrefExpr.MatchAgain;

  preSimpleHref.Subject := UTF8String(ln);
  if preSimpleHref.Match then
  repeat
    Inc(xrefStats.SimpleHref);
  until not preSimpleHref.MatchAgain;
end;


initialization
  preXref := Regex(pXref);
  preSingleRef := Regex(pSingleRef);
  preHrefOpen := Regex(pHrefOpen);
  preHrefClose := Regex(pHrefClose);

  preHrefExpr := Regex(pHrefExpr);
  preSimpleHref := Regex(pSimpleHref);
  AllHrefTypes.Clear;
  AllHrefTypes.Comparison := UniCompareStr;
  FillChar(xrefStats, SizeOf(xrefStats), 0);

finalization
  FreeAndNil(preSimpleHref);
  FreeAndNil(preHrefExpr);

  FreeAndNil(preHrefClose);
  FreeAndNil(preHrefOpen);
  FreeAndNil(preSingleRef);
  FreeAndNil(preXref);

end.
