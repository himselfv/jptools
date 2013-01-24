unit WarodaiXrefs;
{
Matches various XReferences found in Warodai.
}

interface
uses Warodai, WcUtils, EdictWriter, PerlRegEx, PerlRegExUtils;
{$INCLUDE 'Warodai.inc'}

procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);


{
Можно посчитать, какие виды предисловий ко ссылкам встречаются в файле.
Передавайте в MatchHrefs все строки с HTML-разметкой, а в конце проверьте AllHrefTypes;
}

var
  AllHrefTypes: TList<string>;
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
  pRefWri1='【'+pCJKRefStr+'】'; //расшифровка в формате あわ【泡】
  pRefNo='\s*[0-9]{1,3}'; //опционально 1-2о цифры с пробелом или без -- ссылка на подзначение

  //Хвост ссылки
  pRefRoundBrackets = '\s?\(.*?\)'; //любое число символов - русских или японских - в скобках -- продолжение или пояснение
  pRefFreeTail = '\s?['+pCJKRefChar+']+?'; //некоторое число японских символов без скобок

 //одна ссылка в любом формате
  pSingleRef='('+pRefBase+')'
    +'('+pRefWri1+'|)' //любое из пояснений, или ничего -- чтобы число скобок не менялось
    +'(?:' //в любом порядке (RefNo может идти после скобок):
      +'(?:'+pRefNo+')?'
      +'('+pRefRoundBrackets+'|)'
      +'('+pRefFreeTail+')?'
    +'|'
      +'('+pRefRoundBrackets+'|)'
      +'(?:'+pRefNo+')?'
      +'('+pRefFreeTail+')?'
    +'|'
      +'('+pRefRoundBrackets+'|)'
      +'('+pRefFreeTail+')?'
      +'(?:'+pRefNo+')?'
    +')'
    ;

  pRefNames=
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

 //ссылка с названием и пробелами вокруг
  pXref=
      '\s*'
    +'('+pRefNames+')\s'
    +'('+pSingleRef+'(?:\s*[\,\;и]\s+'+pSingleRef+')*)' //любое число ссылок больше одной, через запятую
    +'\s*'; //заканчивается чем-нибудь

 //К сожалению, регэкспы не могут матчить повторяющиеся группы (будет заматчена последняя),
 //так что элементы внутри наборы ссылок надо матчить отдельно

var
  preXref: TPerlRegEx;
  preSingleRef: TPerlRegEx;


{ Находит в строке все элементы ссылочного типа и регистрирует их в записи Sense }
procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);
var xr0, xr1, xr2, xr3: UnicodeString;
  tmp: UnicodeString;
begin
  preXref.Subject := UTF8String(ln);
  if not preXref.Match then exit;
  preXref.Replacement := '';

  repeat
    xr0 := UnicodeString(preXref.Groups[1]); //тип ссылки

   //Может быть несколько: "см. ОДНО, ДРУГОЕ"
   //Матчим все
    preSingleRef.Subject := preXref.Groups[2];
    Assert(preSingleRef.Match); //не может быть чтоб не матчилось
    repeat
      xr1 := UnicodeString(preSingleRef.Groups[1]);
      xr2 := UnicodeString(preSingleRef.Groups[2]);
      DropVariantIndicator(xr1);
      DropVariantIndicator(xr2);

     //Удаляем скобки из записи
      if xr2<>'' then begin
        Assert(xr2[1]='【');
        xr2 := copy(xr2,2,Length(xr2)-2);
      end;

     //Бесскобочные хвосты отлавливаются, но не поддерживаются. Почти не встречаются.
      if (preSingleRef.GroupCount>3) and (preSingleRef.Groups[4]<>'') then
        raise EUnsupportedXref.Create('Invalid Xref tail -- non-bracketed tail');

     //Проверяем на скобочные продолжения -- そとわ【外輪】(の足)
      if (preSingleRef.GroupCount>2) and (preSingleRef.Groups[3]<>'') then begin
        xr3 := Trim(UnicodeString(preSingleRef.Groups[3]));

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

       {
        Раньше мы такому не верили и убивали, но теперь научились разбирать.
        raise EUnsupportedXref.Create('Xref contains (round bracket) free style addition -- unable to parse');
       }

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

     //Объединяем
      if xr2<>'' then xr1 := xr1+'・'+xr2;

     //Всё это ненормально
      if pos('[', xr1)>0 then
        raise EUnsupportedXref.Create('[ in xref value');
      if pos('(', xr1)>0 then
        raise EUnsupportedXref.Create('( in xref value');
     //А вот это нормально, но что с ними делать непонятно
     {$IFDEF BAN_ELLIPSIS}
      if pos('…', xr1)>0 then
        raise EUnsupportedXref.Create('... in xref value');
     {$ENDIF}
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

    until not preSingleRef.MatchAgain;

    preXref.Replace;
  until not preXref.MatchAgain;

  ln := UnicodeString(preXref.Subject); //after replacements
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
  pSimpleHref = '<a hr=';

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

  preHrefExpr := Regex(pHrefExpr);
  preSimpleHref := Regex(pSimpleHref);
  AllHrefTypes.Reset;
  AllHrefTypes.Comparison := UniCompareStr;
  FillChar(xrefStats, SizeOf(xrefStats), 0);

finalization
  FreeAndNil(preSimpleHref);
  FreeAndNil(preHrefExpr);

  FreeAndNil(preSingleRef);
  FreeAndNil(preXref);

end.
