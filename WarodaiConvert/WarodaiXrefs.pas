unit WarodaiXrefs;
{
Matches various XReferences found in Warodai.
}

interface
uses Warodai, WcUtils, EdictWriter, PerlRegEx, PerlRegExUtils;

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
  ぎんこう(眼の銀行)
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

 //одна ссылка в любом формате
  pSingleRef='('+pRefBase+')'
    +'('+pRefWri1+'|)' //любое из пояснений, или ничего -- чтобы число скобок не менялось
    +'(?:\s*[0-9]{1,3})?' //опционально 1-2о цифры с пробелом или без -- ссылка на подзначение, другой формат
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
    +pSingleRef+'(?:\s*[\,\;и]\s+'+pSingleRef+')*' //любое число ссылок больше одной, через запятую
    +'\s*'; //заканчивается чем-нибудь

var
  preXref: TPerlRegEx;



{ Находит в строке все элементы ссылочного типа и регистрирует их в записи Sense }
procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);
var xr0, xr1, xr2: UnicodeString;
  i: integer;
begin
  preXref.Subject := UTF8String(ln);
  if not preXref.Match then exit;
  preXref.Replacement := '';

  repeat
    xr0 := UnicodeString(preXref.Groups[1]); //тип ссылки

   //Может быть несколько: "см. ОДНО, ДРУГОЕ"
    for i := 0 to (preXref.GroupCount-1) div 2 - 1 do begin
      xr1 := UnicodeString(preXref.Groups[2+i*2]);
      xr2 := UnicodeString(preXref.Groups[2+i*2+1]);
      DropVariantIndicator(xr1);
      DropVariantIndicator(xr2);

      if xr2<>'' then begin
        Assert(xr2[1]='【'); //скобки
        xr1 := xr1+'・'+copy(xr2,2,Length(xr2)-2);
      end;

     //Всё это ненормально
      if pos('…', xr1)>0 then
        raise EIllegalXrefChar.Create('... in xref value');
      if pos('[', xr1)>0 then
        raise EIllegalXrefChar.Create('[ in xref value');
      if pos('(', xr1)>0 then
        raise EIllegalXrefChar.Create('( in xref value');
      if pos('/', xr1)>0 then //а вот это нормально, но что с ним делать непонятно
        raise EIllegalXrefChar.Create('/ in xref value');

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
    end;

    preXref.Replace;
  until not preXref.MatchAgain;

  ln := UnicodeString(preXref.Subject); //after replacements
end;



{
Другой вариант:
Все ссылки в формате <a href="#1-107-1-37">ごぎょう【五行】</a>, без разбору.

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
  preHrefExpr := Regex(pHrefExpr);
  preSimpleHref := Regex(pSimpleHref);
  AllHrefTypes.Reset;
  AllHrefTypes.Comparison := UniCompareStr;
  FillChar(xrefStats, SizeOf(xrefStats), 0);

finalization
  FreeAndNil(preSimpleHref);
  FreeAndNil(preHrefExpr);
  FreeAndNil(preXref);

end.
