unit WarodaiParticles;
{
Функции разбора строки.
Решают, какие в строке встречаются "флаги" (какие маркеры добавлять),
и какие из этих флагов нужно выбросить.
(А какие, возможно, заменить).

Отвечают за разбор:
- Маркеров (<i>кн. уст.</i>)
- Неявных макеров (<i>счётный суф. для круглых предметов</i>)
- Языка (<i>англ. leadership</i>)
- Ссылок (<i>см.</i> <a href="#1-177-2-17">じはつせい</a>)
 и связей (<i>ант.</i> <a href="#1-268-2-42">いん【陰】</a>)

Известные типы связей
  <i>см.</i> <a href="#1-604-2-61">みしり</a>
  <i>ср.</i> <a href="#1-737-1-59">おおみそか</a>
  <i>ант.</i> <a href="#1-268-2-42">いん【陰】</a>


Флаги могут идти в любом порядке внутри блока <i></i> либо через пробелы, либо в отд. блоках:
  <i>юр.</i>
  <i>уст. вежл.</i>
  <i>ономат.</i> <i>устар.</i>
Флаги могут смешиваться в одном блоке с не флагами:
  <i>кн. см.</i> (номер статьи)
Список известных флагов есть в файле warodai_abrv.dsl, который включён в DSL-сборку словаря.

}

interface
uses Warodai;
{$INCLUDE 'Warodai.inc'}

{
Частицы.
Список частиц вародая доступен в файле warodai_abrv.dsl в DSL-версии словаря,
но не все из них надо считать флагами.
Некоторые остаются в тексте, напр.:
  (англ. apple pie) яблочный пирог
Кроме того, нужно детектировать теги с большой осторожностью; напр. встречаются
такие фразы:
  <i>после гл. в буд. вр. и наречной формы прил.</i> если даже, хотя бы;
Ни тега "гл." (глагол), ни тега "буд." (буддизм) здесь нет.

Поэтому удаляются только те слова, которые находятся в скобках, где все прочие
слова также определены как теги (не обязательно для удаления).
}
type
  TWarodaiTagFlag = (
    tfLang,  //задаёт язык-источник
    tfField, //задаёт область применения
    tfLink   //задаёт ссылку на другую статью
  );
  TWarodaiTagFlags = set of TWarodaiTagFlag;
  TWarodaiTagDefinition = record
    abbr: string;
    desc: string;
    fl: TWarodaiTagFlags;
    edict: string; //в какой тег едикта переводить
  end;

const
  WD_FLAGS: array[0..166] of TWarodaiTagDefinition = (
    (abbr: 'ав.';       desc: 'авиация'; fl: [tfField]; edict: ''),
    (abbr: 'авт.';      desc: 'автомобильное дело'; fl: [tfField]; edict: ''),
    (abbr: 'альп.';     desc: 'альпинизм'; fl: [tfField]; edict: ''),
    (abbr: 'амер.';     desc: 'американизм'; fl: [tfLang]; edict: ''),
    (abbr: 'анат.';     desc: 'анатомия'; fl: [tfField]; edict: ''),
    (abbr: 'англ.';     desc: 'английский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'ант.';      desc: 'антоним'; fl: []; edict: ''),
    (abbr: 'антр.';     desc: 'антропология'; fl: [tfField]; edict: ''),
    (abbr: 'археол.';   desc: 'археология'; fl: [tfField]; edict: ''),
    (abbr: 'архит.';    desc: 'архитектура'; fl: [tfField]; edict: ''),
    (abbr: 'астр.';     desc: 'астрономия'; fl: [tfField]; edict: ''),
    (abbr: 'биол.';     desc: 'биология'; fl: [tfField]; edict: ''),
    (abbr: 'биохим.';   desc: 'биохимия'; fl: [tfField]; edict: ''),
    (abbr: 'бирж.';     desc: 'биржевой термин'; fl: [tfField]; edict: ''),
    (abbr: 'бот.';      desc: 'ботаника'; fl: [tfField]; edict: ''),
    (abbr: 'бран.';     desc: 'бранное слово'; fl: []; edict: 'X'),
    (abbr: 'буд.';      desc: 'буддизм'; fl: [tfField]; edict: 'Buddh'),
    (abbr: 'букв.';     desc: 'буквально'; fl: []; edict: ''),
    (abbr: 'бухг.';     desc: 'бухгалтерия'; fl: [tfField]; edict: ''),
    (abbr: 'вежл.';     desc: 'вежливое слово'; fl: []; edict: 'pol'),
    (abbr: 'венг.';     desc: 'венгерский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'воен.';     desc: 'военный термин'; fl: [tfField]; edict: 'mil'),
    (abbr: 'вопр.';     desc: 'вопросительная частица'; fl: []; edict: 'part'), //потеря смысла!
    (abbr: 'воскл.';    desc: 'восклицательная частица'; fl: []; edict: 'part'), //потеря смысла!
    (abbr: 'геогр.';    desc: 'география'; fl: [tfField]; edict: ''),
    (abbr: 'геод.';     desc: 'геодезия'; fl: [tfField]; edict: ''),
    (abbr: 'геол.';     desc: 'геология'; fl: [tfField]; edict: ''),
    (abbr: 'геом.';     desc: 'геометрия'; fl: [tfField]; edict: 'geom'),
    (abbr: 'гидр.';     desc: 'гидрология, гидротехника'; fl: [tfField]; edict: ''),
    (abbr: 'гл.';       desc: 'глагол'; fl: []; edict: ''), //TODO: нужно хотя бы пытаться угадать edict-форму глагола по его смыслу
    (abbr: 'гол.';      desc: 'голландский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'голл.';     desc: 'голландский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'горн.';     desc: 'горное дело'; fl: [tfField]; edict: ''),
    (abbr: 'грам.';     desc: 'грамматика'; fl: []; edict: 'gram'),
    (abbr: 'греч.';     desc: 'греческий язык'; fl: [tfLang]; edict: ''),
    (abbr: 'груб.';     desc: 'грубое слово'; fl: []; edict: 'X'),
    (abbr: 'дет.';      desc: 'детская речь'; fl: []; edict: 'chn'),
    (abbr: 'диал.';     desc: 'диалектизм'; fl: []; edict: ''),
    (abbr: 'дип.';      desc: 'дипломатический термин'; fl: [tfField]; edict: ''),
    (abbr: 'жарг.';     desc: 'жаргонное слово'; fl: []; edict: 'sl'),
    (abbr: 'ж.-д.';     desc: 'железнодорожное дело'; fl: [tfField]; edict: ''),
    (abbr: 'женск.';    desc: 'женская речь'; fl: []; edict: 'fem'),
    (abbr: 'жив.';      desc: 'живопись'; fl: [tfField]; edict: ''),
    (abbr: 'зоол.';     desc: 'зоология'; fl: [tfField]; edict: ''),
    (abbr: 'ирон.';     desc: 'в ироническом смысле'; fl: []; edict: ''),
    (abbr: 'иск.';      desc: 'искусство'; fl: [tfField]; edict: ''),
    (abbr: 'искаж.';    desc: 'искажение'; fl: []; edict: ''),
    (abbr: 'исп.';      desc: 'испанский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'ист.';      desc: 'исторический термин'; fl: [tfField]; edict: ''),
    (abbr: 'ит.';       desc: 'итальянский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'итал.';     desc: 'итальянский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'ихт.';      desc: 'ихтиология'; fl: [tfField]; edict: ''),
    (abbr: 'как опред.'; desc: 'в функции определения'; {修飾語} fl: []; edict: ''),
    (abbr: 'как сказ.'; desc: 'в функции сказуемого'; {述語} fl: []; edict: ''),
    (abbr: 'карт.';     desc: 'карточный термин'; fl: [tfField]; edict: ''),
    (abbr: 'кино';      desc: 'кинематография'; fl: [tfField]; edict: ''),
    (abbr: 'кит.';      desc: 'китайский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'кн.';       desc: 'книжный стиль'; fl: []; edict: ''),
    (abbr: 'ком.';      desc: 'коммерческий термин'; fl: [tfField]; edict: ''),
    (abbr: 'кор.';      desc: 'корейский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'кул.';      desc: 'кулинария'; fl: [tfField]; edict: 'food'),
    (abbr: 'лат.';      desc: 'латинский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'лес.';      desc: 'лесное дело'; fl: [tfField]; edict: ''),
    (abbr: 'лингв.';    desc: 'лингвистика'; fl: [tfField]; edict: 'ling'),
    (abbr: 'лит.';      desc: 'литературоведение'; fl: [tfField]; edict: ''),
    (abbr: 'лог.';      desc: 'логика'; fl: [tfField]; edict: ''),
    (abbr: 'малайск.';  desc: 'малайский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'мат.';      desc: 'математика'; fl: [tfField]; edict: 'math'),
    (abbr: 'мед.';      desc: 'медицина'; fl: [tfField]; edict: ''),
    (abbr: 'межд.';     desc: 'междометие'; {感動詞} fl: []; edict: 'int'),
    (abbr: 'мест.';     desc: 'местоимение'; {代名詞} fl: []; edict: 'pn'),
    (abbr: 'метал.';    desc: 'металлургия'; fl: [tfField]; edict: ''),
    (abbr: 'метеор.';   desc: 'метеорология'; fl: [tfField]; edict: ''),
    (abbr: 'мин.';      desc: 'минералогия'; fl: [tfField]; edict: ''),
    (abbr: 'миф.';      desc: 'мифология'; fl: [tfField]; edict: ''),
    (abbr: 'мор.';      desc: 'морской термин'; fl: [tfField]; edict: ''),
    (abbr: 'муз.';      desc: 'музыка'; fl: [tfField]; edict: ''),
    (abbr: 'назв.';     desc: 'название'; fl: []; edict: ''),
    (abbr: 'нареч.';    desc: 'наречие'; {副詞} fl: []; edict: 'adv'),
    (abbr: 'нем.';      desc: 'немецкий язык'; fl: [tfLang]; edict: ''),
    (abbr: 'неперех.';  desc: 'непереходный глагол'; {自動詞} fl: []; edict: 'vi'),
    (abbr: 'обр.';      desc: 'образное выражение'; fl: []; edict: 'id'),
    (abbr: 'ономат.';   desc: 'ономатопоэтическое слово'; {擬声語} fl: []; edict: 'on-mim'),
    (abbr: 'опт.';      desc: 'оптика'; fl: [tfField]; edict: ''),
    (abbr: 'орнит.';    desc: 'орнитология'; fl: [tfField]; edict: ''),
    (abbr: 'отриц.';    desc: 'отрицание'; fl: []; edict: ''),
    (abbr: 'палеонт.';  desc: 'палеонтология'; fl: [tfField]; edict: ''),
    (abbr: 'перен.';    desc: 'в переносном значении'; fl: []; edict: 'id'), //потеря смысла!
    (abbr: 'перех.';    desc: 'переходный глагол'; {他動詞} fl: []; edict: 'vt'),
    (abbr: 'побуд.';    desc: 'побудительный залог'; {使役相} fl: []; edict: ''),
    (abbr: 'повел.';    desc: 'повелительное наклонение'; {命令法} fl: []; edict: ''),
    (abbr: 'погов.';    desc: 'поговорка'; fl: []; edict: 'expr'), //потеря смысла!
    (abbr: 'полигр.';   desc: 'полиграфия'; fl: [tfField]; edict: ''),
    (abbr: 'полит.';    desc: 'политический термин'; fl: [tfField]; edict: ''),
    (abbr: 'португ.';   desc: 'португальский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'посл.';     desc: 'пословица'; fl: []; edict: 'expr'), //потеря смысла!
    (abbr: 'постпоз.';  desc: 'постпозиционно'; fl: []; edict: ''),
    (abbr: 'постпозиц.'; desc: 'постпозиционно'; fl: []; edict: ''),
    (abbr: 'поэт.';     desc: 'поэтическое слово'; fl: []; edict: 'poet'),
    (abbr: 'предл.';    desc: 'предложение'; fl: []; edict: 'expr'),
    (abbr: 'презр.';    desc: 'презрительно'; fl: []; edict: 'derog'),
    (abbr: 'презрит.';  desc: 'презрительно'; fl: []; edict: 'derog'),
    (abbr: 'пренебр.';  desc: 'пренебрежительно'; fl: []; edict: 'derog'),
    (abbr: 'преф.';     desc: 'префикс'; {接頭辞} fl: []; edict: 'pref'),
    (abbr: 'прил.';     desc: 'прилагательное'; fl: []; edict: 'adj'),
    (abbr: 'прост.';    desc: 'просторечие'; fl: []; edict: 'vulg'),
    (abbr: 'противит.'; desc: 'противительный союз'; {逆説的接続詞} fl: []; edict: ''),
    (abbr: 'противоп.'; desc: 'противоположное значение'; fl: []; edict: ''),
    (abbr: 'псих.';     desc: 'психология'; fl: [tfField]; edict: ''),
    (abbr: 'психол.';   desc: 'психология'; fl: [tfField]; edict: ''),
    (abbr: 'радио';     desc: 'радиотехника'; fl: [tfField]; edict: ''),
    (abbr: 'разг.';     desc: 'разговорное слово'; fl: []; edict: 'col'),
    (abbr: 'рел.';      desc: 'религия'; fl: [tfField]; edict: ''),
    (abbr: 'редко';     desc: 'редко'; fl: []; edict: 'rare'),
    (abbr: 'русск.';    desc: 'русский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'санскр.';   desc: 'санскритский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'связ.';     desc: 'связанное употребление'; fl: [tfLink]; edict: ''),
    (abbr: 'синт.';     desc: 'относящийся к синтоизму'; fl: [tfField]; edict: ''),
    (abbr: 'сказ.';     desc: 'сказуемое'; {述語} fl: []; edict: ''),
    (abbr: 'в слож. сл.'; desc: 'в сложных словах'; fl: []; edict: ''),
    (abbr: 'в сложн. сл.'; desc: 'в сложных словах'; fl: []; edict: ''),
    (abbr: 'сложн. сл.'; desc: 'сложное слово'; fl: []; edict: ''),
    (abbr: 'см.';       desc: 'смотри'; fl: [tfLink]; edict: ''),
    (abbr: 'соед.';     desc: 'соединительный союз'; {並立的接続詞} fl: []; edict: ''),
    (abbr: 'сокр.';     desc: 'сокращение'; fl: []; edict: 'abbr'),
    (abbr: 'спорт.';    desc: 'физкультура и спорт'; fl: [tfField]; edict: ''),
    (abbr: 'ср.';       desc: 'сравни'; fl: [tfLink]; edict: ''),
    (abbr: 'стр.';      desc: 'строительное дело'; fl: [tfField]; edict: ''),
    (abbr: 'страд.';    desc: 'страдательный залог'; fl: []; edict: ''),
    (abbr: 'студ.';     desc: 'студенческое слово'; fl: [tfField]; edict: ''),
    (abbr: 'суф.';      desc: 'суффикс'; {接尾語} fl: []; edict: 'suf'),
    (abbr: 'сущ.';      desc: 'имя существительное'; fl: []; edict: 'n'),
    (abbr: 'с.-х.';     desc: 'сельское хозяйство'; fl: [tfField]; edict: ''),
    (abbr: 'твор.';     desc: 'творительный падеж'; fl: []; edict: ''),
    (abbr: 'театр.';    desc: 'театр'; fl: [tfField]; edict: ''),
    (abbr: 'телев.';    desc: 'телевидение'; fl: [tfField]; edict: ''),
    (abbr: 'телевид.';  desc: 'телевидение'; fl: [tfField]; edict: ''),
    (abbr: 'тех.';      desc: 'техника'; fl: [tfField]; edict: ''),
    (abbr: 'тж.';       desc: 'также'; fl: [tfLink]; edict: ''),
    (abbr: 'топ.';      desc: 'топография'; fl: [tfField]; edict: ''),
    (abbr: 'тур.';      desc: 'турецкий язык'; fl: [tfLang]; edict: ''),
    (abbr: 'указ.';     desc: 'указательное местоимение'; {指示代名詞} fl: []; edict: ''),
    (abbr: 'усил.';     desc: 'усилительная частица'; {強調の助辞} fl: []; edict: ''),
    (abbr: 'уст.';      desc: 'устаревшее слово'; fl: []; edict: 'obs'),
    (abbr: 'уступ.';    desc: 'уступительная частица'; {譲歩の助辞} fl: []; edict: ''),
    (abbr: 'утверд.';   desc: 'утвердительная частица'; {肯定の助辞} fl: []; edict: ''),
    (abbr: 'фарм.';     desc: 'фармацевтика'; fl: [tfField]; edict: ''),
    (abbr: 'физ.';      desc: 'физика'; fl: [tfField]; edict: 'physics'),
    (abbr: 'физиол.';   desc: 'физиология'; fl: [tfField]; edict: ''),
    (abbr: 'фил.';      desc: 'философия'; fl: [tfField]; edict: ''),
    (abbr: 'филос.';    desc: 'философия'; fl: [tfField]; edict: ''),
    (abbr: 'фин.';      desc: 'финансовый термин'; fl: [tfField]; edict: ''),
    (abbr: 'фото';      desc: 'фотография'; fl: [tfField]; edict: ''),
    (abbr: 'фр.';       desc: 'французский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'франц.';    desc: 'французский язык'; fl: [tfLang]; edict: ''),
    (abbr: 'хим.';      desc: 'химия'; fl: [tfField]; edict: ''),
    (abbr: 'часто';     desc: 'часто'; fl: []; edict: 'pop'), //не уверен
    (abbr: 'част.';     desc: 'частица'; {助辞} fl: []; edict: 'prt'),
    (abbr: 'числ.';     desc: 'имя числительное'; {数詞} fl: []; edict: 'num'), //не уверен
    (abbr: 'числит.';   desc: 'имя числительное'; fl: []; edict: 'num'), //не уверен
    (abbr: 'шахм.';     desc: 'термин из шахмат'; fl: [tfField]; edict: ''),
    (abbr: 'школ.';     desc: 'школьное слово'; fl: [tfField]; edict: ''),
    (abbr: 'эк.';       desc: 'экономика'; fl: [tfField]; edict: ''),
    (abbr: 'эл.';       desc: 'электротехника'; fl: [tfField]; edict: ''),
    (abbr: 'энт.';      desc: 'энтомология'; fl: [tfField]; edict: ''),
    (abbr: 'эпист.';    desc: 'эпистолярный стиль'; fl: []; edict: ''),
    (abbr: 'юр.';       desc: 'юридический термин'; fl: [tfField]; edict: '')
  );

{
type
  TFlag = integer; //ссылка на таблицу
  TFlags = array of integer;

function ExtractFlags(var s: string): TFlags;
}


{
RemoveFormatting()
Удаляет всё html-форматирование из строки, превращая её в голый текст.
}
function RemoveFormatting(const s: string): string;

implementation
uses StrUtils, UniStrUtils;

{
Разбор флагов.
}
type
  TBlock = record
    i_start: integer; //первый открывающий символ
    i_end: integer;   //последний закрывающий символ
    contents: string;
  end;

{ Находит следующий после i_beg блок <s_op...s_ed> в строке и возвращает его. }
function FindNextBlock(const s: string; const s_op, s_ed: string; i_beg: integer; out block: TBlock): boolean;
var i_len: integer;
begin
  block.i_start := posex(s_op, s, i_beg);
  if block.i_start<=0 then begin
    Result := false;
    exit;
  end;

  block.i_end := posex(s_ed, s, block.i_start);
  if block.i_end<=0 then begin
    Result := false;
    exit;
  end;

  i_len := block.i_end-(block.i_start+Length(s_op));
  block.contents := copy(s, block.i_start+Length(s_op), i_len);
  block.i_end := block.i_end + Length(s_ed) - 1;
  Result := true;
end;

{ Удаляет логический элемент строки с i_op по i_ed включительно. Схлопывает
 пробелы и т.п.
 Вызывать можно не только для блоков, полученных из FindNextBlock. }
procedure DeleteBlock(var s: string; i_op, i_ed: integer);
var had_spaces: boolean;
begin
 //Удаляем пробелы по обе стороны от вырезанного куска, чтобы не получалось длинных дыр
  had_spaces := false;
  while (i_op>1) and (s[i_op-1]=' ') do begin
    had_spaces := true;
    Dec(i_op);
  end;
  while (i_ed<Length(s)) and (s[i_ed+1]=' ') do begin
    had_spaces := true;
    Inc(i_ed);
  end;

 //оставляем один пробел, если только блок не впритык к краю (там пробелов не нужно)
  if had_spaces and (i_op<>1) and (i_ed<>Length(s)) then
    if (s[i_op]=' ') then Inc(i_op) else Dec(i_ed);

  delete(s, i_op, i_ed-i_op+1);
end;

procedure SkipSpaces(const s: string; var i_pos: integer);
begin
  while (i_pos<=Length(s)) and (s[i_pos]=' ') do
    Inc(i_pos);
end;

{ Находит самое длинное совпадение среди флагов для текста, идущего в строке s
 начиная с положения i_pos.
 Пробелы пропускает.
 Возвращает -1, если дальнейших совпадений нет. }
function FindNextFlag(const s: string; var i_pos: integer): integer;
var i: integer;
  max_len: integer;
begin
  Result := -1;
  max_len := 0;
  SkipSpaces(s, i_pos);
  if i_pos>Length(s) then exit;

  for i := 0 to Length(WD_FLAGS) - 1 do
    if StrMatch(PChar(@s[i_pos]), PChar(WD_FLAGS[i].abbr))=Length(WD_FLAGS[i].abbr) then
    if (Result<0) or (max_len<Length(WD_FLAGS[i].abbr)) then
    begin
      Result := i;
      max_len := Length(WD_FLAGS[i].abbr);
    end;

  if Result>=0 then begin
    i_pos := i_pos + Length(WD_FLAGS[Result].abbr);
    SkipSpaces(s, i_pos); //на случай если там завершающие пробелы
  end;
end;

{ True, если это был флаг-блок, мы все флаги разобрали, и блок надо удалить.
 False, если разобрать блок не получилось, его надо оставить в покое, а fl не использовать }
{
function ParseFlags(const s: string; out fl: TFlags): boolean;
var  block_pos: integer;
  flag: integer;
  flags_this_block
begin
  SetLength(fl, 0);
  if Trim(s)='' then begin
    Result := true;
    exit;
  end;


    flags_this_block := 0;
    block_pos := 1;
    flag := FindNextFlag(block.contents, block_pos);
    while flag>=0 do begin

    end;

end;


function ExtractFlags(var s: string): TFlags;
var block: TBlock;
  i_pos: integer;
  fl: TFlags;
  i: integer;
begin
  SetLength(Result, 0);
  i_pos := 1;
  while FindNextBlock(s, '<i>', '</i>', i_pos, block) do begin
    if ParseFlags(block.contents, fl) then begin
      SetLength(Result, Length(Result)+Length(flags));
      for i := 0 to Length(fl) - 1 do


      DeleteBlock(block.i_start, block.i_end);
    end else



    i_pos := block.i_end+1;
  end;
end;
}


{ Находит блок, удаляет его и возвращает содержимое }
function RemoveBlock(var s: string; const s_op, s_ed: string; i_beg: integer; out contents: string): boolean;
var block: TBlock;
begin
  Result := FindNextBlock(s, s_op, s_ed, i_beg, block);
  if not Result then exit;
  contents := block.contents;
  DeleteBlock(s, block.i_start, block.i_end);
  Result := true;
end;


function RemoveFormatting(const s: string): string;
var tag: string;
begin
  Result := s;
  while RemoveBlock(Result, '<', '>', 1, tag) do begin end;
end;

end.
