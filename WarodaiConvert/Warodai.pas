unit Warodai;

interface
uses SysUtils;

type
  EParsingException = class(Exception);

//Если включено, то мы по мере возможности пытаемся починить не до конца
//поддерживаемые записи так, чтобы извлечь из них хоть что-то.
//То, что мы извлечём, должно гарантированно быть правильным (но, м.б., неполным).
{$DEFINE FIX_UNSUPPORTED}

{
Заголовок любой статьи - одна строчка, которая содержит все или некоторые
последующие поля:
  みる【見る･観る】(миру)〔1-603-1-64〕
  катакана 【разные･варианты･кандзи】(киридзи)〔ссылка〕
Чтение и ссылка присутствуют обязательно, всё остальное опционально.

Также возможная схема с несколькими словами:
  よもぎ, よもぎな【艾･蓬, 艾采】(ёмоги, ёмогина)〔1-241-2-52〕
  イロアききん, イロアしきん【イロア基金･EROA基金, イロア資金･EROA資金】(ироа-кйкин, ироа-сйкин)〔1-277-2-69〕
При этом и кандзи, и киридзи должны присутствовать в стольких же версиях,
а кандзи в каждой версии может присутствовать в нескольких вариантах (см. выше).

Также возможны флаги прямо в заголовке:
  アイレ(Айрэ) [геогр.]〔2-595-1-24〕
  おおくまざ【大熊座】(о:кумадза) [нов.]〔A-937-3-28〕

Встречаются следующие редкие неприятные моменты.
Несколько чтений на один кандзи:
  わらしべ, わらすべ【藁稭】(варасибэ, варасубэ)〔1-084-1-41〕 --- тогда кандзи копируется
Несколько киридзи на одно чтение:
  です(дэс, дэсў)〔1-231-2-35〕 --- так лишь в двух записях, пока никак не поддерживается, берём первое киридзи
}
const
  MaxWords = 4;
  MaxKanjiVariants = 10;

type
  TEntryWord = record
    s_reading: string;
    s_kanji: array[0..MaxKanjiVariants-1] of string;
    s_kanji_used: integer;
    s_kiriji: string;
  end;
  PEntryWord = ^TEntryWord;

  TEntryHeader = record
    s_ref: string;
    s_flags: string;
    words: array[0..MaxWords-1] of TEntryWord;
    words_used: integer;
  end;
  PEntryHeader = ^TEntryHeader;

procedure DecodeEntryHeader(s: string; out hdr: TEntryHeader);

{
После некоторых чтений и некоторых записей кандзи идёт римская запись номера
варианта:
  みるI
  みるIV
  見るII
Нижеследующая функция удаляет эту часть из строки.
}
procedure DropVariantIndicator(var s: string);

{
Иногда перевод содержит несколько варианто, тогда они пронумерованы так:
1) перевод 1
2) перевод 2
Так может продолжаться до двухзначных чисел включительно.
Эта функция удаляет из строки такой элемент и возвращает его отдельно, или возвращает false.
}
function PopTranslationNumber(var ln: string; out num: string): boolean;

{
Удаляет точки и точки с запятой в конце статьи (но не существенную пунктуацию,
типа вопросительных знаки и восклицаний).
}
procedure TrimEndPunctuation(var s: string);


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

Предлагается алгоритм:
- Удалять только те слова, которые находятся в скобках, где все прочие слова
 также определены как теги (не обязательно для удаления).
- Если теговое слово находится в блоке с нетеговыми словами, сообщать об этом
 (чтобы проверить, есть ли вообще такие).
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
RemoveFormatting()
Удаляет всё html-форматирование из строки, превращая её в голый текст.
}
function RemoveFormatting(s: string): string;

{
Тело статьи состоит из:
1. Общей части
2. Некоторого количества блоков:
 а. Перевод (1 строка)
 б. Любое количество вариаций и примеров.

Блоки начинаются с номера блока, находящегося на одной с ним строке:
 1) первый блок
 2) второй блок
Если блок только один, номер блока может отсутствовать, и тогда общая часть
является одновременно первой строкой единственного блока.

Как в самом переводе, так и в вариациях может использоваться знак ~, что
означает "подставить слово":
  おおさわぎ【大騒ぎ】(о:саваги)〔1-748-2-50〕
  [большой] шум, переполох, суматоха;
  ～する поднимать [большой] шум, устраивать переполох (суматоху);
Поскольку слов одна статья покрывает несколько, копии такого перевода должны
быть созданы для каждой словарной статьи.

Среди строк попадаются комментарии. Их надо отбрасывать:
  • Правка. Было: depot.
  • Также デポー.
  • Также デポ.
  • Уточнённый перевод: депо в значении склад, место или помещение для хранения чего-либо.

В разных блоках на первой позиции в начале встречается стартовое двоеточие
  でっぷり(дэппури)〔1-230-2-20〕
  : ～した <i>прост.</i> дородный, толстый, жирный;
Чем статья с ни отличается от статьи без него - пока неясно. Считаем, что его
можно отбросить.

Иногда общая строка - это единственное двоеточие. ":"

Как устроена общая строка - до конца не понятно. Бывает вот так:
  デリケート(дэрикэ:то)〔1-230-2-24〕
  (<i>англ.</i> delicate): ～[な]
  1) деликатный, щекотливый <i>(о вопросе и т. п.)</i>;
  デリケートな事柄 деликатный вопрос;
  話がデリケートになって来た разговор перешёл на щекотливую тему;
  2) тонкий, изящный.

Из этого, видимо, надо сделать:
  デリケートな (<i>англ.</i> delicate) деликатный, щекотливый <i>(о вопросе и т. п.)</i>;
  デリケートな (<i>англ.</i> delicate) тонкий, изящный.

Однако каны для "な", как мы видим, в статье нет. Таким образом, это невозможно.
В данном случае な можно отбросить, но как в других случаях?
Каны нет нигде!

Таким образом, нужен преобразователь каны в киридзи.
Его мы стрельнем из вакана.
}

const
  MaxBlock = 22; //пока в словаре встречалось максимум 20

type
  TEntryBlock = record
  end;
  PEntryBlock = ^TEntryBlock;

  TEntryBody = record
    common: string; //если блок только один и "влитой" (без номера), эта строка всегда пустая
    blocks: array[0..MaxBlock-1] of TEntryBlock;
    block_cnt: integer;
//    procedure Reset;
  end;


implementation
uses StrUtils, UniStrUtils;

type
  TBasicEntryHeader = record
    ref: string;
    readings: array[0..MaxWords-1] of string;
    reading_cnt: integer;
    kiriji: array[0..MaxWords-1] of string;
    kiriji_cnt: integer;
    flags: string;
    kanji: array[0..MaxWords-1] of record
      variants: array[0..MaxKanjiVariants-1] of string;
      variant_cnt: integer;
    end;
    kanji_cnt: integer;
    procedure Reset;
    function AddReading: PString;
    function AddKiriji: PString;
    function AddKanji: integer;
    function AddKanjiVariant(kanji_id: integer): PString;
  end;

procedure TBasicEntryHeader.Reset;
begin
  ref := '';
  flags := '';
  reading_cnt := 0;
  kiriji_cnt := 0;
  kanji_cnt := 0;
end;

function TBasicEntryHeader.AddReading: PString;
begin
  if reading_cnt>=Length(readings) then
    raise EParsingException.Create('BasicEntryHeader: cannot add one more reading.');
  Inc(reading_cnt);
  Result := @readings[reading_cnt-1];
  Result^ := '';
end;

function TBasicEntryHeader.AddKiriji: PString;
begin
  if kiriji_cnt>=Length(kiriji) then
    raise EParsingException.Create('BasicEntryHeader: cannot add one more kiriji.');
  Inc(kiriji_cnt);
  Result := @kiriji[kiriji_cnt-1];
  Result^ := '';
end;

function TBasicEntryHeader.AddKanji: integer;
begin
  if kanji_cnt>=Length(kanji) then
    raise EParsingException.Create('BasicEntryHeader: cannot add one more kanji.');
  Inc(kanji_cnt);
  kanji[kanji_cnt-1].variant_cnt := 0;
  Result := kanji_cnt-1;
end;

function TBasicEntryHeader.AddKanjiVariant(kanji_id: integer): PString;
begin
  if kanji[kanji_id].variant_cnt>=Length(kanji[kanji_id].variants) then
    raise EParsingException.Create('BasicEntryHeader: cannot add one more kanji variant.');
  Inc(kanji[kanji_id].variant_cnt);
  with kanji[kanji_id] do
    Result := @variants[variant_cnt-1];
  Result^ := '';
end;

{
Раскодирует строку, просто разбирая её структуру - не пытаясь проверить, что строка правильная
}
procedure DecodeBasicEntryHeader(s: string; out bh: TBasicEntryHeader);
const //parsing state
  EH_READING = 0;
  EH_KANJI = 1;
  EH_KIRIJI = 2;
  EH_FLAGS = 3;
  EH_REF = 4;
var
  state: integer;
  pc: PChar;
  state_over: boolean; //if set, we're in state transition (such as "】(") and normal symbols are not expected

  curreading: PString;
  curkiriji: PString;
  curkanjiid: integer;
  curkanjivariant: PString;

  procedure SkipSpaces();
  begin
    Inc(pc);
    while pc^=' ' do Inc(pc);
    Dec(pc);
  end;

  procedure StartKanji;
  begin
    state := EH_KANJI;
    state_over := false;
    curkanjiid := bh.AddKanji;
    curkanjivariant := bh.AddKanjiVariant(curkanjiid);
  end;

  procedure StartKiriji;
  begin
    state := EH_KIRIJI;
    state_over := false;
    curkiriji := bh.AddKiriji;
  end;

  procedure StartFlags;
  begin
    state := EH_FLAGS;
    state_over := false;
  end;

  procedure StartRef;
  begin
    state := EH_REF;
    state_over := false;
  end;

begin
  state := EH_READING;
  state_over := false;
  bh.Reset;
  curreading := bh.AddReading();

  pc := PWideChar(s);
  while pc^<>#00 do begin
   //Пока читаем "чтение", составляем таблицу слов (определяем words_used)
    if (state=EH_READING) and (pc^=',') then begin
      SkipSpaces;
      curreading := bh.AddReading;
    end else
    if (state=EH_READING) and (pc^='【') then StartKanji else
    if (state=EH_READING) and (pc^='(') then StartKiriji else
    if (state=EH_READING) and (pc^='[') then StartFlags else
    if (state=EH_READING) and (pc^='〔') then StartRef else

   //Дальшей каждой компоненты должно быть по words_used версий
    if (state=EH_KANJI) and (pc^='･') then begin
      curkanjivariant := bh.AddKanjiVariant(curkanjiid);
    end else
    if (state=EH_KANJI) and (pc^=',') then begin
      SkipSpaces;
      curkanjiid := bh.AddKanji;
      curkanjivariant := bh.AddKanjiVariant(curkanjiid);
    end else
    if (state=EH_KANJI) and (pc^='】') then state_over := true else
    if (state=EH_KANJI) and state_over and (pc^='(') then StartKiriji else
    if (state=EH_KANJI) and state_over and (pc^='[') then StartFlags else
    if (state=EH_KANJI) and state_over and (pc^='〔') then StartRef else
    if (state=EH_KIRIJI) and (pc^=',') then curkiriji := bh.AddKiriji else
    if (state=EH_KIRIJI) and (pc^=')') then state_over := true else
    if (state=EH_KIRIJI) and state_over and (pc^='[') then StartFlags else
    if (state=EH_KIRIJI) and state_over and (pc^='〔') then StartRef else
    if (state=EH_FLAGS) and (pc^=']') then state_over := true else
    if (state=EH_FLAGS) and state_over and (pc^='〔') then StartRef else
    if (state=EH_REF) and (pc^='〕') then state_over := true else
    if state_over=true then begin
     //Лишние символы между скобками
     //Можно быть добрыми и просто пропускать их, но давайте контролировать ошибки
      if pc^<>' ' then
        raise EParsingException.Create('Unexpected characters in state_over.');
    end else
    if (state=EH_READING) then curreading^ := curreading^ + pc^ else
    if (state=EH_KANJI) then curkanjivariant^ := curkanjivariant^ + pc^ else
    if (state=EH_KIRIJI) then curkiriji^ := curkiriji^ + pc^ else
    if (state=EH_FLAGS) then bh.flags := bh.flags + pc^ else
    if (state=EH_REF) then bh.ref := bh.ref + pc^ else
      raise EParsingException.Create('DecodeEntryHeader(): Invalid parsing state.'); //this should not happen no matter the input

    Inc(pc);
  end;

 //Контролируем ошибки
  if (state<>EH_READING) and (state_over<>true) then
    raise EParsingException.Create('Unclosed part, state='+IntToStr(state)+', state_over='+BoolToStr(state_over, true));
end;

procedure DecodeEntryHeader(s: string; out hdr: TEntryHeader);
var bh: TBasicEntryHeader;
  i, j: integer;
begin
  DecodeBasicEntryHeader(s, bh);

 { Пробуем устранить всякие простые ошибки }

 //Одно киридзи на несколько чтений - дублируем его каждому чтению
  if (bh.reading_cnt > 1) and (bh.kiriji_cnt = 1) then begin
    bh.kiriji_cnt := bh.reading_cnt;
    for i := 1 to bh.kiriji_cnt - 1 do
      bh.kiriji[i] := bh.kiriji[0];
  end;

 //Кандзи присутствует только в одной форме - дублируем его каждому чтению
  if (bh.reading_cnt > 1) and (bh.kanji_cnt = 1) then begin
    bh.kanji_cnt := bh.reading_cnt;
    for i := 1 to bh.kanji_cnt - 1 do
      bh.kanji[i] := bh.kanji[0];
  end;

 {$IFDEF FIX_UNSUPPORTED}
  if (bh.kiriji_cnt>1) and (bh.reading_cnt = 1) then
    bh.kiriji_cnt := 1;
 {$ENDIF}

 //Проверяем, что все поля совпадают
  if (bh.kiriji_cnt <> bh.reading_cnt) and (bh.kiriji_cnt <> 0) then
    raise EParsingException.Create('Kiriji count not equal to reading count.');
  if (bh.kanji_cnt <> bh.reading_cnt) and (bh.kanji_cnt <> 0) then
    raise EParsingException.Create('Kanji count not equal to reading count.');
  if bh.ref='' then
    raise EParsingException.Create('Reference missing');

 //Иначе всё совпадает - перекодируем
  hdr.s_ref := bh.ref;
  hdr.words_used := bh.reading_cnt;
  for i := 0 to hdr.words_used - 1 do begin
    hdr.words[i].s_reading := bh.readings[i];
    if bh.kiriji_cnt<>0 then
      hdr.words[i].s_kiriji := bh.kiriji[i]
    else
      hdr.words[i].s_kiriji := '';
    if bh.kanji_cnt<>0 then begin
      hdr.words[i].s_kanji_used := bh.kanji[i].variant_cnt;
      for j := 0 to hdr.words[i].s_kanji_used - 1 do
        hdr.words[i].s_kanji[j] := bh.kanji[i].variants[j];
    end else
      hdr.words[i].s_kanji_used := 0;
  end;
end;

procedure DropVariantIndicator(var s: string);
var ch: Char;
  i: integer;
begin
  i := Length(s);
  if i<=0 then exit;
  ch := s[i];
  while (ch='I') or (ch='V') or (ch='X') or (ch=' ') do begin
    Dec(i);
    if i<=0 then break;
    ch := s[i];
  end;
  delete(s, i+1, Length(s)-i);
end;


function PopTranslationNumber(var ln: string; out num: string): boolean;
var i: integer; //по какой символ включительно удалять
begin
  i := 0;
  if CharIsNumber(ln[1]) then
    if ln[2]=')' then begin
      i := 2;
    end else
    if CharIsNumber(ln[2]) then
      if ln[3]=')' then begin
        i := 3;
      end;
  if i<=0 then begin
    Result := false;
    exit;
  end;

  num := copy(ln, 1, i-1);

 //Убираем пробелы, которые следуют за скобкой
  Inc(i);
  while (i<=Length(ln)) and (ln[i]=' ') do
    Inc(i);
  Dec(i);

  delete(ln, 1, i);
  Result := true;
end;

{ Удаляет пунктуацию в конце фразы - точки, точки с запятой, запятые, пробелы.
 Вопросительные и восклицательные знаки, а также троеточие не трогает }
procedure TrimEndPunctuation(var s: string);
var ch: Char;
  i: integer; //last symbol to keep
begin
  i := Length(s);
  if i<=0 then exit;
  ch := s[i];
  while (ch='.') or (ch=';') or (ch=',') or (ch=' ') do begin
    if (i>1) and (ch='.') and (s[i-1]='.') then break; //троеточие не трогаем
    Dec(i);
    if i<=0 then break;
    ch := s[i];
  end;
  delete(s, i+1, Length(s)-i);
end;

{ Finds first occurence of <s_op...s_ed> block in the string, deletes it,
 and returns its contents. }
function RemoveBlock(var s: string; const s_op, s_ed: string; i_beg: integer; out contents: string): boolean;
var i_op, i_ed, i_len: integer;
  had_spaces: boolean;
begin
  i_op := posex(s_op, s, i_beg);
  if i_op<=0 then begin
    Result := false;
    exit;
  end;

  i_ed := posex(s_ed, s, i_op);
  if i_ed<=0 then begin
    Result := false;
    exit;
  end;

  i_len := i_ed-(i_op+Length(s_op));
  contents := copy(s, i_op+Length(s_op), i_len);

 //Удаляем пробелы по обе стороны от вырезанного куска, чтобы не получалось длинных дыр
  had_spaces := false;
  i_ed := i_ed + Length(s_ed) - 1;
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
  Result := true;
end;


function RemoveFormatting(s: string): string;
var tag: string;
begin
  Result := s;
  while RemoveBlock(Result, '<', '>', 1, tag) do begin end;
end;

end.
