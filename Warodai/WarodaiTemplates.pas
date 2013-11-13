﻿unit WarodaiTemplates;
{
Обработка шаблонов и примеров.
Шаблоны:
  ～にする мариновать в мисо;
Примеры:
  味の抜けた безвкусный;
}

interface
uses Warodai, WcUtils;
{$INCLUDE 'Warodai.inc'}

{ Возвращает номер символа подстановки в шаблоне или 0 }
function FindTemplate(const ln: string): integer;

{
Некоторые шаблоны содержат перевод на этой же строчке - это локальные шаблоны:
  ～する поднимать [большой] шум
  ～にする мариновать в мисо;
Другие содержат только шаблон, например:
  ～にする

Такие шаблоны потенциально значат, что дальнейшие записи переводов относятся
уже не к главное статье, а к модифицированной шаблоном.
}
function IsOpenTemplate(const ln: string): boolean;

{ Находит в строке шаблон или список шаблонов, удаляет его и возвращает отдельно.
 Заодно проверяет, что строка - действительно локальная строка шаблона в правильной форме.
 False => в строке нет шаблона }
function ExtractTemplate(var ln: string; out t: string): boolean;

{ Преобразует слово согласно шаблону. Подходит и для каны, и для кандзи.
 Пустые шаблоны тоже поддерживаются. }
function ApplyTemplate(const templ: string; const word: string): string;

{
Некоторые правила содержат несколько шаблонов сразу:
  ～たる, ～とした <i>см.</i> <a href="#1-046-1-45">ばくばく</a>.
Предпочтительнее для разных (грам.) слов иметь разные статьи, поэтому мы считаем это
за два раздельных шаблона.
}

type
  TTemplateList = array of string;

procedure SplitTemplate(const t: string; out t_p: TTemplateList);

{
Правила могут содержать опциональные блоки:
  ～[の] абстрактный;
Или альтернативные блоки:
  ～とした(とし)
Поэтому из одного шаблона мы генерируем список его фиксированных вариантов.
Для шаблона будет создана отдельная статья, в которой будут указаны все
получившиеся комбинации всех вариантов кандзи и каны статьи с вариантами шаблона.

Можно было бы поступить как с запятыми, и сделать из опциональных блоков
тоже отдельные шаблоны, но похоже, что обычно варианты в квадратных скобках
ближе к одному шаблону, чем к разным.
}
type
  TTemplateVariants = TList<string>;
  PTemplateVariants = ^TTemplateVariants;

procedure GenerateTemplateVariants(const templ: string; tvars: PTemplateVariants);

{
Некоторые шаблоны не нужно применять, поскольку они означают определённое
грамматическое употребление:
  ほうそう【包装】(хо:со:)〔2-504-1-29〕
  упаковка; обёртка;
  ～する упаковывать; обёртывать.

Вместо этого в новой статье нужно выставить грамматический флаг.
Часто эту статью можно даже объединить с главной:
  ある限り [あるかぎり] /(n) (1) all (there is)/(exp,n-adv) (2) as long as there is/

Дальше идёт список таких шаблонов.
}
type
  TTemplateMarkerMapping = record
    t: string; //template
    m: string; //markers, через запятую
  end;
  PTemplateMarkerMapping = ^TTemplateMarkerMapping;

const
  TEMPL_MARK: array[0..7] of TTemplateMarkerMapping = (
   (t: '～する'; m: 'vs'),
   (t: '～した'; m: ''),
   (t: '～の'; m: ''),
   (t: '～なる'; m: ''),
   (t: ''; m: ''),
   (t: ''; m: ''),
   (t: ''; m: ''),
   (t: ''; m: '')
  );


{
Разбирает строку в форме примера:
  埃を浴びる быть покрытым пылью, быть в пыли;
  喝采を浴びて под гром аплодисментов;
Вынимает из неё сам пример и кладёт в expr. Оставляет в строке перевод.
Возвращает false, если пример не был найден.
Выполняет некоторые проверки на целостность этого вхождения.
}
function ExtractExample(var ln: string; out expr: string): boolean;

implementation
uses SysUtils, UniStrUtils;

function FindTemplate(const ln: string): integer;
begin
  Result := pos('～', ln);
end;

{ True, если наличие указанного символа относит слово к шаблону }
function IsTrueTemplateChar(const ch: char): boolean;
begin
  Result := IsKana(ch) or IsKanji(ch)
    or IsCJKSymbolOrPunctuation(ch)
    or IsFullWidthCharacter(ch)
    or (ch='～') //сама подстановка
end;

{ True, если указанный символ не разрывает шаблона }
function IsSupplementalTemplateChar(const ch: char): boolean;
begin
  Result :=
       (ch='[') or (ch=']') //необязательные части шаблона
    or (ch='(') or (ch=')') //альтернативные варианты шаблона
    or (ch='…')  //обозначает "какое-нибудь слово"
    or (ch=',');
end;

function IsTemplateChar(const ch: char): boolean;
begin
  Result := IsTrueTemplateChar(ch)
    or IsSupplementalTemplateChar(ch);
end;

function IsOpenTemplate(const ln: string): boolean;
var i: integer;
begin
  i := FindTemplate(ln);
  if i<=0 then begin
    Result := false;
    exit;
  end;
  Inc(i);
  while IsTemplateChar(ln[i]) or (ln[i]=' ') do
    Inc(i);
  Result := (ln[i]=#00);
end;

function ExtractTemplate(var ln: string; out t: string): boolean;
var i_start, i_end, i_tmp: integer;
  w_true: boolean;
begin
  i_start := FindTemplate(ln);
  if i_start<=0 then begin
    Result := false;
    exit;
  end;
  i_end := i_start;

 //Находим начало шаблона
 //Тут мы не будем особо аккуратными, т.к. в целом шаблоны должны идти с начала строки.
 //Поэтому можно не опасаться случайно откусить лишнего.
  Dec(i_start);
  while (i_start>0) and (IsTemplateChar(ln[i_start]) or (ln[i_start]=' ')) do
    Dec(i_start);
  Inc(i_start);

 //Находим конец шаблона. Тут нужно быть осторожными. Откусываем по словам.
  Inc(i_end);
  repeat
   //i_end указывает на первый непробельный символ слова

   //Проматываем следующее слово
    w_true := false;
    i_tmp := i_end;
    repeat
      if IsTrueTemplateChar(ln[i_tmp]) then
        w_true := true
      else
      if not IsSupplementalTemplateChar(ln[i_tmp]) then
        break;
      Inc(i_tmp);
    until false;

   //Теперь мы либо на пробеле, либо на первом символе слова, который не относится к шаблону
   //Хотя вообще говоря, шаблон со словом стыковаться не должен.
    if not w_true then //шаблонные символы не были найдены -- слово разрывает шаблон
      break;

   //Единственный символ, которому допускается прерывать нас - пробел
    if ln[i_tmp]<>' ' then begin
     //Шаблон стыкуется со словом! В очередном слове шаблонные символы найдены,
     //но потом найдены и разрывающие шаблон.
     //Сделаем-ка для порядка ошибку. Так не должно быть.
      raise ETemplateParsingException.Create('Template word merged with normal word');
    end;

   //Проматываем пробелы
    i_end := i_tmp;
    while ln[i_end]=' ' do Inc(i_end);

  until false;
  Dec(i_end); //вернулись на последний пробел

 //Отматываем пробелы и копируем
  i_tmp := i_end;
  while (ln[i_tmp]=' ') do Dec(i_tmp);
  t := copy(ln, i_start, i_tmp-i_start+1);

 //Удаляем вместе с пробелами по обе стороны
  Dec(i_start);
  while (i_start>0) and (ln[i_start]=' ') do
    Dec(i_start);
  Inc(i_start);

  if i_start<>1 then begin
    raise EInsideTemplate.Create('Template is not the first thing in the line');
    Result:=false;
    exit;
  end;

 //А в i_end пробелы и так промотаны
  delete(ln, i_start, i_end-i_start+1);

 {
  Не делаем этого, т.к. кандзи и кана могут остаться в строке легальным образом -- например, в форме "см. также".
  Вместо этого мы проверим на кандзи и кану перед самым добавлением.

  if EvalChars(ln) and (EV_KANA or EV_KANJI) <> 0 then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after doing ExtractTemplate');
 }

 //Эти вещи пока не поддерживаем, так что лучше в корявом виде в словарь их не класть
  if pos('(',t)>0 then
    raise ETemplateParsingException.Create('Alternative template parts -- unsupported');
  if countc(t,'[')>1 then //поддерживаем макс. 1
    raise ETemplateParsingException.Create('Multiple optional template parts -- unsupported');
  if pos('…',t)>0 then
    raise ETemplateParsingException.Create('Partial expression templates -- unsupported');

  Result := true;
end;

procedure SplitTemplate(const t: string; out t_p: TTemplateList);
var parts: TStringArray;
  i: integer;
begin
  parts := StrSplit(PWideChar(t), ',');
  SetLength(t_p, Length(parts));
  for i := 0 to Length(parts) - 1 do
    t_p[i] := Trim(parts[i]);
end;

{ Из множественного шаблона типа "строка [опциональная часть]" делаем набор фиксированных.
 Поддерживается только одна опциональная часть. }
procedure GenerateTemplateVariants(const templ: string; tvars: PTemplateVariants);
var po, pc: integer;
begin
  tvars^.Reset;
  po := pos('[', templ);
  if po<=0 then begin
    tvars^.Add(templ);
    exit;
  end;
  pc := pos(']', templ);
  Assert(pc>po);

  tvars^.Add(copy(templ, 1, po-1)+copy(templ,pc+1,Length(templ)-pc));
  tvars^.Add(copy(templ, 1, po-1)+copy(templ,po+1,pc-po-1)+copy(templ,pc+1,Length(templ)-pc))
end;

function ApplyTemplate(const templ: string; const word: string): string;
begin
  if templ='' then
    Result := word
  else
    Result := repl(templ, '～', word);
end;


{
Примеры.
1. Примеры могут содержать японскую пунктуацию:
  あっ、鍵を忘れた ах, я забыл ключ!;

2. Примеры могут содержать подсказки по чтению:
  あっという間/マ/に в мгновение ока; не успел и ахнуть, как…

3. Примеров может быть несколько с одним переводом
  ああいった, ああした такой;
}

{ True, если наличие указанного символа относит слово к шаблону }
function IsTrueExampleChar(const ch: char): boolean;
begin
  Result := IsKana(ch) or IsKanji(ch)
    or IsCJKSymbolOrPunctuation(ch)
    or IsFullWidthCharacter(ch)
    or (ch='…') //обозначает "какое-нибудь слово"
    or (ch='◇'); //иногда встречается, вырезаем
end;

function IsSupplementalExampleChar(const ch: char): boolean;
begin
  Result :=
       (ch=',')
    or (ch='/') //с помощью этих символов пишется чтение
    or (ch='[') or (ch=']') //необязательные части шаблона
    or (ch='(') or (ch=')'); //альтернативный вариант записи
end;

{ True, если указанный символ не разрывает шаблона. }
function IsExampleChar(const ch: char): boolean;
begin
  Result := IsTrueExampleChar(ch)
    or IsSupplementalExampleChar(ch);
end;

{ Вынимает заголовок примера из строки -- ср. ExtractTemplate! }
function ExtractExample(var ln: string; out expr: string): boolean;
var i_start, i_end, i_tmp: integer;
  w_true: boolean;
begin
  i_start := 1;
  i_end := i_start-1;

 //Находим конец шаблона. Тут нужно быть осторожными. Откусываем по словам.
  Inc(i_end);
  repeat
   //i_end указывает на первый непробельный символ слова

   //Проматываем следующее слово
    w_true := false;
    i_tmp := i_end;
    repeat
      if IsTrueExampleChar(ln[i_tmp]) then
        w_true := true
      else
      if not IsSupplementalExampleChar(ln[i_tmp]) then
        break;
      Inc(i_tmp);
    until false;

   //Теперь мы либо на пробеле, либо на первом символе слова, который не относится к шаблону
   //Хотя вообще говоря, шаблон со словом стыковаться не должен.
    if not w_true then //шаблонные символы не были найдены -- слово разрывает шаблон
      break;

   //Единственный символ, которому допускается прерывать нас - пробел
    if ln[i_tmp]<>' ' then begin
     //Шаблон стыкуется со словом! В очередном слове шаблонные символы найдены,
     //но потом найдены и разрывающие шаблон.
     //Сделаем-ка для порядка ошибку. Так не должно быть.
      raise ETemplateParsingException.Create('Example word merged with normal word');
    end;

   //Проматываем пробелы
    i_end := i_tmp;
    while ln[i_end]=' ' do Inc(i_end);

  until false;
  Dec(i_end); //вернулись на последний пробел

  if i_end<=0 then begin
    Result := false;
    exit;
  end;

 //Отматываем пробелы и копируем
  i_tmp := i_end;
  while (ln[i_tmp]=' ') do Dec(i_tmp);
  expr := copy(ln, i_start, i_tmp-i_start+1);

 { В начале примеров иногда встречаются мусорные ромбики }
  if (Length(expr)>0) and (expr[1]='◇') then
    delete(expr,1,1);

 //Удаляем вместе с пробелами по обе стороны
  Dec(i_start);
  while (i_start>0) and (ln[i_start]=' ') do
    Dec(i_start);
  Inc(i_start);

  if i_start<>1 then begin
    raise EInsideTemplate.Create('Example is not the first thing in the line');
    Result:=false;
    exit;
  end;

 //А в i_end пробелы и так промотаны
  delete(ln, i_start, i_end-i_start+1);

 {
  Не делаем этого -- см. комментарий в ExtractTemplate.

  if EvalChars(ln) and (EV_KANA or EV_KANJI) <> 0 then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after doing ExtractExample');
 }

 //Примеры на наличие (скобок), [опциональных частей] и запятых не проверяем.
 //Запятые в примерах очень даже допустимы, а скобки и прочее - всё равно примеры
 //пишутся в неформальном виде. Разбираться с ними можно будет потом, по готовому файлу.
  Result := true;
end;

end.