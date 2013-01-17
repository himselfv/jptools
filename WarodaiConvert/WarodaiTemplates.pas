unit WarodaiTemplates;
{
Обработка шаблонов и примеров.
Шаблоны:
  ～にする мариновать в мисо;
Примеры:
  味の抜けた безвкусный;
}

interface
uses Warodai;
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

{
Некоторые правила содержат несколько шаблонов сразу:
  ～たる, ～とした <i>см.</i> <a href="#1-046-1-45">ばくばく</a>.
}

function HasMultipleTemplates(const t: string): boolean;

type
  TTemplateList = array of string;

{ Разбивает список шаблонов на шаблоны }
procedure SplitTemplate(const t: string; out t_p: TTemplateList);

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

{ True, если указанный символ не разрывает шаблона. }
function IsTemplateChar(const ch: char): boolean;
begin
  Result := IsKana(ch) or IsKanji(ch)
    or IsCJKSymbolOrPunctuation(ch)
    or (ch='～') //сама подстановка
    or (ch='[') or (ch=']') //необязательные части шаблона
    or (ch='…'); //обозначает "какое-нибудь слово"
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
  while IsTemplateChar(ln[i]) or (ln[i]=' ') or (ln[i]=',') do
    Inc(i);
  Result := (ln[i]=#00);
end;

function ExtractTemplate(var ln: string; out t: string): boolean;
var i_start, i_end: integer;
begin
  i_start := FindTemplate(ln);
  if i_start<=0 then begin
    Result := false;
    exit;
  end;
  i_end := i_start;
  Dec(i_start);
  Inc(i_end);

 //Находим начало и конец шаблонов (их может быть несколько, через запятую)
  while IsTemplateChar(ln[i_end]) or (ln[i_end]=' ') or (ln[i_end]=',') do
    Inc(i_end);
  Dec(i_end);
  while (i_start>0) and IsTemplateChar(ln[i_start]) do
    Dec(i_start);
  Inc(i_start);

 //Пробелы в конце отматываем назад
  while (i_end>0) and (ln[i_end]=' ') do
    Dec(i_end);

  t := copy(ln, i_start, i_end);

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

  Inc(i_end);
  while ln[i_end]=' ' do
    Inc(i_end);
  Dec(i_end);

  delete(ln, i_start, i_end);

  if EvalChars(ln)<>EV_NORMAL then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after doing ExtractTemplate');

  Result := true;
end;

function HasMultipleTemplates(const t: string): boolean;
begin
  Result := pos(',',t)>0;
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


{
Примеры.
1. Примеры могут содержать японскую пунктуацию:
  あっ、鍵を忘れた ах, я забыл ключ!;

2. Примеры могут содержать подсказки по чтению:
  あっという間/マ/に в мгновение ока; не успел и ахнуть, как…

3. Примеров может быть несколько с одним переводом
  ああいった, ああした такой;
}

{ True, если указанный символ не разрывает шаблона. }
function IsExampleChar(const ch: char): boolean;
begin
  Result := IsKana(ch) or IsKanji(ch)
    or IsCJKSymbolOrPunctuation(ch)
    or (ch='/') //с помощью этих символов пишется чтение
    or (ch='[') or (ch=']') //необязательные части шаблона
    or (ch='(') or (ch=')') //альтернативный вариант записи
    or (ch='…') //обозначает "какое-нибудь слово"
    or (ch='◇'); //иногда встречается, вырезаем
end;

function ExtractExample(var ln: string; out expr: string): boolean;
var i: integer;
begin
  i := 1;
  while IsExampleChar(ln[i]) or (ln[i]=',') or (ln[i]=' ') do
    Inc(i);
  Dec(i);

  while (i>0) and (ln[i]=' ') do
    Dec(i);
  if i = 0 then begin
    Result := false;
    exit;
  end;

  expr := Trim(copy(ln, 1, i));

 { В начале примеров иногда встречаются мусорные ромбики }
  if (Length(expr)>0) and (expr[1]='◇') then
    delete(expr,1,1);

  Inc(i);
  while ln[i]=' ' do
    Inc(i);
  Dec(i);

  delete(ln, 1, i);

  if EvalChars(ln)<>EV_NORMAL then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after doing ExtractExample');
  Result := true;
end;

end.
