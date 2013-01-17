unit Warodai;

interface
uses SysUtils, UniStrUtils, StreamUtils;
{$INCLUDE 'Warodai.inc'}

var
 //Общая статистика для всех Warodai-модулей
  WarodaiStats: record
    LinesRead: integer;
    LinesTooShort: integer; //article lines shorter than 2 characters --- skipped
    Comments: integer;
    DataLines: integer; //translations, variations and examples

    GroupCommon: integer; //число общих для групп частей
    BlockCommon: integer; //число общих для блоков частей
    Multitemplates: integer; //число шаблонов, которые содержат несколько вариантов на одной строке

   //Всё последующее - это "# статей, которые..."
    GroupNumberFixed: integer; //первый блок смешался с переводом, но мы починили
    GroupNumberGuessed: integer; //номер первого блока вообще потерян
    GroupNumberMissing: integer; //номер какого-то блока потерян, но не первого
    BlockNumberFixed: integer;
    BlockNumberGuessed: integer;
    BlockNumberMissing: integer;

  end;

type
  EParsingException = class(Exception);
   //Explicit common group blocks: явные блоки в общей группе, затем появление явной группы
  ESilentParsingException = class(EParsingException);

 { Некоторые типы ошибок можно делать Silent, если мы в принципе такое не поддерживаем }

  EOpenTemplate = class(ESilentParsingException); { В статье встречается открытый шаблон - не поддерживаем }
    //статья с открытыми шаблонами (см. шаблоны)
  EMultilineCommon = class(ESilentParsingException); { Общий кусок больше одной строки в длину }
    //Multiline Group Common: несколько строк в общей части 1+ групп
    //Multiline Block Common: несколько строк в общей части 1+ блоков
  EColonAfterTl = class(ESilentParsingException); { Перевод завершается двоеточием - это неполный перевод }
    //translation lines which end with colon ':'. Suspicious.
  EAlternativeIds = class(ESilentParsingException); { Идентификаторы третьего уровня - а) б) в) }
    //статья с 3-м уровнем разбиения ("а)", "б)", и т.п)
  EKanjiKanaLeft = class(ESilentParsingException); { В статье остались кандзи или кана после извлечения шаблона. Обычно это ссылки, но возможно всякое }
    //Kanji or kana left in string after doing ExtractTemplate
    //Kanji or kana left in string after doing ExtractExample
  EInsideTemplate = class(ESilentParsingException); { Шаблоны внутри строки (не в начале) }
  ESeveralProperTranslations = class(ESilentParsingException); { Обычно все добавочные строки - это "ср." и "см. также" }
    //статей, где блок содержит несколько строк простого перевода

type
  TWarodaiReader = class(TCharReader)
  public
    function ReadLine(out ln: string): boolean; reintroduce;
    procedure SkipArticle;
  end;

const
  EV_NORMAL = 0;
  EV_KANA = 1;
  EV_KANJI = 2;

{ Возвращает KANJI, если в строке есть и кандзи, KANA, если только кана, и NORMAL, если ничего нет }
function EvalChars(const s: string): integer;

implementation

function TWarodaiReader.ReadLine(out ln: string): boolean;
begin
  Result := inherited ReadLine(ln);
  if Result then
    Inc(WarodaiStats.LinesRead);
end;

//Проматываем текущую запись до конца
procedure TWarodaiReader.SkipArticle;
var s: string;
begin
  while ReadLine(s) and (s<>'') do begin end;
end;

function EvalChars(const s: string): integer;
var i: integer;
begin
  Result := EV_NORMAL;
  for i := 1 to Length(s) do
    if IsKanji(s[i]) then begin
      Result := EV_KANJI;
      break;
    end else
    if IsKana(s[i]) then
      Result := EV_KANA;
end;



initialization
  FillChar(WarodaiStats, sizeof(WarodaiStats), 0);

end.
