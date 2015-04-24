unit YarxiCore;

interface
uses YarxiStrings, JWBKanaConv;

{
 Данные хранятся в базе Яркси в разновидности кодировки Хэпбёрна. Отделение n
 стандартное: "sen'i". Долгота ou передаётся двоеточием: "so:zo:". Удвоенная
 гласная так и записывается: "tooi", "saabisu". В словах, помеченных как катакана,
 Яркси в этом месте вставит тире: "sa-bisu".
 Для преобразования каны/кандзи требуется таблица Hepburn-Yarxi.roma, которую
 загружает в KanaTran центральный модуль.

 В транскрипции бывают пробелы, кроме того, декодером вводятся дополнительные
 символы для обозначения помет, которые в Яркси хранятся иным образом:
   -     тире в транскрипции
   ´     особая помета для сочетаний ii и ha
   []    опциональная часть
 При переводе в кану и обратно эти символы должны сохраняться. Для их удаления
 в последний момент существуют функции clearMarks и clearKana.
}

var
  YarxiSilent: boolean = false;
  KanaTran: TRomajiTranslator;

//Короткие ссылки
function RomajiToKana(const s: string): string; inline;
function KanaToRomaji(const s: string): string; inline;
{ Преобразование в киридзи мы сейчас не поддерживаем, поскольку для разбора оно
 не нужно, но однажды его можно сделать.
 Просто нужно разделить таблицу на отдельные: yarxi-romaji и yarxi-kiriji. }


{
 Как переводить Яркси-ромадзи в:
   -> ромадзи: enhanceRomaji(s)
   -> печатная кана: clearMarks(RomajiToKana(s))
   -> чистая кана (для поиска или экспорта): clearAll(s)
   -> киридзи: enhanceKiriji(KanaToKiriji(RomajiToKana(s)))
}

function clearMarks(const s: string): string;
function clearAll(const s: string): string;
function enhanceRomaji(const s: string): string;
function enhanceKiriji(const s: string): string;


{ Во всех полях используется "обезъяний русский":
    <a   =>   a
    <b   =>   б
    <c   =>   в

Обычно всё пишется маленькими, а заглавная буква первой делается автоматически.
Но если записано имя собственное, то заглавная прописывается явно.
Заглавные получаются из обычных так: <c -> <C. Для специальных букв заглавные
смотри ниже по табличке.
Цифры не кодируются.
}

function DecodeRussian(const inp: string): string;


{ Функции посылают сюда жалобы на жизнь. }

var
  ComplainContext: string;
  Complaints: integer;
 //добавляется ко всем жалобам. Внешние функции могут записывать сюда
 //номер и/или содержание текущей записи

procedure PushComplainContext(const AData: string);
procedure PopComplainContext;
procedure Complain(const msg: string); inline; overload;
procedure Complain(const msg, data: string); inline; overload;


implementation
uses SysUtils, StrUtils, WcExceptions;

function RomajiToKana(const s: string): string;
begin
  Result := KanaTran.RomajiToKana(s, []);
end;

function KanaToRomaji(const s: string): string;
begin
  Result := KanaTran.KanaToRomaji(s, []);
end;

{ Удаляет из ромадзи, каны или киридзи пометы, в том числе уже имевшиеся в базе
 (пробелы) и внесённые туда декодером (тире, штрих).
 Оставляет пометы, сохраняющиеся в печатной кане (кв. скобки). }
function clearMarks(const s: string): string;
begin
  Result := killc(s, ' -´');
end;

{ Удаляет из ромадзи, каны или киридзи все пометы. }
function clearAll(const s: string): string;
begin
  Result := killc(s, ' -´[]');
end;

{ Заменяет слоги с пометами на их печатные альтернативы (i´->i; ha´->wa) }
function enhanceRomaji(const s: string): string;
begin
  Result := killc(s,'`'); //непродуктивные пометы
  Result := repl(Result, 'ha´', 'wa');
end;

{ То же для киридзи }
function enhanceKiriji(const s: string): string;
begin
  Result := s;
  Result := repl(Result, 'ий´', 'ии');
  Result := repl(Result, 'ха´', 'ва');
end;


{ Заменяет весь обезъяний русский в тексте нормальными русскими буквами. }
function DecodeRussian(const inp: string): string;
const
  eng: string = 'abcdefghijklmnopqrstuvwxyz1234567ABCDEFGHIJKLMNOPQRSTUVWXYZ890!?=+';
  rus: string = 'абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
var pc, po: PChar;
  i: integer;
  found: boolean;
begin
  if inp='' then begin
    Result := '';
    exit;
  end;

  Result := '';
  SetLength(Result, Length(inp)); //not going to be bigger than that

  pc := PChar(@inp[1]);
  po := PChar(@Result[1]);
  while pc^<>#00 do begin
    if pc^='<' then begin
      Inc(pc);
      found := false;
      for i:= 1 to Length(eng) do
        if eng[i]=pc^ then begin
          po^ := rus[i];
          found := true;
          break;
        end;
      if not found then begin
        po^ := '<';
        Dec(pc);
      end;
    end else
      po^ := pc^;
    Inc(pc);
    Inc(po);
  end;
  po^ := #00;
  SetLength(Result, StrLen(PChar(Result))); //trim
end;


{ Сборщик жалоб }

procedure PushComplainContext(const AData: string);
begin
  if ComplainContext<>'' then
    ComplainContext:=ComplainContext+#09+AData
  else
    ComplainContext:=AData;
end;

procedure PopComplainContext;
var i, j: integer;
begin
  i := 0;
  repeat
    j := i;
    i := pos(#09,ComplainContext,j+1);
  until i<=0;
  if j=0 then //nothing found
    ComplainContext := ''
  else
    ComplainContext := Copy(ComplainContext,1,j-1);
end;

procedure Complain(const msg: string);
begin
  Inc(Complaints);
  if YarxiSilent then exit;
  if ComplainContext<>'' then
    Warning(#13#10'  '+repl(ComplainContext,#09,#13#10'  ')+#13#10'  '+msg)
  else
    Warning(msg);
end;

procedure Complain(const msg, data: string);
begin
  Inc(Complaints);
  if YarxiSilent then exit;
  if ComplainContext<>'' then
    Warning(#13#10'  '+repl(ComplainContext,#09,#13#10'  ')+#13#10'  '+data)
  else
    Warning(msg+#13#10'  '+data);
end;


initialization
  KanaTran := TKanaTranslator.Create;

finalization
  FreeAndNil(KanaTran);

end.
