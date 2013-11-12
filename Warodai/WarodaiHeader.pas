unit WarodaiHeader;
{ Разбор заголовков статей. }

interface
uses Warodai;
{$INCLUDE 'Warodai.inc'}

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

procedure DecodeEntryHeader(s: string; hdr: PEntryHeader);

{
После некоторых чтений и некоторых записей кандзи идёт римская запись номера
варианта:
  みるI
  みるIV
  見るII
Нижеследующая функция удаляет эту часть из строки.
}
function DropVariantIndicator(const s: string): string; overload;

implementation
uses SysUtils, StrUtils, UniStrUtils;

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
  PBasicEntryHeader = ^TBasicEntryHeader;

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
procedure DecodeBasicEntryHeader(s: string; bh: PBasicEntryHeader);
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

procedure DecodeEntryHeader(s: string; hdr: PEntryHeader);
var bh: TBasicEntryHeader;
  i, j: integer;
begin
  DecodeBasicEntryHeader(s, @bh);

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

procedure _DropVariantIndicator(var s: string);
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

function DropVariantIndicator(const s: string): string; overload;
begin
  Result := s;
  _DropVariantIndicator(Result);
end;

end.
