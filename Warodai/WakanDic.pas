unit WakanDic;
{
Прослойка между этой программой и модулями Вакана, которые тут используются
для поиска по едикту.
}

interface
uses TextTable, JWBDic, JWBEdictMarkers, JWBKanaConv, Warodai, WarodaiHeader,
  WarodaiBody, EdictWriter;

var
  refStats: record
    TagsFound: integer;  //нашли какие-то вхождения
    SeveralSenses: integer; //слово вроде и нашли, но у него в вародае несколько значений - пришлось пропустить
    MultipleMatches: integer; //подошло несколько противоречивых вариантов
    MultipleMatchSenses: integer; //в найденном варианте несколько противоречивых значений
    MultiKanaKanji: integer; //вариант-то подошёл, но там было несколько вариантов каны-кандзи, мы запомнили
    TagsApplied: integer; //всё подошло - записали флаги
  end;

procedure LoadReferenceDic(filename: string);
procedure FreeReferenceDic;

{
Находит достоверный результат по кане+кандзи или просто кане, или возвращает false.
}

type
  TSearchResult = record
    idx: integer;
    k_markers: TMarkers;
    article: integer;
    entries: TEntries;
  end;

function RefFind(kana: string; kanji: string; out ret: TSearchResult): boolean;

{
Заполняет набор маркеров для каждого из слов в TEntryHeader.
Итоговых наборов маркеров ровно столько, сколько было в TEntryHeader чтений -
некоторые могут быть пустыми, если маркеры для этого слова были не найдены.

Если у одного чтения несколько записей, маркеры берутся для первой найденной
в словаре пары чтение-запись.

Если у чтения нет записи, попытка найти маркеры всё равно будет предпринята,
но шанс на успех меньше (сложнее убедиться, что мы точно нашли нужную статью).
}

type
  TEntryWordMarkers = record
    found: boolean;
    markers: string; //без скобок
    pop: boolean; //макер (P) пишется в конце статьи особым образом
  end;
  TEntryMarkers = array[0..MaxWords-1] of TEntryWordMarkers;

procedure FillMarkers(art: PEdictArticle); overload;


implementation
uses SysUtils;

var
  edict: TJaletDic;
  roma_t: TRomajiTranslator;
  cdic: TDicLookupCursor;

procedure LoadReferenceDic(filename: string);
begin
  edict:=TJaletDic.Create;
  edict.Offline := false;
  edict.LoadOnDemand := false;
  edict.FillInfo(filename);
  edict.Load;
  cdic := edict.NewLookup(mtExactMatch);
  roma_t := TKanaTranslator.Create;
  roma_t.LoadFromFile('c_romaji_base.kcs');
end;

procedure FreeReferenceDic;
begin
  FreeAndNil(roma_t);
  FreeAndNil(cdic);
  FreeAndNil(edict);
end;

{
Находит первый достоверно подходящий результат и возвращает его либо false.
kana должна быть в ромадзи.
Достоверные результаты:
1. Если kanji='', то в словаре тоже должно быть kanji='' и кана должна совпадать
 в точности, и других записей с такой каной быть не должно.
2. Иначе kanji должно совпадать в точности. Из таких вариантов выбирается
 тот, где в точности совпадает и кана, а если такого нет - первый.
}
function RefFind(kana: string; kanji: string; out ret: TSearchResult): boolean;
var rcnt: integer;
  rkana: string;
begin
  if edict=nil then begin
    Result := false;
    exit;
  end;

  kana := roma_t.KanaToRomaji(kana, [rfConvertLatin,rfConvertPunctuation]);

  if kanji<>'' then
    cdic.LookupKanji(kanji)
  else
    cdic.LookupRomaji(kana);

  rcnt := 0;
  while cdic.HaveMatch do begin
    Inc(rcnt);
    if kanji='' then begin
      if (rcnt>1) then begin
       //Кандзи нет и больше одного результата => не можем ничего сказать точно
        Result := false;
        exit;
      end;
      ret.idx := cdic.GetIndex;
      ret.k_markers := cdic.GetKanjiKanaMarkers;
      ret.article := cdic.GetArticle;
      ret.entries := cdic.GetEntries;
      cdic.NextMatch;
      continue;
    end;

   //Иначе кандзи <> ''
   //Если нашли тот, где точно совпадает кана, то читаем его и выходим.
   //Также читаем перевод, если мы ещё никакого пока не видели - первый найденный.
    rkana := cdic.GetPhonetic;
    if (rkana=kana) or (rcnt<=1) then begin
      ret.idx := cdic.GetIndex;
      ret.k_markers := cdic.GetKanjiKanaMarkers;
      ret.article := cdic.GetArticle;
      ret.entries := cdic.GetEntries;
      if rkana=kana then break;
    end;

    cdic.NextMatch;
  end;
  Result := rcnt>0;
end;

procedure FillMarkers(art: PEdictArticle);
var i: integer;
  res: TSearchResult;
  match_article: integer;
  match_entries: TEntries;
  multiple_matches: boolean;
  mark_b: TMarkers;
  mark: TMarkersByType;

begin
  match_article := -1;
  multiple_matches := false;

 //Перебираем все поля кана-кандзи, и находим для каждого флаги - в любом случае

  if art.kanji_used<=0 then begin
    for i := 0 to art.kana_used - 1 do
      if RefFind(art.kana[i].k, '', res) then begin
        art.kana[i].inf := MarkersToStr(res.k_markers, art.kana[i].pop);
        if (match_article>=0) and (match_article <> res.article) then
          multiple_matches := true;
        match_article := res.article;
        match_entries := res.entries;
      end;

  end else
  for i := 0 to art.kanji_used - 1 do
    if RefFind('', art.kanji[i].k, res) then begin
      art.kanji[i].inf := MarkersToStr(res.k_markers, art.kanji[i].pop);
      if (match_article>=0) and (match_article <> res.article) then
        multiple_matches := true;
      match_article := res.article;
      match_entries := res.entries;
    end;

  if match_article<0 then
    exit; //ничего не найдено.

  Inc(refStats.TagsFound);

  if multiple_matches then begin
    Inc(refStats.MultipleMatches);
    exit;
  end;

  mark_b := match_entries.items[0].markers;
  for i := 1 to Length(match_entries.items) - 1 do
    if match_entries.items[i].markers<>mark_b then begin
      Inc(refStats.MultipleMatchSenses);
      exit;
    end;

  if art.senses_used>1 then begin
    Inc(refStats.SeveralSenses);
    exit;
  end;

  if (art.kanji_used>1) or (art.kanji_used>1) then
    Inc(refStats.MultiKanaKanji);

  mark := MarkersToStrEx(mark_b);

  art.senses[0].t_pos := mark.m_pos;
  art.senses[0].t_field := mark.m_field;
  art.senses[0].t_dial := mark.m_dial;
  art.senses[0].t_misc := mark.m_misc;
  Inc(refStats.TagsApplied);
end;

initialization
  FillChar(refStats, sizeof(refStats), 0);

end.
