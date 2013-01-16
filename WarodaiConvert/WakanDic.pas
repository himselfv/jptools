unit WakanDic;
{
ѕрослойка между этой программой и модул€ми ¬акана, которые тут используютс€
дл€ поиска по едикту.
}

interface
uses TextTable, JWBDic, JWBEdictMarkers, JWBKanaConv, Warodai, WarodaiHeader, WarodaiBody;

var
  refStats: record
    edictTagsFound: integer;  //number of articles we have clearly found a match for
    edictTagsCloned: integer; //number of articles we have cloned from a clear match (same source article)
    edictTagsUnsure: integer; //number of articles we have found a possible match for, but weren't sure and dropped it
  end;

procedure LoadReferenceDic(filename: string);
procedure FreeReferenceDic;

{
Ќаходит достоверный результат по кане+кандзи или просто кане, или возвращает false.
}

type
  TSearchResult = record
    idx: integer;
    markers: TMarkers;
  end;

function RefFind(kana: string; kanji: string; out ret: TSearchResult): boolean;

{
«аполн€ет набор маркеров дл€ каждого из слов в TEntryHeader.
»тоговых наборов маркеров ровно столько, сколько было в TEntryHeader чтений -
некоторые могут быть пустыми, если маркеры дл€ этого слова были не найдены.

≈сли у одного чтени€ несколько записей, маркеры берутс€ дл€ первой найденной
в словаре пары чтение-запись.

≈сли у чтени€ нет записи, попытка найти маркеры всЄ равно будет предприн€та,
но шанс на успех меньше (сложнее убедитьс€, что мы точно нашли нужную статью).
}

type
  TEntryWordMarkers = record
    found: boolean;
    markers: string; //без скобок
    pop: boolean; //макер (P) пишетс€ в конце статьи особым образом
  end;
  TEntryMarkers = array[0..MaxWords-1] of TEntryWordMarkers;

function FillWordMarkers(const word: TEntryWord; out mark: TEntryWordMarkers): boolean;
procedure FillMarkers(const hdr: TEntryHeader; out mark: TEntryMarkers);


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
  roma_t := TRomajiTranslator.Create;
  roma_t.LoadFromFile('c_romaji_base.kcs');
end;

procedure FreeReferenceDic;
begin
  FreeAndNil(roma_t);
  FreeAndNil(cdic);
  FreeAndNil(edict);
end;

{
Ќаходит первый достоверно подход€щий результат и возвращает его либо false.
kana должна быть в ромадзи.
ƒостоверные результаты:
1. ≈сли kanji='', то в словаре тоже должно быть kanji='' и кана должна совпадать
 в точности, и других записей с такой каной быть не должно.
2. »наче kanji должно совпадать в точности. »з таких вариантов выбираетс€
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

  kana := roma_t.KanaToRomaji(kana, 1);

  if kanji<>'' then
    cdic.LookupKanji(kanji)
  else
    cdic.LookupRomaji(kana);

  rcnt := 0;
  while cdic.HaveMatch do begin
    Inc(rcnt);
    if kanji='' then begin
      if (rcnt>1) then begin
       // андзи нет и больше одного результата => не можем ничего сказать точно
        Result := false;
        exit;
      end;
      ret.idx := cdic.GetIndex;
      ret.markers := cdic.GetArticleMarkers; //TODO: This is wrong. Separate markers for separate entries
      cdic.NextMatch;
      continue;
    end;

   //»наче кандзи <> ''
   //≈сли нашли тот, где точно совпадает кана, то читаем его и выходим.
   //“акже читаем перевод, если мы ещЄ никакого пока не видели - первый найденный.
    rkana := cdic.GetPhonetic;
    if (rkana=kana) or (rcnt<=1) then begin
      ret.idx := cdic.GetIndex;
      ret.markers := cdic.GetArticleMarkers; //TODO: This is wrong. See above.
      if rkana=kana then break;
    end;

    cdic.NextMatch;
  end;
  Result := rcnt>0;
end;

function FillWordMarkers(const word: TEntryWord; out mark: TEntryWordMarkers): boolean;
var i: integer;
  res: TSearchResult;
begin
  if word.s_kanji_used<=0 then begin
    Result := RefFind(word.s_reading, '', res);
    if Result then begin
      mark.found := true;
      mark.markers := MarkersToStr(res.markers, mark.pop);
      exit;
    end;
  end else
  for i := 0 to word.s_kanji_used - 1 do
    if RefFind(word.s_reading, word.s_kanji[i], res) then begin
      mark.found := true;
      mark.markers := MarkersToStr(res.markers, mark.pop);
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure FillMarkers(const hdr: TEntryHeader; out mark: TEntryMarkers);
var i, m_idx: integer;
begin
  m_idx := -1;
  for i := 0 to hdr.words_used - 1 do
    if not FillWordMarkers(hdr.words[i], mark[i]) then begin
      mark[i].found := false;
      mark[i].markers := '';
      mark[i].pop := false;
    end else begin
      Inc(refStats.edictTagsFound);
      if m_idx<0 then
        m_idx := i;
    end;

 //ƒл€ тех слов, дл€ которых маркеры не нашли, копируем те, где нашли
  if m_idx>=0 then
    for i := 0 to hdr.words_used - 1 do
      if not mark[i].found then begin
        Inc(refStats.edictTagsCloned);
        mark[i] := mark[m_idx];
      end;
end;

initialization
  FillChar(refStats, sizeof(refStats), 0);

end.
