unit WakanDic;
{
ѕрослойка между этой программой и модул€ми ¬акана, которые тут используютс€
дл€ поиска по едикту.
}

interface
uses TextTable, JWBDic, Warodai;

procedure LoadEdict(filename: string);
procedure FreeEdict;

{
Ќаходит достоверный результат по кане+кандзи или просто кане, или возвращает false.
}

type
  TEdictResult = record
    idx: integer;
    markers: string;
  end;

function EdictFind(kana: string; kanji: string; out ret: TEdictResult): boolean;

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
uses SysUtils, JWBEdictMarkers;

var
  edict: TJaletDic;
  cdic: TDicCursor;

procedure LoadEdict(filename: string);
begin
  edict:=TJaletDic.Create;
  edict.Offline := false;
  edict.LoadOnDemand := false;
  edict.FillInfo(filename);
  edict.Load;
  cdic := TDicCursor.Create(edict);
end;

procedure FreeEdict;
begin
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
function EdictFind(kana: string; kanji: string; out ret: TEdictResult): boolean;
var rcnt: integer;
  rkana: string;
begin
  if kanji<>'' then begin
    cdic.SetOrder('Kanji_Ind');
    cdic.Locate(cdic.stKanji,kanji,false);
  end else begin
    cdic.SetOrder('Phonetic_Ind');
    cdic.Locate(cdic.stSort,kana,false);
  end;

  rcnt := 0;
  while (not cdic.EOF) and(
        ((kanji='') and (cdic.Str(cdic.TDictSort)=kana))
    or  ((kanji<>'') and (cdic.Str(cdic.TDictKanji)=kanji))
  ) do begin
    Inc(rcnt);
    if kanji='' then begin
      if (rcnt>1) then begin
       // андзи нет и больше одного результата => не можем ничего сказать точно
        Result := false;
        exit;
      end;
      ret.idx := cdic.Int(cdic.TDictIndex);
      ret.markers := cdic.Str(cdic.TDictMarkers);
      continue;
    end;

   //»наче кандзи <> ''
   //≈сли нашли тот, где точно совпадает кана, то читаем его и выходим.
   //“акже читаем перевод, если мы ещЄ никакого пока не видели - первый найденный.
    rkana := cdic.Str(cdic.TDictPhonetic);
    if (rkana=kana) or (rcnt<=1) then begin
      ret.idx := cdic.Int(cdic.TDictIndex);
      ret.markers := cdic.Str(cdic.TDictMarkers);
      if rkana=kana then break;
    end;

    cdic.Next;
  end;
  Result := rcnt>0;
end;

function FillWordMarkers(const word: TEntryWord; out mark: TEntryWordMarkers): boolean;
var i: integer;
  res: TEdictResult;
begin
  if word.s_kanji_used<=0 then begin
    Result := EdictFind(word.s_reading, '', res);
    if Result then begin
      mark.found := true;
      mark.markers := MarkersToStr(res.markers, mark.pop);
      exit;
    end;
  end else
  for i := 0 to word.s_kanji_used - 1 do
    if EdictFind(word.s_reading, word.s_kanji[i], res) then begin
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
    if not FillWordMarkers(hdr.words[i], mark[i]) then
      mark[i].found := false
    else
    if m_idx<0 then
      m_idx := i;

 //ƒл€ тех слов, дл€ которых маркеры не нашли, копируем те, где нашли
  if m_idx>=0 then
    for i := 0 to hdr.words_used - 1 do
      if not mark[i].found then mark[i] := mark[m_idx];
end;

end.
