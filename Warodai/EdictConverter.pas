﻿unit EdictConverter;

interface
uses Warodai, WarodaiHeader, WarodaiBody, WarodaiTemplates, EdictWriter,
  FastArray, PerlRegEx;

{
Собираем несколько версий статьи, по числу разных шаблонов.
"Просто статья" - это пустой шаблон
}

//TODO: Когда буду парсить указания:
// Любые указания типа (поэт.) относятся ко всему sense, не к отдельным глоссам.
// Нужно проверять, что они идут перед первым глоссом.
// Но бывает так: (поэт.) красивая девушка; (непоэт.) курица
// Что делать? Разбивать на два sense?

type
  TTemplateVersion = record
    templ: string;
    tvars: TTemplateVariants; //template variants!
    art: TEdictArticle;
    procedure Reset;
  end;
  PTemplateVersion = ^TTemplateVersion;
  TTemplateMgr = record
    versions: array[0..10] of TTemplateVersion;
    version_cnt: integer;
    procedure Reset;
    function Get(_templ: string): PEdictArticle;
  end;
  PTemplateMgr = ^TTemplateMgr;

  TExampleList = TArray<string>;
  PExampleList = ^TExampleList;

//Пока что возвращаем один article -- позже нужно заполнять TemplateMgr
procedure ProcessEntry(hdr: PEntryHeader; body: PEntryBody; mg: PTemplateMgr; examples: PExampleList);

implementation
uses SysUtils, UniStrUtils, WarodaiMarkers, WarodaiXrefs, RegexUtils;

procedure TTemplateVersion.Reset;
begin
  templ := '';
  art.Reset;
end;

procedure TTemplateMgr.Reset;
begin
  version_cnt := 0;
end;

function TTemplateMgr.Get(_templ: string): PEdictArticle;
var i: integer;
  pt: PTemplateVersion;
begin
  for i := 0 to version_cnt - 1 do
    if versions[i].templ=_templ then begin
      Result := @versions[i].art;
      exit;
    end;
 //добавляем новую
  Inc(version_cnt);
  if version_cnt>=Length(versions) then
    raise EParsingException.Create('TemplateMgr: Cannot add one more article version.');
  pt := @versions[version_cnt-1];
  pt.Reset;
  pt.templ := _templ;
  Result := @pt.art;
end;



function CompareStr(const a,b: string): integer;
begin
  Result := UniCompareStr(a,b);
end;

{ Возвращает список всех объявленных в заголовке статьи кандзи.
KanaIfNone: если у каны нет ни одной записи кандзи, добавить её саму как запись кандзи. }
function GetUniqueKanji(hdr: PEntryHeader; KanaIfNone: boolean): TArray<string>;
var i, j: integer;
begin
  Result.Comparison := CompareStr;
  Result.Clear;
  for i := 0 to hdr.words_used - 1 do
    if hdr.words[i].s_kanji_used<=0 then begin
      if KanaIfNone then
       //Если кандзей ноль, то само выражение - своя запись
        Result.AddUnique(hdr.words[i].s_reading);
    end else
    for j := 0 to hdr.words[i].s_kanji_used-1 do
      Result.AddUnique(hdr.words[i].s_kanji[j]);
end;

{ Находит кандзи среди возможных записей слова, или возвращает -1. }
function FindKanjiForWord(word: PEntryWord; const kanji: string): integer;
var i: integer;
begin
  Result := -1;
  for i := 0 to word.s_kanji_used - 1 do
    if word.s_kanji[i]=kanji then begin
      Result := i;
      break;
    end;
end;

type
  TWordFlagSet = array[0..MaxWords-1] of boolean;

{ Заполняет массив флагов, выставляя true, если все доступные записи поддерживаются соотв. словом }
function GetAllKanjiUsed(hdr: PEntryHeader; const AllKanji: TArray<string>): TWordFlagSet;
var i, j: integer;
begin
  for i := 0 to hdr.words_used - 1 do begin
    Result[i] := true; //for starters
    for j := 0 to AllKanji.Length - 1 do
      if FindKanjiForWord(@hdr.words[i], AllKanji.items[j])<0 then begin
        Result[i] := false;
        break;
      end;
  end;
end;



var
  AllKanji: TArray<string>;
  AllKanjiUsed: TWordFlagSet;

procedure ProcessBlock(const body_common, group_common: string; bl: PEntryBlock;
  mg: PTemplateMgr; examples: PExampleList); forward;

procedure ProcessEntry(hdr: PEntryHeader; body: PEntryBody; mg: PTemplateMgr; examples: PExampleList);
var i, j, v, ti: integer;
  idx: integer;
  ver: PTemplateVersion;
  art: PEdictArticle;
  pkj: PEdictKanjiEntry;
  pkn: PEdictKanaEntry;

begin
  mg.Reset;

 //Собираем значения
  for i := 0 to body.group_cnt - 1 do
    for j := 0 to body.groups[i].block_cnt - 1 do
      ProcessBlock(body.common, body.groups[i].common, @body.groups[i].blocks[j], mg, examples);

 //Генерируем варианты шаблонов -- раскрываем опциональные и альтернативные варианты
  for v := 0 to mg.version_cnt - 1 do
    GenerateTemplateVariants(mg.versions[v].templ, @mg.versions[v].tvars);

 //Пишем хедеры
  AllKanji := GetUniqueKanji(hdr, {KanaIfNone=}false);
  AllKanjiUsed := GetAllKanjiUsed(hdr,AllKanji);

  for v := 0 to mg.version_cnt - 1 do begin
    ver := @mg.versions[v];
    art := @ver.art;
    art.ref := hdr.s_ref+'-'+IntToStr(v);

    for i := 0 to AllKanji.Length - 1 do
      for ti := 0 to ver.tvars.Length-1 do begin
        pkj := art.AddKanji;
        pkj.k := ApplyTemplateKanji(ver.tvars.items[ti], AllKanji.items[i]);
       //TODO: markers, POP
      end;

    for i := 0 to hdr.words_used - 1 do
      for ti := 0 to ver.tvars.Length-1 do begin
        pkn := art.AddKana;
        pkn.k := ApplyTemplateKana(ver.tvars.items[ti], hdr.words[i].s_reading);
        pkn.AllKanji := AllKanjiUsed[i];

        if not art.kana[i].AllKanji then
          for j := 0 to hdr.words[i].s_kanji_used - 1 do begin
            idx := AllKanji.Find(hdr.words[i].s_kanji[j]);
            Assert(idx>=0, 'Kanji not found in AllKanji');
            art.kana[i].AddKanjiRef(idx);
          end;
       //TODO: markers, POP
      end;
  end;

end;

procedure VerifyBrackets(const ln: string; const br_op, br_cl: WideChar);
var cnt, i: integer;
begin
  cnt := 0;
  for i := 1 to Length(ln) do
    if ln[i]=br_op then Inc(cnt) else
    if ln[i]=br_cl then begin
      Dec(cnt);
      if cnt<0 then
        raise EBracketsMismatch.Create('Brackets mismatch '+br_op+' and '+br_cl);
    end;
  if cnt<>0 then
    raise EBracketsMismatch.Create('Brackets mismatch '+br_op+' and '+br_cl);
end;

{ Разбивает строку на глоссы }
function SplitGlosses(const ln: string): TStringArray;
var ps, pc: PWideChar;
  b_stack: integer;
  i: integer;
begin
  SetLength(Result, 0);
  if ln='' then exit;

 { Мы ищем "," и ";", но не внутри никаких скобок.
  Пока предполагаем, что скобки в формате файла везде расположены правильно,
  и достаточно считать их число, а проверять типы нет необходимости }
  b_stack := 0;

  ps := PWideChar(ln);
  pc := ps;
  while pc^<>#00 do begin
    if (pc^='(') or (pc^='[') or (pc^='{') or (pc^='<') then
      Inc(b_stack)
    else
    if (pc^=')') or (pc^=']') or (pc^='}') or (pc^='>') then begin
      Dec(b_stack);
      if b_stack<0 then
        raise EBracketsMismatch.Create('Brackets mismatch');
    end else
    if (b_stack<=0) and ((pc^=',') or (pc^=';')) then begin
      SetLength(Result, Length(Result)+1);
      Result[Length(Result)-1] := Trim(StrSub(ps,pc));
      ps := PChar(integer(pc)+SizeOf(char));
    end;
    Inc(pc);
  end;

  if pc>=ps then begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := Trim(StrSub(ps,pc));
  end;

  if b_stack<>0 then
    raise EBracketsMismatch.Create('Brackets mismatch');

 //Проверяем, что мы случайно не порезали внутри скобки
  for i := 0 to Length(Result) - 1 do begin
    VerifyBrackets(Result[i], '[', ']');
    VerifyBrackets(Result[i], '(', ')');
    VerifyBrackets(Result[i], '{', '}');
    VerifyBrackets(Result[i], '<', '>');
  end;
end;


const
{
Пустые скобки после удаления всяких флагов.
  ()
  (и) (,) (;,)  --- иногда флаги идут по нескольку
}
  pEmptyParenth='\s*\((?:и|,|;|\s)*\)\s*';

{
Неподдерживаемые обозначения вариантов типа "а)", "б)",
а также поддерживаемые "1)", "2)", оказавшиеся в центре строки.
}
  pNumericId = '(?:^|\s)[0-9]{1,2}\)';
  pLetterId = '(?:^|\s)[а-я]\)';

 //Нужно быть осторожным, чтобы не заматчить легальные строчки типа "(см. 3)"
 //Заматчим ведь!
 //Возможно, придётся проверять, чтобы слева от ")" не было "(" прежде очередной ")"

  pAnyId = pNumericId + '|' + pLetterId;

var
  preEmptyParenth: TPerlRegEx;
  preAnyId: TPerlRegEx;


procedure ParseLn(const ln: string; sn: PEdictSenseEntry);
var tmp: string;
  gloss: string;
begin
  tmp := ln;
  EatXrefs(tmp, sn);
  tmp := Trim(preEmptyParenth.DeleteAll(tmp)); //удаляем оставшиеся пустыми скобки
  if tmp='' then exit; //пустые строки пропускаем

 //Вот это вообще не является ошибкой в принципе, хотя часто является на практике
 //Однако слова в кандзи, и даже неудалённый ссылки, в статье допускаются
  if EvalChars(tmp) and (EV_KANA or EV_KANJI) <> 0 then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after all extractions');

 //Неформатные ссылки в теле статье в принципе допустимы,
 //но чаще это просто результат ошибки разбора.
  if HasHrefParts(UTF8String(tmp)) then begin
    Inc(WarodaiStats.HrefsRemain);
    DumpMsg('WARN -- hrefs remain');
  end;

  if preAnyId.HasMatches(ln) then
    raise EAlternativeIds.Create('Alternative ids or non-parsed ids in string');

 {
  Мы допускаем несколько строк базового перевода, но только для случаев
    Перевод перевод
    см. также ССЫЛКА
  На практике абсолютное большинство двойных базовых строк - такие и есть.
 }
  if sn.glosses_used>0 then //если это не первый перевод, он должен был занулиться и выйти
    raise ESeveralProperTranslations.Create('Block has several proper translations');

  for gloss in SplitGlosses(tmp) do
    sn.AddGloss(gloss); //TODO: markers, lsources
end;


procedure ProcessBlock(const body_common, group_common: string; bl: PEntryBlock;
  mg: PTemplateMgr; examples: PExampleList);
var j, k: integer;
  tmp: string;
  templ: string;
  t_p: TTemplateList;
  sn: PEdictSenseEntry;
  sn_base: PEdictSenseEntry;

  ev: integer;
  tl_lines: integer;
  last_tl: boolean;
  mixed_tl: boolean;
begin
  if bl.line_cnt<0 then
    raise EParsingException.Create('Block has no lines');
  sn_base := nil;

  last_tl := true;
  mixed_tl := false;
  tl_lines := 0;

  for j := 0 to bl.line_cnt - 1 do begin
    tmp := bl.lines[j];
    ev := EvalChars(tmp);

    if ev and EV_KANJI = EV_KANJI then begin
      Inc(WarodaiStats.KanjiLines);
    end else
    if ev and EV_KANA = EV_KANA then begin
      Inc(WarodaiStats.KanaLines);
    end else begin
      Inc(WarodaiStats.TlLines);
      Inc(tl_lines);
      if not last_tl then
        mixed_tl := true;
    end;

    if tmp[Length(tmp)]=':' then
      raise EColonAfterTl.Create('Colon after TL');

   //Template
    if ExtractTemplate(tmp, t_p) then begin
      if Length(t_p)>1 then
        Inc(WarodaiStats.MultiTemplates);
     //Добавляем все в соотв. записи
      for k := 0 to Length(t_p) - 1 do begin
        if t_p[k]='' then
          raise EEmptyTemplatePart.Create('Invalid empty template part.');
        sn := mg.Get(t_p[k])^.AddSense;
        ParseLn(tmp, sn);
      end;
      Inc(WarodaiStats.TemplateLines);
    end else

   //Example
    if ExtractExample(tmp, templ) then begin
      if examples<>nil then
        examples^.Add(templ + ' === '+ tmp);
    end else

   //Text line
    begin
     {
      Проверка отключена. Вместо этого проверяем позже, при добавлении.
      if sn_base<>nil then
        raise ESeveralProperTranslations.Create('Block has several proper translations');
        //мы могли бы просто добавить их, но это странная ситуация, так что не будем
     }

     //Только для основного смысла -- допускаем несколько строк
     //(частая ситуация - "см. также" на второй строке)
      if sn_base<>nil then
        sn := sn_base
      else begin
        sn := mg.Get('').AddSense;
        sn_base := sn;
      end;
      ParseLn(tmp, sn);
    end;

    last_tl := false;
  end;

  if tl_lines>1 then
    Inc(WarodaiStats.SeveralTlLines);
  if mixed_tl then
    Inc(WarodaiStats.MixedTlLines);
end;





initialization
  preEmptyParenth := Regex(pEmptyParenth);
  preAnyId := Regex(pAnyId);

finalization
  FreeAndNil(preAnyId);
  FreeAndNil(preEmptyParenth);

end.
