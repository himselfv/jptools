unit EdictConverter;

interface
uses Warodai, WarodaiHeader, WarodaiBody, WarodaiTemplates, EdictWriter, WcUtils,
  PerlRegEx;

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
    versions: array[0..8] of TTemplateVersion;
    version_cnt: integer;
    procedure Reset;
    function Get(_templ: string): PEdictArticle;
  end;
  PTemplateMgr = ^TTemplateMgr;

  TExampleList = TList<string>;
  PExampleList = ^TExampleList;

//Пока что возвращаем один article -- позже нужно заполнять TemplateMgr
procedure ProcessEntry(hdr: PEntryHeader; body: PEntryBody; mg: PTemplateMgr; examples: PExampleList);

implementation
uses SysUtils, UniStrUtils, WarodaiMarkers;

const
  pHiragana = '\x{3040}-\x{309F}';
  pKatakana = '\x{30A0}-\x{30FF}';
  pCJKUnifiedIdeographs = '\x{4E00}-\x{9FFF}';
  pCJKUnifiedIdeographsExtA = '\x{3400}-\x{4DBF}';
  pCJKUnifiedIdeographsExtB = '\x{20000}-\x{2A6DF}';
  pCJKUnifiedIdeographsExtC = '\x{2A700}-\x{2B73F}';
  pCJKSymbolsAndPunctuation = '\x{3000}-\x{303F}';

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
function GetUniqueKanji(hdr: PEntryHeader; KanaIfNone: boolean): TList<string>;
var i, j: integer;
begin
  Result.Comparison := CompareStr;
  Result.Reset;
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
function GetAllKanjiUsed(hdr: PEntryHeader; const AllKanji: TList<string>): TWordFlagSet;
var i, j: integer;
begin
  for i := 0 to hdr.words_used - 1 do begin
    Result[i] := true; //for starters
    for j := 0 to AllKanji.Count - 1 do
      if FindKanjiForWord(@hdr.words[i], AllKanji.items[j])<0 then begin
        Result[i] := false;
        break;
      end;
  end;
end;






var
  AllKanji: TList<string>;
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

    for i := 0 to AllKanji.Count - 1 do
      for ti := 0 to ver.tvars.Count-1 do begin
        pkj := art.AddKanji;
        pkj.k := ApplyTemplate(ver.tvars.items[ti], AllKanji.items[i]);
       //TODO: markers, POP
      end;

    for i := 0 to hdr.words_used - 1 do
      for ti := 0 to ver.tvars.Count-1 do begin
        pkn := art.AddKana;
        pkn.k := ApplyTemplate(ver.tvars.items[ti], hdr.words[i].s_reading);
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


{
Ссылки:
  см.
  связ.
  связ.:
  тж.
  ср.

Реже:
  производное от, редко, обычно, сокр.
  см.</i> <a href="#1-024-1-49">あおこ</a> <i>и</i> <a href="#1-417-1-45">きなこ</a>

Форма ссылки:
  あわ【泡】 (в едикте через точку)
  ぎんこう(眼の銀行)
}
const
  pLatinNumeral='IVX'; //используются для ссылок на подзначения
  pSomeCJKPunctuation='\x{3000}-\x{3009}\x{3012}-\x{303F}'; //без 【】
  pCJKRefChar = pCJKUnifiedIdeographs + pCJKUnifiedIdeographsExtA
    + pCJKUnifiedIdeographsExtB + pCJKUnifiedIdeographsExtC
    + pHiragana + pKatakana + pSomeCJKPunctuation
    + '\/…';
  pCJKRefStr='['+pCJKRefChar+']+(?:['+pLatinNumeral+']+)?'; //слово и, опционально, латинский номер его вариации (см. DropVariantIndicator)

  pRefBase=pCJKRefStr;
  pRefWri1='【'+pCJKRefStr+'】'; //расшифровка в формате あわ【泡】
  pRefWri2='\('+pCJKRefStr+'\)'; //расшифровка в формате あわ(泡)

 //одна ссылка в любом формате
  pSingleRef='('+pRefBase+')'
    +'('+pRefWri1+'|'+pRefWri2+'|)' //любое из пояснений, или ничего -- чтобы число скобок не менялось
    +'(?:[0-9]{1,3})?' //опционально 1-2о цифры без пробела -- ссылка на подзначение, другой формат
    ;

  pCommonRefNames='см\.|связ\.\:|связ\.|тж\.|ср\.|ант\.';
  pRareRefNames='производное от|редко|чаще|обычно|сокр.|неправ.|уст.|тж. уст.|правильнее';

 //ссылка с названием и пробелами вокруг
  pXref=
      '\s*'
    +'('+pCommonRefNames+'|'+pRareRefNames+')\s'
    +pSingleRef+'(?:\,\s'+pSingleRef+')*' //любое число ссылок больше одной, через запятую
    +'\s*'; //заканчивается чем-нибудь

var
  preXref: TPerlRegEx;

{ Находит в строке все элементы ссылочного типа и регистрирует их в записи Sense }
procedure EatXrefs(var ln: string; sn: PEdictSenseEntry);
var xr0, xr1, xr2: UnicodeString;
  i: integer;
begin
  preXref.Subject := UTF8String(ln);
  if not preXref.Match then exit;
  preXref.Replacement := '';

  repeat
    xr0 := UnicodeString(preXref.Groups[1]); //тип ссылки

   //Может быть несколько: "см. ОДНО, ДРУГОЕ"
    for i := 0 to (preXref.GroupCount-1) div 2 - 1 do begin
      xr1 := UnicodeString(preXref.Groups[2+i*2]);
      xr2 := UnicodeString(preXref.Groups[2+i*2+1]);
      DropVariantIndicator(xr1);
      DropVariantIndicator(xr2);

      if xr2<>'' then
        xr1 := xr1+'・'+xr2;

     //Всё это ненормально
      if pos('…', xr1)>0 then
        raise EIllegalXrefChar.Create('... in xref value');
      if pos('[', xr1)>0 then
        raise EIllegalXrefChar.Create('[ in xref value');
      if pos('(', xr1)>0 then
        raise EIllegalXrefChar.Create('( in xref value');
      if pos('/', xr1)>0 then //а вот это нормально, но что с ним делать непонятно
        raise EIllegalXrefChar.Create('/ in xref value');

      if xr0='ант.' then
        sn.AddAnt(xr1)
      else
      if (xr0='см.') or (xr0='ср.') or (xr0='тж.') or (xr0='связ.') or (xr0='связ.:') then
       //эти xref-ы не требуют пояснений
        sn.AddXref('', xr1)
      else
        sn.AddXref(xr0, xr1);
    end;

    preXref.Replace;
  until not preXref.MatchAgain;

  ln := UnicodeString(preXref.Subject); //after replacements
end;


{
Пустые скобки после удаления всяких флагов.
}
const
  pEmptyParenth= '\s*\(\s*\)\s*';

var
  preEmptyParenth: TPerlRegEx;

procedure RemEmptyParenth(var ln: string);
begin
  preEmptyParenth.Subject := UTF8String(ln);
  preEmptyParenth.Replacement := '';
  if preEmptyParenth.ReplaceAll then
    ln := UnicodeString(preEmptyParenth.Subject); //after replacements
end;


procedure ParseLn(const ln: string; sn: PEdictSenseEntry);
var tmp: string;
  gloss: string;
begin
  tmp := ln;
  EatXrefs(tmp, sn);
  RemEmptyParenth(tmp); //удаляем оставшиеся пустыми скобки
  if EvalChars(tmp)<>EV_NORMAL then
    raise EKanjiKanaLeft.Create('Kanji or kana left in string after all extractions');
  for gloss in SplitGlosses(tmp) do
    sn.AddGloss(gloss); //TODO: markers, xrefs, lsources
end;


procedure ProcessBlock(const body_common, group_common: string; bl: PEntryBlock;
  mg: PTemplateMgr; examples: PExampleList);
var j, k: integer;
  tmp: string;
  templ: string;
  t_p: TTemplateList;
  sn: PEdictSenseEntry;
  bl_cnt: integer;
begin
  if bl.line_cnt<0 then
    raise EParsingException.Create('Block has no lines');
  bl_cnt := 0;

  for j := 0 to bl.line_cnt - 1 do begin
    tmp := bl.lines[j];
    if ExtractTemplate(tmp, templ) then begin
      SplitTemplate(templ, t_p);
      if Length(t_p)>1 then
        Inc(WarodaiStats.MultiTemplates);
     //Добавляем все в соотв. записи
      for k := 0 to Length(t_p) - 1 do begin
        if t_p[k]='' then
          raise EParsingException.Create('Invalid empty template part.');
        sn := mg.Get(t_p[k])^.AddSense;
        ParseLn(tmp, sn);
      end;
    end else
    if ExtractExample(tmp, templ) then begin
      if examples<>nil then
        examples^.Add(templ + ' === '+ tmp);
    end else begin
      if bl_cnt > 0 then
        raise ESeveralProperTranslations.Create('Block has several proper translations');
        //мы могли бы просто добавить их, но это странная ситуация, так что не будем
      sn := mg.Get('').AddSense;
      ParseLn(tmp, sn);
      Inc(bl_cnt);
    end;
  end;

end;




initialization
  preXref := TPerlRegEx.Create;
  preXref.RegEx := pXref;
  preXref.Compile;
  preXref.Study;

  preEmptyParenth := TPerlRegEx.Create;
  preEmptyParenth.RegEx := pEmptyParenth;
  preEmptyParenth.Compile;
  preEmptyParenth.Study;

finalization
  FreeAndNil(preEmptyParenth);
  FreeAndNil(preXref);

end.
