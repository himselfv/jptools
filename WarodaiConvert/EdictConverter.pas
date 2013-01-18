unit EdictConverter;

interface
uses Warodai, WarodaiHeader, WarodaiBody, EdictWriter, WcUtils;


{
Собираем несколько версий статьи, по числу разных шаблонов.
"Просто статья" - это пустой шаблон
}

{
type
  TTemplateVersion = record
    templ: string;
    art: string;
    b_no: integer;
    procedure Reset;
    procedure Add(s: string);
  end;
  PTemplateVersion = ^TTemplateVersion;
  TTemplateMgr = record
    versions: array[0..8] of TTemplateVersion;
    version_cnt: integer;
    procedure Reset;
    function Get(_templ: string): PTemplateVersion;
  end;

procedure TTemplateVersion.Reset;
begin
  templ := '';
  art := '';
  b_no := 0;
end;

procedure TTemplateVersion.Add(s: string);
begin
  if art='' then
    art := s+'/'
  else begin
    if b_no=0 then
     //Превращаем в нумерованный список
     //TODO: Число нужно вставлять после любых локальных грам. флагов -- у нас пока их нет -- и первое, и второе
      art := '(1) '+art+'(2) '+s+'/'
    else
      art := art + '('+IntToStr(b_no)+') '+s+'/';
  end;
  Inc(b_no);
end;

procedure TTemplateMgr.Reset;
begin
  version_cnt := 0;
end;

function TTemplateMgr.Get(_templ: string): PTemplateVersion;
var i: integer;
begin
  for i := 0 to version_cnt - 1 do
    if versions[version_cnt].templ=_templ then begin
      Result := @versions[version_cnt];
      exit;
    end;
 //добавляем новую
  Inc(version_cnt);
  if version_cnt>Length(versions) then
    raise EParsingException.Create('TemplateMgr: Cannot add one more article version.');
  Result := @versions[version_cnt-1];
  Result^.Reset;
  Result^.templ := _templ;
end;

var
  verMgr: TTemplateMgr;
}

procedure ProcessEntry(hdr: PEntryHeader; body: PEntryBody);

implementation
uses SysUtils;

function CompareStr(const a,b: string): integer;
begin
  Result := AnsiCompareStr(a,b);
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
  art: TEdictArticle;
  AllKanji: TList<string>;
  AllKanjiUsed: TWordFlagSet;

procedure ProcessEntry(hdr: PEntryHeader; body: PEntryBody);
var i, j: integer;
  idx: integer;
begin
  art.Reset;
  art.ref := hdr.s_ref;

  AllKanji := GetUniqueKanji(hdr, {KanaIfNone=}false);
  AllKanjiUsed := GetAllKanjiUsed(hdr,AllKanji);

  art.kanji_used := AllKanji.Count;
  for i := 0 to art.kanji_used - 1 do begin
    art.kanji[i].Reset;
    art.kanji[i].k := AllKanji.items[i];
   //TODO: markers, POP
  end;

  art.kana_used := hdr.words_used;
  for i := 0 to hdr.words_used - 1 do begin
    art.kana[i].Reset;
    art.kana[i].k := hdr.words[i].s_reading;
    art.kana[i].AllKanji := AllKanjiUsed[i];
    if not art.kana[i].AllKanji then
      for j := 0 to hdr.words[i].s_kanji_used - 1 do begin
        idx := AllKanji.Find(hdr.words[i].s_kanji[j]);
        Assert(idx>=0, 'Kanji not found in AllKanji');
        art.kana[i].AddKanjiRef(idx);
      end;
   //TODO: markers, POP
  end;

 //TODO: Build senses.
 //TODO: Разделять статью на несколько статей по шаблонам, и возвращать в каком-то виде все эти статьи.

end;

{

procedure TEdict1Writer.Print(hdr: PEntryHeader; body: PEntryBody; mark: TEntryMarkers);
var i, j: integer;
  w_head: TLineHeader;
begin
  for i := 0 to hdr.words_used - 1 do begin
    w_head.mark := mark[i];
    if hdr.words[i].s_kanji_used<=0 then begin
      w_head.kanji := '';
      w_head.kana := hdr.words[i].s_reading;
      PrintEdictBody(w_head, body);
    end else
    for j := 0 to hdr.words[i].s_kanji_used-1 do begin
      w_head.kanji := hdr.words[i].s_kanji[j];
      w_head.kana := hdr.words[i].s_reading;
      PrintEdictBody(w_head, body);
    end;
  end;
end;

procedure TEdict1Writer.PrintEdictBody(const hdr: TLineHeader; const body: PEntryBody);
var i: integer;
begin
  for i := 0 to body.group_cnt - 1 do
    PrintEdictGroup(hdr, body.common, @body.groups[i]);
end;



procedure TEdict1Writer.PrintEdictGroup(const hdr: TLineHeader; const common: string; const group: PEntryGroup);
var i, j, k: integer;
  bl: PEntryBlock;
  bl_cnt: integer;
  s_base: PTemplateVersion;
  templ: string;
  tmp: string;
  t_p: TTemplateList;
begin
  verMgr.Reset;
  s_base := verMgr.Get('');

  for i := 0 to group.block_cnt - 1 do begin
    if group.blocks[i].line_cnt<0 then
      raise EParsingException.Create('Block '+IntToStr(i)+' has no lines');

    bl_cnt := 0; //block proper translation count
    bl := @group.blocks[i];
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
          verMgr.Get(t_p[k])^.Add(tmp);
        end;
      end else
      if ExtractExample(tmp, templ) then begin
        continue //потом будем разбирать ещё и примеры, и строки из каны+этого слова
      end else begin
        if bl_cnt > 0 then
          raise ESeveralProperTranslations.Create('Block '+IntToStr(i)+' has several proper translations');
          //мы могли бы просто добавить их, но это странная ситуация, так что не будем
        s_base.Add(bl.lines[j]);
        Inc(bl_cnt);
      end;
    end;

  end;

 //Печатаем все версии
  PrintVersions(hdr, common, group);
end;

procedure TEdict1Writer.PrintVersions(const hdr: TLineHeader; const common: string; const group: PEntryGroup);
var i: integer;
  s_pre, s_com, s_post: string;
  tmp_hdr: TLineHeader;
  pv: PTemplateVersion;
begin
  s_com := '';
  if common<>'' then
    s_com := s_com + common + ' ';
  if group.common<>'' then
    s_com := s_com + group.common + ' ';

  for i := 0 to verMgr.version_cnt - 1 do  begin
    pv := @verMgr.versions[i];
    if pv.templ<>'' then begin
      tmp_hdr.kanji := repl(pv.templ, '～', hdr.kanji);
      tmp_hdr.kana := repl(pv.templ, '～', hdr.kana);
      tmp_hdr.mark := hdr.mark;
    end else
      tmp_hdr := hdr;
    FormatLineHeader(tmp_hdr, s_pre, s_post);
    outp.WriteLine(s_pre + s_com + pv.art + s_post);
    Inc(FAddedRecords);
  end;
end;

procedure TEdict1Writer.FormatLineHeader(hdr: TLineHeader; out s_pre, s_post: string);
begin
  if hdr.kanji <> '' then
    s_pre := hdr.kanji + ' [' + hdr.kana + '] /'
  else
    s_pre := hdr.kana + ' /';
  if hdr.mark.markers<>'' then
    s_pre := s_pre + '(' + hdr.mark.markers + ') '
  else
    s_pre := s_pre;
  if hdr.mark.pop then
    s_post := s_post + '(P)/'
  else
    s_post := '';
end;
}




end.
