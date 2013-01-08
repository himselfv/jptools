unit EdictWriter;
{
Функции, связанные с записью в формат EDICT
}

interface
uses Warodai, WarodaiHeader, WarodaiBody, WarodaiTemplates, WakanDic;

var
  EdictStats: record
    AddedRecords: integer;
  end;

procedure CreateOutput(const filename: string);
procedure CloseOutput;

procedure PrintEdict(hdr: PEntryHeader; body: PEntryBody; mark: TEntryMarkers);

type
  TLineHeader = record
    kanji: string;
    kana: string;
    mark: TEntryWordMarkers;
  end;
  PLineHeader = ^TLineHeader;

procedure PrintEdictBody(const hdr: TLineHeader; const body: PEntryBody);
procedure PrintEdictGroup(const hdr: TLineHeader; const common: string; const group: PEntryGroup);
procedure PrintVersions(const hdr: TLineHeader; const common: string; const group: PEntryGroup);

procedure FormatLineHeader(hdr: TLineHeader; out s_pre, s_post: string);

implementation
uses SysUtils, Classes, StreamUtils;

var
  outp: TCharWriter;

procedure CreateOutput(const filename: string);
begin
  outp := TCharWriter.Create(TFileStream.Create(filename, fmCreate), csUtf16LE, true);
  outp.WriteBom;
end;

procedure CloseOutput;
begin
  FreeAndNil(outp);
end;

procedure PrintEdict(hdr: PEntryHeader; body: PEntryBody; mark: TEntryMarkers);
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

procedure PrintEdictBody(const hdr: TLineHeader; const body: PEntryBody);
var i: integer;
begin
  for i := 0 to body.group_cnt - 1 do
    PrintEdictGroup(hdr, body.common, @body.groups[i]);
end;


{
Собираем несколько версий статьи, по числу разных шаблонов.
"Просто статья" - это пустой шаблон
}

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


function repl(const s:string;const sub,repl:string):string;
begin
  Result := s;
  while pos(sub,Result)>0 do
    Result:=copy(Result,1,pos(sub,Result)-1)+repl+copy(Result,pos(sub,Result)+length(sub),length(Result)-pos(sub,Result)+1-length(sub));
end;

procedure PrintEdictGroup(const hdr: TLineHeader; const common: string; const group: PEntryGroup);
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
        if bl_cnt > 0 then begin
          Inc(WarodaiStats.SeveralProperTranslations);
          raise ESeveralProperTranslations.Create('Block '+IntToStr(i)+' has several proper translations')
          //мы могли бы просто добавить их, но это странная ситуация, так что не будем
        end;
        s_base.Add(bl.lines[j]);
        Inc(bl_cnt);
      end;
    end;

  end;

 //Печатаем все версии
  PrintVersions(hdr, common, group);
end;

procedure PrintVersions(const hdr: TLineHeader; const common: string; const group: PEntryGroup);
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
    Inc(EdictStats.AddedRecords);
  end;
end;

procedure FormatLineHeader(hdr: TLineHeader; out s_pre, s_post: string);
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


initialization
  FillChar(EdictStats, sizeof(EdictStats), 0);

end.
