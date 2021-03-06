﻿unit EdictWriter;
{
Функции, связанные с записью в формат EDICT.
Все варианты формата более-менее похожи, и происходят из JMDict,
поэтому данные передаются им в одинаковой форме,
однако младшие форматы их немного упрощают.
}

//{$DEFINE ICONV_EDICT1}
{ EDICT2/JMDict поддерживает любые юникод-символы в теле статьи.
 Однако EDICT1 официально позволяет только US-ASCII (а мы расширяем это до CP1251).
 По умолчанию мы плюём на это требование и выводим в EDICT1 идентичные данные,
 но ICONV_EDICT1 заставляет пробовать их преобразовать, а также проверять,
 что в статье нет символов, не умещающихся в этой кодировке.

 На практике это страшно неудобно, т.к. iconv не умеет ни нормально транслитеровать
 большинство не умеющающихся символов, ни отбрасывать символы ударения в русском,
 и поэтому очень много статей оказываются выброшены. }

interface
uses SysUtils, StreamUtils, JWBIO{$IFDEF ICONV_EDICT1}, iconv{$ENDIF};

{$IFDEF FPC}
type
  UnicodeString = WideString;
{$ENDIF}

const
  MaxKanji = 8;
  MaxKana = 8;
  MaxGlosses = 48;
  MaxXrefs = 12; //бывало и 12
  MaxAnts = 1;
  MaxLsources = 1;
  MaxSenses = 32;
 //Если будет нехватать - повышайте

type
  EEdictWriterException = class(Exception);

type
  TEdictKanjiEntry = record
    k: string;
    inf: string; //markers for <ke_pri>
    pop: boolean; // см. заметку
    procedure Reset;
  end;
  PEdictKanjiEntry = ^TEdictKanjiEntry;

  TEdictKanaEntry = record
    k: string;
    inf: string; //markers for <re_pri>
    AllKanji: boolean; //true, если кана годится для всех кандзи в статье
    Kanji: array[0..MaxKanji-1] of integer; //kanji references
    Kanji_used: integer;
    pop: boolean; //см. заметку
    procedure Reset;
    procedure AddKanjiRef(ref: integer);
  end;
  PEdictKanaEntry = ^TEdictKanaEntry;

 {
  О поле POP в кане и кандзи:
  JMDict для каны и для кандзи хранит пометки <pri>, отмечающие вхождение слова
  в разные индексы популярности вроде WORDFREQ.
  При экспорте в EDICT слова, имеющие пометки news1, ichi1, gai1 и spec1,
  получают пометку (P) - либо ко всей записи, либо к кане, либо к кандзи (см. ниже).
  Поскольку нам проверять все эти индексы не с руки, и получаем мы уже готовую
  информацию (P или не P), мы храним только метку POP, по которой пишем в JMDict
  метку spec1 (особая метка для выбранных вручную популярных слов).
  Если наш JMDict будут импортировать в главный, метки <pri> там проставят самостоятельно.
 }

  TEdictLSource = record
    lang: string; //язык оригинала
    expr: string; //слово в языке оригинала, от которого произошло значение
    procedure Reset;
  end;
  PEdictLSource = ^TEdictLSource;

  TEdictXref = record
    tp: string;
    val: string;
    procedure Reset;
  end;
  PEdictXref = ^TEdictXref;

 {
  Также интересны:
    gloss g_gend -- в файле нет -- поддерживается?
    example -- в файле нет -- даже формат неясен
 }
  TEdictSenseEntry = record
    glosses: array[0..MaxGlosses-1] of string;
    glosses_used: integer;
    xrefs: array[0..MaxXrefs-1] of TEdictXref; //ссылка на связанную запись <xref>[кана или кандзи]</xref> -- EDICT2: (See [кана или кандзи])
    xrefs_used: integer;
    ants: array[0..MaxAnts-1] of string; //ссылки на антонимы <ant>[кана или кандзи]</ant> -- EDICT2: (ant: [кана или кандзи])
    ants_used: integer;
    lsources: array[0..MaxLsources] of TEdictLSource; //язык-источник, напр. <lsource xml:lang=ru>собака</lsource>SOBAKA -- EDICT2: (ru: собака)
    lsources_used: integer;                          //также может быть: <lsource xml:lang="lat"/>
   //Теги. Заполняются через запятую, как в EDICT2.
   //В готовом виде: -- JMDict: &n; &uk; -- EDICT2: (n) (uk)
    t_pos: string; //part of speech -- EDICT2: (n,adj-no)
    t_field: string; //field of application -- EDICT2: {math}
    t_dial: string; //dialect -- EDICT2: (ksb:)
    t_misc: string; //прочее -- EDICT2: (uk)
    procedure Reset;
    procedure AddGloss(const val: string);
    function AddXref(const tp, val: string): PEdictXref;
    procedure AddAnt(const val: string);
    function AddLsource(const lang, expr: string): PEdictLSource;
    procedure AddTPos(const tag: string);
    procedure AddTField(const tag: string);
    procedure AddTDial(const tag: string);
    procedure AddTMisc(const tag: string);
  end;
  PEdictSenseEntry = ^TEdictSenseEntry;

  TEdictArticle = record
    ref: string;
    kanji: array[0..MaxKanji-1] of TEdictKanjiEntry;
    kanji_used: integer;
    kana: array[0..MaxKana-1] of TEdictKanaEntry;
    kana_used: integer;
    senses: array[0..MaxSenses-1] of TEdictSenseEntry;
    senses_used: integer;
    procedure Reset;
    function AddKanji: PEdictKanjiEntry;
    function AddKana: PEdictKanaEntry;
    function AddSense: PEdictSenseEntry;
  end;
  PEdictArticle = ^TEdictArticle;

type
  TArticleWriter = class
  protected
    outp: TStreamEncoder;
    FOwnsEncoder: boolean;
    FAddedRecords: integer;
    procedure StartFile; virtual;
    procedure FinalizeFile; virtual;
  public
    constructor Create(const filename: string); overload;
    constructor Create(AEncoder: TStreamEncoder; AOwnsEncoder: boolean = false); overload;
    destructor Destroy; override;
    procedure Print(art: PEdictArticle); overload; virtual; abstract;
    property AddedRecords: integer read FAddedRecords;
  end;

type
  TPopStats = record
    AllKanjiPop: boolean;
    AllKanaPop: boolean;
    HasPop: boolean;
  end;

function GetPopStats(art: PEdictArticle): TPopStats;
function EdictBuildArticleBody(wr: TArticleWriter; art: PEdictArticle): string;

type
  TEdict1Writer = class(TArticleWriter)
  protected
   {$IFDEF ICONV_EDICT1}
    conv: iconv_t;
   {$ENDIF}
    procedure Print2(art: PEdictArticle; const kanji, kana: integer; const body: string);
  public
    constructor Create(const filename: string);
    destructor Destroy; override;
    procedure Print(art: PEdictArticle); override;
  end;

type
  TEdict2Writer = class(TArticleWriter)
  protected
    function KanaToStr(art: PEdictArticle; idx: integer): string;
  public
    procedure Print(art: PEdictArticle); override;
  end;

type
  TJmDictWriter = class(TArticleWriter)
  protected
    procedure StartFile; override;
    procedure FinalizeFile; override;
    procedure PrintTags(const tag_name, tag_vals: string);
  public
    procedure Print(art: PEdictArticle); override;
  end;

implementation
uses Classes;

{
Article
}

procedure TEdictKanjiEntry.Reset;
begin
  k := '';
  inf := '';
  pop := false;
end;

procedure TEdictKanaEntry.Reset;
begin
  k := '';
  inf := '';
  AllKanji := false;
  kanji_used := 0;
  pop := false;
end;

procedure TEdictKanaEntry.AddKanjiRef(ref: integer);
begin
  if Kanji_used >= Length(Kanji) then
    raise EEdictWriterException.Create('EdictKanaEntry: Cannot add one more kana');
  Kanji[Kanji_used] := ref;
  Inc(Kanji_used);
end;

procedure TEdictLSource.Reset;
begin
  lang:='';
  expr:='';
end;

procedure TEdictXref.Reset;
begin
  tp := '';
  val := '';
end;

procedure TEdictSenseEntry.Reset;
begin
  glosses_used := 0;
  xrefs_used := 0;
  ants_used := 0;
  lsources_used := 0;
  t_pos := '';
  t_field := '';
  t_dial := '';
  t_misc := '';
end;

procedure TEdictSenseEntry.AddGloss(const val: string);
begin
  if glosses_used >= Length(glosses) then
    raise EEdictWriterException.Create('EdictSenseEntry: Cannot add one more gloss');
  if val='' then exit; //пустые не добавляем
  glosses[glosses_used] := val;
  Inc(glosses_used);
end;

function TEdictSenseEntry.AddXref(const tp, val: string): PEdictXref;
begin
  if xrefs_used >= Length(xrefs) then
    raise EEdictWriterException.Create('EdictSenseEntry: Cannot add one more xref');
  Result := @xrefs[xrefs_used];
  Result^.Reset;
  Result^.tp := tp;
  Result^.val := val;;
  Inc(xrefs_used);
end;

procedure TEdictSenseEntry.AddAnt(const val: string);
begin
  if ants_used >= Length(ants) then
    raise EEdictWriterException.Create('EdictSenseEntry: Cannot add one more ant');
  ants[ants_used] := val;
  Inc(ants_used);
end;

function TEdictSenseEntry.AddLsource(const lang, expr: string): PEdictLSource;
begin
  if lsources_used >= Length(lsources) then
    raise EEdictWriterException.Create('EdictSenseEntry: Cannot add one more lsources');
  Result := @lsources[lsources_used];
  Result^.Reset;
  Result^.lang := lang;
  Result^.expr := expr;
  Inc(lsources_used);
end;

procedure TEdictSenseEntry.AddTPos(const tag: string);
begin
  if t_pos<>'' then
    t_pos := t_pos+','+tag
  else
    t_pos := tag;
end;

procedure TEdictSenseEntry.AddTField(const tag: string);
begin
  if t_field<>'' then
    t_field := t_field+','+tag
  else
    t_field := tag;
end;

procedure TEdictSenseEntry.AddTDial(const tag: string);
begin
  if t_dial<>'' then
    t_dial := t_dial+','+tag
  else
    t_dial := tag;
end;

procedure TEdictSenseEntry.AddTMisc(const tag: string);
begin
  if t_misc<>'' then
    t_misc := t_misc+','+tag
  else
    t_misc := tag;
end;

procedure TEdictArticle.Reset;
begin
  ref := '';
  kanji_used := 0;
  kana_used := 0;
  senses_used := 0;
end;

function TEdictArticle.AddKanji: PEdictKanjiEntry;
begin
  if kanji_used >= Length(kanji) then
    raise EEdictWriterException.Create('EdictArticle: Cannot add one more kanji');
  Result := @kanji[kanji_used];
  Result^.Reset;
  Inc(kanji_used);
end;

function TEdictArticle.AddKana: PEdictKanaEntry;
begin
  if kana_used >= Length(kana) then
    raise EEdictWriterException.Create('EdictArticle: Cannot add one more kana');
  Result := @kana[kana_used];
  Result^.Reset;
  Inc(kana_used);
end;

function TEdictArticle.AddSense: PEdictSenseEntry;
begin
  if senses_used >= Length(senses) then
    raise EEdictWriterException.Create('EdictArticle: Cannot add one more sense');
  Result := @senses[senses_used];
  Result^.Reset;
  Inc(senses_used);
end;

{
ArticleWriter
}

constructor TArticleWriter.Create(const filename: string);
var AOutp: TStreamEncoder;
begin
  AOutp := CreateTextFile(filename, TUTF16Encoding);
  AOutp.WriteBom;
  Create(AOutp, {OwnsEncoder=}true);
end;

constructor TArticleWriter.Create(AEncoder: TStreamEncoder;
  AOwnsEncoder: boolean = false);
begin
  inherited Create;
  outp := AEncoder;
  FOwnsEncoder := AOwnsEncoder;
  FAddedRecords := 0;
  StartFile;
end;

destructor TArticleWriter.Destroy;
begin
  FinalizeFile;
  FreeAndNil(outp);
  inherited;
end;

procedure TArticleWriter.StartFile;
begin
end;

procedure TArticleWriter.FinalizeFile;
begin
  outp.Flush;
end;


{
Утилиты
}
function GetPopStats(art: PEdictArticle): TPopStats;
var i: integer;
begin
  Result.HasPop := false;
  Result.AllKanjiPop := true;
  for i := 0 to art.kanji_used - 1 do
    if art.kanji[i].pop then
      Result.HasPop := true
    else
      Result.AllKanjiPop := false;

  Result.AllKanaPop := true;
  for i := 0 to art.kana_used - 1 do
    if art.kana[i].pop then
      Result.HasPop := true
    else
      Result.AllKanaPop := false;
end;

//Составляет тело статьи.
function EdictBuildArticleBody(wr: TArticleWriter; art: PEdictArticle): string;
var i, j: integer;
  se: PEdictSenseEntry;
  se_ln: string;
  xr_ref, xr_ant: string;
  expr: string;
  Edict1: boolean;
begin
  Result := '';
  Edict1 := wr is TEdict1Writer;

  for i := 0 to art.senses_used - 1 do begin
    se := @art.senses[i];
    se_ln := '';

    if se.t_pos<>'' then
      se_ln := '('+se.t_pos+') ';

    if art.senses_used>1 then
      se_ln := '('+IntToStr(i+1)+') '; //после грам. тегов -- так сделано в английском EDICT2

    if se.t_field<>'' then
      se_ln := '{'+se.t_field+'} ';
    if se.t_dial<>'' then
      se_ln := '('+se.t_dial+') ';
    if se.t_misc<>'' then
      se_ln := '('+se.t_misc+') ';

    if not Edict1 then begin
     //Ref
      xr_ref := '';
      for j := 0 to se.xrefs_used - 1 do
        xr_ref := xr_ref + se.xrefs[j].val + ',';
      if xr_ref<>'' then
        se_ln := se_ln + '(See '+copy(xr_ref,1,Length(xr_ref)-1)+') ';
     //ant
      xr_ant := '';
      for j := 0 to se.ants_used - 1 do
        xr_ant := xr_ant + se.ants[j] + ',';
      if xr_ant<>'' then
        se_ln := se_ln + '(ant: '+copy(xr_ant,1,Length(xr_ant)-1)+') ';
    end;

   //языки-источники включаем даже в EDICT1, хотя там expr должно быть транслитом!
    for j := 0 to se.lsources_used - 1 do begin
     {$IFDEF ICONV_EDICT1}
      if Edict1 then try
        expr := UnicodeString(iconv2(TEdict1Writer(wr).conv conv,se.lsources[j].expr))
      except
        on E: EIConvError do
          expr := ''; //cannot convert!
      end
      else
     {$ENDIF}
        expr := se.lsources[j].expr;
      se_ln := se_ln + '('+se.lsources[j].lang+':'+expr+') '; //sic! даже когда expr==''. так в едикте
    end;

    if se.glosses_used>0 then begin
      se_ln := se_ln + se.glosses[0];
      for j := 1 to se.glosses_used - 1 do
        se_ln := se_ln + '/' + se.glosses[j];
    end;

    if se_ln<>'' then //пустые значения (напр. только ссылки, и ничего не вошло) не добавляем
      Result := Result + '/' + se_ln;
  end;

 {$IFDEF ICONV_EDICT1}
 //По условию, все символы в поле "значение" Едикта-1 должны входить в US-ASCII.
 //В нашем случае допустимо также CP1251, поскольку словарь русский, но не больше.
 //Конвертируем и проверяем, будет ли ошибка.
 //Вообще-то, мы должны были отфильтровать это ещё раньше, но на всякий случай.
  if Edict1 then try
    iconv2(conv,Result);
  except
    on E: EIConvError do
      raise EParsingException.Create('Invalid target codepage symbols in article body');
  end;
 {$ENDIF}

end;


{
EDICT1
}

constructor TEdict1Writer.Create(const filename: string);
begin
  inherited;
 {$IFDEF ICONV_EDICT1}
  conv := iconv_open('CP1251//TRANSLIT', 'UTF-16LE');
  if conv=iconv_t(-1) then
    raise Exception.Create('Cannot initialize iconv');
 {$ENDIF}
end;

destructor TEdict1Writer.Destroy;
begin
 {$IFDEF ICONV_EDICT1}
  iconv_close(conv);
 {$ENDIF}
  inherited;
end;

procedure TEdict1Writer.Print(art: PEdictArticle);
var i, j: integer;
  body: string;
begin
 //Генерируем тело статьи
  body := EdictBuildArticleBody(Self, art);
  if body='' then exit; //статья пустая -- видимо, ничто в ней не годилось для EDICT1

 //Печатаем все варианты
  for i := 0 to art.kana_used  - 1 do
    if art.kanji_used<=0 then begin
      Print2(art, -1, i, body);
    end else
    if art.kana[i].AllKanji then begin
      for j := 0 to art.kanji_used - 1 do
        Print2(art, j, i, body);
    end else begin
      for j := 0 to art.kana[i].Kanji_used - 1 do
        Print2(art, art.kana[i].Kanji[j], i, body);
    end;
end;

procedure TEdict1Writer.Print2(art: PEdictArticle; const kanji, kana: integer; const body: string);
var ln: string;
  k_flags: string;
begin
  if kanji>=0 then begin
    ln := art.kanji[kanji].k;
    if kana>=0 then
      ln := ln + ' [' + art.kana[kana].k + ']';
  end else
   //kana must be set
    ln := art.kana[kana].k;

 //В EDICT1 флаги каны и кандзи пишутся перед первым вхождением sense
  k_flags := '';
  if (kanji>=0) and (art.kanji[kanji].inf<>'') then
    k_flags := k_flags + '('+art.kanji[kanji].inf+') ';
  if (kana>=0) and (art.kana[kana].inf<>'') then
    k_flags := k_flags + '('+art.kana[kana].inf+') ';
  if k_flags<>'' then begin
    ln := ln + ' /'+k_flags;
    if body<>'' then
      ln := ln + ' ' + copy(body,2,Length(body)-1); //пропускаем стартовый '/'
  end else
    ln := ln + ' ' + body;

  if ((kanji>=0) and art.kanji[kanji].pop)
  or ((kana>=0) and art.kana[kana].pop) then
    ln := ln + '/(P)';

  outp.WriteLn(ln+'/');
  Inc(FAddedRecords);
end;


{
EDICT2
}

//Генерирует запись вида кана(кандзи,кандзи) для каны #idx из статьи art
function TEdict2Writer.KanaToStr(art: PEdictArticle; idx: integer): string;
var i: integer;
begin
  Result := art.kana[idx].k;
  if art.kana[idx].AllKanji then exit;
  if art.kanji_used<=0 then exit; //на всякий случай, хотя тогда должен стоять AllKanji, наверное?

 { Вообще-то говоря, у каны может не быть отсылок на кандзи, и в таком случае
  единственный способ доступно это записать:
    кандзи1;кандзи2;КАНА2[кана1(кандзи1,кандзи2);КАНА2(КАНА2)]
  То есть, объявить кану отдельной записью. Это будет логично.

  Вариант хуже - написать кану с пустыми скобками:
    КАНА2()
  На это мало кто рассчитывает, и вообще, что это значит? Для какой записи это чтение?

  Однако по факту EDICT в таких случаях пишет кану так, как будто она годится для всех записей:
    кандзи1;кандзи2[кана1;КАНА2]
  Это ошибка, но раз так делает EDICT, мы поступим так же. }
  if art.kana[idx].Kanji_used<=0 then exit;

  Result := Result + '(' + art.kanji[art.kana[idx].Kanji[0]].k;
  for i := 1 to art.kana[idx].Kanji_used - 1 do
    Result := Result + ',' + art.kanji[art.kana[idx].Kanji[i]].k;
  Result := Result + ')';
end;

procedure TEdict2Writer.Print(art: PEdictArticle);
var ln: string;
  i: integer;

  s_kanji: string;
  s_kana: string;

  PopStats: TPopStats;

begin
  PopStats := GetPopStats(art);

 //кандзи1;кандзи2;кандзи3
  if art.kanji_used=0 then
    s_kanji := ''
  else begin
    s_kanji := art.kanji[0].k;
    if art.kanji[0].pop and not PopStats.AllKanjiPop then
      s_kanji := s_kanji + '(P)';
    for i := 1 to art.kanji_used - 1 do begin
      s_kanji := s_kanji + ';' + art.kanji[i].k;
      if art.kanji[i].pop and not PopStats.AllKanjiPop then
        s_kanji := s_kanji + '(P)';
    end;
  end;

 //кана1;кана2(кандзи1;кандзи2);кана3(кандзи2;кандзи3)
  if art.kana_used=0 then
    s_kana := ''
  else begin
    s_kana := KanaToStr(art, 0);
    if art.kana[0].pop and not PopStats.AllKanaPop then
      s_kana := s_kana + '(P)';
    for i := 1 to art.kana_used - 1 do begin
      s_kana := s_kana + ';' + KanaToStr(art, i);
      if art.kana[i].pop and not PopStats.AllKanaPop then
        s_kana := s_kana + '(P)';
    end;
  end;

  if s_kanji='' then begin
    s_kanji := s_kana; //исключение: "кана1 /(статья) /"
  end else
  if s_kana<>'' then
    s_kanji := s_kanji + ' [' + s_kana + ']';
  s_kana := '';

 //Теперь в s_kanji полный заголовок
 //Собираем в ln значения
  ln := EdictBuildArticleBody(Self, art);

  if PopStats.HasPop then
    ln := ln + '/(P)';

  if art.ref<>'' then
    ln := ln + '/EntL'+art.ref;
  outp.WriteLn(s_kanji+' '+ln+'/');
  Inc(FAddedRecords);
end;


{
JMDict
}

procedure TJMDictWriter.StartFile;
begin
  inherited;
  outp.WriteLn('<!-- JMdict created: '+FormatDatetime('yyyy-mm-dd', now())+' -->');
  outp.WriteLn('<JMdict>');
end;

procedure TJMDictWriter.FinalizeFile;
begin
  outp.WriteLn('</JMdict>');
  inherited;
end;

//Получает строку вида "val1,val2" и печатает набор тегов
//  <tag_name>val1</tag_name>
//  <tag_name>val2</tag_name>
procedure TJMDictWriter.PrintTags(const tag_name, tag_vals: string);
var tmp: string;
  t_pos: integer;
begin
  tmp := tag_vals;
  t_pos := pos(tmp, ',');
  while t_pos>0 do begin
    outp.WriteLn('<'+tag_name+'>&'+copy(tmp,1,t_pos-1)+';</'+tag_name+'>'); //каждый тег отдельно
    tmp := copy(tmp,t_pos+1,Length(tmp)-t_pos);
    t_pos := pos(tmp, ',');
  end;
  if tmp<>'' then
    outp.WriteLn('<'+tag_name+'>&'+tmp+';</'+tag_name+'>');
end;

procedure TJMDictWriter.Print(art: PEdictArticle);
var i,j: integer;
  se: PEdictSenseEntry;
begin
  outp.WriteLn('<entry>');
  if art.ref<>'' then
    outp.WriteLn('<ent_seq>'+art.ref+'</ent_seq>');

  for i := 0 to art.kanji_used - 1 do begin
    outp.WriteLn('<k_ele>');
    outp.WriteLn('<keb>'+art.kanji[i].k+'</keb>');
    if art.kanji[i].inf<>'' then PrintTags('ke_inf', art.kanji[i].inf);
    if art.kanji[i].pop then
      outp.WriteLn('<ke_pri>spec1</ke_pri>');
    outp.WriteLn('</k_ele>');
  end;

  for i := 0 to art.kana_used - 1 do begin
    outp.WriteLn('<r_ele>');
    outp.WriteLn('<reb>'+art.kana[i].k+'</reb>');
    if art.kana[i].inf<>'' then PrintTags('re_inf', art.kana[i].inf);
    if not art.kana[i].AllKanji then begin
      if art.kana[i].Kanji_used<=0 then
        outp.WriteLn('<re_nokanji/>')
      else
        for j := 0 to art.kana[i].Kanji_used - 1 do
          outp.WriteLn('<re_restr>'+art.kanji[art.kana[i].Kanji[j]].k+'</re_restr>');
      if art.kana[i].pop then
        outp.WriteLn('<re_pri>spec1</re_pri>');
    end;
    outp.WriteLn('</r_ele>');
  end;

  for i := 0 to art.senses_used - 1 do begin
    outp.WriteLn('<sense>');
    se := @art.senses[i];
    for j := 0 to se.xrefs_used - 1 do
      if se.xrefs[j].tp='' then
        outp.WriteLn('<xref>'+se.xrefs[j].val+'</xref>')
      else
        outp.WriteLn('<xref type="'+se.xrefs[j].tp+'">'+se.xrefs[j].val+'</xref>');
    for j := 0 to se.ants_used - 1 do
      outp.WriteLn('<ant>'+se.ants[j]+'</ant>');
    for j := 0 to se.lsources_used - 1 do
      if se.lsources[j].expr='' then
        outp.WriteLn('<lsource xml:lang='+se.lsources[j].lang+'/>')
      else
        outp.WriteLn('<lsource xml:lang='+se.lsources[j].lang+'>'+se.lsources[j].expr+'</ant>');
    if se.t_pos<>'' then PrintTags('pos', se.t_pos);
    if se.t_field<>'' then PrintTags('field', se.t_pos);
    if se.t_dial<>'' then PrintTags('dial', se.t_pos);
    if se.t_misc<>'' then PrintTags('misc', se.t_pos);
    for j := 0 to se.glosses_used - 1 do
      outp.WriteLn('<gloss xml:lang="rus">'+se.glosses[j]+'</gloss>'); //note the gloss xml:lang attribute
    outp.WriteLn('</sense>');
  end;

  outp.WriteLn('</entry>');
  Inc(FAddedRecords);
end;

end.
