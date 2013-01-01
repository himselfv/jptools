program WarodaiConvert;
{$APPTYPE CONSOLE}

//Look into english EDICT for markers to known words. Slow.
{$DEFINE ENMARKERS}

uses
  SysUtils,
  Classes,
  UniStrUtils,
  StreamUtils,
  Warodai in 'Warodai.pas',
  WakanDic in 'WakanDic.pas';

type
  EBadUsage = class(Exception)
  end;

procedure BadUsage(msg: UnicodeString);
begin
  raise EBadUsage.Create(msg);
end;

procedure PrintUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<ewarodai.txt> <output file>');
end;

var
  InputFile: UnicodeString;
  OutputFile: UnicodeString;

procedure ParseCommandLine;
begin
  if ParamCount<>2 then
    BadUsage('Invalid number of params');
  InputFile := ParamStr(1);
  OutputFile := ParamStr(2);
end;


var
  inp: TCharReader;
  outp: TCharWriter;
  stats: record
    lines: integer;
    artcnt: integer;
    badcnt: integer;
    addcnt: integer;
    wtf: integer;
  end;

function ReadLine(out ln: string): boolean;
begin
  Result := inp.ReadLine(ln);
  if Result then
    Inc(stats.lines);
end;

procedure ReadHeader;
var s: string;
begin
  while ReadLine(s) and (s<>'') do begin end;
end;

//Проматываем текущую запись до конца
procedure SkipArticle;
var s: string;
begin
  while ReadLine(s) and (s<>'') do begin end;
end;

{
Тело состоит из нескольких статей:
  1) знакомство;
  …をお見知り置き下さい позвольте вам представить <i>кого-л.</i>, позвольте вас познакомить <i>с кем-л.</i>;
  2) знакомый.
Каждая статья имеет вид:
  1) описание перевода
  実例 пример с переводом
  実例 пример с переводом
  • комментарий
  • комментарий

В примерах встречается:
  ～にする мариновать в мисо;
  : ～の хорошо знакомый;
  …をお見知り置き下さい позвольте вам представить <i>кого-л.</i>, позвольте вас познакомить <i>с кем-л.</i>;

Дополнительные строки (любым номером):
  <i>см.</i> <a href="#1-604-2-61">みしり</a>.
  <i>ср.</i> <a href="#1-737-1-59">おおみそか</a>.

Флаги могут идти в любом порядке внутри блока <i></i> либо через пробелы, либо в отд. блоках:
  <i>юр.</i>
  <i>уст. вежл.</i>
  <i>ономат.</i> <i>устар.</i>
Флаги могут смешиваться в одном блоке с не флагами:
  <i>кн. см.</i> (номер статьи)
Список известных флагов есть в файле warodai_abrv.dsl, который включён в DSL-сборку словаря.

Пример статьи:
  みっしゅう【密集】(миссю:)〔1-605-2-33〕
  1) скопление;
  ～する сгрудиться; толпиться; кишеть <i>(о насекомых)</i>;
  人家が密集している дома стоят тесно (теснятся);
  2): ～する <i>воен.</i> сосредоточиваться; смыкать ряды;
  密集して進む двигаться [вперёд] сомкнутым строем.

Должна превратиться в:
  みっしゅう【密集】 = скопление;
  みっしゅうする【密集する】 = сгрудиться; толпиться; кишеть <i>(о насекомых)</i>;
  ??? 【人家が密集している】 = дома стоят тесно (теснятся);
  みっしゅうする 【密集する】 = <i>воен.</i> сосредоточиваться; смыкать ряды;
  ??? 【密集して進む】 = двигаться [вперёд] сомкнутым строем.

Если в примере только кана, считаем его статьёй, иначе примером и пропускаем.
}

type
  TEntryBody = record
    tls: array[0..49] of string;
    tl_count: integer;
    tl_base: string;
    examples: array[0..49] of string;
    example_count: integer;
  end;
  PEntryBody = ^TEntryBody;

procedure ReadBody(out body: TEntryBody);
const
  B_TL = 0;
  B_EXAMPLES = 1;
var
  ln: string;
  state: integer;
  num: string;
begin
  body.tl_count := -1;
  body.tl_base := '';
  body.example_count := 0;
  body.examples[0] := '';
  state := B_TL;

  if (not ReadLine(ln)) or (ln='')  then
    raise EParsingException.Create('No next line.');

  repeat
    if Length(ln)<2 then begin
      Inc(stats.wtf);
      continue;
    end;

    if PopTranslationNumber(ln, num) then begin
      Inc(body.tl_count);
      if body.tl_count>=Length(body.tls) then
        raise EParsingException.Create('Not enough cells to put another translation.');
      body.tls[body.tl_count] := '';
      state := B_TL;
    end;

    if state=B_TL then begin
      if body.tl_count<0 then
        body.tl_base := ln
      else
        body.tls[body.tl_count] := ln;
      state := B_EXAMPLES;
    end else
    if state=B_EXAMPLES then begin
      //skip for now!
    end;

  until (not ReadLine(ln)) or (ln='');
  Inc(body.tl_count);
end;

procedure PrintEdictBody(const w_head: string; const body: TEntryBody; const mark: TEntryWordMarkers);
var i: integer;
  s_pre, s_post: string;
begin
  if mark.markers<>'' then
    s_pre := w_head + '(' + mark.markers + ') '
  else
    s_pre := w_head;
  if mark.pop then
    s_post := s_post + '/(P)/'
  else
    s_post := '/';
  if body.tl_count<=0 then begin
    outp.WriteLine(s_pre + RemoveFormatting(body.tl_base) + s_post);
    Inc(stats.addcnt);
  end else
  for i := 0 to body.tl_count-1 do begin
    outp.WriteLine(s_pre + RemoveFormatting(body.tls[i]) + s_post);
    Inc(stats.addcnt);
  end;
end;

function ReadArticle: boolean;
var ln: string;
  hdr: TEntryHeader;
  body: TEntryBody;
  i, j: integer;
  w_head: string;
 {$IFDEF ENMARKERS}
  mark: TEntryMarkers;
 {$ENDIF}
begin
  while ReadLine(ln) and (ln='') do begin end;
  if ln='' then begin //couldn't read another line then
    Result := false;
    exit;
  end;

  Inc(stats.artcnt);
  try
    DecodeEntryHeader(ln, hdr);
    ReadBody(body);
  except
    on E: EParsingException do begin
      writeln('Line '+IntToStr(stats.lines) + ' article '+IntToStr(stats.artcnt)+': '
        +E.Message);
      SkipArticle;
      Inc(stats.badcnt);
      Result := true;
      exit;
    end;
  end;

 //Clean up a bit
  for i := 0 to hdr.words_used - 1 do begin
    DropVariantIndicator(hdr.words[i].s_reading);
    for j := 0 to hdr.words[i].s_kanji_used-1 do
      DropVariantIndicator(hdr.words[i].s_kanji[j]);
  end;
  TrimEndPunctuation(body.tl_base);
  for i := 0 to body.tl_count - 1 do
    TrimEndPunctuation(body.tls[i]);

 {$IFDEF ENMARKERS}
 //Query markers from english edict
  FillMarkers(hdr, mark);
 {$ENDIF}

 //Add to edict
  for i := 0 to hdr.words_used - 1 do
    if hdr.words[i].s_kanji_used<=0 then begin
      w_head := hdr.words[i].s_reading + ' /';
      PrintEdictBody(w_head, body, mark[i]);
    end else
    for j := 0 to hdr.words[i].s_kanji_used-1 do begin
      w_head := hdr.words[i].s_kanji[j] + ' ['+hdr.words[i].s_reading+'] /';
      PrintEdictBody(w_head, body, mark[i]);
    end;

  Result := true;
end;

procedure Run;
begin
  inp := TCharReader.Create(TFileStream.Create(InputFile, fmOpenRead), true);
  outp := TCharWriter.Create(TFileStream.Create(OutputFile, fmCreate), csUtf16LE, true);
  LoadEdict('edict.dic');
  try
    outp.WriteBom;
    FillChar(stats, SizeOf(stats), 0);

    ReadHeader();
    while ReadArticle() do begin
      if stats.artcnt mod 1000 = 0 then
        writeln(IntToStr(stats.artcnt));

    end;
    writeln('Lines: '+IntToStr(stats.lines));
    writeln('Articles: '+IntToStr(stats.artcnt));
    writeln('Bad articles: '+IntToStr(stats.badcnt));
    writeln('Added articles (including examples): '+IntToStr(stats.addcnt));
    writeln('Wtf: '+IntToStr(stats.wtf));

  finally
    FreeEdict();
    FreeAndNil(outp);
    FreeAndNil(inp);
  end;
end;

begin
  if ParamCount=0 then begin
    PrintUsage;
    exit;
  end;

  try
    ParseCommandLine;
    Run;
    writeln('Done');
    readln;
  except
    on E: EBadUsage do begin
      writeln('Bad usage. ');
      writeln('  '+E.Message);
      PrintUsage;
    end;
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
