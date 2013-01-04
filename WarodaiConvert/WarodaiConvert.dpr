program WarodaiConvert;
{$APPTYPE CONSOLE}

//Look into english EDICT for markers to known words. Slow.
{$DEFINE ENMARKERS}

uses
  SysUtils,
  Classes,
  Windows,
  UniStrUtils,
  StreamUtils,
  Warodai in 'Warodai.pas',
  WakanDic in 'WakanDic.pas',
  WarodaiParticles in 'WarodaiParticles.pas',
  WarodaiHeader in 'WarodaiHeader.pas',
  WarodaiBody in 'WarodaiBody.pas';

{
Заметки по реализации.
1. Каны в полях "перевод" быть не должно. Из описания EDICT:
 > As the format restricts Japanese characters to the kanji and kana fields,
 > any cross-reference data and other informational fields are omitted.
 Значит, её надо переводить в ромадзи (опционально киридзи).

2. Первая "общая" строчка. Встречаются такие:
 > <i>неперех.</i>
 > (<i>англ.</i> advantage)
 Общие флаги и общий языковой источник

 > 1.
 Просто удалить

 > <i>уст.</i> 嗚呼
 > <i>уст.</i> 穴賢, 穴畏, 恐惶
 > : ～する
 > (<i>редко</i> 明く)
 > (…に, …と)
 > (を)

}

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
  writeln('  '+ExtractFileName(paramstr(0))+' <ewarodai.txt> <output file> [flags]');
  writeln('Flags:');
  writeln('  --with-tags <tag-dict> --- copies grammar tags from <tag-dict> where possible.');
end;

var
  InputFile: UnicodeString; //source Warodai file
  OutputFile: UnicodeString; //output EDICT file
  TagDictFile: UnicodeString; //reference dictionary file with tags (Wakan format)

procedure ParseCommandLine;
var i: integer;
 s: string;
begin
  InputFile := '';
  OutputFile := '';
  TagDictFile := '';

  i := 1;
  while i<=ParamCount() do begin
    s := Trim(ParamStr(i));
    if Length(s)<=0 then begin
      Inc(i);
      continue;
    end;

    if s='--with-tags' then begin
      Inc(i);
      if i>ParamCount() then
        BadUsage('--with-tags requires tag-dict filename');
      TagDictFile := ParamStr(i);
    end else
    if InputFile='' then
      InputFile := s
    else
    if OutputFile='' then
      OutputFile := s
    else
      BadUsage('Unknown parameter: "'+s+'"');

    Inc(i);
  end;

 //Check configuration
  if InputFile='' then
    BadUsage('Input file required.');
  if OutputFile='' then
    BadUsage('Output file required');
end;


var
  inp: TWarodaiReader;
  outp: TCharWriter;
  stats: record
    artcnt: integer;
    badcnt: integer;
    addcnt: integer;
    EmptyBlocks: integer;
  end;


procedure ReadHeader;
var s: string;
begin
  while inp.ReadLine(s) and (s<>'') do begin end;
end;


{
TODO:
  Print blocks together:
  /(num,pref) (1) one/(suf) (2) best in/the most (...) in (where an adjective follows)/
  Print groups as a separate entries.
}

procedure PrintEdictGroup(const s_pre: string; const common: string; const group: PEntryGroup; const s_post: string);
var i: integer;
  s: string;
  b_no: integer;
begin
  s := s_pre;
  if common<>'' then
    s := s + common + ' ';
  if group.common<>'' then
    s := s + group.common + ' ';
  b_no := 1;
  for i := 0 to group.block_cnt - 1 do
    if group.blocks[i].line_cnt>0 then begin
      if group.block_cnt>1 then
        s := s + '('+IntToStr(b_no)+') ';
      s := s + RemoveFormatting(group.blocks[i].lines[0])+'/'; //for now we only print line[0]
      Inc(b_no);
    end;
  s := s + s_post;
  outp.WriteLine(s);
  Inc(stats.addcnt);
end;

procedure PrintEdictBody(const w_head: string; const body: PEntryBody; const mark: TEntryWordMarkers);
var i: integer;
  s_pre, s_post: string;
begin
  if mark.markers<>'' then
    s_pre := w_head + '(' + mark.markers + ') '
  else
    s_pre := w_head;
  if mark.pop then
    s_post := s_post + '(P)/'
  else
    s_post := '';
  for i := 0 to body.group_cnt - 1 do
    PrintEdictGroup(s_pre, body.common, @body.groups[i], s_post);
end;

procedure TrimLine(var ln: string);
begin
  TrimEndPunctuation(ln);
  TrimStartColon(ln);
  ln := Trim(ln);
end;

var
  com: TCharWriter; //common meaning cases
 //Для ускорения храним по одной копии,
 //чтобы не создавать-удалять каждый раз.
  hdr: TEntryHeader;
  body: TEntryBody;

function ReadArticle: boolean;
var ln: string;
  i, j: integer;
  w_head: string;
 {$IFDEF ENMARKERS}
  mark: TEntryMarkers;
 {$ENDIF}
begin
  while inp.ReadLine(ln) and (ln='') do begin end;
  if ln='' then begin //couldn't read another line then
    Result := false;
    exit;
  end;

  Inc(stats.artcnt);
  try
    DecodeEntryHeader(ln, @hdr);
    ReadBody(inp, @body);
  except
    on E: EParsingException do begin
      writeln('Line '+IntToStr(WarodaiStats.LinesRead)
        + ' article '+IntToStr(stats.artcnt)+': '
        +E.Message);
      inp.SkipArticle;
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
  TrimLine(body.common);
  for i := 0 to body.group_cnt - 1 do begin
    TrimLine(body.groups[i].common);
    for j := 0 to body.groups[i].block_cnt - 1 do begin
      if body.groups[i].blocks[j].line_cnt<=0 then
        Inc(stats.EmptyBlocks) //bad!
      else
        TrimLine(body.groups[i].blocks[j].lines[0]); //for now only lines[0]
    end;
  end;

 {$IFDEF ENMARKERS}
 //Query markers from english edict
  FillMarkers(hdr, mark);
 {$ENDIF}

 //Add to edict
  for i := 0 to hdr.words_used - 1 do
    if hdr.words[i].s_kanji_used<=0 then begin
      w_head := hdr.words[i].s_reading + ' /';
      PrintEdictBody(w_head, @body, mark[i]);
    end else
    for j := 0 to hdr.words[i].s_kanji_used-1 do begin
      w_head := hdr.words[i].s_kanji[j] + ' ['+hdr.words[i].s_reading+'] /';
      PrintEdictBody(w_head, @body, mark[i]);
    end;

  Result := true;
end;


procedure Run;
var tm: cardinal;
begin
  inp := TWarodaiReader.Create(TFileStream.Create(InputFile, fmOpenRead), true);
  outp := TCharWriter.Create(TFileStream.Create(OutputFile, fmCreate), csUtf16LE, true);
  com := TCharWriter.Create(TFileStream.Create('commng.txt', fmCreate), csUtf16LE, true);
  if TagDictFile <> '' then
  try
    LoadEdict(TagDictFile);
  except
    on E: Exception do  begin
      E.Message := 'While loading tag dictionary "'+TagDictFile+'": '+E.Message;
      raise;
    end;
  end;
  tm := GetTickCount;
  try
    outp.WriteBom;
    com.WriteBom;
    FillChar(stats, SizeOf(stats), 0);

    ReadHeader();
    while ReadArticle() do begin
      if stats.artcnt mod 1000 = 0 then
        writeln(IntToStr(stats.artcnt));

    end;
    writeln('');
    writeln('Done.');
    writeln('Parsing took '+IntToStr(GetTickCount()-tm)+' msec.');
    writeln('');
    writeln('Lines: '+IntToStr(WarodaiStats.LinesRead));
    writeln('Articles: '+IntToStr(stats.artcnt));
    writeln('Bad articles: '+IntToStr(stats.badcnt));
    writeln('Added articles (including examples): '+IntToStr(stats.addcnt));
    writeln('Comments: '+IntToStr(WarodaiStats.Comments));
    writeln('Data lines: '+IntToStr(WarodaiStats.DataLines));

    writeln('Group common: '+IntToStr(WarodaiStats.GroupCommon));
    writeln('Block common: '+IntToStr(WarodaiStats.BlockCommon));

    writeln('EDICT tags -- match: '+IntToStr(edictStats.edictTagsFound));
    writeln('EDICT tags -- cloned: '+IntToStr(edictStats.edictTagsCloned));
    writeln('EDICT tags -- unsure: '+IntToStr(edictStats.edictTagsUnsure));

    writeln('WTF -- Lines too short: '+IntToStr(WarodaiStats.LinesTooShort));

    writeln('BAD -- Explicit common group blocks: '+IntToStr(WarodaiStats.ExplicitCommonGroupBlocks));
    writeln('BAD -- Multiline group common: '+IntToStr(WarodaiStats.MultilineGroupCommon));
    writeln('BAD -- Multiline block common: '+IntToStr(WarodaiStats.MultilineBlockCommon));
    writeln('BAD -- Empty blocks: '+IntToStr(stats.EmptyBlocks));

  finally
    FreeEdict();
    FreeAndNil(com);
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
