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
  WarodaiMarkers in 'WarodaiMarkers.pas',
  WarodaiHeader in 'WarodaiHeader.pas',
  WarodaiBody in 'WarodaiBody.pas',
  WarodaiTemplates in 'WarodaiTemplates.pas',
  EdictWriter in 'EdictWriter.pas',
  WcUtils in 'WcUtils.pas',
  WcExceptions in 'WcExceptions.pas';

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
  outp: TArticleWriter;
  stats: record
    artcnt: integer;
    badcnt: integer;
    TlLines: integer;
    VarLines: integer;
    KanaLines: integer;
    KanjiLines: integer;

    SeveralTlLines: integer; //block has several basic translation lines. Not a normal case.
    MixedTlLines: integer; //block has several lines + they are intermixed with other types of lines
  end;


procedure ReadHeader;
var s: string;
begin
  while inp.ReadLine(s) and (s<>'') do begin end;
end;

procedure SetLineTypes(block: PEntryBlock);
var i,ev: integer;
  tl_lines: integer;
  last_tl: boolean;
  mixed_tl: boolean;
begin
  last_tl := true;
  mixed_tl := false;
  tl_lines := 0;
  for i := 0 to block.line_cnt - 1 do begin
    block.lines[i] := RemoveFormatting(block.lines[i]);

    ev := EvalChars(block.lines[i]);
    if ev=EV_KANA then begin
      Inc(stats.KanaLines);
      last_tl := false;
    end else
    if ev=EV_KANJI then begin
      Inc(stats.KanjiLines);
      last_tl := false;
    end else begin
      Inc(stats.TlLines);
      Inc(tl_lines);
      if not last_tl then
        mixed_tl := true;
      last_tl := false;
    end;

    if pos('～', block.lines[i])>0 then begin
      Inc(stats.VarLines);
      last_tl := false;
    end;

    if block.lines[i][Length(block.lines[i])]=':' then
      raise EColonAfterTl.Create('Colon after TL');
  end;

  if tl_lines>1 then
    Inc(stats.SeveralTlLines);
  if mixed_tl then
    Inc(stats.MixedTlLines);
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
    on E: ESilentParsingException do begin
      ExceptionStats.RegisterException(E);
      inp.SkipArticle;
      Inc(stats.badcnt);
      Result := true;
      exit;
    end;
    on E: EParsingException do begin
      ExceptionStats.RegisterException(E);
      writeln('Line '+IntToStr(WarodaiStats.LinesRead)
        + ' article '+IntToStr(stats.artcnt)+': '
        +E.Message);
      inp.SkipArticle;
      Inc(stats.badcnt);
      Result := true;
      exit;
    end;
  end;

  try
   //Clean up a bit
    for i := 0 to hdr.words_used - 1 do begin
      DropVariantIndicator(hdr.words[i].s_reading);
      for j := 0 to hdr.words[i].s_kanji_used-1 do
        DropVariantIndicator(hdr.words[i].s_kanji[j]);
    end;

    for i := 0 to body.group_cnt - 1 do
      for j := 0 to body.groups[i].block_cnt - 1 do
        SetLineTypes(@body.groups[i].blocks[j]);

   {$IFDEF ENMARKERS}
   //Query markers from english edict
    FillMarkers(hdr, mark);
   {$ENDIF}

   //Add to edict
    outp.Print(@hdr, @body, mark);

  except
    on E: ESilentParsingException do begin
      ExceptionStats.RegisterException(E);
      Inc(stats.badcnt);
      Result := true;
      exit;
    end;
    on E: EParsingException do begin
      ExceptionStats.RegisterException(E);
      writeln('Line '+IntToStr(WarodaiStats.LinesRead)
        + ' article '+IntToStr(stats.artcnt)+': '
        +E.Message);
      Inc(stats.badcnt);
      Result := true;
      exit;
    end;
  end;

  Result := true;
end;


procedure Run;
var tm: cardinal;
begin
  ExceptionStats.Clear;
  inp := TWarodaiReader.Create(TFileStream.Create(InputFile, fmOpenRead), true);
  com := TCharWriter.Create(TFileStream.Create('commng.txt', fmCreate), csUtf16LE, true);
  outp := TEdict2Writer.Create(OutputFile);
  if TagDictFile <> '' then
  try
    LoadReferenceDic(TagDictFile);
  except
    on E: Exception do  begin
      E.Message := 'While loading tag dictionary "'+TagDictFile+'": '+E.Message;
      raise;
    end;
  end;
  tm := GetTickCount;
  try
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
    writeln('Added articles: '+IntToStr(outp.AddedRecords));
    writeln('');
    writeln('Comments: '+IntToStr(WarodaiStats.Comments));
    writeln('Data lines: '+IntToStr(WarodaiStats.DataLines));
    writeln('TL lines: '+IntToStr(stats.TlLines));
    writeln('Var lines: '+IntToStr(stats.VarLines));
    writeln('Kana lines: '+IntToStr(stats.KanaLines));
    writeln('Kanji lines: '+IntToStr(stats.KanjiLines));
    writeln('');
    writeln('EDICT tags -- match: '+IntToStr(refStats.edictTagsFound));
    writeln('EDICT tags -- cloned: '+IntToStr(refStats.edictTagsCloned));
    writeln('EDICT tags -- unsure: '+IntToStr(refStats.edictTagsUnsure));
    writeln('');

    writeln('Group-common cases: '+IntToStr(WarodaiStats.GroupCommon));
    writeln('Block-common cases: '+IntToStr(WarodaiStats.BlockCommon));
    writeln('Multitemplate cases: '+IntToStr(WarodaiStats.Multitemplates));
    writeln('');

    writeln('Group number fixed: '+IntToStr(WarodaiStats.GroupNumberFixed));
    writeln('WARN -- Group number roughly guessed: '+IntToStr(WarodaiStats.GroupNumberGuessed));
    writeln('BAD -- Group number missing: '+IntToStr(WarodaiStats.GroupNumberMissing));

    writeln('Block number fixed: '+IntToStr(WarodaiStats.BlockNumberFixed));
    writeln('WARN -- Block number roughly guessed: '+IntToStr(WarodaiStats.BlockNumberGuessed));
    writeln('BAD -- Block number missing: '+IntToStr(WarodaiStats.BlockNumberMissing));
    writeln('');

    writeln('WTF -- Lines too short: '+IntToStr(WarodaiStats.LinesTooShort));

    writeln('BAD -- Several TL lines: '+IntToStr(stats.SeveralTlLines));
    writeln('BAD -- Mixed TL lines: '+IntToStr(stats.MixedTlLines));
    writeln('');

    ExceptionStats.PrintStats;
    writeln('');

  finally
    FreeReferenceDic();
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
