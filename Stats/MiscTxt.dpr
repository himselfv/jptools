program MiscTxt;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows, UniStrUtils, StreamUtils, JWBIO;

type
  EBadUsage = class(Exception)
  end;

procedure BadUsage(msg: UniString);
begin
  raise EBadUsage.Create(msg);
end;

procedure PrintUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<command> <filename> <output file>');
  writeln('If no output specified, console will be used.');
  writeln('Supported commands:');
  writeln('  trim = removes empty lines');
  writeln('  stats = output file stats');
end;

var
  Command: UniString;
  InputFile: UniString;
  OutputFile: UniString; {пустой - значит, консоль}

procedure ParseCommandLine;
var i: integer;
  s: UniString;
begin
  Command := '';
  InputFile := '';
  OutputFile := '';

  i := 1;
  while i <= ParamCount do begin
    s := ParamStr(i);
    if Length(s)=0 then begin
      Inc(i);
      continue;
    end;
    if s[1]<>'-' then begin
      if Command='' then
        Command := s
      else
      if InputFile='' then
        InputFile := s
      else
      if OutputFile='' then
        OutputFile := s
      else
        BadUsage('Too many params.');
      Inc(i);
      continue;
    end;

    BadUsage('Unrecognized switch: '+s);
  end;

  if Command='' then
    BadUsage('You have to specify a command');
  if InputFile='' then
    BadUsage('You have to specify an input file');

  if SameStr(Command, 'stats') then
  else
  if SameStr(Command, 'trim') then
  else
    BadUsage('Unrecognized command: '+Command);

end;

procedure Run_Stats(inp: TStreamDecoder);
var c: char;
  CharCnt: integer;
  WhiteSpaceCnt: integer;
  KanjiCnt: integer;
  KanaCnt: integer;
  LineCnt: integer;
  AfterCR: boolean;
begin
  CharCnt := 0;
  WhiteSpaceCnt := 0;
  KanjiCnt := 0;
  KanaCnt := 0;
  LineCnt := 0;
  AfterCR := false;

  while inp.ReadChar(c) do begin

   //Считаем строки
    if c=#13 then begin
      Inc(LineCnt);
      AfterCR := true;
    end else
    if (c=#10) and not AfterCR then begin
      Inc(LineCnt);
    end;

   //Считаем символы
    if (c<>#13) and (c<>#10) and (c<>#09) then
      Inc(CharCnt);
   //Пробелы (бывают разные)
    if (c=' ') or (c='　') then
      Inc(WhiteSpaceCnt);
    if IsKanji(c) then
      Inc(KanjiCnt);
    if IsKana(c) then
      Inc(KanaCnt);

  end;

 //Output stats
  writeln('Char count: '+IntToStr(CharCnt));
  writeln('Non-whitespace char count: '+IntToStr(CharCnt-WhiteSpaceCnt));
  writeln('Kanji count: '+IntToStr(KanjiCnt));
  writeln('Kana count: '+IntToStr(KanaCnt));
  writeln('Line count: '+IntToStr(LineCnt));
  writeln('Rest char count: '+IntToStr(CharCnt-WhiteSpaceCnt-KanjiCnt-KanaCnt));
end;

procedure Run_Trim(inp: TStreamDecoder; outp: TStreamEncoder);
var ln: string;
begin
  while inp.ReadLn(ln) do begin
    if ln<>'' then
      outp.WriteLn(ln);
  end;
end;

//Settings have been loaded already
procedure Run;
var inp: TStreamDecoder;
  outp: TStreamEncoder;
begin
  inp := OpenTextFile(InputFile);
  outp := nil;

  if OutputFile<>'' then begin
    outp := CreateTextFile(OutputFile, TUTF16Encoding);
    outp.WriteBom;
  end else
    outp := ConsoleWriter();

  try
    if SameStr(Command, 'stats') then
      Run_Stats(inp)
    else
    if SameStr(Command, 'trim') then
      Run_Trim(inp, outp)
    else
      BadUsage('Unrecognized command: '+Command);
  finally
    FreeAndNil(inp);
    FreeAndNil(outp);
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

