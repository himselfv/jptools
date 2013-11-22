program MiscTxt;
{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Windows, ConsoleToolbox, UniStrUtils, StreamUtils, JWBIO;

type
  TMiscTxt = class(TCommandLineApp)
  protected
    Command: UniString;
    InputFile: UniString;
    OutputFile: UniString;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure Run_Stats(inp: TStreamDecoder);
    procedure Run_Trim(inp: TStreamDecoder; outp: TStreamEncoder);
  end;

procedure TMiscTxt.ShowUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<command> <filename> <output file>');
  writeln('If no output specified, console will be used.');
  writeln('Supported commands:');
  writeln('  trim = removes empty lines');
  writeln('  stats = output file stats');
end;

function TMiscTxt.HandleParam(const s: string; var i: integer): boolean;
begin
  Result := true;
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
end;

procedure TMiscTxt.Run;
var inp: TStreamDecoder;
  outp: TStreamEncoder;
begin
  if Command='' then BadUsage;
  if InputFile='' then
    BadUsage('You have to specify an input file');

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

procedure TMiscTxt.Run_Stats(inp: TStreamDecoder);
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

procedure TMiscTxt.Run_Trim(inp: TStreamDecoder; outp: TStreamEncoder);
var ln: string;
begin
  while inp.ReadLn(ln) do begin
    if ln<>'' then
      outp.WriteLn(ln);
  end;
end;

begin
  RunApp(TMiscTxt);
end.

