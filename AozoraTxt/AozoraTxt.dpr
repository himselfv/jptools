program AozoraTxt;
{$APPTYPE CONSOLE}


uses
  SysUtils,
  Classes,
  Windows,
  UniStrUtils,
  StreamUtils,
  AozoraParser;

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
  writeln('  stats = output aozora tags stats');
  writeln('  strip = delete all common aozora tags');
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
  if SameStr(Command, 'strip') then
  else
    BadUsage('Unrecognized command: '+Command);

end;

procedure Run_Stats(InputFile: string);
var p: TAozoraStatParser;
begin
  p := TAozoraStatParser.Create;
  p.Parse(TFileStream.Create(InputFile, fmOpenRead));

 //Output stats
  writeln(InputFile + ' parsing results: ');
  writeln('Ruby count: '+IntToStr(p.Stats.RubyCount));
  writeln('Ruby len: '+IntToStr(p.Stats.RubySz));
  writeln('Tag count: '+IntToStr(p.Stats.TagCount));
  writeln('Tag len: '+IntToStr(p.Stats.TagSz));
  writeln('Comment count: '+IntToStr(p.Stats.CommentCount));
  writeln('Comment sz: '+IntToStr(p.Stats.CommentSz));
end;

procedure Run_Strip(InputFile, OutputFile: string);
var p: TAozoraStripParser;
  outp: TStream;
  Bom: AnsiString;
begin
  p := TAozoraStripParser.Create;
  if OutputFile<>'' then begin
    outp := TFileStream.Create(OutputFile, fmCreate);
    Bom := BOM_UTF16LE;
    outp.Write(Bom[1], Length(Bom));
  end else
    outp := THandleStream.Create(GetStdHandle(STD_OUTPUT_HANDLE));
  p.Parse(TFileStream.Create(InputFile, fmOpenRead), outp);
end;

//Settings have been loaded already
procedure Run;
begin
  if SameStr(Command, 'stats') then
    Run_Stats(InputFile)
  else
  if SameStr(Command, 'strip') then
    Run_Strip(InputFile, OutputFile)
  else
    BadUsage('Unrecognized command: '+Command);
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
