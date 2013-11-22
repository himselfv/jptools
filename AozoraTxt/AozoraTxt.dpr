program AozoraTxt;
{$APPTYPE CONSOLE}


uses
  SysUtils, Classes, Windows, ConsoleToolbox, UniStrUtils, StreamUtils,
  AozoraParser;

type
  TAozoraTxt = class(TCommandLineApp)
  protected
    Command: UniString;
    InputFile: UniString;
    OutputFile: UniString; //empty means console
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure Run_Stats(InputFile: string);
    procedure Run_Strip(InputFile, OutputFile: string);
  end;

procedure TAozoraTxt.ShowUsage;
begin
  writeln('Usage: ');
  writeln('  '+ExtractFileName(paramstr(0))+'<command> <filename> <output file>');
  writeln('If no output specified, console will be used.');
  writeln('Supported commands:');
  writeln('  stats = output aozora tags stats');
  writeln('  strip = delete all common aozora tags');
end;

function TAozoraTxt.HandleParam(const s: string; var i: integer): boolean;
begin
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
  Result := true;
end;

procedure TAozoraTxt.Run;
begin
  if Command='' then BadUsage();
  if InputFile='' then BadUsage('Input file required');
  if SameStr(Command, 'stats') then
    Run_Stats(InputFile)
  else
  if SameStr(Command, 'strip') then
    Run_Strip(InputFile, OutputFile)
  else
    BadUsage('Unrecognized command: '+Command);
end;

procedure TAozoraTxt.Run_Stats(InputFile: string);
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

procedure TAozoraTxt.Run_Strip(InputFile, OutputFile: string);
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

begin
  RunApp(TAozoraTxt);
end.
