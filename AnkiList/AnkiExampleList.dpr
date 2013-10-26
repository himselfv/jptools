program AnkiExampleList;
{$APPTYPE CONSOLE}
{
 Similar to a WordList; parses a list of words or expressions and finds
 appropriate examples for each in the Tanaka corpus (or a compatible file).
 Examples are printed in a tab-separated format, wrapped in HTML:
   expression        <span>Sentence</span><span>Sentence</span>
}

uses SysUtils, Classes, ConsoleToolbox, JWBIO;

type
  TAnkiExampleList = class(TCommandLineApp)
  protected
    Files: array of string;
    ExamplesFile: string;
    OutputFile: string;
    TabNumber: integer;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
//    procedure Run; override;
//    procedure ParseFile(const AFilename: string; outp: TStreamEncoder);
  end;

procedure TAnkiExampleList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -e examples.file  specify examples file (otherwise EXAMPLES)');
  writeln('  -tn column        zero-based. Take expressions from this column, if tab-separated');
end;

function TAnkiExampleList.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-o' then begin
    if i>=ParamCount then BadUsage('-o requires file name');
    Inc(i);
    OutputFile := ParamStr(i);
    Result := true;
  end else
  if s='-e' then begin
    if i>=ParamCount then BadUsage('-e needs examples file name');
    Inc(i);
    ExamplesFile := ParamStr(i);
    Result := true
  end else
  if s='-tn' then begin
    if i>=ParamCount then BadUsage('-tn needs column number');
    Inc(i);
    TabNumber := StrToInt(ParamStr(i));
    Result := true
  end else
    Result := inherited;
end;

function TAnkiExampleList.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;



begin
  RunApp(TAnkiExampleList);
end.
