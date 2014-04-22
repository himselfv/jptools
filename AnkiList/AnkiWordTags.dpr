program AnkiWordTags;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils, ConsoleToolbox, JWBIO;

type
  TAnkiWordTags = class(TCommandLineApp)
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

{ area jlpt2 }

procedure TAnkiWordTags.ShowUsage;
begin
  writeln('Takes a bunch of text files listing japanese words and generates '
    +'Anki-compatible tag lists for each word.');
  writeln('Usage: '+ProgramName+' <file1> [file2] ... [-flags]');
  writeln('Flags:');
  writeln('  -o output.file    specify output file (otherwise console)');
  writeln('  -e examples.file  specify examples file (otherwise EXAMPLES)');
  writeln('  -tn column        zero-based. Take expressions from this column, if tab-separated');
end;

function TAnkiWordTags.HandleSwitch(const s: string; var i: integer): boolean;
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

function TAnkiWordTags.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;



begin
  RunApp(TAnkiWordTags);
end.
