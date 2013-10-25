program AnkiWordList;
{$APPTYPE CONSOLE}
{
 Parses a word/expression list - one expression at a line, possibly
 tab-separated table with other unrelated fields.

 Builds a card list in a format suitable for importing to Anki:
   expression      translations

 Intended usage:
   1. Make a word list for studying either manually or by other means.
   2. Generate Anki cards with this tool.
   3. Import to Anki.

 To update existing deck with translations from another dictionary:
   1. Export the notes into tab-separated text file.
   2. Run this tool and generate another tab-separated file with translations.
   3. Create additional note field in Anki.
   4. Import new file, pasting translations into the new field.

 Requires any EDICT-compatible dictionary.
}
uses
  SysUtils, ConsoleToolbox, JWBIO, JWBEdictReader;

type
  TAnkiWordList = class(TCommandLineApp)
  protected
    Files: array of string;
    EdictFilename: string;
    function HandleSwitch(const s: string; var i: integer): boolean; override;
    function HandleParam(const s: string; var i: integer): boolean; override;
  public
    procedure ShowUsage; override;
    procedure Run; override;
    procedure ParseFile(inp: TStreamDecoder; outp: TStreamEncoder);
  end;

procedure TAnkiWordList.ShowUsage;
begin
  writeln('Usage: '+ProgramName+' <filename> [-e EDICT]');
end;

function TAnkiWordList.HandleSwitch(const s: string; var i: integer): boolean;
begin
  if s='-e' then begin
    Inc(i);
    if i>ParamCount then BadUsage('-e needs dictionary name');
    Result := true
  end else
    Result := inherited;
end;

function TAnkiWordList.HandleParam(const s: string; var i: integer): boolean;
begin
  SetLength(Files, Length(Files)+1);
  Files[Length(Files)-1] := s;
  Result := true;
end;

procedure TAnkiWordList.Run;
var outp: TStreamEncoder;
  i: integer;
begin
  if Length(Files)<=0 then BadUsage('Specify input files.');
  outp := ConsoleWriter;
  for i := 0 to Length(Files)-1 do
    ParseFile(OpenTextFile(files[i]), outp);
  FreeAndNil(outp);
end;

//inp is destroyed on exit
procedure TAnkiWordList.ParseFile(inp: TStreamDecoder; outp: TStreamEncoder);
var ed: TEdictArticle;
  ln: string;
begin
  while inp.ReadLn(ln) do begin
    ParseEdict2Line(ln, @ed);


  end;
  FreeAndNil(inp);
end;

begin
  RunApp(TAnkiWordList);
end.
