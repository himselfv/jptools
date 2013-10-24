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
  SysUtils, JWBIO, JWBEdictReader;

var
  Files: array of string;
  EdictFilename: string = '';

procedure ParseCommandLine;
var i: integer;
  s: string;
begin
  SetLength(Files, 0);

  i := 1;
  while i<=ParamCount do begin
    s := Trim(AnsiLowerCase(ParamStr(i)));
    if s='' then continue;

    if s[1]='-' then begin
      if s='-e' then begin
        Inc(i);
        if i>ParamCount then
          raise Exception.Create('-e needs dictionary name');
      end else
        raise Exception.Create('Invalid parameter: '+s);
    end else begin
      SetLength(Files, Length(Files)+1);
      Files[Length(Files)-1] := s;
    end;

    Inc(i);
  end;

  if Length(Files)<=0 then
    raise Exception.Create('Specify input files.');
end;

//inp is destroyed on exit
procedure ParseFile(inp: TStreamDecoder; outp: TStreamEncoder);
var ed: TEdictArticle;
  ln: string;
begin
  while inp.ReadLn(ln) do begin
    ParseEdict2Line(ln, ed);


  end;
  FreeAndNil(inp);
end;

procedure Run;
var outp: TStreamEncoder;
  i: integer;
begin
  outp := ConsoleWriter;

  for i := 0 to Length(Files)-1 do
    ParseFile(OpenTextFile(files[i]), outp);

  FreeAndNil(outp);
end;


begin
  try
    if ParamCount<=0 then begin
      writeln('Usage: '+ExtractFilename(Paramstr(0))+' <filename> [-e EDICT]');
      exit;
    end;
    ParseCommandLine;
    Run;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
