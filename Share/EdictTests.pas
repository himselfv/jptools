unit EdictTests;

interface
uses TestFramework;

type
  TEdictTests = class(TTestCase)
  published
    procedure EdictRead;
    procedure EdictLoad;
  end;

implementation
uses SysUtils, Classes, JWBIO, EdictReader, Edict;

procedure TEdictTests.EdictRead;
var AInput: TStreamDecoder;
  ed: TEdictArticle;
  ln: string;
begin
  AInput := OpenTextFile('EDICT2');
  AInput.Rewind();
  while AInput.ReadLn(ln) do begin
    ln := Trim(ln);
    if ln='' then continue;
    ParseEdict2Line(ln, @ed);
  end;
end;

procedure TEdictTests.EdictLoad;
var Edict: TEdict;
begin
  Edict := TEdict.Create;
  Edict.LoadFromFile('EDICT2');
  FreeAndNil(Edict);
end;

initialization
  RegisterTest(TEdictTests.Suite);

end.