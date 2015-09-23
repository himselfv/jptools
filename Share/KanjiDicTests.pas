unit KanjiDicTests;

interface
uses TestFramework;

type
  TKanjidicTests = class(TTestCase)
  published
    procedure KanjidicRead;
    procedure KanjidicLoad;
  end;

implementation
uses SysUtils, Classes, JWBIO, KanjidicReader, Kanjidic;

procedure TKanjidicTests.KanjidicRead;
var AInput: TStreamDecoder;
  ed: TKanjiDicEntry;
  ln: string;
begin
  AInput := OpenTextFile('kanjidic');
  AInput.Rewind();
  while AInput.ReadLn(ln) do begin
    ln := Trim(ln);
    if ln='' then continue;
    if IsKanjidicComment(ln) then
      continue;
    ParseKanjiDicLine(ln, @ed);
  end;
end;

procedure TKanjidicTests.KanjidicLoad;
var Kanjidic: TKanjidic;
begin
  Kanjidic := TKanjidic.Create;
  Kanjidic.LoadFromFile('kanjidic');
  FreeAndNil(Kanjidic);
end;

initialization
  RegisterTest(TKanjiDicTests.Suite);

end.