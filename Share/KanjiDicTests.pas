unit KanjiDicTests;
// Requires KANJIDIC in the same folder

interface
uses TestFramework;

type
  TKanjidicTests = class(TTestCase)
  published
    procedure KanjidicRead;
    procedure KanjidicLoad;
  end;

  TKanjidicSpeedTests = class(TTestCase)
  public
    FReader: TKanjidicTests;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure KanjidicReadTime;
    procedure KanjidicLoadTime;
  end;


implementation
uses SysUtils, Classes, Windows, TestingCommon, JWBIO, KanjidicReader, Kanjidic;

procedure TKanjidicTests.KanjidicRead;
var AInput: TStreamDecoder;
  ed: TKanjiDicEntry;
  ln: string;
begin
  AInput := OpenTextFile(CommonDataDir+'\kanjidic');
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
  Kanjidic.LoadFromFile(CommonDataDir+'\kanjidic');
  FreeAndNil(Kanjidic);
end;


const
  KANJIDIC_ITER_CNT: integer = 20;

procedure TKanjidicSpeedTests.SetUp;
begin
  FReader := TKanjidicTests.Create('');
end;

procedure TKanjidicSpeedTests.TearDown;
begin
  FreeAndNil(FReader);
end;

procedure TKanjidicSpeedTests.KanjidicReadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to KANJIDIC_ITER_CNT-1 do
    FReader.KanjidicRead();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(KANJIDIC_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

procedure TKanjidicSpeedTests.KanjidicLoadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to KANJIDIC_ITER_CNT-1 do
    FReader.KanjidicLoad();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(KANJIDIC_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;


initialization
  RegisterTest(TNamedTestSuite.Create('Kanjidic', TKanjiDicTests));
  RegisterSpeedTest(TNamedTestSuite.Create('Kanjidic', TKanjidicSpeedTests));

end.