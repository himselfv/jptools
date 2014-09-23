unit SpeedTests;
{ Requires kanjidic, EDICT2 in the same folder }

interface
uses SysUtils, Classes, TestFramework, JWBIO;

type
  TSpeedTests = class(TTestCase)
  protected
    procedure KanjidicRead;
    procedure EdictRead;
    procedure KanjidicLoad;
    procedure EdictLoad;
  published
    procedure KanjidicReadTime;
    procedure EdictReadTime;
    procedure KanjidicLoadTime;
    procedure EdictLoadTime;
  end;

implementation
uses Windows, KanjidicReader, Kanjidic, EdictReader, Edict;

const
  KANJIDIC_ITER_CNT: integer = 20;
  EDICT_ITER_CNT: integer = 1;

procedure TSpeedTests.KanjidicRead;
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

procedure TSpeedTests.EdictRead;
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

procedure TSpeedTests.KanjidicLoad;
var Kanjidic: TKanjidic;
begin
  Kanjidic := TKanjidic.Create;
  Kanjidic.LoadFromFile('kanjidic');
  FreeAndNil(Kanjidic);
end;

procedure TSpeedTests.EdictLoad;
var Edict: TEdict;
begin
  Edict := TEdict.Create;
  Edict.LoadFromFile('EDICT2');
  FreeAndNil(Edict);
end;

procedure TSpeedTests.KanjidicReadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to KANJIDIC_ITER_CNT-1 do
    KanjidicRead();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(KANJIDIC_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

procedure TSpeedTests.EdictReadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to EDICT_ITER_CNT-1 do
    EdictRead();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(EDICT_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

procedure TSpeedTests.KanjidicLoadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to KANJIDIC_ITER_CNT-1 do
    KanjidicLoad();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(KANJIDIC_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

procedure TSpeedTests.EdictLoadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to EDICT_ITER_CNT-1 do
    EdictLoad();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(EDICT_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

{
Will be needed when we have more than one suite:

function SpeedTestsSuite: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.create('Speed Tests');
  ASuite.addTest(TSpeedTests.Suite);
  Result := ASuite;
end;
}

initialization
  RegisterTest(TSpeedTests.Suite);

end.
