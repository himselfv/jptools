unit EdictTests;
// Requires EDICT in the same folder

interface
uses TestFramework;

type
  TEdictTests = class(TTestCase)
  published
    procedure EdictRead;
    procedure EdictLoad;
  end;

  TEdictSpeedTests = class(TTestCase)
  public
    FReader: TEdictTests;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure EdictReadTime;
    procedure EdictLoadTime;
  end;


implementation
uses SysUtils, Classes, Windows, TestingCommon, JWBIO, EdictReader, Edict;

procedure TEdictTests.EdictRead;
var AInput: TStreamDecoder;
  ed: TEdictArticle;
  ln: string;
begin
  AInput := OpenTextFile(CommonDataDir+'\EDICT2');
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
  Edict.LoadFromFile(CommonDataDir+'\EDICT2');
  FreeAndNil(Edict);
end;


const
  EDICT_ITER_CNT: integer = 1;

procedure TEdictSpeedTests.SetUp;
begin
  FReader := TEdictTests.Create('');
end;

procedure TEdictSpeedTests.TearDown;
begin
  FreeAndNil(FReader);
end;

procedure TEdictSpeedTests.EdictReadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to EDICT_ITER_CNT-1 do
    FReader.EdictRead();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(EDICT_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

procedure TEdictSpeedTests.EdictLoadTime;
var i: integer;
  tm: cardinal;
begin
  tm := GetTickCount;
  for i := 0 to EDICT_ITER_CNT-1 do
    FReader.EdictLoad();
  tm := GetTickCount-tm;
  Status('x'+IntToStr(EDICT_ITER_CNT)+' = '+IntToStr(tm)+' ticks.');
end;

initialization
  RegisterTest(TNamedTestSuite.Create('Edict', TEdictTests));
  RegisterSpeedTest(TNamedTestSuite.Create('Edict', TEdictSpeedTests));

end.