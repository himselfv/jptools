unit KanaConvTests;
{ Requires Hepburn.roma, Kiriji - Polivanov.roma, PinYin.rpy.
 JWBIO must be tested beforehand. }

interface
uses SysUtils, Classes, TestFramework, JWBIO, JWBStrings, KanaConv;

type
  TConvTableTest = class(TTestCase)
  protected
    FTableNames: TStringArray;
    FConv: TRomajiTranslator;
  public
    constructor Create(const ATableNames: array of string); reintroduce;
    function GetName: string; override;
    function TableDir: string;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RunTestFile(const AFilename: string);
  published
    procedure RunTests;
  end;
  TKanaTableTest = class(TConvTableTest)
  public
    procedure SetUp; override;
  end;
  TBopomofoTableTest = class(TConvTableTest)
  public
    procedure SetUp; override;
  end;

  TConvTableSuite = class(TTestSuite)
  protected
    FTableNames: TStringArray;
  public
    constructor Create(TestClass: TTestCaseClass; ATableNames: array of string);
    procedure AddTest(ATest: ITest); override;
  end;

implementation
uses TestingCommon;

constructor TConvTableTest.Create(const ATableNames: array of string);
var i: integer;
begin
  inherited Create('RunTests');
  SetLength(FTableNames, Length(ATableNames));
  for i := 0 to Length(ATableNames)-1 do
    FTableNames[i] := ATableNames[Low(ATableNames) + i];
end;

function TConvTableTest.GetName: string;
var i: integer;
begin
  Result := '';
  for i := 0 to Length(FTableNames)-1 do
    Result := Result + ExtractFilename(FTableNames[i]) + ', ';
  if Result <> '' then
    SetLength(Result, Length(Result)-2)
  else
    Result := '[ empty ]';
end;

function TConvTableTest.TableDir: string;
begin
  Result := AppFolder;
end;

procedure TConvTableTest.Setup;
var i: integer;
begin
 //FConv must be created in descendants
  for i := 0 to Length(FTableNames)-1 do
    FConv.LoadFromFile(TableDir+'\'+ExtractFilename(FTableNames[i]));
end;

procedure TConvTableTest.Teardown;
begin
  FreeAndNil(FConv);
end;

procedure TConvTableTest.RunTests;
var i: integer;
begin
  for i := 0 to Length(FTableNames)-1 do
    RunTestFile(FTableNames[i]+'.txt');
end;

procedure TConvTableTest.RunTestFile(const AFilename: string);
var inp: TStreamDecoder;
  ln: string;
  i_ln: integer;
  i_pos: integer;
  lp,rp: string;
  cmp: string;
begin
    inp := OpenTextFile(AFilename);
    try
      i_ln := -1;
      while inp.ReadLn(ln) do begin
        Inc(i_ln);
        ln := Trim(ln);
        if (ln='') or (ln[1]='#') then continue;

        i_pos := pos('=',ln);
        Assert(i_pos>0, 'Invalid test case file: '+AFilename+'. '
          +'Error at line '+IntToStr(i_ln)+': no "=".');

        lp := copy(ln,1,i_pos-1);
        rp := copy(ln,i_pos+1,MaxInt);
        Assert(Length(lp)>0, 'Invalid test case file: '+AFilename+'. '
          +'Error at line '+IntToStr(i_ln)+': empty left part.');

        if EvalChar(lp[1]) in [EC_KATAKANA, EC_HIRAGANA, EC_BOPOMOFO] then
          cmp := FConv.KanaToRomaji(lp,[])
        else
          cmp := ConvertBopomofo(FConv.RomajiToKana(lp,[]));

        Check(SameText(rp, cmp),
          'Error in '+AFilename+'@'+IntToStr(i_ln)+': '+lp+' -> '+cmp+', '
            +'expected '+rp+'.');
      end;
    finally
      FreeAndNil(inp);
    end;
end;

procedure TKanaTableTest.Setup;
begin
  FConv := TKanaTranslator.Create;
  inherited;
end;

procedure TBopomofoTableTest.Setup;
begin
  FConv := TPinyinTranslator.Create;
  inherited;
end;


constructor TConvTableSuite.Create(TestClass: TTestCaseClass;
  ATableNames: array of string);
var i: integer;
  ATableList: string;
begin
  SetLength(FTableNames, Length(ATableNames));
  ATableList := '[';
  for i := 0 to Length(ATableNames)-1 do begin
    FTableNames[i] := ATableNames[Low(ATableNames) + i];
    ATableList := ATableList + FTableNames[i] + ', ';
  end;
  SetLength(ATableList, Length(ATableList)-2);
  ATableList := ATableList + ']';
  inherited Create(TestClass.ClassName+' '+ATableList);
  AddTests(testClass);
end;

procedure TConvTableSuite.AddTest(ATest: ITest);
begin
  inherited;
 //Parametrize
  if ATest is TConvTableTest then
    (ATest as TConvTableTest).FTableNames := FTableNames;
end;


function KanaConvTestSuite: ITestSuite;
var ASuite: TTestSuite;
  fname, rawName: string;
begin
  ASuite := TTestSuite.Create('KanaConv');
  // Load all .txt files from test case dir
  for fname in FileList(TestCasesDir+'\kanaconv', '*.txt') do begin
    rawName := ChangeFileExt(fname, ''); //strip .txt
    if ExtractFileExt(rawName)='.rpy' then
      ASuite.AddTest(TBopomofoTableTest.Create([rawName]))
    else
      ASuite.AddTest(TKanaTableTest.Create([rawName]))
  end;
  Result := ASuite;
end;

initialization
  RegisterTest(KanaConvTestSuite);

end.
