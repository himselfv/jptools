unit TestingCommon;

interface
uses TestFramework;

var
  TestCasesDir: string = ''; //all tests must keep subfolders for data
  SpeedTests: TTestSuite;    //register any performance tests here. Nil => do not register

function CommonDataDir: string; inline; //where stuff like dictionaries and resources is
procedure RegisterSpeedTest(Test: ITest);

type
  //Usage: RegisterTest(TNamedTestSuite.Create(AClass))
  TNamedTestSuite = class(TTestSuite)
  public
    constructor Create(const AName: string; AClass: TTestCaseClass);
  end;

type
  TStringArray = array of string;

function FileList(const APath, AMask: string): TStringArray;

implementation
uses SysUtils;

function CommonDataDir: string;
begin
  Result := ExtractFilePath(ParamStr(0)); //for now
end;

procedure RegisterSpeedTest(Test: ITest);
begin
  if SpeedTests <> nil then
    SpeedTests.AddTest(Test);
end;

constructor TNamedTestSuite.Create(const AName: string; AClass: TTestCaseClass);
begin
  inherited Create(AClass);
  Self.FTestName := AName;
end;

function FileList(const APath, AMask: string): TStringArray;
var sr: TSearchRec;
  res: integer;
begin
  SetLength(Result, 0);
  res := FindFirst(APath+'\'+AMask, faAnyFile and not faDirectory, sr);
  while res = 0 do begin
    SetLength(Result, Length(Result)+1);
    Result[Length(Result)-1] := APath+'\'+sr.Name;
    res := FindNext(sr);
  end;
  SysUtils.FindClose(sr);
end;


initialization
  //To change this in time before tests start using it, set in the initialization section of a unit
  //sufficiently high in the project's uses list.
  TestCasesDir := ExtractFilePath(ParamStr(0))+'\Tests';

  if not FindCmdLineSwitch('nospeed') then begin
    SpeedTests := TTestSuite.Create('Speed Tests');
    RegisterTest(SpeedTests);
  end;

end.
