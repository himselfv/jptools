unit JWBIOTests;

interface
uses SysUtils, Classes, TestFramework, JWBIO;

type
 { Wakan supports reading and writing text in a range of encodings, with or
  without BOM. }
  TEncodingTestCase = class(TTestCase)
  public
    EncodingClass: CEncoding;
    TestFile: string;
    Lines: TStringList;
    UseBom: boolean;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure LoadFile(const AFilename: string; AEncoding: CEncoding);
    procedure SaveFile(const AFilename: string; AEncoding: CEncoding; AWriteBom: boolean);
    procedure VerifyContents(const AFilename: string; AEncoding: CEncoding); virtual;
    procedure LoadSaveCompare(const AFilename: string; AEncoding: CEncoding; ABom: boolean);
  published
    procedure VerifyText;
    procedure GuessEncoding;
    procedure SaveCompare;
  end;
  CEncodingTestCase = class of TEncodingTestCase;

  TAsciiEncodingTestCase = class(TEncodingTestCase)
  public
    procedure VerifyContents(const AFilename: string; AEncoding: CEncoding); override;
  end;

  TAcpEncodingTestCase = class(TEncodingTestCase)
  public
    procedure VerifyContents(const AFilename: string; AEncoding: CEncoding); override;
  end;

  TEncodingTestSuite = class(TTestSuite)
  public
    EncodingClass: CEncoding;
    TestFile: string;
    UseBom: boolean;
    constructor Create(AEncodingClass: CEncoding; AUseBom: boolean; ATestFile: string;
      ATestClass: CEncodingTestCase = nil);
    procedure AddTest(ATest: ITest); override;
    function GetName: string; override;
  end;


  TMiscEncodingTests = class(TTestCase)
  published
    procedure SurrogateLinefeed;
  end;


  TEncodingDetectionTest = class(TTestCase)
  public
    Filename: string;
    constructor Create(const AFilename: string); reintroduce;
    function GetName: string; override;
  published
    procedure DetectEncoding;
  end;


 { Writes ~11Mb through the encoder, outputs the time to Status().
  You need console test runner to see it. }
  TReadWriteSpeedTestCase = class(TTestCase)
  protected
    procedure TestSpeed(AEncoding: CEncoding);
  published
    procedure Ascii;
    procedure UTF8;
    procedure UTF16LE;
    procedure UTF16BE;
    procedure EUC;
    procedure ShiftJis;
    procedure Jis;
    procedure OldJis;
    procedure NECJis;
    procedure GB;
    procedure Big5;
    procedure Acp;

  end;

implementation
uses Windows, JWBStrings, TestingCommon;

procedure TEncodingTestCase.Setup;
begin
  inherited;
  Lines := TStringList.Create;
end;

procedure TEncodingTestCase.TearDown;
begin
  FreeAndNil(Lines);
  inherited;
end;

procedure TEncodingTestCase.LoadFile(const AFilename: string; AEncoding: CEncoding);
var conv: TStreamDecoder;
  ln: string;
begin
  if AEncoding=nil then
    Check(Conv_DetectType(AFilename, AEncoding) or (AEncoding<>nil),
      'Cannot guess file encoding.');

  Lines.Clear;
  conv := OpenTextFile(AFilename, AEncoding);
  try
    conv.TrySkipBom;
    while conv.ReadLn(ln) do
      Lines.Add(ln);
  finally
    FreeAndNil(conv);
  end;
end;

procedure TEncodingTestCase.SaveFile(const AFilename: string; AEncoding: CEncoding;
  AWriteBom: boolean);
var conv: TStreamEncoder;
  i: integer;
begin
  conv := CreateTextFile(AFilename, AEncoding);
  try
    if AWriteBom then
      conv.WriteBom();
    for i := 0 to Lines.Count-2 do
      conv.WriteLn(Lines[i]);
    if Lines.Count>0 then
      conv.Write(Lines[Lines.Count-1]); //last line always without CRLF
  finally
    FreeAndNil(conv);
  end;
end;

{ All unicode text files contain the same text, so we run the same series of tests
to verify that they were decoded properly.
Expand the text to include various corner cases. Do not cover Ruby or any extended
parsing here. }
procedure TEncodingTestCase.VerifyContents(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=3);
  Check(Lines[0].StartsWith('世間体を気にするのは'));
  Check(Lines[2].EndsWith('女子中学生だ。'));
end;

{ Load the file, save it in the same encoding to a temporary folder and then
 compare byte-by-byte to the original file.
 Realistically, there will be cases when some of the nuances are lost. If this
 happens another test might be needed to load the file back and compare as data }
procedure TEncodingTestCase.LoadSaveCompare(const AFilename: string;
  AEncoding: CEncoding; ABom: boolean);
var tempDir: string;
begin
  LoadFile(AFilename, AEncoding);
  tempDir := CreateRandomTempDir();
  try
    SaveFile(tempDir+'\'+ExtractFilename(AFilename), AEncoding, ABom);
    Check(CompareFiles(AFilename, tempDir+'\'+ExtractFilename(AFilename)));
  finally
    DeleteDirectory(tempDir);
  end;
end;

procedure TEncodingTestCase.VerifyText;
begin
  Check(Self.EncodingClass <> nil, 'Encoding not found');
  VerifyContents(Self.TestFile, Self.EncodingClass);
end;

procedure TEncodingTestCase.GuessEncoding;
begin
  Check(Self.EncodingClass <> nil, 'Encoding not found');
  VerifyContents(Self.TestFile, nil);
end;

procedure TEncodingTestCase.SaveCompare;
begin
  Check(Self.EncodingClass <> nil, 'Encoding not found');
  LoadSaveCompare(Self.TestFile, Self.EncodingClass, Self.UseBom);
end;


{ Ascii text is different since it can't contain unicode }
procedure TAsciiEncodingTestCase.VerifyContents(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=3);
  Check(Lines[0].StartsWith('Example ansi'));
  Check(Lines[2].EndsWith('other line.'));
end;

{ With ACP we cannot verify ACP text because the active codepage can be different
 on the PC where tests are run, but at least we check what we can }
procedure TAcpEncodingTestCase.VerifyContents(const AFilename: string; AEncoding: CEncoding);
begin
  LoadFile(AFilename, AEncoding);
  Check(Lines.Count=4);
  Check(Lines[0].StartsWith('Example ansi'));
  Check(Lines[2].EndsWith('other line.'));
end;


constructor TEncodingTestSuite.Create(AEncodingClass: CEncoding; AUseBom: boolean; ATestFile: string;
  ATestClass: CEncodingTestCase);
begin
  Self.EncodingClass := AEncodingClass;
  Self.TestFile := ATestFile;
  Self.UseBom := AUseBom;
  if ATestClass = nil then
    inherited Create(TEncodingTestCase)
  else
    inherited Create(ATestClass);
end;

procedure TEncodingTestSuite.AddTest(ATest: ITest);
var testCase: TEncodingTestCase;
begin
  testCase := ATest as TEncodingTestCase;
  testCase.TestFile := TestFile;
  testCase.EncodingClass := EncodingClass;
  testCase.UseBom := UseBom;
  inherited;
end;

function TEncodingTestSuite.GetName: string;
begin
  Result := ChangeFileExt(ExtractFilename(Self.TestFile), '');
end;

function EncodingTestSuite: ITestSuite;
var ASuite: TTestSuite;
  fname, encName: string;
  EncodingClass: CEncoding;
  TestClass: CEncodingTestCase;
  UseBom: boolean;
begin
  ASuite := TTestSuite.Create('Encoding / decoding');
  for fname in FileList(TestCasesDir+'\encoding\', '*.txt') do begin
    encName := ChangeFileExt(ExtractFilename(fname), '');
    if encName.EndsWith('-bom') then begin
      UseBom := true;
      SetLength(encName, Length(encName)-4);
    end else
      UseBom := false;
    EncodingClass := JWBIO.FindEncodingByName(encName);
    if SameText(encName, 'ascii') or SameText(encName, 'ansi') then
      TestClass := TAsciiEncodingTestCase
    else
    if SameText(encName, 'acp') then
      TestClass := TAcpEncodingTestCase
    else
      TestClass := TEncodingTestCase;
    ASuite.AddTest(TEncodingTestSuite.Create(EncodingClass, UseBom, fname, TestClass));
  end;
  Result := ASuite;
end;



{ Misc encoding tests }

procedure TMiscEncodingTests.SurrogateLinefeed;
var inp: TStreamDecoder;
  ln: UnicodeString;
begin
  inp := OpenTextFile(TestCasesDir+'\encoding-misc\utf8-surrogates.txt', TUTF8Encoding);
  Check(inp.ReadLn(ln), 'Cannot read introductory lines');
  Check(inp.ReadLn(ln), 'Cannot read introductory lines');
  Check(inp.ReadLn(ln), 'Cannot read data line');
  Check(Length(ln)>=4, 'Length(data)<4');
  Check(ln[1]=' ');
  Check(ln[4]=' ');
  Check(Ord(ln[2])=$D860);
  Check(Ord(ln[3])=$DEB0);
end;


{ Encoding detection }

constructor TEncodingDetectionTest.Create(const AFilename: string);
begin
  Filename := AFilename;
  inherited Create('DetectEncoding');
end;

function TEncodingDetectionTest.GetName: string;
begin
  Result := ChangeFileExt(ExtractFilename(Filename), '');
end;

procedure TEncodingDetectionTest.DetectEncoding;
var enc: CEncoding;
  bareFilename: string;
  encName: string;
begin
  JWBIO.Conv_DetectType(Self.Filename, enc);
  Check(enc <> nil, 'Cannot detect encoding');

  bareFilename := ChangeFileExt(Self.Filename, ''); //strip .txt
  encName := ExtractFileExt(bareFilename); //read encoding name (second extension)
  if encName.StartsWith('.') then
    delete(encName, 1, 1);
  Check(FindEncodingByName(encName) = enc, 'Detected: '+enc.Classname+', expected: '+encName);
end;

function EncodingDetectionSuite: ITestSuite;
var ASuite: TTestSuite;
  fname: string;
begin
  ASuite := TTestSuite.create('Encoding detection');
  for fname in FileList(TestCasesDir+'\encoding-detection', '*.txt') do
    ASuite.AddTest(TEncodingDetectionTest.Create(fname));
  Result := ASuite;
end;



{ Speed tests }

procedure TReadWriteSpeedTestCase.TestSpeed(AEncoding: CEncoding);
var enc: TStreamEncoder;
  dec: TStreamDecoder;
  tempDir: string;
  tm: cardinal;
  i: integer;
  ln: string;
begin
  tempDir := CreateRandomTempDir();
  try
    tm := GetTickCount();
    enc := CreateTextFile(tempDir+'\test.txt', AEncoding);
    try
      enc.WriteBom;
      for i := 0 to 110000 do
        enc.WriteLn('私の家、とは、心の中ではあまり言いたくない羽川家のキッチンに'
          +'は、調理器具がとにかく多い。まな板は三枚あり、包丁も三本ある。');
    finally
      FreeAndNil(enc);
    end;
    tm := GetTickCount() - tm;
    Status('Write: '+IntToStr(tm)+' ticks.');

    tm := GetTickCount();
    dec := OpenTextFile(tempDir+'\test.txt', AEncoding);
    try
      i := 0;
      while dec.ReadLn(ln) do
        Inc(i);
      Check(i<>110001, 'Invalid number of files when reading back from file: '
        +IntToStr(i));
    finally
      FreeAndNil(dec);
    end;
    tm := GetTickCount() - tm;
    Status('Read: '+IntToStr(tm)+' ticks.');

  finally
    DeleteDirectory(tempDir);
  end;
end;

procedure TReadWriteSpeedTestCase.Ascii;
begin
  TestSpeed(TAsciiEncoding);
end;

procedure TReadWriteSpeedTestCase.UTF8;
begin
  TestSpeed(TUTF8Encoding);
end;

procedure TReadWriteSpeedTestCase.UTF16LE;
begin
  TestSpeed(TUTF16LEEncoding);
end;

procedure TReadWriteSpeedTestCase.UTF16BE;
begin
  TestSpeed(TUTF16BEEncoding);
end;

procedure TReadWriteSpeedTestCase.EUC;
begin
  TestSpeed(TEUCEncoding);
end;

procedure TReadWriteSpeedTestCase.ShiftJis;
begin
  TestSpeed(TSJISEncoding);
end;

procedure TReadWriteSpeedTestCase.Jis;
begin
  TestSpeed(TJISEncoding);
end;

procedure TReadWriteSpeedTestCase.OldJis;
begin
  TestSpeed(TOldJISEncoding);
end;

procedure TReadWriteSpeedTestCase.NECJis;
begin
  TestSpeed(TNecJISEncoding);
end;

procedure TReadWriteSpeedTestCase.GB;
begin
  TestSpeed(TGBEncoding);
end;

procedure TReadWriteSpeedTestCase.Big5;
begin
  TestSpeed(TBIG5Encoding);
end;

procedure TReadWriteSpeedTestCase.Acp;
begin
  TestSpeed(TACPEncoding);
end;

function JWBIOTestSuite: ITestSuite;
var ASuite: TTestSuite;
begin
  ASuite := TTestSuite.Create('JWBIO');
  ASuite.addTest(EncodingTestSuite);
  ASuite.addTest(TMiscEncodingTests.Suite);
  ASuite.AddTest(EncodingDetectionSuite);
  Result := ASuite;
end;

initialization
  RegisterTest(JWBIOTestSuite);
  RegisterSpeedTest(TNamedTestSuite.Create('JWBIO Read/Write Speed', TReadWriteSpeedTestCase))

end.
