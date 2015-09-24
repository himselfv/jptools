program Tests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestingCommon,
  FastArray in '..\Share\FastArray.pas',
  JWBIOTests in '..\Share\JWBIOTests.pas',
  KanjiDicTests in '..\Share\KanjiDicTests.pas',
  EdictTests in '..\Share\EdictTests.pas',
  KanaConvTests in '..\KanaConv\KanaConvTests.pas';

{$R *.RES}

var
  ExitBehavior: TRunnerExitBehavior;

begin
  Application.Initialize;
  if IsConsole then begin
    if FindCmdLineSwitch('halt') then
      ExitBehavior := rxbHaltOnFailures
    else
      ExitBehavior := rxbContinue;
    with TextTestRunner.RunRegisteredTests(ExitBehavior) do
      Free;
    if FindCmdLineSwitch('pause') then
      readln;
  end else
    GuiTestRunner.RunRegisteredTests;
end.

