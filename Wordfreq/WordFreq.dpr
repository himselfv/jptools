program WordFreq;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  UniStrUtils,
  ConsoleToolbox,
  JWBIO;

type
  TWordFreq = class(TCommandLineApp)
  protected
  end;

begin
  RunApp(TWordFreq);
end.
