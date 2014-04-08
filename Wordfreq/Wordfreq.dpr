program Wordfreq;

uses
  Vcl.Forms,
  WordFreq_MainForm in 'WordFreq_MainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
