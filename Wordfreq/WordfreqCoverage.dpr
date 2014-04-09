program WordfreqCoverage;
{
Loads wordfreq_ck formatted files and shows how many words you have to know
to understand a certain percentage of the texts.
E.g.
  1000 words   =>  60% of the text
  2000 words   =>  70% of the text
  4000 words   =>  80%
  8000 words   =>  90%
And vice versa, shows what percentage of the texts from the number of known
words.
}

uses
  Vcl.Forms,
  WordFreqCoverage_MainForm in 'WordFreqCoverage_MainForm.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
