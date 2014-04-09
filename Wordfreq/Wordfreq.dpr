program Wordfreq;
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

NOTE: This should have been more appropriately named "WordFreq/Coverage", but
for now there are no other wordfreq-related projects so this name will stay.
}

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
