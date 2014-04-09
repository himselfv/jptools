unit WordFreqCoverage_MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JWBIO, FastArray, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TWfRecord = record
    word: string;
    count: int64;
  end;
  PWfRecord = ^TWfRecord;

  TMainForm = class(TForm)
    tbWordCount: TTrackBar;
    edtWordCount: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    tbCoverage: TTrackBar;
    edtCoverage: TEdit;
    Label3: TLabel;
    Bevel1: TBevel;
    edtCutWords: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtWordCountChange(Sender: TObject);
    procedure edtCoverageChange(Sender: TObject);
    procedure tbWordCountChange(Sender: TObject);
    procedure tbCoverageChange(Sender: TObject);
    procedure edtWordCountExit(Sender: TObject);
    procedure edtCoverageExit(Sender: TObject);
    procedure edtCutWordsExit(Sender: TObject);
  protected
    wf: TArray<TWfRecord>;
    TotalCount: int64;
    procedure LoadWordFreq(const AFilename: string; AEncoding: CEncoding);
  protected
    FCutWordCount: integer;
    FTotalUncutCount: int64;
    procedure SetCutWordCount(AValue: integer);
  protected
    FCurWordCount: integer;
    FCurCoverage: integer;
    FIgnoreWordCountChange: integer;
    FIgnoreCoverageChange: integer;
    procedure SetWordCount(AValue: integer; const ARunTriggers: boolean = true);
    procedure SetCoverage(AValue: integer; const ARunTriggers: boolean = true);
    procedure UpdateWordCount;
    procedure UpdateCoverage;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadWordFreq('wordfreq_ck', TEUCEncoding);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  tbWordCount.Max := 100;
  tbCoverage.Max := 100;
  SetCutWordCount(0);
end;

procedure TMainForm.LoadWordFreq(const AFilename: string; AEncoding: CEncoding);
var inp: TStreamDecoder;
  ln: string;
  i: integer;
begin
  inp := OpenTextFile(AFilename, AEncoding);
  try
    wf.Reset;
    TotalCount := 0;
    while inp.ReadLn(ln) do begin
//      ln := Trim(ln); //do not do, trims some literals erroneously (breaks parsing)
      if (ln='') or (ln[1]='#') then continue;
      i := ln.IndexOf(#9);
      if i<=0 then
        raise Exception.Create('Invalid line: '+ln);
      with wf.AddNew^ do begin
        word := copy(ln, 1, i);
        count := StrToInt(copy(ln, i+2, MaxInt));
        Inc(TotalCount, count);
      end;
    end;
  finally
    FreeAndNil(inp);
  end;
  if TotalCount<=0 then
    TotalCount := 1; //simplify things
end;

procedure TMainForm.edtCutWordsExit(Sender: TObject);
begin
  SetCutWordCount(StrToInt(edtCutWords.Text));
end;

procedure TMainForm.SetCutWordCount(AValue: integer);
var i: integer;
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > wf.Count then
    AValue := wf.Count;
  FCutWordCount := AValue;
  FTotalUncutCount := 0;
  for i := FCutWordCount to wf.Count-1 do
    Inc(FTotalUncutCount, wf[i].count);
  if FTotalUncutCount<=0 then
    FTotalUncutCount := 1;
  SetWordCount(0);
end;

{ Actual values are stored in tbWordCount/tbCoverage.Position because that's
 an int.
 When editing the text field, values are saved to .Position and applied when
 valid, then last valid value is restored to text on control exit. }

procedure TMainForm.SetWordCount(AValue: integer; const ARunTriggers: boolean);
var tmp: integer;
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > wf.Count then
    AValue := wf.Count;
  FCurWordCount := AValue;
  Inc(FIgnoreWordCountChange);
  try
    if not TryStrToInt(edtWordCount.Text, tmp) or (tmp<>AValue) then
      edtWordCount.Text := IntToStr(AValue);
    tmp := Trunc(100 * FCurWordCount / (wf.Count-FCutWordCount));
    if tbWordCount.Position<>tmp then
      tbWordCount.Position := tmp;
  finally
    Dec(FIgnoreWordCountChange);
  end;
  if ARunTriggers then
    UpdateCoverage;
end;

procedure TMainForm.tbWordCountChange(Sender: TObject);
begin
  if FIgnoreWordCountChange>0 then exit;
  SetWordCount(Trunc((tbWordCount.Position  / 100) * (wf.Count-FCutWordCount)))
end;

procedure TMainForm.edtWordCountChange(Sender: TObject);
var tmp: integer;
begin
  if FIgnoreWordCountChange>0 then exit;
  if TryStrToInt(edtWordCount.Text, tmp) then
    SetWordCount(tmp);
end;

procedure TMainForm.edtWordCountExit(Sender: TObject);
begin
 //Restore last working wordcount if broken
  SetWordCount(FCurWordCount, false);
end;

//Update Coverage value when WordCount changes
procedure TMainForm.UpdateCoverage;
var cnt: int64;
  i: integer;
begin
  cnt := 0;
  for i := FCutWordCount to FCutWordCount+FCurWordCount-1 do
    Inc(cnt, wf[i].count);
  SetCoverage(Trunc(cnt*100/FTotalUncutCount), false);
end;

procedure TMainForm.SetCoverage(AValue: integer; const ARunTriggers: boolean);
var tmp: integer;
begin
  if AValue < 0 then
    AValue := 0;
  if AValue > tbCoverage.Max then
    AValue := tbCoverage.Max;
  FCurCoverage := AValue;
  Inc(FIgnoreCoverageChange);
  try
    if not TryStrToInt(edtCoverage.Text, tmp) or (tmp<>AValue) then
      edtCoverage.Text := IntToStr(AValue);
    if tbCoverage.Position<>AValue then
      tbCoverage.Position := AValue;
  finally
    Dec(FIgnoreCoverageChange);
  end;
  if ARunTriggers then
    UpdateWordCount;
end;

procedure TMainForm.tbCoverageChange(Sender: TObject);
begin
  if FIgnoreCoverageChange>0 then exit;
  SetCoverage(tbCoverage.Position);
end;

procedure TMainForm.edtCoverageChange(Sender: TObject);
var tmp: integer;
begin
  if FIgnoreCoverageChange>0 then exit;
  if TryStrToInt(edtCoverage.Text, tmp) then
    SetCoverage(tmp);
end;

procedure TMainForm.edtCoverageExit(Sender: TObject);
begin
 //Restore last working coverage if broken
  SetCoverage(FCurCoverage, false);
end;

//Update WordCount value when Coverage changes
procedure TMainForm.UpdateWordCount;
var cnt: int64;
  i: integer;
begin
  cnt := 0;
  i := FCutWordCount;
  while i<wf.Count do begin
    if (cnt * 100 / FTotalUncutCount) >= tbCoverage.Position then
      break;
    Inc(cnt, wf[i].count);
    Inc(i);
  end;
  SetWordCount(i, false);
end;


end.
