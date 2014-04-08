unit WordFreq_MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JWBIO, FastArray, Vcl.StdCtrls,
  Vcl.ComCtrls;

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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtWordCountChange(Sender: TObject);
    procedure edtCoverageChange(Sender: TObject);
    procedure tbWordCountChange(Sender: TObject);
    procedure tbCoverageChange(Sender: TObject);
    procedure edtWordCountExit(Sender: TObject);
    procedure edtCoverageExit(Sender: TObject);
  protected
    wf: TArray<TWfRecord>;
    TotalCount: int64;
    procedure LoadWordFreq(const AFilename: string; AEncoding: CEncoding);
  protected
    FCurWordCount: integer;
    FIgnoreWordCountChange: boolean;
    FIgnoreCoverageChange: boolean;
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
//      ln := Trim(ln);
      if (ln='') or (ln[1]='#') then continue;
      i := ln.IndexOf(#9);
      if i<=0 then
        raise Exception.Create('Invalid line: '+ln);
      with wf.AddNew^ do begin
        word := copy(ln, 1, i);
        count := StrToInt(copy(ln, i+2, MaxINt));
        Inc(TotalCount, count);
      end;
    end;
  finally
    FreeAndNil(inp);
  end;
  if TotalCount<=0 then
    TotalCount := 1; //simplify things
end;

{ Actual values are stored in tbWordCount/tbCoverage.Position because that's
 an int.
 When editing the text field, values are saved to .Position and applied when
 valid, then last valid value is restored to text on control exit. }

procedure TMainForm.tbWordCountChange(Sender: TObject);
begin
  if FIgnoreWordCountChange then exit;
  FCurWordCount := Trunc((tbWordCount.Position  / 100) * wf.Count);
  FIgnoreWordCountChange := true;
  try
    edtWordCount.Text := IntToStr(FCurWordCount);
    UpdateCoverage;
  finally
    FIgnoreWordCountChange := false;
  end;
end;

procedure TMainForm.edtWordCountChange(Sender: TObject);
var tmp: integer;
begin
  if FIgnoreWordCountChange then exit;
  if not TryStrToInt(edtWordCount.Text, tmp) then exit;
  if tmp<0 then tmp := 0;
  if tmp>=wf.Count then tmp := wf.Count;
  FCurWordCount := tmp;
  FIgnoreWordCountChange := true;
  try
    tbWordCount.Position := Trunc(100 * FCurWordCount / wf.Count);
    UpdateCoverage;
  finally
    FIgnoreWordCountChange := false;
  end;
end;

procedure TMainForm.edtWordCountExit(Sender: TObject);
var tmp: integer;
begin
 //Restore last working wordcount if broken
  if not TryStrToInt(edtWordCount.Text, tmp) or (tmp<>FCurWordCount) then begin
    edtWordCount.Text := IntToStr(FCurWordCount);
    edtWordCountChange(edtWordCount);
  end;
end;

procedure TMainForm.tbCoverageChange(Sender: TObject);
begin
  if FIgnoreCoverageChange then exit;
  FIgnoreCoverageChange := true;
  try
    edtCoverage.Text := IntToStr(tbCoverage.Position);
    UpdateWordCount;
  finally
    FIgnoreCoverageChange := false;
  end;
end;

procedure TMainForm.edtCoverageChange(Sender: TObject);
var tmp: integer;
begin
  if FIgnoreCoverageChange then exit;
  if not TryStrToInt(edtCoverage.Text, tmp) then exit;
  tmp := tmp * 10;
  if tmp > tbCoverage.Max then
    tmp := tbCoverage.Max;
  FIgnoreCoverageChange := true;
  try
    tbCoverage.Position := tmp;
    UpdateWordCount;
  finally
    FIgnoreCoverageChange := false;
  end;
end;

procedure TMainForm.edtCoverageExit(Sender: TObject);
begin
  edtCoverage.Text := IntToStr(tbCoverage.Position);
end;

//Update WordCount value when Coverage changes
procedure TMainForm.UpdateWordCount;
var cnt, i: integer;
begin
  cnt := 0;
  i := 0;
  while i<wf.Count do begin
    if (cnt / TotalCount) >= tbCoverage.Position then
      break;
    Inc(i);
  end;
  FCurWordCount := i;
  tbWordCount.Position := Trunc(100 * FCurWordCount / wf.Count);
  edtWordCount.Text := IntToStr(FCurWordCount);
end;

//Update Coverage value when WordCount changes
procedure TMainForm.UpdateCoverage;
var cnt: int64;
  i: integer;
begin
  cnt := 0;
  for i := 0 to FCurWordCount-1 do
    Inc(cnt, wf[i].count);
  tbCoverage.Position := Trunc(cnt * 100 / TotalCount);
  edtCoverageExit(edtCoverage);
end;

end.
