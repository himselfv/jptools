object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Wordfreq'
  ClientHeight = 185
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 366
    Top = 91
    Width = 29
    Height = 13
    Caption = 'words'
  end
  object Label2: TLabel
    Left = 383
    Top = 110
    Width = 14
    Height = 23
    Caption = '='
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label3: TLabel
    Left = 366
    Top = 142
    Width = 11
    Height = 13
    Caption = '%'
  end
  object Bevel1: TBevel
    Left = 8
    Top = 64
    Width = 430
    Height = 10
    Shape = bsBottomLine
  end
  object Label4: TLabel
    Left = 12
    Top = 11
    Width = 17
    Height = 13
    Caption = 'Cut'
  end
  object Label5: TLabel
    Left = 119
    Top = 11
    Width = 55
    Height = 13
    Caption = 'first words.'
  end
  object tbWordCount: TTrackBar
    Left = 8
    Top = 88
    Width = 225
    Height = 45
    TabOrder = 0
    OnChange = tbWordCountChange
  end
  object edtWordCount: TEdit
    Left = 239
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '0'
    OnChange = edtWordCountChange
    OnExit = edtWordCountExit
  end
  object tbCoverage: TTrackBar
    Left = 8
    Top = 139
    Width = 225
    Height = 45
    TabOrder = 2
    OnChange = tbCoverageChange
  end
  object edtCoverage: TEdit
    Left = 239
    Top = 139
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
    OnChange = edtCoverageChange
    OnExit = edtCoverageExit
  end
  object edtCutWords: TEdit
    Left = 51
    Top = 8
    Width = 62
    Height = 21
    TabOrder = 4
    Text = '0'
    OnExit = edtCutWordsExit
  end
end
