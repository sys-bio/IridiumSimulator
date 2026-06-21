object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 684
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 823
    Height = 105
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 721
    object Label1: TLabel
      Left = 272
      Top = 13
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 272
      Top = 60
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object btnSubGraph1: TButton
      Left = 97
      Top = 8
      Width = 75
      Height = 25
      Caption = 'SubGraph'
      TabOrder = 0
      OnClick = btnSubGraph1Click
    end
    object btnPlotDataSin: TButton
      Left = 96
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Plot Data'
      TabOrder = 1
      OnClick = btnPlotDataSinClick
    end
    object btnSubGraph2: TButton
      Left = 178
      Top = 8
      Width = 75
      Height = 25
      Caption = 'SubGraph'
      TabOrder = 2
      OnClick = btnSubGraph2Click
    end
    object Button2: TButton
      Left = 177
      Top = 48
      Width = 75
      Height = 25
      Caption = 'Plot Data'
      TabOrder = 3
      OnClick = Button2Click
    end
    object btnSubGraphs: TButton
      Left = 449
      Top = 8
      Width = 75
      Height = 25
      Caption = 'SubGraphs'
      TabOrder = 4
      OnClick = btnSubGraphsClick
    end
    object btnAddGraph: TButton
      Left = 449
      Top = 41
      Width = 75
      Height = 25
      Caption = 'Add Graph'
      TabOrder = 5
      OnClick = btnAddGraphClick
    end
    object btnDemo: TButton
      Left = 328
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Demo'
      TabOrder = 6
      OnClick = btnDemoClick
    end
  end
  object plt: TRRGraph
    Left = 0
    Top = 105
    Width = 823
    Height = 579
    Align = alClient
    subgraphs = <>
    OnResize = pltResize
    DoubleBuffered = False
    ParentBackground = False
    ExplicitLeft = 88
    ExplicitTop = 128
    ExplicitWidth = 561
    ExplicitHeight = 417
  end
end
