object Form1: TForm1
  Left = 268
  Top = 180
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 541
    Width = 862
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object Bevel1: TBevel
    Left = 812
    Top = 29
    Width = 50
    Height = 512
    Align = alRight
  end
  object Splitter2: TSplitter
    Left = 809
    Top = 29
    Height = 512
    Align = alRight
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 544
    Width = 862
    Height = 66
    Align = alBottom
    RowCount = 2
    TabOrder = 0
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 862
    Height = 29
    Caption = 'ToolBar1'
    TabOrder = 1
  end
  object ListView1: TListView
    Left = 0
    Top = 29
    Width = 809
    Height = 512
    Align = alClient
    Columns = <>
    FlatScrollBars = True
    GridLines = True
    Items.Data = {
      460000000200000000000000FFFFFFFFFFFFFFFF020000000000000005353436
      3534033635340336353400000000FFFFFFFFFFFFFFFF00000000000000000336
      3834FFFFFFFF}
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
  end
end
