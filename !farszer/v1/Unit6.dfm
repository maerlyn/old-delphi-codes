object Form6: TForm6
  Left = 536
  Top = 293
  Width = 315
  Height = 455
  Caption = 'Form6'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 0
    Top = 0
    Width = 307
    Height = 425
    Align = alClient
    Columns = <
      item
        Caption = 'dummy'
        MaxWidth = 1
        Width = 1
      end
      item
        Caption = 'C'#233'g'
        Width = 150
      end
      item
        Caption = 'Term'#233'k'
        Width = 145
      end>
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnChange = ListView1Change
    OnColumnClick = ListView1ColumnClick
    OnCompare = ListView1Compare
    OnDblClick = ListView1DblClick
  end
  object BitBtn1: TBitBtn
    Left = 0
    Top = 400
    Width = 305
    Height = 25
    TabOrder = 1
    Kind = bkOK
  end
end
