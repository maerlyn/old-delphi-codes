object Form1: TForm1
  Left = 195
  Top = 258
  AutoScroll = False
  ClientHeight = 18
  ClientWidth = 536
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
  object Gauge1: TGauge
    Left = 0
    Top = 0
    Width = 536
    Height = 17
    Progress = 0
  end
  object Gauge2: TGauge
    Left = 0
    Top = 24
    Width = 536
    Height = 17
    Progress = 0
    Visible = False
  end
  object ListBox1: TListBox
    Left = 344
    Top = 5
    Width = 121
    Height = 26
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    Visible = False
  end
  object Memo1: TMemo
    Left = 256
    Top = 8
    Width = 49
    Height = 17
    Lines.Strings = (
      'Memo1')
    TabOrder = 1
    Visible = False
  end
  object Memo2: TMemo
    Left = 88
    Top = 8
    Width = 65
    Height = 25
    Lines.Strings = (
      'Memo2')
    TabOrder = 2
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Access adatbázisok|*.mdb'
    Title = 'Válassza ki az adatbázist'
    Left = 32
    Top = 8
  end
  object ADOConnection1: TADOConnection
    LoginPrompt = False
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 192
    Top = 8
  end
  object ADOTable1: TADOTable
    Connection = ADOConnection1
    Left = 224
    Top = 8
  end
end
