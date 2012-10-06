object Form1: TForm1
  Left = 185
  Top = 290
  Width = 544
  Height = 84
  Caption = 'Mdb to Html'
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
    Left = 8
    Top = 8
    Width = 521
    Height = 17
    Progress = 0
  end
  object Gauge2: TGauge
    Left = 9
    Top = 30
    Width = 520
    Height = 19
    Progress = 0
  end
  object Memo1: TMemo
    Left = 160
    Top = 0
    Width = 67
    Height = 49
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Access adatbázis|*.mdb'
    Title = 'Válassza ki az adatbázist!'
    Left = 112
  end
  object ADOConnection1: TADOConnection
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 240
  end
  object ADOTable1: TADOTable
    Connection = ADOConnection1
    TableName = 'Szervek'
    Left = 280
  end
  object ADOTable2: TADOTable
    Connection = ADOConnection1
    TableName = 'Vezetok'
    Left = 320
  end
  object ADOTable3: TADOTable
    Connection = ADOConnection1
    TableName = 'Telefonszamok'
    Left = 360
  end
end
