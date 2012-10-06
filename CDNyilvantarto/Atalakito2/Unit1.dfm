object Form1: TForm1
  Left = 273
  Top = 230
  Width = 257
  Height = 92
  Caption = 'CD-Nyilvántartó átalakító'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Gauge1: TGauge
    Left = 8
    Top = 8
    Width = 233
    Height = 17
    Progress = 0
  end
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 233
    Height = 25
    Caption = 'Átalakítandó adatbázis megnyitása'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ADOTable1: TADOTable
    Left = 96
    Top = 16
  end
  object CDNyDataFile_CDk1: TCDNyDataFile_CDk
    Left = 32
    Top = 16
  end
  object CDNyDataFile_Kolcsonkerok1: TCDNyDataFile_Kolcsonkerok
    Left = 64
    Top = 16
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Data.mdb|Data.mdb'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist]
    Title = 'Válaszdd ki az adatbázist!'
    Left = 128
    Top = 16
  end
end
