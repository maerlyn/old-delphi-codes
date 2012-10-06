object Form1: TForm1
  Left = 370
  Top = 264
  Width = 113
  Height = 69
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 15
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Átalakítás!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'CDNy adatbázis|Data.mdb'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Jelenlegi CDNy adatbásis megnyitása'
  end
  object ADOTable1: TADOTable
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=aaa;Mode=ReadWrite;' +
      'Persist Security Info=False'
    Left = 32
  end
  object Dbf1: TDbf
    Version = 'tDbf v4,010'
    Indexes = <>
    OpenMode = omAutoCreate
    Storage = stoAuto
    ShowDeleted = False
    Left = 64
  end
end
