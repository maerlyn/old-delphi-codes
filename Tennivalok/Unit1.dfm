object TennivalokForm: TTennivalokForm
  Left = 192
  Top = 107
  BorderStyle = bsSingle
  Caption = 'Tennivalók'
  ClientHeight = 346
  ClientWidth = 529
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
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 185
    Height = 345
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'Filename'
        Visible = True
      end>
  end
  object DBNavigator1: TDBNavigator
    Left = 192
    Top = 0
    Width = 336
    Height = 25
    VisibleButtons = [nbFirst, nbPrior, nbNext, nbLast, nbDelete, nbEdit, nbPost, nbCancel]
    TabOrder = 1
  end
  object DBMemo1: TDBMemo
    Left = 192
    Top = 32
    Width = 337
    Height = 313
    DataField = 'Notes'
    DataSource = DataSource1
    TabOrder = 2
  end
  object ADOConnection1: TADOConnection
    LoginPrompt = False
    Mode = cmReadWrite
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 80
    Top = 56
  end
  object DataSource1: TDataSource
    DataSet = ADOTable1
    Left = 80
    Top = 168
  end
  object ADOTable1: TADOTable
    Connection = ADOConnection1
    TableName = 'Tennivalok'
    Left = 80
    Top = 112
  end
end
