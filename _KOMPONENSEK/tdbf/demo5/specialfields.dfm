object SpecialFieldsForm: TSpecialFieldsForm
  Left = 245
  Top = 135
  Width = 400
  Height = 308
  Caption = 'SpecialFieldsForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 8
    Top = 104
    Width = 240
    Height = 25
    DataSource = DataSource1
    TabOrder = 0
  end
  object DBGrid1: TDBGrid
    Left = 8
    Top = 136
    Width = 377
    Height = 137
    DataSource = DataSource1
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 8
    Width = 185
    Height = 89
    Caption = 'Table'
    Items.Strings = (
      'dBase III + '
      'dBase IV'
      'dBase for Windows'
      'Visual dBase')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object Dbf1: TDbf
    Version = 'TDbf V5.001'
    Indexes = <>
    OpenMode = omAutoCreate
    Storage = stoFile
    FilePath = 'data\'
    ShowDeleted = False
    Left = 264
    Top = 8
  end
  object DataSource1: TDataSource
    DataSet = Dbf1
    Left = 296
    Top = 8
  end
end
