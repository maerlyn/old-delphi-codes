object FoForm: TFoForm
  Left = 195
  Top = 112
  Width = 544
  Height = 375
  Caption = 'Hanoi tornyok'
  Color = clWhite
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 28
    Top = 64
    Width = 160
    Height = 14
    OnClick = Image1Click
  end
  object Image2: TImage
    Left = 28
    Top = 78
    Width = 160
    Height = 14
    OnClick = Image2Click
  end
  object Image3: TImage
    Left = 28
    Top = 92
    Width = 160
    Height = 14
    OnClick = Image3Click
  end
  object Image4: TImage
    Left = 28
    Top = 106
    Width = 160
    Height = 14
    OnClick = Image4Click
  end
  object Image5: TImage
    Left = 28
    Top = 120
    Width = 160
    Height = 14
    OnClick = Image5Click
  end
  object Image6: TImage
    Left = 28
    Top = 134
    Width = 160
    Height = 14
    OnClick = Image6Click
  end
  object Image7: TImage
    Left = 28
    Top = 148
    Width = 160
    Height = 14
    OnClick = Image7Click
  end
  object Image8: TImage
    Left = 28
    Top = 162
    Width = 160
    Height = 14
    OnClick = Image8Click
  end
  object Image9: TImage
    Left = 188
    Top = 64
    Width = 160
    Height = 14
    OnClick = Image9Click
  end
  object Image10: TImage
    Left = 188
    Top = 78
    Width = 160
    Height = 14
    OnClick = Image10Click
  end
  object Image11: TImage
    Left = 188
    Top = 92
    Width = 160
    Height = 14
    OnClick = Image11Click
  end
  object Image12: TImage
    Left = 188
    Top = 106
    Width = 160
    Height = 14
    OnClick = Image12Click
  end
  object Image13: TImage
    Left = 188
    Top = 120
    Width = 160
    Height = 14
    OnClick = Image13Click
  end
  object Image14: TImage
    Left = 188
    Top = 134
    Width = 160
    Height = 14
    OnClick = Image14Click
  end
  object Image15: TImage
    Left = 188
    Top = 148
    Width = 160
    Height = 14
    OnClick = Image15Click
  end
  object Image16: TImage
    Left = 188
    Top = 162
    Width = 160
    Height = 14
    OnClick = Image16Click
  end
  object Image17: TImage
    Left = 346
    Top = 64
    Width = 160
    Height = 14
    OnClick = Image17Click
  end
  object Image18: TImage
    Left = 346
    Top = 78
    Width = 160
    Height = 14
    OnClick = Image18Click
  end
  object Image19: TImage
    Left = 346
    Top = 92
    Width = 160
    Height = 14
    OnClick = Image19Click
  end
  object Image20: TImage
    Left = 346
    Top = 106
    Width = 160
    Height = 14
    OnClick = Image20Click
  end
  object Image21: TImage
    Left = 346
    Top = 120
    Width = 160
    Height = 14
    OnClick = Image21Click
  end
  object Image22: TImage
    Left = 346
    Top = 134
    Width = 160
    Height = 14
    OnClick = Image22Click
  end
  object Image23: TImage
    Left = 346
    Top = 148
    Width = 160
    Height = 14
    OnClick = Image23Click
  end
  object Image24: TImage
    Left = 346
    Top = 162
    Width = 160
    Height = 14
    OnClick = Image24Click
  end
  object Label1: TLabel
    Left = 16
    Top = 206
    Width = 82
    Height = 13
    Caption = 'Korongok száma:'
  end
  object cmbKorongokSzama: TComboBox
    Left = 104
    Top = 202
    Width = 49
    Height = 21
    Style = csDropDownList
    DropDownCount = 5
    ItemHeight = 13
    TabOrder = 0
    Items.Strings = (
      '3'
      '4'
      '5'
      '6'
      '7')
  end
  object cmdUjjatek: TButton
    Left = 16
    Top = 232
    Width = 137
    Height = 25
    Caption = '&Új játék'
    TabOrder = 1
    OnClick = cmdUjjatekClick
  end
end
