object Form1: TForm1
  Left = 192
  Top = 133
  Width = 544
  Height = 375
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
  object Button1: TButton
    Left = 128
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Mentés'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Töltés'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 224
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Adatfeltöltés'
    TabOrder = 2
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Left = 24
    Top = 48
  end
  object SaveDialog1: TSaveDialog
    Left = 64
    Top = 48
  end
end
