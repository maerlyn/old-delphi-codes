object frmTeszt: TfrmTeszt
  Left = 192
  Top = 107
  Width = 342
  Height = 133
  Caption = 'Teszt'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 40
    Height = 13
    Caption = 'Évszám:'
  end
  object Label2: TLabel
    Left = 10
    Top = 48
    Width = 46
    Height = 13
    Caption = 'Esemény:'
  end
  object txtEvszam: TEdit
    Left = 64
    Top = 13
    Width = 268
    Height = 21
    TabOrder = 0
  end
  object txtEsemeny: TEdit
    Left = 64
    Top = 45
    Width = 268
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 0
    Top = 72
    Width = 333
    Height = 33
    Caption = '&OK'
    Default = True
    TabOrder = 2
    OnClick = Button1Click
  end
end
