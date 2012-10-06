object Form1: TForm1
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'PC-Lock'
  ClientHeight = 98
  ClientWidth = 209
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 209
    Height = 97
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnMouseDown = GroupBox1MouseDown
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Jelszó:'
    end
    object Label2: TLabel
      Left = 8
      Top = 48
      Width = 61
      Height = 13
      Caption = 'Megerõsítés:'
    end
    object Edit1: TEdit
      Left = 80
      Top = 22
      Width = 121
      Height = 21
      Color = clBlack
      PasswordChar = '*'
      TabOrder = 0
    end
    object Edit2: TEdit
      Left = 80
      Top = 48
      Width = 121
      Height = 21
      Color = clBlack
      PasswordChar = '*'
      TabOrder = 1
    end
    object ActiveButton1: TActiveButton
      Left = 8
      Top = 72
      Width = 193
      Height = 20
      Caption = '                       Blokkolás'
      ColorOFF = clBlack
      ColorON = clBlack
      TextColorOFF = clWhite
      TextColorON = clWhite
      BordColorOFF = 54528
      BordColorON = clGreen
      OnClick = ActiveButton1Click
    end
  end
  object ActiveButton5: TActiveButton
    Left = 8
    Top = 0
    Width = 57
    Height = 20
    Caption = 'PC-Lock'
    ColorOFF = clBlack
    ColorON = clBlack
    TextColorOFF = clLime
    TextColorON = clLime
    BordColorOFF = clWhite
    BordColorON = clWhite
    Enabled = False
  end
  object ActiveButton2: TActiveButton
    Left = 184
    Top = 0
    Width = 17
    Height = 17
    Hint = 'Bezárás'
    Caption = 'x'
    ColorOFF = clBlack
    ColorON = clBlack
    TextColorOFF = clRed
    TextColorON = clLime
    BordColorOFF = clWhite
    BordColorON = clWhite
    OnClick = ActiveButton2Click
  end
end
