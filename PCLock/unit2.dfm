object Form2: TForm2
  Left = 431
  Top = 290
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Gép lezárva'
  ClientHeight = 73
  ClientWidth = 177
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 0
    Top = 8
    Width = 177
    Height = 65
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 32
      Height = 13
      Caption = 'Jelszó:'
    end
    object Edit1: TEdit
      Left = 48
      Top = 16
      Width = 121
      Height = 21
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clLime
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      PasswordChar = '*'
      TabOrder = 0
      OnKeyPress = Edit1KeyPress
    end
    object ActiveButton1: TActiveButton
      Left = 8
      Top = 40
      Width = 161
      Height = 20
      Caption = '                  Feloldás'
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
end
