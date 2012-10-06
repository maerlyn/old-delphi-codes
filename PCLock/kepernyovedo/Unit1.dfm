object Form1: TForm1
  Left = 387
  Top = 277
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
    object ActiveButton1: TBitBtnWithColor
      Left = 8
      Top = 40
      Width = 161
      Height = 20
      Caption = 'Feloldás'
      TabOrder = 1
      OnClick = ActiveButton1Click
      Color = clBlack
    end
  end
end
