object Form1: TForm1
  Left = 300
  Top = 268
  AutoScroll = False
  Caption = 'Form1'
  ClientHeight = 29
  ClientWidth = 146
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 8
    Width = 150
    Height = 20
    AutoSize = True
    Visible = False
  end
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 42
    Height = 25
    Caption = 'Source'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 41
    Top = 0
    Width = 55
    Height = 25
    Caption = 'Destination'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 95
    Top = 0
    Width = 48
    Height = 25
    Caption = 'Come on!'
    TabOrder = 2
    OnClick = Button3Click
  end
  object OpenPictureDialog1: TOpenDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 8
  end
  object SavePictureDialog1: TSaveDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 32
  end
end
