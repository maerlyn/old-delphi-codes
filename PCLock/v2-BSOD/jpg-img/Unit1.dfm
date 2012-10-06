object Form1: TForm1
  Left = 317
  Top = 192
  Width = 161
  Height = 114
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
    Left = 8
    Top = 40
    Width = 25
    Height = 17
    AutoSize = True
    Visible = False
  end
  object Gauge1: TGauge
    Left = 0
    Top = 70
    Width = 153
    Height = 14
    Progress = 0
  end
  object Button1: TButton
    Left = 40
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Kezdés'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 120
    Top = 40
    Width = 33
    Height = 25
    Lines.Strings = (
      'Mem'
      'o1')
    TabOrder = 1
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Filter = 'JPEG|*.jpg'
    Left = 8
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Filter = 'IMG|*.img'
    Left = 120
    Top = 8
  end
end
