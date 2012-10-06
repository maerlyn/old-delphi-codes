object Form1: TForm1
  Left = 293
  Top = 270
  Width = 113
  Height = 68
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
    Left = 8
    Top = 8
    Width = 89
    Height = 25
    Caption = 'Átalakítás'
    TabOrder = 0
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'PHTML|*.phtml|TXT|*.txt'
    FilterIndex = 2
    Left = 8
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Filter = 'CSV|*.csv'
    Left = 32
    Top = 16
  end
end
