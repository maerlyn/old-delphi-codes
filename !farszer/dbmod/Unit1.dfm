object Form1: TForm1
  Left = 414
  Top = 305
  Width = 553
  Height = 388
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
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 529
    Height = 345
    Lines.Strings = (
      'V'#233'grehajtott parancs:'
      'ALTER TABLE main ADD numbers VARCHAR(8) NOT NULL AFTER status;')
    TabOrder = 0
  end
end
