object Form1: TForm1
  Left = 248
  Top = 153
  Width = 870
  Height = 640
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
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 105
    Height = 105
  end
  object StringGrid1: TStringGrid
    Left = 8
    Top = 112
    Width = 841
    Height = 481
    ColCount = 35
    RowCount = 35
    TabOrder = 0
    Visible = False
  end
  object OpenDialog1: TOpenDialog
    Left = 176
    Top = 24
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.bmp'
    Filter = 'bitmap|*.bmp'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 176
    Top = 72
  end
end
