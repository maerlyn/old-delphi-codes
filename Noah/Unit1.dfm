object Form1: TForm1
  Left = 231
  Top = 266
  Width = 385
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
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 177
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Eltelt idõ 2003.09.21. 15:40 óta:'
  end
  object Label2: TLabel
    Left = 192
    Top = 8
    Width = 177
    Height = 13
    AutoSize = False
    Caption = 'másodperc'
  end
  object Label3: TLabel
    Left = 8
    Top = 24
    Width = 177
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Hátralevõ idõ 2003.09.26. 17:34-ig:'
  end
  object Label4: TLabel
    Left = 192
    Top = 24
    Width = 177
    Height = 13
    AutoSize = False
    Caption = 'másodperc'
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 344
  end
end
