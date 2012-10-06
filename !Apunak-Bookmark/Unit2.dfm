object frmModositas: TfrmModositas
  Left = 282
  Top = 217
  Width = 178
  Height = 109
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Módosítás'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 41
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Név:'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 41
    Height = 17
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'Cím:'
  end
  object txtNev: TEdit
    Left = 56
    Top = 4
    Width = 113
    Height = 21
    TabOrder = 0
  end
  object txtCim: TEdit
    Left = 56
    Top = 28
    Width = 113
    Height = 21
    TabOrder = 1
  end
  object BitBtn1: TBitBtn
    Left = 0
    Top = 56
    Width = 81
    Height = 25
    Caption = '&OK'
    TabOrder = 2
    Kind = bkOK
  end
  object BitBtn2: TBitBtn
    Left = 88
    Top = 56
    Width = 81
    Height = 25
    Caption = '&Mégse'
    TabOrder = 3
    Kind = bkCancel
  end
end
