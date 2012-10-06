object frmEredmeny: TfrmEredmeny
  Left = 192
  Top = 107
  Width = 320
  Height = 238
  Caption = 'Eredmény'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 32
    Height = 13
    Caption = 'Neked'
  end
  object lblPont: TLabel
    Left = 56
    Top = 6
    Width = 57
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Caption = '123'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 120
    Top = 16
    Width = 63
    Height = 13
    Caption = 'pontod van a'
  end
  object lblOsszes: TLabel
    Left = 192
    Top = 5
    Width = 57
    Height = 36
    Alignment = taCenter
    AutoSize = False
    Caption = '123'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 256
    Top = 16
    Width = 39
    Height = 13
    Caption = '-ból/bõl.'
  end
  object Label6: TLabel
    Left = 16
    Top = 56
    Width = 106
    Height = 13
    Caption = 'Százalékos eredmény:'
  end
  object lblSzazalek: TLabel
    Left = 124
    Top = 44
    Width = 85
    Height = 37
    Alignment = taCenter
    AutoSize = False
    Caption = '100%'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label8: TLabel
    Left = 120
    Top = 120
    Width = 75
    Height = 37
    Caption = 'Jegy:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -33
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object lblJegy: TLabel
    Left = 232
    Top = 59
    Width = 74
    Height = 148
    Caption = '5'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -133
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 99
    Top = 176
    Width = 113
    Height = 25
    Caption = '&OK'
    Default = True
    TabOrder = 0
    OnClick = Button1Click
  end
end
