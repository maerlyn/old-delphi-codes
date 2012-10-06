object Form1: TForm1
  Left = 192
  Top = 107
  AutoScroll = False
  Caption = 'Törigyakorló - adatbázisszerkesztõ'
  ClientHeight = 202
  ClientWidth = 270
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 80
    Width = 85
    Height = 13
    Caption = 'Aktuális esemény:'
  end
  object Label2: TLabel
    Left = 8
    Top = 48
    Width = 68
    Height = 13
    Caption = 'Megnyitott file:'
  end
  object lblFileNeve: TLabel
    Left = 80
    Top = 48
    Width = 225
    Height = 13
    AutoSize = False
  end
  object Label4: TLabel
    Left = 17
    Top = 128
    Width = 46
    Height = 13
    Caption = 'Esemény:'
  end
  object Label5: TLabel
    Left = 24
    Top = 160
    Width = 40
    Height = 13
    Caption = 'Évszám:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'File &menyitása'
    TabOrder = 0
    OnClick = Button1Click
  end
  object sedAktualisEsemeny: TSpinEdit
    Left = 103
    Top = 77
    Width = 113
    Height = 22
    Enabled = False
    MaxValue = 0
    MinValue = 0
    TabOrder = 1
    Value = 1
    OnChange = sedAktualisEsemenyChange
    OnClick = sedAktualisEsemenyClick
  end
  object txtEsemeny: TEdit
    Left = 72
    Top = 126
    Width = 177
    Height = 21
    TabOrder = 2
    OnExit = txtEsemenyExit
  end
  object txtEvszam: TEdit
    Left = 72
    Top = 158
    Width = 177
    Height = 21
    TabOrder = 3
    OnExit = txtEvszamExit
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 183
    Width = 270
    Height = 19
    Panels = <
      item
        Width = 270
      end>
    SimplePanel = False
  end
  object OpenDialog1: TOpenDialog
    Left = 192
    Top = 8
  end
end
