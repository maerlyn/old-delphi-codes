object Form1: TForm1
  Left = 516
  Top = 325
  Width = 186
  Height = 444
  Caption = 'bmplevago'
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
    Left = 8
    Top = 40
    Width = 161
    Height = 33
    AutoSize = False
    Caption = 'C:\'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 272
    Width = 21
    Height = 13
    Caption = 'Fent'
  end
  object Label3: TLabel
    Left = 8
    Top = 296
    Width = 21
    Height = 13
    Caption = 'Lent'
  end
  object Label4: TLabel
    Left = 8
    Top = 320
    Width = 15
    Height = 13
    Caption = 'Bal'
  end
  object Label5: TLabel
    Left = 8
    Top = 344
    Width = 23
    Height = 13
    Caption = 'Jobb'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 161
    Height = 25
    Caption = 'Mappa v'#225'laszt'#225's'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ListBox1: TListBox
    Left = 8
    Top = 80
    Width = 161
    Height = 185
    ItemHeight = 13
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 48
    Top = 272
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '0'
    OnExit = Edit1Exit
  end
  object Edit2: TEdit
    Left = 48
    Top = 296
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '0'
  end
  object Edit3: TEdit
    Left = 48
    Top = 320
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '0'
  end
  object Edit4: TEdit
    Left = 48
    Top = 344
    Width = 121
    Height = 21
    TabOrder = 5
    Text = '0'
    OnExit = Edit1Exit
  end
  object Button2: TButton
    Left = 8
    Top = 376
    Width = 161
    Height = 25
    Caption = 'Nosza!'
    TabOrder = 6
    OnClick = Button2Click
  end
end
