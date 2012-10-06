object Form5: TForm5
  Left = 481
  Top = 292
  Width = 429
  Height = 488
  Caption = 'Form5'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 421
    Height = 458
    ActivePage = TabSheet3
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'C'#233'g'
      object CheckBox1: TCheckBox
        Left = 16
        Top = 32
        Width = 97
        Height = 17
        Caption = 'C'#233'g neve:'
        TabOrder = 0
      end
      object Edit1: TEdit
        Left = 136
        Top = 32
        Width = 169
        Height = 21
        MaxLength = 30
        TabOrder = 1
      end
      object CheckBox2: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Caption = 'C'#237'm:'
        TabOrder = 2
      end
      object CheckBox3: TCheckBox
        Left = 16
        Top = 112
        Width = 97
        Height = 17
        Caption = 'Telefonsz'#225'm:'
        TabOrder = 3
      end
      object CheckBox4: TCheckBox
        Left = 16
        Top = 152
        Width = 97
        Height = 17
        Caption = 'Faxsz'#225'm:'
        TabOrder = 4
      end
      object CheckBox5: TCheckBox
        Left = 16
        Top = 192
        Width = 97
        Height = 17
        Caption = 'Kapcsolattart'#243':'
        TabOrder = 5
      end
      object CheckBox6: TCheckBox
        Left = 16
        Top = 232
        Width = 97
        Height = 17
        Caption = 'Ad'#243'sz'#225'm:'
        TabOrder = 6
      end
      object CheckBox7: TCheckBox
        Left = 16
        Top = 272
        Width = 97
        Height = 17
        Caption = 'C'#233'gvezet'#337':'
        TabOrder = 7
      end
      object CheckBox8: TCheckBox
        Left = 16
        Top = 312
        Width = 97
        Height = 17
        Caption = 'E-mail c'#237'm:'
        TabOrder = 8
      end
      object CheckBox9: TCheckBox
        Left = 16
        Top = 352
        Width = 97
        Height = 17
        Caption = 'Honlap:'
        TabOrder = 9
      end
      object Edit2: TEdit
        Left = 136
        Top = 72
        Width = 169
        Height = 21
        MaxLength = 100
        TabOrder = 10
      end
      object Edit3: TEdit
        Left = 136
        Top = 112
        Width = 169
        Height = 21
        MaxLength = 50
        TabOrder = 11
      end
      object Edit4: TEdit
        Left = 136
        Top = 152
        Width = 169
        Height = 21
        MaxLength = 50
        TabOrder = 12
      end
      object Edit5: TEdit
        Left = 136
        Top = 192
        Width = 169
        Height = 21
        MaxLength = 35
        TabOrder = 13
      end
      object Edit6: TEdit
        Left = 136
        Top = 232
        Width = 169
        Height = 21
        MaxLength = 10
        TabOrder = 14
      end
      object Edit7: TEdit
        Left = 136
        Top = 272
        Width = 169
        Height = 21
        MaxLength = 35
        TabOrder = 15
      end
      object Edit8: TEdit
        Left = 136
        Top = 312
        Width = 169
        Height = 21
        MaxLength = 40
        TabOrder = 16
      end
      object Edit9: TEdit
        Left = 136
        Top = 352
        Width = 169
        Height = 21
        MaxLength = 40
        TabOrder = 17
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Term'#233'k'
      ImageIndex = 1
      object Label1: TLabel
        Left = 16
        Top = 32
        Width = 66
        Height = 13
        Caption = 'Term'#233'k neve:'
      end
      object Edit10: TEdit
        Left = 144
        Top = 32
        Width = 169
        Height = 21
        MaxLength = 30
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Egy'#233'b'
      ImageIndex = 2
      object CheckBox10: TCheckBox
        Left = 16
        Top = 32
        Width = 97
        Height = 17
        Caption = #193'llapot:'
        TabOrder = 0
      end
      object CheckBox11: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Caption = 'Megjegyz'#233's:'
        TabOrder = 1
      end
      object CheckBox12: TCheckBox
        Left = 16
        Top = 112
        Width = 121
        Height = 17
        Caption = 'Megjegyz'#233's d'#225'tuma:'
        TabOrder = 2
      end
      object CheckBox13: TCheckBox
        Left = 16
        Top = 152
        Width = 97
        Height = 17
        Caption = 'Elv'#233'gzend'#337':'
        TabOrder = 3
      end
      object Edit11: TEdit
        Left = 144
        Top = 32
        Width = 169
        Height = 21
        MaxLength = 5
        TabOrder = 4
      end
      object Edit12: TEdit
        Left = 144
        Top = 64
        Width = 169
        Height = 21
        TabOrder = 5
      end
      object DateTimePicker1: TDateTimePicker
        Left = 240
        Top = 112
        Width = 81
        Height = 21
        Date = 38497.104285983800000000
        Time = 38497.104285983800000000
        TabOrder = 6
      end
      object DateTimePicker2: TDateTimePicker
        Left = 328
        Top = 112
        Width = 81
        Height = 21
        Date = 38497.105227071760000000
        Time = 38497.105227071760000000
        Kind = dtkTime
        TabOrder = 7
      end
      object CheckBox14: TCheckBox
        Left = 144
        Top = 152
        Width = 97
        Height = 17
        Caption = 'nem'
        TabOrder = 8
        OnClick = CheckBox14Click
      end
      object Button3: TButton
        Left = 152
        Top = 112
        Width = 75
        Height = 25
        Caption = #250'jabb, mint'
        TabOrder = 9
        OnClick = Button3Click
      end
    end
  end
  object Button1: TButton
    Left = 141
    Top = 416
    Width = 75
    Height = 25
    Caption = 'Keres'#233's'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 224
    Top = 416
    Width = 75
    Height = 25
    Caption = 'M'#233'gse'
    ModalResult = 2
    TabOrder = 2
  end
end
