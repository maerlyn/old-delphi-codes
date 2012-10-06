object Form1: TForm1
  Left = 272
  Top = 160
  Width = 257
  Height = 348
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
  object Label1: TLabel
    Left = 14
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Sender'
  end
  object Label3: TLabel
    Left = 14
    Top = 56
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object Label4: TLabel
    Left = 14
    Top = 80
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label5: TLabel
    Left = 14
    Top = 104
    Width = 36
    Height = 13
    Caption = 'Subject'
  end
  object Label6: TLabel
    Left = 14
    Top = 136
    Width = 52
    Height = 13
    Caption = 'AttachFiles'
  end
  object Label7: TLabel
    Left = 14
    Top = 192
    Width = 24
    Height = 13
    Caption = 'Body'
  end
  object Edit1: TEdit
    Left = 56
    Top = 8
    Width = 185
    Height = 21
    TabOrder = 0
    Text = 'kldufgsh'
  end
  object Edit2: TEdit
    Left = 56
    Top = 56
    Width = 185
    Height = 21
    TabOrder = 1
    Text = '5555'
  end
  object Edit3: TEdit
    Left = 56
    Top = 104
    Width = 185
    Height = 21
    TabOrder = 2
    Text = '465454'
  end
  object CheckBox1: TCheckBox
    Left = 14
    Top = 32
    Width = 54
    Height = 17
    Alignment = taLeftJustify
    Caption = 'Read'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object DateTimePicker1: TDateTimePicker
    Left = 56
    Top = 80
    Width = 186
    Height = 21
    CalAlignment = dtaLeft
    Date = 38112.7392859954
    Time = 38112.7392859954
    DateFormat = dfShort
    DateMode = dmComboBox
    Kind = dtkDate
    ParseInput = False
    TabOrder = 4
  end
  object ListBox1: TListBox
    Left = 80
    Top = 128
    Width = 161
    Height = 57
    ItemHeight = 13
    Items.Strings = (
      'kkkd'
      'd'
      '2345')
    TabOrder = 5
  end
  object Memo1: TMemo
    Left = 56
    Top = 192
    Width = 185
    Height = 89
    Lines.Strings = (
      'w384qpj'
      'sjsjs848'
      'aa734an'
      'kjhllll')
    TabOrder = 6
  end
  object Button1: TButton
    Left = 8
    Top = 288
    Width = 105
    Height = 25
    Caption = 'Save'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 128
    Top = 288
    Width = 107
    Height = 25
    Caption = 'Load'
    TabOrder = 8
    OnClick = Button2Click
  end
  object POPperData1: TPOPperData
    Count = 0
    Left = 136
    Top = 32
  end
end
