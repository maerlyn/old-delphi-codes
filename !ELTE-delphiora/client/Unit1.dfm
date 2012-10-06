object Form1: TForm1
  Left = 244
  Top = 247
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
  PixelsPerInch = 96
  TextHeight = 13
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 16
    Width = 121
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Nick'
    TabOrder = 0
    Text = 'Maerlyn'
  end
  object LabeledEdit2: TLabeledEdit
    Left = 8
    Top = 56
    Width = 121
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Host'
    TabOrder = 1
    Text = 'localhost'
  end
  object LabeledEdit3: TLabeledEdit
    Left = 8
    Top = 96
    Width = 121
    Height = 21
    EditLabel.Width = 19
    EditLabel.Height = 13
    EditLabel.Caption = 'Port'
    TabOrder = 2
    Text = '12341'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 128
    Width = 97
    Height = 17
    Caption = 'bekapcsolva'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
  object ListBox1: TListBox
    Left = 144
    Top = 16
    Width = 705
    Height = 561
    ItemHeight = 13
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 144
    Top = 584
    Width = 705
    Height = 21
    TabOrder = 5
    Text = 'Edit1'
    OnKeyDown = Edit1KeyDown
  end
  object ClientSocket1: TClientSocket
    Active = False
    ClientType = ctNonBlocking
    Port = 0
    OnRead = ClientSocket1Read
    Left = 24
    Top = 176
  end
end
