object Form1: TForm1
  Left = 192
  Top = 140
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
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Port:'
    TabOrder = 0
    Text = '12341'
  end
  object CheckBox1: TCheckBox
    Left = 16
    Top = 56
    Width = 97
    Height = 17
    Caption = 'bekapcsolva'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object ListBox1: TListBox
    Left = 168
    Top = 32
    Width = 641
    Height = 489
    ItemHeight = 13
    TabOrder = 2
  end
  object ServerSocket1: TServerSocket
    Active = False
    Port = 0
    ServerType = stNonBlocking
    OnClientConnect = ServerSocket1ClientConnect
    OnClientDisconnect = ServerSocket1ClientDisconnect
    OnClientRead = ServerSocket1ClientRead
    Left = 32
    Top = 80
  end
end
