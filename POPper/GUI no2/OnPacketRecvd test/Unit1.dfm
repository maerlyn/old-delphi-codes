object Form1: TForm1
  Left = 276
  Top = 179
  Width = 674
  Height = 480
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
  object Label1: TLabel
    Left = 8
    Top = 128
    Width = 77
    Height = 13
    Caption = 'Current progress'
  end
  object Label2: TLabel
    Left = 8
    Top = 152
    Width = 67
    Height = 13
    Caption = 'Total progress'
  end
  object Button1: TButton
    Left = 8
    Top = 80
    Width = 121
    Height = 25
    Caption = 'Do it!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'host'
  end
  object Edit2: TEdit
    Left = 8
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'username'
  end
  object Edit3: TEdit
    Left = 8
    Top = 56
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 3
    Text = 'password'
  end
  object ListView1: TListView
    Left = 136
    Top = 8
    Width = 521
    Height = 105
    Columns = <
      item
        Width = 0
      end
      item
        Caption = 'Levél'
        Width = 150
      end
      item
        Caption = 'Méret'
        Width = 150
      end>
    TabOrder = 4
    ViewStyle = vsReport
  end
  object ProgressBar1: TProgressBar
    Left = 88
    Top = 128
    Width = 569
    Height = 17
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 5
  end
  object ProgressBar2: TProgressBar
    Left = 88
    Top = 152
    Width = 569
    Height = 17
    Min = 0
    Max = 100
    Smooth = True
    TabOrder = 6
  end
  object ListView2: TListView
    Left = 8
    Top = 176
    Width = 649
    Height = 265
    Columns = <
      item
        Width = 0
      end
      item
        Caption = 'MailNum'
        Width = 150
      end
      item
        Caption = 'BytesRecvd'
        Width = 150
      end
      item
        Caption = 'BytesTotal'
        Width = 150
      end>
    TabOrder = 7
    ViewStyle = vsReport
  end
  object NMPOP31: TNMPOP3
    Port = 110
    ReportLevel = 0
    OnPacketRecvd = NMPOP31PacketRecvd
    Parse = False
    DeleteOnRead = False
    OnList = NMPOP31List
    Left = 208
    Top = 64
  end
end
