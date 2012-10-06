object Form1: TForm1
  Left = 268
  Top = 271
  Width = 497
  Height = 402
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
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 36
    Height = 13
    Caption = 'User ID'
  end
  object Label3: TLabel
    Left = 8
    Top = 72
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object Label4: TLabel
    Left = 8
    Top = 104
    Width = 23
    Height = 13
    Caption = 'From'
  end
  object Label5: TLabel
    Left = 8
    Top = 136
    Width = 36
    Height = 13
    Caption = 'Subject'
  end
  object Label6: TLabel
    Left = 8
    Top = 168
    Width = 57
    Height = 13
    Caption = 'Message ID'
  end
  object Edit1: TEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'pop3.mailbox.hu'
  end
  object Edit2: TEdit
    Left = 72
    Top = 40
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'maerlyn@mailbox.hu'
  end
  object Edit3: TEdit
    Left = 72
    Top = 72
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    Text = '9809751753'
  end
  object Edit4: TEdit
    Left = 72
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object Edit5: TEdit
    Left = 72
    Top = 136
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object Edit6: TEdit
    Left = 72
    Top = 168
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object Button1: TButton
    Left = 208
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Connect/Dis.'
    TabOrder = 6
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 208
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Summarize'
    TabOrder = 7
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 208
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 8
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 208
    Top = 104
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 9
    OnClick = Button4Click
  end
  object Memo1: TMemo
    Left = 296
    Top = 8
    Width = 185
    Height = 89
    Lines.Strings = (
      'Status messages')
    ScrollBars = ssVertical
    TabOrder = 10
  end
  object Memo2: TMemo
    Left = 296
    Top = 104
    Width = 185
    Height = 89
    Lines.Strings = (
      'Message header')
    ScrollBars = ssVertical
    TabOrder = 11
  end
  object Button5: TButton
    Left = 208
    Top = 168
    Width = 75
    Height = 25
    Caption = 'Get it'
    TabOrder = 12
    OnClick = Button5Click
  end
  object Memo3: TMemo
    Left = 8
    Top = 200
    Width = 473
    Height = 169
    Lines.Strings = (
      'Memo3')
    TabOrder = 13
  end
  object NMPOP31: TNMPOP3
    Port = 110
    ReportLevel = 0
    OnConnect = NMPOP31Connect
    Parse = False
    DeleteOnRead = False
    OnAuthenticationNeeded = NMPOP31AuthenticationNeeded
    OnAuthenticationFailed = NMPOP31AuthenticationFailed
    OnReset = NMPOP31Reset
    OnList = NMPOP31List
    OnRetrieveStart = NMPOP31RetrieveStart
    OnRetrieveEnd = NMPOP31RetrieveEnd
    OnSuccess = NMPOP31Success
    OnFailure = NMPOP31Failure
    Left = 312
    Top = 152
  end
end
