object Form1: TForm1
  Left = 323
  Top = 118
  Width = 401
  Height = 548
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
    Left = 200
    Top = 8
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 200
    Top = 32
    Width = 33
    Height = 13
    Caption = 'UserID'
  end
  object Label3: TLabel
    Left = 200
    Top = 56
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label4: TLabel
    Left = 200
    Top = 80
    Width = 61
    Height = 13
    Caption = 'FromAddress'
  end
  object Label5: TLabel
    Left = 200
    Top = 104
    Width = 51
    Height = 13
    Caption = 'FromName'
  end
  object Label6: TLabel
    Left = 200
    Top = 128
    Width = 65
    Height = 13
    Caption = 'LocalProgram'
  end
  object Label7: TLabel
    Left = 200
    Top = 152
    Width = 40
    Height = 13
    Caption = 'ReplyTo'
  end
  object Label8: TLabel
    Left = 200
    Top = 176
    Width = 36
    Height = 13
    Caption = 'Subject'
  end
  object Label9: TLabel
    Left = 200
    Top = 200
    Width = 59
    Height = 13
    Caption = 'Attachments'
  end
  object Label10: TLabel
    Left = 10
    Top = 1
    Width = 51
    Height = 13
    Caption = 'ToAddress'
  end
  object Label11: TLabel
    Left = 10
    Top = 99
    Width = 94
    Height = 13
    Caption = 'ToBlindCarbonCopy'
  end
  object Label12: TLabel
    Left = 10
    Top = 194
    Width = 71
    Height = 13
    Caption = 'ToCarbonCopy'
  end
  object Label13: TLabel
    Left = 10
    Top = 290
    Width = 24
    Height = 13
    Caption = 'Body'
  end
  object Label14: TLabel
    Left = 10
    Top = 387
    Width = 30
    Height = 13
    Caption = 'Status'
  end
  object Memo1: TMemo
    Left = 8
    Top = 16
    Width = 185
    Height = 81
    Lines.Strings = (
      'noa86@emitelnet.hu')
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 8
    Top = 112
    Width = 185
    Height = 81
    TabOrder = 1
  end
  object Memo3: TMemo
    Left = 8
    Top = 208
    Width = 185
    Height = 81
    TabOrder = 2
  end
  object Memo4: TMemo
    Left = 8
    Top = 304
    Width = 185
    Height = 81
    Lines.Strings = (
      '')
    TabOrder = 3
  end
  object Memo5: TMemo
    Left = 8
    Top = 400
    Width = 185
    Height = 81
    Lines.Strings = (
      'Status')
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 272
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'mail.axelero.hu'
  end
  object Edit2: TEdit
    Left = 272
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'paspec'
  end
  object Edit3: TEdit
    Left = 272
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 7
    Text = '200404'
  end
  object Edit4: TEdit
    Left = 272
    Top = 80
    Width = 121
    Height = 21
    TabOrder = 8
    Text = 'maerlyn@mailbox.hu'
  end
  object Edit5: TEdit
    Left = 272
    Top = 104
    Width = 121
    Height = 21
    TabOrder = 9
    Text = 'Maerlyn'
  end
  object Edit6: TEdit
    Left = 272
    Top = 128
    Width = 121
    Height = 21
    TabOrder = 10
    Text = 'Own'
  end
  object Edit7: TEdit
    Left = 272
    Top = 152
    Width = 121
    Height = 21
    TabOrder = 11
    Text = 'maerlyn@mailbox.hu'
  end
  object Edit8: TEdit
    Left = 272
    Top = 176
    Width = 121
    Height = 21
    TabOrder = 12
    Text = 'e-mail'
  end
  object ListBox1: TListBox
    Left = 200
    Top = 216
    Width = 193
    Height = 89
    ItemHeight = 13
    TabOrder = 13
    OnKeyDown = ListBox1KeyDown
  end
  object Button1: TButton
    Left = 200
    Top = 312
    Width = 193
    Height = 25
    Caption = 'Connect / Disconnect'
    TabOrder = 14
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 344
    Width = 193
    Height = 25
    Caption = 'SendMail'
    TabOrder = 15
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 200
    Top = 376
    Width = 193
    Height = 25
    Caption = 'ClearAll'
    TabOrder = 16
    OnClick = Button3Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 488
    Width = 97
    Height = 17
    Caption = 'ClearParams'
    TabOrder = 17
  end
  object RadioGroup1: TRadioGroup
    Left = 200
    Top = 408
    Width = 185
    Height = 105
    Caption = 'RadioGroup1'
    ItemIndex = 0
    Items.Strings = (
      'MIME'
      'UUEncode')
    TabOrder = 18
  end
  object NMSMTP1: TNMSMTP
    Port = 25
    ReportLevel = 0
    OnConnect = NMSMTP1Connect
    EncodeType = uuMime
    ClearParams = True
    SubType = mtPlain
    Charset = 'us-ascii'
    OnRecipientNotFound = NMSMTP1RecipientNotFound
    OnHeaderIncomplete = NMSMTP1HeaderIncomplete
    OnSendStart = NMSMTP1SendStart
    OnSuccess = NMSMTP1Success
    OnFailure = NMSMTP1Failure
    OnEncodeStart = NMSMTP1EncodeStart
    OnEncodeEnd = NMSMTP1EncodeEnd
    OnAttachmentNotFound = NMSMTP1AttachmentNotFound
    OnAuthenticationFailed = NMSMTP1AuthenticationFailed
    Left = 160
    Top = 488
  end
  object OpenDialog1: TOpenDialog
    Title = 'Attach'
    Left = 128
    Top = 488
  end
end
