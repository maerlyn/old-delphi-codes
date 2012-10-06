object frmWebBrowser: TfrmWebBrowser
  Left = 192
  Top = 107
  Width = 506
  Height = 375
  Caption = 'Telefonkönyv v4.0 - Miniböngészõ'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 7
    Width = 21
    Height = 13
    Caption = 'Cím:'
  end
  object WebBrowser1: TWebBrowser
    Left = 5
    Top = 28
    Width = 388
    Height = 179
    TabOrder = 0
    OnDownloadBegin = WebBrowser1DownloadBegin
    OnDownloadComplete = WebBrowser1DownloadComplete
    ControlData = {
      4C0000001A280000801200000100000005000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object txtLocation: TEdit
    Left = 46
    Top = 3
    Width = 330
    Height = 21
    TabOrder = 1
    OnKeyPress = txtLocationKeyPress
  end
  object Button1: TButton
    Left = 387
    Top = 3
    Width = 97
    Height = 21
    Caption = 'Letöltés leállítása'
    TabOrder = 2
    OnClick = Button1Click
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 329
    Width = 498
    Height = 19
    Panels = <
      item
        Width = 100
      end>
    SimplePanel = False
  end
end
