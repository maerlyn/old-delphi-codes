object frmBeallitasok: TfrmBeallitasok
  Left = 247
  Top = 176
  BorderStyle = bsSingle
  Caption = 'Beállítások'
  ClientHeight = 164
  ClientWidth = 225
  Color = clBtnFace
  Font.Charset = EASTEUROPE_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsMDIChild
  Icon.Data = {
    0000010001002020100001000400E80200001600000028000000200000004000
    0000010004000000000000000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000007777770000000000000000
    0000000000000007000000000000000000000000000000070000000000000000
    0000000000777007000000000000000000077070007770070000700000000000
    0077000700787000000007000000000007708000077877000070007000000000
    07088807777777770777000700000000008F88877FFFFF077887700700000000
    00088888F88888FF08870070000000000000880888877778F070007000000007
    77088888880007778F770077777000700008F088007777077F07000000700700
    008F08880800077778F7700000700708888F0880F08F807078F7777700700708
    F88F0780F070F07078F7887700700708888F0780F077807088F7777700700700
    008F0788FF00080888F77000007000000008F0780FFFF0088F77007000000000
    0008F07788000888887700700000000000008F07788888880870007000000000
    00088FF0077788088887000700000000008F888FF00000F87887700700000000
    0708F8088FFFFF88078700700000000007708000088888000070070000000000
    0077007000888007000070000000000000077700008F80070007000000000000
    0000000000888007000000000000000000000000000000070000000000000000
    000000000777777700000000000000000000000000000000000000000000FFFF
    FFFFFFFC0FFFFFFC0FFFFFF80FFFFFF80FFFFE180E7FFC00043FF800001FF800
    000FF800000FFC00001FFE00001FE0000001C000000180000001800000018000
    00018000000180000001FC00001FFC00001FFE00001FFC00000FF800000FF800
    001FF800003FFC180C7FFE380EFFFFF80FFFFFF80FFFFFF80FFFFFFFFFFF}
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BitBtn1: TSRColorButton
    Left = 8
    Top = 139
    Width = 93
    Height = 25
    AllowTimer = False
    BevelWidth = 1
    BorderColor = clBlack
    BorderStyle = bsNormal
    Caption = '&OK'
    ChangeDirection = False
    Color = clBtnFace
    ContrastHighlight = 5
    ContrastShadow = 6
    Down = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    GradientDirection = gdDownRight
    GradientStyle = gsNone
    Layout = blGlyphLeft
    Margin = 1
    NumGlyphs = 2
    Spacing = 1
    TimerDelay = 400
    TimerInterval = 100
    OnClick = BitBtn1Click
  end
  object BitBtn2: TSRColorButton
    Left = 121
    Top = 139
    Width = 93
    Height = 25
    AllowTimer = False
    BevelWidth = 1
    BorderColor = clBlack
    BorderStyle = bsNormal
    Caption = '&Mégse'
    ChangeDirection = False
    Color = clBtnFace
    ContrastHighlight = 5
    ContrastShadow = 6
    Down = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    GradientDirection = gdDownRight
    GradientStyle = gsNone
    Layout = blGlyphLeft
    Margin = 1
    NumGlyphs = 2
    Spacing = 1
    TimerDelay = 400
    TimerInterval = 100
    OnClick = BitBtn2Click
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 225
    Height = 138
    ActivePage = TabSheet3
    MultiLine = True
    OwnerDraw = True
    TabOrder = 0
    OnDrawTab = PageControl1DrawTab
    object TabSheet1: TTabSheet
      Caption = 'Türelmi idõ'
      object Label1: TLabel
        Left = 20
        Top = 8
        Width = 81
        Height = 13
        Caption = 'Türelmi idõ (nap):'
      end
      object Label2: TLabel
        Left = 8
        Top = 40
        Width = 93
        Height = 13
        Caption = 'Késési díj (FT/nap):'
      end
      object SpeedButton1: TSpeedButton
        Left = 4
        Top = 88
        Width = 205
        Height = 22
        Caption = 'Alapbeállítások visszaállítása'
        Flat = True
        OnClick = SpeedButton1Click
      end
      object sedTurelmiIdo: TSpinEdit
        Left = 116
        Top = 4
        Width = 101
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 0
        Value = 2
      end
      object sedForintPerNap: TSpinEdit
        Left = 116
        Top = 37
        Width = 101
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 1
        Value = 0
      end
      object cbxProgramInditasakor: TCheckBox
        Left = 10
        Top = 64
        Width = 207
        Height = 17
        Caption = 'Program indításakor ellenõrzés'
        TabOrder = 2
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Kategóriák'
      ImageIndex = 1
      object Memo1: TMemo
        Left = 0
        Top = 0
        Width = 217
        Height = 105
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Számla'
      ImageIndex = 2
      object Label3: TLabel
        Left = 16
        Top = 24
        Width = 35
        Height = 13
        Caption = 'Neved:'
      end
      object Label4: TLabel
        Left = 16
        Top = 64
        Width = 33
        Height = 13
        Caption = 'Címed:'
      end
      object txtNeved: TEdit
        Left = 64
        Top = 21
        Width = 145
        Height = 21
        TabOrder = 0
      end
      object txtCimed: TEdit
        Left = 64
        Top = 61
        Width = 145
        Height = 21
        TabOrder = 1
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Skin'
      ImageIndex = 3
      object Label5: TLabel
        Left = 0
        Top = 0
        Width = 138
        Height = 13
        Caption = 'Válassz "bõrt" a programnak:'
      end
      object lblSkin: TLabel
        Left = 0
        Top = 95
        Width = 217
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'Aktuális:'
      end
      object lstSkinek: TListBox
        Left = 0
        Top = 16
        Width = 217
        Height = 76
        ItemHeight = 13
        Sorted = True
        TabOrder = 0
      end
      object dlbSkinek: TDirectoryListBox
        Left = 64
        Top = 32
        Width = 33
        Height = 33
        ItemHeight = 16
        TabOrder = 1
        Visible = False
      end
    end
  end
end
