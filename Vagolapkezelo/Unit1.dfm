object Form1: TForm1
  Left = 215
  Top = 115
  ActiveControl = btnOK
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Vágólapszerkesztõ'
  ClientHeight = 273
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020010000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000FFF
    FFF03FFFFFFC7FFFFFFE7FFF3FE2FFFF3FC7FFFF3F8FFFFF3F1FFFFF3F1FFFFF
    3E3FFFFF1C7FFFFF08FFFFFF00FFFF1F21FFFF1F31FFFE0F38FFFE4F3C7FFE4F
    3E3FFCE73F1FFCE73F8FFCE73FC7F9F3FFFFF9F3FFFFF1F1FFFFF3F9FFFFF3F9
    FFFFE7FCFFFFE7FCFFFFE7FCFFFF4FFE7FFE7FFFFFFE3FFFFFFC0FFFFFF0}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object txtSzovegmezo: TMemo
    Left = 0
    Top = 0
    Width = 473
    Height = 225
    Hint = 'A vágólap tartalma'
    ParentShowHint = False
    ScrollBars = ssBoth
    ShowHint = True
    TabOrder = 0
    WordWrap = False
  end
  object btnOK: TButton
    Left = 44
    Top = 232
    Width = 121
    Height = 33
    Hint = 'Ablak bezárása és a szövegmezõ tartalmának vágólapra másolása'
    Caption = 'OK'
    Default = True
    DragCursor = crNo
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 312
    Top = 232
    Width = 121
    Height = 33
    Hint = 'Ablak beárása a módosítások mentése nélkül'
    BiDiMode = bdLeftToRight
    Caption = 'Mégse'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentBiDiMode = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object PopupMenu1: TPopupMenu
    AutoHotkeys = maManual
    AutoPopup = False
    MenuAnimation = [maRightToLeft, maBottomToTop]
    Left = 272
    Top = 24
    object mnuEmpty: TMenuItem
      Caption = 'Törlés'
      Hint = 'Törli a vágólap tartalmát'
      OnClick = mnuEmptyClick
    end
    object mnuEdit: TMenuItem
      Caption = 'Szerkesztés'
      Hint = 'Megjelenít egy szerkesztõablakot'
      OnClick = mnuEditClick
    end
    object mnuWhatIsOn: TMenuItem
      Caption = 'Mi van rajra?'
      Hint = 'Megnézi, hogy mi van a vágólapon (pl. kép, szöveg)'
      OnClick = mnuWhatIsOnClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuAbout: TMenuItem
      Caption = 'Névjegy'
      OnClick = mnuAboutClick
    end
    object mnuExit: TMenuItem
      Caption = 'Kilépés'
      OnClick = mnuExitClick
    end
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 144
    Top = 88
  end
end
