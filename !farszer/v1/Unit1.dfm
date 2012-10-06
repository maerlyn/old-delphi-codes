object Form1: TForm1
  Left = 307
  Top = 218
  Width = 696
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 392
    Top = 40
    Width = 121
    Height = 161
    ItemHeight = 13
    Sorted = True
    TabOrder = 0
    Visible = False
  end
  object StringGrid1: TStringGrid
    Left = 0
    Top = 0
    Width = 688
    Height = 453
    Align = alClient
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    PopupMenu = PopupMenu1
    TabOrder = 1
    OnDblClick = StringGrid1DblClick
  end
  object PopupMenu1: TPopupMenu
    Left = 168
    Top = 144
    object jcg1: TMenuItem
      Caption = #218'j c'#233'g'
      OnClick = jcg1Click
    end
    object jtermk1: TMenuItem
      Caption = #218'j term'#233'k'
      OnClick = jtermk1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cgtrlse1: TMenuItem
      Caption = 'C'#233'g t'#246'rl'#233'se'
      OnClick = Cgtrlse1Click
    end
    object Cgtneve1: TMenuItem
      Caption = 'C'#233'g '#225'tnevez'#233'se'
      OnClick = Cgtneve1Click
    end
    object ermktrlse1: TMenuItem
      Caption = 'Term'#233'k t'#246'rl'#233'se'
      OnClick = ermktrlse1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Keress1: TMenuItem
      Caption = 'Keres'#233's'
      OnClick = Keress1Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 168
    Top = 184
  end
end
