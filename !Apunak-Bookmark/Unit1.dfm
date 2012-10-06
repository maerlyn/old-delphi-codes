object frmMainForm: TfrmMainForm
  Left = 258
  Top = 104
  AutoScroll = False
  Caption = 'Universal Bookmark-Editor'
  ClientHeight = 190
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 0
    Top = 0
    Width = 240
    Height = 190
    Align = alClient
    AutoExpand = True
    ChangeDelay = 10
    DragMode = dmAutomatic
    HideSelection = False
    Images = ImageList1
    Indent = 19
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ReadOnly = True
    ShowHint = False
    TabOrder = 0
    ToolTips = False
    OnCollapsed = TreeView1Collapsed
    OnDblClick = TreeView1DblClick
    OnDragDrop = TreeView1DragDrop
    OnDragOver = TreeView1DragOver
    OnExpanded = TreeView1Expanded
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.mdb'
    Filter = 'Adatb�zis-fileok (*.txt)|*.mdb|Minden file (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofFileMustExist]
    Title = 'Adatb�zis megnyit�sa'
    Left = 24
    Top = 8
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 88
    Top = 8
    object mnuModositas: TMenuItem
      Caption = '&M�dos�t�s'
      OnClick = mnuModositasClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnuUjBejegyzes: TMenuItem
      Caption = '�j &f�bejegyz�s'
      OnClick = mnuUjBejegyzesClick
    end
    object mnuUjAlbejegyzes: TMenuItem
      Caption = '�j &albejegyz�s'
      OnClick = mnuUjAlbejegyzesClick
    end
    object mnuBejegyzesTorlese: TMenuItem
      Caption = 'Bejegyz�s &t�rl�se'
      OnClick = mnuBejegyzesTorleseClick
    end
  end
  object ImageList1: TImageList
    Left = 152
    Top = 8
    Bitmap = {
      494C010105000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001001000000000000018
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
      000000000000000000000000000000000000000000000000000000000000E07F
      0000E07F0000E07F0000E07F0000E07F00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000E07F0000E07F0000E07F0000
      E07F0000E07F0000E07F0000E07F0000000000000000000000000000E07F0000
      E07F8C00E07F8C00E07F8C00E07F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      8C00E07F0000E07F0000E07F000000000000000000000000000000000000E07F
      0000E07F8C008C008C00E07F0000E07F000000000000000000000000FF030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF03000000000000000000000000E07F0000E07F00008C000000
      8C0000008C000000E07F0000E07F0000000000000000000000000000E07F0000
      E07F8C008C00E07F8C008C00E07F000000000000000000000000FF03FF030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF03FF03000000000000000000000000E07F0000E07F00008C00
      8C008C000000E07F0000E07F0000000000000000FF7FFF7FFF7F00000000E07F
      0000E07F8C008C008C00E07F0000E07F0000000000000000FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF03000000000000000000000000FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF030000000000000000E07F0000E07F8C008C008C00
      FF7F8C008C008C00E07F0000E07F000000000000FF7F0F000F000000E07F0000
      E07F8C00E07F8C00E07F8C00E07F00000000000000000000FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF03000000000000000000000000FF03FF03FF03FF03
      FF03FF03FF03FF03FF03FF0300000000000000000000E07F0000E07F00008C00
      8C008C000000E07F0000E07F0000000000000000FF7FFF7FFF7F00000000E07F
      0000E07F0000E07F0000E07F0000E07F00000000000000000000FF03FF030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF03FF0300000000000000000000E07F0000E07F00008C000000
      8C0000008C000000E07F0000E07F000000000000FF7F0F000F00000000000000
      00000000000000000000000000000000000000000000000000000000FF030000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF030000000000000000000000000000E07F0000E07F0000E07F
      8C00E07F0000E07F0000E07F0000000000000000FF7FFF7FFF7FFF7F0000E07F
      0000E07F00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000E07F0000E07F0000E07F0000
      E07F0000E07F0000E07F0000E07F000000000000FF7FE001E001FF7F0F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FE001E001FF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      0000000000000000000000000000000000000000FF7FE001E001FF7F0F000F00
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7F0000FF7F0000FF7F000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      0000E07F0000E07F000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000FF7F00000000000000000000000000000000
      0000000000000000000000000000000000000000E07F0000E07F0000E07F0000
      E07F0000E07F0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      0000E07F0000E07F000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000FF7F00000000000000000000000000000000
      0000000000000000000000000000000000000000E07F0000E07F0000E07F0000
      E07F0000E07F0000000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7FFF7FFF7F000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      0000E07F0000E07F000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7FFF7F0000FF7FE001E001E001FF7F00000000000000000000000000000000
      0000000000000000000000000000000000000000E07F0000E07F0000E07F0000
      E07F0000E07F0000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7F00000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000FF7F0000E003E003E00300000000007C007C00000000007C007C0000
      00000000000000000000000000000000000000000000E07F0000E07F0000E07F
      0000E07F0000E07F000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000007C007C007C007C00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F0000000000000000000000000000000000000000007C007C000000000000
      00000000000000000000000000000000000000000000E07F0000E07F00000000
      0000000000000000000000000000000000000000000000000000000000000000
      FF7F00000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000007C007C007C007C00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F00000000000000000000000000000000007C007C00000000007C007C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF7FFF7FFF7FFF7FFF7FFF7FFF7F
      FF7F000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E07F1863E07F1863
      E07F1863E07F00000000000000000000000000000000FF7FE07FFF7FE07FFF7F
      E07FFF7FE07FFF7F000000000000000000000000000000000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07F0000E07F1863E07F
      1863E07F1863E07F0000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07FE07F0000E07F1863
      E07F1863E07F1863E07F000000000000000000000000FF7FE07FFF7FE07FFF7F
      E07FFF7FE07FFF7F000000000000000000000000000000000000FF7F00000000
      000000000000FF7F000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FE07FE07F00000000
      00000000000000000000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7FE07FFF7FE07F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7F000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F000000000000000000000000E07FFF7FE07F0000E07F
      0000E07F0000E07F0000000000000000000000000000FF7FE07FFF7FE07FFF7F
      E07FFF7FE07FFF7F000000000000000000000000000000000000FF7F00000000
      0000FF7FFF7FFF7F000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF7FE07FFF7FE07FFF7F
      E07FFF7FE07F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7F00000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E07FFF7FE07FFF7FE07F
      FF7F00000000000000000000000000000000000000000000FF7FE07FFF7FE07F
      0000000000000000000000000000000000000000000000000000FF7F00000000
      FF7FFF7F0000FF7F000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000E07FFF7FE07FFF7F
      0000000000000000000000000000000000000000000010420000000000000000
      1042000000000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7F00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000010420000000000000000
      1042000000000000000000000000000000000000000000000000000000000000
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
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00304337382028574D6E643D2430303030
      73656E743A2068774D65737361676520201989048000000078F66900C8168904
      780C000000000000170000005C6E045236323929800000002028244646463732
      647261772E6578657320637573746F6D342050726F6365732430303437333046
      206C506172616D203030303030303032FFFF6172616D2024FFFF343336322920
      FFFF574D5F555345FFFF303030433738FC7F68776E643D24FC7F652073656E74
      FC7F00004D657373E00F89048C188904E00F690018000000E00F000000000000
      FC7F00005C6E0452FC7F392980000000FC7F244646463732FFFF61772E657865
      FFFF637573746F6DFFFF50726F636573FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFC00FE00FFFFFC03F800FE00FFFFF801F800FE00FFFFF800F
      800FE00FFFFF8007800FE00FE0078007800FE00FE007800F800FE00FE007800F
      801FE00FFFFF801FC0FFE00FFFFFC0FFC0FFE01FFFFFC0FFFFFFE03FFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
end