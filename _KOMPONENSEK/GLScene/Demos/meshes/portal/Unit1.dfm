object Form1: TForm1
  Left = 78
  Top = 94
  Width = 706
  Height = 453
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 75
    Height = 18
    Caption = 'Maze Map'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 304
    Top = 8
    Width = 62
    Height = 18
    Caption = '3D View'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 24
    Top = 312
    Width = 241
    Height = 42
    Caption = 
      'To modify map, edit cells with keyboard :'#13#10'- any non-empty cell ' +
      'is a wall'#13#10'- click '#39'process'#39' to commit changes or check '#39'auto'#39
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object GLSceneViewer1: TGLSceneViewer
    Left = 296
    Top = 32
    Width = 393
    Height = 385
    FogEnvironment.FogColor.Color = {00000000000000008180003F0000803F}
    FogEnvironment.FogStart = 4
    FogEnvironment.FogEnd = 11
    FogEnvironment.FogMode = fmLinear
    BackgroundColor = clNavy
    Camera = GLCamera1
    Monitor = True
    Anchors = [akLeft, akTop, akRight, akBottom]
  end
  object BUForward: TButton
    Left = 96
    Top = 360
    Width = 89
    Height = 25
    Caption = 'Forward (Z/W)'
    TabOrder = 1
    OnClick = BUForwardClick
  end
  object BUTurnLeft: TButton
    Left = 8
    Top = 376
    Width = 81
    Height = 25
    Caption = 'Turn Left (Q/A)'
    TabOrder = 2
    OnClick = BUTurnLeftClick
  end
  object BUTurnRight: TButton
    Left = 192
    Top = 376
    Width = 89
    Height = 25
    Caption = 'TurnRight (D)'
    TabOrder = 3
    OnClick = BUTurnRightClick
  end
  object BUBackward: TButton
    Left = 96
    Top = 392
    Width = 89
    Height = 25
    Caption = 'Backward (S)'
    TabOrder = 4
    OnClick = BUBackwardClick
  end
  object SGMap: TStringGrid
    Left = 8
    Top = 32
    Width = 272
    Height = 272
    BorderStyle = bsNone
    ColCount = 16
    DefaultColWidth = 16
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 16
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
    ScrollBars = ssNone
    TabOrder = 5
    OnSetEditText = SGMapSetEditText
  end
  object BBProcess: TButton
    Left = 200
    Top = 8
    Width = 75
    Height = 17
    Caption = 'Process'
    TabOrder = 6
    OnClick = BBProcessClick
  end
  object CBAuto: TCheckBox
    Left = 152
    Top = 8
    Width = 41
    Height = 17
    Caption = 'Auto'
    TabOrder = 7
  end
  object CBFog: TCheckBox
    Left = 640
    Top = 8
    Width = 49
    Height = 17
    Caption = 'Fog'
    TabOrder = 8
    OnClick = CBFogClick
  end
  object GLScene1: TGLScene
    Left = 488
    object GLLightSource1: TGLLightSource
      Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      ConstAttenuation = 1
      Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Position.Coordinates = {000048420000C8420000C8420000803F}
      Specular.Color = {0000000000000000000000000000803F}
      SpotCutOff = 180
      SpotDirection.Coordinates = {0000000000000000000080BF00000000}
    end
    object DummyCube1: TDummyCube
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {00000000000000000000C0400000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      CubeSize = 1
      EdgeColor.Color = {0000803F0000803F0000803F0000803F}
      object GLCamera1: TGLCamera
        DepthOfView = 100
        FocalLength = 50
        Position.Coordinates = {000000000000003F000000000000803F}
        Direction.Coordinates = {0000000000000000000080BF00000000}
        Up.Coordinates = {000000800000803F0000000000000000}
        Left = 264
        Top = 144
      end
    end
    object Portal1: TPortal
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {00000000000000BF000000000000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      Material.BackProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      Material.BackProperties.Emission.Color = {0000000000000000000000000000803F}
      Material.BackProperties.Specular.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      Material.FrontProperties.Emission.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Specular.Color = {0000000000000000000000000000803F}
      Material.MaterialOptions = []
      MaterialLibrary = GLMaterialLibrary1
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <>
    Left = 528
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 568
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    SleepLength = 0
    OnProgress = GLCadencer1Progress
    Left = 448
  end
end
