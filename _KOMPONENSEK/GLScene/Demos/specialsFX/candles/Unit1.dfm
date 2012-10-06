object Form1: TForm1
  Left = 217
  Top = 107
  Width = 382
  Height = 288
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 374
    Height = 232
    FogEnvironment.FogColor.Color = {0000000000000000000000000000803F}
    FogEnvironment.FogStart = 10
    FogEnvironment.FogEnd = 1000
    FogEnvironment.FogMode = fmLinear
    BackgroundColor = clBlack
    Camera = GLCamera1
    Monitor = True
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 232
    Width = 374
    Height = 29
    Hint = 'Wind'
    Align = alBottom
    Max = 20
    Min = -20
    Orientation = trHorizontal
    ParentShowHint = False
    Frequency = 5
    Position = 0
    SelEnd = 0
    SelStart = 0
    ShowHint = True
    TabOrder = 1
    TickMarks = tmBottomRight
    TickStyle = tsAuto
    OnChange = TrackBar1Change
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000803F0000803F0000803F0000803F}
      ConstAttenuation = 1
      Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Position.Coordinates = {0000484200004842000048420000803F}
      Specular.Color = {0000000000000000000000000000803F}
      SpotCutOff = 180
      SpotDirection.Coordinates = {0000000000000000000080BF00000000}
    end
    object DummyCube1: TDummyCube
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {000000000000803F000000000000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      CubeSize = 1
      EdgeColor.Color = {0000803F0000803F0000803F0000803F}
    end
    object RevolutionSolid1: TRevolutionSolid
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {0000000000000000000000000000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      Material.BackProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
      Material.BackProperties.Emission.Color = {0000000000000000000000000000803F}
      Material.BackProperties.Specular.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
      Material.FrontProperties.Diffuse.Color = {0000803F8180003FC9C8C83E0000803F}
      Material.FrontProperties.Emission.Color = {0000000000000000000000000000803F}
      Material.FrontProperties.Specular.Color = {0000000000000000000000000000803F}
      Material.MaterialOptions = []
      Nodes = <
        item
          Y = 0.5
        end
        item
          Y = 0.5
          Z = 2
        end
        item
          Y = 0.300000011920929
          Z = 2
        end
        item
          Y = -0.5
          Z = 2
        end
        item
          Y = -0.5
        end>
      Division = 9
      SplineMode = lsmCubicSpline
      Slices = 19
      object Candle: TCylinder
        Direction.Coordinates = {00000000000000000000803F00000000}
        Position.Coordinates = {000000000000A03F9A99993F0000803F}
        Scale.Coordinates = {0000803F0000803F0000803F00000000}
        Up.Coordinates = {000000000000803F0000000000000000}
        Material.BackProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
        Material.BackProperties.Emission.Color = {0000000000000000000000000000803F}
        Material.BackProperties.Specular.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
        Material.FrontProperties.Diffuse.Color = {F1F0703FCBCA4A3FCBCA4A3F0000803F}
        Material.FrontProperties.Emission.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Specular.Color = {0000000000000000000000000000803F}
        Material.MaterialOptions = []
        BottomRadius = 0.200000002980232
        Height = 1.5
        Slices = 12
        Stacks = 1
        TopRadius = 0.200000002980232
        object Lines1: TLines
          Direction.Coordinates = {00000000000000000000803F00000000}
          Position.Coordinates = {000000006666663F000000000000803F}
          Scale.Coordinates = {0000803F0000803F0000803F00000000}
          Up.Coordinates = {000000000000803F0000000000000000}
          LineColor.Color = {A9A5253FB1A8283EB1A8283E0000803F}
          LineWidth = 2
          Nodes = <
            item
              Y = -0.200000002980232
              Color.Color = {00000000000000000000803F0000803F}
            end
            item
              X = 0.0500000007450581
              Z = 0.0500000007450581
              Color.Color = {00000000000000000000803F0000803F}
            end
            item
              X = -0.0500000007450581
              Y = 0.100000001490116
              Z = -0.0500000007450581
              Color.Color = {00000000000000000000803F0000803F}
            end>
          NodeColor.Color = {00000000000000000000803F0000803F}
          NodesAspect = lnaInvisible
          Options = []
          EffectsData = {
            0201060A54474C4246697265465802000610474C4669726546584D616E616765
            7231}
        end
        object DummyCube2: TDummyCube
          Direction.Coordinates = {F204353F00000000F40435BF00000000}
          Position.Coordinates = {0000000000000000000000000000803F}
          Scale.Coordinates = {0000803F0000803F0000803F00000000}
          Up.Coordinates = {000000000000803F0000000000000000}
          CubeSize = 1
          EdgeColor.Color = {0000803F0000803F0000803F0000803F}
          object Plane1: TPlane
            Direction.Coordinates = {000000000000803F2CBD3BB300000000}
            Position.Coordinates = {0000C03FA4703DBF000000000000803F}
            Scale.Coordinates = {0000803F0000803F0000803F00000000}
            Up.Coordinates = {000000B32FBD3BB3000080BF00000000}
            Material.BackProperties.Ambient.Color = {CDCC4C3ECDCC4C3ECDCC4C3E0000803F}
            Material.BackProperties.Diffuse.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
            Material.BackProperties.Emission.Color = {0000000000000000000000000000803F}
            Material.BackProperties.Specular.Color = {0000000000000000000000000000803F}
            Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
            Material.FrontProperties.Diffuse.Color = {0000000000000000000000003333B33E}
            Material.FrontProperties.Emission.Color = {0000000000000000000000000000803F}
            Material.FrontProperties.Specular.Color = {0000000000000000000000000000803F}
            Material.BlendingMode = bmTransparency
            Material.MaterialOptions = []
            Height = 0.400000005960464
            Width = 3
          end
        end
      end
      object GLProxyObject1: TGLProxyObject
        MasterObject = Candle
        Direction.Coordinates = {00000000000000000000803F00000000}
        Position.Coordinates = {0000803F0000A03F9A9919BF0000803F}
        Scale.Coordinates = {0000803F0000803F0000803F00000000}
        Up.Coordinates = {000000000000803F0000000000000000}
      end
      object GLProxyObject2: TGLProxyObject
        MasterObject = Candle
        Direction.Coordinates = {00000000000000000000803F00000000}
        Position.Coordinates = {000080BF0000A03F9A9919BF0000803F}
        Scale.Coordinates = {0000803F0000803F0000803F00000000}
        Up.Coordinates = {000000000000803F0000000000000000}
      end
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 100
      TargetObject = DummyCube1
      Position.Coordinates = {000070410000E040000040400000803F}
      Direction.Coordinates = {0000000000000000000080BF00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      Left = 192
      Top = 120
    end
  end
  object GLFireFXManager1: TGLFireFXManager
    FireDir.Coordinates = {000000009A99993E0000000000000000}
    InitialDir.Coordinates = {00000000CDCC4C3E0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 128
    ParticleSize = 0.150000005960464
    InnerColor.Color = {0000803F0000803F000000000000803F}
    OuterColor.Color = {0000803F0000003F000000000000803F}
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    ParticleLife = 2
    FireBurst = 1
    FireRadius = 0.100000001490116
    Disabled = False
    Paused = False
    ParticleInterval = 0.0399999991059303
    UseInterval = True
    Left = 64
    Top = 48
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Left = 64
    Top = 88
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 16
    Top = 8
  end
end
