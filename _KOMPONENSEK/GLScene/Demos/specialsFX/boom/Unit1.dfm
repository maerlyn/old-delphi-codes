object Form1: TForm1
  Left = 151
  Top = 91
  Width = 527
  Height = 341
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
    Width = 519
    Height = 314
    FogEnvironment.FogColor.Color = {0000000000000000000000000000803F}
    FogEnvironment.FogStart = 10
    FogEnvironment.FogEnd = 1000
    FogEnvironment.FogMode = fmLinear
    BackgroundColor = clBlack
    Camera = GLCamera1
    DepthTest = False
    Monitor = True
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 35
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object GLScene1: TGLScene
    Left = 64
    Top = 8
    object DummyCube1: TDummyCube
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {0000000000007041000000000000803F}
      Scale.Coordinates = {0000803F0000803F0000803F00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      CubeSize = 1
      EdgeColor.Color = {0000803F0000803F0000803F0000803F}
    end
    object GLLightSource1: TGLLightSource
      Ambient.Color = {0000000000000000000000000000803F}
      ConstAttenuation = 1
      Diffuse.Color = {0000803F0000803F0000803F0000803F}
      Position.Coordinates = {0000484200004842000048420000803F}
      Specular.Color = {0000000000000000000000000000803F}
      SpotCutOff = 180
      SpotDirection.Coordinates = {0000000000000000000080BF00000000}
    end
    object Sphere1: TSphere
      Direction.Coordinates = {00000000000000000000803F00000000}
      Position.Coordinates = {0000000000000000000000000000803F}
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
      Radius = 0.5
      Slices = 9
      Stacks = 6
      BehavioursData = {
        0201060B54474C42496E657274696102000200050000000000000080FF3F0200
        0900000000000000000000000000000000050000000000000000000005000000
        00000000000000050000000000000000000009020008020008}
      EffectsData = {
        0202060A54474C4246697265465802000606466972654658020002000607536D
        6F6B654658}
    end
    object GLCamera1: TGLCamera
      DepthOfView = 150
      FocalLength = 50
      TargetObject = DummyCube1
      Position.Coordinates = {000048420000A041000070410000803F}
      Direction.Coordinates = {0000000000000000000080BF00000000}
      Up.Coordinates = {000000000000803F0000000000000000}
      Left = 248
      Top = 144
    end
  end
  object FireFX: TGLFireFXManager
    FireDir.Coordinates = {00000000000000000000000000000000}
    InitialDir.Coordinates = {00000000000000000000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 512
    ParticleSize = 0.5
    InnerColor.Color = {0000803F0000803F000000000000803F}
    OuterColor.Color = {0000803F0000003F000000000000803F}
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    ParticleLife = 1
    FireRadius = 0.5
    Disabled = False
    Paused = False
    ParticleInterval = 0.00999999977648258
    UseInterval = True
    Reference = Sphere1
    Left = 64
    Top = 56
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 64
    Top = 136
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 64
    Top = 216
  end
  object SmokeFX: TGLFireFXManager
    FireDir.Coordinates = {000000000000803F0000000000000000}
    InitialDir.Coordinates = {000000000000003F0000000000000000}
    Cadencer = GLCadencer1
    MaxParticles = 64
    ParticleSize = 2
    InnerColor.Color = {0000803E0000803E0000803E0000803F}
    OuterColor.Color = {0000000000000000000000000000803F}
    FireDensity = 0.600000023841858
    FireEvaporation = 0.860000014305115
    FireRadius = 1
    Disabled = True
    Paused = False
    ParticleInterval = 0.0700000002980232
    UseInterval = False
    Reference = Sphere1
    Left = 64
    Top = 88
  end
end
