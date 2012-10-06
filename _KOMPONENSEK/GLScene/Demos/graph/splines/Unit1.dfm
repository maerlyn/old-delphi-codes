object Form1: TForm1
  Left = 186
  Top = 146
  Width = 353
  Height = 372
  BorderWidth = 5
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
  object GLSceneViewer1: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 335
    Height = 335
    FogEnvironment.FogStart = 10
    FogEnvironment.FogEnd = 1000
    FogEnvironment.FogMode = fmLinear
    BackgroundColor = clBlack
    Camera = GLCamera1
    DepthTest = False
    Align = alClient
    OnMouseDown = GLSceneViewer1MouseDown
    OnMouseMove = GLSceneViewer1MouseMove
  end
  object GLScene1: TGLScene
    Left = 16
    Top = 16
    object Lines1: TLines
      AntiAliased = True
      LineWidth = 6
      Nodes = <
        item
          X = -1
          Y = -1
        end
        item
          Color.Color = {0000803F0000803F000000000000803F}
        end
        item
          X = 1
          Y = 1
          Color.Color = {0000803F00000000000000000000803F}
        end>
      NodesAspect = lnaCube
      NodeSize = 0.5
      SplineMode = lsmCubicSpline
      Options = [loUseNodeColorForLines]
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100
      FocalLength = 25
      TargetObject = Lines1
      CameraStyle = csOrthogonal
      Position.Coordinates = {00000000000000000000A0400000803F}
      Left = 248
      Top = 152
    end
  end
end
