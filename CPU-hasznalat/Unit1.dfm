object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 15
  ClientWidth = 402
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object LEDMeter1: TLEDMeter
    Left = 0
    Top = 0
    Width = 425
    Height = 17
    BevelStyle = bvLowered
    ColorLED1 = clLime
    ColorLED2 = clYellow
    ColorLED3 = clRed
    ColorSeperator = clBlack
    Direction = mdRight
    LEDContrast = 6
    Max = 100
    Min = 0
    NumDigits = 100
    Position = 0
    SingleLED = False
    StartColor2 = 75
    StartColor3 = 90
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 112
    Top = 65528
  end
end
