unit SRGrad;

{ TSRGradient (C)opyright 2000 Version 1.21
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponente erzeugt einen Farbverlauf. Sie ist abgeleitet
  von TGraphicControl und ist Public Domain, das Urheberrecht liegt
  aber beim Autor. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} SysUtils, Messages,
  Classes, Graphics, Controls, Forms;

type
  TGradDirection = (gdDownRight, gdUpLeft);
  TGradStyle = (gsEllipse, gsHorizontal, gsPyramid, gsVertical);
  TEndColor = (ecBlack, ecWhite);
  TStartColor = (scAqua, scBlue, scFuchsia, scGray, scGreen, scLime, scMaroon,
                 scNavy, scOlive, scPurple, scRed, scTeal, scWhite, scYellow);
  TStepWidth = 1..10;

  TSRGradient = class(TGraphicControl)
  private
    FBC         : array[0..255] of Longint;
    FBitmap     : TBitmap;
    FBuffered   : boolean;
    FDirection  : TGradDirection;
    FEndColor   : TEndColor;
    FOldWidth,
    FOldHeight  : integer;
    FStartColor : TStartColor;
    FStepWidth  : TStepWidth;
    FStyle      : TGradStyle;

    procedure LoadColors;
    procedure DrawGradient(ACanvas: TCanvas);

    procedure SetBuffered(newValue: boolean);
    procedure SetDirection(newValue: TGradDirection);
    procedure SetEndColor(newValue: TEndColor);
    procedure SetStartColor(newValue: TStartColor);
    procedure SetStepWidth(newValue: TStepWidth);
    procedure SetStyle(newValue: TGradStyle);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_EraseBkgnd;

  protected
    procedure Paint; override;

  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property Buffered : boolean read FBuffered write SetBuffered;
    property Direction : TGradDirection read FDirection write SetDirection;
    property EndColor : TEndColor read FEndColor write SetEndColor;
    property StartColor : TStartColor read FStartColor write SetStartColor;
    property StepWidth : TStepWidth read FStepWidth write SetStepWidth;
    property Style : TGradStyle read FStyle write SetStyle;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
{$ELSE}
{$R *.D16}
{$ENDIF}

procedure TSRGradient.Loaded;
begin
  inherited Loaded;
end;

procedure TSRGradient.LoadColors;
var X, Y: Integer;
begin
  if ((FDirection = gdDownRight) and (FEndColor=ecBlack)) or
   ((FDirection = gdUpLeft) and (FEndColor=ecWhite)) then
    Y := $FF
  else
    Y := 0;
  if FEndColor=ecBlack then begin
    for X := 0 to 255 do begin
      case FStartColor of
        scAqua    : FBC[X] := RGB( 0, Y, Y);
        scBlue    : FBC[X] := RGB( 0, 0, Y);
        scPurple  : FBC[X] := RGB( Y div 2, 0, Y div 2);
        scGray    : FBC[X] := RGB( Y div 2, Y div 2, Y div 2);
        scGreen   : FBC[X] := RGB( 0, Y div 2, 0);
        scLime    : FBC[X] := RGB( 0, Y, 0);
        scMaroon  : FBC[X] := RGB( Y div 2, 0, 0);
        scNavy    : FBC[X] := RGB( 0, 0, Y div 2);
        scOlive   : FBC[X] := RGB( Y div 2, Y div 2, 0);
        scFuchsia : FBC[X] := RGB( Y, 0, Y);
        scRed     : FBC[X] := RGB( Y, 0, 0);
        scTeal    : FBC[X] := RGB( 0, Y div 2, Y div 2);
        scWhite   : FBC[X] := RGB( Y, Y, Y);
        scYellow  : FBC[X] := RGB( Y, Y, 0);
      end;
      if FDirection = gdDownRight then
        Dec(Y)
      else
        Inc(Y);
    end;
  end
  else begin
    for X := 0 to 255 do begin
      case FStartColor of
        scAqua    : FBC[X] := RGB( Y, $FF, $FF);
        scBlue    : FBC[X] := RGB( Y, Y, $FF);
        scFuchsia : FBC[X] := RGB( $FF, Y, $FF);
        scGray    : FBC[X] := RGB( $80+(Y div 2), $80+(Y div 2), $80+(Y div 2));
        scGreen   : FBC[X] := RGB( Y, $80+(Y div 2), Y);
        scLime    : FBC[X] := RGB( Y, $FF, Y);
        scMaroon  : FBC[X] := RGB( $80+(Y div 2), Y, Y);
        scNavy    : FBC[X] := RGB( Y, Y, $80+(Y div 2));
        scOlive   : FBC[X] := RGB( $80+(Y div 2), $80+(Y div 2), Y);
        scPurple  : FBC[X] := RGB( $80+(Y div 2), Y, $80+(Y div 2));
        scRed     : FBC[X] := RGB( $FF, Y, Y);
        scTeal    : FBC[X] := RGB( Y, $80+(Y div 2), $80+(Y div 2));
        scWhite   : FBC[X] := RGB( $FF, $FF, $FF);
        scYellow  : FBC[X] := RGB( $FF, $FF, Y);
      end;
      if FDirection = gdDownRight then
        Inc(Y)
      else
        Dec(Y);
    end;
  end;
end;

procedure TSRGradient.DrawGradient(ACanvas: TCanvas);
var
  TempRect   : TRect;
  TempStepV  : Single;
  TempStepH  : Single;
  ColorCode,
  TempLeft,
  TempTop,
  TempHeight,
  TempWidth,
  ECount,i   : integer;
begin
  if FBuffered and (FStyle=gsEllipse) then begin
    TempRect:=Rect(0,0,Width,Height);
    with ACanvas do begin
      Brush.Color:=clSilver;
      FillRect(TempRect);
    end;
  end;
  if (FStyle=gsHorizontal) or (FStyle=gsVertical) then begin
    if FStyle=gsVertical then begin
      TempStepH := 1;
      TempStepV := Height / 255;
      TempHeight := Trunc(TempStepV + 1);
      TempWidth := 1;
    end
    else begin
      TempStepH := Width / 255;
      TempStepV := 1;
      TempHeight := 1;
      TempWidth := Trunc(TempStepH + 1);
    end;
    with ACanvas do begin
      TempTop := 0;
      TempLeft := 0;
      TempRect.Top := 0;
      TempRect.Bottom:= Height;
      TempRect.Left := 0;
      TempRect.Right:= Width;
      for ColorCode := 0 to 255 do begin
        Brush.Color := FBC[ColorCode];
        if FStyle = gsVertical then begin
          TempRect.Top  := TempTop;
          TempRect.Bottom := TempTop + TempHeight;
        end
        else begin
          TempRect.Left  := TempLeft;
          TempRect.Right:= TempLeft + TempWidth;
        end;
        FillRect(TempRect);
        if FStyle = gsVertical then
          TempTop := Trunc(TempStepV * ColorCode)
        else
          TempLeft := Trunc(TempStepH * ColorCode)
      end;
    end;
  end;
  if FStyle=gsEllipse then begin
    with ACanvas do begin
      TempTop := 1;
      TempLeft := 1;
      Pen.Width:=1;
      ECount:=(Width div 2)-2;
      TempStepV:=Height/Width;
      TempStepH:=255/ECount;
      i:=2;
      while i<ECount do begin
        ColorCode:=trunc(TempStepH*i);
        Pen.Color := FBC[ColorCode];
        Brush.Color:=Pen.Color;
        Ellipse(TempLeft, TempTop, Width-TempLeft, Height-TempTop);
        TempTop := Trunc(TempStepV * i);
        TempLeft := i;
        i:=i+FStepWidth;
      end;
    end;
  end;
  if FStyle=gsPyramid then begin
    with ACanvas do begin
      TempLeft := Width div 2;
      TempTop := Height div 2;
      Pen.Width:=FStepWidth;
      ECount:=Width+Height;
      TempStepH:=255/ECount;
      i:=0;
      while i<=Width do begin
        ColorCode:=trunc(TempStepH*i);
        Pen.Color := FBC[ColorCode];
        MoveTo(i,0);
        LineTo(TempLeft,TempTop);
        ColorCode:=trunc(TempStepH*(i+Height));
        Pen.Color := FBC[ColorCode];
        LineTo(i,Height);
        i:=i+FStepWidth;
      end;
      i:=0;
      while i<=Height do begin
        ColorCode:=trunc(TempStepH*(i+Width));
        Pen.Color := FBC[ColorCode];
        MoveTo(Width,i);
        LineTo(TempLeft,TempTop);
        ColorCode:=trunc(TempStepH*i);
        Pen.Color := FBC[ColorCode];
        LineTo(0,i);
        i:=i+FStepWidth;
      end;
    end;
  end;
end;

procedure TSRGradient.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

constructor TSRGradient.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);

  FBuffered := true;
  FEndColor := ecBlack;
  FDirection := gdDownRight;
  FStartColor := scBlue;
  FStepWidth := 1;
  FStyle := gsVertical;
  Width:=100;
  Height:=80;
  FOldWidth := 0;
  FOldHeight := 0;

  FBitmap := TBitmap.Create;
  LoadColors;
end;

destructor TSRGradient.Destroy;
begin
  if FBuffered and assigned(FBitmap) then begin
    FBitmap.Free;
    FBitmap:=nil;
  end;
  inherited Destroy;
end;

procedure TSRGradient.SetBuffered(newValue: boolean);
begin
  if FBuffered<>newValue then begin
    FBuffered:=newValue;
    if FBuffered then
      FBitmap:=TBitmap.Create;
    if not FBuffered and assigned(FBitmap) then begin
      FBitmap.Free;
      FBitmap:=nil;
    end;
    FOldWidth:=0;
    Invalidate;
  end;
end;

procedure TSRGradient.SetDirection(newValue: TGradDirection);
begin
  if FDirection<>newValue then begin
    FDirection:=newValue;
    FOldWidth:=0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TSRGradient.SetEndColor(newValue: TEndColor);
begin
  if FEndColor<>newValue then begin
    FEndColor:=newValue;
    FOldWidth:=0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TSRGradient.SetStartColor(newValue: TStartColor);
begin
  if FStartColor<>newValue then begin
    FStartColor:=newValue;
    FOldWidth:=0;
    LoadColors;
    Invalidate;
  end;
end;

procedure TSRGradient.SetStepWidth(newValue: TStepWidth);
begin
  if (FStepWidth<>newValue) and (newValue>=1) and (newValue<=10) then begin
    FStepWidth:=newValue;
    FOldWidth:=0;
    Invalidate;
  end;
end;

procedure TSRGradient.SetStyle(newValue: TGradStyle);
begin
  if FStyle<>newValue then begin
    FStyle:=newValue;
    FOldWidth:=0;
    Invalidate;
  end;
end;

procedure TSRGradient.Paint;
var BmpRect : TRect;
begin
  if FBuffered and assigned(FBitmap) then begin
    if (FOldWidth<>Width) or (FOldHeight<>Height) then begin
      FOldWidth:=Width;
      FOldHeight:=Height;
      FBitmap.Width:=Width;
      FBitmap.Height:=Height;
      DrawGradient(FBitmap.Canvas);
    end;
    if FStyle=gsEllipse then begin
      BmpRect:=Rect(0,0,Self.Width-1,Self.Height-1);
      with Self.Canvas do begin
        Brush.Style:=bsClear;
        FillRect(BmpRect);
        BrushCopy(BmpRect,FBitmap,BmpRect,clSilver);
      end;
    end
    else
      BitBlT(Self.Canvas.Handle,
             0,0,Width,Height,
             FBitmap.Canvas.Handle,
             0,0,SrcCopy);
  end
  else
    DrawGradient(Self.Canvas);
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRGradient]);
end;

end.
