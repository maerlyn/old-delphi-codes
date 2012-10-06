unit G32_RangeBars;

{*********************************************}
{  This unit is a part of Graphics32 library  }
{  Copyright © 2000-2001 Alex Denisov         }
{  See License.txt for licence information    }
{*********************************************}
// $Id: G32_RangeBars.pas,v 1.2 2001/08/26 07:56:45 alex Exp $

interface

{$I G32.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TRBDirection = (drLeft, drUp, drRight, drDown);
  TRBMouseInfo = (miNone, miBtnLeft, miBtnRight, miHandle, miLeftSp, miRightSp);
  TRBBackgnd = (bgPattern, bgSolid, bgMix);
  TRBGetSizeEvent = procedure (Sender: TObject; var Size: Integer) of object;

  TArrowBar = class(TCustomControl)
  private
    FBackgnd: TRBBackgnd;
    FBorderStyle: TBorderStyle;
    FButtonSize: Integer;
    FFramed: Boolean;
    FGlyphMax: TBitmap;
    FGlyphMin: TBitmap;
    FHandleColor: TColor;
    FKind: TScrollBarKind;
    FShowArrows: Boolean;
    FShowHandleGrip: Boolean;
    FOnChange: TNotifyEvent;
    FOnUserChange: TNotifyEvent;
    procedure CMColorChanged(var Msg: TMessage); message CM_COLORCHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure SetBackgnd(Value: TRBBackgnd);
    procedure SetButtonSize(Value: Integer);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetFramed(Value: Boolean);
    procedure SetGlyphMax(Value: TBitmap);
    procedure SetGlyphMin(Value: TBitmap);
    procedure SetHandleColor(Value: TColor);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetShowArrows(Value: Boolean);
    procedure SetShowHandleGrip(Value: Boolean);
  protected
    GenChange: Boolean;
    MouseInfo: TRBMouseInfo;
    Pattern: TBitmap;
    Timer: TTimer;
    TimerFirst: Boolean;
    StoredX, StoredY: Integer;
    PosBeforeDrag: Single;
    procedure DoChange; virtual;
    procedure DoDrawButton(R: TRect; Pushed: Boolean; Direction: TRBDirection; Enabled: Boolean); virtual;
    procedure DoRebuildPattern; virtual;
    function GetButtonSize: Integer;
    function GetClientBoundary: TRect;
    function GetEnabled: Boolean; reintroduce; virtual;
    function GetMouseInfo(X, Y: Integer): TRBMouseInfo; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure TimerHandler(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    property Color default clBtnShadow;
    property Backgnd: TRBBackgnd read FBackgnd write SetBackgnd default bgPattern;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property ButtonSize: Integer read FButtonSize write SetButtonSize default 0;
    property Framed: Boolean read FFramed write SetFramed default True;
    property GlyphMin: TBitmap read FGlyphMin write SetGlyphMin;
    property GlyphMax: TBitmap read FGlyphMax write SetGlyphMax;
    property HandleColor: TColor read FHandleColor write SetHandleColor default clBtnShadow;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property ShowArrows: Boolean read FShowArrows write SetShowArrows default True;
    property ShowHandleGrip: Boolean read FShowHandleGrip write SetShowHandleGrip;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnUserChange: TNotifyEvent read FOnUserChange write FOnUserChange;
  end;

  TCustomRangeBar = class(TArrowBar)
  private
    FCentered: Boolean;
    FIncrement: Single;
    FPosition: Single;
    FRange: Single;
    FWindow: Single;
    function IsIncrementStored: Boolean;
    function IsPositionStored: Boolean;
    function IsRangeStored: Boolean;
    function IsWindowStored: Boolean;
    procedure SetIncrement(Value: Single);
    procedure SetPosition(Value: Single);
    procedure SetRange(Value: Single);
    procedure SetWindow(Value: Single);
  protected
    OldSize: Single;
    procedure AdjustPosition;
    procedure DoDrawHandle(R: TRect; Pushed: Boolean; IsHorz: Boolean); virtual;
    function DoGetWindowSize: Single; virtual;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetEnabled: Boolean; override;
    function GetHandleRect: TRect;
    function GetMouseInfo(X, Y: Integer): TRBMouseInfo; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure TimerHandler(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetParams(NewRange, NewWindow: Single);

    property Centered: Boolean read FCentered write FCentered;
    property Increment: Single read FIncrement write SetIncrement stored IsIncrementStored;
    property Position: Single read FPosition write SetPosition stored IsPositionStored;
    property Range: Single read FRange write SetRange stored IsRangeStored;
    property Window: Single read FWindow write SetWindow stored IsWindowStored;
  end;

  TRangeBar = class(TCustomRangeBar)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Backgnd;
    property BorderStyle;
    property ButtonSize;
    property Enabled;
    property Framed;
    property GlyphMax;
    property GlyphMin;
    property HandleColor;
    property Increment;
    property Kind;
    property Range;
    property Visible;
    property Window;
    property ShowArrows;
    property ShowHandleGrip;

    property Position; // this should go after the Range property
    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property OnStartDrag;
    property OnUserChange;
  end;

  TCustomGaugeBar = class(TArrowBar)
  private
    FHandleSize: Integer;
    FLargeChange: Integer;
    FMax: Integer;
    FMin: Integer;
    FPosition: Integer;
    FSmallChange: Integer;
    procedure SetHandleSize(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetLargeChange(Value: Integer);
    procedure SetSmallChange(Value: Integer);
  protected
    procedure AdjustPosition;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function GetHandleRect: TRect;
    function GetHandleSize: Integer;
    function GetMouseInfo(X, Y: Integer): TRBMouseInfo; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure TimerHandler(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property HandleSize: Integer read FHandleSize write SetHandleSize default 0;
    property LargeChange: Integer read FLargeChange write SetLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property Position: Integer read FPosition write SetPosition;
    property SmallChange: Integer read FSmallChange write SetSmallChange default 1;
    property OnChange;
    property OnUserChange;
  end;

  TGaugeBar = class(TCustomGaugeBar)
  published
    property Align;
    property Anchors;
    property Constraints;
    property Color;
    property Backgnd;
    property BorderStyle;
    property ButtonSize;
    property Enabled;
    property Framed;
    property GlyphMax;
    property GlyphMin;
    property HandleColor;
    property HandleSize;
    property Kind;
    property LargeChange;
    property Max;
    property Min;
    property ShowArrows;
    property ShowHandleGrip;
    property SmallChange;
    property Visible;

    property Position;

    property OnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUserChange;
  end;

  { TArrowBarAccess }
  { This class is designed to facilitate access to facilitate access to
    properties of TArrowBar class when creating custom controls, which
    incorporate TArrowBar. It allows controlling up to two arrow bars.
    Master is used to read and write properties, slave - only to write.

    Well, maybe it is not so useful itself, but it is a common ancestor
    for TRangeBarAccess and TGaugeBarAccess classes, which work much the
    same way.

    When writing a new control, which uses TArrowBar, declare the bar as
    protected member, TArrowBarAccess as published property, and assign
    its Master to the arrow bar }
  TArrowBarAccess = class(TPersistent)
  private
    FMaster: TArrowBar;
    FSlave: TArrowBar;
    function GetBackgnd: TRBBackgnd;
    function GetBorderStyle: TBorderStyle;
    function GetButtonSize: Integer;
    function GetColor: TColor;
    function GetFramed: Boolean;
    function GetHandleColor: TColor;
    function GetShowArrows: Boolean;
    function GetShowHandleGrip: Boolean;
    procedure SetBackgnd(Value: TRBBackgnd);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetButtonSize(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetFramed(Value: Boolean);
    procedure SetHandleColor(Value: TColor);
    procedure SetShowArrows(Value: Boolean);
    procedure SetShowHandleGrip(Value: Boolean);
  public
    property Master: TArrowBar read FMaster write FMaster;
    property Slave: TArrowBar read FSlave write FSlave;
  published
    property Color: TColor read GetColor write SetColor;
    property Backgnd: TRBBackgnd read GetBackgnd write SetBackgnd default bgPattern;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsNone;
    property ButtonSize: Integer read GetButtonSize write SetButtonSize default 0;
    property Framed: Boolean read GetFramed write SetFramed default True;
    property HandleColor: TColor read GetHandleColor write SetHandleColor default clBtnShadow;
    property ShowArrows: Boolean read GetShowArrows write SetShowArrows default True;
    property ShowHandleGrip: Boolean read GetShowHandleGrip write SetShowHandleGrip;
  end;

implementation

uses Math;

function ClrLighten(C: TColor; Amount: Integer): TColor;
var
  r, g, b: Integer;
begin
  C := ColorToRGB(C);
  r := GetRValue(C) + Amount;
  g := GetGValue(C) + Amount;
  b := GetBValue(C) + Amount;
  if r < 0 then r := 0 else if r > 255 then r := 255;
  if g < 0 then g := 0 else if g > 255 then g := 255;
  if b < 0 then b := 0 else if b > 255 then b := 255;
  Result := RGB(r, g, b);
end;

function MixColors(C1, C2: TColor; W: Integer): TColor;
var
  R1, G1, B1: Integer;
  R2, G2, B2: Integer;
  W1: Integer;
begin
  if W > 255 then W := 255
  else if W < 0 then W := 0;
  W1 := W xor 255;
  C1 := ColorToRGB(C1);
  C2 := ColorToRGB(C2);
  R1 := GetRValue(C1);
  G1 := GetGValue(C1);
  B1 := GetBValue(C1);
  R2 := GetRValue(C2);
  G2 := GetGValue(C2);
  B2 := GetBValue(C2);
  R1 := (R1 * W + R2 * W1) div 255;
  G1 := (G1 * W + G2 * W1) div 255;
  B1 := (B1 * W + B2 * W1) div 255;
  Result := RGB(R1, G1, B1);
end;

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor; Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

procedure DrawHandle(Canvas: TCanvas; R: TRect; Color: TColor;
  Pushed, ShowGrip, IsHorz: Boolean);
var
  CHi, CLo: TColor;
  I, S: Integer;
begin
  CHi := ClrLighten(Color, $30);
  CLo := ClrLighten(Color, -$20);
  if Pushed then
  begin
    Canvas.Brush.Color := ClrLighten(CLo, -30);
    Canvas.FrameRect(R);
    InflateRect(R, -1, -1);
  end
  else
  begin
    Frame3D(Canvas, R, clWindowFrame, clWindowFrame, 1);
    Frame3D(Canvas, R, CHi, ClrLighten(CLo, -$20), 1);
  end;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(R);
  if Pushed then
  begin
    InflateRect(R, -1, -1);
    OffsetRect(R, 1, 1);
  end;

  if not ShowGrip then Exit;

  if IsHorz then
  begin
    I := (R.Left + R.Right) div 2;
    S := R.Right - R.Left;
    R.Left := I - 1;
    R.Right := I + 1;
    Dec(R.Bottom);
    Inc(R.Top);

    OffsetRect(R, -4, 0);
    if S > 10 then
    begin
      Frame3D(Canvas, R, CHi, CLo, 1);
      InflateRect(R, 1, 1);
    end;

    OffsetRect(R, 3, 0);
    Frame3D(Canvas, R, CHi, CLo, 1); InflateRect(R, 1, 1);

    OffsetRect(R, 3, 0);
    Frame3D(Canvas, R, CHi, CLo, 1); InflateRect(R, 1, 1);

    if S > 10 then
    begin
      OffsetRect(R, 3, 0);
      Frame3D(Canvas, R, CHi, CLo, 1);
    end;
  end
  else
  begin
    I := (R.Top + R.Bottom) div 2;
    S := R.Bottom - R.Top;
    R.Top := I - 1;
    R.Bottom := I + 1;
    Dec(R.Right);
    Inc(R.Left);

    OffsetRect(R, 0, -4);
    if S > 10 then
    begin
      Frame3D(Canvas, R, CHi, CLo, 1);
      InflateRect(R, 1, 1);
    end;

    OffsetRect(R, 0, 3);
    Frame3D(Canvas, R, CHi, CLo, 1); InflateRect(R, 1, 1);

    OffsetRect(R, 0, 3);
    Frame3D(Canvas, R, CHi, CLo, 1); InflateRect(R, 1, 1);

    if S > 10 then
    begin
      OffsetRect(R, 0, 3);
      Frame3D(Canvas, R, CHi, CLo, 1);
    end;
  end;
end;

const
  FIRST_DELAY = 600;
  TIMER_DELAY = 100;
  MIN_SIZE = 17;

{ TArrowBar }

procedure TArrowBar.CMColorChanged(var Msg: TMessage);
begin
  DoRebuildPattern;
  Invalidate;
end;

procedure TArrowBar.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

constructor TArrowBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls, csDoubleClicks];
  DoubleBuffered := True;
  Width := 100;
  Height := 16;
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  DoRebuildPattern;
  Color := clBtnShadow;
  Timer := TTimer.Create(Self);
  Timer.OnTimer := TimerHandler;
  FShowArrows := True;
  FFramed := True;
  FGlyphMin := TBitmap.Create;
  FGlyphMin.Transparent := True;
  FGlyphMax := TBitmap.Create;
  FGlyphMax.Transparent := True;
  FHandleColor := clBtnShadow;
  FShowHandleGrip := True;
end;

destructor TArrowBar.Destroy;
begin
  FGlyphMax.Free;
  FGlyphMin.Free;
  Pattern.Free;
  inherited;
end;

procedure TArrowBar.DoChange;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if GenChange and Assigned(FOnUserChange) then FOnUserChange(Self);
end;

procedure TArrowBar.DoDrawButton(R: TRect; Pushed: Boolean; Direction: TRBDirection; Enabled: Boolean);
const
  Flags: array [0..3] of Cardinal = (DFCS_SCROLLLEFT, DFCS_SCROLLUP,
    DFCS_SCROLLRIGHT, DFCS_SCROLLDOWN);
var
  F: Cardinal;
  B: TBitmap;
  Bias: Integer;
  Dx, Dy: Integer;
begin
  if Direction in [drLeft, drDown] then
  begin
    B := GlyphMin;
    Bias := 1;
  end
  else
  begin
    B := GlyphMax;
    Bias := 0;
  end;
  if B.Empty then
  begin
    { Use API call to draw button face}
    if not Enabled then F := DFCS_INACTIVE else F := 0;
    F := F or Flags[Ord(Direction)];
    if Pushed then F := F or DFCS_PUSHED or DFCS_FLAT;
    DrawFrameControl(Canvas.Handle, R, DFC_SCROLL, F);
  end
  else
  begin
    { Draw it explicitly }
    Dx := (R.Left + R.Right - B.Width + Bias) div 2;
    Dy := (R.Top + R.Bottom - B.Height) div 2;
    if Pushed then
    begin
      Canvas.Brush.Color := clBtnShadow;
      Canvas.FrameRect(R);
      InflateRect(R, -1, -1);
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
      Inc(Dx); Inc(Dy);
    end
    else
    begin
      Frame3D(Canvas, R, cl3DLight, cl3DDkShadow, 1);
      Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, 1);
      Canvas.Brush.Color := clBtnFace;
      Canvas.FillRect(R);
    end;
    if Enabled then Canvas.Draw(Dx, Dy, B);
  end;
end;

procedure TArrowBar.DoRebuildPattern;
var
  I, J: Integer;
begin
  Pattern.Width := 8;
  Pattern.Height := 8;
  if Backgnd = bgSolid then
  begin
    Pattern.Canvas.Brush.Color := Color;
    Pattern.Canvas.FillRect(Rect(0, 0, 8, 8));
  end
  else if Backgnd = bgMix then
  begin
    Pattern.Canvas.Brush.Color := MixColors(clBtnFace, Color, $7F);
    Pattern.Canvas.FillRect(Rect(0, 0, 8, 8));
  end
  else
    for J := 0 to 7 do
      for I := 0 to 7 do
        if Odd(I) = Odd(J) then Pattern.Canvas.Pixels[I, J] := Color
        else Pattern.Canvas.Pixels[I, J] := clBtnFace;
end;

function TArrowBar.GetButtonSize: Integer;
var
  W, H, T: Integer;
begin
  if not ShowArrows then
  begin
    Result := 0;
    Exit;
  end;

  Result := ButtonSize;
  W := Width;
  H := Height;
  if BorderStyle = bsSingle then
  begin
    Dec(W, 2);
    Dec(H, 2);
  end;
  if Kind = sbVertical then
  begin
    T := W; W := H; H := T;
  end;
  if Result = 0 then Result := Min(H, 32);
  if Result * 2 > W then Result := W div 2;
end;

function TArrowBar.GetClientBoundary: TRect;
begin
  Result := Rect(0, 0, Width, Height);
  if BorderStyle = bsSingle then InflateRect(Result, -1, -1);
  if Kind = sbHorizontal then InflateRect(Result, -GetButtonSize, 0)
  else InflateRect(Result, 0, -GetButtonSize);
end;

function TArrowBar.GetEnabled: Boolean;
begin
  Result := Enabled;
end;

function TArrowBar.GetMouseInfo(X, Y: Integer): TRBMouseInfo;
var
  P: TPoint;
  R, R1: TRect;
  Sz: Integer;
begin
  Result := miNone;

  P := Point(X, Y);
  R := Rect(0, 0, Width, Height);
  if BorderStyle = bsSingle then InflateRect(R, -1, -1);
  if not PtInrect(R, P) then Exit;

  Sz := GetButtonSize;
  R1 := R;
  if Kind = sbHorizontal then
  begin
    R1.Right := R1.Left + Sz;
    if PtInRect(R1, P) then Result := miBtnLeft
    else
    begin
      R1.Right := R.Right;
      R1.Left := R.Right - Sz;
      if PtInRect(R1, P) then Result := miBtnRight;
    end;
  end
  else
  begin
    R1.Bottom := R1.Top + Sz;
    if PtInRect(R1, P) then Result := miBtnLeft
    else
    begin
      R1.Bottom := R.Bottom;
      R1.Top := R.Bottom - Sz;
      if PtInRect(R1, P) then Result := miBtnRight;
    end;
  end;
end;

procedure TArrowBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if Button <> mbLeft then Exit;
  MouseInfo := GetMouseInfo(X, Y);
  Invalidate;
  StoredX := X;
  StoredY := Y;
  Timer.Interval := FIRST_DELAY;
  TimerFirst := False;
  TimerHandler(Self);
  TimerFirst := True;
  Timer.Enabled := True;
end;

procedure TArrowBar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  MouseInfo := miNone;
  Invalidate;
  Timer.Enabled := False;
end;

procedure TArrowBar.Paint;
var
  BSize: Integer;
  BLeftDown, BRightDown: Boolean;
  R, R2: TRect;
  Horz: Boolean;
  DrawButtons: Boolean;
  C, F: TColor;
begin
  R := Rect(0, 0, Width, Height);
  Horz := Kind = sbHorizontal;

  if BorderStyle = bsSingle then
  begin
    if CTL3D then Frame3D(Canvas, R, clBtnShadow, clBtnHighlight, 1)
    else
    begin
      Canvas.Brush.Color := clBlack;
      Canvas.FrameRect(R);
      InflateRect(R, -1, -1);
    end;
  end;
  BSize := GetButtonSize;

  BLeftDown := MouseInfo = miBtnLeft;
  BRightDown := MouseInfo = miBtnRight;
  DrawButtons := GetEnabled;
  if DrawButtons then F := clWindowFrame else F := clBtnShadow;

  if Framed then
  begin
    Canvas.Brush.Color := F;
    Canvas.FrameRect(R);
  end;

  // left / top button
  R2 := R;
  if Horz then R2.Right := R2.Left + BSize else R2.Bottom := R2.Top + BSize;

  if Horz then DoDrawButton(R2, BLeftDown, drLeft, DrawButtons)
  else DoDrawButton(R2, BLeftDown, drUp, DrawButtons);

  if Framed then
  begin
    Canvas.Brush.Color := F;
    Canvas.FrameRect(R2);
  end;

  // right / bottom button
  R2 := R;
  if Horz then R2.Left := R2.Right - BSize else R2.Top := R2.Bottom - BSize;

  if Horz then DoDrawButton(R2, BRightDown, drRight, DrawButtons)
  else DoDrawButton(R2, BRightDown, drDown, DrawButtons);

  if Framed then
  begin
    Canvas.Brush.Color := F;
    Canvas.FrameRect(R2);
  end;

  if Horz then InflateRect(R, -BSize, 0) else InflateRect(R, 0, -BSize);

  if Framed then
  begin
    if ShowArrows then
      if Horz then InflateRect(R, 1, 0) else InflateRect(R, 0, 1);
    Canvas.Brush.Color := F;
    Canvas.FrameRect(R);
    InflateRect(R, -1, -1);
    if DrawButtons then
    begin
      C := Color;
      if Backgnd <> bgSolid then C:= MixColors(C, clBtnFace, $7F);
      Frame3D(Canvas, R, ClrLighten(C, -$20), ClrLighten(C, $20), 1);
    end;
  end;

  if DrawButtons then
  begin
    Canvas.Brush.Bitmap := Pattern;
    Canvas.FillRect(R);
    Canvas.Brush.Color := Color;
  end
  else
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(R);
  end;
end;

procedure TArrowBar.SetBackgnd(Value: TRBBackgnd);
begin
  if Value <> FBackgnd then
  begin
    FBackgnd := Value;
    DoRebuildPattern;
    Invalidate;
  end;
end;

procedure TArrowBar.SetBorderStyle(Value: TBorderStyle);
begin
  if Value <> FBorderStyle then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetButtonSize(Value: Integer);
begin
  if Value <> FButtonSize then
  begin
    FButtonSize := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetFramed(Value: Boolean);
begin
  if Value <> FFramed then
  begin
    FFramed := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetGlyphMax(Value: TBitmap);
begin
  FGlyphMax.Assign(Value);
  FGlyphMax.Transparent := True;
  Invalidate;
end;

procedure TArrowBar.SetGlyphMin(Value: TBitmap);
begin
  FGlyphMin.Assign(Value);
  FGlyphMin.Transparent := True;
  Invalidate;
end;

procedure TArrowBar.SetHandleColor(Value: TColor);
begin
  if Value <> FHandleColor then
  begin
    FHandleColor := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetKind(Value: TScrollBarKind);
var
  Tmp: Integer;
begin
  if Value <> FKind then
  begin
    FKind := Value;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      Tmp := Width;
      Width := Height;
      Height := Tmp;
    end;
    Invalidate;
  end;
end;

procedure TArrowBar.SetShowArrows(Value: Boolean);
begin
  if Value <> FShowArrows then
  begin
    FShowArrows := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.SetShowHandleGrip(Value: Boolean);
begin
  if Value <> FShowHandleGrip then
  begin
    FShowHandleGrip := Value;
    Invalidate;
  end;
end;

procedure TArrowBar.TimerHandler(Sender: TObject);
begin
  if TimerFirst then
  begin
    Timer.Interval := TIMER_DELAY;
    TimerFirst := False;
  end;
end;


{ TCustomRangeBar }

procedure TCustomRangeBar.AdjustPosition;
var
  Sz: Single;
begin
  Sz := DoGetWindowSize;
  if FPosition + Sz > Range then FPosition := Range - Sz;
  if FPosition < 0 then FPosition := 0;
end;

constructor TCustomRangeBar.Create(AOwner: TComponent);
begin
  inherited;
  FIncrement := 8;
end;

procedure TCustomRangeBar.DoDrawHandle(R: TRect; Pushed, IsHorz: Boolean);
begin
  DrawHandle(Canvas, R, HandleColor, Pushed, ShowHandleGrip, IsHorz);
end;

function TCustomRangeBar.DoGetWindowSize: Single;
begin
  if FWindow > 0 then Result := FWindow
  else
  begin
    if Kind = sbHorizontal then Result := Width
    else Result := Height;
    OldSize := Result;
  end;
end;

function TCustomRangeBar.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then Position := Position + Increment * WheelDelta / 120;
  Result := True;
end;

function TCustomRangeBar.GetEnabled: Boolean;
begin
  Result := Enabled and (Range > DoGetWindowSize);
end;

function TCustomRangeBar.GetHandleRect: TRect;
var
  BtnSz, ClientSz: Integer;
  WinSz: Single;
  HandleSz, HandlePos: Integer;
  R: TRect;
  Horz: Boolean;
begin
  inherited;
  R := Rect(0, 0, Width, Height);
  Horz := Kind = sbHorizontal;
  if BorderStyle = bsSingle then InflateRect(R, -1, -1);
  BtnSz := GetButtonSize;
  if Horz then
  begin
    InflateRect(R, -BtnSz, 0);
    ClientSz := R.Right - R.Left;
  end
  else
  begin
    InflateRect(R, 0, -BtnSz);
    ClientSz := R.Bottom - R.Top;
  end;
  if ClientSz < 18 then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  WinSz := DoGetWindowSize;

  if Range > WinSz then
  begin
    HandleSz := Round(ClientSz * WinSz / Range);
    if HandleSz >= MIN_SIZE then HandlePos := Round(ClientSz * Position / Range)
    else
    begin
      HandleSz := MIN_SIZE;
      HandlePos := Round((ClientSz - MIN_SIZE) * Position / (Range - WinSz));
    end;
    Result := R;
    if Horz then
    begin
      Result.Left := R.Left + HandlePos;
      Result.Right := R.Left + HandlePos + HandleSz;
    end
    else
    begin
      Result.Top := R.Top + HandlePos;
      Result.Bottom := R.Top + HandlePos + HandleSz;
    end;
  end
  else Result := R;
end;

function TCustomRangeBar.GetMouseInfo(X, Y: Integer): TRBMouseInfo;
var
  P: TPoint;
  R: TRect;
begin
  Result := inherited GetMouseInfo(X, Y);
  if Result = miNone then
  begin
    R := GetHandleRect;
    P := Point(X, Y);
    if PtInRect(R, P) then Result := miHandle
    else
    begin
      if Kind = sbHorizontal then
      begin
        if (X > 0) and (X < R.Left) then Result := miLeftSp
        else if (X >= R.Right) and (X < Width - 1) then Result := miRightSp;
      end
      else
      begin
        if (Y > 0) and (Y < R.Top) then Result := miLeftSp
        else if (Y >= R.Bottom) and (Y < Height - 1) then Result := miRightSp;
      end;
    end;
  end;
end;

function TCustomRangeBar.IsIncrementStored: Boolean;
begin
  Result := FIncrement <> 8.0;
end;

function TCustomRangeBar.IsPositionStored: Boolean;
begin
  Result := FPosition > 0;
end;

function TCustomRangeBar.IsRangeStored: Boolean;
begin
  Result := FRange > 0;
end;

function TCustomRangeBar.IsWindowStored: Boolean;
begin
  Result := FWindow > 0;
end;

procedure TCustomRangeBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not GetEnabled then
  begin
    MouseInfo := miNone;
    Exit;
  end;

  inherited;

  if MouseInfo = miHandle then
  begin
    Timer.Enabled := False;
    PosBeforeDrag := Position;
  end
end;

procedure TCustomRangeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta: Single;
  WinSz: Single;
  ClientSz, HandleSz: Integer;
begin
  inherited;
  if MouseInfo = miHandle then
  begin
    WinSz := DoGetWindowSize;

    if Range <= WinSz then Exit;
    if Kind = sbHorizontal then Delta := X - StoredX else Delta := Y - StoredY;

    if Kind = sbHorizontal then ClientSz := Width  else ClientSz := Height;
    Dec(ClientSz, GetButtonSize * 2);
    if BorderStyle = bsSingle then Dec(ClientSz, 2);
    HandleSz := Round(ClientSz * WinSz / Range);

    if HandleSz < MIN_SIZE then Delta := Round(Delta * (Range - WinSz) / (ClientSz - MIN_SIZE))
    else Delta := Delta * Range / ClientSz;

    GenChange := True;
    Position := PosBeforeDrag + Delta;
    GenChange := False;
  end;
end;

procedure TCustomRangeBar.Paint;
var
  R, ClientR: TRect;
begin
  inherited;
  if not GetEnabled then Exit;
  R := GetHandleRect;
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then Exit;

  DoDrawHandle(R, MouseInfo = miHandle, Kind = sbHorizontal);
  if MouseInfo in [miRightSp, miLeftSp] then
  begin
    ClientR := GetClientBoundary;
    if MouseInfo = miRightSp then
      if Kind = sbHorizontal then ClientR.Left := R.Right
      else ClientR.Top := R.Bottom
    else
      if Kind = sbHorizontal then ClientR.Right := R.Left
      else ClientR.Bottom := R.Top;
    Canvas.Brush.Color := clHighLight;
    Canvas.FillRect(ClientR);
  end;
end;

procedure TCustomRangeBar.Resize;
var
  OldSz, WinSz: Single;
  Center: Single;
begin
  if Centered then
  begin
    OldSz := OldSize;
    WinSz := DoGetWindowSize;
    if Range > WinSz then
    begin
      if Range > OldSz then Center := (FPosition + OldSz / 2) / Range
      else Center := 0.5;
      FPosition := Center * Range - WinSz / 2;
    end;
  end;
  AdjustPosition;
  inherited;
end;

procedure TCustomRangeBar.SetIncrement(Value: Single);
begin
  if Value < 0.001 then Value := 0.001;
  FIncrement := Value;
end;

procedure TCustomRangeBar.SetParams(NewRange, NewWindow: Single);
var
  WinSz: Single;
  OldRange: Single;
  Center: Single;
begin
  if NewRange < 0 then NewRange := 0;
  if NewWindow < 0 then NewWindow := 0;
  WinSz := DoGetWindowSize;
  if (NewRange <> FRange) or (NewWindow <> WinSz) then
  begin
    OldRange := FRange;
    FRange := NewRange;
    FWindow := NewWindow;
    WinSz := DoGetWindowSize;
    if Centered and (Range > WinSz) then
    begin
      if OldRange > WinSz then Center := (FPosition + WinSz / 2) / OldRange
      else Center := 0.5;
      FPosition := Center * Range - WinSz / 2;
    end;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomRangeBar.SetPosition(Value: Single);
begin
  if Value <> FPosition then
  begin
    FPosition := Value;
    AdjustPosition;
    Invalidate;
    DoChange;
  end;
end;

procedure TCustomRangeBar.SetRange(Value: Single);
begin
  SetParams(Value, Window);
end;

procedure TCustomRangeBar.SetWindow(Value: Single);
begin
  SetParams(Range, Value);
end;

procedure TCustomRangeBar.TimerHandler(Sender: TObject);
begin
  inherited;
  GenChange := True;

  case MouseInfo of
    miBtnLeft:
      begin
        Position := Position - Increment;
      end;

    miBtnRight:
      begin
        Position := Position + Increment;
      end;

    miRightSp:
      begin
        if GetMouseInfo(StoredX, StoredY) <> miRightSp then
        begin
          Timer.Enabled := False;
          MouseInfo := miNone;
        end
        else Position := Position + DoGetWindowSize;
      end;

    miLeftSp:
      begin
        if GetMouseInfo(StoredX, StoredY) <> miLeftSp then
        begin
          Timer.Enabled := False;
          MouseInfo := miNone;
        end
        else Position := Position - DoGetWindowSize;
      end;
  end;
  GenChange := False;
end;

{ TCustomGaugeBar }

procedure TCustomGaugeBar.AdjustPosition;
begin
  if Position < Min then Position := Min
  else if Position > Max then Position := Max;
end;

constructor TCustomGaugeBar.Create(AOwner: TComponent);
begin
  inherited;
  FLargeChange := 1;
  FMax := 100;
  FSmallChange := 1;
end;

function TCustomGaugeBar.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if not Result then Position := Position + FSmallChange * WheelDelta div 120;
  Result := True;
end;

function TCustomGaugeBar.GetHandleRect: TRect;
var
  Sz, HandleSz: Integer;
  Horz: Boolean;
  Pos: Integer;
begin
  inherited;
  Result := GetClientBoundary;
  Horz := Kind = sbHorizontal;
  HandleSz := GetHandleSize;

  if Horz then Sz := Result.Right - Result.Left
  else Sz := Result.Bottom - Result.Top;


  Pos := Round((Position - Min) * (Sz - GetHandleSize) / (Max - Min));

  if Horz then
  begin
    Inc(Result.Left, Pos);
    Result.Right := Result.Left + HandleSz;
  end
  else
  begin
    Inc(Result.Top, Pos);
    Result.Bottom := Result.Top + HandleSz;
  end;
end;

function TCustomGaugeBar.GetHandleSize: Integer;
var
  R: TRect;
  Sz: Integer;
begin
  Result := HandleSize;
  if Result = 0 then
  begin
    if Kind = sbHorizontal then Result := Height else Result := Width;
    if BorderStyle = bsSingle then Dec(Result, 2);
  end;
  R := GetClientBoundary;
  if Kind = sbHorizontal then Sz := R.Right - R.Left
  else Sz := R.Bottom - R.Top;
  if Sz - Result < 1 then Result := Sz - 1;
  if Result < 0 then Result := 0;
end;

function TCustomGaugeBar.GetMouseInfo(X, Y: Integer): TRBMouseInfo;
var
  P: TPoint;
  R: TRect;
begin
  Result := inherited GetMouseInfo(X, Y);
  if Result = miNone then
  begin
    R := GetHandleRect;
    P := Point(X, Y);
    if PtInRect(R, P) then Result := miHandle
    else
    begin
      if Kind = sbHorizontal then
      begin
        if (X > 0) and (X < R.Left) then Result := miLeftSp
        else if (X >= R.Right) and (X < Width - 1) then Result := miRightSp;
      end
      else
      begin
        if (Y > 0) and (Y < R.Top) then Result := miLeftSp
        else if (Y >= R.Bottom) and (Y < Height - 1) then Result := miRightSp;
      end;
    end;
  end;
end;

procedure TCustomGaugeBar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if MouseInfo = miHandle then
  begin
    Timer.Enabled := False;
    PosBeforeDrag := Position;
  end;
end;

procedure TCustomGaugeBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Delta: Single;
  R: TRect;
  ClientSz: Integer;
begin
  inherited;
  if MouseInfo = miHandle then
  begin
    if Kind = sbHorizontal then Delta := X - StoredX else Delta := Y - StoredY;
    R := GetClientBoundary;

    if Kind = sbHorizontal then ClientSz := R.Right - R.Left
    else ClientSz := R.Bottom - R.Top;

    Delta := Delta * (Max - Min) / (ClientSz - GetHandleSize);

    GenChange := True;
    Position := Round(PosBeforeDrag + Delta);
    GenChange := False;
  end;
end;

procedure TCustomGaugeBar.Paint;
var
  R: TRect;
begin
  inherited;
  if Enabled then
  begin
    R := GetHandleRect;
    if (R.Right > R.Left) and (R.Bottom > R.Top) then
      DrawHandle(Canvas, R, HandleColor, MouseInfo = miHandle,
        ShowHandleGrip, Kind = sbHorizontal);
  end;
end;

procedure TCustomGaugeBar.SetHandleSize(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FHandleSize then
  begin
    FHandleSize := Value;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetLargeChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FLargeChange := Value;
end;

procedure TCustomGaugeBar.SetMax(Value: Integer);
begin
  if (Value <= FMin) and not (csLoading in ComponentState) then Value := FMin + 1;
  if Value <> FMax then
  begin
    FMax := Value;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetMin(Value: Integer);
begin
  if (Value >= FMax) and not (csLoading in ComponentState) then Value := FMax - 1;
  if Value <> FMin then
  begin
    FMin := Value;
    AdjustPosition;
    Invalidate;
  end;
end;

procedure TCustomGaugeBar.SetPosition(Value: Integer);
begin
  if Value < Min then Value := Min
  else if Value > Max then Value := Max;
  if FPosition <> Value then
  begin
    FPosition := Value;
    Invalidate;
    DoChange;
  end;
end;

procedure TCustomGaugeBar.SetSmallChange(Value: Integer);
begin
  if Value < 1 then Value := 1;
  FSmallChange := Value;
end;

procedure TCustomGaugeBar.TimerHandler(Sender: TObject);
begin
  inherited;

  GenChange := True;

  case MouseInfo of
    miBtnLeft:
      begin
        Position := Position - SmallChange;
      end;

    miBtnRight:
      begin
        Position := Position + SmallChange;
      end;

    miRightSp:
      begin
        if GetMouseInfo(StoredX, StoredY) <> miRightSp then
        begin
          Timer.Enabled := False;
          MouseInfo := miNone;
        end
        else Position := Position + LargeChange;
      end;

    miLeftSp:
      begin
        if GetMouseInfo(StoredX, StoredY) <> miLeftSp then
        begin
          Timer.Enabled := False;
          MouseInfo := miNone;
        end
        else Position := Position - LargeChange;
      end;
  end;

  GenChange := False;
end;

{ TArrowBarAccess }

function TArrowBarAccess.GetBackgnd: TRBBackgnd;
begin
  Result := FMaster.Backgnd;
end;

function TArrowBarAccess.GetBorderStyle: TBorderStyle;
begin
  Result := FMaster.BorderStyle;
end;

function TArrowBarAccess.GetButtonSize: Integer;
begin
  Result := FMaster.ButtonSize;
end;

function TArrowBarAccess.GetColor: TColor;
begin
  Result := FMaster.Color;
end;

function TArrowBarAccess.GetFramed: Boolean;
begin
  Result := FMaster.Framed;
end;

function TArrowBarAccess.GetHandleColor: TColor;
begin
  Result := FMaster.HandleColor;
end;

function TArrowBarAccess.GetShowArrows: Boolean;
begin
  Result := FMaster.ShowArrows;
end;

function TArrowBarAccess.GetShowHandleGrip: Boolean;
begin
  Result := FMaster.ShowHandleGrip;
end;

procedure TArrowBarAccess.SetBackgnd(Value: TRBBackgnd);
begin
  FMaster.Backgnd := Value;
  if FSlave <> nil then FSlave.Backgnd := Value;
end;

procedure TArrowBarAccess.SetBorderStyle(Value: TBorderStyle);
begin
  FMaster.BorderStyle := Value;
  if FSlave <> nil then FSlave.BorderStyle := Value;
end;

procedure TArrowBarAccess.SetButtonSize(Value: Integer);
begin
  FMaster.ButtonSize := Value;
  if FSlave <> nil then FSlave.ButtonSize := Value;
end;

procedure TArrowBarAccess.SetColor(Value: TColor);
begin
  FMaster.Color := Value;
  if FSlave <> nil then FSlave.Color := Value;
end;

procedure TArrowBarAccess.SetFramed(Value: Boolean);
begin
  FMaster.Framed := Value;
  if FSlave <> nil then FSlave.Framed := Value;
end;

procedure TArrowBarAccess.SetHandleColor(Value: TColor);
begin
  FMaster.HandleColor := Value;
  if FSlave <> nil then FSlave.HandleColor := Value;
end;

procedure TArrowBarAccess.SetShowArrows(Value: Boolean);
begin
  FMaster.ShowArrows := Value;
  if FSlave <> nil then FSlave.ShowArrows := Value;
end;

procedure TArrowBarAccess.SetShowHandleGrip(Value: Boolean);
begin
  FMaster.ShowHandleGrip := Value;
  if FSlave <> nil then FSlave.ShowHandleGrip := Value;
end;

end.
