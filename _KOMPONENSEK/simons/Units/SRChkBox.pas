unit SRChkBox;

{ TSRCheckBox (C)opyright 2000 Version 1.02
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRCheckBox ist eine Checkbox-Komponente mit Autosize-,
  Transparent- und WordWrap-Eigenschaften. Außerdem wird kein OnClick-Ereignis
  abgefeuert, wenn die Checked-Eigenschaft per Programmcode geändert wird.

  Die Komponente ist abgeleteitet von TGraphicControl und ist Public Domain,
  das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

uses
  Windows, Classes, Graphics, Controls, SysUtils, Messages, StdCtrls;

type
  TCheckStyle = (csCheckBox, csDiamond, csPushButton, csRadioButton, csTrafficLight);

  TSRCheckBox = class(TGraphicControl)
  private
    FAlignment:        TLeftRight;
    FAllowGrayed,
    FAutoSize:         boolean;
    FColor:            TColor;
    FChecked:          boolean;
    FCheckSize:        integer;
    FMouseDown:        boolean;
    FSpacing:          integer;
    FState:            TCheckBoxState;
    FStateChanged:     boolean;
    FStyle:            TCheckStyle;
    FTransparent,
    FWordWrap:         boolean;

    FOnChange,
    FOnClick,
    FOnDblClick:       TNotifyEvent;
    FOnMouseDown:      TMouseEvent;
    FOnMouseMove:      TMouseMoveEvent;
    FOnMouseUp:        TMouseEvent;

    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;

  protected
    procedure AdjustBounds;
    procedure Change; dynamic;
    procedure DblClick; override;
    procedure DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
    function GetTextRect(ARect: TRect): TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure PaintButton;
    procedure PaintCaption;

    procedure SetAlignment(newValue: TLeftRight);
    procedure SetAutosize(newValue: boolean);
    procedure SetColor(newColor: TColor);
    procedure SetChecked(newValue: boolean);
    procedure SetCheckSize(newValue: integer);
    procedure SetSpacing(newValue: integer);
    procedure SetState(newValue: TCheckBoxState);
    procedure SetStyle(newValue: TCheckStyle);
    procedure SetTransparent(newValue: boolean);
    procedure SetWordWrap(newValue: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    {$ENDIF}
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property AllowGrayed: boolean read FAllowGrayed write FAllowGrayed;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: boolean read FAutoSize write SetAutoSize;
    {$IFDEF SR_Delphi5_Up}
    property BiDiMode;
    {$ENDIF}
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property CheckSize: integer read FCheckSize write SetCheckSize;
    property Color: TColor read FColor write SetColor;
    {$IFDEF SR_Delphi5_Up}
    property Constraints;
    {$ENDIF}
    {$IFDEF SR_Delphi4_Up}
    property DragKind;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF SR_Delphi5_Up}
    property ParentBiDiMode;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Spacing: integer read FSpacing write SetSpacing;
    property State: TCheckBoxState read FState write SetState;
    property Style: TCheckStyle read FStyle write SetStyle;
    property Transparent: boolean read FTransparent write SetTransparent;
    property Visible;
    property WordWrap: boolean read FWordWrap write SetWordWrap;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF SR_Delphi5_Up}
    property OnContextPopup;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF SR_Delphi4_Up}
    property OnEndDock;
    {$ENDIF}
    property OnEndDrag;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnStartDock;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

procedure Register;

implementation

function IsAccellerator(VK: Word; const Str: string): Boolean;
var
  P : Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (Upcase(Str[P + 1])=Upcase(Char(VK)));
end;

procedure DrawDiamond(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState);
var Offset        : integer;
    OldBrushStyle : TBrushStyle;
begin
  Offset:=(ARect.Right-ARect.Left) div 2;
  with ACanvas do begin
    Pen.Width:=1;
    if AState=cbUnChecked then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Left+Offset, ARect.Top);
    LineTo(ARect.Left, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Bottom-1);
    if AState=cbUnChecked then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    LineTo(ARect.Right-1, ARect.Top+Offset);
    LineTo(ARect.Left+Offset, ARect.Top);
    if AState<>cbUnchecked then begin
      OldBrushStyle:=Brush.Style;
      Pen.Color:=Brush.Color;
      Brush.Style:=bsSolid;
      if AState=cbChecked then
        Brush.Color:=clBlack
      else
        Brush.Color:=clGray;
      Polygon([Point(ARect.Left+Offset, ARect.Top+1),
              Point(ARect.Right-2, ARect.Top+Offset),
              Point(ARect.Left+Offset, ARect.Bottom-2),
              Point(ARect.Left+1, ARect.Top+Offset)]);
      Brush.Color:=Pen.Color;
      Brush.Style:=OldBrushStyle;
    end;
  end;
end;

procedure DrawTrafficLight(ACanvas:TCanvas; ARect:TRect; AState:TCheckBoxState);
const LightColors : array[TCheckBoxState] of TColor =
  (clLime, clRed, clYellow);
var OldColor      : TColor;
    OldBrushStyle : TBrushStyle;
begin
  with ACanvas do begin
    Pen.Color:=clBlack;
    Pen.Width:=1;
    OldColor:=Brush.Color;
    OldBrushStyle:=Brush.Style;
    Brush.Color:=LightColors[AState];
    Brush.Style:=bsSolid;
    InflateRect(ARect, -1, -1);
    Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    InflateRect(ARect, 1, 1);
    Pen.Color:=clBtnShadow;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Right, ARect.Top, ARect.Left, ARect.Bottom);
    Pen.Color:=clBtnHighlight;
    Arc(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom,
        ARect.Left, ARect.Bottom, ARect.Right, ARect.Top);
    Brush.Color:=OldColor;
    Brush.Style:=OldBrushStyle;
  end;
end;

{ Komponente TSRCheckBox }
constructor TSRCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {Vorgabewerte setzen}
  FAlignment:=taLeftJustify;
  FAllowGrayed:=false;
  FAutosize:=false;
  FColor:=clBtnFace;
  FChecked:=false;
  FCheckSize:=13;
  FSpacing:=4;
  FState:=cbUnchecked;
  FTransparent:=false;
  FWordWrap:=false;
  Width:=90;
  Height:=15;

  FMouseDown:=False;
  AdjustBounds;
end;

destructor  TSRCheckBox.Destroy;
begin
  inherited Destroy;
end;

procedure TSRCheckBox.AdjustBounds;
var ARect : TRect;
begin
  if FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    ARect.Right:=ARect.Right+FCheckSize+FSpacing;
    InflateRect(ARect, 1, 1);
    SetBounds(Left, Top, ARect.Right, ARect.Bottom);
  end;
//  if (csReading in ComponentState) and not FAutoSize then
//    SetBounds(Left, Top, Left+80, Top+15);
end;

procedure TSRCheckBox.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSRCheckBox.CMDialogChar(var Message: TCMDialogChar);
var AState : integer;
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      AState:=ord(FState);
      inc(AState);
      if (FAllowGrayed and (AState=3)) or (not FAllowGrayed and (AState=2)) then
        AState:=0;
      SetState(TCheckBoxState(AState));
      if Enabled and Assigned(FOnClick) then
        FOnClick(Self);
      Result:=1;
    end
    else
      inherited;
  end;
end;

procedure TSRCheckBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TSRCheckBox.CMTextChanged(var msg: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TSRCheckBox.DblClick;
begin
  if Enabled then
    if Assigned(FOnDblClick) then
      FOnDblClick(Self);
end;

procedure TSRCheckBox.DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
begin
  DrawText(Canvas.Handle, PChar(AText), Length(AText), ARect, AFormat);
end;

function TSRCheckBox.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result:=ARect;
  AText:=Self.Caption;
  if (AText='') or ((AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font:=Font;
  DrawTextComp(AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  Canvas.Handle:=OldHandle;
  ReleaseDC(0, DC);
end;

procedure TSRCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    if Button=mbLeft then begin
      FMouseDown:=true;
      if FState=cbUnChecked then begin
        SetState(cbChecked);
        FStateChanged:=true;
        if Assigned(FOnClick) then
          FOnClick(Self);
      end;
    end;
    if Assigned(FOnMouseDown) then
      FOnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TSRCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled and FMouseDown then begin
    { State-Eigenschaft berechnen }
    if not FStateChanged then begin
      if FState=cbChecked then begin
        if FAllowGrayed then
          SetState(cbGrayed)
        else
          SetState(cbUnChecked);
        if Assigned(FOnClick) then
          FOnClick(Self);
      end
      else begin
        if FState=cbGrayed then begin
          SetState(cbUnChecked);
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end;
    end;
    FStateChanged:=false;

    { OnClick-Ereignis abfeuern }
    if Assigned(FOnMouseUp) then
      FOnMouseUp(Self, Button, Shift, X, Y);
  end;
  FMouseDown:=false;
end;

procedure TSRCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TSRCheckBox.Paint;
var ARect : TRect;
begin
  { Hintergrund zeichnen }
  with Canvas do begin
    if FTransparent then
      Brush.Style:=bsClear
    else begin
      Brush.Style:=bsSolid;
      Brush.Color:=Color;
      ARect:=GetClientRect;
      FillRect(ARect);
    end;
  end;

  { Den Rest zeichnen }
  PaintButton;
  PaintCaption;
end;

procedure TSRCheckBox.PaintButton;
var ARect : TRect;
begin
  { Ausgaberechteck für Checkbox ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Right:=ARect.Left+FCheckSize
  else
    ARect.Left:=ARect.Right-FCheckSize;
  ARect.Top:=(ARect.Bottom-ARect.Top-FCheckSize) div 2;
  ARect.Bottom:=ARect.Top+FCheckSize;

  { Checkbox zeichnen }
  with Canvas do begin
    if FStyle=csCheckBox then begin
      if FState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck);
      if FState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck or DFCS_Checked);
      if FState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonCheck or DFCS_Checked or DFCS_Inactive);
    end;

    if FStyle=csRadioButton then begin
      if FState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio);
      if FState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked);
      if FState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonRadio or DFCS_Checked or DFCS_Inactive);
    end;

    if FStyle=csPushButton then begin
      if FState=cbUnchecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush);
      if FState=cbChecked then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Pushed);
      if FState=cbGrayed then
        DrawFrameControl(Handle, ARect, DFC_Button, DFCS_ButtonPush or DFCS_Flat);
    end;

    if FStyle=csDiamond then
      DrawDiamond(Canvas, ARect, FState);

    if FStyle=csTrafficLight then
      DrawTrafficLight(Canvas, ARect, FState);
  end;
end;

procedure TSRCheckBox.PaintCaption;
const
  Alignments: array[TAlignment] of Word =
   (DT_Left, DT_Right, DT_Center);
  WordWraps: array[Boolean] of Word =
   (0, DT_WordBreak);
  Lines: array[Boolean] of Word =
   (DT_SingleLine, 0);
var ARect     : TRect;
    DrawStyle : Integer;
begin
  { Ausgaberechteck für Caption ermitteln }
  ARect:=GetClientRect;
  if FAlignment=taLeftJustify then
    ARect.Left:=ARect.Left+FCheckSize+FSpacing
  else
    ARect.Right:=ARect.Right-FCheckSize-FSpacing;

  { Caption zeichnen }
  Canvas.Font.Assign(Font);
  if not Enabled then begin
    Canvas.Font.Color:=clBtnHighlight;
    OffsetRect(ARect, 1, 1);
  end;
  DrawStyle:=DT_ExpandTabs or DT_VCenter or Lines[FWordWrap] or WordWraps[FWordWrap] or Alignments[FAlignment];
  DrawTextComp(Caption, ARect, DrawStyle);
  if not Enabled then begin
    Canvas.Font.Color:=clInactiveCaption;
    OffsetRect(ARect, -1, -1);
    DrawTextComp(Caption, ARect, DrawStyle);
  end;
end;

procedure TSRCheckBox.SetAlignment(newValue: TLeftRight);
begin
  if FAlignment<>newValue then begin
    FAlignment:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetAutosize(newValue: boolean);
begin
  if FAutosize<>newValue then begin
    FAutosize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetChecked(newValue: boolean);
begin
  if FChecked<>newValue then begin
    FChecked:=newValue;
    if FChecked then
      SetState(cbChecked)
    else
      SetState(cbUnChecked);
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetCheckSize(newValue: integer);
begin
  if FCheckSize<>newValue then begin
    FCheckSize:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetSpacing(newValue: integer);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetState(newValue: TCheckBoxState);
begin
  if FState<>newValue then begin
    FState:=newValue;
    if FState=cbChecked then
      FChecked:=true;
    if FState=cbUnChecked then
      FChecked:=false;
    Change;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetStyle(newValue: TCheckStyle);
begin
  if FStyle<>newValue then begin
    FStyle:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetTransparent(newValue: boolean);
begin
  if FTransparent<>newValue then begin
    FTransparent:=newValue;
    Invalidate;
  end;
end;

procedure TSRCheckBox.SetWordWrap(newValue: boolean);
begin
  if FWordWrap<>newValue then begin
    FWordWrap:=newValue;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Simon',[TSRCheckBox]);
end;

end.
