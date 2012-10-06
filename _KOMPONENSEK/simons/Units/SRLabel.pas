unit SRLabel;

{ TSRLabel (C)opyright 2000 Version 1.13
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Diese Komponente ist eine Label-Komponente mit Schatteneffekt und
  Internet-Link-Funktion. Sie ist abgeleitet von TGraphicControl und
  sie ist Public Domain, das Urheberrecht liegt aber beim Autor.

  Vielen Dank an Robert Rossmair für die rrColors-Unit! }

interface

{$I SRDefine.inc}

uses {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, SysUtils, {$ENDIF}
  Messages, Classes, Controls, Graphics, ExtCtrls, StdCtrls, Forms;

type
  THighLightPos = (hpTopLeft, hpTopRight);
  TLinkType = (ltEMail, ltNews, ltNone, ltWWW);
  {$IFNDEF SR_Delphi3_Up}
  TTextLayout = (tlBottom, tlCenter, tlTop);
  {$ENDIF}

  TSRLabel = class(TGraphicControl)
  private
    FAlignment         : TAlignment;
    FAutoSize          : boolean;
    FBevelStyle        : TPanelBevel;
    FBorderWidth       : word;
    FFocusControl      : TWinControl;
    FHighlightColor    : TColor;
    FHighlightOffset   : word;
    FHighlightOnEnter  : boolean;
    FHighlightPos      : THighlightPos;
    FLayout            : TTextLayout;
    FLinkActive        : boolean;
    FLinkedAdress      : string;
    FLinkType          : TLinkType;
    FMouseOnControl    : boolean;
    FOldCursor         : TCursor;
    FShadowColor       : TColor;
    FShadowOffset      : word;
    FShortenFilenames  : boolean;
    FShowAccelChar,
    FShowHighlight,
    FShowShadow,
    FUnderlineOnEnter : boolean;
    FWordWrap         : boolean;

    FOnClick,
    FOnExecuteLink,
    FOnMouseEnter,
    FOnMouseExit     : TNotifyEvent;

    procedure AdjustBounds;
    procedure DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
    procedure GetTextAndTextRect(var AText:string;var ARect:TRect);
    function GetTextRect(ARect: TRect): TRect;
    function GetTransparent: Boolean;
    procedure PaintBevel(ARect: TRect);
    procedure PaintText(ARect: TRect; AFlags: Word);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBevelStyle(Value: TPanelBevel);
    procedure SetBorderWidth(Value: word);
    {$IFDEF SR_Delphi2_Up}
    procedure SetFocusControl(Value: TWinControl);
    {$ENDIF}
    procedure SetHighlightColor(Value: TColor);
    procedure SetHighlightOffset(Value: word);
    procedure SetHighlightOnEnter(Value: boolean);
    procedure SetHighlightPos(Value: THighlightPos);
    procedure SetLayout(Value: TTextLayout);
    procedure SetShadowColor(Value: TColor);
    procedure SetShadowOffset(Value: word);
    procedure SetShortenFilenames(Value: Boolean);
    procedure SetShowAccelChar(Value: Boolean);
    procedure SetShowHighlight(Value: Boolean);
    procedure SetShowShadow(Value: Boolean);
    procedure SetTransparent(Value: Boolean);
    procedure SetUnderlineOnEnter(Value: boolean);
    procedure SetWordWrap(Value: Boolean);

    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;

  protected
    procedure Click; override;
    procedure ExecuteLink; dynamic;
    function GetLabelText: string; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;

  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment
      default taLeftJustify;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoSize: Boolean read FAutoSize write SetAutoSize default True;
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BorderWidth: word read FBorderWidth write SetBorderWidth;
    property Caption;
    property Color;
    {$IFDEF SR_Delphi4_Up}
    property Constraints;
    {$ENDIF}
    property Cursor;
    {$IFDEF SR_Delphi4_Up}
    property DragKind;
    {$ENDIF}
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi2_Up}
    property FocusControl: TWinControl read FFocusControl write SetFocusControl;
    {$ELSE}
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    {$ENDIF}
    property Font;
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default clBtnHighlight;
    property HighlightOffset: word read FHighlightOffset write SetHighlightOffset default 1;
    property HighlightOnEnter: Boolean read FHighlightOnEnter write SetHighlightOnEnter default True;
    property HighlightPos: THighlightPos read FHighlightPos write SetHighlightPos default hpTopLeft;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LinkActive: boolean read FLinkActive write FLinkActive;
    property LinkedAdress: string read FLinkedAdress write FLinkedAdress;
    property LinkType: TLinkType read FLinkType write FLinkType;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBtnShadow;
    property ShadowOffset: word read FShadowOffset write SetShadowOffset default 1;
    property ShortenFilenames: Boolean read FShortenFilenames write SetShortenFilenames;
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default True;
    property ShowHighlight: Boolean read FShowHighlight write SetShowHighlight;
    property ShowHint;
    property ShowShadow: Boolean read FShowShadow write SetShowShadow;
    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property UnderlineOnEnter: Boolean read FUnderlineOnEnter write SetUnderlineOnEnter default True;
    property Visible;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {$IFDEF SR_Delphi5_Up}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    {$IFDEF SR_Delphi4_Up}
    property OnEndDock;
    {$ENDIF}
    property OnExecuteLink: TNotifyEvent read FOnExecuteLink write FOnExecuteLink;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi4_Up}
    property OnStartDock;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
{$R *.R32}
uses ShellAPI, FileCtrl, rrColors;
{$ELSE}
{$R *.D16}
{$R *.R16}
uses ShellAPI, FileCtrl;
{$ENDIF}

const
  HLFactor    = 0.65;
  HLContrast  = 5;
  ShContrast  = 4;
  crLinkPoint = TCursor(-30);

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  {Rot}
  Red:=GetRValue(RGBColor);
  NewR:=Red+(Percentage*Red div 100);
  if NewR>255 then begin
    Overflow:=NewR-255;
    NewG:=Overflow;
    NewB:=Overflow;
  end
  else begin
    NewG:=0;
    NewB:=0;
  end;
  {Grün}
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  {Blau}
  Blue:=GetBValue(RGBColor);
  NewB:=NewB+Blue+(Percentage*Blue div 100);
  if NewB>255 then begin
    Overflow:=NewB-255;
    if NewG<=255 then
      NewR:=NewR+Overflow;
  end;
  if NewR>255 then
    NewR:=255;
  if NewG>255 then
    NewG:=255;
  if NewB>255 then
    NewB:=255;
  if NewR<0 then
    NewR:=0;
  if NewG<0 then
    NewG:=0;
  if NewB<0 then
    NewB:=0;
  Result:=NewR+(NewG shl 8)+(NewB shl 16);
end;
{$ENDIF}

function CalcHighlightColor(AColor:TColor):TColor;
var ShColor : TColor;
begin
  {$IFDEF SR_Delphi1}
  Result:=ChangeBrightness(AColor,round(HLFactor*100));
  {$ELSE}
  Get3DColors(AColor,Result,ShColor,HLFactor,1);
  {$ENDIF}
end;

procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  {$IFDEF SR_Delphi1}
  HighlightColor:=ChangeBrightness(FaceColor,100 div 10*HLContrast);
  ShadowColor:=ChangeBrightness(FaceColor,-100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FaceColor,HighlightColor,ShadowColor,(10-HLContrast)/10,(10-ShContrast)/10);
  {$ENDIF}
end;


constructor TSRLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Screen.Cursors[crLinkPoint]:=LoadCursor(HInstance, 'CRSRLINKPOINT');
  {$IFDEF SR_Delphi2_Up}
  ControlStyle:=ControlStyle+[csOpaque, csReplicatable];
  FAutoSize:=True;
  {$ELSE}
  ControlStyle:=ControlStyle+[csOpaque];
  FAutoSize:=False;
  {$ENDIF}
  Width:=65;
  Height:=17;

  FBevelStyle:=bvNone;
  FBorderWidth:=0;
  FHighlightColor:=clBtnHighlight;
  FHighlightOffset:=1;
  FHighlightOnEnter:=true;
  FHighlightPos:=hpTopLeft;
  FLayout:=tlTop;
  FLinkType:=ltWWW;
  FLinkedAdress:='http://www.picsoft.de';
  FLinkActive:=false;
  FOldCursor:=Cursor;
  FShadowColor:=clBtnShadow;
  FShadowOffset:=1;
  FShortenFilenames:=false;
  FShowAccelChar:=True;
  FShowHighlight:=False;
  FShowShadow:=False;
  FUnderlineOnEnter:=true;
end;

procedure TSRLabel.AdjustBounds;
var X     : Integer;
    ARect : TRect;
begin
  if not (csReading in ComponentState) and FAutoSize then begin
    ARect:=GetTextRect(ClientRect);
    if FBorderWidth>0 then
      InflateRect(ARect, FBorderWidth*2, FBorderWidth*2);
    if FShowHighlight then begin
      ARect.Right:=ARect.Right+FHighlightOffset;
      ARect.Bottom:=ARect.Bottom+FHighlightOffset;
    end;
    if FShowShadow then begin
      ARect.Right:=ARect.Right+FShadowOffset;
      ARect.Bottom:=ARect.Bottom+FShadowOffset;
    end;
    if FAlignment=taRightJustify then
      X:=Left+Width-(ARect.Right-ARect.Left)
    else
      X:=Left;
    SetBounds(X, Top, ARect.Right, ARect.Bottom);
  end;
end;

procedure TSRLabel.Click;
var URL   : string;
{$IFDEF SR_Delphi1}
    CText : PChar;
{$ENDIF}
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
  if Enabled and FLinkActive and (FLinkType<>ltNone) and (FLinkedAdress<>'') then begin
    case FLinkType of
      ltEMail : URL:='mailto:'+FLinkedAdress;
      ltNews  : URL:='news:'+FLinkedAdress;
      else URL:=FLinkedAdress;
    end;
    ExecuteLink;
    {$IFDEF SR_Delphi1}
    CText:=StrAlloc(255);
    StrPCopy(CText, URL);
    ShellExecute(0, 'open', CText, nil, nil, SW_ShowNormal);
    StrDispose(CText);
    {$ELSE}
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_ShowNormal);
    {$ENDIF}
  end;
end;

procedure TSRLabel.CMDialogChar(var Message: TCMDialogChar);
begin
  if (FFocusControl <> nil) and Enabled and ShowAccelChar and
   IsAccel(Message.CharCode, Caption) then
    with FFocusControl do
      if CanFocus then begin
        SetFocus;
        Message.Result:=1;
      end;
end;

procedure TSRLabel.CMFontChanged(var Message: TMessage);
begin
  inherited;
  AdjustBounds;
end;

procedure TSRLabel.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseOnControl and FLinkActive and Enabled then begin
    FMouseOnControl:=true;
    FOldCursor:=Cursor;
    Cursor:=crLinkPoint;
    Invalidate;
  end;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TSRLabel.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseOnControl and FLinkActive and Enabled then begin
    FMouseOnControl:=false;
    Cursor:=FOldCursor;
    Invalidate;
  end;
  if assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TSRLabel.CMTextChanged(var Message: TMessage);
begin
  AdjustBounds;
  Invalidate;
end;

procedure TSRLabel.DrawTextComp(AText:string; var ARect:TRect; AFormat:Word);
{$IFDEF SR_Delphi1}
var CText : PChar;
{$ENDIF}
begin
  {$IFDEF SR_Delphi1}
  CText:=StrAlloc(255);
  StrPCopy(CText, AText);
  DrawText(Canvas.Handle, CText, StrLen(CText), ARect, AFormat);
  StrDispose(CText);
  {$ELSE}
  DrawText(Canvas.Handle, PChar(AText), Length(AText), ARect, AFormat);
  {$ENDIF}
end;

procedure TSRLabel.ExecuteLink;
begin
  if Assigned(FOnExecuteLink) then
    FOnExecuteLink(Self);
end;

function TSRLabel.GetLabelText: string;
begin
  if FShortenFilenames then begin
    Result:=MinimizeName(Caption, Canvas, ClientWidth-10);
  end
  else
    Result:=Caption;
end;

procedure TSRLabel.GetTextAndTextRect(var AText:string;var ARect:TRect);
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var DC        : HDC;
    OldHandle : THandle;
    OutText   : string;
begin
  AText:=GetLabelText;
  OutText:=AText;
  if (OutText='') or (FShowAccelChar and (OutText[1]='&') and (OutText[2]=#0)) then
    OutText:=OutText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font:=Font;
  DrawTextComp(OutText, ARect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  Canvas.Handle:=OldHandle;
  ReleaseDC(0, DC);
end;

function TSRLabel.GetTextRect(ARect: TRect): TRect;
const WordWraps : array[Boolean] of Word = (0, DT_WORDBREAK);
var AText     : string;
    DC        : HDC;
    OldHandle : THandle;
begin
  Result:=ARect;
  AText:=GetLabelText;
  if (AText='') or (FShowAccelChar and (AText[1]='&') and (AText[2]=#0)) then
    AText:=AText+' ';
  OldHandle:=Canvas.Handle;
  DC:=GetDC(0);
  Canvas.Handle:=DC;
  Canvas.Font:=Font;
  DrawTextComp(AText, Result, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[FWordWrap]);
  Canvas.Handle:=OldHandle;
  ReleaseDC(0, DC);
end;

function TSRLabel.GetTransparent: Boolean;
begin
  Result:=not (csOpaque in ControlStyle);
end;

procedure TSRLabel.Loaded;
begin
  inherited Loaded;
  AdjustBounds;
end;

procedure TSRLabel.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FFocusControl) then
    FFocusControl:=nil;
end;

procedure TSRLabel.PaintBevel(ARect: TRect);
var ShColor,HLColor : TColor;
begin
  AssignBevelColors(Color, HLColor, ShColor, HLContrast, ShContrast);
  with Canvas do begin
    if FBevelStyle=bvRaised then
      Pen.Color:=HLColor
    else
      Pen.Color:=ShColor;
    MoveTo(ARect.Right-1, ARect.Top);
    LineTo(ARect.Left, ARect.Top);
    LineTo(ARect.Left, ARect.Bottom-1);
    if FBevelStyle=bvRaised then
      Pen.Color:=ShColor
    else
      Pen.Color:=HLColor;
    LineTo(ARect.Right-1, ARect.Bottom-1);
    LineTo(ARect.Right-1, ARect.Top);
  end;
end;

procedure TSRLabel.PaintText(ARect: TRect; AFlags: Word);
var AText      : string;
    TextHeight,
    TextWidth  : integer;
    TextRect,
    OutRect    : TRect;
begin
  if not FShowAccelChar then
    AFlags:=AFlags or DT_NOPREFIX;
  Canvas.Font:=Font;
  TextRect:=ARect;
  GetTextAndTextRect(AText, TextRect);
  TextHeight:=TextRect.Bottom-TextRect.Top;
  TextWidth:=TextRect.Right-TextRect.Left;

  { horizontale und vertikale Offsets berechnen }
  OutRect:=TextRect;
  if FAlignment=taLeftJustify then
    OffsetRect(OutRect, FBorderWidth, 0);
  if FAlignment=taCenter then
    OffsetRect(OutRect, (Width-TextWidth) div 2, 0);
  if FAlignment=taRightJustify then
    OffsetRect(OutRect, Width-TextWidth-2-FBorderWidth, 0);
  if FLayout=tlTop then
    OffsetRect(OutRect, 0, FBorderWidth);
  if FLayout=tlCenter then
    OffsetRect(OutRect, 0, ((Height-TextHeight) div 2));
  if FLayout=tlBottom then
    OffsetRect(OutRect, 0, Height-TextHeight-FBorderWidth);
  if FShowHighlight then begin
    if FHighlightPos=hpTopLeft then
      OffsetRect(OutRect, FHighlightOffset, 0)
    else
      if Alignment=taRightJustify then
        OffsetRect(OutRect, -FHighlightOffset, 0);
    if FLayout=tlTop then
      OffsetRect(OutRect, 0, FHighlightOffset);
  end;
  if FShowShadow then begin
    if FHighlightPos=hpTopRight then
      OffsetRect(OutRect, FShadowOffset, 0)
    else
      if Alignment=taRightJustify then
        OffsetRect(OutRect, -FShadowOffset, 0);
    if FLayout=tlBottom then
      OffsetRect(OutRect, 0, -FShadowOffset);
  end;

  if not Enabled then begin
    OffsetRect(OutRect, 1, 1);
    Canvas.Font.Color:=clBtnHighlight;
    DrawTextComp(AText, OutRect, AFlags);
    OffsetRect(OutRect, -1, -1);
    Canvas.Font.Color:=clBtnShadow;
    DrawTextComp(AText, OutRect, AFlags);
  end
  else begin

    { Highlight-Text ausgeben }
    if FShowHighlight then begin
      Canvas.Font.Color:=FHighlightColor;
      TextRect:=OutRect;
      { horizontaler Offset }
      if FHighlightPos=hpTopLeft then
        OffsetRect(TextRect, -FHighlightOffset, -FHighlightOffset)
      else
        OffsetRect(TextRect, FHighlightOffset, -FHighlightOffset);
      DrawTextComp(AText, TextRect, AFlags);
    end;

    { Shadow-Text ausgeben }
    if FShowShadow then begin
      Canvas.Font.Color:=FShadowColor;
      TextRect:=OutRect;
      { horizontaler Offset }
      if FHighlightPos=hpTopLeft then
        OffsetRect(TextRect, FShadowOffset, FShadowOffset)
      else
        OffsetRect(TextRect, -FShadowOffset, FShadowOffset);
      DrawTextComp(AText, TextRect, AFlags);
    end;

    { Haupttext ausgeben }
    Canvas.Font.Color:=Font.Color;
    if FLinkActive and FMouseOnControl and Enabled then begin
      if FHighlightOnEnter then
        Canvas.Font.Color:=CalcHighlightColor(Font.Color);
      if FUnderlineOnEnter then
        Canvas.Font.Style:=Font.Style+[fsUnderline];
    end;
    DrawTextComp(AText, OutRect, AFlags);

  end;
end;

procedure TSRLabel.Paint;
const
  Alignments: array[TAlignment] of Word =
   (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word =
   (0, DT_WORDBREAK);
var DrawStyle : Integer;
begin
  with Canvas do begin

    { Hintergrund }
    if not Transparent then begin
      Brush.Color:=Self.Color;
      Brush.Style:=bsSolid;
      FillRect(ClientRect);
    end;

    { Text }
    Brush.Style:=bsClear;
    DrawStyle:=DT_EXPANDTABS or WordWraps[FWordWrap] or Alignments[FAlignment];
    PaintText(ClientRect, DrawStyle);

    { Rahmen }
    if FBevelStyle<>bvNone then
      PaintBevel(ClientRect);
  end;
end;

procedure TSRLabel.SetAlignment(Value: TAlignment);
begin
  if FAlignment<>Value then begin
    FAlignment:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetAutoSize(Value: Boolean);
begin
  if FAutoSize<>Value then begin
    FAutoSize:=Value;
    AdjustBounds;
  end;
end;

procedure TSRLabel.SetBevelStyle(Value: TPanelBevel);
begin
  if FBevelStyle<>Value then begin
    FBevelStyle:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetBorderWidth(Value: word);
begin
  if FBorderWidth<>Value then begin
    FBorderWidth:=Value;
    AdjustBounds;
  end;
end;

{$IFDEF SR_Delphi2_Up}
procedure TSRLabel.SetFocusControl(Value: TWinControl);
begin
  FFocusControl:=Value;
  if Value<>nil then
    Value.FreeNotification(Self);
end;
{$ENDIF}

procedure TSRLabel.SetHighlightColor(Value: TColor);
begin
  if FHighlightColor<>Value then begin
    FHighlightColor:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetHighlightOffset(Value: word);
begin
  if FHighlightOffset<>Value then begin
    FHighlightOffset:=Value;
    if FShowHighlight then begin
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TSRLabel.SetHighlightOnEnter(Value: boolean);
begin
  if FHighlightOnEnter<>Value then begin
    FHighlightOnEnter:=Value;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRLabel.SetHighlightPos(Value: THighlightPos);
begin
  if FHighlightPos<>Value then begin
    FHighlightPos:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout<>Value then begin
    FLayout:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShadowColor(Value: TColor);
begin
  if FShadowColor<>Value then begin
    FShadowColor:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShadowOffset(Value: word);
begin
  if FShadowOffset<>Value then begin
    FShadowOffset:=Value;
    if FShowShadow then begin
      AdjustBounds;
      Invalidate;
    end;
  end;
end;

procedure TSRLabel.SetShortenFilenames(Value: Boolean);
begin
  if FShortenFilenames<>Value then begin
    FShortenFilenames:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowAccelChar(Value: Boolean);
begin
  if FShowAccelChar<>Value then begin
    FShowAccelChar:=Value;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowHighlight(Value: Boolean);
begin
  if FShowHighlight<>Value then begin
    FShowHighlight:=Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRLabel.SetShowShadow(Value: Boolean);
begin
  if FShowShadow<>Value then begin
    FShowShadow:=Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TSRLabel.SetTransparent(Value: Boolean);
begin
  if Transparent<>Value then begin
    if Value then
      ControlStyle:=ControlStyle-[csOpaque]
    else
      ControlStyle:=ControlStyle+[csOpaque];
    Invalidate;
  end;
end;

procedure TSRLabel.SetUnderlineOnEnter(Value: boolean);
begin
  if FUnderlineOnEnter<>Value then begin
    FUnderlineOnEnter:=Value;
    if FMouseOnControl then
      Invalidate;
  end;
end;

procedure TSRLabel.SetWordWrap(Value: Boolean);
begin
  if FWordWrap<>Value then begin
    FWordWrap:=Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TSRLabel]);
end;

end.