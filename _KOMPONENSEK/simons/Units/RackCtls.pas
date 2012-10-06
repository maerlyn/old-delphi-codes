unit RackCtls;

{ RackControls:
  TLEDButton, TButtonPanel, TScrewPanel, TLEDDisplay, TLEDMeter

  (C)opyright 2000 Version 1.04
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  RackControls ist eine Komponentensammlung zur Erstellung von
  Audiorack-ähnlichen Oberflächen. Diese Komponenten sind
  Public Domain, das Urheberrecht liegt aber beim Autor.

  Die Komponente TLEDDisplay ist eine Weiterentwicklung
  der Komponente TLCDDisplay von Luis Iglesias:
  luis.iglesias@vigo.org

  Änderungen, die bei LEDDisplay nachfolgende Nullen bei LeadingZeros=False doch zeichnet
  Ergänzt von Wolfgang Kleinrath

  Eigenschaft FSingleLED ergänzt von U. Conrad }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, {$ENDIF} Classes,
  Graphics, Controls, ExtCtrls, SysUtils, Messages, Forms;

type
  TBorderStyle     = (bsNone, bsSingle);
  TButtonDirection = (bdBottomUp, bdLeftUp, bdNone, bdRightUp, bdTopUp);
  TContrast        = 0..9;
  TDecSeperator    = (dsPoint,dsComma,dsDoublePoint,dsMinus);
  TMeterDirection  = (mdDown, mdLeft, mdRight, mdUp);
  TNumGlyphs       = 0..4;
  TScrewSize       = 1..8;
  TSegmentStyle    = (ssRectangular, ssBeveled);
  TTextPosition    = (tpAbove, tpBelow, tpNone, tpOnButton);

  TLEDButton = class(TGraphicControl)
  private
    FBeveled:          boolean;
    FBorderStyle:      TBorderStyle;
    FButtonDirection:  TButtonDirection;
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorLED:         TColor;
    FColorLEDOff:      TColor;
    FColorShadow:      TColor;
    FDepth:            integer;
    FDown:             boolean;
    FFont:             TFont;
    FGlyph:            TBitmap;
    FLEDContrast:      TContrast;
    FNumGlyphs:        TNumGlyphs;
    FShowLED:          boolean;
    FStateOn:          boolean;
    FSwitching:        boolean;
    FTextPosition:     TTextPosition;

    FMouseDown:        boolean;
    FOnClick:          TNotifyEvent;

  protected
    procedure Paint;  override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
       override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
       override;

    procedure SetBeveled(newValue: boolean);
    procedure SetBorderStyle(newBorderStyle: TBorderStyle);
    procedure SetButtonDirection(NewDirection: TButtonDirection);
    procedure SetColor(newColor: TColor);
    procedure SetColorLED(newColor: TColor);
    procedure SetDepth(newValue: integer);
    procedure SetFont(newFont: TFont);
    procedure SetGlyph(newGlyph: TBitmap);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetNumGlyphs(newNumGlyphs: TNumGlyphs);
    procedure SetShowLED(newValue: boolean);
    procedure SetStateOn(newValue: boolean);
    procedure SetTextPosition(newValue: TTextPosition);

    procedure DrawBorder(Dest:TRect);
    procedure DrawCaption(Dest:TRect);
    procedure DrawGlyph(Dest:TRect);
    procedure DrawLED(var Dest:TRect);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CMTextChanged(var msg: TMessage);message CM_TEXTCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar);message CM_DIALOGCHAR;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    property Anchors;
    {$ENDIF}
    property Beveled: boolean read FBeveled write SetBeveled;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ButtonDirection: TButtonDirection read FButtonDirection write SetButtonDirection;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property ColorLED: TColor read FColorLED write SetColorLED;
    property Depth: integer read FDepth write SetDepth;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property Glyph: TBitmap read FGlyph write SetGlyph;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    {$IFDEF SR_Delphi3_Up}
    property PopupMenu;
    {$ENDIF}
    property ShowHint;
    property ShowLED: boolean read FShowLED write SetShowLED;
    property StateOn: boolean read FStateOn write SetStateOn;
    property Switching: boolean read FSwitching write FSwitching;
    property TextPosition: TTextPosition read FTextPosition write SetTextPosition;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TButtonPanel = class(TCustomPanel)
  private
    FBeveled:          boolean;
    FBorderStyle:      TBorderStyle;
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorShadow:      TColor;
    FDepth:            integer;
    FPanelDirection:   TButtonDirection;
    FShowLED:          boolean;

  protected
    procedure Paint;  override;

    procedure SetBeveled(newValue: boolean);
    procedure SetBorderStyle(newBorderStyle: TBorderStyle);
    procedure SetColor(newColor: TColor);
    procedure SetDepth(newValue: integer);
    procedure SetPanelDirection(NewDirection: TButtonDirection);
    procedure SetShowLED(newValue: boolean);

    procedure DrawBorder(Dest:TRect);
    procedure DrawCaption(Dest:TRect);
    procedure DrawLED(var Dest:TRect);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    property Alignment;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property Beveled: boolean read FBeveled write SetBeveled;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property Ctl3D;
    property Depth: integer read FDepth write SetDepth;
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi3_Up}
    property FullRepaint;
    {$ENDIF}
    property Font;
    property Locked;
    property PanelDirection: TButtonDirection read FPanelDirection write SetPanelDirection;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowLED: boolean read FShowLED write SetShowLED;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF SR_Delphi3_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

  TScrewPanel = class(TCustomPanel)
  private
    FColor:            TColor;
    FColorHighlight:   TColor;
    FColorShadow:      TColor;
    FMargin:           integer;
    FScrewSize:        TScrewSize;
    FShowScrews:       boolean;

  protected
    procedure Paint;  override;

    procedure SetColor(newColor: TColor);
    procedure SetMargin(newValue: integer);
    procedure SetScrewSize(newValue: TScrewSize);
    procedure SetShowScrews(newValue: boolean);

    procedure DrawScrew(X,Y:integer);
    procedure DrawBevel(ARect:TRect;Raised:boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    property Alignment;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color: TColor read FColor write SetColor;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    {$IFDEF SR_Delphi3_Up}
    property FullRepaint;
    {$ENDIF}
    property Font;
    property Locked;
    property Margin: integer read FMargin write SetMargin;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrewSize: TScrewSize read FScrewSize write SetScrewSize;
    property ShowHint;
    property ShowScrews: boolean read FShowScrews write SetShowScrews;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF SR_Delphi3_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

  TLEDDisplay = class(TGraphicControl)
  private
    FBevelStyle      : TPanelBevel;
    FBorderStyle     : TBorderStyle;
    FColorBackGround,
    FColorLED        : TColor;
    FDecSeperator    : TDecSeperator;
    FDigit           : array [0..10] of TBitmap;
    FDigitHeight,
    FDigitWidth,
    FFractionDigits  : integer;
    FLEDContrast     : TContrast;
    FLEDOffColor     : TColor;
    FLineWidth,
    FNumDigits       : integer;
    FLeadingZeros    : boolean;
    FSegCl           : array [0..10, 1..7] of TColor;
    FSegmentStyle    : TSegmentStyle;
    FValue           : extended;

    FOnChange        : TNotifyEvent;

    procedure SetBevelStyle (newValue: TPanelBevel);
    procedure SetBorderStyle (newValue: TBorderStyle);
    procedure SetColorBackGround (newValue: TColor);
    procedure SetColorLED (newValue: TColor);
    procedure SetDecSeperator (newValue: TDecSeperator);
    procedure SetDigitHeight (newValue: integer);
    procedure SetDigitWidth (newValue: integer);
    procedure SetFractionDigits (newValue: integer);
    procedure SetLeadingZeros (newValue: boolean);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetLineWidth (newValue: integer);
    procedure SetNumDigits (newValue: integer);
    procedure SetSegmentStyle (newValue: TSegmentStyle);
    procedure SetValue (newValue: extended);

    procedure GenerateBitMaps;
    procedure AssignColors (seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);

  protected
    procedure paint; override;
    procedure Change; dynamic;


  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    property ColorBackGround: TColor read FColorBackGround write setColorBackGround default clOlive;
    property ColorLED: TColor read FColorLED write setColorLED default cllime;
    property DecSeperator: TDecSeperator read FDecSeperator write setDecSeperator;
    property DigitHeight: integer read FDigitHeight write setDigitHeight default 30;
    property DigitWidth: integer read FDigitWidth write setDigitWidth default 20;
    property DigitLineWidth: integer read FLineWidth write setLineWidth default 3;
    property FractionDigits: integer read FFractionDigits write setFractionDigits default 0;
    property Height default 36;
    property LeadingZeros: boolean read FLeadingZeros write setLeadingZeros default true;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property NumDigits: integer read FNumDigits write setNumDigits default 6;
    property SegmentStyle: TSegmentStyle read FSegmentStyle write setSegmentStyle;
    property Value: extended read FValue write setValue;
    property Visible;
    property Width default 168;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TLEDMeter = class(TGraphicControl)
  private
    FBevelStyle     : TPanelBevel;
    FColorLED1,
    FColorLED2,
    FColorLED3,
    FColorOff1,
    FColorOff2,
    FColorOff3,
    FColorSeperator : TColor;
    FDirection      : TMeterDirection;
    FLEDContrast    : TContrast;
    FMax, FMin,
    FNumDigits,
    FPosition	    : integer;
    FSingleLED      : boolean;
    FStartColor2,
    FStartColor3    : integer;

    FOnChange       : TNotifyEvent;

    procedure SetBevelStyle(newVal : TPanelBevel);
    procedure SetColorLED1(newVal : tColor);
    procedure SetColorLED2(newVal : tColor);
    procedure SetColorLED3(newVal : tColor);
    procedure SetColorSeperator(newVal : tColor);
    procedure SetDirection(newVal : TMeterdirection);
    procedure SetLEDContrast(newContrast: TContrast);
    procedure SetMax(newVal : integer);
    procedure SetMin(newVal : integer);
    procedure SetNumDigits(newVal : integer);
    procedure SetPosition(newVal : integer);
    procedure SetSingleLED(newVal : boolean);
    procedure SetStartColor2(newVal : integer);
    procedure SetStartColor3(newVal : integer);

  protected
    procedure Paint;override;
    procedure Change; dynamic;

  public
    constructor Create(AOwner : TComponent);override;
    destructor Destroy ; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property BevelStyle: TPanelBevel read FBevelStyle write SetBevelStyle;
    property ColorLED1 : TColor read FColorLED1 write setColorLED1;
    property ColorLED2 : TColor read FColorLED2 write setColorLED2;
    property ColorLED3 : TColor read FColorLED3 write setColorLED3;
    property ColorSeperator: TColor read FColorSeperator write setColorSeperator;
    property Cursor;
    property Direction: TMeterDirection read FDirection write setDirection;
    property DragCursor;
    property DragMode;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property Max: integer read FMax write setMax;
    property Min: integer read FMin write setMin;
    property NumDigits: integer read FNumDigits write setNumDigits;
    property Position: integer read FPosition write setPosition;
    property SingleLED : boolean read FSingleLED write setSingleLED;
    property StartColor2: integer read FStartColor2 write setStartColor2;
    property StartColor3: integer read FStartColor3 write setStartColor3;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
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
uses rrColors;
{$ELSE}
{$R *.D16}
{$ENDIF}

const
  DefaultWidth  = 45;
  DefaultHeight = 45;
  DefaultDepth  = 3;
  FHLContrast   = 5;
  FShContrast   = 4;

function IsAccellerator(VK: Word; const Str: string): Boolean;
var
  P : Integer;
begin
  P := Pos('&', Str);
  Result := (P <> 0) and (P < Length(Str)) and
    (Upcase(Str[P + 1])=Upcase(Char(VK)));
end;

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  { Rot }
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
  { Grün }
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  { Blau }
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

procedure AssignBevelColors(FaceColor:TColor;var HighlightColor,ShadowColor:TColor;HLContrast,ShContrast:integer);
begin
  {$IFDEF SR_Delphi1}
  HighlightColor:=ChangeBrightness(FaceColor,100 div 10*HLContrast);
  ShadowColor:=ChangeBrightness(FaceColor,-100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FaceColor,HighlightColor,ShadowColor,(10-HLContrast)/10,(10-ShContrast)/10);
  {$ENDIF}
end;


{ Komponente TLEDButton }
constructor TLEDButton.Create(AOwner: TComponent);
var Dummy : TColor;
begin
  inherited Create(AOwner);

  { Vorgabewerte setzen }
  FBeveled:=true;
  FBorderStyle:=bsSingle;
  FButtonDirection:=bdBottomUp;
  FColor:=clGray;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FColorLED:=clBlue;
  FLEDContrast:=6;
  AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
  FDepth:=DefaultDepth;
  FDown:=false;
  FFont:=TFont.Create;
  FGlyph:=TBitmap.Create;
  FNumGlyphs:=1;
  FShowLED:=true;
  FStateOn:=false;
  FSwitching:=true;
  FTextPosition:=tpNone;
  Height:=DefaultHeight;
  Width:=DefaultWidth;

  FMouseDown:=False;
end;

destructor  TLEDButton.Destroy;
begin
  FFont.Free;
  FGlyph.Free;
  inherited Destroy;
end;

procedure TLEDButton.SetBeveled(NewValue: boolean);
begin
  if FBeveled<>NewValue then begin
    FBeveled:=NewValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetBorderStyle(NewBorderStyle: TBorderStyle);
begin
  if FBorderStyle<>NewBorderStyle then begin
    FBorderStyle:=NewBorderStyle;
    Invalidate;
  end;
end;

procedure TLEDButton.SetButtonDirection(NewDirection: TButtonDirection);
begin
  if FButtonDirection<>NewDirection then begin
    FButtonDirection:=NewDirection;
    Invalidate;
  end;
end;

procedure TLEDButton.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetColorLED(newColor: TColor);
var Dummy : TColor;
begin
  if FColorLED<>newColor then begin
    FColorLED:=newColor;
    AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetDepth(newValue: integer);
begin
  if FDepth<>newValue then begin
    FDepth:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetFont(newFont: TFont);
begin
  FFont.Assign(NewFont);
  Invalidate;
end;

procedure TLEDButton.SetGlyph(newGlyph: TBitmap);
begin
  if(Assigned(FGlyph)) then begin
    FGlyph.Assign(newGlyph);

    if (csDesigning in ComponentState) then begin
      { Glyph 1: Normal, 2: Disabled, 3: Down;
        Muß die Ausmaße (Height * NumGlyphs) = Width  haben}
      if (newGlyph.width mod newGlyph.height = 0) then
        FNumGlyphs:= newGlyph.width div newGlyph.height
      else
        FNumGlyphs:= 1;
    end;

    Invalidate;
  end;
end;

procedure TLEDButton.SetLEDContrast(newContrast: TContrast);
var Dummy : TColor;
begin
  if (FLEDContrast<>newContrast) and (newContrast>=0) and (newContrast<10) then begin
    FLEDContrast:=newContrast;
    AssignBevelColors(FColorLED,Dummy,FColorLEDOff,FLEDContrast,FLEDContrast);
    Invalidate;
  end;
end;

procedure TLEDButton.SetNumGlyphs(newNumGlyphs: TNumGlyphs);
begin
  if FNumGlyphs<>newNumGlyphs then begin
    FNumGlyphs:=newNumGlyphs;
    Invalidate;
  end;
end;

procedure TLEDButton.SetShowLED(newValue: boolean);
begin
  if FShowLED<>newValue then begin
    FShowLED:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetStateOn(newValue: boolean);
begin
  if FStateOn<>newValue then begin
    FStateOn:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.SetTextPosition(newValue: TTextPosition);
begin
  if FTextPosition<>newValue then begin
    FTextPosition:=newValue;
    Invalidate;
  end;
end;

procedure TLEDButton.DrawBorder(Dest:TRect);
var i : integer;
begin
  Dest:=GetClientRect;
  if FTextPosition=tpAbove then
    Dest.Top:=Dest.Top+Canvas.TextWidth('W')+2;
  if FTextPosition=tpBelow then
    Dest.Bottom:=Dest.Bottom-Canvas.TextWidth('W')-2;
  with Canvas do begin
    if FBorderStyle=bsSingle then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Dest);
      InflateRect(Dest,-1,-1);
    end;
    Pen.Width:=1;
    if FButtonDirection=bdBottomUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Left,Dest.Top);
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          if FDown then
            LineTo(Dest.Left+(i div 2),Dest.Bottom-1)
          else
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          if FDown then
            LineTo(Dest.Right-(i div 2)-1,Dest.Bottom-1)
          else
            LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      { unten }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left,Dest.Bottom-i-1);
            LineTo(Dest.Right-1,Dest.Bottom-i-1);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Bottom-i-1);
            LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
          end;
        end;
    end;
    if FButtonDirection=bdTopUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left,Dest.Top+i);
            LineTo(Dest.Right-1,Dest.Top+i);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Top+i);
            LineTo(Dest.Right-i-1,Dest.Top+i);
          end;
        end;
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Left+(i div 2),Dest.Top)
          else
            LineTo(Dest.Left+i,Dest.Top+i);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Right-(i div 2)-1,Dest.Top)
          else
            LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      { unten }
      MoveTo(Dest.Right-1,Dest.Bottom-1);
      LineTo(Dest.Left,Dest.Bottom-1);
    end;
    if FButtonDirection=bdLeftUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          if FDown then
            LineTo(Dest.Left,Dest.Top+(i div 2))
          else
            LineTo(Dest.Left+i,Dest.Top+i);
        end;
      { links }
      if FDown then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Left+i,Dest.Top);
            LineTo(Dest.Left+i,Dest.Bottom-1);
          end
          else begin
            MoveTo(Dest.Left+i,Dest.Top+i);
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
          end;
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Right-1,Dest.Bottom-1);
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Bottom-1);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Left,Dest.Bottom-(i div 2)-1)
          else
            LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
    end;
    if FButtonDirection=bdRightUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          if FDown then
            LineTo(Dest.Right-1,Dest.Top+(i div 2))
          else
            LineTo(Dest.Right-1-i,Dest.Top+i);
        end;
      { links }
      MoveTo(Dest.Left,Dest.Top);
      LineTo(Dest.Left,Dest.Bottom-1);
      Pen.Color:=FColorShadow;
      { rechts }
      if FDown then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          if not FBeveled then begin
            MoveTo(Dest.Right-1-i,Dest.Top);
            LineTo(Dest.Right-1-i,Dest.Bottom-1);
          end
          else begin
            MoveTo(Dest.Right-1-i,Dest.Top+i);
            LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
          end;
        end;
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          if FDown then
            LineTo(Dest.Right-1,Dest.Bottom-(i div 2)-1)
          else
            LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
    end;
  end;
end;

procedure TLEDButton.DrawCaption(Dest:TRect);
var OutText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsClear;
    StrPCopy(OutText, Caption);
    if not Enabled then
      Font.Color:=clGrayText;
    if FTextPosition=tpAbove then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_top or dt_singleline);
    if FTextPosition=tpBelow then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_bottom or dt_singleline);
    if FTextPosition=tpOnButton then
      DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_vcenter or dt_singleline);
  end;
end;

procedure TLEDButton.DrawGlyph(Dest:TRect);
var
  Source    : TRect;
  outWidth  : integer;
  outHeight : integer;
begin
  with Canvas do begin
    { Größe des Destination-Rechtecks }
    outWidth:=  FGlyph.Width div FNumGlyphs;
    outHeight:= FGlyph.Height;
    with Source do begin
      Top:=0;
      Bottom:=FGlyph.Height;
      { Glyph 1: Normal, 2: Disabled, 3: Down;}
      if Enabled then begin
        if FStateOn and (FNumGlyphs>2) then
          Left:=(outWidth*2)+1
        else
          Left:=0;
      end
      else
        Left:=outWidth+1;
      Right:= Left+outWidth;
    end;
    Dest.Left:=  ((Dest.Right +Dest.Left - outWidth)  shr 1);
    Dest.Right:= Dest.Left+outWidth;
    Dest.Top:=   ((Dest.Bottom + Dest.Top - outHeight) shr 1);
    Dest.Bottom:=Dest.Top+outHeight;
    Pen.Color := Color;
    BrushCopy(Dest,FGlyph,Source,FGlyph.Canvas.Pixels[0,FGlyph.Height-1]);
  end;
end;

procedure TLEDButton.DrawLED(var Dest:TRect);
begin
  with Canvas do begin
    if FStateOn then
      Brush.Color:=FColorLED
    else
      Brush.Color:=FColorLEDOff;
    if not Enabled then
      Brush.Color:=FColor;
    case ButtonDirection of
      bdLeftUp : begin
        if FDown then
          OffsetRect(Dest,-FDepth div 2,0);
        Rectangle(Dest.Left+FDepth+9,Dest.Top+FDepth+3,Dest.Left+FDepth+4,Dest.Bottom-FDepth-3);
        Dest.Left:=Dest.Left+FDepth+9;
      end;
      bdRightUp : begin
        if FDown then
          OffsetRect(Dest,FDepth div 2,0);
        Rectangle(Dest.Right-FDepth-9,Dest.Top+FDepth+3,Dest.Right-FDepth-4,Dest.Bottom-FDepth-3);
        Dest.Right:=Dest.Right-FDepth-9;
      end;
      bdTopUp : begin
        if FDown then
          OffsetRect(Dest,0,-FDepth div 2);
        Rectangle(Dest.Left+FDepth+3,Dest.Top+FDepth+4,Dest.Right-FDepth-3,Dest.Top+FDepth+9);
        Dest.Top:=Dest.Top+FDepth+7;
      end;
      else begin
        if FDown then
          OffsetRect(Dest,0,FDepth div 2);
        Rectangle(Dest.Left+FDepth+3,Dest.Bottom-FDepth-9,Dest.Right-FDepth-3,Dest.Bottom-FDepth-4);
        Dest.Bottom:=Dest.Bottom-FDepth-7;
      end;
    end;
  end;
end;

procedure TLEDButton.Paint;
var ARect : TRect;
begin
  Canvas.Font.Assign(Font);
  with Canvas do begin
    ARect:=GetClientRect;
    if (Caption<>'') and (FTextPosition<>tpOnButton) and (FTextPosition<>tpNone) then
      DrawCaption(ARect);
    if FTextPosition=tpAbove then
      ARect.Top:=ARect.Top+TextWidth('W')+2;
    if FTextPosition=tpBelow then
      ARect.Bottom:=ARect.Bottom-TextWidth('W')-2;
    Brush.Style:=bsSolid;
    Brush.Color:=FColor;
    FillRect(ARect);
    DrawBorder(ARect);
    Pen.Color:=clBlack;
    if FShowLED then
      DrawLED(ARect);
    if (Caption<>'') and (FTextPosition=tpOnButton) then
      DrawCaption(ARect);
    Brush.Color:=Self.Color;
    if Assigned(FGlyph) and (FNumGlyphs > 0) and (FTextPosition<>tpOnButton) then
      DrawGlyph(ARect);
  end;
end;

procedure TLEDButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do begin
    if IsAccellerator(CharCode, Caption) then begin
      if Enabled then begin
        Click;
        if FSwitching then
          FStateOn:=not FStateOn;
        Invalidate;
      end;
      Result := 1;
    end
    else
      inherited;
  end;
end;

procedure TLEDButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    FDown:=true;
    Invalidate;
  end;
  FMouseDown:= True;
end;

procedure TLEDButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    FDown:=false;
    if FSwitching then
      FStateOn:=not FStateOn;
    Paint;
    if Assigned(FOnClick) then
       FOnClick(Self);
  end;
  FMouseDown:= False;
end;

procedure TLEDButton.CMTextChanged(var msg: TMessage);
begin
  Invalidate;
end;


{ Komponente TButtonPanel }
constructor TButtonPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Vorgabewerte setzen }
  FBeveled:=true;
  FBorderStyle:=bsSingle;
  FPanelDirection:=bdBottomUp;
  FColor:=clGray;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FDepth:=DefaultDepth;
  FShowLED:=true;
  Height:=DefaultHeight;
  Width:=DefaultWidth;
end;

destructor  TButtonPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TButtonPanel.SetBeveled(NewValue: boolean);
begin
  if FBeveled<>NewValue then begin
    FBeveled:=NewValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetBorderStyle(NewBorderStyle: TBorderStyle);
begin
  if FBorderStyle<>NewBorderStyle then begin
    FBorderStyle:=NewBorderStyle;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetPanelDirection(NewDirection: TButtonDirection);
begin
  if FPanelDirection<>NewDirection then begin
    FPanelDirection:=NewDirection;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TButtonPanel.SetDepth(newValue: integer);
begin
  if FDepth<>newValue then begin
    FDepth:=newValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.SetShowLED(newValue: boolean);
begin
  if FShowLED<>newValue then begin
    FShowLED:=newValue;
    Invalidate;
  end;
end;

procedure TButtonPanel.DrawBorder(Dest:TRect);
var i : integer;
begin
  Dest:=GetClientRect;
  with Canvas do begin
    if FBorderStyle=bsSingle then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Dest);
      InflateRect(Dest,-1,-1);
    end;
    Pen.Width:=1;
    if FPanelDirection=bdBottomUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Left,Dest.Top);
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      { unten }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left,Dest.Bottom-i-1);
          LineTo(Dest.Right-1,Dest.Bottom-i-1);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Bottom-i-1);
          LineTo(Dest.Right-i-1,Dest.Bottom-i-1);
        end;
      end;
    end;
    if FPanelDirection=bdTopUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left,Dest.Top+i);
          LineTo(Dest.Right-1,Dest.Top+i);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Top+i);
          LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      end;
      { links }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          LineTo(Dest.Left+i,Dest.Top+i);
        end;
      Pen.Color:=FColorShadow;
      { rechts }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Top);
        LineTo(Dest.Right-1,Dest.Bottom);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          LineTo(Dest.Right-i-1,Dest.Top+i);
        end;
      { unten }
      MoveTo(Dest.Right-1,Dest.Bottom-1);
      LineTo(Dest.Left,Dest.Bottom-1);
    end;
    if FPanelDirection=bdLeftUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Top);
          LineTo(Dest.Left+i,Dest.Top+i);
        end;
      { links }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Left+i,Dest.Top);
          LineTo(Dest.Left+i,Dest.Bottom-1);
        end
        else begin
          MoveTo(Dest.Left+i,Dest.Top+i);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
      end;
      Pen.Color:=FColorShadow;
      { rechts }
      MoveTo(Dest.Right-1,Dest.Top);
      LineTo(Dest.Right-1,Dest.Bottom-1);
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Right-1,Dest.Bottom-1);
        LineTo(Dest.Left,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Right-1,Dest.Bottom-1);
          LineTo(Dest.Left+i,Dest.Bottom-i-1);
        end;
    end;
    if FPanelDirection=bdRightUp then begin
      Pen.Color:=FColorHighlight;
      { oben }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Top);
        LineTo(Dest.Right-1,Dest.Top);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Top);
          LineTo(Dest.Right-1-i,Dest.Top+i);
        end;
      { links }
      MoveTo(Dest.Left,Dest.Top);
      LineTo(Dest.Left,Dest.Bottom-1);
      Pen.Color:=FColorShadow;
      { rechts }
      for i:=0 to FDepth do begin
        if not FBeveled then begin
          MoveTo(Dest.Right-1-i,Dest.Top);
          LineTo(Dest.Right-1-i,Dest.Bottom-1);
        end
        else begin
          MoveTo(Dest.Right-1-i,Dest.Top+i);
          LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
      end;
      { unten }
      if not FBeveled then begin
        MoveTo(Dest.Left,Dest.Bottom-1);
        LineTo(Dest.Right-1,Dest.Bottom-1);
      end
      else
        for i:=0 to FDepth do begin
          MoveTo(Dest.Left,Dest.Bottom-1);
          LineTo(Dest.Right-1-i,Dest.Bottom-i-1);
        end;
    end;
  end;
end;

procedure TButtonPanel.DrawCaption(Dest:TRect);
var OutText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsClear;
    StrPCopy(OutText, Caption);
    DrawText(Handle, OutText, length(Caption), Dest, dt_center or dt_vcenter or dt_singleline);
  end;
end;

procedure TButtonPanel.DrawLED(var Dest:TRect);
begin
  with Canvas do begin
    Brush.Color:=clWindowFrame;
    case PanelDirection of
      bdLeftUp : begin
        FrameRect(Rect(Dest.Left+FDepth+9,Dest.Top+FDepth+3,Dest.Left+FDepth+4,Dest.Bottom-FDepth-3));
        Dest.Left:=Dest.Left+FDepth+9;
      end;
      bdRightUp : begin
        FrameRect(Rect(Dest.Right-FDepth-9,Dest.Top+FDepth+3,Dest.Right-FDepth-4,Dest.Bottom-FDepth-3));
        Dest.Right:=Dest.Right-FDepth-9;
      end;
      bdTopUp : begin
        FrameRect(Rect(Dest.Left+FDepth+3,Dest.Top+FDepth+4,Dest.Right-FDepth-3,Dest.Top+FDepth+9));
        Dest.Top:=Dest.Top+FDepth+7;
      end;
      else begin
        FrameRect(Rect(Dest.Left+FDepth+3,Dest.Bottom-FDepth-9,Dest.Right-FDepth-3,Dest.Bottom-FDepth-4));
        Dest.Bottom:=Dest.Bottom-FDepth-7;
      end;
    end;
  end;
end;

procedure TButtonPanel.Paint;
var ARect : TRect;
begin
  Canvas.Font.Assign(Font);
  with Canvas do begin
    ARect:=GetClientRect;
    Brush.Style:=bsSolid;
    Brush.Color:=FColor;
    FillRect(ARect);
    DrawBorder(ARect);
    Pen.Color:=clBlack;
    if FShowLED then
      DrawLED(ARect);
    if Caption<>'' then
      DrawCaption(ARect);
  end;
end;


{ Komponente TScrewPanel }
constructor TScrewPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {Vorgabewerte setzen}
  FColor:=clBtnFace;
  AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
  FMargin:=2;
  FScrewSize:=2;
  FShowScrews:=true;
  Height:=DefaultHeight;
  Width:=DefaultWidth;
end;

destructor TScrewPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TScrewPanel.SetColor(newColor: TColor);
begin
  if FColor<>newColor then begin
    FColor:=newColor;
    AssignBevelColors(FColor,FColorHighlight,FColorShadow,FHLContrast,FShContrast);
    Invalidate;
  end;
end;

procedure TScrewPanel.SetMargin(newValue: integer);
begin
  if (NewValue<(Width-FScrewSize)) and (NewValue<(Height-FScrewSize)) and (FMargin<>newValue) then begin
    FMargin:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.SetScrewSize(newValue: TScrewSize);
begin
  if (NewValue<Width) and (NewValue<Height) and (FScrewSize<>newValue) then begin
    FScrewSize:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.SetShowScrews(newValue: boolean);
begin
  if FShowScrews<>newValue then begin
    FShowScrews:=newValue;
    Invalidate;
  end;
end;

procedure TScrewPanel.DrawScrew(X,Y:integer);
var Size : integer;
begin
  Size:=FScrewSize*4;
  with Canvas do begin
    Pen.Color:=FColorShadow;
    Brush.Color:=clSilver;
    Ellipse(X,Y,X+Size,Y+Size);
    Arc(X,Y,X+Size,Y+Size,
        X+((Size div 4)*3),Y+(Size div 4),
        X+(Size div 4),Y+((Size div 4)*3));
    Pen.Color:=clGray;
    MoveTo(X+(Size div 4)-1,Y+((Size div 4)*3)-1);
    LineTo(X+((Size div 4)*3),Y+(Size div 4)-2);
    Pen.Color:=FColorHighlight;
    Arc(X,Y,X+Size,Y+Size,
        X+(Size div 4),Y+((Size div 4)*3),
        X+((Size div 4)*3),Y+(Size div 4));
    Pen.Color:=clWhite;
    MoveTo(X+(Size div 4),Y+((Size div 4)*3));
    LineTo(X+((Size div 4)*3)+1,Y+(Size div 4)-1);
  end;
end;

procedure TScrewPanel.DrawBevel(ARect:TRect;Raised:boolean);
begin
  with Canvas do begin
    Pen.Width:=BevelWidth;
    if Raised then
      Pen.Color:=FColorHighlight
    else
      Pen.Color:=FColorShadow;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Left,ARect.Top);
    LineTo(ARect.Left,ARect.Bottom-1);
    if Raised then
      Pen.Color:=FColorShadow
    else
      Pen.Color:=FColorHighlight;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Right-1,ARect.Bottom-1);
    LineTo(ARect.Left,ARect.Bottom-1);
  end;
end;

procedure TScrewPanel.Paint;
var ARect   : TRect;
    Border  : integer;
    outText : array [0..79] of char;
begin
  with Canvas do begin
    Brush.Style:=bsSolid;
    Brush.Color:=Self.Color;
    ARect:=GetClientRect;
    FillRect(ARect);
    if BevelOuter<>bvNone then begin
      DrawBevel(ARect,BevelOuter=bvRaised);
      Border:=BevelWidth+BorderWidth;
    end
    else
      Border:=BorderWidth;
    InflateRect(ARect,-Border,-Border);
    if BevelInner<>bvNone then begin
      DrawBevel(ARect,BevelInner=bvRaised);
      InflateRect(ARect,-BevelWidth,-BevelWidth);
    end;
    if FShowScrews then begin
      DrawScrew(ARect.Left+FMargin,ARect.Top+FMargin);
      DrawScrew(ARect.Right-FMargin-(FScrewSize*4),ARect.Top+FMargin);
      DrawScrew(ARect.Left+FMargin,ARect.Bottom-FMargin-(FScrewSize*4));
      DrawScrew(ARect.Right-FMargin-(FScrewSize*4),ARect.Bottom-FMargin-(FScrewSize*4));
    end;
    Font:=Self.Font;
    Brush.Style:=bsClear;
    StrPCopy(outText,Caption);
    if Alignment=taCenter then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_CENTER);
    if Alignment=taLeftJustify then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_LEFT);
    if Alignment=taRightJustify then
      DrawText(Handle,outText,length(Caption),Arect,DT_SINGLELINE or DT_VCENTER or DT_RIGHT);
  end;
end;


{ Komponente TLEDDisplay }
procedure TLEDDisplay.AssignColors(seg:integer; s1,s2,s3,s4,s5,s6,s7:Boolean);
begin
  if s1 then
    FSegCl[seg, 1] := FColorLED
  else
    FSegCl[seg, 1] := FLEDOffColor;
  if s2 then
    FSegCl[seg, 2] := FColorLED
  else
    FSegCl[seg, 2] := FLEDOffColor;
  if s3 then
    FSegCl[seg, 3] := FColorLED
  else
    FSegCl[seg, 3] := FLEDOffColor;
  if s4 then
    FSegCl[seg, 4] := FColorLED
  else
    FSegCl[seg, 4] := FLEDOffColor;
  if s5 then
    FSegCl[seg, 5] := FColorLED
  else
    FSegCl[seg, 5] := FLEDOffColor;
  if s6 then
    FSegCl[seg, 6] := FColorLED
  else
    FSegCl[seg, 6] := FLEDOffColor;
  if s7 then
    FSegCl[seg, 7] := FColorLED
  else
    FSegCl[seg, 7] := FLEDOffColor;
end;

procedure TLEDDisplay.GenerateBitMaps;
var
  TL, TR, TBL, TBR,
  ML, MTL, MTR, MR,
  MBL, MBR, BL, BTL,
  BTR, BR            : TPoint;
  c, wAlt, LineW     : integer;
begin
  LineW:=FLineWidth+2;
  wAlt := FDigitHeight;
  { Polygonpunkte zuweisen }
  TL.x := 0;
  TL.y := 0;
  TR.x := FDigitWidth-1;
  TR.y := 0;
  TBL.x := LineW - 1;
  TBL.y := LineW -1;
  TBR.x := FDigitWidth - LineW;
  TBR.y := TBL.y;
  ML.x := 0;
  ML.y := wAlt div 2;
  MTL.x := TBL.x;
  MTL.y := ML.y - (LineW div 2);
  MTR.x := TBR.x;
  MTR.y := MTL.y;
  MR.x := TR.x;
  MR.y := ML.y;
  MBL.x := TBL.x;
  MBL.y := ML.y + (LineW div 2);
  MBR.x := MTR.x; MBR.y := MBL.y;
  BL.x := 0;
  BL.y := wAlt - 1;
  BR.x := TR.x;
  BR.y := BL.y;
  BTL.x := TBL.x;
  BTL.y := wAlt - LineW;
  BTR.x := TBR.x;
  BTR.y := BTL.y;

  { Segmentfarben zuweisen }
  AssignColors (0, true, true, true, false, true, true, true);
  AssignColors (1, false, false, true, false, false, true, false);
  AssignColors (2, true, false, true, true, true, false, true);
  AssignColors (3, true, false, true, true, false, true, true);
  AssignColors (4, false, true, true, true, false, true, false);
  AssignColors (5, true, true, false, true, false, true, true);
  AssignColors (6, false, true, false, true, true, true, true);
  AssignColors (7, true, false, true, false, false, true, false);
  AssignColors (8, true, true, true, true, true, true, true);
  AssignColors (9, true, true, true, true, false, true, true);
  AssignColors (10, false, false, false, true, false, false, false);

  { Bitmap erstellen }
  for c := 0 to 10 do begin
    FDigit[c].free;
    FDigit[c] := TBitmap.create;
    FDigit[c].width := FDigitWidth;
    FDigit[c].height := wAlt;
    with FDigit[c].canvas do begin
      Pen.Color := FColorBackGround;
      Brush.Color := FColorBackGround;
      Brush.style := bsSolid;
      Pen.Width := 1;
      Rectangle (TL.x, TL.y, BR.x+1, BR.y+1);
      if FSegmentStyle=ssRectangular then
        Pen.Width:=FLineWidth;
      { Segment 1 }
      Brush.Color := FSegCl[c, 1];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 1];
        MoveTo(FLineWidth, FLineWidth div 2);
        LineTo(FDigit[c].Width-FLineWidth-1, FLineWidth div 2);
      end
      else
        Polygon ([TL, TR, TBR, TBL]);
      { Segment 2 }
      Brush.Color := FSegCl[c, 2];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 2];
        MoveTo(FLineWidth div 2, FLineWidth*3 div 2);
        LineTo(FLineWidth div 2, (FDigit[c].Height div 2)-FLineWidth);
      end
      else
        Polygon ([TL, TBL, MTL, ML]);
      { Segment 3 }
      Brush.Color := FSegCl[c, 3];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 3];
        MoveTo(FDigit[c].Width-(FLineWidth div 2)-1, FLineWidth*3 div 2);
        LineTo(FDigit[c].Width-(FLineWidth div 2)-1, (FDigit[c].Height div 2)-FLineWidth);
      end
      else
        Polygon ([TR, MR, MTR, TBR]);
      { Segment 4 }
      Brush.Color := FSegCl[c, 4];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 4];
        MoveTo(FLineWidth, FDigit[c].Height div 2);
        LineTo(FDigit[c].Width-FLineWidth, FDigit[c].Height div 2);
      end
      else
        Polygon ([ML, MTL, MTR, MR, MBR, MBL]);
      { Segment 5 }
      Brush.Color := FSegCl[c, 5];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 5];
        MoveTo(FLineWidth div 2, (FDigit[c].Height div 2)+FLineWidth);
        LineTo(FLineWidth div 2, FDigit[c].Height-(FLineWidth*3 div 2));
      end
      else
        Polygon ([ML, MBL, BTL, BL]);
      { Segment 6 }
      Brush.Color := FSegCl[c, 6];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 6];
        MoveTo(FDigit[c].Width-(FLineWidth div 2)-1, (FDigit[c].Height div 2)+FLineWidth);
        LineTo((FDigit[c].Width-FLineWidth div 2)-1, FDigit[c].Height-(FLineWidth*3 div 2));
      end
      else
        Polygon ([MR, BR, BTR, MBR]);
      { Segment 7 }
      Brush.Color := FSegCl[c, 7];
      if FSegmentStyle=ssRectangular then begin
        Pen.Color := FSegCl[c, 7];
        MoveTo(FLineWidth, FDigit[c].Height-(FLineWidth div 2)-1);
        LineTo(FDigit[c].Width-FLineWidth, FDigit[c].Height-(FLineWidth div 2)-1);
      end
      else
        Polygon ([BL, BTL, BTR, BR]);
    end;
  end;
end;

constructor TLEDDisplay.Create(AOwner: TComponent);
var Dummy : TColor;
begin
  inherited Create (AOwner);

  FBevelStyle:= bvLowered;
  FBorderStyle:= bsSingle;
  FColorBackGround:= clBlack;
  FColorLED:= clLime;
  FLEDContrast:=7;
  AssignBevelColors(FColorLED,Dummy,FLEDOffColor,FLEDContrast,FLEDContrast);
  FDecSeperator:= dsComma;
  FDigitHeight:= 30;
  FDigitWidth:= 20;
  FLineWidth:= 3;
  FLeadingZeros:= true;
  FNumDigits:= 6;
  FSegmentStyle:= ssBeveled;
  FValue:= 0;
  Height:= 36;
  Width:= 168;

  GenerateBitMaps;
end;

destructor TLEDDisplay.Destroy;
begin
  inherited destroy;
end;

procedure TLEDDisplay.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLEDDisplay.Paint;
var
  Area        : TRect;
  outText     : string;
  anchoPosi,
  posiLeft,
  PosiTop, c,
  DigitIndex,
  SepPosition : integer;
  ANZeroDigit : Boolean;

  function GetDigitIndex(C:Char):integer;
  begin
    case C of
      '0'..'9' : Result:=StrToIntDeF(C, -1);
      '-'      : Result:=10;
      else       Result:=-1;
    end;
  end; {GetDigitIndex}

begin
  Area := getClientRect;
  try
    outText:=FloatToStrF(FValue, ffFixed, 18, FFractionDigits);
  except
    outText:='';
  end;
  SepPosition:=Pos(DecimalSeparator, outText);
  if SepPosition>0 then
    delete(outText, SepPosition, 1);
  while length(outText)<FNumDigits do begin
    outText:='0'+outText;
    if SepPosition>0 then
      inc(SepPosition);
  end;
  ANZeroDigit:=False; { bis jetzt noch keine Ziffer von 1..9 }
  with canvas do begin
    Brush.Color:=FColorBackGround;
    FillRect(Area);
    anchoPosi:=Self.width div FNumDigits;
    PosiTop:=(Self.height-DigitHeight) div 2;
    posiLeft:=(anchoPosi-FDigitwidth) div 2;
    Brush.Color:=FColorLED;
    Pen.Color:=ColorLED;
    { Bitmaps und DecSeperator zeichnen }
    for c:=1 to FNumDigits do begin
      { nachfolgende Nullen müssen gezeichnet werden! }
      if FLeadingZeros or (strToInt(outText[c])<>0) or ANZeroDigit then begin
        DigitIndex:=GetDigitIndex(outText[c]);
        if (DigitIndex>=0) and (DigitIndex<=10) then
          Draw(posiLeft, posiTop, FDigit[DigitIndex]);
        ANZeroDigit:=True; { spätestens jetzt isse da... }
      end;
      inc(posiLeft, anchoPosi);
      if c=(SepPosition-1) then begin
        Pen.Width:=1;
        if FDecSeperator=dsPoint then
          Ellipse(posiLeft-6, posiTop+FDigitHeight-5, posiLeft-2, posiTop+FDigitHeight-1);
        if FDecSeperator=dsComma then begin
          Ellipse(posiLeft-6, posiTop+FDigitHeight-8, posiLeft-2, posiTop+FDigitHeight-4);
          MoveTo(posiLeft-3, posiTop+FDigitHeight-5);
          LineTo(posiLeft-6, posiTop+FDigitHeight-1);
          LineTo(posiLeft-2, posiTop+FDigitHeight-6);
        end;
        if FDecSeperator=dsDoublePoint then begin
          Ellipse(posiLeft-6, posiTop+(FDigitHeight div 3)-2, posiLeft-2, posiTop+(FDigitHeight div 3)+2);
          Ellipse(posiLeft-6, posiTop+(FDigitHeight*2 div 3)-2, posiLeft-2, posiTop+(FDigitHeight*2 div 3)+2);
        end;
        if FDecSeperator=dsMinus then begin
          Pen.Width:=FLineWidth;
          MoveTo(posiLeft-6, posiTop+((FDigitHeight) div 2));
          LineTo(posiLeft-FLineWidth, posiTop+((FDigitHeight) div 2));
        end;
      end;
    end;
    { Bevel zeichnen }
    if BevelStyle<>bvNone then begin
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnHighlight
      else
        Pen.Color:=clBtnShadow;
      MoveTo(Area.Right-1, Area.Top);
      LineTo(Area.Left, Area.Top);
      LineTo(Area.Left, Area.Bottom-1);
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnShadow
      else
        Pen.Color:=clBtnHighlight;
      MoveTo(Area.Left, Area.Bottom-1);
      LineTo(Area.Right-1, Area.Bottom-1);
      LineTo(Area.Right-1, Area.Top);
      InflateRect(Area, -1, -1);
    end;
    { Border zeichnen }
    if BorderStyle<>bsNone then begin
      Brush.Color:=clWindowFrame;
      FrameRect(Area);
    end;
  end;
end;

procedure TLEDDisplay.SetBevelStyle(newValue: TPanelBevel);
begin
  if FBevelStyle<>newValue then begin
    FBevelStyle:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetBorderStyle(newValue: TBorderStyle);
begin
  if FBorderStyle<>newValue then begin
    FBorderStyle:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorBackGround(newValue: TColor);
begin
  if FColorBackGround<>NewValue then begin
    FColorBackGround:=NewValue;
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetColorLED(newValue: TColor);
var Dummy : TColor;
begin
  if FColorLED<>newValue then begin
    FColorLED:=newValue;
    AssignBevelColors(FColorLED,Dummy,FLEDOffColor,FLEDContrast,FLEDContrast);
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDecSeperator(newValue: TDecSeperator);
begin
  if FDecSeperator<>newValue then begin
    FDecSeperator:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitHeight(newValue: integer);
begin
  if FDigitHeight<>newValue then begin
    FDigitHeight:=newValue;
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetDigitWidth(newValue: integer);
begin
  if FDigitWidth<>newValue then begin
    FDigitWidth:=newValue;
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetFractionDigits(newValue: integer);
begin
  if FFractionDigits<>newValue then begin
    FFractionDigits:=newValue;
    if FFractionDigits>(FNumDigits-1) then
      FFractionDigits:=FNumDigits-1;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLeadingZeros(newValue: boolean);
begin
  if FLeadingZeros<>newValue then begin
    FLeadingZeros:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLEDContrast(newContrast: TContrast);
var Dummy : TColor;
begin
  if (FLEDContrast<>newContrast) and (newContrast>=0) and (newContrast<10) then begin
    FLEDContrast:=newContrast;
    AssignBevelColors(FColorLED,Dummy,FLEDOffColor,FLEDContrast,FLEDContrast);
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetLineWidth(newValue: integer);
begin
  if FLineWidth<>newValue then begin
    FLineWidth:=newValue;
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetNumDigits(newValue: integer);
begin
  if FNumDigits<>newValue then begin
    FNumDigits:=newValue;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetSegmentStyle(newValue: TSegmentStyle);
begin
  if FSegmentStyle<>newValue then begin
    FSegmentStyle:=newValue;
    GenerateBitMaps;
    Invalidate;
  end;
end;

procedure TLEDDisplay.SetValue(newValue: extended);
begin
  if FValue<>NewValue then begin
    FValue:=NewValue;
    GenerateBitMaps;
    Invalidate;
    Change;
  end;
end;


{ Komponente TLEDMeter }
constructor TLEDMeter.Create(AOwner: TComponent);
var Dummy : TColor;
begin
  inherited Create(AOwner);
  FBevelStyle:=bvlowered;
  FColorLED1:=clLime;
  FColorLED2:=clYellow;
  FColorLED3:=clRed;
  AssignBevelColors(FColorLED1,Dummy,FColorOff1,FHLContrast,FShContrast);
  AssignBevelColors(FColorLED2,Dummy,FColorOff2,FHLContrast,FShContrast);
  AssignBevelColors(FColorLED3,Dummy,FColorOff3,FHLContrast,FShContrast);
  FColorSeperator:=clBlack;
  FDirection:=mdRight;
  FMax:=100;
  FMin:=0;
  FLEDContrast:=6;
  FNumDigits:=20;
  FPosition:=0;
  FStartColor2:=75;
  FStartColor3:=90;
  Height:=16;
  Width:=143;
end;

destructor TLEDMeter.Destroy;
begin
  inherited Destroy;
end;

procedure TLEDMeter.SetBevelStyle(newVal: TPanelBevel);
begin
  if newVal<>FBevelStyle then begin
    FBevelStyle:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetColorLED1(newVal: TColor);
var Dummy : TColor;
begin
  if newVal<>FColorLED1 then begin
    FColorLED1:=newVal;
    AssignBevelColors(FColorLED1,Dummy,FColorOff1,FLEDContrast,FLEDContrast);
    Paint;
  end;
end;

procedure TLEDMeter.SetColorLED2(newVal: TColor);
var Dummy : TColor;
begin
  if newVal<>FColorLED2 then begin
    FColorLED2:=newVal;
    AssignBevelColors(FColorLED2,Dummy,FColorOff2,FLEDContrast,FLEDContrast);
    Paint;
  end;
end;

procedure TLEDMeter.SetColorLED3(newVal: TColor);
var Dummy : TColor;
begin
  if newVal<>FColorLED3 then begin
    FColorLED3:=newVal;
    AssignBevelColors(FColorLED3,Dummy,FColorOff3,FLEDContrast,FLEDContrast);
    Paint;
  end;
end;

procedure TLEDMeter.SetColorSeperator(newVal: TColor);
begin
  if newVal<>FColorSeperator then begin
    FColorSeperator:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetDirection(newVal: TMeterDirection);
begin
  if newVal<>FDirection then begin
    FDirection:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetLEDContrast(newContrast: TContrast);
var Dummy : TColor;
begin
  if (FLEDContrast<>newContrast) and (newContrast>=0) and (newContrast<10) then begin
    FLEDContrast:=newContrast;
    AssignBevelColors(FColorLED1,Dummy,FColorOff1,FLEDContrast,FLEDContrast);
    AssignBevelColors(FColorLED2,Dummy,FColorOff2,FLEDContrast,FLEDContrast);
    AssignBevelColors(FColorLED3,Dummy,FColorOff3,FLEDContrast,FLEDContrast);
    Invalidate;
  end;
end;

procedure TLEDMeter.SetMax(newVal: integer);
begin
  if newVal<>FMax then begin
    FMax:=newVal;
    if newVal<FStartColor2 then
      FStartColor2:=newVal;
    if newVal<FStartColor3 then
      FStartColor3:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetMin(newVal: integer);
begin
  if newVal<>FMin then begin
    FMin:=newVal;
    if newVal>FStartColor2 then
      FStartColor2:=newVal;
    if newVal>FStartColor3 then
      FStartColor3:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetNumDigits(newVal: integer);
begin
  if newVal<>FNumDigits then begin
    FNumDigits:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetPosition(newVal: integer);
begin
  if newVal<>FPosition then begin
    if newVal>FMax then
      newVal:=FMax;
    if newVal<FMin then
      newVal:=FMin;
    FPosition:=newVal;
    Paint;
    Change;
  end;
end;

procedure TLEDMeter.SetStartColor2(newVal: integer);
begin
  if newVal<>FStartColor2 then begin
    if newVal>Max then
      newVal:=Max;
    if newVal<Min then
      newVal:=Min;
    if newVal>FStartColor3 then
      newVal:=FStartColor3;
    FStartColor2:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetStartColor3(newVal: integer);
begin
  if newVal<>FStartColor3 then begin
    if newVal>Max then
      newVal:=Max;
    if newVal<Min then
      newVal:=Min;
    if newVal<FStartColor2 then
      newVal:=FStartColor2;
    FStartColor3:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.SetSingleLED(newVal: boolean);
begin
  if newVal<>FSingleLED then begin
    FSingleLED:=newVal;
    Paint;
  end;
end;

procedure TLEDMeter.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TLEDMeter.Paint;
var ARect       : TRect;
    NumPos,DPos,
    FirstYel,
    FirstRed,
    DigitLeft,
    DigitTop,i,
    DigitWidth,
    DigitHeight,
    BevelWidth  : integer;
begin
  with Canvas do begin
    NumPos:=FMax-FMin;
    if BevelStyle<>bvNone then
      BevelWidth:=1
    else
      BevelWidth:=0;
    if (FDirection=mdRight) or (FDirection=mdLeft) then begin
      DigitWidth:=(Width-(2*BevelWidth)) div FNumDigits;
      DigitHeight:=Height-(2*BevelWidth);
      DigitLeft:=BevelWidth;
      DigitTop:=BevelWidth;
    end
    else begin
      DigitWidth:=Width-(2*BevelWidth)-1;
      DigitHeight:=(Height-(2*BevelWidth)) div FNumDigits;
      DigitTop:=BevelWidth;
      DigitLeft:=BevelWidth;
    end;
    if (NumPos>0) and (FPosition>0) then
      DPos:=round(FNumDigits/(NumPos/FPosition))
    else
      DPos:=0;
    if (NumPos>0) and (FStartColor2>0) then
      FirstYel:=round(FNumDigits/(NumPos/FStartColor2))
    else
      FirstYel:=0;
    if (NumPos>0) and (FStartColor3>0) then
      FirstRed:=round(FNumDigits/(NumPos/FStartColor3))
    else
      FirstRed:=0;

    for i:=0 to FNumDigits-1 do begin
      if i>=DPos then begin
        if i<FirstYel then
          Brush.Color:=FColorOff1
        else
          if i<FirstRed then
            Brush.Color:=FColorOff2
          else
            Brush.Color:=FColorOff3;
      end
      else begin
        if FSingleLED then begin
          if (i=DPos-1) then begin
            if i<FirstYel then
              Brush.Color:=FColorLED1
            else
              if i<FirstRed then
                Brush.Color:=FColorLED2
              else
                Brush.Color:=FColorLED3;
          end
          else begin
            if i<FirstYel then
              Brush.Color:=FColorOff1
            else
              if i<FirstRed then
                Brush.Color:=FColorOff2
              else
                Brush.Color:=FColorOff3;
          end;
        end
        else begin
          if i<FirstYel then
            Brush.Color:=FColorLED1
          else
            if i<FirstRed then
              Brush.Color:=FColorLED2
            else
              Brush.Color:=FColorLED3;
        end;
      end;

      if FDirection=mdRight then
        DigitLeft:=BevelWidth+(i*DigitWidth);
      if FDirection=mdLeft then
        DigitLeft:=Width-BevelWidth-((i+1)*DigitWidth)-1;
      if FDirection=mdUp then
        DigitTop:=Height-BevelWidth-((i+1)*DigitHeight);
      if FDirection=mdDown then
        DigitTop:=BevelWidth+(i*DigitHeight);
      Pen.Color:=FColorSeperator;
      Rectangle(DigitLeft,DigitTop,DigitLeft+DigitWidth+1,DigitTop+DigitHeight);
    end;
    if BevelStyle<>bvNone then begin
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnHighlight
      else
        Pen.Color:=clBtnShadow;
      ARect:=GetClientRect;
      MoveTo(ARect.Right-1,ARect.Top);
      LineTo(ARect.Left,ARect.Top);
      LineTo(ARect.Left,ARect.Bottom-1);
      if BevelStyle=bvRaised then
        Pen.Color:=clBtnShadow
      else
        Pen.Color:=clBtnHighlight;
      MoveTo(ARect.Left,ARect.Bottom-1);
      LineTo(ARect.Right-1,ARect.Bottom-1);
      LineTo(ARect.Right-1,ARect.Top);
    end;
  end;
end;


procedure Register;
begin
  RegisterComponents('Simon', [TLEDButton]);
  RegisterComponents('Simon', [TButtonPanel]);
  RegisterComponents('Simon', [TScrewPanel]);
  RegisterComponents('Simon', [TLEDDisplay]);
  RegisterComponents('Simon', [TLEDMeter]);
end;

end.
