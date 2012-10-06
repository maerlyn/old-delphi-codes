{*******************************************************}
{            Button with Color property                 }
{ Version:   1.0                                        }
{ E-Mail:    info@utilmind.com                          }
{ Home Page: www.utilmind.com                           }
{ Created:   September 18, 1999                         }
{ Modified:  September 18, 1999                         }
{ Legal:     Copyright (c) 1999, UtilMind Solutions     }
{*******************************************************}
{ This component is FREEWARE. Enjoy with it but please  }
{ don't ask us about support. However, if you have      }
{ found a way how to improve the TDotFrame - we shall   }
{ be grateful for sent examples.                        }
{*******************************************************}

unit ButtonWithColor;

{$S-,W-,R-}
{$C PRELOAD}

interface

uses Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl;

type
  TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);
  TButtonState = (bsUp, bsDisabled, bsDown, bsExclusive);
  TButtonStyle = (bsAutoDetect, bsWin31, bsNew);
  TNumGlyphs = 1..4;

  TBitBtnWithColorKind = (bkCustom, bkOK, bkCancel, bkHelp, bkYes, bkNo, bkClose,
    bkAbort, bkRetry, bkIgnore, bkAll);

  TBitBtnWithColor = class(TButton)
  private
    FCanvas: TCanvas;
    FColor: TColor;
    FGlyph: Pointer;
    FStyle: TButtonStyle;
    FKind: TBitBtnWithColorKind;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    IsFocused: Boolean;
    FModifiedGlyph: Boolean;

    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
      message WM_LBUTTONDBLCLK;

    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetColor(Value: TColor);
    procedure SetGlyph(Value: TBitmap);
    function GetGlyph: TBitmap;
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);

    procedure GlyphChanged(Sender: TObject);
    function IsCustom: Boolean;
    function IsCustomCaption: Boolean;
    procedure SetStyle(Value: TButtonStyle);
    procedure SetKind(Value: TBitBtnWithColorKind);
    function GetKind: TBitBtnWithColorKind;
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure SetButtonStyle(ADefault: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Cancel stored IsCustom;
    property Caption stored IsCustomCaption;
    property Color: TColor read FColor write SetColor;    
    property Default stored IsCustom;
    property Enabled;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustom;
    property Kind: TBitBtnWithColorKind read GetKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property ModalResult stored IsCustom;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs stored IsCustom default 1;
    property ParentShowHint;
    property ShowHint;
    property Style: TButtonStyle read FStyle write SetStyle default bsAutoDetect;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
  end;

function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: TButtonStyle; IsRounded, IsDown,
  IsFocused: Boolean; FColor: TColor): TRect;

procedure Register;

implementation

uses Consts, SysUtils;

{$R *.RES}

{ TBitBtnWithColor data }
const
  SOKButton = 61508;
  SCancelButton = 61509;
  SYesButton = 61510;
  SNoButton = 61511;
  SHelpButton = 61512;
  SCloseButton = 61513;
  SIgnoreButton = 61514;
  SRetryButton = 61515;
  SAbortButton = 61516;
  SAllButton = 61517;

  BitBtnResNames: array[TBitBtnWithColorKind] of PChar = (
    nil, 'BBCOK', 'BBCCANCEL', 'BBCHELP', 'BBCYES', 'BBCNO', 'BBCCLOSE',
    'BBCABORT', 'BBCRETRY', 'BBCIGNORE', 'BBCALL');
  BitBtnCaptions: array[TBitBtnWithColorKind] of Word = (
    0, SOKButton, SCancelButton, SHelpButton, SYesButton, SNoButton,
    SCloseButton, SAbortButton, SRetryButton, SIgnoreButton,
    SAllButton);
  BitBtnModalResults: array[TBitBtnWithColorKind] of TModalResult = (
    0, mrOk, mrCancel, 0, mrYes, mrNo, 0, mrAbort, mrRetry, mrIgnore,
    mrAll);

var
  BitBtnGlyphs: array[TBitBtnWithColorKind] of TBitmap;

{ DrawButtonFace - returns the remaining usable area inside the Client rect.}
function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
  BevelWidth: Integer; Style: TButtonStyle; IsRounded, IsDown,
  IsFocused: Boolean; FColor: TColor): TRect;
var
  NewStyle: Boolean;
  R: TRect;
  DC: THandle;
begin
  NewStyle := ((Style = bsAutoDetect) and NewStyleControls) or (Style = bsNew);

  R := Client;
  with Canvas do
  begin
    if NewStyle then
    begin
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      DC := Canvas.Handle;    { Reduce calls to GetHandle }

      if IsDown then
      begin    { DrawEdge is faster than Polyline }
        DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT);              { black     }
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);          { btnhilite }
        Dec(R.Bottom);
        Dec(R.Right);
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); { btnshadow }
      end
      else
      begin
        DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT);          { black }
        Dec(R.Bottom);
        Dec(R.Right);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT);              { btnhilite }
        Inc(R.Top);
        Inc(R.Left);
        DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); { btnshadow }
      end;
    end
    else
    begin
      Pen.Color := clWindowFrame;
      Brush.Color := FColor;
      Brush.Style := bsSolid;
      Rectangle(R.Left, R.Top, R.Right, R.Bottom);

      { round the corners - only applies to Win 3.1 style buttons }
      if IsRounded then
      begin
        Pixels[R.Left, R.Top] := FColor;
        Pixels[R.Left, R.Bottom - 1] := FColor;
        Pixels[R.Right - 1, R.Top] := FColor;
        Pixels[R.Right - 1, R.Bottom - 1] := FColor;
      end;

      if IsFocused then
      begin
        InflateRect(R, -1, -1);
        Brush.Style := bsClear;
        Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;

      InflateRect(R, -1, -1);
      if not IsDown then
        Frame3D(Canvas, R, clBtnHighlight, clBtnShadow, BevelWidth)
      else
      begin
        Pen.Color := clBtnShadow;
        PolyLine([Point(R.Left, R.Bottom - 1), Point(R.Left, R.Top),
          Point(R.Right, R.Top)]);
      end;
    end;
  end;

  Result := Rect(Client.Left + 1, Client.Top + 1,
    Client.Right - 2, Client.Bottom - 2);
  if IsDown then OffsetRect(Result, 1, 1);
end;

function GeTBitBtnWithColorGlyph(Kind: TBitBtnWithColorKind): TBitmap;
begin
  if BitBtnGlyphs[Kind] = nil then
  begin
    BitBtnGlyphs[Kind] := TBitmap.Create;
    BitBtnGlyphs[Kind].Handle := LoadBitmap(HInstance, BitBtnResNames[Kind]);
  end;
  Result := BitBtnGlyphs[Kind];
end;

type
  TGlyphList = class(TImageList)
  private
    Used: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function Add(Image, Mask: TBitmap): Integer;
    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    GlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;
  
  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexs: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState; FColor: TColor): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
      State: TButtonState; FColor: TColor);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      var GlyphPos: TPoint; var TextBounds: TRect);
  public
    constructor Create;
    destructor Destroy; override;
    { return the text rectangle }
    function Draw(Canvas: TCanvas; const Client: TRect;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; FColor: TColor): TRect;
    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TGlyphList }

constructor TGlyphList.Create(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  Used := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  Used.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := Used.OpenBit;
  if Result >= Used.Size then
  begin
    Result := inherited Add(nil, nil);
    Used.Size := Result + 1;
  end;
  Used[Result] := True;
end;

function TGlyphList.Add(Image, Mask: TBitmap): Integer;
begin
  Result := AllocateIndex;
  Replace(Result, Image, Mask);
  Inc(FCount);
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if Used[Index] then
  begin
    Dec(FCount);
    Used[Index] := False;
  end;
end;

{ TGlyphCache }

constructor TGlyphCache.Create;
begin
  inherited Create;
  GlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  GlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := GlyphLists.Count - 1 downto 0 do
  begin
    Result := GlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then Exit;
  end;
  Result := TGlyphList.Create(AWidth, AHeight);
  GlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then Exit;
  if List.Count = 0 then
  begin
    GlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := GlyphLists.Count = 0;
end;

var
  GlyphCache: TGlyphCache = nil;

{ TButtonGlyph }

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexs[I] := -1;
  if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexs[I] <> -1 then FGlyphList.Delete(FIndexs[I]);
    FIndexs[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState; FColor: TColor): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then State := bsUp;
  Result := FIndexs[State];
  if Result <> -1 then Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then Exit;
  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := FColor;
    I := State;
    if Ord(I) >= NumGlyphs then I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown:
        begin
          TmpImage.Canvas.BrushCopy(IRect, FOriginal, ORect, FTransparentColor);
          FIndexs[State] := FGlyphList.Add(TmpImage, nil);
        end;
      bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          FIndexs[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor);
        end;
      bsDisabled:
        begin
          MonoBmp := TBitmap.Create;
          try
            if NumGlyphs > 1 then
            with TmpImage.Canvas do
            begin    { Change white & gray to clBtnHighlight and clBtnShadow }
              CopyRect(IRect, FOriginal.Canvas, ORect);
              MonoBmp.Width := IWidth;
              MonoBmp.Height := IHeight;
              MonoBmp.Monochrome := True;

              { Convert white to clBtnHighlight }
              FOriginal.Canvas.Brush.Color := clWhite;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnHighlight;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert gray to clBtnShadow }
              FOriginal.Canvas.Brush.Color := clGray;
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := clBtnShadow;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);

              { Convert transparent color to FColor }
              FOriginal.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
              MonoBmp.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
              Brush.Color := FColor;
              DestDC := Handle;
              SetTextColor(DestDC, clBlack);
              SetBkColor(DestDC, clWhite);
              BitBlt(DestDC, 0, 0, IWidth, IHeight,
                     MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
            end
            else
            begin
              { Create a disabled version }
              with MonoBmp do
              begin
                Assign(FOriginal);
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;
              with TmpImage.Canvas do
              begin
                Brush.Color := FColor;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight,
                  MonoBmp.Canvas.Handle, 0, 0, ROP_DSPDxax);
              end;
            end;
            FIndexs[State] := FGlyphList.Add(TmpImage, nil);
          finally
            MonoBmp.Free;
          end;
       end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexs[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; X, Y: Integer;
  State: TButtonState; FColor: TColor);
var
  Index: Integer;
begin
  if FOriginal = nil then Exit;
  if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;
  Index := CreateButtonGlyph(State, FColor);
  if State = bsExclusive then
    ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
      clNone, clNone, ILD_Transparent)
  else
    ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
      ColorToRGB(FColor), clNone, ILD_Normal);
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clWhite;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clDkGray;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, 0);
    end else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  var GlyphPos: TPoint; var TextBounds: TRect);
var
  TextPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
begin
  { calculate the item sizes }
  ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
    Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height) else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds, DT_CALCRECT);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  { if there is no text or no bitmap, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  { adjust Margin and Spacing }
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  { fixup the result variables }
  Inc(GlyphPos.X, Client.Left);
  Inc(GlyphPos.Y, Client.Top);
  OffsetRect(TextBounds, TextPos.X + Client.Left, TextPos.Y + Client.Top);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
  State: TButtonState; FColor: TColor): TRect;
var
  GlyphPos: TPoint;
begin
  CalcButtonLayout(Canvas, Client, Caption, Layout, Margin, Spacing,
    GlyphPos, Result);
  DrawButtonGlyph(Canvas, GlyphPos.X, GlyphPos.Y, State, FColor);
  DrawButtonText(Canvas, Caption, Result, State);
end;

{ TBitBtnWithColor }

constructor TBitBtnWithColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  FCanvas := TCanvas.Create;
  FColor := clBtnFace;  
  FStyle := bsAutoDetect;
  FKind := bkCustom;
  FLayout := blGlyphLeft;
  FSpacing := 4;
  FMargin := -1;
end;

destructor TBitBtnWithColor.Destroy;
begin
  TButtonGlyph(FGlyph).Free;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TBitBtnWithColor.CreateHandle;
var
  State: TButtonState;
begin
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  inherited CreateHandle;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State, FColor);
end;

procedure TBitBtnWithColor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or BS_OWNERDRAW;
end;

procedure TBitBtnWithColor.SetButtonStyle(ADefault: Boolean);
begin
  if ADefault <> IsFocused then
  begin
    IsFocused := ADefault;
    Refresh;
  end;
end;

procedure TBitBtnWithColor.Click;
var
  Form: TForm;
  Control: TWinControl;
begin
  case FKind of
    bkClose:
      begin
        Form := TForm(GetParentForm(Self));
        if Form <> nil then Form.Close
        else inherited Click;
      end;
    bkHelp:
      begin
        Control := Self;
        while (Control <> nil) and (Control.HelpContext = 0) do
          Control := Control.Parent;
        if Control <> nil then Application.HelpContext(Control.HelpContext)
        else inherited Click;
      end;
    else
      inherited Click;
  end;
end;

procedure TBitBtnWithColor.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do
  begin
    itemWidth := Width;
    itemHeight := Height;
  end;
end;

procedure TBitBtnWithColor.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
end;

procedure TBitBtnWithColor.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  State: TButtonState;
  R: TRect;
begin
  FCanvas.Handle := DrawItemStruct.hDC;
  R := ClientRect;

  with DrawItemStruct do
  begin
    IsDown := itemState and ODS_SELECTED <> 0;
    IsDefault := itemState and ODS_FOCUS <> 0;

    if not Enabled then State := bsDisabled
    else if IsDown then State := bsDown
    else State := bsUp;
  end;

  { DrawFrameControl doesn't allow for drawing a button as the
      default button, so it must be done here. }
  if IsFocused or IsDefault then
  begin
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);

    { DrawFrameControl must draw within this border }
    InflateRect(R, -1, -1);
  end;

  { DrawFrameControl does not draw a pressed button correctly }
  if IsDown then
  begin
    FCanvas.Pen.Color := clBtnShadow;
    FCanvas.Pen.Width := 1;
    FCanvas.Brush.Color := FColor;
    FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    InflateRect(R, -1, -1);
  end
  else
   with FCanvas do
   begin
     DrawEdge(Handle, R, BDR_RAISEDINNER or BDR_RAISEDOUTER, BF_RECT or BF_ADJUST);
     FCanvas.Pen.Color := FColor;
     FCanvas.Pen.Width := 1;
     FCanvas.Brush.Color := FColor;
     FCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
     InflateRect(R, -1, -1);
   end;

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -1, -1);
  end;

  FCanvas.Font := Self.Font;
  if IsDown then
    OffsetRect(R, 1, 1);
  TButtonGlyph(FGlyph).Draw(FCanvas, R, Caption, FLayout,
    FMargin, FSpacing, State, FColor);

  if IsFocused then
  begin
    R := ClientRect;
    InflateRect(R, -4, -4);
    FCanvas.Pen.Color := clWindowFrame;
    FCanvas.Brush.Color := FColor;
    DrawFocusRect(FCanvas.Handle, R);
  end;

  FCanvas.Handle := 0;
end;

procedure TBitBtnWithColor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TBitBtnWithColor.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TBitBtnWithColor.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  Perform(WM_LBUTTONDOWN, Message.Keys, Longint(Message.Pos));
end;

function TBitBtnWithColor.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

procedure TBitBtnWithColor.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    SetGlyph(Glyph);
    Invalidate;
  end;
end;

procedure TBitBtnWithColor.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value as TBitmap;
  FModifiedGlyph := True;
  Invalidate;
end;

function TBitBtnWithColor.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

procedure TBitBtnWithColor.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

function TBitBtnWithColor.IsCustom: Boolean;
begin
  Result := Kind = bkCustom;
end;

procedure TBitBtnWithColor.SetStyle(Value: TButtonStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TBitBtnWithColor.SetKind(Value: TBitBtnWithColorKind);
begin
  if Value <> FKind then
  begin
    if Value <> bkCustom then
    begin
      Default := Value in [bkOK, bkYes];
      Cancel := Value in [bkCancel, bkNo];

      if ((csLoading in ComponentState) and (Caption = '')) or
        (not (csLoading in ComponentState)) then
      begin
        if BitBtnCaptions[Value] > 0 then
          Caption := LoadStr(BitBtnCaptions[Value]);
      end;

      ModalResult := BitBtnModalResults[Value];
      TButtonGlyph(FGlyph).Glyph := GeTBitBtnWithColorGlyph(Value);
      NumGlyphs := 2;
      FModifiedGlyph := False;
    end;
    FKind := Value;
    Invalidate;
  end;
end;

function TBitBtnWithColor.IsCustomCaption: Boolean;
begin
  Result := CompareStr(Caption, LoadStr(BitBtnCaptions[FKind])) <> 0;
end;

function TBitBtnWithColor.GetKind: TBitBtnWithColorKind;
begin
  if FKind <> bkCustom then
    if ((FKind in [bkOK, bkYes]) xor Default) or
      ((FKind in [bkCancel, bkNo]) xor Cancel) or
      (ModalResult <> BitBtnModalResults[FKind]) or
      FModifiedGlyph then
      FKind := bkCustom;
  Result := FKind;
end;

procedure TBitBtnWithColor.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

function TBitBtnWithColor.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

procedure TBitBtnWithColor.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then Value := 1
  else if Value > 4 then Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TBitBtnWithColor.SetSpacing(Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TBitBtnWithColor.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= - 1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure DestroyLocals; far;
var
  I: TBitBtnWithColorKind;
begin
  for I := Low(TBitBtnWithColorKind) to High(TBitBtnWithColorKind) do
    BitBtnGlyphs[I].Free;
end;

procedure Register;
begin
  RegisterComponents('UtilMind', [TBitBtnWithColor]);
end;

initialization
  FillChar(BitBtnGlyphs, SizeOf(BitBtnGlyphs), 0);
finalization
  DestroyLocals;
end.
