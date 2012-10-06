unit G32_Transforms;

{*********************************************}
{  This unit is a part of Graphics32 library  }
{  Copyright © 2000-2001 Alex Denisov         }
{  See License.txt for licence information    }
{*********************************************}
// $Id: G32_Transforms.pas,v 1.3 2001/09/15 01:37:22 alex Exp $

interface

{$I G32.INC}

uses
  Windows, SysUtils, Classes, G32, G32_Blend;

type
  ETransformError = class(Exception);

procedure BlockTransfer(
  Dst: TBitmap32;
  DstX: Integer;
  DstY: Integer;
  Src: TBitmap32;
  SrcRect: TRect;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent = nil);

procedure StretchTransfer(
  Dst: TBitmap32;
  DstRect: TRect;
  Src: TBitmap32;
  SrcRect: TRect;
  StretchFilter: TStretchFilter;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent = nil);

type
  TFloatMatrix = array[0..2, 0..2] of Single;  // 3x3 single precision

const
  IdentityMatrix: TFloatMatrix = (
    (1, 0, 0),
    (0, 1, 0),
    (0, 0, 1));

type
  TTransformation = class(TObject)
  private
    FSrcRect: TFloatRect;
    procedure SetSrcRect(const Value: TFloatRect);
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); virtual; abstract;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); virtual; abstract;
  protected
    TransformValid: Boolean;
    procedure PrepareTransform; virtual; abstract;
  public
    function  GetTransformedBounds: TRect; virtual; abstract;
    property SrcRect: TFloatRect read FSrcRect write SetSrcRect;
  end;

  TAffineTransformation = class(TTransformation)
  protected
    A, B, C: Integer;
    D, E, F: Integer;
    procedure PrepareTransform; override;
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
  public
    Matrix: TFloatMatrix;
    constructor Create; virtual;
    function  GetTransformedBounds: TRect; override;
    procedure Clear;
    procedure Rotate(Cx, Cy, Alpha: Single); // degrees
    procedure Skew(Fx, Fy: Single);
    procedure Scale(Sx, Sy: Single);
    procedure Translate(Dx, Dy: Single);
  end;

  TProjectiveTransformation = class(TTransformation)
  private
    Wx0, Wx1, Wx2, Wx3: Single;
    Wy0, Wy1, Wy2, Wy3: Single;
    procedure SetX0(Value: Single);
    procedure SetX1(Value: Single);
    procedure SetX2(Value: Single);
    procedure SetX3(Value: Single);
    procedure SetY0(Value: Single);
    procedure SetY1(Value: Single);
    procedure SetY2(Value: Single);
    procedure SetY3(Value: Single);
  protected
    M: TFloatMatrix;
    procedure PrepareTransform; override;
    procedure Transform(DstX, DstY: Integer; out SrcX, SrcY: Integer); override;
    procedure Transform256(DstX, DstY: Integer; out SrcX256, SrcY256: Integer); override;
  public
    function  GetTransformedBounds: TRect; override;
    property X0: Single read Wx0 write SetX0;
    property X1: Single read Wx1 write SetX1;
    property X2: Single read Wx2 write SetX2;
    property X3: Single read Wx3 write SetX3;
    property Y0: Single read Wy0 write SetY0;
    property Y1: Single read Wy1 write SetY1;
    property Y2: Single read Wy2 write SetY2;
    property Y3: Single read Wy3 write SetY3;
  end;

procedure Transform(Dst, Src: TBitmap32; Transformation: TTransformation);
procedure SetBorderTransparent(ABitmap: TBitmap32; ARect: TRect);

implementation

uses G32_LowLevel, Math;

const
  SDstEmpty = 'Destination bitmap is nil or empty';
  SSrcEmpty = 'Source bitmap is nil or empty';
  SSrcInvalid = 'Source rectangle is invalid';

procedure CheckBitmaps(Dst, Src: TBitmap32);
begin
  if not Assigned(Dst) or Dst.Empty then raise ETransformError.Create(SDstEmpty);
  if not Assigned(Src) or Src.Empty then raise ETransformError.Create(SSrcEmpty);
end;

function CheckSrcRect(Src: TBitmap32; const SrcRect: TRect): Boolean;
begin
  Result := False;
  if IsRectEmpty(SrcRect) then Exit;
  if (SrcRect.Left < 0) or (SrcRect.Right > Src.Width) or
    (SrcRect.Top < 0) or (SrcRect.Bottom > Src.Height) then
    raise ETransformError.Create(SSrcInvalid);
  Result := True;
end;

procedure BlockTransfer(
  Dst: TBitmap32;
  DstX: Integer;
  DstY: Integer;
  Src: TBitmap32;
  SrcRect: TRect;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
var
  SrcX, SrcY: Integer;
  S, D: TRect;
  I, J, N: Integer;
  Ps, Pd: PColor32;
  MstrAlpha: TColor32;
begin
  CheckBitmaps(Src, Dst);

  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  if CombineOp = dmOpaque then
  begin
    BitBlt(Dst.Handle, DstX, DstY, SrcRect.Right - SrcRect.Left,
      SrcRect.Bottom - SrcRect.Top, Src.Handle, SrcRect.Left, SrcRect.Top,
      SRCCOPY);
    Exit;
  end;

  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  // clip the rectangles with bitmap boundaries
  SrcX := SrcRect.Left;
  SrcY := SrcRect.Top;
  IntersectRect(S, SrcRect, Rect(0, 0, Src.Width, Src.Height));
  OffsetRect(S, DstX - SrcX, DstY - SrcY);
  IntersectRect(D, S, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(D) then Exit;

  MstrAlpha := Src.MasterAlpha;
  N := D.Right - D.Left;

  try
    if CombineOp = dmBlend then
    begin
      if MstrAlpha >= 255 then
        for J := D.Top to D.Bottom - 1 do
        begin
          Ps := Src.PixelPtr[D.Left + SrcX - DstX, J + SrcY - DstY];
          Pd := Dst.PixelPtr[D.Left, J];
          BlendLine(Ps, Pd, N);
        end
      else
        for J := D.Top to D.Bottom - 1 do
        begin
          Ps := Src.PixelPtr[D.Left + SrcX - DstX, J + SrcY - DstY];
          Pd := Dst.PixelPtr[D.Left, J];
          BlendLineEx(Ps, Pd, N, MstrAlpha);
        end;
    end
    else // CombineOp = dmCustom
      for J := D.Top to D.Bottom - 1 do
      begin
        Ps := Src.PixelPtr[D.Left + SrcX - DstX, J + SrcY - DstY];
        Pd := Dst.PixelPtr[D.Left, J];
        for I := 0 to N - 1 do
        begin
          CombineCallBack(Ps^, Pd^, MstrAlpha);
          Inc(Ps); Inc(Pd);
        end;
      end;
  finally
    EMMS;
  end;
end;

procedure StretchNearestOpaque(
  Dst: TBitmap32;
  DstRect: TRect;
  Src: TBitmap32;
  SrcRect: TRect);
var
  SrcW, SrcH, DstW, DstH: Integer;
  R: TRect;
  SFX, SFY: TFixed;
  SrcP, DstP: PColor32;
  DstX, DstY, SrcX: Integer;
  SrcXXXX, SrcYYYY: TFixed;
  DeltaXXXX, DeltaYYYY: TFixed;
begin
  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;
  if IsRectEmpty(DstRect) then Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then Exit;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    BlockTransfer(Dst, DstRect.Left, DstRect.Top, Src, SrcRect, dmOpaque, nil);
    Exit;
  end;

  SFX := MulDiv((R.Left - DstRect.Left) * (SrcW - 1), 65535, (DstW - 1));
  SFY := MulDiv((R.Top - DstRect.Top) * (SrcH - 1), 65535, (DstH - 1));

  DeltaXXXX := (SrcW shl 16) div (DstW);
  DeltaYYYY := (SrcH shl 16) div (DstH);
  SrcYYYY := SFY;
  for DstY := R.Top to R.Bottom - 1 do
  begin
    SrcP := Src.PixelPtr[SrcRect.Left, SrcYYYY shr 16 + SrcRect.Top]; // source scanline
    DstP := Dst.PixelPtr[R.Left, DstY]; // destination scanline
    SrcXXXX := SFX;
    for DstX := R.Left to R.Right - 1 do
    begin
      SrcX := SrcXXXX shr 14 and $FFFFFFFC;
      Inc(SrcXXXX, DeltaXXXX);
      DstP^ := PColor32(Integer(SrcP) + SrcX)^;
      Inc(DstP);
    end;
    Inc(SrcYYYY, DeltaYYYY);
  end;
end;  

procedure StretchNearestBlend(
  Dst: TBitmap32;
  DstRect: TRect;
  Src: TBitmap32;
  SrcRect: TRect;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH, DstW, DstH: Integer;
  R: TRect;
  SFX, SFY: TFixed;
  SrcP, DstP: PColor32;
  DstX, DstY, SrcX: Integer;
  SrcXXXX, SrcYYYY: TFixed;
  DeltaXXXX, DeltaYYYY: TFixed;
  MasterAlpha: TColor32;
begin
  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;
  if IsRectEmpty(DstRect) then Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then Exit;
  if Src.MasterAlpha = 0 then Exit;
  MasterAlpha := Src.MasterAlpha;
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then CombineOp := dmBlend;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    BlockTransfer(Dst, DstRect.Left, DstRect.Top, Src, SrcRect, CombineOp, CombineCallBack);
    Exit;
  end;

  SFX := MulDiv((R.Left - DstRect.Left) * SrcW, 65535, DstW);
  SFY := MulDiv((R.Top - DstRect.Top) * SrcH, 65535, DstH);

  try
    DeltaXXXX := (SrcW shl 16) div DstW;
    DeltaYYYY := (SrcH shl 16) div DstH;
    SrcYYYY := SFY;
    for DstY := R.Top to R.Bottom - 1 do
    begin
      SrcP := Src.PixelPtr[SrcRect.Left, SrcYYYY shr 16 + SrcRect.Top]; // source scanline
      DstP := Dst.PixelPtr[R.Left, DstY]; // destination scanline
      SrcXXXX := SFX;
      if CombineOp = dmCustom then
        for DstX := R.Left to R.Right - 1 do
        begin
          SrcX := SrcXXXX shr 14 and $FFFFFFFC;
          Inc(SrcXXXX, DeltaXXXX);
          CombineCallBack(PColor32(Integer(SrcP) + SrcX)^, DstP^, MasterAlpha);
          Inc(DstP);
        end
      else if MasterAlpha >= 255 then
        for DstX := R.Left to R.Right - 1 do
        begin
          SrcX := SrcXXXX shr 14 and $FFFFFFFC;
          Inc(SrcXXXX, DeltaXXXX);
          BlendMem(PColor32(Integer(SrcP) + SrcX)^, DstP^);
          Inc(DstP);
        end
      else
        for DstX := R.Left to R.Right - 1 do
        begin
          SrcX := SrcXXXX shr 14 and $FFFFFFFC;
          Inc(SrcXXXX, DeltaXXXX);
          BlendMemEx(PColor32(Integer(SrcP) + SrcX)^, DstP^, MasterAlpha);
          Inc(DstP);
        end;
      Inc(SrcYYYY, DeltaYYYY);
    end;
  finally
    EMMS;
  end;
end;

procedure StretchLinear(
  Dst: TBitmap32;
  DstRect: TRect;
  Src: TBitmap32;
  SrcRect: TRect;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH, DstW, DstH: Integer;
  R: TRect;
  SrcP1, SrcP2, DstP: PColor32;
  DstX, DstY, SrcX, SrcY: Integer;
  SFX, SFY: TFixed;
  SrcXXXX, SrcYYYY: TFixed;
  DeltaXXXX, DeltaYYYY: TFixed;
  C, MasterAlpha: TColor32;
  WY, WX: Cardinal;
  Cr, Cg, Cb, Ca: Integer;
begin
  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;
  if IsRectEmpty(DstRect) then Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then Exit;
  if Src.MasterAlpha = 0 then Exit;
  MasterAlpha := Src.MasterAlpha;
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then CombineOp := dmBlend;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) then
  begin
    BlockTransfer(Dst, DstRect.Left, DstRect.Top, Src, SrcRect, CombineOp, CombineCallBack);
    Exit;
  end;

  SFX := MulDiv((R.Left - DstRect.Left) * (SrcW - 1), 65535, DstW);
  SFY := MulDiv((R.Top - DstRect.Top) * (SrcH - 1), 65535, DstH);

  try
    DeltaXXXX := ((SrcW - 1) shl 16) div DstW;
    DeltaYYYY := ((SrcH - 1) shl 16) div DstH;
    SrcYYYY := SFY;
    for DstY := R.Top to R.Bottom - 1 do
    begin
      SrcY := SrcYYYY shr 16 + SrcRect.Top;
      SrcP1 := Src.PixelPtr[SrcRect.Left, SrcY]; // source scanline
      if SrcY < SrcRect.Bottom - 1 then SrcP2 := Src.PixelPtr[SrcRect.Left, SrcY + 1]
      else SrcP2 := SrcP1;
      DstP := Dst.PixelPtr[R.Left, DstY]; // destination scanline
      WY := SrcYYYY shr 8 and $FF xor $FF;
      SrcXXXX := SFX;
      case CombineOp of
        dmOpaque:
          for DstX := R.Left to R.Right - 1 do
          begin
            SrcX := SrcXXXX shr 14 and $FFFFFFFC;
            WX := SrcXXXX shr 8 and $FF xor $FF;
            Inc(SrcXXXX, DeltaXXXX);
            DstP^ := CombineReg(
              CombineReg(PColor32(Integer(SrcP1) + SrcX)^, PColor32(Integer(SrcP2) + SrcX)^, WY),
              CombineReg(PColor32(Integer(SrcP1) + SrcX + 4)^, PColor32(Integer(SrcP2) + SrcX + 4)^, WY),
              WX);
            Inc(DstP);
          end;

        dmBlend:
          if MasterAlpha >= 255 then
            for DstX := R.Left to R.Right - 1 do
            begin
              SrcX := SrcXXXX shr 14 and $FFFFFFFC;
              WX := SrcXXXX shr 8 and $FF xor $FF;
              Inc(SrcXXXX, DeltaXXXX);
              C := CombineReg(
                CombineReg(PColor32(Integer(SrcP1) + SrcX)^, PColor32(Integer(SrcP2) + SrcX)^, WY),
                CombineReg(PColor32(Integer(SrcP1) + SrcX + 4)^, PColor32(Integer(SrcP2) + SrcX + 4)^, WY),
                WX);
              BlendMem(C, DstP^);
              Inc(DstP);
            end
          else // MasterAlpha <= 255
            for DstX := R.Left to R.Right - 1 do
            begin
              SrcX := SrcXXXX shr 14 and $FFFFFFFC;
              WX := SrcXXXX shr 8 and $FF xor $FF;
              Inc(SrcXXXX, DeltaXXXX);
              //C := PColor32(Integer(SrcP1) + SrcX + 4)^;
              C := CombineReg(
                CombineReg(PColor32(Integer(SrcP1) + SrcX)^, PColor32(Integer(SrcP1) + SrcX + 4)^, WX),
                CombineReg(PColor32(Integer(SrcP2) + SrcX)^, PColor32(Integer(SrcP2) + SrcX + 4)^, WX),
                WY);
              BlendMemEx(C, DstP^, MasterAlpha);
              Inc(DstP);
            end;

        dmCustom:
          for DstX := R.Left to R.Right - 2 do
          begin
            SrcX := SrcXXXX shr 14 and $FFFFFFFC;
            WX := SrcXXXX shr 8 and $FF xor $FF;
            Inc(SrcXXXX, DeltaXXXX);
            C := CombineReg(
              CombineReg(PColor32(Integer(SrcP1) + SrcX)^, PColor32(Integer(SrcP2) + SrcX)^, WY),
              CombineReg(PColor32(Integer(SrcP1) + SrcX + 4)^, PColor32(Integer(SrcP2) + SrcX + 4)^, WY),
              WX);
            CombineCallBack(C, DstP^, MasterAlpha);
            Inc(DstP);
          end;
      end;
      Inc(SrcYYYY, DeltaYYYY);
    end;
  finally
    EMMS;
  end;
end;



{ StretchFlt }

type
  TPointRec = record
    Pos: Integer;
    Weight: Integer;
  end;
  TCluster = array of TPointRec;
  TMappingTable = array of TCluster;
  TFilterFunc = function(Value: Single): Single;

function NearestFilter(Value: Single): Single;
begin
  if (Value > -0.5) and (Value <= 0.5) then Result := 1
  else Result := 0;
end;

function LinearFilter(Value: Single): Single;
begin
  if Value < -1 then Result := 0
  else if Value < 0 then Result := 1 + Value
  else if Value < 1 then Result := 1 - Value
  else Result := 0;
end;

function SplineFilter(Value: Single): Single;
var
  tt: Single;
begin
  Value := Abs(Value);
  if Value < 1 then
  begin
    tt := Sqr(Value);
    Result := 0.5 * tt * Value - tt + 2 / 3;
  end
  else if Value < 2 then
  begin
    Value := 2 - Value;
    Result := 1 / 6 * Sqr(Value) * Value;
  end
  else Result := 0;
end;

function BuildMappingTable(
  DstWidth: Integer;
  SrcWidth: Integer;
  SrcLo, SrcHi: Integer;
  StretchFilter: TStretchFilter): TMappingTable;
const
  { the two first filters from this array are never used since nearest and
    simple linear filters are implemented in separate routines }
  FILTERS: array[TStretchFilter] of TFilterFunc = (NearestFilter, LinearFilter,
    LinearFilter, SplineFilter);
var
  Filter: TFilterFunc;
  FilterWidth: Single;
  Scale, OldScale: Single;
  Center: Single;
  Bias: Single;
  Left, Right: Integer;
  I, J, K: Integer;
  Weight: Integer;
begin
  // in current version the table will be build only for spline interpolation

  if SrcWidth = 0 then
  begin
    Result := nil;
    Exit;
  end
  else if SrcWidth = 1 then
  begin
    SetLength(Result, DstWidth);
    for I := 0 to DstWidth - 1 do
    begin
      SetLength(Result[I], 1);
      Result[I][0].Pos := 0;
      Result[I][0].Weight := 255;
    end;
    Exit;
  end;
  Filter := FILTERS[StretchFilter];
  if StretchFilter in [sfNearest, sfLinear, sfLinear2] then FilterWidth := 1.001
  else FilterWidth := 1.5;
  SetLength(Result, DstWidth);
  if SrcHi <= SrcLo - 1 then Exit;
  if DstWidth <= 1 then Exit;
  if SrcHi - SrcLo = 1 then Scale := 1
  else Scale := (DstWidth - 1) / (SrcHi - SrcLo - 1);

  if (Scale < 1) then
  begin
    OldScale := Scale;
    Scale := 1 / Scale;
    FilterWidth := FilterWidth * Scale;
    for I := 0 to DstWidth - 1 do
    begin
      Center := SrcLo + I * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      Bias := 0;
      for J := Left to Right do
      begin
        Weight := Round(255 * Filter((Center - J) * OldScale) * OldScale);
        if Weight <> 0 then
        begin
          Bias := Bias + Weight / 255;
          k := Length(Result[I]);
          SetLength(Result[I], k + 1);
          Result[I][k].Pos := Constrain(J, 0, SrcWidth - 1);
          Result[I][k].Weight := Weight;
        end;
      end;
      Assert(Bias > 0);
      if (Bias > 0) and (Bias <> 1) then
      begin
        Bias := 1 / Bias;
        for k := 0 to High(Result[I]) do
          Result[I][k].Weight := Round(Result[I][k].Weight * Bias);
      end
    end;
  end
  else // scale > 1
  begin
    FilterWidth := 1 / FilterWidth;
    Scale := 1 / Scale;
    for I := 0 to DstWidth - 1 do
    begin
      Center := SrcLo + I * Scale;
      Left := Floor(Center - FilterWidth);
      Right := Ceil(Center + FilterWidth);
      for j := Left to Right do
      begin
        Weight := Round(256 * Filter(Center - j));
        if Weight <> 0 then
        begin
          k := Length(Result[I]);
          SetLength(Result[I], k + 1);
          Result[i][k].Pos := Constrain(j, 0, SrcWidth - 1);
          Result[i][k].Weight := Weight;
        end;
      end;
    end;
  end;
end;

procedure StretchTransfer(
  Dst: TBitmap32;
  DstRect: TRect;
  Src: TBitmap32;
  SrcRect: TRect;
  StretchFilter: TStretchFilter;
  CombineOp: TDrawMode;
  CombineCallBack: TPixelCombineEvent);
var
  SrcW, SrcH: Single;
  DstW, DstH: Integer;
  t: Single;
  MapX, MapY: TMappingTable;
  DstX, DstY: Integer;
  R: TRect;
  I, J, X, Y: Integer;
  P: PColor32;
  ClusterX, ClusterY: TCluster;
  C, Wt, Cr, Cg, Cb, Ca: Integer;
  ClustYP, ClustYW: Integer;
  MstrAlpha: TColor32;
begin
  if (CombineOp = dmCustom) and not Assigned(CombineCallBack) then
    CombineOp := dmOpaque;

  if StretchFilter = sfNearest then
  begin
    if CombineOp = dmOpaque then
      StretchNearestOpaque(Dst, DstRect, Src, SrcRect)
    else
      StretchNearestBlend(Dst, DstRect, Src, SrcRect, CombineOp, CombineCallBack);
    Exit;
  end;

  if StretchFilter = sfLinear then
  begin
    StretchLinear(Dst, DstRect, Src, SrcRect, CombineOp, CombineCallBack);
    Exit;
  end;

  // check source and destination
  CheckBitmaps(Dst, Src);
  if not CheckSrcRect(Src, SrcRect) then Exit;
  if IsRectEmpty(DstRect) then Exit;
  IntersectRect(R, DstRect, Rect(0, 0, Dst.Width, Dst.Height));
  if IsRectEmpty(R) then Exit;
  if (CombineOp = dmBlend) and (Src.MasterAlpha = 0) then Exit;

  SrcW := SrcRect.Right - SrcRect.Left;
  SrcH := SrcRect.Bottom - SrcRect.Top;
  DstW := DstRect.Right - DstRect.Left;
  DstH := DstRect.Bottom - DstRect.Top;
  DstX := DstRect.Left;
  DstY := DstRect.Top;
  MstrAlpha := Src.MasterAlpha;

  // check if we actually have to stretch anything
  if (SrcW = DstW) and (SrcH = DstH) and (SrcRect.Left = Round(SrcRect.Left)) and
    (SrcRect.Top = Round(SrcRect.Top)) then
  begin
    BlockTransfer(Dst, DstX, DstY, Src, SrcRect, CombineOp);
    Exit;
  end;

  // mapping tables
  MapX := BuildMappingTable(DstW, Src.Width, SrcRect.Left, SrcRect.Right, StretchFilter);
  MapY := BuildMappingTable(DstH, Src.Height, SrcRect.Top, SrcRect.Bottom, StretchFilter);
  ClusterX := nil;
  ClusterY := nil;
  try
    if (MapX = nil) or (MapY = nil) then Exit;

    // transfer pixels
    for J := R.Top to R.Bottom - 1 do
    begin
      ClusterY := MapY[J - DstY];
      P := Dst.PixelPtr[R.Left, J];
      for I := R.Left to R.Right - 1 do
      begin
        ClusterX := MapX[I - DstX];

        // reset color accumulators
        Ca := 0; Cr := 0; Cg := 0; Cb := 0;

        // now iterate through each cluster
        for y := 0 to High(ClusterY) do
        begin
          with ClusterY[Y] do
          begin
            ClustYP := Pos;
            ClustYW := Weight;
          end;
          for x := 0 to High(ClusterX) do
          begin
            C := Src[ClusterX[x].Pos, ClustYP];
            Wt := ClusterX[x].Weight * ClustYW;
            Inc(Ca, C shr 24 * Wt);
            Inc(Cr, (C and $00FF0000) shr 16 * Wt);
            Inc(Cg, (C and $0000FF00) shr 8 * Wt);
            Inc(Cb, (C and $000000FF) * Wt);
          end;
        end;
        if Ca > $FFFFFF then Ca := $FF0000 else Ca := Ca and $00FF0000;
        if Cr > $FF0000 then Cr := $FF0000 else Cr := Cr and $00FF0000;
        if Cg > $FF0000 then Cg := $FF0000 else Cg := Cg and $00FF0000;
        if Cb > $FF0000 then Cb := $FF0000 else Cb := Cb and $00FF0000;
        C := (Ca shl 8) or Cr or (Cg shr 8) or (Cb shr 16);

        // combine it with the background
        case CombineOp of
          dmOpaque: P^ := C;
          dmBlend: BlendMemEx(C, P^, MstrAlpha);
          dmCustom: CombineCallBack(C, P^, MstrAlpha);
        end;
        Inc(P);
      end;
    end;
  finally
    EMMS;
    MapX := nil;
    MapY := nil;
  end;
end;


{ A bit of linear algebra }

function _DET(a1, a2, b1, b2: Single): Single; overload;
begin
  Result := a1 * b2 - a2 * b1;
end;

function _DET(a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; overload;
begin
  Result :=
    a1 * (b2 * c3 - b3 * c2) -
    b1 * (a2 * c3 - a3 * c2) +
    c1 * (a2 * b3 - a3 * b2);
end;

procedure Adjoint(var M: TFloatMatrix);
var
  a1, a2, a3: Single;
  b1, b2, b3: Single;
  c1, c2, c3: Single;
begin
  a1 := M[0,0]; a2:= M[0,1]; a3 := M[0,2];
  b1 := M[1,0]; b2:= M[1,1]; b3 := M[1,2];
  c1 := M[2,0]; c2:= M[2,1]; c3 := M[2,2];

  M[0,0]:= _DET(b2, b3, c2, c3);
  M[0,1]:=-_DET(a2, a3, c2, c3);
  M[0,2]:= _DET(a2, a3, b2, b3);

  M[1,0]:=-_DET(b1, b3, c1, c3);
  M[1,1]:= _DET(a1, a3, c1, c3);
  M[1,2]:=-_DET(a1, a3, b1, b3);

  M[2,0]:= _DET(b1, b2, c1, c2);
  M[2,1]:=-_DET(a1, a2, c1, c2);
  M[2,2]:= _DET(a1, a2, b1, b2);
end;

function Determinant(const M: TFloatMatrix): Single;
begin
  Result := _DET(M[0,0], M[1,0], M[2,0],
                 M[0,1], M[1,1], M[2,1],
                 M[0,2], M[1,2], M[2,2]);
end;

procedure Scale(var M: TFloatMatrix; Factor: Single);
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      M[i,j] := M[i,j] * Factor;
end;

procedure Invert(var M: TFloatMatrix);
var
  Det: Single;
begin
  Det := Determinant(M);
  if Abs(Det) < 1E-5 then M := IdentityMatrix
  else
  begin
    Adjoint(M);
    Scale(M, 1 / Det);
  end;
end;

function Mult(const M1, M2: TFloatMatrix): TFloatMatrix;
var
  i, j: Integer;
begin
  for i := 0 to 2 do
    for j := 0 to 2 do
      Result[i, j] :=
        M1[0, j] * M2[i, 0] +
        M1[1, j] * M2[i, 1] +
        M1[2, j] * M2[i, 2];
end;

type
  TVector3f = array[0..2] of Single;
  TVector3i = array[0..2] of Integer;

function VectorTransform(const M: TFloatMatrix; const V: TVector3f): TVector3f;
begin
  Result[0] := M[0,0] * V[0] + M[1,0] * V[1] + M[2,0] * V[2];
  Result[1] := M[0,1] * V[0] + M[1,1] * V[1] + M[2,1] * V[2];
  Result[2] := M[0,2] * V[0] + M[1,2] * V[1] + M[2,2] * V[2];
end;


{ Transformation functions }

procedure Transform(Dst, Src: TBitmap32; Transformation: TTransformation);
var
  C, SrcAlpha: TColor32;
  R, SrcRectI, DstRect, SrcRect256: TRect;
  Pixels: PColor32Array;
  I, J, X, Y: Integer;
  DrawMode: TDrawMode;
  CombineCallBack: TPixelCombineEvent;

  function GET_S256(X256, Y256: Integer; out C: TColor32): Boolean;
  var
    flrx, flry, celx, cely: Longword;
    C1, C2, C3, C4: TColor32;
    P: PColor32;
  begin
    flrx := X256 and $FF;
    flry := Y256 and $FF;

    X := SAR_8(X256);
    Y := SAR_8(Y256);

    celx := flrx xor 255;
    cely := flry xor 255;

    if (X > SrcRectI.Left) and (X < SrcRectI.Right - 1) and
      (Y > SrcRectI.Top) and (Y < SrcRectI.Bottom - 1) then
    begin
      // everything is ok interpolate between four neighbors
      P := Src.PixelPtr[X, Y];
      C1 := P^;
      Inc(P);
      C2 := P^;
      Inc(P, Src.Width);
      C4 := P^;
      Dec(P);
      C3 := P^;
      C := CombineReg(CombineReg(C1, C2, celx), CombineReg(C3, C4, celx), cely);
      Result := True;
    end
    else if (X < -1) or (Y < -1) or (X >= SrcRectI.Right) or (Y >= SrcRectI.Bottom) then
    begin
      // (X,Y) coordinate is out of the SrcRect, do not interpolate
      C := 0; // just write something to disable compiler warnings
      Result := False;
    end
    else
    begin
      // handle edge
      C1 := Src.PixelS[X, Y];
      C2 := Src.PixelS[X + 1, Y];
      C3 := Src.PixelS[X, Y + 1];
      C4 := Src.PixelS[X + 1, Y + 1];
      C := CombineReg(CombineReg(C1, C2, celx), CombineReg(C3, C4, celx), cely);   
      Result := True;
    end;
  end;
begin
  if not Transformation.TransformValid then Transformation.PrepareTransform;

  CombineCallBack := Src.OnPixelCombine; // store it into a local variable
  DrawMode := Src.DrawMode;    // store it into a local variable
  SrcAlpha := Src.MasterAlpha;
  if (DrawMode = dmCustom) and not Assigned(CombineCallBack) then
    DrawMode := dmOpaque;

  // clip SrcRect
  IntersectRect(SrcRectI, Rect(Transformation.SrcRect),
    Rect(0, 0, Src.Width - 1, Src.Height - 1));

  with Transformation.SrcRect do
  begin
    IntersectRect(SrcRect256, Rect(Round(Left * 256), Round(Top * 256),
      Round(Right * 256), Round(Bottom * 256)), Rect(0, 0, (Src.Width - 1) * 256,
      (Src.Height - 1) * 256));
  end;

  // clip DstRect
  R := Transformation.GetTransformedBounds;
  IntersectRect(DstRect, R, Rect(0, 0, Dst.Width - 1, Dst.Height - 1));
  if IsRectEmpty(DstRect) then Exit;

  try
    if Src.StretchFilter <> sfNearest then
      for J := DstRect.Top to DstRect.Bottom do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right do
        begin
          Transformation.Transform256(I, J, X, Y);
          if GET_S256(X, Y, C) then
            case DrawMode of
              dmOpaque: Pixels[I] := C;
              dmBlend: BlendMemEx(C, Pixels[I], SrcAlpha);
            else // dmCustom:
              CombineCallBack(C, Pixels[I], SrcAlpha);
            end;
        end;
      end
    else // nearest filter
      for J := DstRect.Top to DstRect.Bottom do
      begin
        Pixels := Dst.ScanLine[J];
        for I := DstRect.Left to DstRect.Right do
        begin
          Transformation.Transform(I, J, X, Y);
          if (X >= SrcRectI.Left) and (X <= SrcRectI.Right) and
            (Y >= SrcRectI.Top) and (Y <= SrcRectI.Bottom) then
          case DrawMode of
            dmOpaque: Pixels[I] := Src.Pixel[X, Y];
            dmBlend: BlendMemEx(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
          else // dmCustom:
            CombineCallBack(Src.Pixel[X, Y], Pixels[I], SrcAlpha);
          end;
        end;
      end;
  finally
    EMMS;
  end;
  Dst.Changed;
end;

procedure SetBorderTransparent(ABitmap: TBitmap32; ARect: TRect);
var
  I: Integer;
begin
  if TestClip(ARect.Left, ARect.Right, ABitmap.Width) and
    TestClip(ARect.Top, ARect.Bottom, ABitmap.Height) then
  begin
    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Top] := ABitmap[I, ARect.Top] and $00FFFFFF;

    for I := ARect.Left to ARect.Right do
      ABitmap[I, ARect.Bottom] := ABitmap[I, ARect.Bottom] and $00FFFFFF;

    if ARect.Bottom > ARect.Top + 1 then
      for I := ARect.Top + 1 to ARect.Bottom - 1 do
      begin
        ABitmap[ARect.Left, I] := ABitmap[ARect.Left, I] and $00FFFFFF;
        ABitmap[ARect.Right, I] := ABitmap[ARect.Right, I] and $00FFFFFF;
      end;

    ABitmap.Changed;
  end;
end;


{ TTransformation }

procedure TTransformation.SetSrcRect(const Value: TFloatRect);
begin
  FSrcRect := Value;
  TransformValid := False;
end;


{ TAffineTransformation }

procedure TAffineTransformation.Clear;
begin
  Matrix := IdentityMatrix;
  TransformValid := False;
end;

constructor TAffineTransformation.Create;
begin
  Clear;
end;

function TAffineTransformation.GetTransformedBounds: TRect;
var
  V1, V2, V3, V4: TVector3f;
begin
  V1[0] := FSrcRect.Left;  V1[1] := FSrcRect.Top;    V1[2] := 1;
  V2[0] := FSrcRect.Right; V2[1] := V1[1];           V2[2] := 1;
  V3[0] := V1[0];          V3[1] := FSrcRect.Bottom; V3[2] := 1;
  V4[0] := V2[0];          V4[1] := V3[1];           V4[2] := 1;
  V1 := VectorTransform(Matrix, V1);
  V2 := VectorTransform(Matrix, V2);
  V3 := VectorTransform(Matrix, V3);
  V4 := VectorTransform(Matrix, V4);
  Result.Left   := Round(Min(Min(V1[0], V2[0]), Min(V3[0], V4[0])) - 0.5);
  Result.Right  := Round(Max(Max(V1[0], V2[0]), Max(V3[0], V4[0])) + 0.5);
  Result.Top    := Round(Min(Min(V1[1], V2[1]), Min(V3[1], V4[1])) - 0.5);
  Result.Bottom := Round(Max(Max(V1[1], V2[1]), Max(V3[1], V4[1])) + 0.5);
end;

procedure TAffineTransformation.PrepareTransform;
var
  M: TFloatMatrix;
begin
  M := Matrix;
  Invert(M);

  // calculate a fixed point (4096) factors
  A := Round(M[0,0] * 4096); B := Round(M[1,0] * 4096); C := Round(M[2,0] * 4096);
  D := Round(M[0,1] * 4096); E := Round(M[1,1] * 4096); F := Round(M[2,1] * 4096);

  TransformValid := True;
end;

procedure TAffineTransformation.Rotate(Cx, Cy, Alpha: Single);
var
  S, C: Single;
  M: TFloatMatrix;
begin
  TransformValid := False;
  if (Cx <> 0) and (Cy <> 0) then Translate(-Cx, -Cy);
  Alpha := DegToRad(Alpha);
  S := Sin(Alpha); C := Cos(Alpha);
  M := IdentityMatrix;
  M[0,0] := C;   M[1,0] := S;
  M[0,1] := -S;  M[1,1] := C;
  Matrix := Mult(M, Matrix);
  if (Cx <> 0) and (Cy <> 0) then Translate(Cx, Cy);
end;

procedure TAffineTransformation.Scale(Sx, Sy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[0,0] := Sx;
  M[1,1] := Sy;
  Matrix := Mult(M, Matrix);
end;

procedure TAffineTransformation.Skew(Fx, Fy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[1, 0] := Fx;
  M[0, 1] := Fy;
  Matrix := Mult(M, Matrix);
end;

procedure TAffineTransformation.Transform(
  DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
begin
  SrcX := SAR_12(DstX * A + DstY * B + C + 2047);
  SrcY := SAR_12(DstX * D + DstY * E + F + 2047);
end;

procedure TAffineTransformation.Transform256(
  DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
begin
  SrcX256 := SAR_4(DstX * A + DstY * B + C + 7);
  SrcY256 := SAR_4(DstX * D + DstY * E + F + 7);
end;

procedure TAffineTransformation.Translate(Dx, Dy: Single);
var
  M: TFloatMatrix;
begin
  TransformValid := False;
  M := IdentityMatrix;
  M[2,0] := Dx;
  M[2,1] := Dy;
  Matrix := Mult(M, Matrix);
end;


{ TProjectiveTransformation }

function TProjectiveTransformation.GetTransformedBounds: TRect;
begin
  Result.Left   := Round(Min(Min(Wx0, Wx1), Min(Wx2, Wx3)) - 0.5);
  Result.Right  := Round(Max(Max(Wx0, Wx1), Max(Wx2, Wx3)) + 0.5);
  Result.Top    := Round(Min(Min(Wy0, Wy1), Min(Wy2, Wy3)) - 0.5);
  Result.Bottom := Round(Max(Max(Wy0, Wy1), Max(Wy2, Wy3)) + 0.5);
end;

procedure TProjectiveTransformation.PrepareTransform;
var
  dx1, dx2, px, dy1, dy2, py: Single;
  g, h, k: Single;
  R: TFloatMatrix;
begin
  px  := Wx0 - Wx1 + Wx2 - Wx3;
  py  := Wy0 - Wy1 + Wy2 - Wy3;

  if (px = 0) and (py = 0) then
  begin
    // affine mapping
    M[0,0] := Wx1 - Wx0;
    M[1,0] := Wx2 - Wx1;
    M[2,0] := Wx0;

    M[0,1] := Wy1 - Wy0;
    M[1,1] := Wy2 - Wy1;
    M[2,1] := Wy0;

    M[0,2] := 0;
    M[1,2] := 0;
    M[2,2] := 1;
  end
  else
  begin
    // projective mapping
    dx1 := Wx1 - Wx2;
    dx2 := Wx3 - Wx2;
    dy1 := Wy1 - Wy2;
    dy2 := Wy3 - Wy2;
    k := dx1 * dy2 - dx2 * dy1;
    Assert(k <> 0);
    g := (px * dy2 - py * dx2) / k;
    h := (dx1 * py - dy1 * px) / k;

    M[0,0] := Wx1 - Wx0 + g * Wx1;
    M[1,0] := Wx3 - Wx0 + h * Wx3;
    M[2,0] := Wx0;

    M[0,1] := Wy1 - Wy0 + g * Wy1;
    M[1,1] := Wy3 - Wy0 + h * Wy3;
    M[2,1] := Wy0;

    M[0,2] := g;
    M[1,2] := h;
    M[2,2] := 1;
  end;

  // denormalize texture space (u, v)
  R := IdentityMatrix;
  R[0,0] := 1 / (SrcRect.Right - SrcRect.Left);
  R[1,1] := 1 / (SrcRect.Bottom - SrcRect.Top);
  M := Mult(M, R);

  R := IdentityMatrix;
  R[2,0] := -SrcRect.Left;
  R[2,1] := -SrcRect.Top;
  M := Mult(M, R);

  Invert(M);

  TransformValid := True;
end;

procedure TProjectiveTransformation.SetX0(Value: Single);
begin
  Wx0 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX1(Value: Single);
begin
  Wx1 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX2(Value: Single);
begin
  Wx2 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetX3(Value: Single);
begin
  Wx3 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY0(Value: Single);
begin
  Wy0 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY1(Value: Single);
begin
  Wy1 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY2(Value: Single);
begin
  Wy2 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.SetY3(Value: Single);
begin
  Wy3 := Value;  TransformValid := False;
end;

procedure TProjectiveTransformation.Transform(DstX, DstY: Integer;
  out SrcX, SrcY: Integer);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := M[0,2] * X + M[1,2] * Y + M[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX := Round(M[0,0] * X + M[1,0] * Y + M[2,0]);
    SrcY := Round(M[0,1] * X + M[1,1] * Y + M[2,1]);
  end
  else
  begin
    Z := 1 / Z;
    SrcX := Round((M[0,0] * X + M[1,0] * Y + M[2,0]) * Z);
    SrcY := Round((M[0,1] * X + M[1,1] * Y + M[2,1]) * Z);
  end;
end;

procedure TProjectiveTransformation.Transform256(DstX, DstY: Integer;
  out SrcX256, SrcY256: Integer);
var
  X, Y, Z: Single;
begin
  EMMS;
  X := DstX; Y := DstY;
  Z := M[0,2] * X + M[1,2] * Y + M[2,2];
  if Z = 0 then Exit
  else if Z = 1 then
  begin
    SrcX256 := Round(256 * (M[0,0] * X + M[1,0] * Y + M[2,0]));
    SrcY256 := Round(256 * (M[0,1] * X + M[1,1] * Y + M[2,1]));
  end
  else
  begin
    Z := 1 / Z;
    SrcX256 := Round(256 * (M[0,0] * X + M[1,0] * Y + M[2,0]) * Z);
    SrcY256 := Round(256 * (M[0,1] * X + M[1,1] * Y + M[2,1]) * Z);
  end;
end;

end.
