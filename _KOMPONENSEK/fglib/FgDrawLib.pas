{------------------------------------------------------------------------------}
{ Copyright 2001 by EuroSoft Software Development                              }
{ designl@worldnet.net                                                         }
{                                                                              }
{ This software is provided 'as-is', without any express or implied warranty.  }
{ In no event will the author be held liable for any  damages arising from     }
{ the use of this software.                                                    }
{                                                                              }
{ No part of this Unit may be copied in any way without a written permission.  }
{------------------------------------------------------------------------------}

unit FgDrawLib;

interface

uses SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Typefglib;


procedure DrawShadow(Handle : HDC; left,top,right,bottom,width,intensity : integer;
                     ShadowPos:TShadowPos; Quality : TQuality);
procedure DrawFilter(Handle : HDC; left,top,right,bottom,intensity : integer; Quality : TQuality);
procedure DrawMosaic(Handle : HDC; Left,Top,Width,Height : integer; Bitmap : HBitmap);
function PixelChanged(Color:TColor; value:integer) : TColor;
procedure TransparentBitmap(Handle : HDC; xx,yy : integer; Bitmap : HBitmap;
                            TranparentColor : TColor; Intensity : integer);
procedure DrawBorder(Handle : HDC; left,top,right,bottom,value,shading,reduction : integer;
                     BorderPos:TBorderPos; Quality : TQuality);
procedure DrawBorderRect(Handle : HDC; left,top,right,bottom,width,intensity,shading,reduction : integer;
                         Quality : TQuality);
procedure DrawBitmap(Handle : HDC; Left,Top : integer; Bitmap : HBitmap);
procedure DrawHint(Handle : HDC;
                    HintFont : TFontData;
                   Left,Top,Width,Height : integer;
                   RealTextHeight : integer;
                   Color : TColor;
                   Caption : string;
                   Position : THintPosition;
                   HintStyle : THintStyle;
                   LinkStyle : TlinkStyle;
                   LinkSize : integer;
                   Shadow : Boolean;
                   ShadowQuality : TQuality;
                   ShadowWidth : integer;
                   ShadowIntensity : integer;
                   bitmap : HBitmap;
                   Border : boolean;
                   BorderColor : TColor);

implementation


procedure DrawShadow(Handle : HDC; left,top,right,bottom,width,intensity : integer;
                     ShadowPos:TShadowPos; Quality : TQuality);
begin
  case ShadowPos of
    shTopLeft:
    begin
      DrawFilter(Handle,left+width+1,top,right-width,top+width,intensity,Quality);
      DrawFilter(Handle,left,top,left+width,bottom-width,intensity,Quality);
    end;
    shTopRight:
    begin
      DrawFilter(Handle,left+width,top,right-width-1,top+width,intensity,Quality);
      DrawFilter(Handle,right-width,top,right,bottom-width,intensity,Quality);
    end;
    shBottomRight:
    begin
      DrawFilter(Handle,left+width,bottom-width,right-width-1,bottom,intensity,Quality);
      DrawFilter(Handle,right-width,top+width,right,bottom,intensity,Quality);
    end;
    shBottomLeft:
    begin
      DrawFilter(Handle,left+width+1,bottom-width,right-width,bottom,intensity,Quality);
      DrawFilter(Handle,left,top+width,left+width,bottom,intensity,Quality);
    end;
  end;
end;

procedure DrawFilter(Handle : HDC; left,top,right,bottom,intensity : integer; Quality : TQuality);
var
  x,y : integer;
  width : integer;
  Red : integer;
  Cv : TCanvas;
begin
  x := left;
  repeat
    y := top;
    repeat
      case Quality of
        quLow : if (odd(x)=odd(y)) then setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
        quHi  : setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
      end;
      inc(y);
    until y>bottom;
    inc(x);
  until x>right;
end;

procedure DrawMosaic(Handle : HDC; Left,Top,Width,Height : integer; Bitmap : HBitmap);
var
  i,ii,nh,nw : integer;
  Cv : TCanvas;
  CvTmp : TBitmap;
  Bmp : TBitmap;
begin
  Cv := TCanvas.create;
  Cv.handle := Handle;
  Bmp := TBitmap.create;
  Bmp.handle := Bitmap;
  CvTmp := TBitmap.create;
  CvTmp.height := height;
  CvTmp.width := width;
  if ((Bmp.height>0) and (Bmp.width>0)) then
  begin
    nh := trunc(height/Bmp.height);
    nw := trunc(width/Bmp.width);
    for i := 0 to nh do for ii := 0 to nw do CvTmp.Canvas.Draw(ii*Bmp.width,i*Bmp.height,Bmp);
    Cv.Draw(Left,Top,CvTmp);
  end;
  Bmp.releasehandle;
  Bmp.free;
  CvTmp.free;
  Cv.free;
end;

function PixelChanged(Color:TColor; value:integer) : TColor;
var
  r,g,b : integer;
begin
  r := getrvalue(ColorToRGB(Color));
  g := getgvalue(ColorToRGB(Color));
  b := getbvalue(ColorToRGB(Color));
  if value>0 then
  begin
    if r+value>255 then r := 254 else inc(r,value);
    if g+value>255 then g := 254 else inc(g,value);
    if b+value>255 then b := 254 else inc(b,value);
  end else
  begin
    if r+value<0 then r := 1 else inc(r,value);
    if g+value<0 then g := 1 else inc(g,value);
    if b+value<0 then b := 1 else inc(b,value);
  end;
  result := rgb(r,g,b);
end;

procedure DrawBorder(Handle : HDC; left,top,right,bottom,value,shading,reduction : integer;
                     BorderPos:TBorderPos; Quality : TQuality);
var
  x,y : integer;
  width : integer;
  Red : integer;
  Intensity : integer;
  Cv : TCanvas;
begin
  if reduction=-1 then
  begin
    if BorderPos=boHorizontal then Red := bottom-top;
    if BorderPos=boVertical then Red := right-left;
  end else Red := 0;
  Intensity := value;
  if BorderPos=boHorizontal then
  begin
    y := top;
    repeat
      if value>0 then inc(Intensity,Shading) else dec(Intensity,Shading);
      x := left+Red;
      repeat
        case Quality of
          quLow : if (odd(x)=odd(y)) then setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
          quHi  : setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
        end;
        inc(x);
      until x>right-Red;
      inc(y);
      inc(Red,reduction);
    until y>bottom;
  end else
  begin
    x := left;
    repeat
      if value>0 then inc(Intensity,Shading) else dec(Intensity,Shading);
      y := top+Red;
      repeat
        case Quality of
          quLow : if (odd(x)=odd(y)) then setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
          quHi  : setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),Intensity));
        end;
        inc(y);
      until y>bottom-Red;
      inc(x);
      inc(Red,reduction);
    until x>right;
  end;
end;

procedure DrawBorderRect(Handle : HDC; left,top,right,bottom,width,intensity,shading,reduction : integer;
                         Quality : TQuality);
begin
  DrawBorder(Handle,left,top,right,top+width,intensity,shading,reduction,boHorizontal,Quality);
  DrawBorder(Handle,left,top,left+width,bottom,intensity,shading,reduction,boVertical,Quality);
  DrawBorder(Handle,left,bottom-width,right,bottom,-intensity,shading,-reduction,boHorizontal,Quality);
  DrawBorder(Handle,right-width,top,right,bottom,-intensity,shading,-reduction,boVertical,Quality);
end;

procedure TransparentBitmap(Handle : HDC; xx,yy : integer; Bitmap : HBitmap;
                            TranparentColor : TColor; Intensity : integer);
var
  Cv : TCanvas;
  Bmp : TBitmap;
  CanvasMem : TBitmap;
  x,y : integer;
  ClientRect : TRect;
begin
  Cv := TCanvas.create;
  Cv.handle := Handle;
  Bmp := TBitmap.create;
  Bmp.handle := Bitmap;
  ClientRect := rect(0,0,Bmp.Width+xx*2,Bmp.height+yy*2);
  CanvasMem := TBitmap.Create;
  CanvasMem.Width := Bmp.Width+xx*2;
  CanvasMem.height := Bmp.height+yy*2;
  CanvasMem.Canvas.CopyRect(ClientRect,Cv,ClientRect);
  for x := 0 to Bmp.width-1 do for y := 0 to Bmp.height-1 do
    if getpixel(Bmp.Canvas.Handle,x,y)<>TranparentColor then
    if Intensity<>0 then
    setpixel(CanvasMem.Canvas.Handle,xx+x,yy+y,PixelChanged(getpixel(Bmp.Canvas.Handle,x,y),Intensity)) else
    setpixel(CanvasMem.Canvas.Handle,xx+x,yy+y,getpixel(Bmp.Canvas.Handle,x,y));
  cv.CopyRect(ClientRect,CanvasMem.Canvas,ClientRect);
  CanvasMem.free;
  Bmp.releasehandle;
  Bmp.free;
  Cv.free;
end;

procedure DrawBitmap(Handle : HDC; Left,Top : integer; Bitmap : HBitmap);
var
  Cv : TCanvas;
  Bmp : TBitmap;
begin
  Cv := TCanvas.create;
  Cv.handle := Handle;
  Bmp := TBitmap.create;
  Bmp.handle := Bitmap;
  Cv.Draw(Left,Top,Bmp);
  Bmp.releasehandle;
  Bmp.free;
  Cv.free;
end;

procedure DrawHint(Handle : HDC;
                    HintFont : TFontData;
                   Left,Top,Width,Height : integer;
                   RealTextHeight : integer;
                   Color : TColor;
                   Caption : string;
                   Position : THintPosition;
                   HintStyle : THintStyle;
                   LinkStyle : TlinkStyle;
                   LinkSize : integer;
                   Shadow : Boolean;
                   ShadowQuality : TQuality;
                   ShadowWidth : integer;
                   ShadowIntensity : integer;
                   bitmap : HBitmap;
                   Border : boolean;
                   BorderColor : TColor); export;
var
 HintRect,ShadowRect : TRect;
 RectTmp : TRect;
 HintPoint : array[0..2] of TPoint;
 ShadowPoint : array[0..2] of TPoint;
 ShadowRgn,ShadowRgn_,b0s : HRgn;
 BRect : array[0..2] of TRect;
 BSRect : array[0..2] of TRect;
 text : array[0..255] of Char;
 TmpPicture : TPicture;
 i,ii,x,y,nh,nw : integer;
 Cv : Tcanvas;
 ColorTmp : TColor;
procedure PaintShadow(Handle : HDC; Rect : TRect; Rgn : HRgn; ShadowQuality : TQuality; Intensity : integer);
var
 x,y : integer;
begin
  for x := Rect.left to Rect.right do
  for y := Rect.top to Rect.bottom do
  begin
    case ShadowQuality of
      quLow : if (odd(x)=odd(y)) and PtInRegion(Rgn,x,y) then
              setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),-Intensity));
      quHi  : if PtInRegion(Rgn,x,y) then
              setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),-Intensity));
    end;
  end;
end;
procedure PaintShadow_(Handle : HDC; Rect : TRect; Rgn,Rgnb : HRgn; ShadowQuality : TQuality; Intensity : integer);
var
 x,y : integer;
begin
  for x := Rect.left to Rect.right do
  for y := Rect.top to Rect.bottom do
  begin
    case ShadowQuality of
      quLow : if (odd(x)=odd(y)) and PtInRegion(Rgn,x,y) and not PtInRegion(Rgnb,x,y) then
              setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),-Intensity));
      quHi  : if PtInRegion(Rgn,x,y) and not PtInRegion(Rgnb,x,y) then
              setpixel(Handle,x,y,PixelChanged(getpixel(Handle,x,y),-Intensity));
    end;
  end;
end;
begin
  Cv := Tcanvas.create;
  Cv.handle := Handle;
  if (not Shadow or (HintStyle=hiText)) then ShadowWidth := 0;
  case Position of
    hiTopRight :
    begin
      HintRect := Rect(0,0,Width-ShadowWidth, Height-ShadowWidth-LinkSize);
      ShadowRect := Rect(0+ShadowWidth,0+ShadowWidth,Width,Height-LinkSize);
      if HintStyle=hiBubble then
      begin
        HintPoint[0] := Point(trunc(width/3),
                              Height-ShadowWidth-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        HintPoint[1] := Point(0,height-ShadowWidth);
        HintPoint[2] := Point(trunc(width/3)*2,
                              Height-ShadowWidth-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[0] := Point(trunc(width/3)+ShadowWidth,
                                Height-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[1] := Point(ShadowWidth,height);
        ShadowPoint[2] := Point(trunc(width/3)*2+ShadowWidth,
                                Height-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
      end else
      begin
        HintPoint[0] := Point(10,Height-ShadowWidth-LinkSize-2);
        HintPoint[1] := Point(0,height-ShadowWidth);
        HintPoint[2] := Point(30,Height-ShadowWidth-LinkSize-2);
        ShadowPoint[0] := Point(10+ShadowWidth,Height-LinkSize-1);
        ShadowPoint[1] := Point(0+ShadowWidth,height);
        ShadowPoint[2] := Point(30+ShadowWidth,Height-LinkSize-1);  
      end;
      BRect[0] := Rect(0,height-ShadowWidth-6,6,height-ShadowWidth);
      BRect[1] := Rect(7,height-ShadowWidth-12,17,height-ShadowWidth-2);
      BRect[2] := Rect(15,height-ShadowWidth-25,30,height-ShadowWidth-10);
      x := 0;
      y := 1;
    end;
    hiBottomRight :
    begin
      HintRect := Rect(0,0+LinkSize,Width-ShadowWidth, Height-ShadowWidth);
      ShadowRect := Rect(0+ShadowWidth,0+LinkSize+ShadowWidth,Width,Height);
      if HintStyle=hiBubble then
      begin
        HintPoint[0] := Point(trunc(width/3),
                              LinkSize+trunc((Height-ShadowWidth-LinkSize)/2));
        HintPoint[1] := Point(0,0);
        HintPoint[2] := Point(trunc(width/3)*2,
                              LinkSize+trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[0] := Point(trunc(width/3)+ShadowWidth,
                                LinkSize+ShadowWidth+trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[1] := Point(ShadowWidth,ShadowWidth);
        ShadowPoint[2] := Point(trunc(width/3)*2+ShadowWidth,
                                LinkSize+ShadowWidth+trunc((Height-ShadowWidth-LinkSize)/2));
      end else
      begin
        HintPoint[0] := Point(10,LinkSize+1);
        HintPoint[1] := Point(0,0);
        HintPoint[2] := Point(30,LinkSize+1);
        ShadowPoint[0] := Point(10+ShadowWidth,LinkSize+ShadowWidth);
        ShadowPoint[1] := Point(0+ShadowWidth,ShadowWidth);
        ShadowPoint[2] := Point(30+ShadowWidth,LinkSize+ShadowWidth);
      end;
      BRect[0] := Rect(0,0,6,6);
      BRect[1] := Rect(7,2,17,12);
      BRect[2] := Rect(15,10,30,25);
      x := 0;
      y := -1;
    end;
    hiTopLeft :
    begin
      HintRect := Rect(0,0,Width-ShadowWidth, Height-ShadowWidth-LinkSize);
      ShadowRect := Rect(0+ShadowWidth,0+ShadowWidth,Width,Height-LinkSize);
      if HintStyle=hiBubble then
      begin
        HintPoint[0] := Point(width-trunc(width/3)-ShadowWidth,
                              Height-ShadowWidth-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        HintPoint[1] := Point(width-ShadowWidth,height-ShadowWidth);
        HintPoint[2] := Point(width-trunc(width/3)-trunc(width/3)-ShadowWidth,
                              Height-ShadowWidth-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[0] := Point(width-trunc(width/3),
                                Height-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[1] := Point(width,height);
        ShadowPoint[2] := Point(width-trunc(width/3)-trunc(width/3),
                                Height-LinkSize-trunc((Height-ShadowWidth-LinkSize)/2));
      end else
      begin
        HintPoint[0] := Point(width-10-ShadowWidth,Height-ShadowWidth-LinkSize-2);
        HintPoint[1] := Point(width-ShadowWidth,height-ShadowWidth);
        HintPoint[2] := Point(width-30-ShadowWidth,Height-ShadowWidth-LinkSize-2);
        ShadowPoint[0] := Point(width-10,Height-LinkSize-1);
        ShadowPoint[1] := Point(width,height);
        ShadowPoint[2] := Point(width-30,Height-LinkSize-1);
      end;
      BRect[0] := Rect(width-ShadowWidth-6,height-ShadowWidth-6,width-ShadowWidth,height-ShadowWidth);
      BRect[1] := Rect(width-ShadowWidth-17,height-ShadowWidth-12,width-ShadowWidth-7,height-ShadowWidth-2);
      BRect[2] := Rect(width-ShadowWidth-30,height-ShadowWidth-25,width-ShadowWidth-15,height-ShadowWidth-10);
      x := -2;
      y := 1;
    end;
    hiBottomLeft :
    begin
      HintRect := Rect(0,0+LinkSize,Width-ShadowWidth, Height-ShadowWidth);
      ShadowRect := Rect(0+ShadowWidth,0+LinkSize+ShadowWidth,Width,Height);
      if HintStyle=hiBubble then
      begin
        HintPoint[0] := Point(width-trunc(width/3)-ShadowWidth,
                              LinkSize+trunc((Height-ShadowWidth-LinkSize)/2));
        HintPoint[1] := Point(width-ShadowWidth,0);
        HintPoint[2] := Point(width-trunc(width/3)-trunc(width/3)-ShadowWidth,
                              LinkSize+trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[0] := Point(width-trunc(width/3),
                                LinkSize+ShadowWidth+trunc((Height-ShadowWidth-LinkSize)/2));
        ShadowPoint[1] := Point(width,ShadowWidth);
        ShadowPoint[2] := Point(width-trunc(width/3)-trunc(width/3),
                                LinkSize+ShadowWidth+trunc((Height-ShadowWidth-LinkSize)/2));
      end else
      begin
        HintPoint[0] := Point(width-10-ShadowWidth,LinkSize+2);
        HintPoint[1] := Point(width-ShadowWidth,0);
        HintPoint[2] := Point(width-30-ShadowWidth,LinkSize+2);
        ShadowPoint[0] := Point(width-10,LinkSize+ShadowWidth);
        ShadowPoint[1] := Point(width,ShadowWidth);
        ShadowPoint[2] := Point(width-30,LinkSize+ShadowWidth);
      end;
      BRect[0] := Rect(width-ShadowWidth-6,0,width-ShadowWidth,6);
      BRect[1] := Rect(width-ShadowWidth-17,2,width-ShadowWidth-7,12);
      BRect[2] := Rect(width-ShadowWidth-30,10,width-ShadowWidth-15,25);
      x := -2;
      y := -1;
    end;
  end;
  with cv do
  begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    if ShadowWidth>0 then
    begin
      case HintStyle of
        hiRectangle : ShadowRgn := CreateRectRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom-1);
        hiRoundrect : ShadowRgn := CreateRoundRectRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom,15,15);
        hiBubble : ShadowRgn := CreateEllipticRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom);
        hiImage : ShadowRgn := CreateRectRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom);
        hiTexture : ShadowRgn := CreateRectRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom);
      end;
      PaintShadow(cv.handle,ShadowRect,ShadowRgn,ShadowQuality,ShadowIntensity);
      DeleteObject(ShadowRgn);
      case LinkStyle of
        liBubble:
        begin
          ShadowRgn := CreateEllipticRgn(BRect[0].left+ShadowWidth,BRect[0].top+ShadowWidth,
                                         BRect[0].right+ShadowWidth,BRect[0].bottom+ShadowWidth);
          RectTmp := Rect(BRect[0].left+ShadowWidth,BRect[0].top+ShadowWidth,
                          BRect[0].right+ShadowWidth,BRect[0].bottom+ShadowWidth);
          PaintShadow(cv.handle,RectTmp,ShadowRgn,ShadowQuality,ShadowIntensity);
          DeleteObject(ShadowRgn);
          ShadowRgn := CreateEllipticRgn(BRect[1].left+ShadowWidth,BRect[1].top+ShadowWidth,
                                         BRect[1].right+ShadowWidth,BRect[1].bottom+ShadowWidth);
          RectTmp := Rect(BRect[1].left+ShadowWidth,BRect[1].top+ShadowWidth,
                          BRect[1].right+ShadowWidth,BRect[1].bottom+ShadowWidth);
          PaintShadow(cv.handle,RectTmp,ShadowRgn,ShadowQuality,ShadowIntensity);
          DeleteObject(ShadowRgn);
          ShadowRgn := CreateEllipticRgn(BRect[2].left+ShadowWidth,BRect[2].top+ShadowWidth,
                                         BRect[2].right+ShadowWidth,BRect[2].bottom+ShadowWidth);
          RectTmp := Rect(BRect[2].left+ShadowWidth,BRect[2].top+ShadowWidth,
                          BRect[2].right+ShadowWidth,BRect[2].bottom+ShadowWidth);
          PaintShadow(cv.handle,RectTmp,ShadowRgn,ShadowQuality,ShadowIntensity);
          DeleteObject(ShadowRgn);
        end;
        liArrow:
        begin
          ShadowRgn := CreatePolygonRgn(ShadowPoint,3,Winding);
          ShadowRgn_ := CreateEllipticRgn(ShadowRect.left,ShadowRect.top,ShadowRect.right,ShadowRect.bottom);
          case Position of
            hiTopRight : RectTmp := Rect(ShadowPoint[1].x,ShadowPoint[0].y,ShadowPoint[2].x,ShadowPoint[1].y);
            hiBottomRight : RectTmp := Rect(ShadowPoint[1].x,ShadowPoint[1].y,ShadowPoint[2].x,ShadowPoint[0].y);
            hiTopLeft : RectTmp := Rect(ShadowPoint[2].x,ShadowPoint[0].y,ShadowPoint[1].x,ShadowPoint[1].y);
            hiBottomLeft : RectTmp := Rect(ShadowPoint[2].x,ShadowPoint[1].y,ShadowPoint[1].x,ShadowPoint[0].y);
          end;
          PaintShadow_(cv.handle,RectTmp,ShadowRgn,ShadowRgn_,ShadowQuality,ShadowIntensity);
          DeleteObject(ShadowRgn);
          DeleteObject(ShadowRgn_);
        end;
      end;
    end;
    Brush.Style := bsSolid;
    Pen.Color := Color;
    Brush.Color := Color;
    ColorTmp := BorderColor;
    if Border then Pen.Color := BorderColor;

    case HintStyle of
      hiRectangle : Rectangle(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom);
      hiRoundrect : RoundRect(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom,15,15);
      hiBubble : Ellipse(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom);
      hiImage :
      begin
        if Border then
        begin
          Pen.Style := psSolid;
          Brush.Style := bsClear;
          Rectangle(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom);
          DrawBitmap(handle,HintRect.left+1,HintRect.top+1,bitmap);
        end else DrawBitmap(handle,HintRect.left,HintRect.top,bitmap);
      end;
      hiTexture :
      begin
        Brush.Style := bsClear;
        if Border then
        begin
          Pen.Style := psSolid;
          Rectangle(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom);
          DrawMosaic(handle,HintRect.left+1,HintRect.top+1,HintRect.right-2,HintRect.bottom-2,bitmap);
        end else DrawMosaic(handle,HintRect.left,HintRect.top,HintRect.right,HintRect.bottom,bitmap);
      end;
      hiText :
      if Shadow then
      begin
        ShadowRgn := CreateRectRgn(HintRect.left, HintRect.top, HintRect.right, HintRect.bottom);
        PaintShadow(cv.handle,HintRect,ShadowRgn,ShadowQuality,ShadowIntensity);
        DeleteObject(ShadowRgn);
      end;
    end;

    if LinkStyle=liArrow then
    begin
      Polygon(HintPoint);
    end;
    if LinkStyle=liBubble then
    begin
      Ellipse(BRect[0].left,BRect[0].top,BRect[0].right,BRect[0].bottom);
      Ellipse(BRect[1].left,BRect[1].top,BRect[1].right,BRect[1].bottom);
      Ellipse(BRect[2].left,BRect[2].top,BRect[2].right,BRect[2].bottom);
    end;
    Pen.Style := psClear;

    case HintStyle of
      hiRectangle : Rectangle(HintRect.left+1, HintRect.top+1, HintRect.right, HintRect.bottom);
      hiRoundrect : RoundRect(HintRect.left+1, HintRect.top+1, HintRect.right, HintRect.bottom,15,15);
      hiBubble : Ellipse(HintRect.left+1, HintRect.top+1, HintRect.right, HintRect.bottom);
    end;

    SetBkMode(Handle,Transparent);
    StrPCopy(Text, Caption);
    Case HintStyle of
      hiRectangle:
      begin
        inc(HintRect.left,2);
        dec(HintRect.right,2);
      end;
      hiRoundRect:
      begin
        inc(HintRect.left,3);
        dec(HintRect.right,3);
      end;
      hiBubble:
      begin
        inc(HintRect.left,trunc(RealTextHeight/2));
        dec(HintRect.right,trunc(RealTextHeight/2));
        inc(HintRect.top,trunc((HintRect.Bottom-HintRect.Top-RealTextHeight)/2));
      end;
      hiImage:
      begin
        inc(HintRect.left,5);
        dec(HintRect.right,5);
        inc(HintRect.top,5);
        dec(HintRect.bottom,5);
      end;
      hiTexture:
      begin
        inc(HintRect.left,2);
        dec(HintRect.right,2);
      end;
    end;
    Font.Color := HintFont.Color;
    if HintFont.size<=30 then Font.size := HintFont.size else Font.size := 30;
    Font.Style := HintFont.Style;
    Font.Name := HintFont.Name;
    DrawText(Handle, Text, StrLen(Text), HintRect, DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  end; 
  Cv.free;
end;
end.
