// GLGraphics
{: Egg<p>

	Fonction utilitaires graphiques<p>

	<b>Historique : </b><font size=-1><ul>
      <li>20/02/01 - Egg - Fixed SetHeight & SetWidth (thx Nelson Chu)
      <li>14/02/01 - Egg - Simplified RegisterAsOpenGLTexture
      <li>15/01/01 - Egg - Fixed RegisterAsOpenGLTexture (clamping) 
      <li>14/01/01 - Egg - Fixed isEmpty (was invalid for rectangles)
      <li>08/10/00 - Egg - Fixed RegisterAsOpenGLTexture and Assign(nil)
      <li>25/09/00 - Egg - First operational code
	   <li>19/08/00 - Egg - Creation
	</ul></font>
}
unit GLGraphics;

interface

uses Windows, Classes, Graphics, GLMisc, OpenGL12;

type

   // TGLPixel24
   //
   TGLPixel24 = packed record
      r, g, b : Byte;
   end;
   PGLPixel24 = ^TGLPixel24;

   // TGLPixel32
   //
   TGLPixel32 = packed record
      r, g, b, a : Byte;
   end;
   PGLPixel32 = ^TGLPixel32;

   TGLPixel32Array = array [0..MaxInt shr 3] of TGLPixel32;
   PGLPixel32Array = ^TGLPixel32Array;

	// TGLBitmap32
	//
   {: Contains and manipulates a 32 bits (24+8) bitmap.<p>
      This is the base class for preparing and manipulating textures in GLScene,
      this function does not rely on a windows handle and should be used for
      in-memory manipulations only.<br>
      16 bits textures are automatically converted to 24 bits and a null (zero)
      alpha channel is assumed for all planes, the byte order is as specified
      in GL_RGBA.<p>
      If 32 bits is used in this class, it can however output 16 bits texture
      data for use in OpenGL. }
	TGLBitmap32 = class (TPersistent)
	   private
	      { Private Declarations }
         FData : PGLPixel32Array;
         FWidth, FHeight : Integer;
         FDataSize : Integer;

	   protected
	      { Protected Declarations }
         procedure SetWidth(val : Integer);
         procedure SetHeight(const val : Integer);
         function GetScanLine(index : Integer) : PGLPixel32Array;
         procedure AssignFrom24BitsBitmap(aBitmap : TBitmap);
         procedure AssignFrom32BitsBitmap(aBitmap : TBitmap);

	   public
	      { Public Declarations }
	      constructor Create;
         destructor Destroy; override;

         {: Accepts TGLBitmap32 and TGraphic subclasses. }
         procedure Assign(Source: TPersistent); override;

         {: Create a 32 bits TBitmap from self content. }
         function Create32BitsBitmap : TBitmap;

         {: True if the bitmap is empty (ie. width or height is zero). }
	      function IsEmpty : Boolean;

         {: Width of the bitmap.<p>
            Will be forced to the nearest superior multiple of 4, f.i. writing
            Width:=6 is equivalent to writing Width:=8. }
         property Width : Integer read FWidth write SetWidth;
         {: Height of the bitmap. }
         property Height : Integer read FHeight write SetHeight;
         {: Size of the bitmap data in bytes. }
         property DataSize : Integer read FDataSize;

         {: Access to a specific Bitmap ScanLine.<p>
            index should be in the [0; Height[ range.<p>
            Warning : this function is NOT protected against invalid indexes,
            and invoking it is invalid if the bitmap is Empty. }
         property ScanLine[index : Integer] : PGLPixel32Array read GetScanLine;

         {: Grants direct access to the bitmap's data.<p>
            This property is equivalent to ScanLine[0], and may be nil is the
            bitmap is empty. }
         property Data : PGLPixel32Array read FData;

         {: Set Alpha channel values to the pixel intensity.<p>
            The intensity is calculated as the mean of RGB components. }
         procedure SetAlphaFromIntensity;
         {: Set Alpha channel to 0 for pixels of given color, 255 for others).<p>
            This makes pixels of given color totally transparent while the others
            are completely opaque. }
         procedure SetAlphaTransparentForColor(const aColor : TColor); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel32); overload;
         procedure SetAlphaTransparentForColor(const aColor : TGLPixel24); overload;
         {: Set Alpha channel values to given byte value. }
         procedure SetAlphaToValue(const aValue : Byte);
         {: Set Alpha channel values to given float [0..1] value. }
         procedure SetAlphaToFloatValue(const aValue : Single);
         {: Inverts the AlphaChannel component.<p>
            What was transparent becomes opaque and vice-versa. }
         procedure InvertAlpha;
         {: AlphaChannel components are replaced by their sqrt.<p> }
         procedure SqrtAlpha;

         {: Registers the bitmap's content as an OpenGL texture map.<p>
            Legal values for bytesPerPixel are :<ul>
            <li>4 : RGB+A (32 bits)
            <li>3 : RGB (24 bits)
            <li>1 : Alpha channel only (8 bits)
            </ul> }
         procedure RegisterAsOpenGLTexture(minFilter : TGLMinFilter;
                                           texFormat : Integer = GL_RGBA);

         {: Reads the given area from the current active OpenGL rendering context.<p>
            The best spot for reading pixels is within a SceneViewer's PostRender
            event : the scene has been fully rendered and the OpenGL context
            is still active. } 
         procedure ReadPixels(const area : TRect);
         {: Draws the whole bitmap at given position in the current OpenGL context.<p>
            This function must be called with a rendering context active.<p>
            Blending and Alpha channel functions are not altered by this function
            and must be adjusted separately. }
         procedure DrawPixels(const x, y : Single);
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, Geometry;

// ------------------
// ------------------ TGLBitmap32 ------------------
// ------------------

// Create
//
constructor TGLBitmap32.Create;
begin
	inherited Create;
end;

// Destroy
//
destructor TGLBitmap32.Destroy;
begin
   FreeMem(FData);
	inherited Destroy;
end;

// Assign
//
procedure TGLBitmap32.Assign(Source: TPersistent);
var
   bmp : TBitmap;
   graphic : TGraphic;
begin
   if Source=nil then begin
      FDataSize:=0;
      FWidth:=0;
      FHeight:=0;
      FreeMem(FData);
   end else if Source is TGLBitmap32 then begin
      // duplicate the data
      FDataSize:=TGLBitmap32(Source).DataSize;
      FWidth:=TGLBitmap32(Source).Width;
      FHeight:=TGLBitmap32(Source).Height;
      ReallocMem(FData, FDataSize);
      Move(TGLBitmap32(Source).Data^, Data^, DataSize);
   end else if Source is TGraphic then begin
      if (Source is TBitmap) and (TBitmap(Source).PixelFormat in [pf24bit, pf32bit])
            and ((TBitmap(Source).Width and 3)=0) then begin
         if TBitmap(Source).PixelFormat=pf24bit then
            AssignFrom24BitsBitmap(TBitmap(Source))
         else AssignFrom32BitsBitmap(TBitmap(Source))
      end else begin
         graphic:=TGraphic(Source);
         bmp:=TBitmap.Create;
         try
            bmp.PixelFormat:=pf24bit;
            bmp.Height:=graphic.Height;
            if (graphic.Width and 3)=0 then begin
               bmp.Width:=graphic.Width;
               bmp.Canvas.Draw(0, 0, graphic);
            end else begin
               bmp.Width:=(graphic.Width and $FFFC)+4;
               bmp.Canvas.StretchDraw(Rect(0, 0, bmp.Width, bmp.Height), graphic);
            end;
            AssignFrom24BitsBitmap(bmp);
         finally
            bmp.Free;
         end;
      end;
   end else inherited;
end;

// AssignFrom24BitsBitmap
//
procedure TGLBitmap32.AssignFrom24BitsBitmap(aBitmap : TBitmap);
var
   y, x : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=pf24bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   for y:=0 to Height-1 do begin
      pSrc:=aBitmap.ScanLine[y];
      for x:=0 to Width-1 do begin
         pDest[x*4+0]:=pSrc[x*3+2];
         pDest[x*4+1]:=pSrc[x*3+1];
         pDest[x*4+2]:=pSrc[x*3+0];
         pDest[x*4+3]:=#0;
      end;
      Dec(pDest, Width*4);
   end;
end;

// AssignFrom32BitsBitmap
//
procedure TGLBitmap32.AssignFrom32BitsBitmap(aBitmap : TBitmap);
var
   y, x : Integer;
   pSrc, pDest : PChar;
begin
   Assert(aBitmap.PixelFormat=pf32bit);
   Assert((aBitmap.Width and 3)=0);
   FWidth:=aBitmap.Width;
   FHeight:=aBitmap.Height;
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   pDest:=@PChar(FData)[Width*4*(Height-1)];
   for y:=0 to Height-1 do begin
      pSrc:=aBitmap.ScanLine[y];
      for x:=0 to Width-1 do begin
         pDest[x*4+0]:=pSrc[x*4+2];
         pDest[x*4+1]:=pSrc[x*4+1];
         pDest[x*4+2]:=pSrc[x*4+0];
         pDest[x*4+3]:=pSrc[x*4+3];
      end;
      Dec(pDest, Width*4);
   end;
end;

// Create32BitsBitmap
//
function TGLBitmap32.Create32BitsBitmap : TBitmap;
var
   y, x, x4 : Integer;
   pSrc, pDest : PChar;
begin
   Result:=TBitmap.Create;
   Result.PixelFormat:=pf32bit;
   Result.Width:=Width;
   Result.Height:=Height;
   pSrc:=@PChar(FData)[Width*4*(Height-1)];
   for y:=0 to Height-1 do begin
      pDest:=Result.ScanLine[y];
      for x:=0 to Width-1 do begin
         x4:=x*4;
         pDest[x4+0]:=pSrc[x4+2];
         pDest[x4+1]:=pSrc[x4+1];
         pDest[x4+2]:=pSrc[x4+0];
         pDest[x4+3]:=pSrc[x4+3];
      end;
      Dec(pSrc, Width*4);
   end;
end;

// IsEmpty
//
function TGLBitmap32.IsEmpty : Boolean;
begin
	Result:=(Width=0) or (Height=0);
end;

// SetWidth
//
procedure TGLBitmap32.SetWidth(val : Integer);
begin
   if (val and 3)>0 then
      val:=(val and $FFFC)+4;
   if val<>FWidth then begin
      Assert(val>=0);
      FWidth:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// SetHeight
//
procedure TGLBitmap32.SetHeight(const val : Integer);
begin
   if val<>FHeight then begin
      Assert(val>=0);
      FHeight:=val;
      FDataSize:=FWidth*FHeight*4;
      ReallocMem(FData, FDataSize);
   end;
end;

// GetScanLine
//
function TGLBitmap32.GetScanLine(index : Integer) : PGLPixel32Array;
begin
   Result:=PGLPixel32Array(@FData[index*Width]);
end;

// SetAlphaFromIntensity
//
procedure TGLBitmap32.SetAlphaFromIntensity;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=(Integer(r)+Integer(g)+Integer(b)) div 3;
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TColor);
var
   color : TGLPixel24;
begin
   color.r:=GetRValue(aColor);
   color.g:=GetGValue(aColor);
   color.b:=GetBValue(aColor);
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel32);
var
   color : TGLPixel24;
begin
   color.r:=aColor.r;
   color.g:=aColor.g;
   color.b:=aColor.b;
   SetAlphaTransparentForColor(color);
end;

// SetAlphaTransparentForColor
//
procedure TGLBitmap32.SetAlphaTransparentForColor(const aColor : TGLPixel24);
var
   i : Integer;
   intCol : Integer;
begin
   intCol:=(PInteger(@aColor)^) and $FFFFFF;
   for i:=0 to (FDataSize div 4)-1 do
      if PInteger(@FData[i])^ and $FFFFFF=intCol then
         FData[i].a:=0
      else FData[i].a:=255;
end;

// SetAlphaToValue
//
procedure TGLBitmap32.SetAlphaToValue(const aValue : Byte);
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=aValue
end;

// SetAlphaToFloatValue
//
procedure TGLBitmap32.SetAlphaToFloatValue(const aValue : Single);
begin
   SetAlphaToValue(Byte(Trunc(aValue*255) and 255));
end;

// InvertAlpha
//
procedure TGLBitmap32.InvertAlpha;
var
   i : Integer;
begin
   for i:=0 to (FDataSize div 4)-1 do
      FData[i].a:=255-FData[i].a;
end;

// SqrtAlpha
//
procedure TGLBitmap32.SqrtAlpha;
var
   i : Integer;
	sqrt255Array : PSqrt255Array;
begin
   sqrt255Array:=GetSqrt255Array;
   for i:=0 to (FDataSize div 4)-1 do with FData[i] do
      a:=sqrt255Array[(Integer(r)+Integer(g)+Integer(b)) div 3];
end;

// RegisterAsOpenGLTexture
//
procedure TGLBitmap32.RegisterAsOpenGLTexture(minFilter : TGLMinFilter;
                                              texFormat : Integer = GL_RGBA);
var
   w2, h2, maxSize : Integer;
   buffer : Pointer;
begin
   if DataSize>0 then begin
      w2:=RoundUpToPowerOf2(Width);
      h2:=RoundUpToPowerOf2(Height);
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
      if w2>maxSize then w2:=maxSize;
      if h2>maxSize then h2:=maxSize;
      if (w2<>Width) or (h2<>Height) then begin
         GetMem(buffer, w2*h2*4);
         gluScaleImage(GL_RGBA, Width, Height, GL_UNSIGNED_BYTE, Data, w2, h2,
                       GL_UNSIGNED_BYTE, buffer);
      end else buffer:=Pointer(FData);
      try
   		case minFilter of
			   miNearest, miLinear :
		   		glTexImage2d(GL_TEXTURE_2D, 0, texFormat, w2, h2, 0,
	   							 GL_RGBA, GL_UNSIGNED_BYTE, buffer)
   		else
		   	gluBuild2DMipmaps(GL_TEXTURE_2D, texFormat, w2, h2,
	   								GL_RGBA, GL_UNSIGNED_BYTE, buffer);
   		end;
		finally
         if buffer<>Pointer(FData) then
   			FreeMem(buffer);
		end;
   end;
end;

// ReadPixels
//
procedure TGLBitmap32.ReadPixels(const area : TRect);
begin
   FWidth:=(area.Right-area.Left) and $FFFC;
   FHeight:=(area.Bottom-area.Top);
   FDataSize:=FWidth*FHeight*4;
   ReallocMem(FData, FDataSize);
   glReadPixels(0, 0, FWidth, FHeight, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// DrawPixels
//
procedure TGLBitmap32.DrawPixels(const x, y : Single);
begin
   glRasterPos2f(x, y);
   glDrawPixels(Width, Height, GL_RGBA, GL_UNSIGNED_BYTE, FData);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

end.

