// GLBitmapFont
{: Bitmap Fonts management classes for GLScene<p>

	<b>History : </b><font size=-1><ul>
      <li>21/02/01 - Egg - Now XOpenGL based (multitexture)
	   <li>15/01/01 - EG - Creation
	</ul></font>
}
unit GLBitmapFont;

interface

uses Classes, GLScene, Graphics, Geometry, GLMisc, StdCtrls;

type

	// TBitmapFontRange
	//
   {: An individual character range in a bitmap font.<p>
      A range allows mapping ASCII characters to character tiles in a font
      bitmap, tiles are enumerated line then column (raster). }
	TBitmapFontRange = class (TCollectionItem)
	   private
	      { Private Declarations }
         FStartASCII, FStopASCII : Char;
         FStartGlyphIdx : Integer;

	   protected
	      { Protected Declarations }
         procedure SetStartASCII(const val : Char);
         procedure SetStopASCII(const val : Char);
         procedure SetStartGlyphIdx(const val : Integer);
         function GetDisplayName : String; override;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

	   published
	      { Published Declarations }
         property StartASCII : Char read FStartASCII write SetStartASCII;
         property StopASCII : Char read FStopASCII write SetStopASCII;
         property StartGlyphIdx : Integer read FStartGlyphIdx write SetStartGlyphIdx;
	end;

	// TBitmapFontRanges
	//
	TBitmapFontRanges = class (TCollection)
	   protected
	      { Protected Declarations }
	      owner : TComponent;
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TBitmapFontRange);
	      function GetItems(index : Integer) : TBitmapFontRange;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);
         function Add: TBitmapFontRange;
	      function FindItemID(ID: Integer): TBitmapFontRange;
	      property Items[index : Integer] : TBitmapFontRange read GetItems write SetItems; default;

         {: Converts an ASCII character into a tile index.<p>
            Return -1 if character cannot be rendered. }
	      function CharacterToTileIndex(aChar : Char) : Integer;
         procedure NotifyChange;
   end;

	// TBitmapFont
	//
   {: Provides access to individual characters in a BitmapFont.<p>
      Only fixed-width bitmap fonts are supported, the characters are enumerated
      in a raster fashion (line then column).<br>
      Transparency is all or nothing, the transparent color being that of the
      top left pixel of the Glyphs bitmap.<p>
      Performance note: as usual, for best performance, you base font bitmap
      dimensions should be close to a power of two, and have at least 1 pixel
      spacing between characters (horizontally and vertically) to avoid artefacts
      when rendering with linear filtering. }
	TBitmapFont = class (TComponent)
	   private
	      { Private Declarations }
         FRanges : TBitmapFontRanges;
         FGlyphs : TPicture;
         FCharWidth, FCharHeight : Integer;
         FGlyphsIntervalX, FGlyphsIntervalY : Integer;
         FHSpace, FVSpace : Integer;
         FUsers : TList;
         FHandle : Integer;
         FHandleIsDirty : Boolean;
			FMinFilter : TGLMinFilter;
			FMagFilter : TGLMagFilter;
         FTextureWidth, FTextureHeight : Integer;

	   protected
	      { Protected Declarations }
         procedure SetRanges(const val : TBitmapFontRanges);
         procedure SetGlyphs(const val : TPicture);
         procedure SetCharWidth(const val : Integer);
         procedure SetCharHeight(const val : Integer);
         procedure SetGlyphsIntervalX(const val : Integer);
         procedure SetGlyphsIntervalY(const val : Integer);
	      procedure OnGlyphsChanged(Sender : TObject);
         procedure SetHSpace(const val : Integer);
         procedure SetVSpace(const val : Integer);
			procedure SetMagFilter(AValue: TGLMagFilter);
			procedure SetMinFilter(AValue: TGLMinFilter);

	      procedure InvalidateUsers;
	      function CharactersPerRow : Integer;
	      procedure TileIndexToTexCoords(tileIndex : Integer; var topLeft, bottomRight : TTexPoint);
         procedure PrepareImage;
         procedure PrepareParams;

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

	      procedure RegisterUser(anObject : TGLBaseSceneObject);
	      procedure UnRegisterUser(anObject : TGLBaseSceneObject);
         {: Invoke to free all OpenGL list and handles eventually allocated. }
	      procedure FreeList(glsceneOnly : Boolean);

         {: Renders the given string at current position.<p>
            The current matrix is blindly used, meaning you can render all kinds
            of rotated and linear distorted text with this method. }
	      procedure RenderString(const aString : String; alignment : TAlignment;
                                layout : TTextLayout);

	   published
	      { Published Declarations }
         {: A single bitmap containing all the characters.<p>
            The transparent color is that of the top left pixel. }
         property Glyphs : TPicture read FGlyphs write SetGlyphs;
         {: Nb of horizontal pixels between two columns in the Glyphs. }
         property GlyphsIntervalX : Integer read FGlyphsIntervalX write SetGlyphsIntervalX;
         {: Nb of vertical pixels between two rows in the Glyphs. }
         property GlyphsIntervalY : Integer read FGlyphsIntervalY write SetGlyphsIntervalY;
         {: Ranges allow converting between ASCII and tile indexes.<p>
            See TBitmapFontRange. }
         property Ranges : TBitmapFontRanges read FRanges write SetRanges;

         {: Width of a single character. }
         property CharWidth : Integer read FCharWidth write SetCharWidth default 16;
         {: Height of a single character. }
         property CharHeight : Integer read FCharHeight write SetCharHeight default 16;
         {: Pixels in between rendered characters (horizontally). }
         property HSpace : Integer read FHSpace write SetHSpace default 1;
         {: Pixels in between rendered lines (vertically). }
         property VSpace : Integer read FVSpace write SetVSpace default 1;

			property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maLinear;
			property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miLinear;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLGraphics, XOpenGL;

// ------------------
// ------------------ TBitmapFontRange ------------------
// ------------------

// Create
//
constructor TBitmapFontRange.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

// Destroy
//
destructor TBitmapFontRange.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TBitmapFontRange.Assign(Source: TPersistent);
begin
	if Source is TBitmapFontRange then begin
      FStartASCII:=TBitmapFontRange(Source).FStartASCII;
      FStopASCII:=TBitmapFontRange(Source).FStopASCII;
      FStartGlyphIdx:=TBitmapFontRange(Source).FStartGlyphIdx;
	end;
	inherited Destroy;
end;

// GetDisplayName
//
function TBitmapFontRange.GetDisplayName : String;
begin
	Result:=Format('ASCII [#%d, #%d] -> Glyphs [%d, %d]',
                  [Integer(StartASCII), Integer(StopASCII), StartGlyphIdx,
                   StartGlyphIdx+Integer(StopASCII)-Integer(StartASCII)]);
end;

// SetStartASCII
//
procedure TBitmapFontRange.SetStartASCII(const val : Char);
begin
   FStartASCII:=val;
   if FStartASCII>FStopASCII then
      FStopASCII:=FStartASCII;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// SetStopASCII
//
procedure TBitmapFontRange.SetStopASCII(const val : Char);
begin
   FStopASCII:=val;
   if FStopASCII<FStartASCII then
      FStartASCII:=FStopASCII;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// SetStartGlyphIdx
//
procedure TBitmapFontRange.SetStartGlyphIdx(const val : Integer);
begin
   if val>=0 then
      FStartGlyphIdx:=val
   else FStartGlyphIdx:=0;
   TBitmapFontRanges(Collection).NotifyChange;
end;

// ------------------
// ------------------ TBitmapFontRanges ------------------
// ------------------

constructor TBitmapFontRanges.Create(AOwner : TComponent);
begin
	Owner:=AOwner;
	inherited Create(TBitmapFontRange);
end;

function TBitmapFontRanges.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

procedure TBitmapFontRanges.SetItems(index : Integer; const val : TBitmapFontRange);
begin
	inherited Items[index]:=val;
end;

function TBitmapFontRanges.GetItems(index : Integer) : TBitmapFontRange;
begin
	Result:=TBitmapFontRange(inherited Items[index]);
end;

function TBitmapFontRanges.Add: TBitmapFontRange;
begin
	Result:=(inherited Add) as TBitmapFontRange;
end;

function TBitmapFontRanges.FindItemID(ID: Integer): TBitmapFontRange;
begin
	Result:=(inherited FindItemID(ID)) as TBitmapFontRange;
end;

// CharacterToTileIndex
//
function TBitmapFontRanges.CharacterToTileIndex(aChar : Char) : Integer;
var
   i : Integer;
begin
   Result:=-1;
   for i:=0 to Count-1 do with Items[i] do
      if (aChar>=StartASCII) and (aChar<=StopASCII) then begin
         Result:=StartGlyphIdx+Integer(aChar)-Integer(StartASCII);
         Break;
      end;
end;

// NotifyChange
//
procedure TBitmapFontRanges.NotifyChange;
begin
   if Assigned(Owner) and (Owner is TGLBaseSceneObject) then
      TGLBaseSceneObject(Owner).StructureChanged;
end;

// ------------------
// ------------------ TBitmapFont ------------------
// ------------------

// Creat
//
constructor TBitmapFont.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FRanges:=TBitmapFontRanges.Create(Self);
   FGlyphs:=TPicture.Create;
   FGlyphs.OnChange:=OnGlyphsChanged;
   FCharWidth:=16;
   FCharHeight:=16;
   FHSpace:=1;
   FVSpace:=1;
   FUsers:=TList.Create;
   FHandleIsDirty:=True;
   FMinFilter:=miLinear;
   FMagFilter:=maLinear;
end;

// Destroy
//
destructor TBitmapFont.Destroy;
begin
	inherited Destroy;
   FreeList(True);
   FRanges.Free;
   FGlyphs.Free;
   Assert(FUsers.Count=0);
   FUsers.Free;
end;

// SetRanges
//
procedure TBitmapFont.SetRanges(const val : TBitmapFontRanges);
begin
   FRanges.Assign(val);
end;

// SetGlyphs
//
procedure TBitmapFont.SetGlyphs(const val : TPicture);
begin
   FGlyphs.Assign(val);
end;

// SetCharWidth
//
procedure TBitmapFont.SetCharWidth(const val : Integer);
begin
   if val>1 then
      FCharWidth:=val
   else FCharWidth:=1;
   InvalidateUsers;
end;

// SetCharHeight
//
procedure TBitmapFont.SetCharHeight(const val : Integer);
begin
   if val>1 then
      FCharHeight:=val
   else FCharHeight:=1;
   InvalidateUsers;
end;

// SetGlyphsIntervalX
//
procedure TBitmapFont.SetGlyphsIntervalX(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalX:=val
   else FGlyphsIntervalX:=0;
   InvalidateUsers;
end;

// SetGlyphsIntervalY
//
procedure TBitmapFont.SetGlyphsIntervalY(const val : Integer);
begin
   if val>0 then
      FGlyphsIntervalY:=val
   else FGlyphsIntervalY:=0;
   InvalidateUsers;
end;

// SetHSpace
//
procedure TBitmapFont.SetHSpace(const val : Integer);
begin
   if val>0 then
      FHSpace:=val
   else FHSpace:=0;
   InvalidateUsers;
end;

// SetVSpace
//
procedure TBitmapFont.SetVSpace(const val : Integer);
begin
   if val>0 then
      FVSpace:=val
   else FVSpace:=0;
   InvalidateUsers;
end;

// SetMagFilter
//
procedure TBitmapFont.SetMagFilter(AValue: TGLMagFilter);
begin
	if AValue <> FMagFilter then begin
		FMagFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// SetMinFilter
//
procedure TBitmapFont.SetMinFilter(AValue: TGLMinFilter);
begin
	if AValue <> FMinFilter then begin
		FMinFilter:=AValue;
      FHandleIsDirty:=True;
      InvalidateUsers;
	end;
end;

// OnGlyphsChanged
//
procedure TBitmapFont.OnGlyphsChanged(Sender : TObject);
begin
   InvalidateUsers;
end;

// RegisterUser
//
procedure TBitmapFont.RegisterUser(anObject : TGLBaseSceneObject);
begin
   Assert(FUsers.IndexOf(anObject)<0);
   FUsers.Add(anObject);
end;

// UnRegisterUser
//
procedure TBitmapFont.UnRegisterUser(anObject : TGLBaseSceneObject);
begin
   FUsers.Remove(anObject);
end;

// FreeList
//
procedure TBitmapFont.FreeList(glsceneOnly : Boolean);
begin
	if FHandle<>0 then begin
      if not glsceneOnly then begin
         Assert(CurrentRenderingContextDC>0);
 	   	glDeleteTextures(1, @FHandle);
      end;
      FHandle:=0;
      FHandleIsDirty:=True;
   end;
end;

// PrepareImage
//
procedure TBitmapFont.PrepareImage;
var
   bitmap : TBitmap;
   bitmap32 : TGLBitmap32;
begin
   bitmap:=TBitmap.Create;
   with bitmap do begin
      PixelFormat:=pf24bit;
      Width:=RoundUpToPowerOf2(Glyphs.Width);
      Height:=RoundUpToPowerOf2(Glyphs.Height);
      Canvas.Draw(0, 0, Glyphs.Graphic);
   end;
   bitmap32:=TGLBitmap32.Create;
   bitmap32.Assign(bitmap);
   bitmap.Free;
   with bitmap32 do begin
      SetAlphaTransparentForColor(Data[Width*(Height-1)]);
      RegisterAsOpenGLTexture(MinFilter);
      FTextureWidth:=Width;
      FTextureHeight:=Height;
      Free;
   end;
end;

// PrepareParams
//
procedure TBitmapFont.PrepareParams;
const
	cTextureMagFilter : array [maNearest..maLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR );
	cTextureMinFilter : array [miNearest..miLinearMipmapLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
							  GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
							  GL_LINEAR_MIPMAP_LINEAR );
begin
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);
end;

// RenderString
//
procedure TBitmapFont.RenderString(const aString : String; alignment : TAlignment;
                                   layout : TTextLayout);

   function AlignmentAdjustement(p : Integer) : Single;
   var
      i : Integer;
   begin
      i:=0;
      while (p<=Length(aString)) and (aString[p]<>#13) do begin
         Inc(p); Inc(i);
      end;
      case alignment of
         taLeftJustify : Result:=0;
         taRightJustify : Result:=-(i*(CharWidth+HSpace)-HSpace)
      else // taCenter
          Result:=-(i*(CharWidth+HSpace)-HSpace)/2;
      end;
   end;

   function LayoutAdjustement : Single;
   var
      i, n : Integer;
   begin
      n:=1;
      for i:=1 to Length(aString) do
         if aString[i]=#13 then Inc(n);
      case layout of
         tlTop : Result:=0;
         tlBottom : Result:=(n*(CharHeight+VSpace)-VSpace);
      else // tlCenter
         Result:=(n*(CharHeight+VSpace)-VSpace)/2;
      end;
   end;

var
   i, idx : Integer;
   topLeft, bottomRight : TTexPoint;
   vTopLeft, vBottomRight : TVector;
   deltaH, deltaV : Single;
begin
   if (Glyphs.Width=0) or (aString='') then Exit;
   // prepare texture if necessary
   if FHandleIsDirty then begin
      // prepare handle
      if FHandle = 0 then begin
         glGenTextures(1, @FHandle);
         Assert(FHandle<>0);
      end;
      glBindtexture(GL_TEXTURE_2D, FHandle);
      // texture registration
      if Glyphs.Width<>0 then begin
         PrepareImage;
         PrepareParams;
      end;
      FHandleIsDirty:=False;
   end;
   // precalcs
   MakePoint(vTopLeft, AlignmentAdjustement(1),  LayoutAdjustement, 0);
   MakePoint(vBottomRight, vTopLeft[0]+CharWidth-1, vTopLeft[1]-(CharHeight-1), 0);
   deltaH:=CharWidth+HSpace;
   deltaV:=-(CharHeight+VSpace);
   // set states
	glEnable(GL_TEXTURE_2D);
   glDisable(GL_LIGHTING);
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glBindTexture(GL_TEXTURE_2D, FHandle);
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
   // start rendering
   glBegin(GL_QUADS);
   for i:=1 to Length(aString) do begin
      case aString[i] of
         #0..#12, #14..#31 : ; // ignore
         #13 : begin
            vTopLeft[0]:=AlignmentAdjustement(i+1);
            vTopLeft[1]:=vTopLeft[1]+deltaV;
            vBottomRight[0]:=vTopLeft[0]+CharWidth-1;
            vBottomRight[1]:=vBottomRight[1]+deltaV;
         end
      else
         idx:=Ranges.CharacterToTileIndex(aString[i]);
         if idx>0 then begin
            TileIndexToTexCoords(idx, topLeft, bottomRight);

            xglTexCoord2fv(@topLeft);
            glVertex4fv(@vTopLeft);

            xglTexCoord2f(topLeft.S, bottomRight.T);
            glVertex2f(vTopLeft[0], vBottomRight[1]);

            xglTexCoord2fv(@bottomRight);
            glVertex4fv(@vBottomRight);

            xglTexCoord2f(bottomRight.S, topLeft.T);
            glVertex2f(vBottomRight[0], vTopLeft[1]);
         end;
         vTopLeft[0]:=vTopLeft[0]+deltaH;
         vBottomRight[0]:=vBottomRight[0]+deltaH;
      end;
   end;
   glEnd;
end;

// CharactersPerRow
//
function TBitmapFont.CharactersPerRow : Integer;
begin
   if FGlyphs.Width>0 then
   	Result:=(FGlyphs.Width+FGlyphsIntervalX) div (FGlyphsIntervalX+FCharWidth)
   else Result:=0;
end;

// TileIndexToTexCoords
//
procedure TBitmapFont.TileIndexToTexCoords(tileIndex : Integer;
                                           var topLeft, bottomRight : TTexPoint);
var
   carX, carY : Integer;
begin
   carX:=(tileIndex mod CharactersPerRow)*(CharWidth+GlyphsIntervalX);
   carY:=(tileIndex div CharactersPerRow)*(CharHeight+GlyphsIntervalY);
   topLeft.S:=(carX+0.05)/FTextureWidth;
   topLeft.T:=FTextureHeight-(carY+0.05)/FTextureHeight;
   bottomRight.S:=(carX+CharWidth-1.05)/FTextureWidth;
   bottomRight.T:=FTextureHeight-(carY+CharHeight-1.05)/FTextureHeight;
end;

// InvalidateUsers
//
procedure TBitmapFont.InvalidateUsers;
var
   i : Integer;
begin
   for i:=FUsers.Count-1 downto 0 do
      TGLBaseSceneObject(FUsers[i]).NotifyChange(Self);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([TBitmapFont]);

end.

