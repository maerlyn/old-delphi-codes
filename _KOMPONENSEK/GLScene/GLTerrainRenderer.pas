// GLTerrainRenderer
{: GLScene's brute-force terrain renderer.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>04/03/01 - Egg - Completed for first release
	   <li>12/02/01 - Egg - Creation
	</ul></font>
}
unit GLTerrainRenderer;

interface

uses Classes, GLScene, GLHeightData, GLTexture, Geometry;

type

	// TTerrainRenderer
	//
   {: Basic terrain renderer.<p>
      This renderer uses no sophisticated meshing, it just builds and maintains
      a set of terrain tiles, performs basic visibility culling and renders its
      stuff. You can use it has a base class/sample for more specialized
      terrain renderers.<p>
      The Terrain heightdata is retrieved directly from a THeightDataSource, and
      expressed as z=f(x, y) data. }
	TTerrainRenderer = class (TGLSceneObject)
	   private
	      { Private Declarations }
         FHeightDataSource : THeightDataSource;
         FTileSize : Integer;
         FQualityDistance : Single;
         FLastTriangleCount : Integer;
         FTilesPerTexture : Single;

	   protected
	      { Protected Declarations }
         FTiles : TList;

         procedure SetHeightDataSource(const val : THeightDataSource);
         procedure SetTileSize(const val : Integer);
         procedure SetTilesPerTexture(const val : Single);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure DoDestroyList(glsceneOnly : Boolean); override;

         procedure ReleaseAllTiles; dynamic;
         procedure OnTileDestroyed(sender : TObject); virtual;

         {: Render the best fitting tile for tilePos.<p>
            tilePos is in *local* coordinates }
         procedure RenderTile(const tilePos : TAffineVector; eyeDistance : Single); virtual;

         {: Renders a THeightData as a quad, axis-aligned tile.<p>
            The tile is rendered with a triangle strips. }
         procedure RenderTileAsTriangleStrip(aTile : THeightData;
                       const leftTop, scale, texLeftTop, texScale : TAffineVector);
         {: Renders a THeightData as a quad, axis-aligned tile.<p>
            The tile is rendered with a single triangle fans (center to edges). }
         procedure RenderTileAsTriangleFan(aTile : THeightData;
                       const leftTop, scale, texLeftTop, texScale : TAffineVector);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

			procedure BuildList(var rci : TRenderContextInfo); override;

         {: Interpolates height for the given point.<p>
            Expects a point expressed in absolute coordinates. }
         function InterpolatedHeight(const p : TVector) : Single; virtual;

         property LastTriangleCount : Integer read FLastTriangleCount;

	   published
	      { Published Declarations }

         {: Specifies the HeightData provider component. }
         property HeightDataSource : THeightDataSource read FHeightDataSource write SetHeightDataSource;
         {: Size of the terrain tiles.<p>
            Must be a power of two. }
         property TileSize : Integer read FTileSize write SetTileSize default 16;
         {: Number of tiles required for a full texture map. }
         property TilesPerTexture : Single read FTilesPerTexture write SetTilesPerTexture;

         {: Quality distance hint.<p>
            This parameter gives an hint to the terrain renderer at which distance
            the terrain quality can be degraded to favor speed. The distance is
            expressed in absolute coordinates units.<p>
            A value of 0 (default) should be interpreted as "the highest quality". }
         property QualityDistance : Single read FQualityDistance write FQualityDistance;
	end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL12, GLMisc, Graphics, XOpenGL;

// ------------------
// ------------------ TTerrainRenderer ------------------
// ------------------

// Create
//
constructor TTerrainRenderer.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FTiles:=TList.Create;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FTileSize:=16;
   FTilesPerTexture:=1;
end;

// Destroy
//
destructor TTerrainRenderer.Destroy;
begin
   ReleaseAllTiles;
   FTiles.Free;
	inherited Destroy;
end;

// Notification
//
procedure TTerrainRenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FHeightDataSource) then
      HeightDataSource:=nil;
   inherited;
end;

// DoDestroyList
//
procedure TTerrainRenderer.DoDestroyList(glsceneOnly : Boolean);
begin
   inherited;
   ReleaseAllTiles;
   if Assigned(HeightDataSource) then
      HeightDataSource.Clear;
end;

// ReleaseAllTiles
//
procedure TTerrainRenderer.ReleaseAllTiles;
var
   i : Integer;
   hd : THeightData;
begin
   if FTiles.Count=0 then Exit;
   for i:=0 to FTiles.Count-1 do begin
      hd:=THeightData(FTiles[i]);
      hd.Release;
   end;
   FTiles.Clear;
end;

// OnTileDestroyed
//
procedure TTerrainRenderer.OnTileDestroyed(sender : TObject);
var
   activated : Boolean;
begin
   if CurrentRC=0 then begin
      try
         Scene.ActivateDefaultRenderingContext;
         activated:=True;
      except
         activated:=False;
      end;
   end else activated:=False;
   try
      with sender as THeightData do if Tag>0 then begin
         if CurrentRC<>0 then
            glDeleteLists(Tag, 1);
         Tag:=0;
      end;
   finally
      if activated and (CurrentRC<>0) then
         DeactivateRenderingContext;
   end;
end;

// InterpolatedHeight
//
function TTerrainRenderer.InterpolatedHeight(const p : TVector) : Single;
var
   pLocal : TVector;
begin
   if Assigned(HeightDataSource) then begin
      pLocal:=VectorTransform(p, InvAbsoluteMatrix);
      Result:=(HeightDataSource.InterpolatedHeight(pLocal[0], pLocal[1])-128)*Scale.Z;
   end else Result:=0;
end;

// BuildList
//
procedure TTerrainRenderer.BuildList(var rci : TRenderContextInfo);
var
   vEye : TVector;
   tilePos, absTilePos : TAffineVector;
   delta, n : Integer;
   f, tileRadius : Single;
begin
   f:=1/(TilesPerTexture*TileSize);
   Material.Texture.InitAutoTexture(TexPointMake(f, -f));
   if csDesigning in ComponentState then Exit;
   if HeightDataSource=nil then Exit;
   // first project eye position into heightdata coordinates
   vEye:=VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
   vEye[0]:=Round(vEye[0]/(TileSize-3)-0.5)*(TileSize-3)+(TileSize-3)*0.5;
   vEye[1]:=Round(vEye[1]/(TileSize-3)-0.5)*(TileSize-3)+(TileSize-3)*0.5;
   tileRadius:=Sqrt(Sqr(TileSize*0.5*Scale.X)+Sqr(TileSize*0.5*Scale.Y)+Sqr(128*Scale.Z))*1.2;
   // mark all tiles as unused
   for n:=0 to FTiles.Count-1 do
      THeightData(FTiles[n]).ObjectTag:=nil;
   FLastTriangleCount:=0;
   // now, we render a quad centered on eye position
   SetVector(tilePos, vEye);
   delta:=TileSize-3;
   tilePos[2]:=0;
   f:=(rci.rcci.farClippingDistance+tileRadius)/Scale.X;
   f:=Round(f/(TileSize-3)+0.5)*(TileSize-3);
   tilePos[1]:=vEye[1]-f;
   while tilePos[1]<=vEye[1]+f do begin
      tilePos[0]:=vEye[0]-f;
      while tilePos[0]<=vEye[0]+f do begin
         absTilePos:=VectorTransform(tilePos, AbsoluteMatrix);
         if not IsVolumeClipped(absTilePos, tileRadius, rci.rcci) then
            RenderTile(tilePos, VectorDistance(absTilePos, AffineVectorMake(rci.cameraPosition)));
         tilePos[0]:=tilePos[0]+delta;
      end;
      tilePos[1]:=tilePos[1]+delta;
   end;
   // release all unused tiles
   for n:=FTiles.Count-1 downto 0 do
      if THeightData(FTiles[n]).ObjectTag=nil then begin
         THeightData(FTiles[n]).Release;
         FTiles.Delete(n);
      end;
   Material.Texture.DisableAutoTexture;
end;

// RenderTile
//
procedure TTerrainRenderer.RenderTile(const tilePos : TAffineVector; eyeDistance : Single);
var
   i : Integer;
   hd, tile : THeightData;
   xLeft, yTop : Integer;
begin
   xLeft:=Round(tilePos[0]/(TileSize-3)-0.5)*(TileSize-3);
   yTop:=Round(tilePos[1]/(TileSize-3)-0.5)*(TileSize-3);
   // is the tile already in our list?
   tile:=nil;
   for i:=0 to FTiles.Count-1 do begin
      hd:=THeightData(FTiles[i]);
      if (hd.XLeft=xLeft) and (hd.YTop=yTop) then begin
         tile:=hd;
         Break;
      end;
   end;
   // if not, request it
   if not Assigned(tile) then begin
      tile:=HeightDataSource.GetData(xLeft, yTop, TileSize, hdtByte);
      tile.OnDestroy:=OnTileDestroyed;
      FTiles.Add(tile);
   end;
   // build/rebuild list
   if (QualityDistance>0) and (tile.Tag<>0) then begin
      if (((eyeDistance<QualityDistance) and (tile.Tag2=0))
          or ((eyeDistance>=QualityDistance) and (tile.Tag2=1))) then begin
         glDeleteLists(tile.Tag, 1);
         tile.Tag:=0;
      end;
   end;
   if tile.Tag=0 then begin
      tile.Tag:=glGenLists(1);
      glNewList(tile.Tag, GL_COMPILE);
      if (eyeDistance<QualityDistance) or (QualityDistance<=0) then begin
         tile.Tag2:=1;
         RenderTileAsTriangleStrip(tile,
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1),
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1));
      end else begin
         tile.Tag2:=0;
         RenderTileAsTriangleFan(tile,
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1),
            AffineVectorMake(xLeft, yTop, 0), AffineVectorMake(1, 1, 1));
      end;
      glEndList;
      glCallList(tile.Tag);
   end;
   // start rendering
   glCallList(tile.Tag);
   if tile.Tag2=1 then
      Inc(FLastTriangleCount, 2*(tile.Size-3)*(tile.Size-3))
   else Inc(FLastTriangleCount, 4*(tile.Size-2)-2);
   // mark tile as used
   tile.ObjectTag:=Pointer(1);
end;

// RenderTileAsTriangleStrip
//
procedure TTerrainRenderer.RenderTileAsTriangleStrip(aTile : THeightData;
              const leftTop, scale, texLeftTop, texScale : TAffineVector);
var
   x, y : Integer;
   pTop, pBottom : TAffineVector;
   n : TAffineVector;
   bottomRow, topRow : GLHeightData.PByteArray;
begin
   // to optimize : normals calculation is slooooowwww
   // the cacheing takes care of it, but still...
   for y:=1 to aTile.Size-3 do begin
      pTop[1]:=leftTop[1]+y*scale[1];
      pBottom[1]:=leftTop[1]+(y+1)*scale[1];
      bottomRow:=aTile.ByteRaster[y+1];
      topRow:=aTile.ByteRaster[y];
      glBegin(GL_TRIANGLE_STRIP);
      for x:=1 to aTile.Size-2 do begin
         pTop[0]:=leftTop[0]+x*scale[0];
         pBottom[0]:=pTop[0];
         pBottom[2]:=(bottomRow[x]-128)*scale[2];
         n:=aTile.Normal(x, y+1, scale);
         glNormal3fv(@n);
         glVertex3fv(@pBottom);
         pTop[2]:=(topRow[x]-128)*scale[2];
         n:=aTile.Normal(x, y, scale);
         glNormal3fv(@n);
         glVertex3fv(@pTop);
      end;
      glEnd;
   end;
end;

// RenderTileAsTriangleFan
//
procedure TTerrainRenderer.RenderTileAsTriangleFan(aTile : THeightData;
              const leftTop, scale, texLeftTop, texScale : TAffineVector);
var
   x, y : Integer;
   p : TAffineVector;
   n : TAffineVector;
   row : GLHeightData.PByteArray;
begin
   // to optimize : normals calculation is slooooowwww
   // the cacheing takes care of it, but still...
   x:=(aTile.Size-2) div 2;
   p[0]:=leftTop[0]+x*scale[0];
   p[1]:=leftTop[1]+x*scale[1];
   p[2]:=(aTile.ByteRaster[x][x]-128)*scale[2];
   n:=aTile.Normal(x, x, scale);
   glBegin(GL_TRIANGLE_FAN);
      glNormal3fv(@n);
      glVertex3fv(@p);
      p[1]:=leftTop[1]+1*scale[0];
      row:=aTile.ByteRaster[1];
      for x:=1 to aTile.Size-2 do begin
         p[0]:=leftTop[0]+x*scale[0];
         p[2]:=(row[x]-128)*scale[2];
         n:=aTile.Normal(x, 1, scale);
         glNormal3fv(@n);
         glVertex3fv(@p);
      end;
      for y:=2 to aTile.Size-2 do begin
         p[1]:=leftTop[1]+y*scale[1];
         p[2]:=(aTile.ByteRaster[y][aTile.Size-2]-128)*scale[2];
         n:=aTile.Normal(aTile.Size-2, y, scale);
         glNormal3fv(@n);
         glVertex3fv(@p);
      end;
      row:=aTile.ByteRaster[aTile.Size-2];
      for x:=aTile.Size-3 downto 1 do begin
         p[0]:=leftTop[0]+x*scale[0];
         p[2]:=(row[x]-128)*scale[2];
         n:=aTile.Normal(x, aTile.Size-2, scale);
         glNormal3fv(@n);
         glVertex3fv(@p);
      end;
      for y:=aTile.Size-3 downto 1 do begin
         p[1]:=leftTop[1]+y*scale[1];
         p[2]:=(aTile.ByteRaster[y][1]-128)*scale[2];
         n:=aTile.Normal(1, y, scale);
         glNormal3fv(@n);
         glVertex3fv(@p);
      end;
   glEnd;
end;

// SetHeightDataSource
//
procedure TTerrainRenderer.SetHeightDataSource(const val : THeightDataSource);
begin
   if FHeightDataSource<>val then begin
      FHeightDataSource:=val;
      ReleaseAllTiles;
      StructureChanged;
   end;
end;

// SetTileSize
//
procedure TTerrainRenderer.SetTileSize(const val : Integer);
begin
   if val<>FTileSize then begin
      if val<8 then
         FTileSize:=8
      else FTileSize:=RoundUpToPowerOf2(val);
      ReleaseAllTiles;
      StructureChanged;
   end;
end;

// SetTilesPerTexture
//
procedure TTerrainRenderer.SetTilesPerTexture(const val : Single);
begin
   if val<>FTilesPerTexture then begin
      FTilesPerTexture:=val;
      StructureChanged;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TTerrainRenderer);

end.

