{: GLMisc<p>

   Object with support for complex polygons.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>21/02/01 - Egg - Now XOpenGL based (multitexture)
      <li>08/01/01 - Egg - Compatibility fix (TGLLineNodes change),
                           Delphi 4 compatibility (removed TVectorPool) and
                           added/renamed some properties, various fixes
      <li>08/10/00 - Egg - Added header, code contributed by Uwe Raabe
   </ul>
}
unit GLMultiPolygon;

interface

uses
   Classes, OpenGL12, Geometry, GLScene, GLObjects, GLMisc, GLTexture;

type

   // TMultiPolygon
   //
   {: A polygon that can have holes and multiple contours.<p>
      Use the Contour property to access a contour or one of the AddNode methods
      to add a node to a contour (contours are allocated automatically). }
   TMultiPolygon = class (TGLSceneObject)
      private
         { Private Declarations }
         FParts: TPolygonParts;
         FContours : array of TGLNodes;

      protected
         { Protected Declarations }
         function GetContour(i : Integer): TGLNodes;
         procedure SetContour(i : Integer; const Value: TGLNodes);
         procedure SetParts(const value : TPolygonParts);
         procedure RenderTesselatedPolygon(textured : Boolean;
                              normal : PAffineVector; invertNormals: Boolean);
         function GetContourCount : Integer;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure AddNode(const i : Integer; const coords : TGLCoordinates); overload;
         procedure AddNode(const i : Integer; const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const i : Integer; const value : TVector); overload;
         procedure AddNode(const i : Integer; const value : TAffineVector); overload;

         property Contours[i : Integer] : TGLNodes read GetContour write SetContour;
         property ContourCount : Integer read GetContourCount;
         procedure Clear;

      published
         { Published Declarations }
         property Parts : TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
   end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses SysUtils, VectorLists, XOpenGL;

// ------------------
// ------------------ TMultiPolygon ------------------
// ------------------

// Create
//
constructor TMultiPolygon.Create(AOwner: TComponent);
begin
   inherited;
   FParts:=[ppTop, ppBottom];
end;

// Destroy
//
destructor TMultiPolygon.Destroy;
begin
   Clear;
   inherited;
end;

// Assign
//
procedure TMultiPolygon.Assign(Source: TPersistent);
var
   i : Integer;
begin
   if Source is TMultiPolygon then begin
      Clear;
      for i:=Low(TMultiPolygon(Source).FContours) to High(TMultiPolygon(Source).FContours) do
         Contours[i]:=TMultiPolygon(Source).FContours[i];
      FParts:=TMultiPolygon(Source).FParts;
  end;
  inherited;
end;

// AddNode (vector)
//
procedure TMultiPolygon.AddNode(const i : Integer; const value : TVector);
begin
   Contours[i].Add.AsVector:=value;
end;

// AddNode (float)
//
procedure TMultiPolygon.AddNode(const i : Integer; const x, y, z : TGLfloat);
begin
   Contours[i].Add.AsVector:=PointMake(x, y, z);
end;

// AddNode (coords)
//
procedure TMultiPolygon.AddNode(const i : Integer; const coords : TGLCoordinates);
begin
   Contours[i].Add.AsVector:=coords.AsVector;
end;

// AddNode (affine vector)
//
procedure TMultiPolygon.AddNode(const I: Integer; const value: TAffineVector);
begin
   Contours[i].Add.AsVector:=VectorMake(value);
end;

// Clear
//
procedure TMultiPolygon.Clear;
var
   i : Integer;
begin
   if Assigned(FContours) then for i:=Low(FContours) to High(FContours) do
      FContours[i].Free;
end;

// BuildList
//
procedure TMultiPolygon.BuildList(var rci: TRenderContextInfo);
var
   normal : TAffineVector;
   p : PAffineVector;
   n : Integer;
begin
   if ContourCount<1 then Exit;
   // find normal vector to contour[0]
   p:=nil;
   for n:=2 to Contours[0].Count-1 do begin
      CalcPlaneNormal(Contours[0].Vector(0), Contours[0].Vector(1),
                      Contours[0].Vector(N), normal);
      if VectorNorm(normal)>0 then begin
         NormalizeVector(normal);
         p:=@normal;
         Break;
      end;
   end;
   // Render
   if Assigned(p) then begin
      // tessellate top polygon
      if ppTop in FParts then
         RenderTesselatedPolygon(True, p, False);
      // tessellate bottom polygon
      if ppBottom in FParts then begin
         ScaleVector(normal, -1);
         RenderTesselatedPolygon(True, p, True)
      end;
   end;
end;

// GetContourCount
//
function TMultiPolygon.GetContourCount : Integer;
begin
   if Assigned(FContours) then
      Result:=High(FContours)+1
   else Result:=0;
end;

// GetContour
//
function TMultiPolygon.GetContour(i : Integer): TGLNodes;
var
   k : Integer;
begin
   Assert(i>=0);
   if (not Assigned(FContours)) or (i>High(FContours)) then begin
      k:=High(FContours)+1;
      SetLength(FContours, i+1);
      while k<=i do begin
         FContours[k]:=TGLNodes.Create(Self);
         Inc(k);
      end;
   end;
   Result:=FContours[i];
end;

// SetContour
//
procedure TMultiPolygon.SetContour(i : Integer; const value: TGLNodes);
begin
   Contours[i].Assign(value);
end;

// SetParts
//
procedure TMultiPolygon.SetParts(const value : TPolygonParts);
begin
   if FParts<>value then begin
      FParts:=value;
      StructureChanged;
   end;
end;

//
// Tessellation routines (OpenGL callbacks)
//

var
   vVertexPool : TAffineVectorList;

procedure tessError(errno : TGLEnum); stdcall;
begin
   Assert(False, IntToStr(errno)+' : '+gluErrorString(errno));
end;

procedure tessIssueVertex(vertexData : Pointer); stdcall;
begin
   xglTexCoord2fv(vertexData);
   glVertex3fv(vertexData);
end;

procedure tessCombine(coords : PDoubleVector; vertex_data : Pointer;
                      weight : PGLFloat; var outData : Pointer); stdcall;
var
   i : Integer;
begin
   i:=vVertexPool.Add(coords[0], coords[1], coords[2]);
   outData:=@vVertexPool.List[i];
end;

// RenderTesselatedPolygon
//
procedure TMultiPolygon.RenderTesselatedPolygon(textured : Boolean;
                                                normal : PAffineVector;
                                                invertNormals : Boolean);
var
   i,n : Integer;
   tess : PGLUTesselator;
   dblVector : TAffineDblVector;
begin
   if (ContourCount>0) and (FContours[0].Count>2) then begin
      // Vertex count
      n:=0;
      for i:=Low(FContours) to High(FContours) do
         n:=n+FContours[i].Count;
      // Create and initialize the GLU tesselator
      vVertexPool:=TAffineVectorList.Create;
      vVertexPool.Capacity:=4*n;
      tess:=gluNewTess;
      try
         gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
         if textured then
            gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
         else gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
         gluTessCallback(tess, GLU_TESS_END, @glEnd);
         gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
         gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
         // Issue normal
         if Assigned(normal) then begin
            glNormal3fv(PGLFloat(normal));
            gluTessNormal(tess, normal[0], normal[1], normal[2]);
         end;
         gluTessProperty(Tess,GLU_TESS_WINDING_RULE,GLU_TESS_WINDING_POSITIVE);
         // Issue polygon
         gluTessBeginPolygon(tess, nil);
         for n:=Low(FContours) to High(FContours) do begin
            gluTessBeginContour(tess);
            if invertNormals xor (n>0) then begin
               for i:=FContours[n].Count-1 downto 0 do begin
                  SetVector(dblVector, PAffineVector(FContours[n].Items[i].AsAddress)^);
                  gluTessVertex(tess, dblVector, FContours[n].Items[i].AsAddress);
               end;
            end else begin
               for i:=0 to FContours[n].Count-1 do begin
                  SetVector(dblVector, PAffineVector(FContours[n].Items[i].AsAddress)^);
                  gluTessVertex(tess, dblVector, FContours[n].Items[i].AsAddress);
               end;
            end;
            gluTessEndContour(tess);
         end;
         gluTessEndPolygon(tess);
      finally
         gluDeleteTess(tess);
         vVertexPool.Free;
         vVertexPool:=nil;
      end;
   end;
end;

end.
