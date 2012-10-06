{: GLPortal<p>

	Portal Rendering support for GLScene.<p>

   The portal structures are subclasses of the Mesh structures, with a "sector"
   being assimilated to a "MeshObject" and sector polygons to facegroups.<p>

	<b>Historique : </b><font size=-1><ul>
	   <li>13/08/00 - Egg - Creation
	</ul></font>
}
unit GLPortal;

interface

uses Classes, GLVectorFileObjects, GLScene, GLTexture, GLMisc, Geometry;

type

   // TPortalMeshObjectList
   //
   {: A mesh object list that handles portal rendering.<p>
      The items are treated as being sectors. } 
   TPortalMeshObjectList = class (TMeshObjectList)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner : TPersistent);
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
   end;


   // TSectorMeshObject
   //
   {: A portal renderer sector.<p> }
   TSectorMeshObject = class (TMorphableMeshObject)
      private
         { Private Declarations }
         FRenderDone : Boolean;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner : TMeshObjectList); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;
         procedure Prepare; override;

         property RenderDone : Boolean read FRenderDone write FRenderDone;
   end;

	// TFGPolygon
	//
   {: A portal polygon.<p>
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPolygon = class (TFGVertexNormalTexIndexList)
	   private
	      { Private Declarations }

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure Prepare; override;
	end;

	// TFGPolygon
	//
   {: A portal polygon.<p>
      This is the base class for portal polygons, the TFGPortalPolygon class
      implements the portal. }
	TFGPortalPolygon = class (TFGPolygon)
	   private
	      { Private Declarations }
         FDestinationSectorIndex : Integer;
         FCenter, FNormal : TAffineVector;
         FRadius : Single;

	   protected
	      { Protected Declarations }

	   public
	      { Public Declarations }
	      constructor Create(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         procedure Prepare; override;

         property DestinationSectorIndex : Integer read FDestinationSectorIndex write FDestinationSectorIndex;
	end;

   // TPortal
   //
   {: Portal Renderer class. }
   TPortal = class(TBaseMesh)
      private
         { Private Declarations }

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         { Published Declarations }
         property MaterialLibrary;
    end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils;

// ------------------
// ------------------ TPortalMeshObjectList ------------------
// ------------------

// Create
//
constructor TPortalMeshObjectList.Create(AOwner : TPersistent);
begin
   inherited;
end;

// Destroy
//
destructor TPortalMeshObjectList.Destroy;
begin
   inherited;
end;

// BuildList
//
procedure TPortalMeshObjectList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   startSector : TMeshObject;
begin
   for i:=0 to Count-1 do with TSectorMeshObject(Items[i]) do
      if InheritsFrom(TSectorMeshObject) then RenderDone:=False;
   startSector:=nil;
   for i:=0 to Count-1 do begin
      if Items[i].PointInObject(PAffineVector(@mrci.cameraPosition)^) then begin
         startSector:=Items[i];
         Break;
      end;
   end;
   if startSector<>nil then
      startSector.BuildList(mrci)
   else for i:=0 to Count-1 do Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TSectorMeshObject ------------------
// ------------------

// Create
//
constructor TSectorMeshObject.Create(AOwner : TMeshObjectList);
begin
	inherited;
   Mode:=momFaceGroups;
end;

// Destroy
//
destructor TSectorMeshObject.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TSectorMeshObject.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   if not RenderDone then begin
      RenderDone:=True;
      // single pass : portals/polygons were sorted earlier
      if Assigned(mrci.materialLibrary) then begin
         for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
            if Length(MaterialName)>0 then begin
               libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
               if Assigned(libMat) then begin
                  libMat.Apply(mrci);
                  BuildList(mrci);
                  libMat.UnApply(mrci);
               end else BuildList(mrci);
            end else BuildList(mrci);
         end;
      end else for i:=0 to FaceGroups.Count-1 do
         FaceGroups[i].BuildList(mrci);
   end;
end;

// Prepare
//
procedure TSectorMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      TFGPolygon(FaceGroups[i]).Prepare;
   FaceGroups.SortByMaterial; // this brings portals first
end;

// ------------------
// ------------------ TFGPolygon ------------------
// ------------------

// Create
//
constructor TFGPolygon.Create;
begin
	inherited;
   Mode:=fgmmTriangleFan;
end;

// Destroy
//
destructor TFGPolygon.Destroy;
begin
	inherited;
end;

// Prepare
//
procedure TFGPolygon.Prepare;
begin
   // nothing, ain't no portal !
end;

// ------------------
// ------------------ TFGPortalPolygon ------------------
// ------------------

// Create
//
constructor TFGPortalPolygon.Create;
begin
	inherited;
end;

// Destroy
//
destructor TFGPortalPolygon.Destroy;
begin
	inherited;
end;

// BuildList
//
procedure TFGPortalPolygon.BuildList(var mrci : TRenderContextInfo);
var
   dir : TAffineVector;
begin
   if FDestinationSectorIndex>=0 then begin
      VectorSubtract(FCenter, PAffineVector(@mrci.rcci.origin)^, dir);
      if (VectorDotProduct(FNormal, dir)<=0) and
            (not IsVolumeClipped(FCenter, FRadius, mrci.rcci)) then begin
         Owner.Owner.Owner.Items[FDestinationSectorIndex].BuildList(mrci);
      end
   end;
end;

// Prepare
//
procedure TFGPortalPolygon.Prepare;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   FNormal:=GetNormal;
   VectorAdd(min, max, FCenter);
   ScaleVector(FCenter, 0.5);
   FRadius:=VectorDistance(min, max)*0.5;
end;

// ------------------
// ------------------ TPortal ------------------
// ------------------

// Create
//
constructor TPortal.Create(AOwner: TComponent);
begin
   FMeshObjects:=TPortalMeshObjectList.Create(Self);
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   UseMeshMaterials:=True;
end;

// Destroy
//
destructor TPortal.Destroy;
begin
   inherited;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClass(TPortal);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

end.

