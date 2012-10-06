{: GLVectorFileObjects<p>

	Vector File related objects for GLScene<p>

	<b>Historique : </b><font size=-1><ul>
      <li>08/03/01 - Egg - TGL3DSVectorFile now loads materials for TBaseMesh
      <li>26/02/01 - Egg - Added TBaseMeshObject & BuildNormals, MD2 normals auto-builded
      <li>21/02/01 - Egg - Now XOpenGL based (multitexture)
      <li>15/01/01 - Egg - Added Translate methods
      <li>10/01/01 - Egg - Fixed in TBaseMesh.DoRender for RenderChildren states
      <li>08/01/01 - Egg - Fixed TBaseMesh.BuildList messup of attrib states
      <li>22/12/00 - Egg - Fixed non-interpolated TActor animation (was freezing),
                           Fixed TBaseMesh.DoRender messup of attrib states
      <li>18/12/00 - Egg - TFGIndexTexCoordList now supports normals (automatically),
                           NormalsOrientation code moved to TBaseMesh
      <li>11/12/00 - Egg - Fix for NormalOrientation (3DS importer)
      <li>06/12/00 - Egg - Added PrepareBuildList mechanism
      <li>08/10/00 - Egg - Removed TGLOBJVectorFile, use GLFileOBJ instead 
      <li>13/08/00 - Egg - Enhancements for Portal Rendering support,
                           Added utility methods & triangle fans
      <li>10/08/00 - Egg - Added CurrentAnimation, fixed TMeshObject.GetExtents
      <li>21/07/00 - Egg - Vastly improved memory use and mechanisms for MD2/TActor
      <li>19/07/00 - Egg - Introduced enhanced mesh structure
      <li>16/07/00 - Egg - Made use of new TDataFile class
      <li>15/07/00 - Egg - FreeForm can now handle 3DS files with multiple textures,
                           Added TBaseMesh.GetExtents
      <li>28/06/00 - Egg - Support for "ObjectStyle"
      <li>23/06/00 - Egg - Reversed "t" texture coord for MD2,
                           TActorAnimations can now load/save
      <li>21/06/00 - Egg - Added frame change events to TActor,
                           Added TActorAnimations collection
      <li>19/06/00 - Egg - Completed smooth movement interpolation for TActor
      <li>07/06/00 - Egg - TVectorFile now longers assumes a TFreeForm as Owner,
                           Added generic TVectorFile.LoadFromFile
      <li>26/05/00 - Egg - Removed dependency to GLObjects,
                           TFreeForm now may use InterleavedArrays instead of
                           IndexedArrays (better BuildList compatibility)
      <li>22/04/00 - Egg - Fixed Material handlings in TFreeForm, inverted CCW/CW
                           convention for 3DS Release3
		<li>11/04/00 - Egg - Removed unnecessary code in finalization (thanks Uwe)
	   <li>09/02/00 - Egg - Creation from split of GLObjects,
                           fixed class registrations and formats unregistration
	</ul></font>

   // BuildInterleavedArray
//
procedure TFaceGroup.BuildInterleavedArray(vertexList : TVertexList);
var
   k, idx, p : Integer;
   tc : TTexPoint;
   vn , vc : TAffineVector;
begin
   ReallocMem(FInterLeavedArray, 8 * IndexCount * SizeOf(TGLFloat));
   for k:=0 to FIndexCount-1 do begin
      idx:=FIndices[k];  p:=k*8;
      tc:=vertexList.VertexTexCoord[idx];
      FInterleavedArray[p+0]:=tc.S;
      FInterleavedArray[p+1]:=tc.T;
      vn:=vertexList.VertexNormal[idx];
      FInterleavedArray[p+2]:=vn[0];
      FInterleavedArray[p+3]:=vn[1];
      FInterleavedArray[p+4]:=vn[2];
      vc:=vertexList.VertexCoord[idx];
      FInterleavedArray[p+5]:=vc[0];
      FInterleavedArray[p+6]:=vc[1];
      FInterleavedArray[p+7]:=vc[2];
   end;
end;

}
unit GLVectorFileObjects;

interface

uses Windows, Classes, GLScene, OpenGL12, Geometry, SysUtils, GLMisc, GLTexture,
   GLMesh, VectorLists;

type

   TMeshObjectList = class;
   TFaceGroups = class;

   // TMeshObjectMode
   //
   TMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);

   // TBaseMeshObject
   //
   {: A base class for mesh objects. }
   TBaseMeshObject = class (TPersistent)
      private
         { Private Declarations }
         FName : String;
         FVertices : TAffineVectorList;
         FNormals : TAffineVectorList;

      protected
         { Protected Declarations }
         procedure SetVertices(const val : TAffineVectorList);
         procedure SetNormals(const val : TAffineVectorList);

      public
         { Public Declarations }
         constructor Create;
         destructor Destroy; override;

         {: Translates all the vertices by the given delta. }
         procedure Translate(const delta : TAffineVector); dynamic;
         {: Builds (smoothed) normals for the vertex list.<p>
            If normalIndices is nil, the method assumes a bijection between
            vertices and normals sets, and when exiting Normals and Vertices
            list will have the same number of itmes (whatever previously was in
            the Normals list is ignored/removed).<p>
            If normalIndices is defined, normals will be added to the list and
            their indices will be added to normalIndices. Already defined
            normals and indices are preserved.<p>
            The only valid modes are currently momTriangles and momTriangleStrip }
         procedure BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                normalIndices : TIntegerList = nil);

         property Name : String read FName write FName;
         property Vertices : TAffineVectorList read FVertices write SetVertices;
         property Normals : TAffineVectorList read FNormals write SetNormals;
   end;

   // TMeshObject
   //
   {: Base mesh class.<p>
      Introduces base methods and properties for mesh objects.<p>
      Subclasses are named "TMOxxx". }
   TMeshObject = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshObjectList;
         FTexCoords : TAffineVectorList; // provision for 3D textures
         FColors : TVectorList;
         FFaceGroups: TFaceGroups;
         FMode : TMeshObjectMode;
         FArraysDeclared : Boolean;

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);
         procedure SetColors(const val : TVectorList);

         procedure DeclareArraysToOpenGL(evenIfAlreadyDeclared : Boolean = False);

      public
         { Public Declarations }
         constructor Create(AOwner : TMeshObjectList); virtual;
         destructor Destroy; override;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         //: The extents of the object (min and max coordinates)
         procedure GetExtents(var min, max : TAffineVector); dynamic;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         function PointInObject(const aPoint : TAffineVector) : Boolean; virtual;

         property Owner : TMeshObjectList read FOwner;
         property Mode : TMeshObjectMode read FMode write FMode;
         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
         property Colors : TVectorList read FColors write SetColors;
         property FaceGroups : TFaceGroups read FFaceGroups;
   end;

   // TMeshObjectList
   //
   {: A list of TMeshObject objects. }
   TMeshObjectList = class (TList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetMeshObject(Index: Integer) : TMeshObject;

      public
         { Public Declarations }
         constructor Create(AOwner : TPersistent);
         destructor Destroy; override;

         {: Prepare the texture and materials before rendering.<p>
            Invoked once, before building the list and NOT while building the list. }
         procedure PrepareBuildList(var mrci : TRenderContextInfo); virtual;
         //: Similar to regular scene object's BuildList method
         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);
         function MorphTargetCount : Integer;

         procedure GetExtents(var min, max : TAffineVector);
         procedure Translate(const delta : TAffineVector);

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshObject read GetMeshObject; default;
   end;

   TMeshObjectListClass = class of TMeshObjectList;

   TMeshMorphTargetList = class;
   
   // TMeshMorphTarget
   //
   {: A morph target, stores alternate lists of vertices and normals. }
   TMeshMorphTarget = class (TBaseMeshObject)
      private
         { Private Declarations }
         FOwner : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner : TMeshMorphTargetList);
         destructor Destroy; override;

         property Owner : TMeshMorphTargetList read FOwner;
   end;

   // TMeshMorphTargetList
   //
   {: A list of TMeshMorphTarget objects. }
   TMeshMorphTargetList = class (TList)
      private
         { Private Declarations }
         FOwner : TPersistent;

      protected
         { Protected Declarations }
         function GetMeshMorphTarget(Index: Integer) : TMeshMorphTarget;

      public
         { Public Declarations }
         constructor Create(AOwner : TPersistent);
         destructor Destroy; override;

         procedure Translate(const delta : TAffineVector);

         property Owner : TPersistent read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TMeshMorphTarget read GetMeshMorphTarget; default;
   end;

   // TMorphableMeshObject
   //
   {: Mesh object with support for morph targets.<p>
      The morph targets allow to change vertices and normals according to pre-
      existing "morph targets". }
   TMorphableMeshObject = class (TMeshObject)
      private
         { Private Declarations }
         FMorphTargets : TMeshMorphTargetList;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(AOwner : TMeshObjectList); override;
         destructor Destroy; override;

         procedure Translate(const delta : TAffineVector); override;

         procedure MorphTo(morphTargetIndex : Integer);
         procedure Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                        lerpFactor : Single);

         property MorphTargets : TMeshMorphTargetList read FMorphTargets;
   end;

   // TFaceGroup
   //
   {: Describes a face group of a TMeshObject.<p>
      Face groups should be understood as "a way to use mesh data to render
      a part or the whole mesh object".<p>
      Subclasses implement the actual behaviours, and should have at least
      one "Add" method, taking in parameters all that is required to describe
      a single base facegroup element. }
   TFaceGroup = class (TPersistent)
      private
         { Private Declarations }
         FOwner : TFaceGroups;
         FMaterialName : String;

      public
         { Public Declarations }
         constructor Create(AOwner : TFaceGroups); virtual;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); virtual;

         //: Precalculate whatever is needed for rendering, called once
         procedure Prepare; dynamic;

         property Owner : TFaceGroups read FOwner;
         property MaterialName : String read FMaterialName write FMaterialName;
   end;

   // TFaceGroupMeshMode
   //
   {: Known descriptions for face group mesh modes.<p>
      - fgmmTriangles : issue all vertices with GL_TRIANGLES<br>
      - fgmmTriangleStrip : issue all vertices with GL_TRIANGLE_STRIP<br>
      - fgmmFlatTriangles : same as fgmmTriangles, but take advantage of having
         the same normal for all vertices of a triangle.<br>
      - fgmmTriangleFan : issue all vertices with GL_TRIANGLE_FAN }
   TFaceGroupMeshMode = (fgmmTriangles, fgmmTriangleStrip, fgmmFlatTriangles,
                         fgmmTriangleFan);

   // TFGVertexIndexList
   //
   {: A face group based on an indexlist.<p>
      The index list refers to items in the mesh object (vertices, normals, etc.),
      that are all considered in sync, the render is obtained issueing the items
      in the order given by the vertices.<p> }
   TFGVertexIndexList = class (TFaceGroup)
      private
         { Private Declarations }
         FVertexIndices : TIntegerList;
         FMode : TFaceGroupMeshMode;

      protected
         { Protected Declarations }
         procedure SetVertexIndices(const val : TIntegerList);

      public
         { Public Declarations }
         constructor Create(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         procedure Add(idx : Integer);
         procedure GetExtents(var min, max : TAffineVector);
         //: Return the normal from the 1st three points in the facegroup
         function  GetNormal : TAffineVector;

         property Mode : TFaceGroupMeshMode read FMode write FMode;
         property VertexIndices : TIntegerList read FVertexIndices write SetVertexIndices;
   end;

   // TFGVertexNormalTexIndexList
   //
   {: Adds normals and texcoords indices.<p>
      Allows very compact description of a mesh. }
   TFGVertexNormalTexIndexList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FNormalIndices : TIntegerList;
         FTexCoordIndices : TIntegerList;

      protected
         { Protected Declarations }
         procedure SetNormalIndices(const val : TIntegerList);
         procedure SetTexCoordIndices(const val : TIntegerList);

      public
         { Public Declarations }
         constructor Create(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         procedure Add(vertexIdx, normalIdx, texCoordIdx : Integer);

         property NormalIndices : TIntegerList read FNormalIndices write SetNormalIndices;
         property TexCoordIndices : TIntegerList read FTexCoordIndices write SetTexCoordIndices;
   end;

   // TFGIndexTexCoordList
   //
   {: Adds per index texture coordinates to its ancestor.<p>
      Per index texture coordinates allows having different texture coordinates
      per triangle, depending on the face it is used in. }
   TFGIndexTexCoordList = class (TFGVertexIndexList)
      private
         { Private Declarations }
         FTexCoords : TAffineVectorList;

      protected
         { Protected Declarations }
         procedure SetTexCoords(const val : TAffineVectorList);

      public
         { Public Declarations }
         constructor Create(AOwner : TFaceGroups); override;
         destructor Destroy; override;

         procedure BuildList(var mrci : TRenderContextInfo); override;

         procedure Add(idx : Integer; const texCoord : TAffineVector); overload;
         procedure Add(idx : Integer; const s, t : Single); overload;

         property TexCoords : TAffineVectorList read FTexCoords write SetTexCoords;
   end;

   // TFaceGroups
   //
   {: A list of TFaceGroup objects. }
   TFaceGroups = class (TList)
      private
         { Private Declarations }
         FOwner : TMeshObject;

      protected
         { Protected Declarations }
         function GetFaceGroup(Index: Integer) : TFaceGroup;

      public
         { Public Declarations }
         constructor Create(AOwner : TMeshObject);
         destructor Destroy; override;

         property Owner : TMeshObject read FOwner;
         procedure Clear; override;
         property Items[Index: Integer] : TFaceGroup read GetFaceGroup; default;

         //: Sort faces by material, those without material first in list
         procedure SortByMaterial;
   end;

   // TMeshNormalsOrientation
   //
   {: Determines how normals orientation is defined in a mesh.<p>
      - mnoDefault : uses default orientation<br>
      - mnoInvert : inverse of default orientation<br>
      - mnoAutoSolid : autocalculate to make the mesh globally solid<br>
      - mnoAutoHollow : autocalculate to make the mesh globally hollow<br> }
   TMeshNormalsOrientation = (mnoDefault, mnoInvert); //, mnoAutoSolid, mnoAutoHollow);

   TBaseMesh = class;

   // TVectorFile
   //
   {: Abstract base class for different vector file formats.<p>
      The actual implementation for these files (3DS, DXF..) must be done
      seperately. The concept for TVectorFile is very similar to TGraphic
      (see Delphi Help). }
   TVectorFile = class (TDataFile)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;

      protected
         { Protected Declarations }
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation); virtual;

      public
         { Public Declarations }
         constructor Create(AOwner: TPersistent); virtual;

         function Owner : TBaseMesh;

         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation;
   end;

   TVectorFileClass = class of TVectorFile;

   // TGL3DSVectorFile
   //
   {: The 3DStudio vector file.<p>
      Uses 3DS import library by Mike Lischke (http://www.lishcke-online.de).<p>
      A 3DS file may contain material information and require textures when
      loading. Only the primary texture map is used by GLScene, transparency,
      bump mapping, etc. are ignored as of now. }
   TGL3DSVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLMD2VectorFile
   //
   {: The MD2 vector file (Quake2 actor file).<p>
      Stores a set of "frames" describing the different postures of the actor,
      it may be animated by TActor. The "Skin" must be loaded indepentendly
      (the whole mesh uses a single texture bitmap).<p>
      Based on code by Roger Cao. }
   TGLMD2VectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLTINVectorFile
   //
   {: The TIN vector file (triangle irregular network).<p>
      It is a simple text format, with one triangle record per line, no materials,
      no texturing (there may be more, but I never saw anything in this files).<p>
      This format is encountered in the DEM/DTED world and used in place of grids. }
   TGLTINVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TGLSTLVectorFile
   //
   {: The STL vector file (stereolithography format).<p>
      It is a list of the triangular surfaces that describe a computer generated
      solid model. This is the standard input for most rapid prototyping machines.<p>
      There are two flavors of STL, the "text" and the "binary", this reader
      supports only the "binary" version.<p>
      Original code by Paul M. Bearne. }
   TGLSTLVectorFile = class(TVectorFile)
      public
         { Public Declarations }
         procedure LoadFromStream(aStream: TStream); override;
   end;

   // TBaseMesh
   //
   {: Base class for mesh objects. }
   TBaseMesh = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormalsOrientation : TMeshNormalsOrientation;
         FMaterialLibrary : TGLMaterialLibrary;
         FUseMeshMaterials : Boolean;

      protected
         { Protected Declarations }
         FMeshObjects : TMeshObjectList;     // a list of mesh objects
         procedure SetUseMeshMaterials(const val : Boolean);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetNormalsOrientation(const val : TMeshNormalsOrientation);

         procedure DoDestroyList(glsceneOnly : Boolean); override;
         {: Invoked after creating a TVectorFile in LoadFromStream and before loading.<p>
            Allows to adjust/transfer subclass-specific features. }
         procedure PrepareVectorFile(aFile : TVectorFile); dynamic;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
			procedure DoRender(var rci : TRenderContextInfo); override;
         procedure StructureChanged; override;

         property MeshObjects : TMeshObjectList read FMeshObjects;
         {: Calculates the extents of a mesh.<p> }
         procedure GetExtents(var min, max : TAffineVector);

         {: Loads a vector file.<p>
            A vector files (for instance a ".3DS") stores the definition of
            a mesh as well as materials property.<p>
            Loading a file replaces the current one (if any). }
         procedure LoadFromFile(const Filename : String);
         {: Loads a vector file from a stream.<p>
            See LoadFromFile.. }
         procedure LoadFromStream(const Filename : String; aStream : TStream);

         {: Material library where mesh materials will be stored/retrieved.<p>
            If this property is not defined or if UseMeshMaterials is false,
            only the FreeForm's material will be used (and the mesh's materials
            will be ignored. }
         property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
         {: Defines wether materials declared in the vector file mesh are used.<p>
            You must also define the MaterialLibrary property. }
         property UseMeshMaterials : Boolean read FUseMeshMaterials write SetUseMeshMaterials default True;

         {: Normals orientation for owned mesh.<p> }
         property NormalsOrientation : TMeshNormalsOrientation read FNormalsOrientation write SetNormalsOrientation default mnoDefault;
   end;

   // TFreeForm
   //
   {: Container objects for a vector file mesh.<p>
      FreeForms allows loading and rendering vector files (like 3DStudio
      ".3DS" file) in GLScene. Meshes can be loaded with the LoadFromFile
      method.<p>
      A FreeForm may contain more than one mesh, but they will all be handled
      as a single object in a scene. }
   TFreeForm = class (TBaseMesh)
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
         property UseMeshMaterials;
         property NormalsOrientation;
    end;

	// TActorAnimation
	//
	TActorAnimation = class (TCollectionItem)
	   private
	      { Private Declarations }
         FName : String;
         FStartFrame : Integer;
         FEndFrame : Integer;

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure SetStartFrame(const val : Integer);
         procedure SetEndFrame(const val : Integer);
         procedure SetAsString(const val : String);
         function GetAsString : String;

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

         property AsString : String read GetAsString write SetAsString;

	   published
	      { Published Declarations }
         property Name : String read FName write FName;
         property StartFrame : Integer read FStartFrame write SetStartFrame;
         property EndFrame : Integer read FEndFrame write SetEndFrame;
	end;

   TActor = class;

	// TActorAnimations
	//
	TActorAnimations = class (TCollection)
	   private
	      { Private Declarations }
	      owner : TActor;

	   protected
	      { Protected Declarations }
	      function GetOwner: TPersistent; override;
         procedure SetItems(index : Integer; const val : TActorAnimation);
	      function GetItems(index : Integer) : TActorAnimation;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TActor);
         function Add: TActorAnimation;
	      function FindItemID(ID: Integer): TActorAnimation;
	      function FindName(const aName : String) : TActorAnimation;
         function FindFrame(frame : Integer) : TActorAnimation;

	      procedure SetToStrings(aStrings : TStrings);
         procedure SaveToStream(aStream : TStream);
         procedure LoadFromStream(aStream : TStream);
         procedure SaveToFile(const fileName : String);
         procedure LoadFromFile(const fileName : String);

	      property Items[index : Integer] : TActorAnimation read GetItems write SetItems; default;
   end;

   // TActorFrameInterpolation
   //
   {: Actor frame-interpolation mode.<p>
      - afpNone : no interpolation, display CurrentFrame only<br>
      - afpLinear : perform linear interpolation between current and next frame }
   TActorFrameInterpolation = (afpNone, afpLinear);

   // TActorActionMode
   //
   {: Defines how an actor plays between its StartFrame and EndFrame.<p>
      <ul>
      <li>aamNone : no animation is performed
      <li>aamPlayOnce : play from current frame to EndFrame, once end frame has
         been reached, switches to aamNone
      <li>aamLoop : play from current frame to EndFrame, once end frame has
         been reached, sets CurrentFrame to StartFrame
      <li>aamBounceForward : play from current frame to EndFrame, once end frame
         has been reached, switches to aamBounceBackward
      <li>aamBounceBackward : play from current frame to StartFrame, once start
         frame has been reached, switches to aamBounceForward
      </ul> }
   TActorAnimationMode = (aamNone, aamPlayOnce, aamLoop, aamBounceForward,
                          aamBounceBackward);

   // TActor
   //
   TActor = class(TBaseMesh)
      private
         { Private Declarations }
         FFrameCount : Integer;
         FStartFrame, FEndFrame : Integer;
         FCurrentFrame : Integer;
         FCurrentFrameDelta : Single;
         FFrameInterpolation : TActorFrameInterpolation;
         FInterval : Integer;
         FAnimationMode : TActorAnimationMode;
         FOnFrameChanged : TNotifyEvent;
         FOnEndFrameReached, FOnStartFrameReached : TNotifyEvent;
         FAnimations : TActorAnimations;

      protected
         { Protected Declarations }
         procedure SetCurrentFrame(Value: Integer);
         procedure SetStartFrame(Value: Integer);
         procedure SetEndFrame(Value: Integer);
         procedure SetAnimations(const val : TActorAnimations);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure LoadFromFile(const Filename: string);

			procedure DoProgress(const deltaTime, newTime : Double); override;

	      procedure SwitchToAnimation(anAnimation : TActorAnimation); overload;
	      procedure SwitchToAnimation(const animationName : String); overload;
	      procedure SwitchToAnimation(animationIndex : Integer); overload;
         function CurrentAnimation : String;

         {: Synchronize self animation with an other actor.<p>
            Copies Start/Current/End Frame values, CurrentFrameDelta,
            AnimationMode and FrameInterpolation. }
         procedure Synchronize(referenceActor : TActor);

         function  NextFrameIndex : Integer;
         procedure NextFrame(n : Integer = 1);
         procedure PrevFrame(n : Integer = 1);

         property FrameCount: Integer read FFrameCount;

      published
         { Published Declarations }
         property StartFrame: Integer read FStartFrame write SetStartFrame;
         property EndFrame: Integer read FEndFrame write SetEndFrame;

         property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
         {: Value in the [0; 1] range expressing the delta to the next frame.<p> }
         property CurrentFrameDelta : Single read FCurrentFrameDelta write FCurrentFrameDelta;
         property FrameInterpolation : TActorFrameInterpolation read FFrameInterpolation write FFrameInterpolation default afpLinear;

         {: See TActorAnimationMode.<p> }
         property AnimationMode : TActorAnimationMode read FAnimationMode write FAnimationMode default aamNone;
         {: Interval between frames, in milliseconds. }
         property Interval : Integer read FInterval write FInterval;

         {: Triggered after each CurrentFrame change. }
         property OnFrameChanged : TNotifyEvent read FOnFrameChanged write FOnFrameChanged;
         {: Triggered after EndFrame has been reached by progression or "nextframe" }
         property OnEndFrameReached : TNotifyEvent read FOnEndFrameReached write FOnEndFrameReached;
         {: Triggered after StartFrame has been reached by progression or "nextframe" }
         property OnStartFrameReached : TNotifyEvent read FOnStartFrameReached write FOnStartFrameReached;

         property Animations : TActorAnimations read FAnimations write SetAnimations;

         property MaterialLibrary;
         property UseMeshMaterials;
         property NormalsOrientation;
   end;

   PVectorFileFormat = ^TVectorFileFormat;
   TVectorFileFormat = record
      VectorFileClass : TVectorFileClass;
      Extension       : String;
      Description     : String;
      DescResID       : Integer;
   end;

   TVectorFileFormatsList = class(TList)
   public
     destructor Destroy; override;
     procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TVectorFileClass);
     function FindExt(Ext: string): TVectorFileClass;
     procedure Remove(AClass: TVectorFileClass);
     procedure BuildFilterStrings(VectorFileClass: TVectorFileClass; var Descriptions, Filters: string);
   end;

   EInvalidVectorFile = class(Exception);


function GetVectorFileFormats : TVectorFileFormatsList;
procedure RegisterVectorFileFormat(const aExtension, aDescription: String;
                                   aClass : TVectorFileClass);
procedure UnregisterVectorFileClass(aClass : TVectorFileClass);

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses GLStrings, consts, XOpenGL,
     // 3DS Support
	  File3DS, Types3DS,
     // STL Support
     TypesSTL,
     // MD2 Support
	  FileMD2, TypesMD2;

var
   vVectorFileFormats : TVectorFileFormatsList;

const
   cAAFHeader = 'AAF';

function GetVectorFileFormats: TVectorFileFormatsList;
begin
   if not Assigned(vVectorFileFormats)then
      vVectorFileFormats:=TVectorFileFormatsList.Create;
   Result:=vVectorFileFormats;
end;

procedure RegisterVectorFileFormat(const AExtension, ADescription: String; AClass: TVectorFileClass);
begin
   RegisterClass(AClass);
	GetVectorFileFormats.Add(AExtension, ADescription, 0, AClass);
end;

procedure UnregisterVectorFileClass(AClass: TVectorFileClass);
begin
	if Assigned(vVectorFileFormats) then
		vVectorFileFormats.Remove(AClass);
end;

//----------------- vector format support --------------------------------------

destructor TVectorFileFormatsList.Destroy;

var I: Integer;

begin
  for I:=0 to Count - 1 do Dispose(PVectorFileFormat(Items[I]));
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
                                     AClass: TVectorFileClass);

var NewRec: PVectorFileFormat;

begin
  New(NewRec);
  with NewRec^ do
  begin
    Extension:=AnsiLowerCase(Ext);
    VectorFileClass:=AClass;
    Description:=Desc;
    DescResID:=DescID;
  end;
  inherited Add(NewRec);
end;

//------------------------------------------------------------------------------

function TVectorFileFormatsList.FindExt(Ext: string): TVectorFileClass;

var I: Integer;

begin
  Ext:=AnsiLowerCase(Ext);
  for I:=Count-1 downto 0 do
    with PVectorFileFormat(Items[I])^ do
      if Extension = Ext then
      begin
        Result:=VectorFileClass;
        Exit;
      end;
  Result:=nil;
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.Remove(AClass: TVectorFileClass);

var I : Integer;
    P : PVectorFileFormat;

begin
  for I:=Count-1 downto 0 do
  begin
    P:=PVectorFileFormat(Items[I]);
    if P^.VectorFileClass.InheritsFrom(AClass) then
    begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TVectorFileFormatsList.BuildFilterStrings(VectorFileClass: TVectorFileClass;
                                                    var Descriptions, Filters: string);

var C, I : Integer;
    P    : PVectorFileFormat;

begin
  Descriptions:='';
  Filters:='';
  C:=0;
  for I:=Count-1 downto 0 do
  begin
    P:=PVectorFileFormat(Items[I]);
    if P^.VectorFileClass.InheritsFrom(VectorFileClass) and (P^.Extension <> '') then
      with P^ do
      begin
        if C <> 0 then
        begin
          Descriptions:=Descriptions+'|';
          Filters:=Filters+';';
        end;
        if (Description = '') and (DescResID <> 0) then Description:=LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then FmtStr(Descriptions, '%s (%s)|%1:s|%s', [sAllFilter, Filters, Descriptions]);
end;

// ------------------
// ------------------ TBaseMeshObject ------------------
// ------------------

// Create
//
constructor TBaseMeshObject.Create;
begin
   FVertices:=TAffineVectorList.Create;
   FNormals:=TAffineVectorList.Create;
   inherited Create;
end;

// Destroy
//
destructor TBaseMeshObject.Destroy;
begin
   FNormals.Free;
   FVertices.Free;
   inherited;
end;

// Translate
//
procedure TBaseMeshObject.Translate(const delta : TAffineVector);
begin
   FVertices.Translate(delta);
end;

// BuildNormals
//
procedure TBaseMeshObject.BuildNormals(vertexIndices : TIntegerList; mode : TMeshObjectMode;
                                       normalIndices : TIntegerList = nil);
var
   i, base : Integer;
   n : TAffineVector;
   newNormals : TList;

   procedure TranslateNewNormal(vertexIndex : Integer; const delta : TAffineVector);
   var
      pv : PAffineVector;
   begin
      pv:=PAffineVector(newNormals[vertexIndex]);
      if not Assigned(pv) then begin
         Normals.Add(NullVector);
         pv:=@Normals.List[Normals.Count-1];
         newNormals[vertexIndex]:=pv;
      end;
      AddVector(pv^, delta);
   end;

begin
   if not Assigned(normalIndices) then begin
      // build bijection
      Normals.Clear;
      Normals.Count:=Vertices.Count;
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do with Normals do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               with Normals do begin
                  TranslateItem(vertexIndices[i+0], n);
                  TranslateItem(vertexIndices[i+1], n);
                  TranslateItem(vertexIndices[i+2], n);
               end;
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      Normals.Normalize;
   end else begin
      // add new normals
      base:=Normals.Count;
      newNormals:=TList.Create;
      newNormals.Count:=Vertices.Count;
      case mode of
         momTriangles : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                  Items[vertexIndices[i+2]], n);
               end;
               with Normals do begin
                  TranslateNewNormal(vertexIndices[i+0], n);
                  TranslateNewNormal(vertexIndices[i+1], n);
                  TranslateNewNormal(vertexIndices[i+2], n);
               end;
               Inc(i, 3);
            end;
         end;
         momTriangleStrip : begin
            i:=0; while i<=vertexIndices.Count-3 do begin
               with Vertices do begin
                  if (i and 1)=0 then
                     CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+1]],
                                     Items[vertexIndices[i+2]], n)
                  else CalcPlaneNormal(Items[vertexIndices[i+0]], Items[vertexIndices[i+2]],
                                       Items[vertexIndices[i+1]], n);
               end;
               with Normals do begin
                  TranslateNewNormal(vertexIndices[i+0], n);
                  TranslateNewNormal(vertexIndices[i+1], n);
                  TranslateNewNormal(vertexIndices[i+2], n);
               end;
               Inc(i, 1);
            end;
         end;
      else
         Assert(False);
      end;
      for i:=base to Normals.Count-1 do
         NormalizeVector(Normals.List[i]);
      newNormals.Free;
   end;
end;

// SetVertices
//
procedure TBaseMeshObject.SetVertices(const val : TAffineVectorList);
begin
   FVertices.Assign(val);
end;

// SetNormals
//
procedure TBaseMeshObject.SetNormals(const val : TAffineVectorList);
begin
   FNormals.Assign(val);
end;

// ------------------
// ------------------ TMeshObject ------------------
// ------------------

// Create
//
constructor TMeshObject.Create(AOwner : TMeshObjectList);
begin
   FOwner:=AOwner;
   FMode:=momTriangles;
   FTexCoords:=TAffineVectorList.Create;
   FColors:=TVectorList.Create;
   FFaceGroups:=TFaceGroups.Create(Self);
   inherited Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TMeshObject.Destroy;
begin
   FFaceGroups.Free;
   FColors.Free;
   FTexCoords.Free;
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// GetExtents
//
procedure TMeshObject.GetExtents(var min, max : TAffineVector);
begin
   FVertices.GetExtents(min, max);
end;

// Prepare
//
procedure TMeshObject.Prepare;
var
   i : Integer;
begin
   for i:=0 to FaceGroups.Count-1 do
      FaceGroups[i].Prepare;
end;

// PointInObject
//
function TMeshObject.PointInObject(const aPoint : TAffineVector) : Boolean;
var
   min, max : TAffineVector;
begin
   GetExtents(min, max);
   Result:=(aPoint[0]>=min[0]) and (aPoint[1]>=min[1]) and (aPoint[2]>=min[2])
           and (aPoint[0]<=max[0]) and (aPoint[1]<=max[1]) and (aPoint[2]<=max[2]);
end;

// SetTexCoords
//
procedure TMeshObject.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// SetColors
//
procedure TMeshObject.SetColors(const val : TVectorList);
begin
   FColors.Assign(val);
end;

// DeclareArraysToOpenGL
//
procedure TMeshObject.DeclareArraysToOpenGL(evenIfAlreadyDeclared : Boolean = False);
begin
   if evenIfAlreadyDeclared or (not FArraysDeclared) then begin
      if Vertices.Count>0 then begin
         glEnableClientState(GL_VERTEX_ARRAY);
         glVertexPointer(3, GL_FLOAT, 0, Vertices.List);
      end else glDisableClientState(GL_VERTEX_ARRAY);
      if Normals.Count>0 then begin
         glEnableClientState(GL_NORMAL_ARRAY);
         glNormalPointer(GL_FLOAT, 0, Normals.List);
      end else glDisableClientState(GL_NORMAL_ARRAY);
      if Colors.Count>0 then begin
         glEnableClientState(GL_COLOR_ARRAY);
         glColorPointer(4, GL_FLOAT, 0, Colors.List);
      end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      if TexCoords.Count>0 then begin
         glEnableClientState(GL_TEXTURE_COORD_ARRAY);
         xglTexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), TexCoords.List);
      end else glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      FArraysDeclared:=True;
   end;
end;

// PrepareMaterials
//
procedure TMeshObject.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   libMat : TGLLibMaterial;
begin
   if (Mode=momFaceGroups) and Assigned(mrci.materialLibrary) then begin
      for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
         libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
         if Assigned(libMat) then
            libMat.PrepareBuildList;
      end;
   end;
end;

// BuildList
//
procedure TMeshObject.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   gotNormals, gotTexCoords, gotColor : Boolean;
   libMat : TGLLibMaterial;
begin
   FArraysDeclared:=False;
   case Mode of
      momTriangles, momTriangleStrip : if Vertices.Count>0 then begin
         DeclareArraysToOpenGL;
         if Mode=momTriangles then
            glBegin(GL_TRIANGLES)
         else glBegin(GL_TRIANGLE_STRIP);
         gotNormals:=(Vertices.Count=Normals.Count);
         gotTexCoords:=(Vertices.Count=TexCoords.Count);
         gotColor:=(Vertices.Count=Colors.Count);
         if gotColor then begin
            glPushAttrib(GL_ENABLE_BIT);
            glEnable(GL_COLOR_MATERIAL);
            glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
            ResetGLMaterialColors;
         end;
         for i:=0 to Vertices.Count-1 do begin
            if gotNormals   then glNormal3fv(@Normals.List[i]);
            if gotTexCoords then xglTexCoord2fv(@TexCoords.List[i]);
            if gotColor     then glColor4fv(@Colors.List[i]);
            glVertex3fv(@Vertices.List[i]);
         end;
         glEnd;
         if gotColor then glPopAttrib;
      end;
      momFaceGroups : begin
         if Assigned(mrci.materialLibrary) then begin
            for i:=0 to FaceGroups.Count-1 do with FaceGroups[i] do begin
               libMat:=mrci.materialLibrary.Materials.GetLibMaterialByName(MaterialName);
               if Assigned(libMat) then begin
                  libMat.Apply(mrci);
                  BuildList(mrci);
                  libMat.UnApply(mrci);
               end else BuildList(mrci);
            end;
         end else for i:=0 to FaceGroups.Count-1 do
            FaceGroups[i].BuildList(mrci);
      end;
   else
      Assert(False);
   end;
end;

// ------------------
// ------------------ TMeshObjectList ------------------
// ------------------

// Create
//
constructor TMeshObjectList.Create(AOwner : TPersistent);
begin
   FOwner:=AOwner;
   inherited Create;
end;

// Destroy
//
destructor TMeshObjectList.Destroy;
begin
   Clear;
   inherited;
end;

// PrepareBuildList
//
procedure TMeshObjectList.PrepareBuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].PrepareBuildList(mrci);
end;

// BuildList
//
procedure TMeshObjectList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].BuildList(mrci);
end;

// MorphTo
//
procedure TMeshObjectList.MorphTo(morphTargetIndex : Integer);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

// Lerp
//
procedure TMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                               lerpFactor : Single);
var
   i : Integer;
begin
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      TMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2, lerpFactor);
end;

// MorphTargetCount
//
function TMeshObjectList.MorphTargetCount : Integer;
var
   i : Integer;
begin
   Result:=MaxInt;
   for i:=0 to Count-1 do if Items[i] is TMorphableMeshObject then
      with TMorphableMeshObject(Items[i]) do
         if Result>MorphTargets.Count then
            Result:=MorphTargets.Count;
   if Result=MaxInt then
      Result:=0;
end;

// Clear
//
procedure TMeshObjectList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshObject
//
function TMeshObjectList.GetMeshObject(Index: Integer): TMeshObject;
begin
   Result:=Get(Index);
end;

// GetExtents
//
procedure TMeshObjectList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to Count-1 do begin
      GetMeshObject(i).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// Translate
//
procedure TMeshObjectList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      GetMeshObject(i).Translate(delta);
end;

// Prepare
//
procedure TMeshObjectList.Prepare;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Prepare;
end;

// ------------------
// ------------------ TMeshMorphTarget ------------------
// ------------------

// Create
//
constructor TMeshMorphTarget.Create(AOwner : TMeshMorphTargetList);
begin
   FOwner:=AOwner;
   inherited Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TMeshMorphTarget.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// ------------------
// ------------------ TMeshMorphTargetList ------------------
// ------------------

// Create
//
constructor TMeshMorphTargetList.Create(AOwner : TPersistent);
begin
   FOwner:=AOwner;
   inherited Create;
end;

// Destroy
//
destructor TMeshMorphTargetList.Destroy;
begin
   Clear;
   inherited;
end;

// Translate
//
procedure TMeshMorphTargetList.Translate(const delta : TAffineVector);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Translate(delta);
end;

// Clear
//
procedure TMeshMorphTargetList.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with Items[i] do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetMeshMorphTarget
//
function TMeshMorphTargetList.GetMeshMorphTarget(Index: Integer): TMeshMorphTarget;
begin
   Result:=Get(Index);
end;

// ------------------
// ------------------ TMorphableMeshObject ------------------
// ------------------

// Create
//
constructor TMorphableMeshObject.Create(AOwner : TMeshObjectList);
begin
   FMorphTargets:=TMeshMorphTargetList.Create(Self);
   inherited;
end;

// Destroy
//
destructor TMorphableMeshObject.Destroy;
begin
   FMorphTargets.Free;
   inherited;
end;

// Translate
//
procedure TMorphableMeshObject.Translate(const delta : TAffineVector);
begin
   inherited;
   MorphTargets.Translate(delta);
end;

// MorphTo
//
procedure TMorphableMeshObject.MorphTo(morphTargetIndex : Integer);
begin
   Assert(Cardinal(morphTargetIndex)<Cardinal(MorphTargets.Count));
   with MorphTargets[morphTargetIndex] do begin
      if Vertices.Count>0 then
         Self.Vertices.Assign(Vertices);
      if Normals.Count>0 then
         Self.Normals.Assign(Normals);
   end;
end;

// Lerp
//
procedure TMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2 : Integer;
                                    lerpFactor : Single);
var
   mt1, mt2 : TMeshMorphTarget;
begin
   Assert((Cardinal(morphTargetIndex1)<Cardinal(MorphTargets.Count))
          and (Cardinal(morphTargetIndex2)<Cardinal(MorphTargets.Count)));
   if lerpFactor=0 then
      MorphTo(morphTargetIndex1)
   else if lerpFactor=1 then
      MorphTo(morphTargetIndex2)
   else begin
      mt1:=MorphTargets[morphTargetIndex1];
      mt2:=MorphTargets[morphTargetIndex2];
      if mt1.Vertices.Count>0 then
         Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      if mt1.Normals.Count>0 then begin
         Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
         Normals.Normalize;
      end;
   end;
end;

// ------------------
// ------------------ TFaceGroup ------------------
// ------------------

// Create
//
constructor TFaceGroup.Create(AOwner : TFaceGroups);
begin
   FOwner:=AOwner;
   inherited Create;
   if Assigned(FOwner) then
      FOwner.Add(Self);
end;

// Destroy
//
destructor TFaceGroup.Destroy;
begin
   if Assigned(FOwner) then
      FOwner.Remove(Self);
   inherited;
end;

// BuildList
//
procedure TFaceGroup.BuildList(var mrci : TRenderContextInfo);
begin
   // nothing
end;

// Prepare
//
procedure TFaceGroup.Prepare;
begin
   // nothing
end;

// ------------------
// ------------------ TFGVertexIndexList ------------------
// ------------------

// Create
//
constructor TFGVertexIndexList.Create(AOwner : TFaceGroups);
begin
   inherited;
   FVertexIndices:=TIntegerList.Create;
   FMode:=fgmmTriangles;
end;

// Destroy
//
destructor TFGVertexIndexList.Destroy;
begin
   FVertexIndices.Free;
   inherited;
end;

// SetIndices
//
procedure TFGVertexIndexList.SetVertexIndices(const val : TIntegerList);
begin
   FVertexIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexIndexList.BuildList(var mrci : TRenderContextInfo);
begin
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLES, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
      fgmmTriangleStrip : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLE_STRIP, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
      fgmmTriangleFan : begin
         Owner.Owner.DeclareArraysToOpenGL(False);
         glDrawElements(GL_TRIANGLE_FAN, VertexIndices.Count,
                        GL_UNSIGNED_INT, VertexIndices.List)
      end;
   else
      Assert(False);
   end;
end;

// Add
//
procedure TFGVertexIndexList.Add(idx : Integer);
begin
   FVertexIndices.Add(idx);
end;

// GetExtents
//
procedure TFGVertexIndexList.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   f : Single;
   ref : PFloatArray;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to VertexIndices.Count-1 do begin
      ref:=Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
      for k:=0 to 2 do begin
         f:=ref[k];
         if f<min[k] then min[k]:=f;
         if f>max[k] then max[k]:=f;
      end;
   end;
end;

// GetNormal
//
function TFGVertexIndexList.GetNormal : TAffineVector;
begin
   if VertexIndices.Count<3 then
      Result:=NullVector
   else with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
                      Items[VertexIndices[2]], Result);
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

// Create
//
constructor TFGVertexNormalTexIndexList.Create(AOwner : TFaceGroups);
begin
   inherited;
   FNormalIndices:=TIntegerList.Create;
   FTexCoordIndices:=TIntegerList.Create;
end;

// Destroy
//
destructor TFGVertexNormalTexIndexList.Destroy;
begin
   FTexCoordIndices.Free;
   FNormalIndices.Free;
   inherited;
end;

// SetNormalIndices
//
procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val : TIntegerList);
begin
   FNormalIndices.Assign(val);
end;

// SetTexCoordIndices
//
procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val : TIntegerList);
begin
   FTexCoordIndices.Assign(val);
end;

// BuildList
//
procedure TFGVertexNormalTexIndexList.BuildList(var mrci : TRenderContextInfo);
var
   i : Integer;
   vertexPool : PAffineVectorArray;
   normalPool : PAffineVectorArray;
   texCoordPool : PAffineVectorArray;
begin
   Assert((VertexIndices.Count=TexCoordIndices.Count)
          and (VertexIndices.Count=NormalIndices.Count));
   vertexPool:=Owner.Owner.Vertices.List;
   normalPool:=Owner.Owner.Normals.List;
   texCoordPool:=Owner.Owner.TexCoords.List;
   case Mode of
      fgmmTriangles, fgmmFlatTriangles : glBegin(GL_TRIANGLES);
      fgmmTriangleStrip :                glBegin(GL_TRIANGLE_STRIP);
      fgmmTriangleFan   :                glBegin(GL_TRIANGLE_FAN);
   else
      Assert(False);
   end;
   for i:=0 to VertexIndices.Count-1 do begin
      glNormal3fv(@normalPool[NormalIndices.List[i]]);
      xglTexCoord2fv(@texCoordPool[TexCoordIndices.List[i]]);
      glVertex3fv(@vertexPool[VertexIndices.List[i]]);
   end;
   glEnd;
end;

// Add
//
procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx : Integer);
begin
   inherited Add(vertexIdx);
   FNormalIndices.Add(normalIdx);
   FTexCoordIndices.Add(texCoordIdx);
end;

// ------------------
// ------------------ TFGIndexTexCoordList ------------------
// ------------------

// Create
//
constructor TFGIndexTexCoordList.Create(AOwner : TFaceGroups);
begin
   inherited;
   FTexCoords:=TAffineVectorList.Create;
end;

// Destroy
//
destructor TFGIndexTexCoordList.Destroy;
begin
   FTexCoords.Free;
   inherited;
end;

// SetTexCoords
//
procedure TFGIndexTexCoordList.SetTexCoords(const val : TAffineVectorList);
begin
   FTexCoords.Assign(val);
end;

// BuildList
//
procedure TFGIndexTexCoordList.BuildList(var mrci : TRenderContextInfo);
var
   i, k : Integer;
   texCoordPool : PAffineVectorArray;
   vertexPool : PAffineVectorArray;
   normalPool : PAffineVectorArray;
   indicesPool : PIntegerArray;
begin
   Assert(VertexIndices.Count=TexCoords.Count);
   texCoordPool:=TexCoords.List;
   vertexPool:=Owner.Owner.Vertices.List;
   indicesPool:=@VertexIndices.List[0];
   case Mode of
      fgmmTriangles, fgmmFlatTriangles :
         glBegin(GL_TRIANGLES);
      fgmmTriangleStrip :
         glBegin(GL_TRIANGLE_STRIP);
      fgmmTriangleFan :
         glBegin(GL_TRIANGLE_FAN);
   else
      Assert(False);
   end;
   if Owner.Owner.Normals.Count=Owner.Owner.Vertices.Count then begin
      normalPool:=Owner.Owner.Normals.List;
      for i:=0 to VertexIndices.Count-1 do begin
         xglTexCoord2fv(@texCoordPool[i]);
         k:=indicesPool[i];
         glNormal3fv(@normalPool[k]);
         glVertex3fv(@vertexPool[k]);
      end;
   end else begin
      for i:=0 to VertexIndices.Count-1 do begin
         xglTexCoord2fv(@texCoordPool[i]);
         glVertex3fv(@vertexPool[indicesPool[i]]);
      end;
   end;
   glEnd;
end;

// Add (affine)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const texCoord : TAffineVector);
begin
   TexCoords.Add(texCoord);
   inherited Add(idx);
end;

// Add (s, t)
//
procedure TFGIndexTexCoordList.Add(idx : Integer; const s, t : Single);
begin
   TexCoords.Add(s, t, 0);
   inherited Add(idx);
end;

// ------------------
// ------------------ TFaceGroups ------------------
// ------------------

// Create
//
constructor TFaceGroups.Create(AOwner : TMeshObject);
begin
   FOwner:=AOwner;
   inherited Create;
end;

// Destroy
//
destructor TFaceGroups.Destroy;
begin
   Clear;
   inherited;
end;

// Clear
//
procedure TFaceGroups.Clear;
var
   i : Integer;
begin
   for i:=0 to Count-1 do with GetFaceGroup(i) do begin
      FOwner:=nil;
      Free;
   end;
   inherited;
end;

// GetFaceGroup
//
function TFaceGroups.GetFaceGroup(Index: Integer): TFaceGroup;
begin
   Result:=Get(Index);
end;

// SortByMaterial
//
procedure TFaceGroups.SortByMaterial;
var
   i, j : Integer;
   cur : String;
   curIdx : Integer;
begin
   for i:=0 to Count-2 do begin
      curIdx:=i;
      cur:=Items[i].MaterialName;
      for j:=i+1 to Count-1 do begin
         if Items[j].MaterialName<cur then begin
            cur:=Items[j].MaterialName;
            curIdx:=j;
         end;
      end;
      Exchange(curIdx, i);
   end;
end;

// ------------------
// ------------------ TVectorFile ------------------
// ------------------

// Create
//
constructor TVectorFile.Create(AOwner: TPersistent);
begin
   Assert(AOwner is TBaseMesh);
   inherited;
end;

// Owner
//
function TVectorFile.Owner : TBaseMesh;
begin
   Result:=TFreeForm(GetOwner);
end;

// SetNormalsOrientation
//
procedure TVectorFile.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   FNormalsOrientation:=val;
end;

// ------------------
// ------------------ TGL3DSVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGL3DSVectorFile.LoadFromStream(aStream: TStream);
type
   TSmoothIndexEntry = array[0..31] of Cardinal;
   PSmoothIndexArray = ^TSmoothIndexArray;
   TSmoothIndexArray = array[0..MaxInt shr 8] of TSmoothIndexEntry;
var
   Marker: PByteArray;
   CurrentVertexCount: Integer;
   SmoothIndices: PSmoothIndexArray;
   mesh : TMeshObject;

   //--------------- local functions -------------------------------------------

   function GetOrAllocateMaterial(materials : TMaterialList; const name : String) : String;
   var
      material : PMaterial3DS;
      specColor : TVector;
      matLib : TGLMaterialLibrary;
      libMat : TGLLibMaterial;
   begin
      material:=Materials.MaterialByName[Name];
      Assert(Assigned(material));
      if GetOwner is TBaseMesh then begin
         matLib:=TBaseMesh(GetOwner).MaterialLibrary;
         if Assigned(matLib) then begin
            Result:=name;
            libMat:=matLib.Materials.GetLibMaterialByName(name);
            if not Assigned(libMat) then begin
               libMat:=matLib.Materials.Add;
               libMat.Name:=name;
               with libMat.Material.FrontProperties do begin
                  Ambient.Color:=VectorMake(material.Ambient.R, material.Ambient.G, material.Ambient.B, 1);
                  Diffuse.Color:=VectorMake(material.Diffuse.R, material.Diffuse.G, material.Diffuse.B, 1);
                  specColor:=VectorMake(material.Specular.R, material.Specular.G, material.Specular.B, 1);
                  ScaleVector(specColor, 1 - material.Shininess);
                  Specular.Color:=specColor;
                  Shininess:=Round((1 - material.ShinStrength) * 128);
               end;
               if Trim(material.Texture.Map.Name)<>'' then begin
                  libMat.Material.Texture.Image.LoadFromFile(material.Texture.Map.Name);
                  libMat.Material.Texture.Disabled:=False;
               end;
            end;
         end else Result:='';
      end else Result:='';
   end;

   //----------------------------------------------------------------------

   function IsVertexMarked(P: Pointer; Index: Integer): Boolean; assembler;
      // tests the Index-th bit, returns True if set else False
   asm
                     BT [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   function MarkVertex(P: Pointer; Index: Integer): Boolean; assembler;
      // sets the Index-th bit and return True if it was already set else False
   asm
                     BTS [EAX], EDX
                     SETC AL
   end;

   //---------------------------------------------------------------------------

   procedure StoreSmoothIndex(ThisIndex, SmoothingGroup, NewIndex: Cardinal; P: Pointer);
      // Stores new vertex index (NewIndex) into the smooth index array of vertex ThisIndex
      // using field SmoothingGroup, which must not be 0.
      // For each vertex in the vertex array (also for duplicated vertices) an array of 32 cardinals
      // is maintained (each for one possible smoothing group. If a vertex must be duplicated because
      // it has no smoothing group or a different one then the index of the newly created vertex is
      // stored in the SmoothIndices to avoid loosing the conjunction between not yet processed vertices
      // and duplicated vertices.
      // Note: Only one smoothing must be assigned per vertex. Some available models break this rule and
      //       have more than one group assigned to a face. To make the code fail safe the group ID
      //       is scanned for the lowest bit set.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index (convert flag into an index)
                   MOV EDX, [P]                  // get address of index array
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, EDX
                   LEA EDX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV [EDX], ECX
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   function GetSmoothIndex(ThisIndex, SmoothingGroup: Cardinal; P: Pointer): Cardinal;
      // Retrieves the vertex index for the given index and smoothing group.
      // This redirection is necessary because a vertex might have been duplicated.
   asm
                   PUSH EBX
                   BSF EBX, EDX                  // determine smoothing group index
                   SHL EAX, 7                    // ThisIndex * SizeOf(TSmoothIndexEntry)
                   ADD EAX, ECX
                   LEA ECX, [4 * EBX + EAX]      // Address of array + vertex index + smoothing group index
                   MOV EAX, [ECX]
                   POP EBX
   end;

   //---------------------------------------------------------------------------

   procedure DuplicateVertex(Index: Integer);
      // extends the vector and normal array by one entry and duplicates the vertex data given by Index
      // the marker and texture arrays will be extended too, if necessary
   begin
      // enhance vertex array
      with mesh.Vertices do Add(Items[index]);
      mesh.Normals.Add(NullVector);
      // enhance smooth index array
      ReallocMem(SmoothIndices, (CurrentVertexCount + 1) * SizeOf(TSmoothIndexEntry));
      FillChar(SmoothIndices[CurrentVertexCount], SizeOf(TSmoothIndexEntry), $FF);
      // enhance marker array
      if (CurrentVertexCount div 8) <> ((CurrentVertexCount + 1) div 8) then begin
         ReallocMem(Marker, ((CurrentVertexCount + 1) div 8) + 1);
         Marker[(CurrentVertexCount div 8) + 1]:=0;
      end;
      with mesh.TexCoords do if Count>0 then Add(Items[index]);
      Inc(CurrentVertexCount);
   end;

   //--------------- end local functions ---------------------------------------

var
  Size: Cardinal;
  iMaterial, i, j : Integer;
  aFaceGroup : TFGVertexIndexList;
  V1, V2: TAffineVector;
  Face, SubFace, Vertex, TargetVertex: Integer;
  SmoothingGroup: Cardinal;
  CurrentIndex: Word;
  Vector1, Vector2, Normal, center : TAffineVector;
  TotalCount: Cardinal;
  vertexData : TVertexData;
  standardNormalsOrientation : Boolean;
begin
   center:=NullVector;
   with TFile3DS.Create do try
      LoadFromStream(aStream);
      // determine front face winding
      { TODO : better face winding }
      standardNormalsOrientation:=not (NormalsOrientation=mnoDefault);
      TotalCount:=0;
      for i:=0 to Objects.MeshCount-1 do with PMesh3DS(Objects.Mesh[I])^ do begin
         if IsHidden or (NVertices<3) then Continue;
         // New() just calls GetMem, but I want the memory cleared
         mesh:=TMeshObject.Create(Owner.MeshObjects);
         mesh.Name:=PMesh3DS(Objects.Mesh[I])^.Name;
         with mesh do begin
            Mode:=momFaceGroups;
            // make a copy of the vertex data, this must always be available
            Vertices.Capacity:=NVertices;
            Normals.AddNulls(NVertices);
            if NTextVerts>0 then begin
               TexCoords.Capacity:=NVertices;
               for j:=0 to NVertices-1 do begin
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
                  TexCoords.Add(PTexPoint(@TextArray[j])^);
               end;
            end else begin
               for j:=0 to NVertices-1 do
                  Vertices.Add(PAffineVector(@VertexArray[j])^);
            end;
         end;
         // allocate memory for the smoothindices and the marker array
         CurrentVertexCount:=NVertices;
         Marker:=AllocMem((NVertices div 8) + 1); // one bit for each vertex
         GetMem(SmoothIndices, NVertices * SizeOf(TSmoothIndexEntry));

         if SmoothArray=nil then begin
            // no smoothing groups to consider
            for face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  CurrentIndex:=FaceRec[Vertex];
                  // already been touched?
                  if IsVertexMarked(Marker, CurrentIndex) then begin
                     // already touched vertex must be duplicated
                     DuplicateVertex(CurrentIndex);
                     FaceRec[Vertex]:=CurrentVertexCount-1;
                     mesh.Normals[CurrentVertexCount-1]:=Normal;
                  end else begin
                     // not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end else begin
            // smoothing groups are to be considered
            for Face:=0 to NFaces-1 do with FaceArray[Face] do begin
               // normal vector for the face
               with mesh.Vertices do begin
                  VectorSubtract(Items[V1], Items[V2], vector1);
                  VectorSubtract(Items[V3], Items[V2], vector2);
               end;
               if standardNormalsOrientation then
                  Normal:=VectorCrossProduct(Vector1, Vector2)
               else Normal:=VectorCrossProduct(Vector2, Vector1);
               SmoothingGroup:=SmoothArray[Face];
               // go for each vertex in the current face
               for Vertex:=0 to 2 do begin
                  // copy current index for faster access
                  currentIndex:=FaceRec[Vertex];
                  // Has vertex already been touched?
                  if IsVertexMarked(Marker, currentIndex) then begin
                     // check smoothing group
                     if SmoothingGroup = 0 then begin
                        // no smoothing then just duplicate vertex
                        DuplicateVertex(CurrentIndex);
                        FaceRec[Vertex]:=CurrentVertexCount - 1;
                        mesh.Normals[CurrentVertexCount - 1]:=Normal;
                        // mark new vertex also as touched
                        MarkVertex(Marker, CurrentVertexCount - 1);
                     end else begin
                        // this vertex must be smoothed, check if there's already
                        // a (duplicated) vertex for this smoothing group
                        TargetVertex:=GetSmoothIndex(CurrentIndex, SmoothingGroup, SmoothIndices);
                        if TargetVertex < 0 then begin
                           // vertex has not yet been duplicated for this smoothing
                           // group, so do it now
                           DuplicateVertex(CurrentIndex);
                           FaceRec[Vertex]:=CurrentVertexCount - 1;
                           mesh.Normals[CurrentVertexCount - 1]:=Normal;
                           StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           StoreSmoothIndex(CurrentVertexCount - 1, SmoothingGroup, CurrentVertexCount - 1, SmoothIndices);
                           // mark new vertex also as touched
                           MarkVertex(Marker, CurrentVertexCount - 1);
                        end else begin
                           // vertex has already been duplicated,
                           // so just add normal vector to other vertex...
                           mesh.Normals[TargetVertex]:=VectorAdd(mesh.Normals[TargetVertex], Normal);
                           // ...and tell which new vertex has to be used from now on
                           FaceRec[Vertex]:=TargetVertex;
                        end;
                     end;
                  end else begin
                     // vertex not yet touched, so just store the normal
                     mesh.Normals[CurrentIndex]:=Normal;
                     // initialize smooth indices for this vertex
                     FillChar(SmoothIndices[CurrentIndex], SizeOf(TSmoothIndexEntry), $FF);
                     if SmoothingGroup <> 0 then
                        StoreSmoothIndex(CurrentIndex, SmoothingGroup, CurrentIndex, SmoothIndices);
                     MarkVertex(Marker, CurrentIndex);
                  end;
               end;
            end;
         end;
         FreeMem(Marker);
         FreeMem(SmoothIndices);

         Assert(mesh.Vertices.Count=CurrentVertexCount);

         // sum up all vertices to do auto centering
         Inc(TotalCount, CurrentVertexCount);
         center:=mesh.Vertices.Sum;

         // and normalize the Normals array
         mesh.Normals.Normalize;

         // now go for each material group
         // if there's no face to material assignment then just copy the
         // face definitions and rely on the default texture of the scene object
         if NMats = 0 then begin
            aFaceGroup:=TFGVertexIndexList.Create(mesh.FaceGroups);
            with aFaceGroup do begin
               MaterialName:='';
               // copy the face list
               for j:=0 to NFaces-1 do begin
                  Add(FaceArray[J].V1);
                  Add(FaceArray[J].V2);
                  Add(FaceArray[J].V3);
               end;
            end;
         end else begin
            for iMaterial:=0 to NMats - 1 do begin
               aFaceGroup:=TFGVertexIndexList.Create(mesh.FaceGroups);
               with aFaceGroup do begin
                  MaterialName:=GetOrAllocateMaterial(Materials, MatArray[iMaterial].Name);
                  // copy all vertices belonging to the current face into our index array,
                  // there won't be redundant vertices since this would mean a face has more than one
                  // material
                  // NFaces is the one from FaceGroup
                  with MatArray[iMaterial] do for j:=0 to NFaces - 1 do begin
                     Add(FaceArray[FaceIndex[J]].V1);
                     Add(FaceArray[FaceIndex[J]].V2);
                     Add(FaceArray[FaceIndex[J]].V3);
                  end;
               end;
            end;
         end;
      end;
      // Auto-centering
      if TotalCount>0 then begin
         ScaleVector(center, -1/TotalCount);
         for i:=0 to Owner.MeshObjects.Count-1 do
            Owner.MeshObjects[i].Vertices.Translate(center);
      end;
   finally
      Free;
   end;
end;

//----------------- TBaseMesh --------------------------------------------------

// Create
//
constructor TBaseMesh.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding];
   if FMeshObjects=nil then
      FMeshObjects:=TMeshObjectList.Create(Self);
   FUseMeshMaterials:=True;
end;

// Destroy
//
destructor TBaseMesh.Destroy;
begin
   FMeshObjects.Free;
   inherited Destroy;
end;

// LoadFromFile
//
procedure TBaseMesh.LoadFromFile(const Filename: String);
var
   Ext           : String;
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if FileName <> '' then begin
      MeshObjects.Clear;
      Ext:=ExtractFileExt(Filename);
      Delete(Ext, 1, 1);
      vectorFileClass:=GetVectorFileFormats.FindExt(Ext);
      if not Assigned(vectorFileClass) then
         raise EInvalidVectorFile.CreateFmt(SUnknownExtension, [Ext]);
      newVectorFile:=VectorFileClass.Create(Self);
      PrepareVectorFile(newVectorFile);
      try
         if Assigned(Scene) then Scene.BeginUpdate;
         newVectorFile.LoadFromFile(Filename);
         if Assigned(Scene) then Scene.EndUpdate;
      finally
         NewVectorFile.Free;
      end;
   end;
   StructureChanged;
end;

// LoadFromStream
//
procedure TBaseMesh.LoadFromStream(const filename : String; aStream : TStream);
var
   Ext           : String;
   newVectorFile : TVectorFile;
   vectorFileClass : TVectorFileClass;
begin
   if FileName <> '' then begin
      MeshObjects.Clear;
      Ext:=ExtractFileExt(Filename);
      Delete(Ext, 1, 1);
      vectorFileClass:=GetVectorFileFormats.FindExt(Ext);
      if not Assigned(vectorFileClass) then
         raise EInvalidVectorFile.CreateFmt(SUnknownExtension, [Ext]);
      newVectorFile:=VectorFileClass.Create(Self);
      try
         if Assigned(Scene) then Scene.BeginUpdate;
         newVectorFile.LoadFromStream(aStream);
         if Assigned(Scene) then Scene.EndUpdate;
      finally
         NewVectorFile.Free;
      end;
   end;
   StructureChanged;
end;

// GetExtents
//
procedure TBaseMesh.GetExtents(var min, max : TAffineVector);
var
   i, k : Integer;
   lMin, lMax : TAffineVector;
const
   cBigValue : Single = 1e50;
   cSmallValue : Single = -1e50;
begin
   SetVector(min, cBigValue, cBigValue, cBigValue);
   SetVector(max, cSmallValue, cSmallValue, cSmallValue);
   for i:=0 to MeshObjects.Count-1 do begin
      TMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
      for k:=0 to 2 do begin
          if lMin[k]<min[k] then min[k]:=lMin[k];
          if lMax[k]>max[k] then max[k]:=lMax[k];
      end;
   end;
end;

// SetMaterialLibrary
//
procedure TBaseMesh.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   if FMaterialLibrary<>val then begin
      if Assigned(FMaterialLibrary) then begin
         FreeList(False);
         FMaterialLibrary.RemoveFreeNotification(Self);
      end;
      FMaterialLibrary:=val;
      if Assigned(FMaterialLibrary) then
         FMaterialLibrary.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetNormalsOrientation
//
procedure TBaseMesh.SetNormalsOrientation(const val : TMeshNormalsOrientation);
begin
   if val<>FNormalsOrientation then begin
      FNormalsOrientation:=val;
      StructureChanged;
   end;
end;

// Notification
//
procedure TBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FMaterialLibrary) then
      MaterialLibrary:=nil;
   inherited;
end;

// DoDestroyList
//
procedure TBaseMesh.DoDestroyList(glsceneOnly : Boolean);
begin
   if Assigned(FMaterialLibrary) then
      MaterialLibrary.DestroyHandles(glsceneOnly);
   inherited;
end;

// PrepareVectorFile
//
procedure TBaseMesh.PrepareVectorFile(aFile : TVectorFile);
begin
   aFile.NormalsOrientation:=NormalsOrientation;
end;

// SetUseMeshMaterials
//
procedure TBaseMesh.SetUseMeshMaterials(const val : Boolean);
begin
   if val<>FUseMeshMaterials then begin
      FUseMeshMaterials:=val;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TBaseMesh.BuildList(var rci : TRenderContextInfo);
var
   states : TGLStates;
begin
   if rci.materialLibrary<>nil then begin
      states:=rci.currentStates;
      glPushAttrib(GL_ENABLE_BIT);
      MeshObjects.BuildList(rci);
      glPopAttrib;
      rci.currentStates:=states;
   end else MeshObjects.BuildList(rci);
end;

// DoRender
//
procedure TBaseMesh.DoRender(var rci : TRenderContextInfo);
begin
   // set winding
   case FNormalsOrientation of
      mnoDefault : glFrontFace(GL_CCW);
      mnoInvert : glFrontFace(GL_CW);
   else
      Assert(False);
   end;
   if UseMeshMaterials and Assigned(MaterialLibrary) then
      rci.materialLibrary:=MaterialLibrary
   else rci.materialLibrary:=nil;
   MeshObjects.PrepareBuildList(rci);
  	Material.Apply(rci);
   if osDirectDraw in ObjectStyle then
      BuildList(rci)
   else glCallList(GetHandle(rci));
   Material.UnApply(rci);
   rci.materialLibrary:=nil;
   if FNormalsOrientation<>mnoDefault then
      glFrontFace(GL_CCW);
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// StructureChanged
//
procedure TBaseMesh.StructureChanged;
begin
   MeshObjects.Prepare;
   inherited;
end;

// ------------------
// ------------------ TFreeForm ------------------
// ------------------

// Create
//
constructor TFreeForm.Create(AOwner:TComponent);
begin
   inherited;
   FUseMeshMaterials:=True;
end;

// Destroy
//
destructor TFreeForm.Destroy;
begin
   inherited Destroy;
end;

// ------------------
// ------------------ TActorAnimation ------------------
// ------------------

// Create
//
constructor TActorAnimation.Create(Collection : TCollection);
begin
	inherited Create(Collection);
end;

destructor TActorAnimation.Destroy;
begin
	inherited Destroy;
end;

procedure TActorAnimation.Assign(Source: TPersistent);
begin
	if Source is TActorAnimation then begin
      FName:=TActorAnimation(Source).FName;
      FStartFrame:=TActorAnimation(Source).FStartFrame;
      FEndFrame:=TActorAnimation(Source).FEndFrame;
	end;
	inherited Destroy;
end;

// GetDisplayName
//
function TActorAnimation.GetDisplayName : String;
begin
	Result:=Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

// SetStartFrame
//
procedure TActorAnimation.SetStartFrame(const val : Integer);
begin
   if val<0 then
      FStartFrame:=0
   else if val>=TActorAnimations(Collection).Owner.FrameCount then
      FStartFrame:=TActorAnimations(Collection).Owner.FrameCount-1
   else FStartFrame:=val;
   if FStartFrame>FEndFrame then
      FEndFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TActorAnimation.SetEndFrame(const val : Integer);
begin
   if val<0 then
      FEndFrame:=0
   else if val>=TActorAnimations(Collection).Owner.FrameCount then
      FEndFrame:=TActorAnimations(Collection).Owner.FrameCount-1
   else FStartFrame:=val;
   if FStartFrame>FEndFrame then
      FStartFrame:=FEndFrame;
end;

// SetAsString
//
procedure TActorAnimation.SetAsString(const val : String);
var
   sl : TStringList;
begin
   sl:=TStringList.Create;
   try
      sl.CommaText:=val;
      Assert(sl.Count=3);
      FName:=sl[0];
      FStartFrame:=StrToInt(sl[1]);
      FEndFrame:=StrToInt(sl[2]);
   finally
      sl.Free;
   end;
end;

// GetAsString
//
function TActorAnimation.GetAsString : String;
begin
   Result:=Format('%s,%d,%d', [FName, FStartFrame, FEndFrame]);
end;

// ------------------
// ------------------ TActorAnimations ------------------
// ------------------

// Create
//
constructor TActorAnimations.Create(AOwner : TActor);
begin
	Owner:=AOwner;
	inherited Create(TActorAnimation);
end;

// GetOwner
//
function TActorAnimations.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

// SetItems
//
procedure TActorAnimations.SetItems(index : Integer; const val : TActorAnimation);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TActorAnimations.GetItems(index : Integer) : TActorAnimation;
begin
	Result:=TActorAnimation(inherited Items[index]);
end;

// Add
//
function TActorAnimations.Add: TActorAnimation;
begin
	Result:=(inherited Add) as TActorAnimation;
end;

// FindItemID
//
function TActorAnimations.FindItemID(ID: Integer): TActorAnimation;
begin
	Result:=(inherited FindItemID(ID)) as TActorAnimation;
end;

// FindName
//
function TActorAnimations.FindName(const aName : String) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do if CompareText(Items[i].Name, aName)=0 then begin
      Result:=Items[i];
      Break;
   end;
end;

// FindFrame
//
function TActorAnimations.FindFrame(frame : Integer) : TActorAnimation;
var
   i : Integer;
begin
	Result:=nil;
   for i:=0 to Count-1 do with Items[i] do
      if (StartFrame<=frame) and (EndFrame>=frame) then begin
         Result:=Items[i];
         Break;
      end;
end;

// SetToStrings
//
procedure TActorAnimations.SetToStrings(aStrings : TStrings);
var
   i : Integer;
begin
   with aStrings do begin
      BeginUpdate;
      Clear;
      for i:=0 to Self.Count-1 do
         Add(Self.Items[i].Name);
      EndUpdate;
   end;
end;

// SaveToStream
//
procedure TActorAnimations.SaveToStream(aStream : TStream);
var
   i : Integer;
begin
   WriteCRLFString(aStream, cAAFHeader);
   WriteCRLFString(aStream, IntToStr(Count));
   for i:=0 to Count-1 do
      WriteCRLFString(aStream, Items[i].AsString);
end;

// LoadFromStream
//
procedure TActorAnimations.LoadFromStream(aStream : TStream);
var
   i, n : Integer;
begin
   Clear;
   Assert(ReadCRLFString(aStream)=cAAFHeader);
   n:=StrToInt(ReadCRLFString(aStream));
   for i:=0 to n-1 do
      Add.AsString:=ReadCRLFString(aStream);
end;

// SaveToFile
//
procedure TActorAnimations.SaveToFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmCreate);
   try
      SaveToStream(fs);
   finally
      fs.Free;
   end;
end;

// LoadFromFile
//
procedure TActorAnimations.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyWrite);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// ------------------
// ------------------ TActor ------------------
// ------------------

// Create
//
constructor TActor.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FFrameInterpolation:=afpLinear;
   FAnimationMode:=aamNone;
   FInterval:=100; // 10 animation frames per second
   FAnimations:=TActorAnimations.Create(Self);
end;

// Destroy
//
destructor TActor.Destroy;
begin
   FAnimations.Free;
   inherited Destroy;
end;

// SetCurrentFrame
//
procedure TActor.SetCurrentFrame(Value: Integer);
begin
   if Value<>CurrentFrame then begin
      if Value>FrameCount-1 then
         FCurrentFrame:=FrameCount-1
      else if Value<0 then
         FCurrentFrame:=0
      else FCurrentFrame:=Value;
      FCurrentFrameDelta:=0;
      case AnimationMode of
         aamPlayOnce :
            if CurrentFrame=EndFrame then FAnimationMode:=aamNone;
         aamBounceForward :
            if CurrentFrame=EndFrame then FAnimationMode:=aamBounceBackward;
         aamBounceBackward :
            if CurrentFrame=StartFrame then FAnimationMode:=aamBounceForward;
      end;
      StructureChanged;
      if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
   end;
end;

// SetStartFrame
//
procedure TActor.SetStartFrame(Value: Integer);
begin
   if (Value>=0) and (Value<FrameCount) and (Value<>StartFrame) then
      FStartFrame:=Value;
   if EndFrame<StartFrame then
      FEndFrame:=FStartFrame;
   if CurrentFrame<StartFrame then
      CurrentFrame:=FStartFrame;
end;

// SetEndFrame
//
procedure TActor.SetEndFrame(Value: Integer);
begin
   if (Value>=0) and (Value<FrameCount) and (Value<>EndFrame) then
      FEndFrame:=Value;
   if CurrentFrame>EndFrame then
      CurrentFrame:=FEndFrame;
end;

// SetAnimations
//
procedure TActor.SetAnimations(const val : TActorAnimations);
begin
   FAnimations.Assign(val);
end;

// NextFrameIndex
//
function TActor.NextFrameIndex : Integer;
begin
   case AnimationMode of
      aamNone, aamPlayOnce, aamLoop, aamBounceForward : begin
         Result:=CurrentFrame+1;
         if Result>EndFrame then begin
            Result:=StartFrame+(Result-EndFrame-1);
            if Result>EndFrame then
               Result:=EndFrame;
         end;
      end;
      aamBounceBackward : begin
         Result:=CurrentFrame-1;
         if Result<StartFrame then begin
            Result:=EndFrame-(StartFrame-Result-1);
            if Result<StartFrame then
               Result:=StartFrame;
         end;
      end;
   else
      Result:=CurrentFrame;
      Assert(False);
   end;
end;

// NextFrame
//
procedure TActor.NextFrame(n : Integer = 1);
begin
   while n>0 do begin
      CurrentFrame:=NextFrameIndex;
      Dec(n);
      if Assigned(FOnEndFrameReached) and (CurrentFrame=EndFrame) then
         FOnEndFrameReached(Self);
      if Assigned(FOnStartFrameReached) and (CurrentFrame=StartFrame) then
         FOnStartFrameReached(Self);
   end;
end;

// PrevFrame
//
procedure TActor.PrevFrame(n : Integer = 1);
var
   value: Integer;
begin
   value:=FCurrentFrame-N;
   if value<FStartFrame then begin
      Value:=FEndFrame-(FStartFrame - Value);
      if Value<FStartFrame then
         Value:=FStartFrame;
   end;
   CurrentFrame:=Value;
end;

// BuildList
//
procedure TActor.BuildList(var rci : TRenderContextInfo);
begin
   case FrameInterpolation of
      afpLinear : // Linear frame interpolation
         MeshObjects.Lerp(CurrentFrame, NextFrameIndex, CurrentFrameDelta);
   else
      // afpNone
      MeshObjects.MorphTo(CurrentFrame);
   end;
   inherited;
end;

// LoadFromFile
//
procedure TActor.LoadFromFile(const Filename: String);
begin
   inherited LoadFromFile(fileName);
   FFrameCount:=MeshObjects.MorphTargetCount;
   FStartFrame:=0;
   FEndFrame:=FFrameCount-1;
   FCurrentFrame:=0;
   if Assigned(FOnFrameChanged) then FOnFrameChanged(Self);
end;

// DoProgress
//
procedure TActor.DoProgress(const deltaTime, newTime : Double);
var
   fDelta : Single;
begin
   inherited;
   if (AnimationMode<>aamNone) and (StartFrame<>EndFrame) and (FrameCount>1) and (Interval>0) then begin
      FCurrentFrameDelta:=FCurrentFrameDelta+(deltaTime*1000)/FInterval;
      if FCurrentFrameDelta>1 then begin
         // we need to step on
         fDelta:=Frac(FCurrentFrameDelta);
         NextFrame(Trunc(FCurrentFrameDelta));
         FCurrentFrameDelta:=fDelta;
         StructureChanged;
      end else if FrameInterpolation<>afpNone then
         StructureChanged;
   end;
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(const animationName : String);
begin
   SwitchToAnimation(Animations.FindName(animationName));
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(animationIndex : Integer);
begin
   if (animationIndex>=0) and (animationIndex<Animations.Count) then
      SwitchToAnimation(Animations[animationIndex]);
end;

// SwitchToAnimation
//
procedure TActor.SwitchToAnimation(anAnimation : TActorAnimation);
begin
   if Assigned(anAnimation) then begin
      StartFrame:=anAnimation.StartFrame;
      EndFrame:=anAnimation.EndFrame;
      CurrentFrame:=StartFrame;
   end;
end;

// CurrentAnimation
//
function TActor.CurrentAnimation : String;
var
   aa : TActorAnimation;
begin
   aa:=Animations.FindFrame(CurrentFrame);
   if Assigned(aa) then
      Result:=aa.Name
   else Result:='';
end;

// Synchronize
//
procedure TActor.Synchronize(referenceActor : TActor);
begin
   if Assigned(referenceActor) then begin
      FStartFrame:=referenceActor.StartFrame;
      FEndFrame:=referenceActor.EndFrame;
      CurrentFrame:=referenceActor.CurrentFrame;
      CurrentFrameDelta:=referenceActor.CurrentFrameDelta;
      AnimationMode:=referenceActor.AnimationMode;
      FrameInterpolation:=referenceActor.FrameInterpolation;
   end;
end;

// ------------------
// ------------------ TGLMD2VectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLMD2VectorFile.LoadFromStream(aStream : TStream);
var
   i, j : Integer;
   MD2File : TFileMD2;
   mesh : TMorphableMeshObject;
   faceGroup : TFGIndexTexCoordList;
   morphTarget : TMeshMorphTarget;
begin
   MD2File:=TFileMD2.Create;
   MD2File.LoadFromStream(aStream);
   try
      // retrieve mesh data
      mesh:=TMorphableMeshObject.Create(Owner.MeshObjects);
      with mesh, MD2File do begin
         Mode:=momFaceGroups;
         faceGroup:=TFGIndexTexCoordList.Create(FaceGroups);
         with faceGroup do begin
            MaterialName:='';
            VertexIndices.Capacity:=m_iTriangles*3;
            TexCoords.Capacity:=m_iTriangles*3;
            // copy the face list
            for i:=0 to m_iTriangles-1 do with IndexList(m_index_list)[i] do begin
               Add(a, a_s, -a_t);
               Add(b, b_s, -b_t);
               Add(c, c_s, -c_t);
            end;
         end;
         // retrieve frames data (morph targets)
         for i:=0 to m_iFrames-1 do begin
            morphTarget:=TMeshMorphTarget.Create(MorphTargets);
            with morphTarget do begin
               Name:='Frame'+IntToStr(i);
               Vertices.Capacity:=m_iVertices;
               for j:=0 to m_iVertices-1 do
                  with VertList(frameList(m_frame_list)[i].vertex)[j] do
                     Vertices.Add(x, y, z);
               BuildNormals(faceGroup.VertexIndices, momTriangles);
            end;
         end;
      end;
      if GetOwner is TActor then with TActor(GetOwner).Animations do begin
         Clear;
         with MD2File do for i:=0 to frameNames.Count-1 do with Add do begin
            Name:=frameNames[i];
            FStartFrame:=Integer(frameNames.Objects[i]);
            if i<frameNames.Count-1 then
               FEndFrame:=Integer(frameNames.Objects[i+1])-1
            else FEndFrame:=m_iFrames-1;
         end;
      end;
   finally
      MD2File.Free;
   end;
end;

// ------------------
// ------------------ TGLTINVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLTINVectorFile.LoadFromStream(aStream : TStream);
var
   i : Integer;
   sl, tl : TStringList;
   mesh : TMeshObject;
   v1, v2, v3, n : TAffineVector;
begin
   sl:=TStringList.Create;
   tl:=TStringList.Create;
   try
      sl.LoadFromStream(aStream);
      mesh:=TMeshObject.Create(Owner.MeshObjects);
      mesh.Mode:=momTriangles;
      for i:=0 to sl.Count-1 do if Copy(sl[i], 1, 2)='t ' then begin
         tl.CommaText:=Trim(Copy(sl[i], 3, MaxInt));
         if tl.Count=9 then begin
            SetVector(v1, StrToFloat(tl[0]), StrToFloat(tl[1]), StrToFloat(tl[2]));
            SetVector(v2, StrToFloat(tl[3]), StrToFloat(tl[4]), StrToFloat(tl[5]));
            SetVector(v3, StrToFloat(tl[6]), StrToFloat(tl[7]), StrToFloat(tl[8]));
            mesh.Vertices.Add(v1, v2, v3);
            n:=CalcPlaneNormal(v1, v2, v3);
            mesh.Normals.Add(n, n, n);
         end;
      end;
   finally
      tl.Free;
      sl.Free;
   end;
end;

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

// LoadFromStream
//
procedure TGLSTLVectorFile.LoadFromStream(aStream : TStream);
var
   i : Integer;
   mesh : TMeshObject;
   header : TSTLHeader;
   dataFace : TSTLFace;
   calcNormal : TAffineVector;
begin
   mesh:=TMeshObject.Create(Owner.MeshObjects);
   mesh.Mode:=momTriangles;
   aStream.Read(header, SizeOf(TSTLHeader));
   for i:=0 to header.nbFaces-1 do begin
      aStream.Read(dataFace, SizeOf(TSTLFace));
      with dataFace, mesh do begin
         // STL faces have a normal, but do not necessarily follow the winding rule,
         // so we must first determine if the triangle is properly oriented
         // and rewind it properly if not...
         calcNormal:=CalcPlaneNormal(v1, v2, v3);
         if VectorDotProduct(calcNormal, normal)>0 then
            Vertices.Add(v1, v2, v3)
         else Vertices.Add(v3, v2, v1);
         Normals.Add(normal, normal, normal);
      end;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterVectorFileFormat('md2', 'Quake II model files', TGLMD2VectorFile);
   RegisterVectorFileFormat('3ds', '3D Studio files', TGL3DSVectorFile);
   RegisterVectorFileFormat('prj', '3D Studio project files', TGL3DSVectorFile);
   RegisterVectorFileFormat('tin', 'Triangular Irregular Network', TGLTINVectorFile);
   RegisterVectorFileFormat('stl', 'Stereolithography files', TGLSTLVectorFile);
   RegisterClasses([TFreeForm, TActor]);

finalization

   vVectorFileFormats.Free;

end.

