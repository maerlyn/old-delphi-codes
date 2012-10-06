// GLObjects
{: Implementation of standard scene objects plus some management routines.<p>

   All objects declared in this unit are part of the basic GLScene package,
   these are only simple objects and should be kept simple and lightweight.<br>
   More complex or more specialized versions should be placed in dedicated
   units where they can grow and prosper untammed.<p>

   TODO : Classes on the move :<ul>
      <li>TSpaceText will move to their own units
      <li>TTeapot will move to a 'doodad objects' unit
   </ul>

	<b>Historique : </b><font size=-1><ul>
      <li>27/02/01 - Egg - Fix in TCube texcoords, added TFrustrum (thx Robin Gerrets)
      <li>22/02/01 - Egg - Added AxisAlignedDimensions overrides by Uwe Raabe
      <li>05/02/01 - Egg - Minor changes to TCube.BuildList
      <li>21/01/01 - Egg - BaseProjectionMatrix fix for THUDSprite (picking issue),
                           THUDSprite moved to GLHUDObjects
      <li>14/01/01 - Egg - Fixed TSphere texture coordinates
      <li>13/01/01 - Egg - TSprite matrix compatibility update
      <li>09/01/01 - Egg - TSpaceText now handles its TFont.OnFontChange
      <li>08/01/01 - Egg - Added TGLLinesNode (color support) and Node size control
      <li>22/12/00 - Egg - Sprites are no longer texture enabled by default,
                           updated TSprite.BuildList to work with new matrices
      <li>14/11/00 - Egg - Added TDummyCube.Destroy (thx Airatz)
      <li>08/10/00 - Egg - Fixed call to wglUseFontOutlines
      <li>06/08/00 - Egg - TRotationSolid renamed to TRevolutionSolid & moved to GLExtrusion
      <li>04/08/00 - Egg - Fixed sphere main body texture coords + slight speedup
      <li>02/08/00 - Egg - Added TPolygonBase
      <li>19/07/00 - Egg - Added THUDSprite
      <li>18/07/00 - Egg - Added TRevolutionSolid
      <li>15/07/00 - Egg - Code reduction and minor speedup for all quadric objects,
                           Added TLineBase (split of TLines),
                           TDummyCube now uses osDirectDraw instead of special behaviour
      <li>13/07/00 - Egg - Added TArrowLine (code by Aaron Hochwimmer)
      <li>28/06/00 - Egg - Support for "ObjectStyle"
      <li>23/06/00 - Egg - Reduced default Loop count for TDisk
      <li>18/06/00 - Egg - TMesh and accompanying stuff moved to GLMesh
      <li>14/06/00 - Egg - Added Capacity to TVertexList
      <li>09/06/00 - Egg - First row of Geometry-related upgrades
      <li>08/06/00 - Egg - Added ReleaseFontManager, fixed TSpaceText DestroyList,
      <li>01/06/00 - Egg - Added TAnnulus (code by Aaron Hochwimmer)
      <li>29/05/00 - Egg - TLines now uses TGLNode/TGLNodes
      <li>28/05/00 - Egg - Added persistence ability to TLines,
                           Added defaults for all TLines properties
      <li>27/05/00 - Egg - Moved in RogerCao's TLines object, added a TLineNode
                           class (currently private) and various enhancements + fixes,
                           DodecahedronBuildList now available as a procedure,
                           CubeWireframeBuildList now available as a procedure
      <li>26/05/00 - RoC - Added division property to TLines, and Spline supported
      <li>26/05/00 - Egg - Moved vectorfile remnants to GLVectorFiles
      <li>14/05/00 - Egg - Removed Top/Bottom checks for TSphere,
                           Added mmTriangleStrip support in CalcNormals
      <li>08/05/00 - Egg - Uncommented DisableAutoTexture in TSpaceText.BuildList
      <li>07/05/00 - RoC - TLines added, to show a list of vertex
		<li>26/04/00 - Egg - Reactivated stuff in SetupQuadricParams (thanks Nelson Chu)
		<li>18/04/00 - Egg - Overriden TDummyCube.Render
		<li>16/04/00 - Egg - FontManager now published and auto-creating
		<li>12/04/00 - Egg - Added TCylinderBase.Loops (fixes a bug, thanks Uwe)
      <li>24/03/00 - Egg - Added Rotation to TSprite, fixed sprite size
		<li>20/03/00 - Egg - Enhanced FontManager
		<li>17/03/00 - Egg - Fixed SpaceText glBaseList bug,
									TSprite now uses a transposition of the globalmatrix
		<li>16/03/00 - Egg - Enhanced TFontManager to allow lower quality
		<li>14/03/00 - Egg - Added subobjects Barycenter support for TDummyCube
      <li>09/02/00 - Egg - ObjectManager stuff moved to GLSceneRegister,
                           FreeForm and vector file stuff moved to new GLVectorFileObjects
      <li>08/02/00 - Egg - Added TDummyCube
      <li>05/02/00 - Egg - Javadocisation, fixes and enhancements :
                           TVertexList.AddVertex, "default"s to properties
   </ul></font>
}
unit GLObjects;

// GLObjects   - implementation of scene objects plus some management routines
// version     - 0.5.8
// 05-JAN-2000 ml: adjustment of loader routine for 3DS files
// 04-JAN-2000 ml: included new 3DS classes

{$R-}

interface

uses Windows, Classes, Geometry, GLScene, GLTexture, GLMisc, Graphics,
   OpenGL12, SysUtils, extctrls;

type

	// TDummyCube
	//
	{: A simple cube, invisible at run-time.<p>
		DummyCube's barycenter is their children's barycenter. }
	TDummyCube = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
			FCubeSize : TGLFloat;
			FEdgeColor : TGLColor;
			FVisibleAtRunTime : Boolean;

		protected
			{ Protected Declarations }
			procedure SetCubeSize(const val : TGLFloat);
			procedure SetEdgeColor(const val : TGLColor);
			procedure SetVisibleAtRunTime(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

			procedure BuildList(var rci : TRenderContextInfo); override;
			function BarycenterAbsolutePosition : TVector; override;

		published
			{ Published Declarations }
			property CubeSize : TGLFloat read FCubeSize write SetCubeSize;
			property EdgeColor : TGLColor read FEdgeColor write SetEdgeColor;
			property VisibleAtRunTime : Boolean read FVisibleAtRunTime write SetVisibleAtRunTime default False;

	end;

   // Plane
   //
   {: A simple plane object.<p>
      Note that a plane is always made of a single quad (two triangles) and the
      tiling is only applied to texture coordinates. }
	TPlane = class (TGLSceneObject)
	   private
			{ Private Declarations }
	      FXOffset, FYOffset : TGLFloat;
			FWidth, FHeight : TGLFloat;
		   FXTiles, FYTiles: Cardinal;

		protected
			{ Protected Declarations }
		   procedure SetHeight(AValue: TGLFloat);
		   procedure SetWidth(AValue: TGLFloat);
		   procedure SetXOffset(const Value: TGLFloat);
		   procedure SetXTiles(const Value: Cardinal);
		   procedure SetYOffset(const Value: TGLFloat);
		   procedure SetYTiles(const Value: Cardinal);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

		   procedure BuildList(var rci : TRenderContextInfo); override;

		   procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

		published
			{ Public Declarations }
			property Height: TGLFloat read FHeight write SetHeight;
         property Width: TGLFloat read FWidth write SetWidth;
         property XOffset: TGLFloat read FXOffset write SetXOffset;
         property XTiles: Cardinal read FXTiles write SetXTiles default 1;
         property YOffset: TGLFloat read FYOffset write SetYOffset;
         property YTiles: Cardinal read FYTiles write SetYTiles default 1;
   end;

	// Sprite
	//
	{: A rectangular area, perspective projected, but always facing the camera.<p>
      A TSprite is perspective projected and as such is scaled with distance,
      if you want a 2D sprite that does not get scaled, see THUDSprite. }
	TSprite = class (TGLSceneObject)
		private
			{ Private Declarations }
			FWidth : TGLFloat;
			FHeight : TGLFloat;
			FRotation : TGLFloat;
         FAlphaChannel : Single;

		protected
			{ Protected Declarations }
			procedure SetWidth(const val : TGLFloat);
			procedure SetHeight(const val : TGLFloat);
         procedure SetRotation(const val : TGLFloat);
         procedure SetAlphaChannel(const val : Single);
         function StoreAlphaChannel : Boolean;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

			procedure SetSize(const width, height : TGLFloat);
			//: Set width and height to "size"
			procedure SetSquareSize(const size : TGLFloat);

		published
			{ Published Declarations }
         {: Sprite Width in 3D world units. }
			property Width : TGLFloat read FWidth write SetWidth;
         {: Sprite Height in 3D world units. }
			property Height : TGLFloat read FHeight write SetHeight;
			{: This the ON-SCREEN rotation of the sprite.<p>
            Rotatation=0 is handled faster. }
         property Rotation : TGLFloat read FRotation write SetRotation;
         {: If different from 1, this value will replace that of Diffuse.Alpha }
         property AlphaChannel : Single read FAlphaChannel write SetAlphaChannel stored StoreAlphaChannel;
	end;

   // TLineNodesAspect
   //
   {: Possible aspects for the nodes of a TLine. }
   TLineNodesAspect = (lnaInvisible, lnaAxes, lnaCube, lnaDodecahedron);

   // TLineSplineMode
   //
   {: Available spline modes for a TLine. }
   TLineSplineMode = (lsmLines, lsmCubicSpline);

   // TGLLinesNode
   //
   {: Specialized Node for use in a TLines objects.<p>
      Adds a Color property (TGLColor). }
   TGLLinesNode = class(TGLNode)
      private
			{ Private Declarations }
         FColor : TGLColor;

		protected
			{ Protected Declarations }
         procedure SetColor(const val : TGLColor);
         procedure OnColorChange(sender : TObject);
         function StoreColor : Boolean;

      public
         { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;
	      procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }

         {: The node color.<p>
            Can also defined the line color (interpolated between nodes) if
            loUseNodeColorForLines is set (in TLines). }
         property Color : TGLColor read FColor write SetColor stored StoreColor;
   end;

   // TGLLinesNodes
   //
   {: Specialized collection for Nodes in a TLines objects.<p>
      Stores TGLLinesNode items. }
   TGLLinesNodes = class(TGLNodes)
      public
        { Public Declarations }
	      constructor Create(AOwner : TComponent);

         procedure NotifyChange; override;
   end;

   // TLineBase
   //
   {: Base class for line objects.<p>
      Introduces line style properties (width, color...). }
   TLineBase = class(TGLImmaterialSceneObject)
      private
			{ Private Declarations }
         FLineColor : TGLColor;
         FLinePattern : TGLushort;
         FLineWidth : Single;
         FAntiAliased : Boolean;

		protected
			{ Protected Declarations }
         procedure SetLineColor(const value: TGLColor);
         procedure SetLinePattern(const value: TGLushort);
         procedure SetLineWidth(const val : Single);
         function StoreLineWidth : Boolean;
         procedure SetAntiAliased(const val : Boolean);

         {: Setup OpenGL states according to line style.<p>
            You must call RestoreLineStyle after drawing your lines.<p>
            You may use nested calls with SetupLineStyle/RestoreLineStyle. }
         procedure SetupLineStyle;
         {: Restore OpenGL states, must follow a SetupLineStyle }
         procedure RestoreLineStyle;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

      published
			{ Published Declarations }
         {: Indicates if OpenGL should smooth line edges.<p>
            Smoothed lines looks better but are poorly implemented in most OpenGL
            drivers and take *lots* of rendering time. }
         property AntiAliased : Boolean read FAntiAliased write SetAntiAliased default False;
         {: Default color of the lines. }
         property LineColor: TGLColor read FLineColor write SetLineColor;
         {: Bitwise line pattern.<p>
            For instance $FFFF (65535) is a white line (stipple disabled), $0000
            is a black line, $CCCC is the stipple used in axes and dummycube, etc. }
         property LinePattern: TGLushort read FLinePattern write SetLinePattern default $FFFF;
         {: Default width of the lines. }
         property LineWidth : Single read FLineWidth write SetLineWidth stored StoreLineWidth;
         property Visible;
   end;

   // TLinesOptions
   //
   TLinesOption = (loUseNodeColorForLines);
   TLinesOptions = set of TLinesOption;

   // TLines
   //
   {: Set of 3D line segments.<p>
      You define a 3D Line by adding its nodes in the "Nodes" property. The line
      may be rendered as a set of segment or as a curve (nodes then act as spline
      control points).<p>
      Alternatively, you can also use it to render a set of spacial nodes (points
      in space), just make the lines transparent and the nodes visible by picking
      the node aspect that suits you. }
   TLines = class(TLineBase)
      private
			{ Private Declarations }
         FNodes : TGLLinesNodes;
         FNodesAspect : TLineNodesAspect;
         FNodeColor : TGLColor;
         FDivision : Integer;
         FSplineMode : TLineSplineMode;
         FOptions : TLinesOptions;
         FNodeSize : Single;
         FOldNodeColor : TColorVector;

		protected
			{ Protected Declarations }
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetNodesAspect(const value : TLineNodesAspect);
         procedure SetNodeColor(const value: TGLColor);
         procedure OnNodeColorChanged(sender : TObject);
         procedure SetDivision(const value: Integer);
         procedure SetNodes(const aNodes : TGLLinesNodes);
         procedure SetOptions(const val : TLinesOptions);
         procedure SetNodeSize(const val : Single);
         function StoreNodeSize : Boolean;

         procedure DrawNode(X, Y, Z: Single; Color: TGLColor);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

      published
			{ Published Declarations }
         {: The nodes list.<p> }
         property Nodes : TGLLinesNodes read FNodes write SetNodes;

         {: Default color for nodes.<p>
            lnaInvisible and lnaAxes ignore this setting. }
         property NodeColor: TGLColor read FNodeColor write SetNodeColor;
         {: Default aspect of line nodes.<p>
            May help you materialize nodes, segments and control points. }
         property NodesAspect: TLineNodesAspect read FNodesAspect write SetNodesAspect default lnaAxes;
         {: Size for the various node aspects. }
         property NodeSize : Single read FNodeSize write SetNodeSize stored StoreNodeSize;

         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p> }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

         {: Rendering options for the line.<p>
            <ul>
            <li>loUseNodeColorForLines: if set lines will be drawn using node
               colors (and color interpolation between nodes), if not, LineColor
               will be used (single color).
            </ul> }
         property Options : TLinesOptions read FOptions write SetOptions;
   end;

	TCubePart  = (cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight);
	TCubeParts = set of TCubePart;

   // TCube
   //
   {: A simple cube object.<p>
      This cube use the same material for each of its faces, ie. all faces look
      the same. If you want a multi-material cube, use a mesh in conjunction
      with a TFreeForm and a material library. }
   TCube = class (TGLSceneObject)
		private
			{ Private Declarations }
         FCubeWidth, FCubeHeight, FCubeDepth : TGLFloat;
         FParts : TCubeParts;
         FNormalDirection : TNormalDirection;
         procedure SetCubeWidth(AValue: TGLFloat);
         procedure SetCubeHeight(AValue: TGLFloat);
         procedure SetCubeDepth(AValue: TGLFloat);
         procedure SetParts(AValue: TCubeParts);
         procedure SetNormalDirection(AValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

      published
			{ Published Declarations }
         property CubeWidth: TGLFloat read FCubeWidth write SetCubeWidth stored False;
         property CubeHeight: TGLFloat read FCubeHeight write SetCubeHeight stored False;
         property CubeDepth: TGLFloat read FCubeDepth write SetCubeDepth stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TCubeParts read FParts write SetParts default [cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
   end;

   // TFrustrumParts
   //
	TFrustrumPart = (fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight);
   TFrustrumParts = set of TFrustrumPart;

const
	cAllFrustrumParts = [fpTop, fpBottom, fpFront, fpBack, fpLeft, fpRight];

type
   // TFrustrum
   //
   { A frustrum is a pyramid with the top chopped off.<p>
      The height of the imaginary pyramid is ApexHeight, the height of the
      frustrum is Height. If ApexHeight and Height are the same, the frustrum
      degenerates into a pyramid.<br>
      Height cannot be greater than ApexHeight. }
   TFrustrum = class(TGLSceneObject)
      private
			{ Private Declarations }
         FApexHeight, FBaseDepth, FBaseWidth, FHeight: TGLFloat;
         FParts: TFrustrumParts;
         FNormalDirection: TNormalDirection;
         procedure SetApexHeight(AValue: TGLFloat);
         procedure SetBaseDepth(AValue: TGLFloat);
         procedure SetBaseWidth(AValue: TGLFloat);
         procedure SetHeight(AValue: TGLFloat);
         procedure SetParts(AValue: TFrustrumParts);
         procedure SetNormalDirection(AValue: TNormalDirection);

      protected
			{ Protected Declarations }
         procedure DefineProperties(Filer: TFiler); override;
         procedure ReadData(Stream: TStream);
         procedure WriteData(Stream: TStream);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci: TRenderContextInfo); override;
         procedure Assign(Source: TPersistent); override;

         function TopDepth: TGLFloat;
         function TopWidth: TGLFloat;

      published
			{ Published Declarations }
         property ApexHeight: TGLFloat read FApexHeight write SetApexHeight stored False;
         property BaseDepth: TGLFloat read FBaseDepth write SetBaseDepth stored False;
         property BaseWidth: TGLFloat read FBaseWidth write SetBaseWidth stored False;
         property Height: TGLFloat read FHeight write SetHeight stored False;
         property NormalDirection: TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
         property Parts: TFrustrumParts read FParts write SetParts default cAllFrustrumParts;
   end;

   // TNormalSmoothing
   //
   {: Determines how and if normals are smoothed.<p>
      - nsFlat : facetted look<br>
      - nsSmooth : smooth look<br>
      - nsNone : unlighted rendering, usefull for decla texturing }
   TNormalSmoothing = (nsFlat, nsSmooth, nsNone);

   // TQuadricObject
   //
   {: Base class for quadric objects.<p>
      Introduces some basic Quadric interaction functions (the actual quadric
      math is part of the GLU library). }
   TQuadricObject = class(TGLSceneObject)
      private
         { Private Declarations }
         FNormals : TNormalSmoothing;
         FNormalDirection : TNormalDirection;

      protected
         { Protected Declarations }
         procedure SetNormals(aValue : TNormalSmoothing);
         procedure SetNormalDirection(aValue : TNormalDirection);
         procedure SetupQuadricParams(quadric : PGLUquadricObj);
         procedure SetNormalQuadricOrientation(quadric : PGLUquadricObj);
         procedure SetInvertedQuadricOrientation(quadric : PGLUquadricObj);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations }
         property Normals : TNormalSmoothing read FNormals write SetNormals default nsSmooth;
         property NormalDirection : TNormalDirection read FNormalDirection write SetNormalDirection default ndOutside;
   end;

   TAngleLimit1 = -90..90;
   TAngleLimit2 = 0..360;
   TCapType = (ctNone, ctCenter, ctFlat);

   // TSphere
   //
   {: A sphere object.<p>
      The sphere can have to and bottom caps, as well as being just a slice
      of sphere. }
   TSphere = class (TQuadricObject)
      private
         { Private Declarations }
         FRadius  : TGLFloat;
         FSlices, FStacks  : TGLInt;
         FTop     : TAngleLimit1;
         FBottom  : TAngleLimit1;
         FStart   : TAngleLimit2;
         FStop    : TAngleLimit2;
         FTopCap, FBottomCap : TCapType;
         procedure SetBottom(AValue: TAngleLimit1);
         procedure SetBottomCap(AValue: TCapType);
         procedure SetRadius(AValue: TGLFloat);
         procedure SetSlices(AValue: TGLInt);
         procedure SetStart(AValue: TAngleLimit2);
         procedure SetStop(AValue: TAngleLimit2);
         procedure SetStacks(AValue: TGLInt);
         procedure SetTop(AValue: TAngleLimit1);
         procedure SetTopCap(AValue: TCapType);

      public
         { Public Declarations }
         constructor Create(AOwner:TComponent); override;
         procedure Assign(Source:TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensions : TVector; override;

      published
         { Published Declarations }
         property Bottom: TAngleLimit1 read FBottom write SetBottom default -90;
         property BottomCap: TCapType read FBottomCap write SetBottomCap default ctNone;
         property Radius: TGLFloat read FRadius write SetRadius;
         property Slices: TGLInt read FSlices write SetSlices default 16;
         property Stacks: TGLInt read FStacks write SetStacks default 16;
         property Start: TAngleLimit2 read FStart write SetStart default 0;
         property Stop: TAngleLimit2 read FStop write SetStop default 360;
         property Top: TAngleLimit1 read FTop write SetTop default 90;
         property TopCap: TCapType read FTopCap write SetTopCap default ctNone;
   end;

   // TDisk
   //
   {: A Disk object.<p>
      The disk may not be complete, it can have a hole (controled by the
      InnerRadius property) and can only be a slice (controled by the StartAngle
      and SweepAngle properties). }
   TDisk = class(TQuadricObject)
      private
         { Private Declarations }
         FStartAngle, FSweepAngle, FOuterRadius, FInnerRadius : TGLFloat;
         FSlices, FLoops : TGLInt;
         procedure SetOuterRadius(AValue: TGLFloat);
         procedure SetInnerRadius(AValue: TGLFloat);
         procedure SetSlices(AValue: TGLInt);
         procedure SetLoops(AValue: TGLInt);
         procedure SetStartAngle(AValue: TGLFloat);
         procedure SetSweepAngle(AValue: TGLFloat);

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

         procedure Assign(Source: TPersistent); override;
         function AxisAlignedDimensions : TVector; override;

      published
         { Published Declarations }
         {: Allows defining a "hole" in the disk. }
         property InnerRadius: TGLFloat read FInnerRadius write SetInnerRadius;
         {: Number of radial mesh subdivisions. }
         property Loops : TGLInt read FLoops write SetLoops default 2;
         {: Outer radius for the disk.<p>
            If you leave InnerRadius at 0, this is the disk radius. }
         property OuterRadius : TGLFloat read FOuterRadius write SetOuterRadius;
         {: Number of mesh slices.<p>
            For instance, if Slices=6, your disk will look like an hexagon. }
         property Slices : TGLInt read FSlices write SetSlices default 16;
         property StartAngle : TGLFloat read FStartAngle write SetStartAngle;
         property SweepAngle : TGLFloat read FSweepAngle write SetSweepAngle;
   end;

	// TCylinderBase
   //
   {: Base class to cylinder-like objects.<p>
      Introduces the basic cylinder description properties. }
	TCylinderBase = class (TQuadricObject)
		private
			{ Private Declarations }
			FBottomRadius : TGLFloat;
			FSlices,	FStacks, FLoops  : TGLInt;
			FHeight  : TGLFloat;

		protected
			{ Protected Declarations }
			procedure SetBottomRadius(AValue: TGLFloat);
			procedure SetHeight(AValue: TGLFloat);
			procedure SetSlices(AValue: TGLInt);
			procedure SetStacks(AValue: TGLInt);
			procedure SetLoops(AValue: TGLInt);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

			procedure Assign(Source: TPersistent); override;

		published
			{ Published Declarations }
			property BottomRadius: TGLFloat read FBottomRadius write SetBottomRadius;
			property Height: TGLFloat read FHeight write SetHeight;
			property Slices: TGLInt read FSlices write SetSlices default 16;
			property Stacks: TGLInt read FStacks write SetStacks default 8;
			{: Number of concentric rings for top/bottom disk(s). }
			property Loops: TGLInt read FLoops write SetLoops default 1;
	end;

   // TConePart
   //
	TConePart  = (coSides, coBottom);
	TConeParts = set of TConePart;

	// TCone
	//
   {: A cone object. }
	TCone = class (TCylinderBase)
		private
			{ Private Declarations }
			FParts : TConeParts;

		protected
			{ Protected Declarations }
			procedure SetParts(AValue: TConeParts);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensions : TVector; override;

		published
			{ Published Declarations }
			property Parts : TConeParts read FParts Write SetParts default [coSides, coBottom];
	end;

	// TCylinderPart
	//
	TCylinderPart = (cySides, cyBottom, cyTop);
	TCylinderParts = set of TCylinderPart;

	// TCylinder
	//
   {: Cylinder object, can also be used to make truncated cones }
	TCylinder = class(TCylinderBase)
		private
			{ Private Declarations }
			FParts     : TCylinderparts;
			FTopRadius : TGLFloat;

		protected
			{ Protected Declarations }
			procedure SetTopRadius(AValue: TGLFloat);
			procedure SetParts(AValue: TCylinderParts);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			procedure Assign(Source: TPersistent); override;

			procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensions : TVector; override;

		published
			{ Published Declarations }
			property TopRadius : TGLFloat read FTopRadius write SetTopRadius;
			property Parts : TCylinderParts read FParts Write SetParts default [cySides, cyBottom, cyTop];
	end;

   // TAnnulusPart
   //
   TAnnulusPart = (anInnerSides, anOuterSides, anBottom, anTop);
	TAnnulusParts = set of TAnnulusPart;

   // TAnnulus
   //
   {: An annulus is a cylinder that can be made hollow (pipe-like). }
   TAnnulus = class(TCylinderBase)
      private
			{ Private Declarations }
         FParts : TAnnulusParts;
         FBottomInnerRadius : TGLFloat;
         FTopInnerRadius : TGLFloat;
         FTopRadius : TGLFloat;

      protected
			{ Protected Declarations }
         procedure SetTopRadius(AValue: TGLFloat);
         procedure SetTopInnerRadius(AValue: TGLFloat);
         procedure SetBottomInnerRadius(AValue: TGLFloat);
         procedure SetParts(AValue:TAnnulusParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensions : TVector; override;

      published
			{ Published Declarations }
         property BottomInnerRadius: TGLFLoat read FBottomInnerRadius write SetBottomInnerRadius;
         property TopInnerRadius: TGLFloat read FTopInnerRadius write SetTopInnerRadius;
         property TopRadius: TGLFloat read FTopRadius write SetTopRadius;
         property Parts: TAnnulusParts read FParts Write SetParts default [anInnerSides, anOuterSides, anBottom, anTop];
   end;

   // TTorus
   //
   {: A Torus object. }
   TTorus = class(TGLSceneObject)
      private
			{ Private Declarations }
         FRings, FSides : Cardinal;
         FMinorRadius, FMajorRadius  : Single;

      protected
			{ Protected Declarations }
         procedure SetMajorRadius(AValue: Single);
         procedure SetMinorRadius(AValue: Single);
         procedure SetRings(AValue: Cardinal);
         procedure SetSides(aValue : Cardinal);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         function AxisAlignedDimensions : TVector; override;

      published
			{ Published Declarations }
         property MajorRadius: Single read FMajorRadius write SetMajorRadius;
         property MinorRadius: Single read FMinorRadius write SetMinorRadius;
         property Rings: Cardinal read FRings write SetRings default 25;
         property Sides: Cardinal read FSides write SetSides default 15;
   end;

   // TSpaceTextCharRange
   //
   TSpaceTextCharRange = (stcrAlphaNum, stcrNumbers, stcrAll);

   // TSpaceText
   //
   {: Renders a text in 3D. }
   TSpaceText = class(TGLSceneObject)
      private
			{ Private Declarations }
         FFont       : TFont;
         FText       : String;
         FExtrusion  : Single;
         FAllowedDeviation : Single;
         FCharacterRange : TSpaceTextCharRange;
         procedure SetCharacterRange(const val : TSpaceTextCharRange);
         procedure SetAllowedDeviation(const val : Single);
         procedure SetExtrusion(AValue: Single);
         procedure SetFont(AFont: TFont);
         procedure SetText(AText: String);

		protected
			{ Protected Declarations }
         BaseList    : TGLuint;
         FontChanged : Boolean;
         procedure DoDestroyList(glsceneOnly : Boolean); override;
         procedure OnFontChange(sender : TObject);

		public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure PrepareObject(var rci : TRenderContextInfo); override;

		published
			{ Published Declarations }
         {: Adjusts the 3D font extrusion.<p>
            If Extrusion=0, the characters will be flat (2D), values >0 will
            give them a third dimension. }
         property Extrusion: Single read FExtrusion write SetExtrusion;
         property Font: TFont read FFont write SetFont;
         property Text: String read FText write SetText;
         {: Quality related, see Win32 help for wglUseFontOutlines }
         property AllowedDeviation : Single read FAllowedDeviation write SetAllowedDeviation;
         {: Character range to convert.<p>
            Converting less characters saves time and memory... }
         property CharacterRange : TSpaceTextCharRange read FCharacterRange write SetCharacterRange default stcrAll;
   end;

   // TTeapot
   //
   {: The age old teapot.<p>
      The only use of this object is testing... }
   TTeapot = class(TGLSceneObject)
      private
			{ Private Declarations }
         FGrid : Cardinal;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TArrowLinePart
   //
   TArrowLinePart = (alLine, alTopArrow, alBottomArrow);
   TArrowLineParts = set of TArrowLinePart;

   // TArrowHeadStackingStyle
   //
   TArrowHeadStackingStyle = (ahssStacked, ahssCentered, ahssIncluded);

   // TArrowLine
   //
   {: Draws an arrowhead (cylinder + cone).<p>
      The arrow head is a cone that shares the attributes of the cylinder
      (ie stacks/slices, materials etc). Seems to work ok.<br>
      This is useful for displaying a vector based field (eg velocity) or
      other arrows that might be required.<br>
      By default the bottom arrow is off }
   TArrowLine = class(TCylinderBase)
      private
         { Private Declarations}
         fParts: TArrowLineParts;
         fTopRadius:TGLFloat;
         fTopArrowHeadHeight:TGLFloat;
         fTopArrowHeadRadius:TGLFloat;
         fBottomArrowHeadHeight:TGLFloat;
         fBottomArrowHeadRadius:TGLFloat;
         FHeadStackingStyle : TArrowHeadStackingStyle;

      protected
         { Protected Declarations}
         procedure SetTopRadius(AValue:TGLFloat);
         procedure SetTopArrowHeadHeight(AValue:TGLFloat);
         procedure SetTopArrowHeadRadius(AValue:TGLFloat);
         procedure SetBottomArrowHeadHeight(AValue:TGLFloat);
         procedure SetBottomArrowHeadRadius(AValue:TGLFloat);
         procedure SetParts(AValue:TArrowLineParts);
         procedure SetHeadStackingStyle(const val : TArrowHeadStackingStyle);

      public
         { Public Declarations}
         constructor Create(AOwner:TComponent);override;
         procedure BuildList(var rci : TRenderContextInfo);override;
         procedure Assign(Source:TPersistent);override;

      published
         { Published Declarations}
         property TopRadius : TGLFloat read fTopRadius write SetTopRadius;
         property HeadStackingStyle : TArrowHeadStackingStyle read FHeadStackingStyle write SetHeadStackingStyle default ahssStacked;
         property Parts : TArrowLineParts read fParts write SetParts default [alLine, alTopArrow];
         property TopArrowHeadHeight : TGLFloat read fTopArrowHeadHeight write SetTopArrowHeadHeight;
         property TopArrowHeadRadius : TGLFloat read fTopArrowHeadRadius write SetTopArrowHeadRadius;
         property BottomArrowHeadHeight : TGLFloat read fBottomArrowHeadHeight write SetBottomArrowHeadHeight;
         property BottomArrowHeadRadius : TGLFloat read fBottomArrowHeadRadius write SetBottomArrowHeadRadius;
   end;

   // TDodecahedron
   //
   {: A Dodecahedron.<p>
      The dodecahedron has no texture coordinates defined. }
   TDodecahedron = class(TGLSceneObject)
      public
			{ Public Declarations }
         procedure BuildList(var rci : TRenderContextInfo); override;
   end;

   // TPolygonBase
   //
   {: Base class for objects based on a polygon. }
   TPolygonBase = class(TGLSceneObject)
      private
			{ Private Declarations }
         FDivision : Integer;
         FSplineMode : TLineSplineMode;

		protected
			{ Protected Declarations }
         FNodes : TGLNodes;
         procedure CreateNodes; dynamic;
         procedure SetSplineMode(const val : TLineSplineMode);
         procedure SetDivision(const value: Integer);
         procedure SetNodes(const aNodes : TGLNodes);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure NotifyChange(Sender : TObject); override;

         procedure AddNode(const coords : TGLCoordinates); overload;
         procedure AddNode(const X, Y, Z: TGLfloat); overload;
         procedure AddNode(const value : TVector); overload;
         procedure AddNode(const value : TAffineVector); overload;

      published
			{ Published Declarations }
         {: The nodes list.<p> }
         property Nodes : TGLNodes read FNodes write SetNodes;
         {: Number of divisions for each segment in spline modes.<p>
            Minimum 1 (disabled), ignored in lsmLines mode. }
         property Division: Integer read FDivision write SetDivision default 10;
         {: Default spline drawing mode.<p>
            This mode is used only for the curve, not for the rotation path. }
         property SplineMode : TLineSplineMode read FSplineMode write SetSplineMode default lsmLines;

   end;

   // TPolygonParts
   //
   TPolygonPart = (ppTop, ppBottom);
   TPolygonParts = set of TPolygonPart;

   // TPolygon
   //
   {: A basic polygon object.<p>
      The curve is described by the Nodes and SplineMode properties, should be
      planar and is automatically tessellated.<p>
      Texture coordinates are deduced from X and Y coordinates only.<p>
      This object allows only for polygons described by a single curve, if you
      need "complex polygons" with holes, patches and cutouts, see GLMultiPolygon. }
   TPolygon = class(TPolygonBase)
      private
			{ Private Declarations }
         FParts : TPolygonParts;

		protected
			{ Protected Declarations }
         procedure SetParts(const val : TPolygonParts);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;

      published
			{ Published Declarations }
         {: Parts of polygon.<p>
            The 'top' of the polygon is the position were the curve describing
            the polygon spin counter-clockwise (i.e. right handed convention). }
         property Parts : TPolygonParts read FParts write SetParts default [ppTop, ppBottom];
   end;

   // holds an entry in the font manager list (used in TSpaceText)
   PFontEntry        = ^TFontEntry;
   TFontEntry        = record
                         Name      : String;
                         Styles    : TFontStyles;
                         Extrusion : Single;
                         Base      : TGLuint;
                         RefCount  : Integer;
                         allowedDeviation : Single;
                         firstChar, lastChar : Integer;
                       end;

   // TFontManager
   //
   {: Manages a list of fonts for which display lists were created. }
   TFontManager = class(TList)
	   public
			{ Public Declarations }
         destructor Destroy; override;
         function FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                           FAllowedDeviation : Single;
                           FFirstChar, FLastChar : Integer) : PFontEntry;
         function FindFontByList(AList: TGLuint): PFontEntry;
         function GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
                              allowedDeviation : Single;
                              firstChar, lastChar : Integer) : TGLuint;
         procedure Release(List: TGLuint);
   end;

function FontManager : TFontManager;
procedure ReleaseFontManager;

{: Issues OpenGL for a unit-size dodecahedron. }
procedure DodecahedronBuildList;
{: Issues OpenGL for a unit-size cube stippled wireframe. }
procedure CubeWireframeBuildList(size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
implementation
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

uses Consts, Dialogs, Forms, GLStrings, PlugInIntf, Spline, XOpenGL;

var
	vFontManager : TFontManager;

// FontManager
//
function FontManager : TFontManager;
begin
	if not Assigned(vFontManager) then
		vFontManager:=TFontManager.Create;
	Result:=vFontManager;
end;

// ReleaseFontManager
//
procedure ReleaseFontManager;
begin
   if Assigned(vFontManager) then begin
      vFontManager.Free;
      vFontManager:=nil;
   end;
end;

// CubeWireframeBuildList
//
procedure CubeWireframeBuildList(size : TGLFloat; stipple : Boolean;
                                 const color : TColorVector);
var
	mi, ma : Single;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   glEnable(GL_LINE_SMOOTH);
   if stipple then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, $CCCC);
   end;
   glLineWidth(1);
   ma:=0.5*size;
   mi:=-ma;
   glColor3f(Color[0], Color[1], Color[2]);
   glBegin(GL_LINE_STRIP);
      // front face
      glVertex3f(ma, mi, mi); glVertex3f(ma, ma, mi);
      glVertex3f(ma, ma, ma); glVertex3f(ma, mi, ma);
      glVertex3f(ma, mi, mi);
      // partial up back fac
      glVertex3f(mi, mi, mi); glVertex3f(mi, mi, ma);
      glVertex3f(mi, ma, ma); glVertex3f(mi, ma, mi);
      // right side low
      glVertex3f(ma, ma, mi);
   glEnd;
   glBegin(GL_LINES);
      // right high
      glVertex3f(ma, ma, ma);	glVertex3f(mi, ma, ma);
      // back low
      glVertex3f(mi, mi, mi); glVertex3f(mi, ma, mi);
      // left high
      glVertex3f(ma, mi, ma); glVertex3f(mi, mi, ma);
   glEnd;
   glPopAttrib;
end;


//----------------- TFontManager -----------------------------------------------

// Destroy
//
destructor TFontManager.Destroy;
var
   i : Integer;
begin
   for I:=0 to Count-1 do begin
      if TFontEntry(Items[I]^).Base<>0 then
         glDeleteLists(TFontEntry(Items[I]^).Base, 255);
      FreeMem(Items[I], SizeOf(TFontEntry));
   end;
   inherited Destroy;
end;

// FindFond
//
function TFontManager.FindFont(AName: String; FStyles: TFontStyles; FExtrusion: Single;
										 FAllowedDeviation : Single;
										 FFirstChar, FLastChar : Integer) : PFontEntry;
var
	i : Integer;
begin
	Result:=nil;
	// try to find an entry with the required attributes
	for I :=0 to Count-1 do with TFontEntry(Items[I]^) do
		if (CompareText(Name, AName) = 0) and (Styles = FStyles)
				and (Extrusion = FExtrusion) and (allowedDeviation=FAllowedDeviation)
				and (firstChar=FFirstChar)	and (lastChar=FLastChar) then begin
			// entry found
			Result:=Items[I];
			Break;
		end;
end;

// FindFontByList
//
function TFontManager.FindFontByList(AList: TGLuint): PFontEntry;
var
   i : Integer;
begin
   Result:=nil;
   // try to find an entry with the required attributes
   for I :=0 to Count-1 do
      with TFontEntry(Items[I]^) do
         if Base = AList then begin // entry found
            Result:=Items[I];
            Break;
         end;
end;

// GetFontBase
//
function TFontManager.GetFontBase(AName: String; FStyles: TFontStyles; FExtrusion: Single;
											 allowedDeviation : Single;
											 firstChar, lastChar : Integer) : TGLuint;
var
   NewEntry : PFontEntry;
	MemDC    : HDC;
	AFont    : TFont;
begin
   NewEntry:=FindFont(AName, FStyles, FExtrusion, allowedDeviation, firstChar, lastChar);
   if Assigned(NewEntry) then begin
	   Inc(NewEntry^.RefCount);
      Result:=NewEntry^.Base;
      Exit;
   end;
   // no entry found, so create one
   New(NewEntry);
   try
      NewEntry^.Name:=AName;
      NewEntry^.Styles:=FStyles;
      NewEntry^.Extrusion:=FExtrusion;
	   NewEntry^.RefCount:=1;
	   NewEntry^.firstChar:=firstChar;
	   NewEntry^.lastChar:=lastChar;
	   NewEntry^.allowedDeviation:=allowedDeviation;

      // create a font to be used while display list creation
      AFont:=TFont.Create;
      MemDC:=CreateCompatibleDC(0);
      try
         AFont.Name:=AName;
         AFont.Style:=FStyles;
         SelectObject(MemDC, AFont.Handle);
         NewEntry^.Base:=glGenLists(255);
		   if NewEntry^.Base = 0 then
			   raise Exception.Create('FontManager: no more display lists available');
		   if not OpenGL12.wglUseFontOutlines(MemDC, firstChar, lastChar-firstChar+1,
                                            NewEntry^.Base, allowedDeviation,
                                            FExtrusion, WGL_FONT_POLYGONS, nil) then
		  	raise Exception.Create('FontManager: font creation failed');
      finally
		   AFont.Free;
         DeleteDC(MemDC);
      end;
      Add(NewEntry);
      Result:=NewEntry^.Base;
   except
      if NewEntry^.Base<>0 then glDeleteLists(NewEntry^.Base, 255);
      Dispose(NewEntry);
      raise;
   end;
end;

//------------------------------------------------------------------------------

procedure TFontManager.Release(List: TGLuint);

var Entry : PFontEntry;

begin
  Entry:=FindFontByList(List);
  if assigned(Entry) then
  begin
    Dec(Entry^.RefCount);
    if Entry^.RefCount = 0 then
    begin
      glDeleteLists(Entry^.Base, 255);
      Remove(Entry);
    end;
  end;
end;

//----------------- TDummyCube -----------------------------------------------------

// Create
//
constructor TDummyCube.Create(AOwner : TComponent);
begin
	inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
	FCubeSize:=1;
	FEdgeColor:=TGLColor.Create(Self);
	FEdgeColor.Initialize(clrWhite);
end;

// Destroy
//
destructor TDummyCube.Destroy;
begin
   FEdgeColor.Free;
   inherited;
end;

// Assign
//
procedure TDummyCube.Assign(Source: TPersistent);
begin
	if Source is TDummyCube then begin
		FCubeSize:=TDummyCube(Source).FCubeSize;
		FEdgeColor.Color:=TDummyCube(Source).FEdgeColor.Color;
		FVisibleAtRunTime:=TDummyCube(Source).FVisibleAtRunTime;
		NotifyChange(Self);
	end;
	inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TDummyCube.AxisAlignedDimensions : TVector;
begin
   VectorScale(XYZHmgVector, 0.5*Abs(FCubeSize), Result);
end;

// BuildList
//
procedure TDummyCube.BuildList(var rci : TRenderContextInfo);
begin
 	if (csDesigning in ComponentState) or (FVisibleAtRunTime) then
      CubeWireframeBuildList(FCubeSize, True, EdgeColor.Color);
end;

// BarycenterAbsolutePosition
//
function TDummyCube.BarycenterAbsolutePosition : TVector;
var
	i : Integer;
begin
	if Count>0 then begin
		Result:=Children[0].BarycenterAbsolutePosition;
		for i:=1 to Count-1 do
			Result:=VectorAdd(Result, Children[i].BarycenterAbsolutePosition);
		ScaleVector(Result, 1/Count);
	end else Result:=AbsolutePosition;
end;

// SetCubeSize
//
procedure TDummyCube.SetCubeSize(const val : TGLFloat);
begin
	if val<>FCubeSize then begin
		FCubeSize:=val;
		StructureChanged;
	end;
end;

// SetEdgeColor
//
procedure TDummyCube.SetEdgeColor(const val : TGLColor);
begin
	if val<>FEdgeColor then begin
		FEdgeColor.Assign(val);
		StructureChanged;
	end;
end;

// SetVisibleAtRunTime
//
procedure TDummyCube.SetVisibleAtRunTime(const val : Boolean);
begin
	if val<>FVisibleAtRunTime then begin
		FVisibleAtRunTime:=val;
		StructureChanged;
	end;
end;

//----------------- TPlane -----------------------------------------------------

// Create
//
constructor TPlane.Create(AOwner:Tcomponent);
begin
   inherited Create(AOwner);
   FWidth:=1;
   FHeight:=1;
   FXTiles:=1;
   FXOffset:=0;
   FYTiles:=1;
   FYOffset:=0;
end;

// BuildList
//
procedure TPlane.BuildList(var rci : TRenderContextInfo);
var
   hw, hh : TGLFloat;
begin
   hw:=FWidth*0.5;
   hh:=FHeight*0.5;
   glBegin(GL_QUADS);
      glNormal3fv(@ZVector);
      xglTexCoord2f(FXTiles+FXOffset, FYTiles+FYOffset);
      glVertex2f( hw, hh);
      xglTexCoord2f(0, FYTiles+FYOffset);
      glVertex2f(-hw, hh);
      xglTexCoord2f(0, 0);
      glVertex2f(-hw, -hh);
      xglTexCoord2f(FXTiles+FXOffset, 0);
      glVertex2f( hw, -hh);
   glEnd;
end;

// SetWidth
//
procedure TPlane.SetWidth(AValue : TGLFloat);
begin
   if AValue<>FWidth then begin
      FWidth:=AValue;
	   StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TPlane.SetHeight(AValue:TGLFloat);

begin
  if AValue<>FHeight then
  begin
    FHeight:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TPlane.SetXOffset(const Value: TGLFloat);

begin
  if Value<>FXOffset then
  begin
    FXOffset:=Value;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TPlane.SetXTiles(const Value: Cardinal);

begin
  if Value<>FXTiles then
  begin
    FXTiles:=Value;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TPlane.SetYOffset(const Value: TGLFloat);

begin
  if Value<>FYOffset then
  begin
    FYOffset:=Value;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TPlane.SetYTiles(const Value: Cardinal);

begin
  if Value<>FYTiles then
  begin
    FYTiles:=Value;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TPlane.Assign(Source: TPersistent);

begin
  if assigned(Source) and (Source is TCube) then
  begin
    FWidth:=TPlane(Source).FWidth;
    FHeight:=TPlane(Source).FHeight;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TPlane.AxisAlignedDimensions: TVector;
begin
   Result:=VectorMake(0.5*Abs(FWidth), 0.5*Abs(FHeight), 0);
end;

// ------------------
// ------------------ TSprite ------------------
// ------------------

// Create
//
constructor TSprite.Create(AOwner:Tcomponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
   FAlphaChannel:=1;
	FWidth:=1;
	FHeight:=1;
end;

// Assign
//
procedure TSprite.Assign(Source: TPersistent);
begin
	if Source is TSprite then begin
		FWidth:=TSprite(Source).FWidth;
		FHeight:=TSprite(Source).FHeight;
		FRotation:=TSprite(Source).FRotation;
	end;
	inherited Assign(Source);
end;

// BuildList
//
procedure TSprite.BuildList(var rci : TRenderContextInfo);
var
	vx, vy, vx1, vy1 : TAffineVector;
	i : Integer;
   w, h, c, s : Single;
   mat : TMatrix;
begin
   if FAlphaChannel<>1 then
      SetGLMaterialAlphaChannel(GL_FRONT, FAlphaChannel);
   glGetFloatv(GL_MODELVIEW_MATRIX, @mat);
	glBegin(GL_QUADS);
		// extraction of the "vecteurs directeurs de la matrice"
		// (dunno how they are named in english)
      w:=FWidth*0.5;
      h:=FHeight*0.5;
   	vx[0]:=mat[0][0]*w;  vy[0]:=mat[0][1]*h;
   	vx[1]:=mat[1][0]*w;  vy[1]:=mat[1][1]*h;
   	vx[2]:=mat[2][0]*w;  vy[2]:=mat[2][1]*h;
      if FRotation=0 then begin
         // no rotation, use fast, direct projection
   		xglTexCoord2f(1, 1);  glVertex3f( vx[0]+vy[0], vx[1]+vy[1], vx[2]+vy[2]);
	   	xglTexCoord2f(0, 1);  glVertex3f(-vx[0]+vy[0],-vx[1]+vy[1],-vx[2]+vy[2]);
		   xglTexCoord2f(0, 0);  glVertex3f(-vx[0]-vy[0],-vx[1]-vy[1],-vx[2]-vy[2]);
   		xglTexCoord2f(1, 0);  glVertex3f( vx[0]-vy[0], vx[1]-vy[1], vx[2]-vy[2]);
      end else begin
         // we need to compose main vectors...
         SinCos(FRotation*cPIdiv180, s, c);
   		for i:=0 to 2 do begin
            vx1[i]:=vx[i]+vy[i];
            vy1[i]:=vy[i]-vx[i];
         end;
         // ...and apply rotation... way slower
   		xglTexCoord2f(1, 1);  glVertex3f( c*vx1[0]+s*vy1[0], c*vx1[1]+s*vy1[1], c*vx1[2]+s*vy1[2]);
	   	xglTexCoord2f(0, 1);  glVertex3f(-s*vx1[0]+c*vy1[0],-s*vx1[1]+c*vy1[1],-s*vx1[2]+c*vy1[2]);
		   xglTexCoord2f(0, 0);  glVertex3f(-c*vx1[0]-s*vy1[0],-c*vx1[1]-s*vy1[1],-c*vx1[2]-s*vy1[2]);
   		xglTexCoord2f(1, 0);  glVertex3f( s*vx1[0]-c*vy1[0], s*vx1[1]-c*vy1[1], s*vx1[2]-c*vy1[2]);
      end;
	glEnd;
end;

// SetWidth
//
procedure TSprite.SetWidth(const val : TGLFloat);
begin
	if FWidth<>val then begin
		FWidth:=val;
		StructureChanged;
	end;
end;

// SetHeight
//
procedure TSprite.SetHeight(const val : TGLFloat);
begin
	if FHeight<>val then begin
		FHeight:=val;
		StructureChanged;
	end;
end;

// SetRotation
//
procedure TSprite.SetRotation(const val : TGLFloat);
begin
	if FRotation<>val then begin
		FRotation:=val;
		StructureChanged;
	end;
end;

// SetAlphaChannel
//
procedure TSprite.SetAlphaChannel(const val : Single);
begin
   if val<>FAlphaChannel then begin
      if val<0 then
         FAlphaChannel:=0
      else if val>1 then
         FAlphaChannel:=1
      else FAlphaChannel:=val;
		StructureChanged;
   end;
end;

// StoreAlphaChannel
//
function TSprite.StoreAlphaChannel : Boolean;
begin
	Result:=(FAlphaChannel<>1);
end;

// SetSize
//
procedure TSprite.SetSize(const width, height : TGLFloat);
begin
	FWidth:=width;
	FHeight:=height;
	StructureChanged;
end;

// SetSquareSize
//
procedure TSprite.SetSquareSize(const size : TGLFloat);
begin
	FWidth:=size;
	FHeight:=size;
	StructureChanged;
end;

// ------------------
// ------------------ TLineBase ------------------
// ------------------

// Create
//
constructor TLineBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FLineColor:=TGLColor.Create(Self);
   FLineColor.Initialize(clrWhite);
   FLinePattern:=$FFFF;
   FAntiAliased:=False;
   FLineWidth:=1.0;
end;

// Destroy
//
destructor TLineBase.Destroy;
begin
   FLineColor.Free;
   inherited Destroy;
end;

// SetLineColor
//
procedure TLineBase.SetLineColor(const value: TGLColor);
begin
   FLineColor.Color:=Value.Color;
   StructureChanged;
end;

// SetLinePattern
//
procedure TLineBase.SetLinePattern(const value: TGLushort);
begin
   if FLinePattern<>value then begin
      FLinePattern:=Value;
      StructureChanged;
   end;
end;

// SetLineWidth
//
procedure TLineBase.SetLineWidth(const val : Single);
begin
   if FLineWidth<>val then begin
      FLineWidth:=val;
      StructureChanged;
   end;
end;

// StoreLineWidth
//
function TLineBase.StoreLineWidth : Boolean;
begin
   Result:=(FLineWidth<>1.0);
end;

// SetAntiAliased
//
procedure TLineBase.SetAntiAliased(const val : Boolean);
begin
   if FAntiAliased<>val then begin
      FAntiAliased:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TLineBase.Assign(Source: TPersistent);
begin
   if Source is TLineBase then begin
      FLineColor:=TLineBase(Source).FLineColor;
      FLinePattern:=TLineBase(Source).FLinePattern;
      FLineWidth:=TLineBase(Source).FLineWidth;
      FAntiAliased:=TLineBase(Source).FAntiAliased;
   end else inherited Assign(Source);
end;

// SetupLineStyle
//
procedure TLineBase.SetupLineStyle;
begin
   glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT or GL_COLOR_BUFFER_BIT);
   glDisable(GL_LIGHTING);
   if FLinePattern<>$FFFF then begin
      glEnable(GL_LINE_STIPPLE);
      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glLineStipple(1, FLinePattern);
   end;
   if FAntiAliased then
      glEnable(GL_LINE_SMOOTH)
   else glDisable(GL_LINE_SMOOTH);
   glLineWidth(FLineWidth);
   glColor3f(FLineColor.Red, FLineColor.Green, FLineColor.Blue);
end;

// RestoreLineStyle
//
procedure TLineBase.RestoreLineStyle;
begin
   glPopAttrib;
end;

// ------------------
// ------------------ TGLLinesNode ------------------
// ------------------

// Create
//
constructor TGLLinesNode.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   FColor:=TGLColor.Create(Self);
   FColor.Initialize((TGLLinesNodes(Collection).GetOwner as TLines).NodeColor.Color);
   FColor.OnNotifyChange:=OnColorChange;
end;

// Destroy
//
destructor TGLLinesNode.Destroy;
begin
   FColor.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLLinesNode.Assign(Source: TPersistent);
begin
	if Source is TGLNode then begin
      FColor.Assign(TGLLinesNode(Source).FColor);
	end else inherited;
end;

// SetColor
//
procedure TGLLinesNode.SetColor(const val : TGLColor);
begin
   FColor.Assign(val);
end;

// OnColorChange
//
procedure TGLLinesNode.OnColorChange(sender : TObject);
begin
   (Collection as TGLNodes).NotifyChange;
end;

// StoreColor
//
function TGLLinesNode.StoreColor : Boolean;
begin
   Result:=not VectorEquals((TGLLinesNodes(Collection).GetOwner as TLines).NodeColor.Color,
                            FColor.Color);
end;

// ------------------
// ------------------ TGLLinesNodes ------------------
// ------------------

// Create
//
constructor TGLLinesNodes.Create(AOwner : TComponent);
begin
   OldCreate(AOwner, TGLLinesNode);
end;

// NotifyChange
//
procedure TGLLinesNodes.NotifyChange;
begin
   if Assigned(Owner) then
      (Owner as TGLBaseSceneObject).StructureChanged;
end;

// ------------------
// ------------------ TLines ------------------
// ------------------

// Create
//
constructor TLines.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FNodes:=TGLLinesNodes.Create(Self);
   FNodeColor:=TGLColor.Create(Self);
   FNodeColor.Initialize(clrBlue);
   FNodeColor.OnNotifyChange:=OnNodeColorChanged;
   FOldNodeColor:=clrBlue;
   FDivision:=10;
   FNodesAspect:=lnaAxes;
   FSplineMode:=lsmLines;
   FNodeSize:=1;
end;

// Destroy
//
destructor TLines.Destroy;
begin
   FNodes.Free;
   FNodeColor.Free;
   inherited Destroy;
end;

// SetNodesAspect
//
procedure TLines.SetNodesAspect(const value : TLineNodesAspect);
begin
   if Value<>FNodesAspect then begin
      FNodesAspect:=value;
      StructureChanged;
   end;
end;

// SetNodeColor
//
procedure TLines.SetNodeColor(const value: TGLColor);
begin
   FNodeColor.Color:=Value.Color;
   StructureChanged;
end;

// OnNodeColorChanged
//
procedure TLines.OnNodeColorChanged(sender : TObject);
var
   i : Integer;
begin
   // update color for nodes...
   for i:=0 to Nodes.Count-1 do
      if VectorEquals(TGLLinesNode(Nodes[i]).Color.Color, FOldNodeColor) then
         TGLLinesNode(Nodes[i]).Color.Assign(FNodeColor);
   SetVector(FOldNodeColor, FNodeColor.Color);
end;

// SetDivision
//
procedure TLines.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetNodes
//
procedure TLines.SetNodes(const aNodes : TGLLinesNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetOptions
//
procedure TLines.SetOptions(const val : TLinesOptions);
begin
   FOptions:=val;
   StructureChanged;
end;

// SetNodeSize
//
procedure TLines.SetNodeSize(const val : Single);
begin
   if val<=0 then
      FNodeSize:=1
   else FNodeSize:=val;
   StructureChanged;
end;

// StoreNodeSize
//
function TLines.StoreNodeSize : Boolean;
begin
   Result:=FNodeSize<>1;
end;

// SetSplineMode
//
procedure TLines.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TLines.Assign(Source: TPersistent);
begin
   if Source is TLines then begin
      SetNodes(TLines(Source).FNodes);
      FNodesAspect:=TLines(Source).FNodesAspect;
      FNodeColor.Color:=TLines(Source).FNodeColor.Color;
      FDivision:=TLines(Source).FDivision;
      FSplineMode:=TLines(Source).FSplineMode;
   end;
   inherited Assign(Source);
end;

// DrawNode
//
procedure TLines.DrawNode(X, Y, Z: Single; Color: TGLColor);
begin
   glPushMatrix;
   glTranslatef(x, y, z);
   case NodesAspect of
      lnaAxes :
         AxesBuildList($CCCC, FNodeSize*0.5);
      lnaCube :
         CubeWireframeBuildList(FNodeSize, False, Color.Color);
      lnaDodecahedron : begin
         if FNodeSize<>1 then begin
            glPushMatrix;
            glScalef(FNodeSize, FNodeSize, FNodeSize);
            SetGLMaterialColors(GL_FRONT, @clrBlack, @clrGray20, Color.AsAddress, @clrBlack, 0);
            DodecahedronBuildList;
            glPopMatrix;
         end else begin
            SetGLMaterialColors(GL_FRONT, @clrBlack, @clrGray20, Color.AsAddress, @clrBlack, 0);
            DodecahedronBuildList;
         end;
      end;
   else
      Assert(False)
   end;
   glPopMatrix;
end;

// BuildList
//
procedure TLines.BuildList(var rci : TRenderContextInfo);
var
   i, n : Integer;
   a, b, c : TGLFloat;
   f : Single;
   spline : TCubicSpline;
   vertexColor : TVector;
begin
  if Nodes.Count>1 then begin
      // first, we setup the line color & stippling styles
      SetupLineStyle;
      // start drawing the line
      glBegin(GL_LINE_STRIP);
         if (FDivision<2) or (FSplineMode=lsmLines) then begin
            // standard line(s), draw directly
            if loUseNodeColorForLines in Options then begin
               // node color interpolation
               for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do begin
                  glColor4fv(Color.AsAddress);
                  glVertex3f(X, Y, Z);
               end;
            end else begin
               // single color
               for i:=0 to Nodes.Count-1 do with Nodes[i] do
                  glVertex3f(X, Y, Z);
            end;
         end else begin
            // cubic spline
            spline:=Nodes.CreateNewCubicSpline;
            try
               f:=1/FDivision;
               for i:=0 to (Nodes.Count-1)*FDivision  do begin
                  Spline.SplineXYZ(i*f, a, b, c);
                  if loUseNodeColorForLines in Options then begin
                     n:=(i div FDivision);
                     if n<Nodes.Count-1 then
                        VectorLerp(TGLLinesNode(Nodes[n]).Color.Color,
                                   TGLLinesNode(Nodes[n+1]).Color.Color,
                                   (i mod FDivision)*f, vertexColor)
                     else SetVector(vertexColor, TGLLinesNode(Nodes[Nodes.Count-1]).Color.Color);
                     glColor4fv(@vertexColor);
                  end;
                  glVertex3f(a, b, c);
               end;
            finally
               Spline.Free;
            end;
         end;
      glEnd;
      RestoreLineStyle;
      if FNodesAspect<>lnaInvisible then
         for i:=0 to Nodes.Count-1 do with TGLLinesNode(Nodes[i]) do
            DrawNode(X, Y, Z, Color);
   end;
end;

// AddNode (coords)
//
procedure TLines.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TLines.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TLines.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TLines.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

//----------------- TCube ------------------------------------------------------

constructor TCube.Create(AOwner:Tcomponent);
begin
  inherited Create(AOwner);
  FCubeWidth:=1;
  FCubeHeight:=1;
  FCubeDepth:=1;
  FParts:=[cpTop, cpBottom, cpFront, cpBack, cpLeft, cpRight];
  FNormalDirection:=ndOutside;
end;

//------------------------------------------------------------------------------

procedure TCube.BuildList(var rci : TRenderContextInfo);
var
	hw, hh, hd, nd  : TGLFloat;
begin
   if FNormalDirection = ndInside then
      nd:=-1
   else nd:=1;
   hw:= FCubeWidth*0.5;
   hh:= FCubeHeight*0.5;
   hd:= FCubeDepth*0.5;

   glBegin(GL_QUADS);
   if cpFront in FParts then begin
      glNormal3f(  0,  0, nd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, hh, hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, hh, hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, hd);
   end;
   if cpBack in FParts then begin
      glNormal3f(  0,  0, -nd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f( hw, hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw, hh, -hd);
   end;
   if cpLeft in FParts then begin
      glNormal3f(-nd,  0,  0);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(-hw, hh, hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(-hw, -hh, hd);
   end;
   if cpRight in FParts then begin
      glNormal3f(nd,  0,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(hw, hh, hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(hw, -hh, hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f(hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f(hw, hh, -hd);
   end;
   if cpTop in FParts then begin
      glNormal3f(  0, nd,  0);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, hh, -hd);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, hh, hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, hh, hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, hh, -hd);
   end;
   if cpBottom in FParts then begin
      glNormal3f(  0, -nd,  0);
      xglTexCoord2fv(@NullTexPoint);   glVertex3f(-hw, -hh, -hd);
      xglTexCoord2fv(@XTexPoint);      glVertex3f( hw, -hh, -hd);
      xglTexCoord2fv(@XYTexPoint);     glVertex3f( hw, -hh, hd);
      xglTexCoord2fv(@YTexPoint);      glVertex3f(-hw, -hh, hd);
   end;
   glEnd;
end;

// SetCubeWidth
//
procedure TCube.SetCubeWidth(AValue : TGLFloat);
begin
   if AValue<>FCubeWidth then begin
      FCubeWidth:=AValue;
      StructureChanged;
   end;
end;

// SetCubeHeight
//
procedure TCube.SetCubeHeight(AValue:TGLFloat);
begin
   if AValue<>FCubeHeight then begin
      FCubeHeight:=AValue;
      StructureChanged;
   end;
end;

// SetCubeDepth
//
procedure TCube.SetCubeDepth(AValue: TGLFloat);
begin
   if AValue<>FCubeDepth then begin
      FCubeDepth:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TCube.SetParts(AValue:TCubeParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// SetNormalDirection
//
procedure TCube.SetNormalDirection(AValue: TNormalDirection);
begin
   if AValue<>FNormalDirection then begin
      FNormalDirection:=AValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TCube.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TCube) then begin
      FCubeWidth:=TCube(Source).FCubewidth;
      FCubeHeight:=TCube(Source).FCubeHeight;
      FCubeDepth:=TCube(Source).FCubeDepth;
      FParts:=TCube(Source).FParts;
      FNormalDirection:=TCube(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TCube.AxisAlignedDimensions : TVector;
begin
   VectorScale(VectorMake(Abs(FCubeWidth), Abs(FCubeHeight), Abs(FCubeDepth)),
               0.5, Result);
end;

// DefineProperties
//
procedure TCube.DefineProperties(Filer: TFiler);
begin
   inherited;
   Filer.DefineBinaryProperty('CubeSize', ReadData, WriteData,
                              (FCubeWidth<>1) or (FCubeHeight<>1) or (FCubeDepth<>1));
end;

// ReadData
//
procedure TCube.ReadData(Stream: TStream);
begin
   with Stream do begin
      Read(FCubeWidth, SizeOf(FCubeWidth));
      Read(FCubeHeight, SizeOf(FCubeHeight));
      Read(FCubeDepth, SizeOf(FCubeDepth));
   end;
end;

// WriteData
//
procedure TCube.WriteData(Stream: TStream);
begin
   with Stream do begin
      Write(FCubeWidth, SizeOf(FCubeWidth));
      Write(FCubeHeight, SizeOf(FCubeHeight));
      Write(FCubeDepth, SizeOf(FCubeDepth));
   end;
end;

//----------------- TCube ------------------------------------------------------

constructor TFrustrum.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FApexHeight := 1;
  FBaseWidth := 1;
  FBaseDepth := 1;
  FHeight := 0.5;
  FParts := cAllFrustrumParts;
  FNormalDirection := ndOutside;
end;

procedure TFrustrum.BuildList(var rci: TRenderContextInfo);
var
  HBW, HBD: TGLFloat; // half of width, half of depth at base
  HTW, HTD: TGLFloat; // half of width, half of depth at top of frustrum
  Sign: TGLFloat;     // +1 or -1
  Angle: TGLFloat;    // in radians
  ASin, ACos: TGLFloat;
begin
  if FNormalDirection = ndInside then
    Sign := -1
  else
    Sign := 1;
  HBW := FBaseWidth * 0.5;
  HBD := FBaseDepth * 0.5;
  HTW := HBW * (FApexHeight - FHeight) / FApexHeight;
  HTD := HBD * (FApexHeight - FHeight) / FApexHeight;

  glBegin(GL_QUADS);

  if [fpFront, fpBack] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBD); // angle of front plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpFront in FParts then
    begin
      glNormal3f(0, Sign * ACos, Sign * ASin);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, HBD);
    end;
    if fpBack in FParts then
    begin
      glNormal3f(0, Sign * ACos, -Sign * ASin);
      xglTexCoord2fv(@YTexPoint);     glVertex3f( HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f( HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight, -HTD);
    end;
  end;

  if [fpLeft, fpRight] * FParts <> [] then
  begin
    Angle := Arctan(FApexHeight/ HBW); // angle of side plane with bottom plane
    SinCos(Angle, ASin, ACos);
    if fpLeft in FParts then
    begin
      glNormal3f(-Sign * ACos, Sign * ACos, 0);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(-HTW, FHeight,  HTD);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(-HBW, 0,  HBD);
    end;
    if fpRight in FParts then
    begin
      glNormal3f(Sign * ACos, Sign * ACos, 0);
      xglTexCoord2fv(@YTexPoint);     glVertex3f(HTW, FHeight, HTD);
      xglTexCoord2fv(@NullTexPoint);  glVertex3f(HBW, 0,  HBD);
      xglTexCoord2fv(@XTexPoint);     glVertex3f(HBW, 0, -HBD);
      xglTexCoord2fv(@XYTexPoint);    glVertex3f(HTW, FHeight, -HTD);
    end;
  end;

  if (fpTop in FParts) and (FHeight < FApexHeight) then
  begin
    glNormal3f(0, Sign, 0);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HTW, FHeight, -HTD);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HTW, FHeight,  HTD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HTW, FHeight,  HTD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HTW, FHeight, -HTD);
  end;
  if fpBottom in FParts then
  begin
    glNormal3f(0, -Sign, 0);
    xglTexCoord2fv(@NullTexPoint);  glVertex3f(-HBW, 0, -HBD);
    xglTexCoord2fv(@XTexPoint);     glVertex3f( HBW, 0, -HBD);
    xglTexCoord2fv(@XYTexPoint);    glVertex3f( HBW, 0,  HBD);
    xglTexCoord2fv(@YTexPoint);     glVertex3f(-HBW, 0,  HBD);
  end;

  glEnd;
end;

procedure TFrustrum.SetApexHeight(AValue: TGLFloat);
begin
  if (AValue <> FApexHeight) and (AValue >= 0) then
  begin
    FApexHeight := AValue;
    if FHeight > AValue then
      FHeight := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.SetBaseDepth(AValue: TGLFloat);
begin
  if (AValue <> FBaseDepth) and (AValue >= 0) then
  begin
    FBaseDepth := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.SetBaseWidth(AValue: TGLFloat);
begin
  if (AValue <> FBaseWidth) and (AValue >= 0) then
  begin
    FBaseWidth := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.SetHeight(AValue: TGLFloat);
begin
  if (AValue <> FHeight) and (AValue >= 0) then
  begin
    FHeight := AValue;
    if FApexHeight < AValue then
      FApexHeight := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.SetParts(AValue: TFrustrumParts);
begin
  if AValue <> FParts then
  begin
    FParts := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.SetNormalDirection(AValue: TNormalDirection);
begin
  if AValue <> FNormalDirection then
  begin
    FNormalDirection := AValue;
    StructureChanged;
  end;
end;

procedure TFrustrum.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TFrustrum) then
  begin
    FApexHeight := TFrustrum(Source).FApexHeight;
    FBaseDepth := TFrustrum(Source).FBaseDepth;
    FBaseWidth := TFrustrum(Source).FBaseWidth;
    FHeight := TFrustrum(Source).FHeight;
    FParts := TFrustrum(Source).FParts;
    FNormalDirection := TFrustrum(Source).FNormalDirection;
  end;
  inherited Assign(Source);
end;

function TFrustrum.TopDepth: TGLFloat;
begin
  Result := FBaseDepth * (FApexHeight - FHeight) / FApexHeight;
end;

function TFrustrum.TopWidth: TGLFloat;
begin
  Result := FBaseWidth * (FApexHeight - FHeight) / FApexHeight;
end;

procedure TFrustrum.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('FrustrumSize', ReadData, WriteData,
    (FApexHeight <> 1) or (FBaseDepth <> 1) or (FBaseWidth <> 1) or
    (FHeight <> 0.5));
end;

procedure TFrustrum.ReadData(Stream: TStream);
begin
  with Stream do
  begin
    Read(FApexHeight, SizeOf(FApexHeight));
    Read(FBaseDepth, SizeOf(FBaseDepth));
    Read(FBaseWidth, SizeOf(FBaseWidth));
    Read(FHeight, SizeOf(FHeight));
  end;
end;

procedure TFrustrum.WriteData(Stream: TStream);
begin
  with Stream do
  begin
    Write(FApexHeight, SizeOf(FApexHeight));
    Write(FBaseDepth, SizeOf(FBaseDepth));
    Write(FBaseWidth, SizeOf(FBaseWidth));
    Write(FHeight, SizeOf(FHeight));
  end;
end;

//----------------- TQuadricObject ---------------------------------------------

// Create
//
constructor TQuadricObject.Create(AOwner : TComponent);
begin
  inherited;
  FNormals:=nsSmooth;
  FNormalDirection:=ndOutside;
end;

// SetNormals
//
procedure TQuadricObject.SetNormals(aValue : TNormalSmoothing);
begin
	if AValue<>FNormals then begin
		FNormals:=AValue;
		StructureChanged;
	end;
end;

// SetNormalDirection
//
procedure TQuadricObject.SetNormalDirection(aValue : TNormalDirection);
begin
	if AValue<>FNormalDirection then begin
		FNormalDirection:=AValue;
		StructureChanged;
  	end;
end;

// SetupQuadricParams
//
procedure TQuadricObject.SetupQuadricParams(quadric : PGLUquadricObj);
const
   cNormalSmoothinToEnum : array [nsFlat..nsNone] of TGLEnum = (
         GLU_FLAT, GLU_SMOOTH, GLU_NONE );
var
	withTexture : Boolean;
begin
	gluQuadricDrawStyle(Quadric, GLU_FILL);
	gluQuadricNormals(Quadric, cNormalSmoothinToEnum[FNormals]);
   SetNormalQuadricOrientation(Quadric);
{ TODO : things don't look that good here... we'll have to check it. }
	{WithTexture:=(stTexture1D in Scene.CurrentStates) or
					(stTexture2D in Scene.CurrentStates);}
	WithTexture:=True;
	gluQuadricTexture(Quadric, Ord(WithTexture));
end;

// SetNormalQuadricOrientation
//
procedure TQuadricObject.SetNormalQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_INSIDE, GLU_OUTSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// SetInvertedQuadricOrientation
//
procedure TQuadricObject.SetInvertedQuadricOrientation(quadric : PGLUquadricObj);
const
   cNormalDirectionToEnum : array [ndInside..ndOutside] of TGLEnum =
      (GLU_OUTSIDE, GLU_INSIDE);
begin
   gluQuadricOrientation(quadric, cNormalDirectionToEnum[FNormalDirection]);
end;

// Assign
//
procedure TQuadricObject.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TQuadricObject) then begin
      FNormals:=TQuadricObject(Source).FNormals;
      FNormalDirection:=TQuadricObject(Source).FNormalDirection;
   end;
   inherited Assign(Source);
end;

//----------------- TSphere ----------------------------------------------------

constructor TSphere.Create(AOwner:TComponent);

begin
  inherited Create(AOwner);
  FRadius:=0.5;
  FSlices:=16;
  FStacks:=16;
  FTop:=90;
  FBottom:=-90;
  FStart:=0;
  FStop:=360;
end;

// BuildList
//
procedure TSphere.BuildList(var rci : TRenderContextInfo);
var
   V1, V2, N1 : TAffineVector;
   AngTop, AngBottom, AngStart, AngStop, StepV, StepH : Extended;
   SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: Extended;
   uTexCoord, uTexFactor, vTexFactor, vTexCoord0, vTexCoord1 : Single;
   I, J: Integer;
   DoReverse: Boolean;
begin
   DoReverse:=FNormalDirection = ndInside;
   glPushAttrib(GL_POLYGON_BIT);
   if DoReverse then
      glFrontFace(GL_CW)
   else glFrontFace(GL_CCW);

   // common settings
   AngTop:=DegToRad(FTop);
   AngBottom:=DegToRad(FBottom);
   AngStart:=DegToRad(FStart);
   AngStop:=DegToRad(FStop);
   StepH:=(AngStop - AngStart) / FSlices;
   StepV:=(AngTop - AngBottom) / FStacks;
   glPushMatrix;
   glScalef(Radius, Radius, Radius);

   // top cap
   if (FTop < 90) and (FTopCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngTop, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, -1, 0)
      else glNormal3f(0, 1, 0);
      if FTopCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1[1]:=SinP;
      Theta:=AngStart;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP*SinT;
         V1[2]:=CosP*CosT;
         if FTopCap=ctCenter then begin
            N1:=VectorPerpendicular(YVector, V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT / 2 + 0.5, CosT / 2 + 0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta + StepH;
      end;
      glEnd;
   end;

   // main body
   Phi:=AngTop;
   Phi2:=Phi-StepV;
   uTexFactor:=1/FSlices;
   vTexFactor:=1/(FStacks-1);

   for J:=0 to FStacks-1 do begin
      Theta:=AngStart;
      SinCos(Phi, SinP, CosP);
      SinCos(Phi2, SinP2, CosP2);
      V1[1]:=SinP;
      V2[1]:=SinP2;
      vTexCoord0:=1-j*vTexFactor;
      vTexCoord1:=1-(j+1)*vTexFactor;

      glBegin(GL_TRIANGLE_STRIP);
      for i:=0 to FSlices do begin

         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V2[0]:=CosP2 * SinT;
         V1[2]:=CosP * CosT;
         V2[2]:=CosP2 * CosT;

         uTexCoord:=i*uTexFactor;
         xglTexCoord2f(uTexCoord, vTexCoord0);
         if DoReverse then begin
            N1:=V1;
            NegateVector(N1);
            glNormal3fv(@N1);
         end else glNormal3fv(@V1);
         glVertex3fv(@V1);

         xglTexCoord2f(uTexCoord, vTexCoord1);
         if DoReverse then begin
            N1:=V2;
            NegateVector(N1);
            glNormal3fv(@N1);
         end else glNormal3fv(@V2);
         glVertex3fv(@V2);

         Theta:=Theta+StepH;
      end;
      glEnd;
      Phi:=Phi2;
      Phi2:=Phi2 - StepV;
   end;

   // bottom cap
   if (FBottom > -90) and (FBottomCap in [ctCenter, ctFlat]) then begin
      glBegin(GL_TRIANGLE_FAN);
      SinCos(AngBottom, SinP, CosP);
      xglTexCoord2f(0.5, 0.5);
      if DoReverse then
         glNormal3f(0, 1, 0)
      else glNormal3f(0, -1, 0);
      if FBottomCap = ctCenter then
         glVertex3f(0, 0, 0)
      else begin
         glVertex3f(0, SinP, 0);
         if DoReverse then
            MakeVector(N1, 0, -1, 0)
         else N1:=YVector;
      end;
      V1[1]:=SinP;
      Theta:=AngStop;
      for I:=0 to FSlices do begin
         SinCos(Theta, SinT, CosT);
         V1[0]:=CosP * SinT;
         V1[2]:=CosP * CosT;
         if FTopCap = ctCenter then begin
            N1:=VectorPerpendicular(AffineVectorMake(0, -1, 0), V1);
            if DoReverse then NegateVector(N1);
         end;
         xglTexCoord2f(SinT*0.5+0.5, CosT*0.5+0.5);
         glNormal3fv(@N1);
         glVertex3fv(@V1);
         Theta:=Theta - StepH;
      end;
      // restore face winding
      glEnd;
   end;
   glPopMatrix;
   glPopAttrib;
end;

// SetBottom
//
procedure TSphere.SetBottom(AValue: TAngleLimit1);
begin
   if FBottom<>AValue then begin
      FBottom:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetBottomCap(AValue: TCapType);

begin
  if FBottomCap<>AValue then
  begin
    FBottomCap:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetRadius(AValue:TGLFloat);

begin
  if AValue<>FRadius then
  begin
    FRadius:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetSlices(AValue:TGLInt);

begin
  if AValue<>FSlices then
  begin
    FSlices:=AValue;
    if FSlices = 0 then FSlices:=1;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetStacks(AValue:TGLInt);

begin
  if AValue<>FStacks then
  begin
    FStacks:=AValue;
    if FStacks = 0 then FStacks:=1;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetStart(AValue: TAngleLimit2);
begin
   if FStart<>AValue then begin
      Assert(AValue <= FStop);
      FStart:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetStop(AValue: TAngleLimit2);
begin
   if FStop<>AValue then begin
      Assert(AValue >= FStart);
      FStop:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetTop(AValue: TAngleLimit1);
begin
   if FTop<>AValue then begin
      FTop:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TSphere.SetTopCap(AValue: TCapType);

begin
  if FTopCap<>AValue then
  begin
    FTopCap:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TSphere.Assign(Source:TPersistent);

begin
  if assigned(Source) and (Source is TSphere) then
  begin
    FRadius:=TSphere(Source).FRadius;
    FSlices:=TSphere(Source).FSlices;
    FStacks:=TSphere(Source).FStacks;
    FBottom:=TSphere(Source).FBottom;
    FTop:=TSphere(Source).FTop;
    FStart:=TSphere(Source).FStart;
    FStop:=TSphere(Source).FStop;
  end;
  inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TSphere.AxisAlignedDimensions : TVector;
// ToDo: take bottom and top into account
begin
   VectorScale(XYZHmgVector, Abs(FRadius), Result);
end;

//----------------- TDisk ------------------------------------------------------

// Create
//
constructor TDisk.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FOuterRadius:=0.5;
  FInnerRadius:=0;
  FSlices:=16;
  FLoops:=2;
  FStartAngle:=0;
  FSweepAngle:=360;
end;

// BuildList
//
procedure TDisk.BuildList(var rci : TRenderContextInfo);
var
   quadric : PGLUquadricObj;
begin
   quadric:=gluNewQuadric();
   SetupQuadricParams(Quadric);
   gluPartialDisk(Quadric, FInnerRadius, FOuterRadius, FSlices, FLoops, FStartAngle, FSweepAngle);
   gluDeleteQuadric(Quadric);
end;

// SetOuterRadius
//
procedure TDisk.SetOuterRadius(AValue:TGLFloat);
begin
   if AValue<>FOuterRadius then begin
      FOuterRadius:=AValue;
      StructureChanged;
   end;
end;

//------------------------------------------------------------------------------

procedure TDisk.SetInnerRadius(AValue:TGLFloat);

begin
  if AValue<>FInnerRadius then
  begin
    FInnerRadius:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TDisk.SetSlices(AValue:TGLInt);

begin
  if AValue<>FSlices then
  begin
    FSlices:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TDisk.SetLoops(AValue:TGLInt);

begin
  if AValue<>FLoops then
  begin
    FLoops:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TDisk.SetStartAngle(AValue:TGLFloat);

begin
  if AValue<>FStartAngle then
  begin
    FStartAngle:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TDisk.SetSweepAngle(AValue:TGLFloat);

begin
  if AValue<>FSweepAngle then
  begin
    FSweepAngle:=AValue;
    StructureChanged;
  end;
end;

// Assign
//
procedure TDisk.Assign(Source:TPersistent);
begin
   if Assigned(Source) and (Source is TDisk) then begin
      FOuterRadius:=TDisk(Source).FOuterRadius;
      FInnerRadius:=TDisk(Source).FInnerRadius;
      FSlices:=TDisk(Source).FSlices;
      FLoops:=TDisk(Source).FLoops;
      FStartAngle:=TDisk(Source).FStartAngle;
      FSweepAngle:=TDisk(Source).FSweepAngle;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TDisk.AxisAlignedDimensions : TVector;
var
  r : TGLFloat;
begin
   r:=Abs(FOuterRadius);
   Result:=VectorMake(r, r, 0);
end;

//----------------- TCylinderBase ----------------------------------------------

// Create
//
constructor TCylinderBase.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  FBottomRadius:=0.5;
  FHeight:=1;
  FSlices:=16;
  FStacks:=8;
  FLoops:=1;
end;

// SetBottomRadius
//
procedure TCylinderBase.SetBottomRadius(AValue : TGLFloat);
begin
	if AValue<>FBottomRadius then begin
		FBottomRadius:=AValue;
		StructureChanged;
	end;
end;

// SetHeight
//
procedure TCylinderBase.SetHeight(AValue:TGLFloat);
begin
	if AValue<>FHeight then begin
		FHeight:=AValue;
		StructureChanged;
	end;
end;

// SetSlices
//
procedure TCylinderBase.SetSlices(AValue : TGLInt);
begin
	if AValue<>FSlices then begin
		FSlices:=AValue;
		StructureChanged;
  	end;
end;

// SetStack
//
procedure TCylinderBase.SetStacks(AValue : TGLInt);
begin
	if AValue<>FStacks then begin
		FStacks:=AValue;
		StructureChanged;
	end;
end;

// SetLoops
//
procedure TCylinderBase.SetLoops(AValue : TGLInt);
begin
	if (AValue>=1) and (AValue<>FLoops) then begin
		FLoops:=AValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TCylinderBase.Assign(Source : TPersistent);
begin
	if assigned(Source) and (Source is TCylinderBase) then begin
		FBottomRadius:=TCylinderBase(Source).FBottomRadius;
		FSlices:=TCylinderBase(Source).FSlices;
		FStacks:=TCylinderBase(Source).FStacks;
		FLoops :=TCylinderBase(Source).FLoops;
		FHeight:=TCylinderBase(Source).FHeight;
	end;
	inherited Assign(Source);
end;

//----------------- TCone ------------------------------------------------------

// Create
//
constructor TCone.Create(AOwner:TComponent);
begin
	inherited Create(AOwner);
	FParts:=[coSides, coBottom];
end;

// BuildList
//
procedure TCone.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric();
	SetupQuadricParams(Quadric);
	glRotated(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if coSides in FParts then
		gluCylinder(quadric, BottomRadius, 0, Height, Slices, Stacks);
	if coBottom in FParts then begin
		// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, BottomRadius, Slices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetParts
//
procedure TCone.SetParts(AValue : TConeParts);
begin
	if AValue<>FParts then begin
		FParts:=AValue;
		StructureChanged;
	end;
end;

// Assign
//
procedure TCone.Assign(Source: TPersistent);
begin
	if Assigned(Source) and (Source is TCone) then begin
		FParts:=TCone(Source).FParts;
	end;
  	inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TCone.AxisAlignedDimensions : TVector;
var
   r : TGLFloat;
begin
   r:=Abs(FBottomRadius);
   Result:=VectorMake(r, 0.5*FHeight, r);
end;

//----------------- TCylinder --------------------------------------------------

// Create
//
constructor TCylinder.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FTopRadius:=0.5;
   FParts:=[cySides, cyBottom, cyTop];
end;

// BuildList
//
procedure TCylinder.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if cySides in FParts then
		gluCylinder(Quadric, FBottomRadius, FTopRadius, FHeight, FSlices, FStacks);
	if cyTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
	if cyBottom in FParts then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
		gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
	end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// SetTopRadius
//
procedure TCylinder.SetTopRadius(AValue: TGLFloat);
begin
   if AValue<>FTopRadius then begin
      FTopRadius:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TCylinder.SetParts(AValue: TCylinderParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// Assign
//
procedure TCylinder.Assign(Source: TPersistent);
begin
   if Assigned(SOurce) and (Source is TCylinder) then begin
      FParts:=TCylinder(Source).FParts;
      FTopRadius:=TCylinder(Source).FTopRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TCylinder.AxisAlignedDimensions: TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FBottomRadius);
  r1:=Abs(FTopRadius);
  if r1>r then r:=r1;
  Result:=VectorMake(r, 0.5*FHeight, r);
end;

//----------------- TAnnulus ---------------------------------------------------

// Create
//
constructor TAnnulus.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   fBottomInnerRadius:=0.3;
   fTopInnerRadius:=0.3;
   fTopRadius:=0.5;
   fParts:=[anInnerSides, anOuterSides, anBottom, anTop];
end;

// SetBottomInnerRadius
//
procedure TAnnulus.SetBottomInnerRadius(AValue : TGLFloat);
begin
	if AValue<>FBottomInnerRadius then begin
		FBottomInnerRadius:=AValue;
		StructureChanged;
	end;
end;

// SetTopRadius
//
procedure TAnnulus.SetTopRadius(AValue : TGLFloat);
begin
	if AValue<>FTopRadius then begin
		FTopRadius:=AValue;
		StructureChanged;
	end;
end;

// SetTopInnerRadius
//
procedure TAnnulus.SetTopInnerRadius(AValue : TGLFloat);
begin
	if AValue<>FTopInnerRadius then begin
		FTopInnerRadius:=AValue;
		StructureChanged;
	end;
end;

// SetParts
//
procedure TAnnulus.SetParts(AValue: TAnnulusParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TAnnulus.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
begin
   glPushMatrix;
	quadric:=gluNewQuadric;
	SetupQuadricParams(Quadric);
	glRotatef(-90, 1, 0, 0);
	glTranslatef(0, 0, -FHeight*0.5);
	if anOuterSides in FParts then
		gluCylinder(Quadric, fBottomRadius, fTopRadius, fHeight, fSlices, fStacks);
	if anTop in FParts then begin
		glPushMatrix;
		glTranslatef(0, 0, FHeight);
		gluDisk(Quadric,fTopInnerRadius, FTopRadius, FSlices, FLoops);
		glPopMatrix;
	end;
   if [anBottom, anInnerSides]*FParts<>[] then begin
		// swap quadric orientation because top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
   	if anBottom in FParts then
	   	gluDisk(quadric,fBottominnerRadius,FBottomRadius, FSlices, FLoops);
      if anInnerSides in fParts then
         gluCylinder(Quadric, fBottomInnerRadius, fTopInnerRadius, fHeight, fSlices, fStacks);
   end;
	gluDeleteQuadric(Quadric);
   glPopMatrix;
end;

// Assign
//
procedure TAnnulus.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TAnnulus) then begin
      FParts:=TAnnulus(Source).FParts;
      FTopRadius:=TAnnulus(Source).FTopRadius;
      FTopInnerRadius:=TAnnulus(Source).fTopInnerRadius;
      FBottomRadius:=TAnnulus(Source).fBottomRadius;
      FBottomInnerRadius:=TAnnulus(Source).fbottomInnerRadius;
   end;
   inherited Assign(Source);
end;

// AxisAlignedDimensions
//
function TAnnulus.AxisAlignedDimensions : TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FBottomRadius);
  r1:=Abs(FTopRadius);
  if r1>r then r:=r1;
  Result:=VectorMake(r, 0.5*FHeight, r);
end;

//----------------- TTorus -----------------------------------------------------

constructor TTorus.Create(AOwner:TComponent);

begin
  inherited Create(AOwner);
  FRings:=25;
  FSides:=15;
  FMinorRadius:=0.1;
  FMajorRadius:=0.4;
end;

//------------------------------------------------------------------------------

procedure TTorus.BuildList(var rci : TRenderContextInfo);

var I, J         : Integer;
    Theta, Phi,
    Theta1, 
    cosPhi, sinPhi, dist : TGLFloat;
    cosTheta, sinTheta: TGLFloat;
    cosTheta1, sinTheta1: TGLFloat;
    ringDelta, sideDelta: TGLFloat;

begin
  // handle texture generation
  Material.Texture.InitAutoTexture(nil);

  ringDelta:=2*Pi/FRings;
  sideDelta:=2*Pi/FSides;
  theta:=0;
  cosTheta:=1;
  sinTheta:=0;
  for I:=FRings-1 downto 0 do
  begin
    theta1:=theta+ringDelta;
    cosTheta1:=cos(theta1);
    sinTheta1:=sin(theta1);
    glBegin(GL_QUAD_STRIP);
    phi:=0;
    for J:=FSides downto 0 do
    begin
      phi:=phi+sideDelta;
      cosPhi:=cos(phi);
      sinPhi:=sin(phi);
      dist:=FMajorRadius+FMinorRadius*cosPhi;

      glNormal3f(cosTheta1*cosPhi, -sinTheta1*cosPhi, sinPhi);
      glVertex3f(cosTheta1*dist, -sinTheta1*dist, FMinorRadius*sinPhi);
      glNormal3f(cosTheta*cosPhi, -sinTheta*cosPhi, sinPhi);
      glVertex3f(cosTheta*dist, -sinTheta*dist, FMinorRadius*sinPhi);
    end;
    glEnd;
    theta:=theta1;
    cosTheta:=cosTheta1;
    sinTheta:=sinTheta1;
  end;
  Material.Texture.DisableAutoTexture;
end;

//------------------------------------------------------------------------------

procedure TTorus.SetMajorRadius(AValue: Single);

begin
  if FMajorRadius<>AValue then
  begin
    FMajorRadius:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TTorus.SetMinorRadius(AValue: Single);

begin
  if FMinorRadius<>AValue then
  begin
    FMinorRadius:=AValue;
    StructureChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TTorus.SetRings(AValue: Cardinal);

begin
  if FRings<>AValue then
  begin
    FRings:=AValue;
    StructureChanged;
  end;
end;

// SetSides
//
procedure TTorus.SetSides(aValue : Cardinal);
begin
   if FSides<>aValue then begin
      FSides:=aValue;
      StructureChanged;
   end;
end;

// AxisAlignedDimensions
//
function TTorus.AxisAlignedDimensions : TVector;
var
  r, r1 : TGLFloat;
begin
  r:=Abs(FMajorRadius);
  r1:=Abs(FMinorRadius);
  Result:=VectorMake(r, r1, r);
end;

//----------------- TTeapot ----------------------------------------------------

constructor TTeapot.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  FGrid:=5;
end;

//------------------------------------------------------------------------------

procedure TTeapot.BuildList(var rci : TRenderContextInfo);

const PatchData : array[0..9, 0..15] of Integer =
      ((102, 103, 104, 105,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15), // rim
       ( 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27), // body
       ( 24, 25, 26, 27, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40), // body
       ( 96, 96, 96, 96, 97, 98, 99, 100, 101, 101, 101, 101,  0,  1,  2,  3), // lid
       (  0,  1,  2,  3, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117), // lid
       (118, 118, 118, 118, 124, 122, 119, 121, 123, 126, 125, 120, 40, 39, 38, 37), // bottom
       ( 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56), // handle
       ( 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 28, 65, 66, 67), // handle
       ( 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83), // spout
       ( 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95));// spout

      CPData : array[0..126, 0..2] of TGLFloat =
      ((0.2, 0, 2.7), (0.2, -0.112, 2.7), (0.112, -0.2, 2.7), (0, -0.2, 2.7), (1.3375, 0, 2.53125), 
       (1.3375, -0.749, 2.53125), (0.749, -1.3375, 2.53125), (0, -1.3375, 2.53125),
       (1.4375, 0, 2.53125), (1.4375, -0.805, 2.53125), (0.805, -1.4375, 2.53125), 
       (0, -1.4375, 2.53125), (1.5, 0, 2.4), (1.5, -0.84, 2.4), (0.84, -1.5, 2.4), (0, -1.5, 2.4), 
       (1.75, 0, 1.875), (1.75, -0.98, 1.875), (0.98, -1.75, 1.875), (0, -1.75, 1.875), (2, 0, 1.35), 
       (2, -1.12, 1.35), (1.12, -2, 1.35), (0, -2, 1.35), (2, 0, 0.9), (2, -1.12, 0.9), (1.12, -2, 0.9), 
       (0, -2, 0.9), (-2, 0, 0.9), (2, 0, 0.45), (2, -1.12, 0.45), (1.12, -2, 0.45), (0, -2, 0.45), 
       (1.5, 0, 0.225), (1.5, -0.84, 0.225), (0.84, -1.5, 0.225), (0, -1.5, 0.225), (1.5, 0, 0.15), 
       (1.5, -0.84, 0.15), (0.84, -1.5, 0.15), (0, -1.5, 0.15), (-1.6, 0, 2.025), (-1.6, -0.3, 2.025), 
       (-1.5, -0.3, 2.25), (-1.5, 0, 2.25), (-2.3, 0, 2.025), (-2.3, -0.3, 2.025), (-2.5, -0.3, 2.25), 
       (-2.5, 0, 2.25), (-2.7, 0, 2.025), (-2.7, -0.3, 2.025), (-3, -0.3, 2.25), (-3, 0, 2.25), 
       (-2.7, 0, 1.8), (-2.7, -0.3, 1.8), (-3, -0.3, 1.8), (-3, 0, 1.8), (-2.7, 0, 1.575), 
       (-2.7, -0.3, 1.575), (-3, -0.3, 1.35), (-3, 0, 1.35), (-2.5, 0, 1.125), (-2.5, -0.3, 1.125), 
       (-2.65, -0.3, 0.9375), (-2.65, 0, 0.9375), (-2, -0.3, 0.9), (-1.9, -0.3, 0.6), (-1.9, 0, 0.6),
       (1.7, 0, 1.425), (1.7, -0.66, 1.425), (1.7, -0.66, 0.6), (1.7, 0, 0.6), (2.6, 0, 1.425), 
       (2.6, -0.66, 1.425), (3.1, -0.66, 0.825), (3.1, 0, 0.825), (2.3, 0, 2.1), (2.3, -0.25, 2.1), 
       (2.4, -0.25, 2.025), (2.4, 0, 2.025), (2.7, 0, 2.4), (2.7, -0.25, 2.4), (3.3, -0.25, 2.4), 
       (3.3, 0, 2.4), (2.8, 0, 2.475), (2.8, -0.25, 2.475), (3.525, -0.25, 2.49375), 
       (3.525, 0, 2.49375), (2.9, 0, 2.475), (2.9, -0.15, 2.475), (3.45, -0.15, 2.5125), 
       (3.45, 0, 2.5125), (2.8, 0, 2.4), (2.8, -0.15, 2.4), (3.2, 0.15, 2.4), (3.2, 0, 2.4), 
       (0, 0, 3.15), (0.8, 0, 3.15), (0.8, -0.45, 3.15), (0.45, -0.8, 3.15), (0, -0.8, 3.15),
       (0, 0, 2.85), (1.4, 0, 2.4), (1.4, -0.784, 2.4), (0.784, -1.4, 2.4), (0, -1.4, 2.4), 
       (0.4, 0, 2.55), (0.4, -0.224, 2.55), (0.224, -0.4, 2.55), (0, -0.4, 2.55), (1.3, 0, 2.55), 
       (1.3, -0.728, 2.55), (0.728, -1.3, 2.55), (0, -1.3, 2.55), (1.3, 0, 2.4), (1.3, -0.728, 2.4),
       (0.728, -1.3, 2.4), (0, -1.3, 2.4), (0, 0, 0), (1.425, -0.798, 0), (1.5, 0, 0.075), (1.425, 0, 0), 
       (0.798, -1.425, 0), (0, -1.5, 0.075), (0, -1.425, 0), (1.5, -0.84, 0.075), (0.84, -1.5, 0.075));

      Tex : array[0..1, 0..1, 0..1] of TGLFloat = (((0, 0), (1, 0)), ((0, 1), (1, 1)));

var P, Q, R, S  : array[0..3, 0..3, 0..2] of TGLFloat;
    I, J, K, L, 
    GRD      : Integer;

begin
  if FGrid < 2 then FGrid:=2;
  GRD:=FGrid;
  glPushMatrix;
  glTranslatef(0, -0.25, 0);
  glRotatef(-90, 1, 0, 0);
  glScalef(0.15, 0.15, 0.15);
  glPushAttrib(GL_POLYGON_BIT or GL_ENABLE_BIT or GL_EVAL_BIT);
  glFrontFace(GL_CW);
  glEnable(GL_AUTO_NORMAL);
  glEnable(GL_MAP2_VERTEX_3);
  glEnable(GL_MAP2_TEXTURE_COORD_2);
  for I:=0 to 9 do begin
    for J:=0 to 3 do begin
      for K:=0 to 3 do begin
        for L:=0 to 2 do begin
          P[J, K, L]:=CPData[PatchData[I, J*4+K], L];
          Q[J, K, L]:=CPData[PatchData[I, J*4+(3-K)], L];
          if L = 1 then Q[J, K, L]:=-Q[J, K, L];
          if I < 6 then begin
            R[J, K, L]:=CPData[PatchData[I, J*4+(3-K)], L];
            if L = 0 then R[J, K, L]:=-R[J, K, L];
            S[J, K, L]:=CPData[PatchData[I, J*4+K], L];
            if L < 2 then S[J, K, L]:=-S[J, K, L];
          end;
        end;
      end;
    end;
    glMapGrid2f(GRD, 0, 1, GRD, 0, 1);
    glMap2f(GL_MAP2_TEXTURE_COORD_2, 0, 1, 2, 2, 0, 1, 4, 2, @Tex);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @P);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @Q);
    glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    if I < 6 then begin
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @R);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
      glMap2f(GL_MAP2_VERTEX_3, 0, 1, 3, 4, 0, 1, 12, 4, @S);
      glEvalMesh2(GL_FILL, 0, GRD, 0, GRD);
    end;
  end;
  glPopAttrib;
  glPopMatrix;
end;

//----------------- TSpaceText ----------------------------------------------------

// Create
//
constructor TSpaceText.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FFont:=TFont.Create;
   FFont.Name:='Arial';
   FontChanged:=True;
   FExtrusion:=0;
   CharacterRange:=stcrAll;
   FFont.OnChange:=OnFontChange;
end;

// Destroy
//
destructor TSpaceText.Destroy;
begin
   FFont.OnChange:=nil;
   FFont.Free;
   inherited Destroy;
end;

// BuildList
//
procedure TSpaceText.BuildList(var rci : TRenderContextInfo);
begin
	if Length(FText) > 0 then begin
		// create texture coordinates if necessary
		//    if not (Material.Texture.Disabled)  and  not (Material.Texture.IsInherited) then
		Material.Texture.InitAutoTexture(nil);
		glPushAttrib(GL_POLYGON_BIT);
		case FCharacterRange of
			stcrAlphaNum :	glListBase(BaseList-32);
			stcrNumbers : glListBase(BaseList-Cardinal('0'));
		else
			glListBase(BaseList);
		end;
		glCallLists(Length(FText), GL_UNSIGNED_BYTE, PChar(FText));
		glPopAttrib;
		Material.Texture.DisableAutoTexture;
	end;
end;

// DoDestroyList
//
procedure TSpaceText.DoDestroyList(glsceneOnly : Boolean);
begin
   ReleaseFontManager;
   inherited;
end;

// PrepareObject
//
procedure TSpaceText.PrepareObject(var rci : TRenderContextInfo);
var
	firstChar, lastChar : Integer;
begin
	if FontChanged and (Length(FText) > 0) then with FFont do begin
		FontManager.Release(BaseList);
		case FCharacterRange of
			stcrAlphaNum : begin
				firstChar:=32; lastChar:=127;
			end;
			stcrNumbers : begin
				firstChar:=Integer('0'); lastChar:=Integer('9');
			end;
		else
			// stcrAll
			firstChar:=0; lastChar:=255;
		end;
		BaseList:=FontManager.GetFontBase(Name, Style, FExtrusion,
														FAllowedDeviation, firstChar, lastChar);
		FontChanged:=False;
	end;
	inherited;
end;

// SetExtrusion
//
procedure TSpaceText.SetExtrusion(AValue: Single);
begin
   Assert(AValue>=0, 'Extrusion must be >=0');
	if FExtrusion<>AValue then begin
		FExtrusion:=AValue;
      OnFontChange(nil);
	end;
end;

// SetAllowedDeviation
//
procedure TSpaceText.SetAllowedDeviation(const val : Single);
begin
	if FAllowedDeviation<>val then begin
		FAllowedDeviation:=val;
      OnFontChange(nil);
	end;
end;

// SetCharacterRange
//
procedure TSpaceText.SetCharacterRange(const val : TSpaceTextCharRange);
begin
	if FCharacterRange<>val then begin
		FCharacterRange:=val;
      OnFontChange(nil);
	end;
end;

// SetFont
//
procedure TSpaceText.SetFont(AFont: TFont);
begin
   FFont.Assign(AFont);
   OnFontChange(nil);
end;

// OnFontChange
//
procedure TSpaceText.OnFontChange(sender : TObject);
begin
   FontChanged:=True;
   StructureChanged;
end;

// SetText
//
procedure TSpaceText.SetText(AText: String);
begin
   if FText<>AText then begin
      FText:=AText;
      StructureChanged;
   end;
end;

//----------------- TDodecahedron ----------------------------------------------

procedure DodecahedronBuildList;
const
   A = 1.61803398875; // (Sqrt(5)+1)/2
   B = 0.61803398875; // (Sqrt(5)-1)/2
   C = 1;
const
   Vertices : array[0..19, 0..2] of TGLFloat =
      ((-A, 0, B), (-A, 0, -B), (A, 0, -B), (A, 0, B),
       (B, -A, 0), (-B, -A, 0), (-B, A, 0), (B, A, 0),
       (0, B, -A), (0, -B, -A), (0, -B, A), (0, B, A),
       (-C, -C, C), (-C, -C, -C), (C, -C, -C), (C, -C, C),
       (-C, C, C), (-C, C, -C), (C, C, -C), (C, C, C));

   Polygons : array[0..11, 0..4] of TGLInt =
      (( 0, 12, 10, 11, 16),
       ( 1, 17, 8, 9, 13),
       ( 2, 14, 9, 8, 18),
       ( 3, 19, 11, 10, 15),
       ( 4, 14, 2, 3, 15),
       ( 5, 12, 0, 1, 13),
       ( 6, 17, 1, 0, 16),
       ( 7, 19, 3, 2, 18),
       ( 8, 17, 6, 7, 18),
       ( 9, 14, 4, 5, 13),
       (10, 12, 5, 4, 15),
       (11, 19, 7, 6, 16));

var I     : Integer;
    U, V, N : TAffineVector;

begin
  glPushMatrix;
  glScalef(0.3, 0.3, 0.3);
  for I:=0 to 11 do begin
    U[0]:=Vertices[Polygons[I, 2], 0]-Vertices[Polygons[I, 1], 0];
    U[1]:=Vertices[Polygons[I, 2], 1]-Vertices[Polygons[I, 1], 1];
    U[2]:=Vertices[Polygons[I, 2], 2]-Vertices[Polygons[I, 1], 2];

    V[0]:=Vertices[Polygons[I, 0], 0]-Vertices[Polygons[I, 1], 0];
    V[1]:=Vertices[Polygons[I, 0], 1]-Vertices[Polygons[I, 1], 1];
    V[2]:=Vertices[Polygons[I, 0], 2]-Vertices[Polygons[I, 1], 2];

    VectorCrossProduct(U, V, N);
    NormalizeVector(N);

    glBegin(GL_TRIANGLE_FAN);
       glNormal3fv(@N);
       glVertex3fv(@Vertices[Polygons[I, 0], 0]);
       glVertex3fv(@Vertices[Polygons[I, 1], 0]);
       glVertex3fv(@Vertices[Polygons[I, 2], 0]);
       glVertex3fv(@Vertices[Polygons[I, 3], 0]);
       glVertex3fv(@Vertices[Polygons[I, 4], 0]);
    glEnd;
  end;
  glPopMatrix;
end;

// BuildList
//
procedure TDodecahedron.BuildList(var rci : TRenderContextInfo);
begin
   DodecahedronBuildList;
end;

//----------------- TArrowLine -------------------------------------------------

// Create
constructor TArrowLine.Create(AOwner:TComponent);
begin
   inherited;
   fTopRadius:=0.1;
   BottomRadius:=0.1;
   fTopArrowHeadRadius:=0.2;
   fTopArrowHeadHeight:=0.5;
   fBottomArrowHeadRadius:=0.2;
   fBottomArrowHeadHeight:=0.5;
   FHeadStackingStyle:=ahssStacked;
   { by default there is not much point having the top of the line (cylinder)
     showing is it is coincidental with the Toparrowhead bottom.
     Note I've defaulted to "vector" type arrows (arrow head on top only}
   fParts:=[alLine, alTopArrow];
end;

// SetTopRadius
//
procedure TArrowLine.SetTopRadius(AValue:TGLFloat);
begin
   if AValue<>fTopRadius then begin
      fTopRadius:=AValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadHeight
//
procedure TArrowLine.SetTopArrowHeadHeight(AValue:TGLFloat);
begin
   if AValue<>fTopArrowHeadHeight then begin
      fTopArrowHeadHeight:=AValue;
      StructureChanged;
   end;
end;

// SetTopArrowHeadRadius
//
procedure TArrowLine.SetTopArrowHeadRadius(AValue:TGLFloat);
begin
   if AValue<>fTopArrowHeadRadius then begin
      fTopArrowHeadRadius:=AValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadHeight
//
procedure TArrowLine.SetBottomArrowHeadHeight(AValue:TGLFloat);
begin
   if AValue<>fBottomArrowHeadHeight then begin
      fBottomArrowHeadHeight:=AValue;
      StructureChanged;
   end;
end;

// SetBottomArrowHeadRadius
//
procedure TArrowLine.SetBottomArrowHeadRadius(AValue:TGLFloat);
begin
   if AValue<>fBottomArrowHeadRadius then begin
      fBottomArrowHeadRadius:=AValue;
      StructureChanged;
   end;
end;

// SetParts
//
procedure TArrowLine.SetParts(AValue: TArrowLineParts);
begin
   if AValue<>FParts then begin
      FParts:=AValue;
      StructureChanged;
   end;
end;

// SetHeadStackingStyle
//
procedure TArrowLine.SetHeadStackingStyle(const val : TArrowHeadStackingStyle);
begin
   if val<>FHeadStackingStyle then begin
      FHeadStackingStyle:=val;
      StructureChanged;
   end;
end;

// BuildList
//
procedure TArrowLine.BuildList(var rci : TRenderContextInfo);
var
	quadric : PGLUquadricObj;
   cylHeight, cylOffset, headInfluence : Single;
begin
   case HeadStackingStyle of
      ahssCentered : headInfluence:=0.5;
      ahssIncluded : headInfluence:=1;
   else // ahssStacked
      headInfluence:=0;
   end;
   cylHeight:=Height;
   cylOffset:=-FHeight*0.5;
   // create a new quadric
 	quadric:=gluNewQuadric;
   SetupQuadricParams(Quadric);
   // does the top arrow part - the cone
   if alTopArrow in Parts then begin
      cylHeight:=cylHeight-TopArrowHeadHeight*headInfluence;
      glPushMatrix;
      glTranslatef(0, 0, Height*0.5-TopArrowHeadHeight*headInfluence);
      gluCylinder(quadric, fTopArrowHeadRadius, 0, fTopArrowHeadHeight, Slices, Stacks);
     	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fTopRadius, fTopArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fTopArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the bottom arrow part - another cone
   if alBottomArrow in Parts then begin
      cylHeight:=cylHeight-BottomArrowHeadHeight*headInfluence;
      cylOffset:=cylOffset+BottomArrowHeadHeight*headInfluence;
      glPushMatrix;
      // make the bottom arrow point in the other direction
	   glRotatef(180, 1, 0, 0);
      glTranslatef(0, 0, Height*0.5-BottomArrowHeadHeight*headInfluence);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(quadric, fBottomArrowHeadRadius, 0, fBottomArrowHeadHeight, Slices, Stacks);
   	// top of a disk is defined as outside
      SetInvertedQuadricOrientation(quadric);
      if alLine in Parts then
         gluDisk(quadric, fBottomRadius, fBottomArrowHeadRadius, Slices, FLoops)
      else gluDisk(quadric, 0, fBottomArrowHeadRadius, Slices, FLoops);
      glPopMatrix;
   end;
   // does the cylinder that makes the line
   if (cylHeight>0) and (alLine in Parts) then begin
      glPushMatrix;
      glTranslatef(0, 0, cylOffset);
      SetNormalQuadricOrientation(quadric);
      gluCylinder(Quadric, FBottomRadius, FTopRadius, cylHeight, FSlices, FStacks);
      if not (alTopArrow in Parts) then begin
         glPushMatrix;
         glTranslatef(0, 0, cylHeight);
         gluDisk(Quadric, 0, FTopRadius, FSlices, FLoops);
         glPopMatrix;
      end;
      if not (alBottomArrow in Parts) then begin
         // swap quadric orientation because top of a disk is defined as outside
         SetInvertedQuadricOrientation(quadric);
         gluDisk(quadric, 0, FBottomRadius, FSlices, FLoops);
      end;
      glPopMatrix;
   end;
   gluDeleteQuadric(Quadric);
end;

// Assign
//
procedure TArrowLine.Assign(Source: TPersistent);
begin
   if assigned(SOurce) and (Source is TArrowLine) then begin
      FParts:=TArrowLine(Source).FParts;
      FTopRadius:=TArrowLine(Source).FTopRadius;
      fTopArrowHeadHeight:=TArrowLine(Source).fTopArrowHeadHeight;
      fTopArrowHeadRadius:=TArrowLine(Source).fTopArrowHeadRadius;
      fBottomArrowHeadHeight:=TArrowLine(Source).fBottomArrowHeadHeight;
      fBottomArrowHeadRadius:=TArrowLine(Source).fBottomArrowHeadRadius;
      FHeadStackingStyle:=TArrowLine(Source).FHeadStackingStyle;
   end;
   inherited Assign(Source);
end;

// ------------------
// ------------------ TPolygonBase ------------------
// ------------------

// Create
//
constructor TPolygonBase.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   CreateNodes;
   FDivision:=10;
   FSplineMode:=lsmLines;
end;

// CreateNodes
//
procedure TPolygonBase.CreateNodes;
begin
   FNodes:=TGLNodes.Create(Self);
end;

// Destroy
//
destructor TPolygonBase.Destroy;
begin
   FNodes.Free;
   inherited Destroy;
end;

// Assign
//
procedure TPolygonBase.Assign(Source: TPersistent);
begin
   if Source is TPolygonBase then begin
      SetNodes(TPolygonBase(Source).FNodes);
      FDivision:=TPolygonBase(Source).FDivision;
      FSplineMode:=TPolygonBase(Source).FSplineMode;
   end else inherited Assign(Source);
end;

// NotifyChange
//
procedure TPolygonBase.NotifyChange(Sender : TObject);
begin
   if Sender=Nodes then StructureChanged;
   inherited;
end;

// SetDivision
//
procedure TPolygonBase.SetDivision(const value: Integer);
begin
   if Value<>FDivision then begin
      if value<1 then
         FDivision:=1
      else FDivision:=value;
      StructureChanged;
   end;
end;

// SetNodes
//
procedure TPolygonBase.SetNodes(const aNodes : TGLNodes);
begin
   FNodes.Assign(aNodes);
   StructureChanged;
end;

// SetSplineMode
//
procedure TPolygonBase.SetSplineMode(const val : TLineSplineMode);
begin
   if FSplineMode<>val then begin
      FSplineMode:=val;
      StructureChanged;
   end;
end;

// AddNode (coords)
//
procedure TPolygonBase.AddNode(const coords : TGLCoordinates);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   if Assigned(coords) then
      n.AsVector:=coords.AsVector;
   StructureChanged;
end;

// AddNode (xyz)
//
procedure TPolygonBase.AddNode(const X, Y, Z: TGLfloat);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(X, Y, Z, 1);
   StructureChanged;
end;

// AddNode (vector)
//
procedure TPolygonBase.AddNode(const value : TVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=value;
   StructureChanged;
end;

// AddNode (affine vector)
//
procedure TPolygonBase.AddNode(const value : TAffineVector);
var
   n : TGLNode;
begin
   n:=Nodes.Add;
   n.AsVector:=VectorMake(value);
   StructureChanged;
end;

// ------------------
// ------------------ TPolygon ------------------
// ------------------

// Create
//
constructor TPolygon.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   FParts:=[ppTop, ppBottom];
end;

// Destroy
//
destructor TPolygon.Destroy;
begin
   inherited Destroy;
end;

// SetParts
//
procedure TPolygon.SetParts(const val : TPolygonParts);
begin
   if FParts<>val then begin
      FParts:=val;
      StructureChanged;
   end;
end;

// Assign
//
procedure TPolygon.Assign(Source: TPersistent);
begin
   if Source is TPolygon then begin
      FParts:=TPolygon(Source).FParts;
   end;
   inherited Assign(Source);
end;

// BuildList
//
procedure TPolygon.BuildList(var rci : TRenderContextInfo);
begin
   if (Nodes.Count>1) then begin
      // tessellate top polygon
      if ppTop in FParts then begin
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, nil, 1)
         else Nodes.RenderTesselatedPolygon(True, nil, FDivision);
      end;
      // tessellate bottom polygon
      if ppBottom in FParts then begin
         if SplineMode=lsmLines then
            Nodes.RenderTesselatedPolygon(True, nil, 1, True)
         else Nodes.RenderTesselatedPolygon(True, nil, FDivision, True);
      end;
   end;
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TSphere, TCube, TFrustrum, TCylinder, TCone, TTorus,
                    TSpaceText, TTeapot, TDodecahedron, TDisk, TPlane, TSprite,
                    TDummyCube, TLines, TAnnulus, TArrowLine, TPolygon]);

finalization

   ReleaseFontManager;

end.
