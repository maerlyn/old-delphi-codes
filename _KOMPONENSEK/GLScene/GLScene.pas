// GLScene
{: Base classes and structures for GLScene.<p>

	<b>History : </b><font size=-1><ul>
      <li>03/03/01 - Egg - Added Stencil buffer support
      <li>02/03/01 - Egg - Added TGLSceneViewer.CreateSnapShot
      <li>01/03/01 - Egg - Fixed initialization of rci.proxySubObject (broke picking)
      <li>26/02/01 - Egg - Added support for GL_NV_fog_distance
      <li>25/02/01 - Egg - Proxy's subobjects are now pushed onto the picking stack
      <li>22/02/01 - Egg - Changed to InvAbsoluteMatrix code by Uwe Raabe
      <li>15/02/01 - Egg - Added SubObjects picking code by Alan Ferguson
		<li>31/01/01 - Egg - Fixed Delphi4 issue in TGLProxyObject.Notification,
                           Invisible objects are no longer depth-sorted
      <li>21/01/01 - Egg - Simplified TGLBaseSceneObject.SetName
      <li>17/01/01 - Egg - New TGLCamera.MoveAroundTarget code by Alan Ferguson,
									Fixed TGLBaseSceneObject.SetName (thx Jacques Tur),
									Fixed AbsolutePosition/AbsoluteMatrix
      <li>13/01/01 - Egg - All transformations are now always relative,
                           GlobalMatrix removed/merged with AbsoluteMatrix
      <li>10/01/01 - Egg - If OpenGL is unavailable, TGLSceneViewer will now
                           work as a regular (blank) WinControl
      <li>08/01/01 - Egg - Added DoRender for TGLLightSource & TGLCamera
      <li>04/01/01 - Egg - Fixed Picking (broken by Camera-Sprite fix)
      <li>22/12/00 - Egg - Fixed error detection in DoDestroyList
      <li>20/12/00 - Egg - Fixed bug with deleting/freeing a branch with cameras
      <li>18/12/00 - Egg - Fixed deactivation of Fog (wouldn't deactivate)
      <li>11/12/00 - Egg - Changed ConstAttenuation default to 0 (VCL persistence bug)
      <li>05/11/00 - Egg - Completed Screen->World function set,
                           finalized rendering logic change,
                           added orthogonal projection
      <li>03/11/00 - Egg - Fixed sorting pb with osRenderBlendedLast,
                           Changed camera/world matrix logic,
                           WorldToScreen/WorldToScreen now working
      <li>30/10/00 - Egg - Fixes for FindChild (thx Steven Cao)
      <li>12/10/00 - Egg - Added some doc
      <li>08/10/00 - Egg - Fixed assignment HDC/display list quirk
      <li>08/10/00 - Egg - Based on work by Roger Cao :
									Rotation property in TGLBaseSceneObject,
                           Fix in LoadFromfile to avoid name changing and lighting error,
                           Added LoadFromTextFile and SaveToTextFile,
                           Added FindSceneObject
      <li>25/09/00 - Egg - Added Null checks for SetDirection and SetUp
      <li>13/08/00 - Egg - Fixed TGLCamera.Apply when camera is not targeting,
                           Added clipping support stuff
      <li>06/08/00 - Egg - TGLCoordinates moved to GLMisc
      <li>23/07/00 - Egg - Added GetPixelColor/Depth
      <li>19/07/00 - Egg - Fixed OpenGL states messup introduces with new logic,
                           Fixed StructureChanged clear flag bug (thanks Roger Cao)
      <li>15/07/00 - Egg - Altered "Render" logic to allow relative rendering,
                           TProxyObject now renders children too
      <li>13/07/00 - Egg - Completed (?) memory-leak fixes
      <li>12/07/00 - Egg - Added 'Hint' property to TGLCustomSceneObject,
                           Completed TGLBaseSceneObject.Assign,
                           Fixed memory loss in TGLBaseSceneObject (Scaling),
                           Many changes to list destruction scheme
      <li>11/07/00 - Egg - Eased up propagation in Structure/TransformationChanged
      <li>28/06/00 - Egg - Added ObjectStyle to TGLBaseSceneObject, various
                           changes to the list/handle mechanism
		<li>22/06/00 - Egg - Added TLightStyle (suggestion by Roger Cao)
      <li>19/06/00 - Egg - Optimized SetXxxAngle
      <li>09/06/00 - Egg - First row of Geometry-related upgrades
      <li>07/06/00 - Egg - Removed dependency to 'Math',
                           RenderToFile <-> Bitmap Overload (Aaron Hochwimmer)
      <li>28/05/00 - Egg - AxesBuildList now available as a procedure,
                           Un-re-fixed TGLLightSource.DestroyList,
                           Fixed RenderToBitmap
      <li>26/05/00 - Egg - Slightly changed DrawAxes to avoid a bug in nVidia OpenGL
      <li>23/05/00 - Egg - Added first set of collision-detection methods
      <li>22/05/00 - Egg - SetXxxxAngle now properly assigns to FXxxxAngle
      <li>08/05/00 - Egg - Added Absolute?Vector funcs to TGLBaseSceneObject
      <li>08/05/00 - RoC - Fixes in TGLScene.LoadFromFile, TGLLightSource.DestroyList
      <li>26/04/00 - Egg - TagFloat now available in TGLBaseSceneObject,
									Added TGLProxyObject
      <li>18/04/00 - Egg - Added TGLObjectEffect structures,
                           TGLCoordinates.CreateInitialized
		<li>17/04/00 - Egg - Fixed BaseSceneObject.Assign (wasn't duping children),
									Removed CreateSceneObject,
                           Optimized TGLSceneViewer.Invalidate
      <li>16/04/00 - Egg - Splitted Render to Render + RenderChildren
		<li>11/04/00 - Egg - Added TGLBaseSceneObject.SetScene (thanks Uwe)
									and fixed various funcs accordingly
      <li>10/04/00 - Egg - Improved persistence logic for behaviours,
									Added RegisterGLBehaviourNameChangeEvent
		<li>06/04/00 - Egg - RebuildMatrix should be slightly faster now
		<li>05/04/00 - Egg - Added TGLBehaviour stuff,
									Angles are now public stuff in TGLBaseSceneObject
      <li>26/03/00 - Egg - Added TagFloat to TGLCustomSceneObject,
                           Parent is now longer copied in "Assign"
		<li>22/03/00 - Egg - TGLStates moved to GLMisc,
                           Removed TGLCamera.FModified stuff,
                           Fixed position bug in TGLScene.SetupLights
		<li>20/03/00 - Egg - PickObjects now uses "const" and has helper funcs,
                           Dissolved TGLRenderOptions into material and face props (RIP),
                           Joystick stuff moved to a separate unit and component
      <li>19/03/00 - Egg - Added DoProgress method and event
		<li>18/03/00 - Egg - Fixed a few "Assign" I forgot to update after adding props,
									Added bmAdditive blending mode
		<li>14/03/00 - Egg - Added RegisterGLBaseSceneObjectNameChangeEvent,
									Added BarycenterXxx and SqrDistance funcs,
									Fixed (?) AbsolutePosition,
									Added ResetPerformanceMonitor
		<li>14/03/00 - Egg - Added SaveToFile, LoadFromFile to GLScene,
		<li>03/03/00 - Egg - Disabled woTransparent handling
		<li>12/02/00 - Egg - Added Material Library
		<li>10/02/00 - Egg - Added Initialize to TGLCoordinates
      <li>09/02/00 - Egg - All GLScene objects now begin with 'TGL',
                           OpenGL now initialized upon first create of a TGLSceneViewer
		<li>07/02/00 - Egg - Added ImmaterialSceneObject,
									Added Camera handling funcs : MoveAroundTarget,
									AdjustDistanceToTarget, DistanceToTarget,
									ScreenDeltaToVector, TGLCoordinates.Translate,
                           Deactivated "specials" (ain't working yet),
                           Scaling now a TGLCoordinates
      <li>06/02/00 - Egg - balanced & secured all context activations,
									added Assert & try..finally & default galore,
                           OpenGLError renamed to EOpenGLError,
                           ShowErrorXxx funcs renamed to RaiseOpenGLError,
                           fixed CreateSceneObject (was wrongly requiring a TCustomForm),
                           fixed DoJoystickCapture error handling,
                           added TGLUpdateAbleObject
		<li>05/02/00 - Egg - Javadocisation, fixes and enhancements :<br>
									TGLSceneViewer.SetContextOptions,
									TActiveMode -> TJoystickDesignMode,
                           TGLCamera.TargetObject and TGLCamera.AutoLeveling,
                           TGLBaseSceneObject.CoordinateChanged
	</ul></font>
}
unit GLScene;
// TGLScene    - An encapsulation of the OpenGL API
// Version     - 0.5.8
// 30-DEC-99 ml: adjustments for Delphi 5

interface

{$R-}

{$I DFS.INC}

uses Windows, Classes, Controls, GLScreen, GLMisc, GLTexture, SysUtils,
	Graphics, Messages, OpenGL12, Geometry, XCollection, GLGraphics;

type

   // TGLProxyObjectOption
   //
   {: Defines which features are taken from the master object. }
   TGLProxyObjectOption = (pooEffects, pooObjects);
   TGLProxyObjectOptions = set of TGLProxyObjectOption;

const
   cDefaultProxyOptions = [pooEffects, pooObjects];

type
  TObjectHandle = TGLUInt; // a display list name or GL_LIGHTx constant

  TNormalDirection = (ndInside, ndOutside);
  TTransformationMode = (tmLocal, tmParentNoPos, tmParentWithPos);

  // used to decribe only the changes in an object, which have to be reflected in the scene
  TObjectChange = (ocSpot, ocAttenuation, ocTransformation, ocStructure);
  TObjectChanges = set of TObjectChange;

  // flags for design notification
  TSceneOperation = (soAdd, soRemove, soMove, soRename, soSelect, soBeginUpdate, soEndUpdate);

  // flags for allocated buffers
  TBuffer = (buColor, buDepth, buStencil, buAccum);
  TBuffers = set of TBuffer;

  // options for the rendering context
  TContextOption = (roDoubleBuffer, roRenderToWindow, roTwoSideLighting);
  TContextOptions = set of TContextOption;

  // IDs for limit determination
  TLimitType = (limClipPlanes, limEvalOrder, limLights, limListNesting,
                limModelViewStack, limNameStack, limPixelMapTable, limProjectionStack,
                limTextureSize, limTextureStack, limViewportDims, limAccumAlphaBits,
                limAccumBlueBits, limAccumGreenBits, limAccumRedBits, limAlphaBits,
                limAuxBuffers, limBlueBits, limGreenBits, limRedBits, limIndexBits,
                limStereo, limDoubleBuffer, limSubpixelBits, limDepthBits, limStencilBits);

	TGLBaseSceneObject = class;
	TGLSceneObjectClass = class of TGLBaseSceneObject;
	TGLCustomSceneObject = class;
	TGLScene = class;
	TGLBehaviours = class;
   TGLObjectEffects = class;
   TGLSceneViewer = class;

   // TGLObjectStyle
   //
   {: Possible styles/options for a GLScene object.<p>
   - osDirectDraw : object shall not make use of compiled call lists, but issue
      direct calls each time a render should be performed.<br>
   - osDoesTemperWithColorsOrFaceWinding : object is not "GLScene compatible" for
       color/face winding. "GLScene compatible" objects must use GLMisc functions
       for color or face winding setting (to avoid redundant OpenGL calls), for
       objects that don't comply, the internal cache must be flushed.
   }
   TGLObjectStyle = (osDirectDraw, osDoesTemperWithColorsOrFaceWinding);
   TGLObjectStyles = set of TGLObjectStyle;

	// TGLProgressEvent
	//
   {: Progression event for time-base animations/simulations.<p>
		deltaTime is the time delta since last progress and newTime is the new
      time after the progress event is completed. }
   TGLProgressEvent = procedure (Sender : TObject; const deltaTime, newTime : Double) of object;

	// TGLBaseSceneObject
   //
   {: Base class for all scene objects.<p> }
	TGLBaseSceneObject = class (TGLUpdateAbleComponent)
		private
			{ Private Declarations }
         FObjectStyle : TGLObjectStyles;
			FHandle: TObjectHandle;
         FPosition : TGLCoordinates;
         FDirection, FUp : TGLCoordinates;
         FScaling : TGLCoordinates;
			FChanges : TObjectChanges;
			FParent : TGLBaseSceneObject;
         FScene : TGLScene;
         FChildren : TList;
         FVisible : Boolean;
         FLocalMatrix, FAbsoluteMatrix, FInvAbsoluteMatrix : TMatrix;
			FLocalMatrixDirty, FAbsoluteMatrixDirty, FInvAbsoluteMatrixDirty : Boolean;
         FUpdateCount : Integer;
			FShowAxes : Boolean;
         FRotation: TGLCoordinates; // current rotation angles
			FIsCalculating: Boolean;
         FTransMode: TTransformationMode;
			FObjectsSorting : TGLObjectsSorting;
			FOnProgress : TGLProgressEvent;
			FBehaviours : TGLBehaviours;
         FObjectEffects : TGLObjectEffects;
			FTagFloat: TGLFloat;

			function Get(Index : Integer) : TGLBaseSceneObject;
			function GetCount : Integer;
         function GetIndex : Integer;
         procedure SetParent(const val : TGLBaseSceneObject);
         procedure SetIndex(AValue : Integer);
         procedure SetDirection(AVector : TGLCoordinates);
         procedure SetUp(AVector : TGLCoordinates);
         function GetMatrix : TMatrix;
			procedure SetMatrix(AValue : TMatrix);
         procedure SetPosition(APosition : TGLCoordinates);

         procedure SetPitchAngle(AValue : Single);
         procedure SetRollAngle(AValue : Single);
			procedure SetTurnAngle(AValue : Single);
         procedure SetRotation(aRotation : TGLCoordinates);
         function GetPitchAngle : Single;
         function GetTurnAngle : Single;
         function GetRollAngle : Single;

         procedure SetShowAxes(AValue: Boolean);
			procedure SetScaling(AValue: TGLCoordinates);
			procedure SetVisible(AValue: Boolean);
			procedure SetObjectsSorting(const val : TGLObjectsSorting);
			procedure SetBehaviours(const val : TGLBehaviours);
			procedure SetEffects(const val : TGLObjectEffects);
			procedure SetScene(const value : TGLScene);

		protected
			{ Protected Declarations }
         procedure Loaded; override;

			procedure DefineProperties(Filer: TFiler); override;
			procedure WriteBehaviours(stream : TStream);
         procedure ReadBehaviours(stream : TStream);
			procedure WriteEffects(stream : TStream);
         procedure ReadEffects(stream : TStream);

			procedure DrawAxes(Pattern: TGLushort);
			procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
			function  GetHandle(var rci : TRenderContextInfo) : TObjectHandle; virtual;
         //: Returns Up and Direction vectors depending on the transformation mode
			procedure GetOrientationVectors(var up, direction: TAffineVector);
			procedure RebuildMatrix;
			procedure SetName(const NewName: TComponentName); override;
			procedure SetParentComponent(Value: TComponent); override;
         procedure FreeList(glsceneOnly : Boolean);
			procedure DoDestroyList(glsceneOnly : Boolean); virtual;
         procedure DeleteChildCameras;

         property ObjectStyle : TGLObjectStyles read FObjectStyle write FObjectStyle;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			destructor Destroy; override;
			procedure Assign(Source: TPersistent); override;

         {: Holds the local transformation (relative to parent). }
			property LocalMatrix : TMatrix read FLocalMatrix;
			{: The object's absolute matrix by composing all local matrices.<p>
            Multiplying a local coordinate with this matrix gives an absolute coordinate. }
         function AbsoluteMatrix : TMatrix;
         {: See AbsoluteMatrix. }
         function AbsoluteMatrixAsAddress : PMatrix;

			{: Calculates the object's absolute inverse matrix.<p>
            Multiplying an absolute coordinate with this matrix gives a local coordinate.<p>
				The current implem uses transposition(AbsoluteMatrix), which is true
            unless you're using some scaling... }
         function InvAbsoluteMatrix : TMatrix;
         {: Calculate the direction vector in absolute coordinates. } 
         function AbsoluteDirection : TVector;
			{: Calculates the object's absolute coordinates.<p>
				The current implem is probably buggy and slow... }
			function AbsolutePosition : TVector;
         {: Returns the Absolute X Vector expressed in local coordinates. }
         function AbsoluteXVector : TVector;
         {: Returns the Absolute Y Vector expressed in local coordinates. }
         function AbsoluteYVector : TVector;
         {: Returns the Absolute Z Vector expressed in local coordinates. }
         function AbsoluteZVector : TVector;

			{: Calculates the object's square distance to a point.<p>
            pt is assumed to be in absolute coordinates,
				AbsolutePosition is considered as being the object position. }
			function SqrDistanceTo(const pt : TVector) : Single;
			{: Calculates the object's barycenter in absolute coordinates.<p>
				Default behaviour is to consider Barycenter=AbsolutePosition
				(whatever the number of children).<br>
				SubClasses where AbsolutePosition is not the barycenter should
				override this method as it is used for distance calculation, during
				rendering for instance, and may lead to visual inconsistencies. }
			function BarycenterAbsolutePosition : TVector; virtual;
			{: Calculates the object's barycenter distance to a point.<p> }
			function BarycenterSqrDistanceTo(const pt : TVector) : Single;

         {: Shall returns the object's axis aligned extensions.<p>
            The dimensions are measured from object center and are expressed
            <i>with</i> scale accounted for, in the object's coordinates
            (not in absolute coordinates).<p>
            Default value is half the object's Scale. }
         function AxisAlignedDimensions : TVector; virtual;
         {: Max component of AxisAlignedDimensions. }
         function BoundingSphereRadius : Single;
         {: Indicates if a point is within an object.<p>
            Given coordinate is an absolute coordinate.<br>
            Linear or surfacic objects shall always return False.<p>
            Default value is based on AxisAlignedDimension and a cube bounding. }
         function PointInObject(const point : TVector) : Boolean; virtual;
         {: Request to determine an intersection with a casted ray.<p>
            Given coordinates & vector are in absolute coordinates.<bR>
            rayStart may be a point inside the object, allowing retrieval of
            the multiple intersects of the ray.<p>
            When intersectXXX parameters are nil (default) implementation should
            take advantage of this to optimize calculus, if not, and an intersect
            is found, non nil parameters should be defined.<p>
            Default value is based on AxisAlignedDimension and a cube bounding. }
         function RayCastIntersect(const rayStart, rayVector : TAffineVector;
                                   intersectPoint : PAffineVector = nil;
                                   intersectNormal : PAffineVector = nil) : Boolean; virtual;

			//: Create a new scene object and add it to this object as new child
			function AddNewChild(AChild: TGLSceneObjectClass): TGLBaseSceneObject; dynamic;
			//: Create a new scene object and add it to this object as first child
			function AddNewChildFirst(AChild: TGLSceneObjectClass): TGLBaseSceneObject; dynamic;
         procedure AddChild(AChild: TGLBaseSceneObject); dynamic;
			procedure DeleteChildren; dynamic;
			procedure Insert(AIndex: Integer; AChild: TGLBaseSceneObject); dynamic;
			{: Takes a scene object out of the child list, but doesn't destroy it.<p>
				If 'KeepChildren' is true its children will be kept as new children
				in this scene object. }
			procedure Remove(AChild: TGLBaseSceneObject; KeepChildren: Boolean); dynamic;
			function IndexOfChild(AChild: TGLBaseSceneObject) : Integer;
         function FindChild(const aName : String; ownChildrenOnly : Boolean) : TGLBaseSceneObject;
         procedure MoveChildUp(anIndex : Integer);
         procedure MoveChildDown(anIndex : Integer);

			procedure DoProgress(const deltaTime, newTime : Double); override;
         procedure MoveTo(newParent : TGLBaseSceneObject); dynamic;
			procedure MoveUp;
			procedure MoveDown;
			procedure BeginUpdate; virtual;
			procedure EndUpdate; virtual;
			{: Make object-specific geometry description here.<p>
				Subclasses should MAINTAIN OpenGL states (restore the states if
				they were altered). }
			procedure BuildList(var rci : TRenderContextInfo); virtual;
			function GetParentComponent: TComponent; override;
         function HasParent: Boolean; override;
         function  IsUpdating: Boolean;
         //: Moves object along the Up vector (move up/down)
         procedure Lift(ADistance: Single);
         //: Moves object along the direction vector
			procedure Move(ADistance: Single);
         procedure Translate(tx, ty, tz : TGLFloat);
         procedure Pitch(Angle: Single);
			procedure Roll(Angle: Single);
			procedure Turn(Angle: Single);
         //: Moves camera along the right vector (move left and right)
         procedure Slide(ADistance: Single);

			procedure PrepareObject(var rci : TRenderContextInfo); virtual;
         procedure FinishObject(var rci : TRenderContextInfo); virtual;
			procedure Render(var rci : TRenderContextInfo);
         procedure DoRender(var rci : TRenderContextInfo); virtual;
			procedure RenderChildren(firstChildIndex, lastChildIndex : Integer;
                                  var rci : TRenderContextInfo); virtual;

         procedure StructureChanged; dynamic;
			//: Recalculate an orthonormal system
			procedure CoordinateChanged(Sender: TGLCoordinates); virtual;
			procedure TransformationChanged;
			procedure NotifyChange(Sender : TObject); override;
			//: Calculate matrix and let the children do the same with their's
			procedure ValidateTransformation; virtual;

         property Rotation: TGLCoordinates Read FRotation Write SetRotation;
         property PitchAngle: single Read GetPitchAngle Write SetPitchAngle stored False;
         property RollAngle: single Read GetRollAngle Write SetRollAngle stored False;
         property TurnAngle: single Read GetTurnAngle Write SetTurnAngle stored False;
         property TransformationMode: TTransformationMode read FTransMode write FTransMode default tmLocal;

			property ShowAxes: Boolean read FShowAxes write SetShowAxes default False;
         property Changes: TObjectChanges read FChanges;
         property Children[Index: Integer]: TGLBaseSceneObject read Get; default;
         property Count: Integer read GetCount;
         property Index: Integer read GetIndex write SetIndex;
			property Matrix: TMatrix read GetMatrix write SetMatrix;
			property Parent: TGLBaseSceneObject read FParent write SetParent;
			property Position: TGLCoordinates read FPosition write SetPosition;
			property Direction: TGLCoordinates read FDirection write SetDirection;
			property Up: TGLCoordinates read FUp write SetUp;
			property Scale : TGLCoordinates read FScaling write SetScaling;
			property Scene : TGLScene read FScene;
			property Visible : Boolean read FVisible write SetVisible default True;
			property ObjectsSorting : TGLObjectsSorting read FObjectsSorting write SetObjectsSorting default osInherited;
			property OnProgress : TGLProgressEvent read FOnProgress write FOnProgress;
			property Behaviours : TGLBehaviours read FBehaviours write SetBehaviours stored False;
         property Effects : TGLObjectEffects read FObjectEffects write SetEffects stored False;

		published
			{ Published Declarations }
			property TagFloat: TGLFLoat read FTagFloat write FTagFloat;

	end;

	// TGLBaseBehaviour
	//
	{: Base class for implementing behaviours in TGLScene.<p>
		Behaviours are regrouped in a collection attached to a TGLBaseSceneObject,
		and are part of the "Progress" chain of events. Behaviours allows clean
		application of time-based alterations to objects (movements, shape or
		texture changes...).<p>
		Since behaviours are implemented as classes, there are basicly two kinds
		of strategies for subclasses :<ul>
		<li>stand-alone : the subclass does it all, and holds all necessary data
			(covers animation, inertia etc.)
		<li>proxy : the subclass is an interface to and external, shared operator
			(like gravity, force-field effects etc.)
		</ul><br>
		Some behaviours may be cooperative (like force-fields affects inertia)
		or unique (e.g. only one inertia behaviour per object).<p>
		NOTES :<ul>
		<li>Don't forget to override the ReadFromFiler/WriteToFiler persistence
			methods if you add data in a subclass !
		<li>Subclasses must be registered using the RegisterXCollectionItemClass
         function
		</ul> }
	TGLBaseBehaviour = class (TXCollectionItem)
		protected
			{ Protected Declarations }
         procedure SetName(const val : String); override;

         {: Override this function to write subclass data. }
         procedure WriteToFiler(writer : TWriter); override;
         {: Override this function to read subclass data. }
         procedure ReadFromFiler(reader : TReader); override;

			{: Returns the TGLBaseSceneObject on which the behaviour should be applied.<p>
				Does NOT check for nil owners. }
			function OwnerBaseSceneObject : TGLBaseSceneObject;

		public
			{ Public Declarations }
			constructor Create(aOwner : TXCollection); override;
			destructor Destroy; override;

			procedure DoProgress(const deltaTime, newTime : Double); virtual;
	end;

   // TGLBehaviour
   //
   {: Ancestor for non-rendering behaviours.<p>
		This class shall never receive any properties, it's just here to differentiate
      rendereing and non-rendering behaviours. Rendereing behaviours are named
      "TGLObjectEffect", non-rendering effects (like inertia) are simply named
      "TGLBehaviour". }
   TGLBehaviour = class (TGLBaseBehaviour)
   end;

	TGLBehaviourClass = class of TGLBehaviour;

	// TGLBehaviours
	//
	{: Holds a list of TGLBehaviour objects.<p>
		This object expects itself to be owned by a TGLBaseSceneObject.<p> }
	TGLBehaviours = class (TXCollection)
		protected
			{ Protected Declarations }
			function GetBehaviour(index : Integer) : TGLBehaviour;

		public
			{ Public Declarations }
			constructor Create(aOwner : TPersistent); override;

         class function ItemsClass : TXCollectionItemClass; override;

			property Behaviour[index : Integer] : TGLBehaviour read GetBehaviour; default;

         function CanAdd(aClass : TXCollectionItemClass) : Boolean; override;
			procedure DoProgress(const deltaTime, newTime : Double);
	end;

   // TGLObjectEffect
   //
   {: A rendering effect that can be applied to SceneObjects.<p>
		ObjectEffect is a subclass of behaviour that gets a chance to Render
      an object-related special effect.<p>
      TGLObjectEffect should not be used as base class for custom effects,
      instead you should use the following base classes :<ul>
      <li>TGLObjectPreEffect is rendered before owner object render
      <li>TGLObjectPostEffect is rendered after the owner object render
      <li>TGLObjectAfterEffect is rendered at the end of the scene rendering
      </ul><br>NOTES :<ul>
		<li>Don't forget to override the ReadFromFiler/WriteToFiler persistence
			methods if you add data in a subclass !
		<li>Subclasses must be registered using the RegisterXCollectionItemClass
         function
		</ul> }
   TGLObjectEffect = class (TGLBehaviour)
		protected
			{ Protected Declarations }
			{: Override this function to write subclass data. }
         procedure WriteToFiler(writer : TWriter); override;
         {: Override this function to read subclass data. }
         procedure ReadFromFiler(reader : TReader); override;

		public
			{ Public Declarations }
         procedure Render(sceneViewer : TGLSceneViewer;
								  var rci : TRenderContextInfo); virtual;
   end;

   // TGLObjectPreEffect
   //
   {: An object effect that gets rendered before owner object's render.<p>
      The current OpenGL matrices and material are that of the owner object. }
	TGLObjectPreEffect = class (TGLObjectEffect)
   end;

   // TGLObjectPostEffect
   //
   {: An object effect that gets rendered after owner object's render.<p>
      The current OpenGL matrices and material are that of the owner object. }
   TGLObjectPostEffect = class (TGLObjectEffect)
   end;

   // TGLObjectAfterEffect
   //
   {: An object effect that gets rendered at scene's end.<p>
      No particular OpenGL matrices or material should be assumed. }
   TGLObjectAfterEffect = class (TGLObjectEffect)
   end;

	// TGLObjectEffects
	//
	{: Holds a list of object effects.<p>
		This object expects itself to be owned by a TGLBaseSceneObject.<p> }
	TGLObjectEffects = class (TXCollection)
		protected
			{ Protected Declarations }
			function GetEffect(index : Integer) : TGLObjectEffect;

		public
			{ Public Declarations }
			constructor Create(aOwner : TPersistent); override;

         class function ItemsClass : TXCollectionItemClass; override;

			property ObjectEffect[index : Integer] : TGLObjectEffect read GetEffect; default;

         function CanAdd(aClass : TXCollectionItemClass) : Boolean; override;
			procedure DoProgress(const deltaTime, newTime : Double);
         procedure RenderPreEffects(sceneViewer : TGLSceneViewer;
                                    var rci : TRenderContextInfo);
         {: Also take care of registering after effects with the GLSceneViewer. }
         procedure RenderPostEffects(sceneViewer : TGLSceneViewer;
                                     var rci : TRenderContextInfo);
	end;

	// TGLCustomSceneObject
	//
	{: Extended base class with material and rendering options. }
	TGLCustomSceneObject = class(TGLBaseSceneObject)
		private
			{ Private Declarations }
			FMaterial: TGLMaterial;
         FHint : String;

		protected
			{ Protected Declarations }
			procedure SetGLMaterial(AValue: TGLMaterial);
			procedure DoDestroyList(glsceneOnly : Boolean); override;

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
			procedure DoRender(var rci : TRenderContextInfo); override;

			property Material: TGLMaterial read FMaterial write SeTGLMaterial;
			property Visible: Boolean read FVisible write SetVisible default True;
         property Hint : String read FHint write FHint;
   end;

   // TGLSceneRootObject
   //
   {: This class shall be used only as a hierarchy root.<p>
      It exists only as a container and shall never be rotated/scaled etc. as
      the class type is used in parenting optimizations.<p>
      Shall never implement or add any functionality, the "Create" override
      only take cares of disabling the build list. }
   TGLSceneRootObject = class (TGLCustomSceneObject)
		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
   end;

	// TGLImmaterialSceneObject
	//
	{: Base class for objects that do not have a published "material".<p>
		Note that the material is available in public properties.<br>
		Subclassing should be reserved to structural objects. }
	TGLImmaterialSceneObject = class(TGLCustomSceneObject)
		published
			property ObjectsSorting;
			property Direction;
			property PitchAngle;
			property Position;
			property RollAngle;
			property Scale;
			property ShowAxes;
			property TransformationMode;
			property TurnAngle;
			property Up;
			property Visible;
			property OnProgress;
			property Behaviours;
         property Effects;
         property Hint;
	end;

	// TGLSceneObject
	//
	{: Base class for standard scene objects. }
	TGLSceneObject = class(TGLImmaterialSceneObject)
		published
			{ Published Declarations }
			property Material;
	end;

   // TDirectRenderEvent
   //
   TDirectRenderEvent = procedure (var rci : TRenderContextInfo) of object;

	// TDirectOpenGL
	//
	{: Provides a way to issue direct OpenGL calls during the rendering.<p>
		You can use this object to do your specific rendering task in its OnRender
      event. The OpenGL calls shall restore the OpenGL states they found when
      entering, or exclusively use the GLMisc utility functions to alter the
      states.<br> }
	TDirectOpenGL = class (TGLImmaterialSceneObject)
		private
			{ Private Declarations }
         FUseBuildList : Boolean;
         FOnRender : TDirectRenderEvent;

		protected
			{ Protected Declarations }
         procedure SetUseBuildList(const val : Boolean);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;

			procedure Assign(Source: TPersistent); override;
			procedure BuildList(var rci : TRenderContextInfo); override;

		published
			{ Published Declarations }
         property UseBuildList : Boolean read FUseBuildList write SetUseBuildList;
         property OnRender : TDirectRenderEvent read FOnRender write FOnRender;
   end;

	// TGLProxyObject
	//
	{: A full proxy object.<p>
		This object literally uses another object's Render method to do its own
		rendering, however, it has a coordinate system and a life of its own.<br>
		Use it for duplicates of an object. }
	TGLProxyObject = class (TGLBaseSceneObject)
		private
			{ Private Declarations }
			FMasterObject : TGLBaseSceneObject;
			FProxyOptions : TGLProxyObjectOptions;

		protected
			{ Protected Declarations }
			procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure SetMasterObject(const val : TGLBaseSceneObject);
			procedure SetProxyOptions(const val : TGLProxyObjectOptions);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;
         procedure DoRender(var rci : TRenderContextInfo); override;

		published
			{ Published Declarations }
         property MasterObject : TGLBaseSceneObject read FMasterObject write SetMasterObject;
         property ProxyOptions : TGLProxyObjectOptions read FProxyOptions write SetProxyOptions default cDefaultProxyOptions;

	      property ObjectsSorting;
         property Direction;
         property PitchAngle;
			property Position;
         property RollAngle;
         property Scale;
			property ShowAxes;
         property TransformationMode;
         property TurnAngle;
         property Up;
			property OnProgress;
			property Behaviours;
   end;

   // TLightStyle
   //
   {: Defines the various styles for lightsources.<p>
      - lsSpot : a spot light, oriented and with a cutoff zone (note that if
         cutoff is 180, the spot is rendered as an omni source)<br>
      - lsOmni : an omnidirectionnal source, punctual and sending light in
         all directions uniformously<br>
      - lsParallel : a parallel light, oriented as the light source is (this
         type of light help speed up rendering on non-T&L accelerated cards) }
   TLightStyle = (lsSpot, lsOmni, lsParallel);

	// TGLLightSource
	//
	{: Standard light source.<p>
		The standard GLScene light source covers spotlights, omnidirectionnal and
      parallel sources (see TLightStyle).<br>
      Lights are colored, have distance attenuation parameters and are turned
      on/off through their Shining property.<p>
		Lightsources are managed in a specific object by the TGLScene for rendering
		purposes. The maximum number of light source in a scene is limited by the
	  	OpenGL implementation (8 lights are supported under most ICD). }
   TGLLightSource = class(TGLBaseSceneObject)
      private
			{ Private Declarations }
         FLightID: TObjectHandle;
         FSpotDirection: TGLCoordinates;
         FSpotExponent, FSpotCutOff: TGLFloat;
         FConstAttenuation, FLinearAttenuation, FQuadraticAttenuation: TGLFloat;
         FShining: Boolean;
         FAmbient, FDiffuse, FSpecular: TGLColor;
         FLightStyle : TLightStyle;

		protected
			{ Protected Declarations }
	      //: light sources have different handle types than normal scene objects
      	function GetHandle(var rci : TRenderContextInfo) : TObjectHandle; override;

         procedure SetAmbient(AValue: TGLColor);
         procedure SetDiffuse(AValue: TGLColor);
         procedure SetSpecular(AValue: TGLColor);
         procedure SetConstAttenuation(AValue: TGLFloat);
         procedure SetLinearAttenuation(AValue: TGLFloat);
         procedure SetQuadraticAttenuation(AValue: TGLFloat);
         procedure SetShining(AValue: Boolean);
         procedure SetSpotDirection(AVector: TGLCoordinates);
         procedure SetSpotExponent(AValue: TGLFloat);
         procedure SetSpotCutOff(AValue: TGLFloat);
         procedure SetLightStyle(const val : TLightStyle);
         procedure DoDestroyList(glsceneOnly : Boolean); override;

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure DoRender(var rci : TRenderContextInfo); override;
         procedure CoordinateChanged(Sender: TGLCoordinates); override;
//    procedure RenderLensFlares(from, at: TAffineVector; near_clip: TGLFloat);

      published
			{ Published Declarations }
         property Ambient: TGLColor read FAmbient write SetAmbient;
         property ConstAttenuation: TGLFloat read FConstAttenuation write SetConstAttenuation;
         property Diffuse: TGLColor read FDiffuse write SetDiffuse;
         property LinearAttenuation: TGLFloat read FLinearAttenuation write SetLinearAttenuation;
         property QuadraticAttenuation: TGLFloat read FQuadraticAttenuation write SetQuadraticAttenuation;
         property Position;
         property LightStyle : TLightStyle read FLightStyle write SetLightStyle default lsSpot;
         property Shining: Boolean read FShining write SetShining default True;
         property Specular: TGLColor read FSpecular write SetSpecular;
         property SpotCutOff: TGLFloat read FSpotCutOff write SetSpotCutOff;
         property SpotDirection: TGLCoordinates read FSpotDirection write SetSpotDirection;
         property SpotExponent: TGLFloat read FSpotExponent write SetSpotExponent;
         property OnProgress;
   end;

   // TGLCameraStyle
   //
   TGLCameraStyle = (csPerspective, csOrthogonal);

   // TGLCamera
   //
   {: Camera object.<p>
      This object is commonly referred by TGLSceneViewer and defines a position,
      direction, focal length, depth of view... all the properties needed for
      defining a point of view and optical characteristics. }
	TGLCamera = class(TGLBaseSceneObject)
      private
         { Private Declarations }
         FFocalLength: Single;
			FDepthOfView: Single;
         FNearPlane: Single;                  // nearest distance to the camera
         FViewPortRadius : Single;            // viewport bounding radius per distance unit
         FTargetObject : TGLBaseSceneObject;
         FLastDirection : TVector; // Not persistent
         FCameraStyle : TGLCameraStyle;

		protected
			{ Protected Declarations }
			procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			procedure SetTargetObject(const val : TGLBaseSceneObject);
         procedure SetDepthOfView(AValue: Single);
			procedure SetFocalLength(AValue: Single);
         procedure SetCameraStyle(const val : TGLCameraStyle);
			procedure DoDestroyList(glsceneOnly : Boolean); override;

		public
			{ Public Declarations }
			constructor Create(aOwner : TComponent); override;
			destructor Destroy; override;

         {: Nearest clipping plane for the frustum.<p>
            This value depends on the FocalLength and DepthOfView fields and
            is calculated to minimize Z-Buffer crawling as suggested by the
            OpenGL documentation. }
         property NearPlane : Single read FNearPlane;

			//: Apply camera transformation
			procedure Apply;
         procedure DoRender(var rci : TRenderContextInfo); override;
			procedure ApplyPerspective(Viewport: TRectangle; Width, Height: Integer; DPI: Integer);
			procedure AutoLeveling(Factor: Single);
			procedure Reset;
			//: Position the camera so that the whole scene can be seen
			procedure ZoomAll;
			{: Change camera's position to make it move around its target.<p>
				If TargetObject is nil, nothing happens. This method helps in quickly
				implementing camera controls. Camera's Up and Direction properties
				are unchanged.<br>
				Angle deltas are in degrees, camera parent's coordinates should be identity.<p>
				Tip : make the camera a child of a "target" dummycube and make
				it a target the dummycube. Now, to pan across the scene, just move
				the dummycube, to change viewing angle, use this method. }
			procedure MoveAroundTarget(pitchDelta, turnDelta : Single);
			{: Adjusts distance from camera to target by applying a ratio.<p>
				If TargetObject is nil, nothing happens. This method helps in quickly
				implementing camera controls. Only the camera's position is changed.<br>
				Camera parent's coordinates should be identity. }
			procedure AdjustDistanceToTarget(distanceRatio : Single);
			{: Returns the distance from camera to target.<p>
				If TargetObject is nil, returns 1. }
			function DistanceToTarget : Single;
			{: Calculate an absolute translation vector from a screen vector.<p>
				Ratio is applied to both screen delta, planeNormal should be the
				translation plane's normal. }
			function ScreenDeltaToVector(deltaX, deltaY : Integer; ratio : Single;
												  const planeNormal : TVector) : TVector;
			{: Same as ScreenDeltaToVector but optimized for XY plane. }
			function ScreenDeltaToVectorXY(deltaX, deltaY : Integer; ratio : Single) : TVector;
         {: Same as ScreenDeltaToVector but optimized for XZ plane. }
         function ScreenDeltaToVectorXZ(deltaX, deltaY : Integer; ratio : Single) : TVector;
         {: Same as ScreenDeltaToVector but optimized for YZ plane. }
         function ScreenDeltaToVectorYZ(deltaX, deltaY : Integer; ratio : Single) : TVector;

		published
			{ Published Declarations }
         {: Depth of field/view.<p>
            Adjusts the maximum distance, beyond which objects will be clipped
            (ie. not visisble).<p>
            You must adjust this value if you are experiencing disappearing
            objects (increase the value) of Z-Buffer crawling (decrease the
            value). Z-Buffer crawling happens when depth of view is too large
            and the Z-Buffer precision cannot account for all that depth
            accurately : objects farther overlap closer objects and vice-versa. }
			property DepthOfView: Single read FDepthOfView write SetDepthOfView;
         {: Focal Length of the camera.<p>
            Adjusting this value allows for zooming effects. }
			property FocalLength: Single read FFocalLength write SetFocalLength;
			{: If set, camera will point to this object.<p>
				When camera is pointing an object, the Direction vector is ignored
				and the Up vector is used as an absolute vector to the up. }
			property TargetObject : TGLBaseSceneObject read FTargetObject write SetTargetObject;
         {: Adjust the camera style.<p>
            Two styles are available : csPerspective, the default value for
            perspective projection and csOrthogonal for orthogonal (or isometric)
            projection. } 
         property CameraStyle : TGLCameraStyle read FCameraStyle write SetCameraStyle default csPerspective;

			property Position;
			property Direction;
			property Up;
         property OnProgress;
	end;

	// TGLScene
	//
	TGLScene = class(TGLUpdateAbleComponent)
      private
			{ Private Declarations }
         FUpdateCount: Integer;
         FObjects: TGLSceneRootObject;
         FCameras: TGLBaseSceneObject;
         FBaseContext: HGLRC;
         FLights, FViewers: TList;
         FLasTGLCamera, FCurrenTGLCamera: TGLCamera;
         FCurrentViewer: TGLSceneViewer;
         FObjectsSorting : TGLObjectsSorting;
         FOnProgress : TGLProgressEvent;

		protected
			{ Protected Declarations }
			procedure AddLight(ALight: TGLLightSource);
         procedure SetupLights(Maximum: Integer);
         procedure DoAfterRender;
         procedure GetChildren(AProc: TGetChildProc; Root: TComponent); override;
			procedure Loaded; override;
			procedure RemoveLight(ALight: TGLLightSource);
			procedure SetChildOrder(AChild: TComponent; Order: Integer); override;
			procedure SetObjectsSorting(const val : TGLObjectsSorting);

		public
			{ Public Declarations }
			constructor Create(AOwner: TComponent); override;
			destructor Destroy; override;

{$ifndef DFS_DELPHI_5_UP}
			procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$endif}

			procedure AddViewer(AViewer: TGLSceneViewer);
         procedure BeginUpdate;
			procedure RenderScene(AViewer: TGLSceneViewer;
                               const viewPortSizeX, viewPortSizeY : Integer);
         procedure EndUpdate;
         function  IsUpdating: Boolean;
         procedure NotifyChange(Sender : TObject); override;
         procedure RemoveViewer(AViewer: TGLSceneViewer);
			procedure ValidateTransformation(ACamera: TGLCamera);
         {: Returns True is an RC was actually activated. }
         function ActivateDefaultRenderingContext : Boolean;
         procedure Progress(const deltaTime, newTime : Double);

         function FindSceneObject(const name : String) : TGLBaseSceneObject;

         procedure ShutdownAllLights;

         {: Saves the scene to a file (recommended extension : .GLS) }
			procedure SaveToFile(const fileName : String);
         {: Load the scene from a file.<p>
            Existing objects/lights/cameras are freed, then the file is loaded.<br>
            Delphi's IDE is not handling this behaviour properly yet, ie. if
            you load a scene in the IDE, objects will be properly loaded, but
            no declare will be placed in the code. }
			procedure LoadFromFile(const fileName : String);

         {: Saves the scene to a text file }
         procedure SaveToTextFile(const fileName : String);
         {: Load the scene from a text files.<p>
            See LoadFromFile for details. }
         procedure LoadFromTextFile(const fileName : String);

         property Cameras: TGLBaseSceneObject read FCameras;
			property CurrentGLCamera: TGLCamera read FCurrentGLCamera;
         property Lights: TList read FLights;
         property Objects: TGLSceneRootObject read FObjects;
         property CurrentViewer: TGLSceneViewer read FCurrentViewer;

		published
			{ Published Declarations }
			property ObjectsSorting : TGLObjectsSorting read FObjectsSorting write SetObjectsSorting default osRenderBlendedLast;
         property OnProgress : TGLProgressEvent read FOnProgress write FOnProgress;

   end;

   TPickSubObjects = array of LongInt;

   PPickRecord = ^TPickRecord;
   TPickRecord = record
	   AObject    : TGLBaseSceneObject;
      SubObjects : TPickSubObjects;
      ZMin, ZMax : Single;
   end;

   TPickSortType = (psDefault, psName, psMinDepth, psMaxDepth);

   // TGLPickList
   //
   {: List class for object picking. }
   TGLPickList =  class(TList)
      private
         { Private Declarations }
         function GetFar(aValue : Integer) : Single;
         function GetHit(aValue : Integer) : TGLBaseSceneObject;
	      function GetNear(aValue : Integer) : Single;
         function GetSubObjects(aValue : Integer) : TPickSubObjects;

      protected
         { Protected Declarations }

      public
         { Public Declarations }
         constructor Create(SortType: TPickSortType);
         destructor Destroy; override;
	      procedure AddHit(obj : TGLBaseSceneObject; subObj : TPickSubObjects;
                          zMin, zMax : Single);
         procedure Clear; override;
         function FindObject(AObject: TGLBaseSceneObject): Integer;
         property FarDistance[Index: Integer]: Single read GetFar;
         property Hit[Index: Integer]: TGLBaseSceneObject read GetHit; default;
         property NearDistance[Index: Integer]: Single read GetNear;
   		property SubObjects[Index: Integer]: TPickSubObjects read GetSubObjects;
   end;

   TDrawState = (dsNone, dsRendering, dsPicking, dsPrinting);

   // TFogMode
   //
   TFogMode = (fmLinear, fmExp, fmExp2);

   // TFogDistance
   //
   TFogDistance = (fdDefault, fdEyeRadial, fdEyePlane);

   // TGLFogEnvironment
   //
   {: Parameters for fog environment in a scene.<p>
      The fog descibed by this object is a distance-based fog, ie. the "intensity"
      of the fog is given by a formula depending solely on the distance, this
      intensity is used for blending to a fixed color. }
   TGLFogEnvironment = class (TGLUpdateAbleObject)
      private
         { Private Declarations }
         FSceneViewer : TGLSceneViewer;
         FFogColor : TGLColor;       // alpha value means the fog density
         FFogStart, FFogEnd : TGLfloat;
         FFogMode : TFogMode;
         FFogDistance : TFogDistance;
         FChanged : Boolean;

      protected
         { Protected Declarations }
         procedure SetFogColor(Value: TGLColor);
         procedure SetFogStart(Value: TGLfloat);
         procedure SetFogEnd(Value: TGLfloat);
         procedure SetFogMode(Value: TFogMode);
         procedure SetFogDistance(const val : TFogDistance);

      public
         { Public Declarations }
         constructor Create(Owner : TPersistent); override;
         destructor Destroy; override;

         procedure NotifyChange(Sender : TObject); override;
         procedure ApplyFog;
         procedure Assign(Source: TPersistent); override;

      published
         { Published Declarations }
         {: Color of the fog when it is at 100% intensity. }
         property FogColor: TGLColor read FFogColor write SetFogColor;
         {: Minimum distance for fog, what is closer is not affected. }
         property FogStart: TGLfloat read FFogStart write SetFogStart;
         {: Maximum distance for fog, what is farther is at 100% fog intensity. }
         property FogEnd: TGLfloat read FFogEnd write SetFogEnd;
         {: The formula used for converting distance to fog intensity. }
         property FogMode: TFogMode read FFogMode write SetFogMode;
         {: Adjusts the formula used for calculating fog distances.<p>
            This option is honoured if and only if the OpenGL ICD supports the
            GL_NV_fog_distance extension, otherwise, it is ignored.<ul>
               <li>fdDefault: let OpenGL use its default formula
               <li>fdEyeRadial: uses radial "true" distance (best quality)
               <li>fdEyePlane: uses the distance to the projection plane
                  (same as Z-Buffer, faster)
            </ul> }
         property FogDistance : TFogDistance read FFogDistance write SetFogDistance default fdDefault;
   end;

   TSpecial = (spLensFlares, spLandScape);
   TSpecials = set of TSpecial;

   // TGLSceneViewer
   //
   {: Component where the GLScene objects get rendered.<p>
      This component delimits the area where OpenGL renders the scene,
      it represents the 3D scene viewed from a camera (specified in the
      camera property). This component can also render to a file or to a bitmap.<p>
      Even if it is primarily a windowed component, it can handle full-screen
      operations : adjust display resolution with DisplayOptions, and simply
      make this component fit the whole screen (use a borderless form).<p>
      This viewer also allows to define rendering options such a fog, face culling,
      depth testing, etc. and can take care of framerate calculation.<p> }
   TGLSceneViewer = class(TWinControl)
      private
         { Private Declarations }
         // handle
         FRenderingContext: HGLRC;
         FIsOpenGLAvailable: Boolean;

         // OpenGL properties
         FMaxLightSources: Integer;
         FDoubleBuffered, FFaceCulling, FFogEnable, FLighting : Boolean;
         FDepthTest, FStencilTest : Boolean;

         FCurrentStates : TGLStates;
         FBackgroundColor: TColor;
         FBackground: TGLTexture;

         // private variables
         FCanvas: TCanvas;
         FFrames: Longint;          // used to perform monitoring
         FTicks: TLargeInteger;     // used to perform monitoring
         FState: TDrawState;
         FMonitor: Boolean;
         FFramesPerSecond: TGLFloat;
         FViewPort: TRectangle;
         FBuffers: TBuffers;
         FDisplayOptions: TDisplayOptions;
         FContextOptions: TContextOptions;
         FCamera: TGLCamera;
         FFogEnvironment: TGLFogEnvironment;
         invalidated : Boolean;
         afterRenderEffects : TList;
         FProjectionMatrix, FModelViewMatrix : TMatrix;
         FBaseProjectionMatrix : TMatrix;
         FCameraAbsolutePosition : TVector;

         FBeforeRender: TNotifyEvent;
         FPostRender : TNotifyEvent;
         FAfterRender: TNotifyEvent;

         procedure SetBackgroundColor(AColor: TColor);
         //: Determine the limit of the given kind from OpenGL implementation
         function  GetLimit(Which: TLimitType): Integer;
         procedure SetCamera(ACamera: TGLCamera);
         procedure SetContextOptions(Options: TContextOptions);
         procedure SetDepthTest(AValue: Boolean);
         procedure SetStencilTest(val : Boolean);
         procedure SetFaceCulling(AValue: Boolean);
         procedure SetLighting(AValue: Boolean);
         procedure SetFogEnable(AValue: Boolean);
         procedure SeTGLFogEnvironment(AValue: TGLFogEnvironment);

         procedure PrepareRenderingMatrices(const aViewPort : TRectangle;
                              resolution : Integer; pickingRect : PRect = nil);
         procedure DoBaseRender(const aViewPort : TRectangle; resolution : Integer);

         procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); Message WM_ERASEBKGND;
         procedure WMPaint(var Message: TWMPaint); Message WM_PAINT;
         procedure WMSize(var Message: TWMSize); Message WM_SIZE;
         procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;

      protected
         { Protected Declarations }
         procedure CreateParams(var Params: TCreateParams); override;
         procedure CreateWnd; override;
         {: Clear all allocated OpenGL buffers.<p>
            The color buffer is a special case, because transparency must be
            simulated if required. }
         procedure ClearBuffers;
         procedure DestroyRC;
         procedure DestroyWindowHandle; override;
         procedure Loaded; override;
         function ObjectInScene(Obj: TGLBaseSceneObject): Boolean;
         procedure ReadContextProperties;
         procedure SetupRenderingContext;

      public
         { Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor  Destroy; override;

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

         //: Fills the PickList with objects in Rect area
         procedure PickObjects(const Rect: TRect; PickList: TGLPickList; ObjectCountGuess: Integer);
         {: Returns a PickList with objects in Rect area.<p>
            Returned list should be freed by caller.<br>
            Objects are sorted by depth (nearest objects first). }
         function GetPickedObjects(const Rect: TRect; objectCountGuess : Integer = 64) : TGLPickList;
         //: Returns the nearest object at x, y coordinates or nil if there is none
         function GetPickedObject(x, y : Integer) : TGLBaseSceneObject;

         //: Returns the color of the pixel at x, y in the frame buffer
         function GetPixelColor(x, y : Integer) : TColor;
         {: Returns the depth (Z buffer) of the pixel at x, y in the frame buffer.<p>
            This value does not map to the actual eye-object distance, but to
            a depth buffer value in the [0; 1] range. }
         function GetPixelDepth(x, y : Integer) : Single;

         {: Renders the scene on the viewer.<p>
            You do not need to call this method, unless you explicitly want a
            render at a specific time. If you just want the control to get
            refreshed, use Invalidate instead. }
         procedure Render;
         {: Render the scene to a bitmap at given DPI.<p>
            DPI = "dots per inch".<p>
            The "magic" DPI of the screen is 96 under Windows. }
         procedure RenderToBitmap(ABitmap: TBitmap; DPI: Integer = 0);
         {: Render the scene to a bitmap at given DPI and saves it to a file.<p>
            DPI = "dots per inch".<p>
            The "magic" DPI of the screen is 96 under Windows. }
         procedure RenderToFile(const AFile: String; DPI: Integer = 0); overload;
         {: Renders to bitmap of given size, then saves it to a file.<p>
            DPI is adjusted to make the bitmap similar to the viewer. }
         procedure RenderToFile(const AFile: String; bmpWidth, bmpHeight : Integer); overload;
         {: Create a TGLBitmap32 that is a snapshot of current OpenGL content.<p>
            When possible, use this function instead of RenderToBitmap, it won't
            request a redraw and will be significantly faster.<p>
            The returned TGLBitmap32 should be freed by calling code. }
         function CreateSnapShot : TGLBitmap32;

         procedure Invalidate; override;
         procedure SetViewPort(X, Y, W, H: Integer);
         {: Displays a window with info on current OpenGL ICD and context. }
         procedure ShowInfo;
         {: Resets the perfomance monitor and begin a new statistics set.<p>
            See FramesPerSecond. }
         procedure ResetPerformanceMonitor;

         {: Returns the projection matrix in use or used for the last rendering. }
         property ProjectionMatrix : TMatrix read FProjectionMatrix;
         {: Returns the modelview matrix in use or used for the last rendering. }
         property ModelViewMatrix : TMatrix read FModelViewMatrix;
         {: Returns the base projection matrix in use or used for the last rendering.<p>
            The "base" projection is (as of now) either identity or the pick
            matrix, ie. it is the matrix on which the perspective or orthogonal
            matrix gets applied. }
         property BaseProjectionMatrix : TMatrix read FBaseProjectionMatrix;

         {: Converts a screen coordinate into world (3D) coordinates.<p>
            This methods wraps a call to gluUnProject.<p>
            Note that screen coord (0,0) is the lower left corner. }
         function ScreenToWorld(const aPoint : TAffineVector) : TAffineVector; overload;
         {: Converts a screen pixel coordinate into 3D world coordinates.<p>
            This function accepts standard canvas coordinates, with (0,0) being
            the top left corner. }
         function ScreenToWorld(screenX, screenY : Integer) : TAffineVector; overload;
         {: Converts an absolute world coordinate into screen coordinate.<p>
            This methods wraps a call to gluProject.<p>
            Note that screen coord (0,0) is the lower left corner. }
         function WorldToScreen(const aPoint : TAffineVector) : TAffineVector;
         {: Calculates the 3D vector corresponding to a 2D screen coordinate.<p>
            The vector originates from the camera's absolute position and is
            expressed in absolute coordinates.<p>
            Note that screen coord (0,0) is the lower left corner. }
         function ScreenToVector(const aPoint : TAffineVector) : TAffineVector;
         {: Calculates intersection between a plane and screen vector.<p>
            If an intersection is found, returns True and places result in
            intersectPoint. }
         function ScreenVectorIntersectWithPlane(
               const aScreenPoint : TAffineVector;
               const planePoint, planeNormal : TAffineVector;
               var intersectPoint : TAffineVector) : Boolean;
         {: Calculates intersection between plane XY and screen vector.<p>
            If an intersection is found, returns True and places result in
            intersectPoint. }
         function ScreenVectorIntersectWithPlaneXY(
               const aScreenPoint : TAffineVector; const z : TGLFloat;
               var intersectPoint : TAffineVector) : Boolean;
         {: Calculates intersection between plane YZ and screen vector.<p>
            If an intersection is found, returns True and places result in
            intersectPoint. }
         function ScreenVectorIntersectWithPlaneYZ(
               const aScreenPoint : TAffineVector; const x : TGLFloat;
               var intersectPoint : TAffineVector) : Boolean;
         {: Calculates intersection between plane XZ and screen vector.<p>
            If an intersection is found, returns True and places result in
            intersectPoint. }
         function ScreenVectorIntersectWithPlaneXZ(
               const aScreenPoint : TAffineVector; const y : TGLFloat;
               var intersectPoint : TAffineVector) : Boolean;

         property IsOpenGLAvailable : Boolean read FIsOpenGLAvailable;
         property Buffers : TBuffers read FBuffers;
         property Canvas : TCanvas read FCanvas;
         property CurrentStates : TGLStates read FCurrentStates;
         property DoubleBuffered : Boolean read FDoubleBuffered;
         {: Current FramesPerSecond rendering speed.<p>
            You must set Monitor to true and keep the renderer busy to get
            accurate figures from this property.<p>
            This is an average value, to reset the counter, call
            ResetPerfomanceMonitor. }
         property FramesPerSecond : TGLFloat read FFramesPerSecond;
         {: Retrieve one the OpenGL limits for the current viewer.<p>
            Limits include max texture size, OpenGL stack depth, etc. }
         property LimitOf[Which : TLimitType] : Integer read GetLimit;
         property MaxLightSources : Integer read FMaxLightSources;
         property RenderingContext : HGLRC read FRenderingContext;
         property State: TDrawState read FState;

      published
         { Public Declarations }
         {: Fog environment options. }
         property FogEnvironment: TGLFogEnvironment read FFogEnvironment write SeTGLFogEnvironment;
         {: Color used for filling the background prior to any rendering. }
         property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
         {: The camera from which the scene is rendered.<p>
            A camera is an object you can add and define in a TGLScene component. }
         property Camera: TGLCamera read FCamera write SetCamera;
         property Constraints;
         property ContextOptions: TContextOptions read FContextOptions write SetContextOptions default [roDoubleBuffer, roRenderToWindow];
         {: DepthTest enabling.<p>
            When DepthTest is enabled, objects closer to the camera will hide
            farther (Z-Buffering is used).<br>
            When DepthTest is disable, the latest objects drawn/rendered overlap
            all previous objects, whatever their distance to the camera. }
         property DepthTest : Boolean read FDepthTest write SetDepthTest default True;
         {: Request a stencil buffer.<p>
            When set to True, a Stencil buffer will be requested to OpenGL, the
            stencil buffer allows a variety of special effects, but be aware that
            old 3D boards may not accelerate stencil rendering, thus triggering
            a fallback to software mode (non-accelerated). }  
         property StencilTest : Boolean read FStencilTest write SetStencilTest default False;
         {: DisplayOptions allows adjusting screen resolution and color depth. }
         property DisplayOptions: TDisplayOptions read FDisplayOptions write FDisplayOptions;
         {: Enable or disable face culling in the renderer.<p>
            Face culling is used in hidden faces removal algorithms : each face
            is given a normal or 'outside' direction. When face culling is enabled,
            only faces whose normal points towards the observer are rendered. }
         property FaceCulling: Boolean read FFaceCulling write SetFaceCulling default True;
         {: Toggle to enable or disable the fog settings. }
         property FogEnable: Boolean read FFogEnable write SetFogEnable default False;
         {: Toggle to enable or disable lighting calculations.<p>
            When lighting is enabled, objects will be lit according to lightsources,
            when lighting is disabled, objects are rendered in their own colors,
            without any shading.<p>
            Lighting does NOT generate shadows in OpenGL. }
         property Lighting: Boolean read FLighting write SetLighting default True;
         {: Toggle to enable or disable framerate monitoring.<p>
            When set to true, you can retrieve the rendering framerate with
            the FramesPerSecond property. }
         property Monitor: Boolean read FMonitor write FMonitor default False;

         {: Triggered before the scene's objects get rendered.<p>
            You may use this event to execute your own OpenGL rendering. }
         property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender;
         {: Triggered just after all the scene's objects have been rendered.<p>
            The OpenGL context is still active in this event, and you may use it
            to execute your own OpenGL rendering.<p> }
         property PostRender: TNotifyEvent read FPostRender write FPostRender;
         {: Called after rendering.<p>
            You cannot issue OpenGL calls in this event, if you want to do your own
            OpenGL stuff, use the PostRender event. }
         property AfterRender: TNotifyEvent read FAfterRender write FAfterRender;

         property Align;
         property Anchors;
         property DragCursor;
         property DragMode;
         property Enabled;
         property HelpContext;
         property Hint;
         property PopupMenu;
         property Visible;

         property OnClick;
         property OnDragDrop;
         property OnDragOver;
         property OnMouseDown;
         property OnMouseMove;
         property OnMouseUp;

         procedure Recreate;
  end;

  EOpenGLError = class(Exception);

{: Gets the oldest error from OpenGL engine and tries to clear the error queue.<p> }
procedure CheckOpenGLError;
procedure RaiseOpenGLError(const msg : String);

{: Register an event handler triggered by any TGLBaseSceneObject Name change.<p>
	*INCOMPLETE*, currently allows for only 1 (one) event, and is used by
	GLSceneEdit in the IDE. }
procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent : TNotifyEvent);
{: Deregister an event handler triggered by any TGLBaseSceneObject Name change.<p>
	See RegisterGLBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent : TNotifyEvent);
{: Register an event handler triggered by any TGLBehaviour Name change.<p>
	*INCOMPLETE*, currently allows for only 1 (one) event, and is used by
	FBehavioursEditor in the IDE. }
procedure RegisterGLBehaviourNameChangeEvent(notifyEvent : TNotifyEvent);
{: Deregister an event handler triggered by any TGLBaseSceneObject Name change.<p>
	See RegisterGLBaseSceneObjectNameChangeEvent. }
procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent : TNotifyEvent);

{: Issues OpenGL calls for drawing X, Y, Z axes in a standard style. }
procedure AxesBuildList(Pattern: TGLushort; AxisLen: TGLFloat);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
   Consts, Dialogs, ExtDlgs, Forms, GLStrings, Info, VectorLists, XOpenGL;

const
   GLAllStates = [stAlphaTest..stStencilTest];

var
   vCounterFrequency : TLargeInteger;

procedure TGLSceneViewer.Recreate;
begin
   RecreateWnd;
end;

//------------------ external global routines ----------------------------------

// CheckOpenGLError
//
procedure CheckOpenGLError;
var
   GLError: TGLEnum;
	Count: Word;
begin
	GLError:=glGetError;
	if GLError <> GL_NO_ERROR then begin
		Count:=0;
      // Because under some circumstances reading the error code creates a new error
      // and thus hanging up the thread, we limit the loop to 6 reads.
      try
         while (glGetError <> GL_NO_ERROR) and (Count < 6) do Inc(Count);
      except
         // Egg : ignore exceptions here, will perhaps avoid problem expressed before
		end;
		raise EOpenGLError.Create(gluErrorString(GLError));
	end;
end;

procedure RaiseOpenGLError(const msg : String);
begin
	raise EOpenGLError.Create(msg);
end;

// AxesBuildList
//
procedure AxesBuildList(Pattern: TGLushort; AxisLen: TGLFloat);
begin
   glPushAttrib(GL_ENABLE_BIT or GL_LIGHTING_BIT or GL_LINE_BIT);
   glDisable(GL_LIGHTING);
   glEnable(GL_LINE_STIPPLE);
   glEnable(GL_LINE_SMOOTH);
   glEnable(GL_BLEND);
   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glLineWidth(1);
   glLineStipple(1, Pattern);
   glBegin(GL_LINES);
      glColor3f(0.5, 0.0, 0.0); glVertex3f(0, 0, 0); glVertex3f(-AxisLen, 0, 0);
      glColor3f(1.0, 0.0, 0.0); glVertex3f(0, 0, 0); glVertex3f(AxisLen, 0, 0);
      glColor3f(0.0, 0.5, 0.0); glVertex3f(0, 0, 0); glVertex3f(0, -AxisLen, 0);
      glColor3f(0.0, 1.0, 0.0); glVertex3f(0, 0, 0); glVertex3f(0, AxisLen, 0);
      glColor3f(0.0, 0.0, 0.5); glVertex3f(0, 0, 0); glVertex3f(0, 0, -AxisLen);
      glColor3f(0.0, 0.0, 1.0); glVertex3f(0, 0, 0); glVertex3f(0, 0, AxisLen);
   glEnd;
   glPopAttrib;
   // clear fpu exception flag (sometime raised by the call to glEnd)
   asm fclex end;
end;

//------------------ internal global routines ----------------------------------

var
	vGLBaseSceneObjectNameChangeEvent : TNotifyEvent;
	vGLBehaviourNameChangeEvent : TNotifyEvent;

// RegisterGLBaseSceneObjectNameChangeEvent
//
procedure RegisterGLBaseSceneObjectNameChangeEvent(notifyEvent : TNotifyEvent);
begin
	vGLBaseSceneObjectNameChangeEvent:=notifyEvent;
end;

// DeRegisterGLBaseSceneObjectNameChangeEvent
//
procedure DeRegisterGLBaseSceneObjectNameChangeEvent(notifyEvent : TNotifyEvent);
begin
	vGLBaseSceneObjectNameChangeEvent:=nil;
end;

// RegisterGLBehaviourNameChangeEvent
//
procedure RegisterGLBehaviourNameChangeEvent(notifyEvent : TNotifyEvent);
begin
   vGLBehaviourNameChangeEvent:=notifyEvent;
end;

// DeRegisterGLBehaviourNameChangeEvent
//
procedure DeRegisterGLBehaviourNameChangeEvent(notifyEvent : TNotifyEvent);
begin
   vGLBehaviourNameChangeEvent:=nil;
end;

//----------------- TGLPickList -------------------------------------------------

var
  SortFlag: TPickSortType;

constructor TGLPickList.Create(SortType: TPickSortType);

begin
  SortFlag:=SortType;
  inherited Create;
end;

//------------------------------------------------------------------------------

destructor TGLPickList.Destroy;

begin
  Clear;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

function CompareFunction(Item1, Item2: Pointer): Integer;

var
  Diff: Single;

begin
  Result:=0;
  case SortFlag of
    psName:
      Result:=CompareText(PPickRecord(Item1).AObject.Name, PPickRecord(Item2).AObject.Name);
    psMinDepth:
      begin
        Diff:=PPickRecord(Item1).ZMin - PPickRecord(Item2).ZMin;
        if Diff < 0 then Result:=-1
                    else
          if Diff > 0 then Result:=1
                      else Result:=0;
      end;
    psMaxDepth:
      begin
        Diff:=Round(PPickRecord(Item1).ZMax - PPickRecord(Item2).ZMax);
        if Diff < 0 then Result:=-1
                    else
          if Diff > 0 then Result:=1
                      else Result:=0;
      end;
  end;
end;

// AddHit
//
procedure TGLPickList.AddHit(obj : TGLBaseSceneObject; subObj : TPickSubObjects;
                             zMin, zMax : Single);
var
   newRecord : PPickRecord;
begin
   New(newRecord);
   try
      newRecord.AObject:=obj;
		newRecord.SubObjects:=subObj;
      newRecord.zMin:=zMin;
      newRecord.zMax:=zMax;
      Add(newRecord);
      if SortFlag<>psDefault then
         Sort(CompareFunction);
   except
  	   newRecord.SubObjects:=nil;
      Dispose(newRecord);
      raise;
   end;
end;

// Clear
//
procedure TGLPickList.Clear;
var
   i : Integer;
begin
   for I:=0 to Count-1 do begin
      PPickRecord(Items[I]).SubObjects:=nil;
      Dispose(PPickRecord(Items[I]));
   end;
   inherited Clear;
end;

// FindObject
//
function TGLPickList.FindObject(aObject : TGLBaseSceneObject): Integer;
var
   i : Integer;
begin
   Result:=-1;
   if Assigned(AObject) then for i:=0 to Count-1 do
      if Hit[i]=AObject then begin
         Result:=i;
         Break;
      end;
end;

// GetFar
//
function TGLPickList.GetFar(aValue : Integer): Single;
begin
   Result:=PPickRecord(Items[AValue]).ZMax;
end;

// GetHit
//
function TGLPickList.GetHit(aValue : Integer) : TGLBaseSceneObject;
begin
  Result:=PPickRecord(Items[AValue]).AObject;
end;

// GetNear
//
function TGLPickList.GetNear(aValue : Integer): Single;
begin
   Result:=PPickRecord(Items[AValue]).ZMin;
end;

// GetSubobjects
//
function TGLPickList.GetSubObjects(aValue : Integer) : TPickSubobjects;
begin
   Result:=PPickRecord(Items[AValue]).SubObjects;
end;

// ------------------
// ------------------ TGLBaseSceneObject ------------------
// ------------------

// Create
//
constructor TGLBaseSceneObject.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FObjectStyle:=[];
	FChanges:=[ocTransformation, ocStructure];
	FPosition:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
   FRotation:=TGLCoordinates.CreateInitialized(Self, NullHmgPoint, csVector);
	FDirection:=TGLCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
	FUp:=TGLCoordinates.CreateInitialized(Self, YHmgVector, csVector);
	FScaling:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
	FAbsoluteMatrix:=IdentityHmgMatrix;
	FInvAbsoluteMatrix:=IdentityHmgMatrix;
	FLocalMatrix:=IdentityHmgMatrix;
	FChildren:=TList.Create;
	FVisible:=True;
	FLocalMatrixDirty:=True;
   FAbsoluteMatrixDirty:=True;
   FInvAbsoluteMatrixDirty:=True;
	FObjectsSorting:=osInherited;
	FBehaviours:=TGLBehaviours.Create(Self);
   FObjectEffects:=TGLObjectEffects.Create(Self);
end;

// Destroy
//
destructor TGLBaseSceneObject.Destroy;
begin
   DeleteChildCameras;
   FObjectEffects.Free;
   FBehaviours.Free;
   if Assigned(FScene) then FreeList(False);
   FPosition.Free;
   FRotation.Free;
   FDirection.Free;
   FUp.Free;
   FScaling.Free;
   if Assigned(FParent) then FParent.Remove(Self, False);
   if FChildren.Count>0 then DeleteChildren;
   FChildren.Free;
   inherited Destroy;
end;

// GetHandle
//
function TGLBaseSceneObject.GetHandle(var rci : TRenderContextInfo) : TObjectHandle;
begin
   if ocStructure in FChanges then begin
      if FHandle = 0 then begin
         FHandle:=glGenLists(1);
         Assert(FHandle<>0, 'Handle=0 for '+ClassName);
      end;
      glNewList(FHandle, GL_COMPILE);
      try
         BuildList(rci);
      finally
         glEndList;
      end;
   end;
   Result:=FHandle;
end;

// FreeList
//
procedure TGLBaseSceneObject.FreeList(glsceneOnly : Boolean);
var
   i : Integer;
   forceAContext : Boolean;
begin
   forceAContext:=(not glsceneOnly) and ((CurrentRenderingContextDC=0) and Assigned(FScene));
   if forceAContext then
      forceAContext:=FScene.ActivateDefaultRenderingContext;
   try
      Include(FChanges, ocStructure);
      for i:=0 to FChildren.Count-1 do
         TGLBaseSceneObject(FChildren[i]).FreeList(glsceneOnly);
      DoDestroyList(glsceneOnly);
   finally
      if forceAContext then
         DeactivateRenderingContext;
   end;
end;

// DoDestroyList
//
procedure TGLBaseSceneObject.DoDestroyList(glsceneOnly : Boolean);
begin
   if FHandle > 0 then begin
      if not glsceneOnly then begin
         Assert(CurrentRenderingContextDC>0);
         glGetError;
         glDeleteLists(FHandle, 1);
         CheckOpenGLError;
      end;
      FHandle:=0;
   end;
end;

// BeginUpdate
//
procedure TGLBaseSceneObject.BeginUpdate;
begin
   Inc(FUpdateCount);
end;

// BuildList
//
procedure TGLBaseSceneObject.BuildList(var rci : TRenderContextInfo);
begin
   // nothing
end;

// DeleteChildCameras
//
procedure TGLBaseSceneObject.DeleteChildCameras;
var
   i : Integer;
   child : TGLBaseSceneObject;
begin
   i:=0;
   while i<FChildren.Count do begin
      child:=TGLBaseSceneObject(FChildren.Items[i]);
      child.DeleteChildCameras;
      if child is TGLCamera then begin
         Remove(child, True);
         child.Free;
      end else Inc(i);
   end;
end;

// DeleteChildren
//
procedure TGLBaseSceneObject.DeleteChildren;
var
	child : TGLBaseSceneObject;
begin
   DeleteChildCameras;
 	while FChildren.Count>0 do begin
     	child:=TGLBaseSceneObject(FChildren.Items[FChildren.Count-1]);
	   child.FParent:=nil;
   	FChildren.Delete(FChildren.Count-1);
	   child.Free;
   end;
end;

// Loaded
//
procedure TGLBaseSceneObject.Loaded;
begin
   inherited;
   FPosition.W:=1;
	FBehaviours.Loaded;
   FObjectEffects.Loaded;
end;

// DefineProperties
//
procedure TGLBaseSceneObject.DefineProperties(Filer: TFiler);
begin
	inherited;
	Filer.DefineBinaryProperty('BehavioursData',
                              ReadBehaviours, WriteBehaviours, 
								      (FBehaviours.Count>0));
	Filer.DefineBinaryProperty('EffectsData',
                              ReadEffects, WriteEffects,
								      (FObjectEffects.Count>0));
end;

// WriteBehaviours
//
procedure TGLBaseSceneObject.WriteBehaviours(stream : TStream);
var
   writer : TWriter;
begin
   writer:=TWriter.Create(stream, 16384);
   try
      FBehaviours.WriteToFiler(writer);
   finally
      writer.Free;
   end;
end;

// ReadBehaviours
//
procedure TGLBaseSceneObject.ReadBehaviours(stream : TStream);
var
   reader : TReader;
begin
   reader:=TReader.Create(stream, 16384);
   try
      FBehaviours.ReadFromFiler(reader);
   finally
      reader.Free;
   end;
end;

// WriteEffects
//
procedure TGLBaseSceneObject.WriteEffects(stream : TStream);
var
   writer : TWriter;
begin
   writer:=TWriter.Create(stream, 16384);
   try
      FObjectEffects.WriteToFiler(writer);
   finally
      writer.Free;
   end;
end;

// ReadEffects
//
procedure TGLBaseSceneObject.ReadEffects(stream : TStream);
var
   reader : TReader;
begin
   reader:=TReader.Create(stream, 16384);
   try
      FObjectEffects.ReadFromFiler(reader);
   finally
      reader.Free;
   end;
end;

// DrawAxes
//
procedure TGLBaseSceneObject.DrawAxes(Pattern: TGLushort);
begin
   AxesBuildList(Pattern, FScene.CurrentViewer.FCamera.FDepthOfView);
end;

// GetChildren
//
procedure TGLBaseSceneObject.GetChildren(AProc: TGetChildProc; Root: TComponent);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      AProc(FChildren[I]);
end;

// RebuildMatrix
//
procedure TGLBaseSceneObject.RebuildMatrix;
begin
	if ocTransformation in Changes then begin
		FLocalMatrix[0]:=VectorCrossProduct(FUp.AsVector, FDirection.AsVector);
   	ScaleVector(FLocalMatrix[0], Scale.X);
      VectorScale(FUp.AsVector, Scale.Y, FLocalMatrix[1]);
      VectorScale(FDirection.AsVector, Scale.Z, FLocalMatrix[2]);
		FLocalMatrix[3]:=FPosition.AsVector;
      FAbsoluteMatrixDirty:=True;
      FInvAbsoluteMatrixDirty:=True;
  	end;
end;

// Get
//
function TGLBaseSceneObject.Get(Index: Integer): TGLBaseSceneObject;
begin
	Result:=FChildren[Index];
end;

// GetCount
//
function TGLBaseSceneObject.GetCount: Integer;
begin
	Result:=FChildren.Count;
end;

// AddChild
//
procedure TGLBaseSceneObject.AddChild(AChild: TGLBaseSceneObject);
begin
	if Assigned(FScene) and (AChild is TGLLightSource) then
		FScene.AddLight(TGLLightSource(AChild));
	FChildren.Add(AChild);
	AChild.FParent:=Self;
	AChild.SetScene(FScene);
	TransformationChanged;
end;

// AddNewChild
//
function TGLBaseSceneObject.AddNewChild(AChild: TGLSceneObjectClass): TGLBaseSceneObject;
begin
	Result:=AChild.Create(Self);
	AddChild(Result);
end;

// AddNewChildFirst
//
function TGLBaseSceneObject.AddNewChildFirst(AChild: TGLSceneObjectClass): TGLBaseSceneObject;
begin
	Result:=AChild.Create(Self);
	Insert(0, Result);
end;

// AbsoluteMatrix
//
function TGLBaseSceneObject.AbsoluteMatrix : TMatrix;
begin
   Result:=AbsoluteMatrixAsAddress^;
end;

// AbsoluteMatrixAsAddress
//
function TGLBaseSceneObject.AbsoluteMatrixAsAddress : PMatrix;
begin
   if FAbsoluteMatrixDirty then begin
      if Assigned(Parent) and (not (Parent is TGLSceneRootObject)) then begin
         TGLBaseSceneObject(Parent).AbsoluteMatrixAsAddress;
         MatrixMultiply(FLocalMatrix, TGLBaseSceneObject(Parent).FAbsoluteMatrix,
                        FAbsoluteMatrix);
      end else FAbsoluteMatrix:=FLocalMatrix;
      FAbsoluteMatrixDirty:=False;
      FInvAbsoluteMatrixDirty:=True;
   end;
   Result:=@FAbsoluteMatrix;
end;

// InvAbsoluteMatrix
//
function TGLBaseSceneObject.InvAbsoluteMatrix : TMatrix;
begin
  if FInvAbsoluteMatrixDirty then begin
    FInvAbsoluteMatrix:=AbsoluteMatrixAsAddress^;
    InvertMatrix(FInvAbsoluteMatrix);
    FInvAbsoluteMatrixDirty:=False;
  end;
  Result:=FInvAbsoluteMatrix;
end;

// AbsoluteDirection
//
function TGLBaseSceneObject.AbsoluteDirection : TVector;
begin
   Result:=VectorTransform(Direction.AsVector, AbsoluteMatrix);
end;

// AbsolutePosition
//
function TGLBaseSceneObject.AbsolutePosition : TVector;
begin
   Result:=AbsoluteMatrix[3]
end;

// AbsoluteXVector
//
function TGLBaseSceneObject.AbsoluteXVector : TVector;
begin
   AbsoluteMatrixAsAddress;
   SetVector(Result, PAffineVector(@FAbsoluteMatrix[0])^);
end;

// AbsoluteYVector
//
function TGLBaseSceneObject.AbsoluteYVector : TVector;
begin
   AbsoluteMatrixAsAddress;
   SetVector(Result, PAffineVector(@FAbsoluteMatrix[1])^);
end;

// AbsoluteZVector
//
function TGLBaseSceneObject.AbsoluteZVector : TVector;
begin
   AbsoluteMatrixAsAddress;
   SetVector(Result, PAffineVector(@FAbsoluteMatrix[2])^);
end;

// BarycenterAbsolutePosition
//
function TGLBaseSceneObject.BarycenterAbsolutePosition : TVector;
begin
	Result:=AbsolutePosition;
end;

// SqrDistanceTo
//
function TGLBaseSceneObject.SqrDistanceTo(const pt : TVector) : Single;
begin
	Result:=VectorDistance2(pt, AbsolutePosition);
end;

// BarycenterSqrDistanceTo
//
function TGLBaseSceneObject.BarycenterSqrDistanceTo(const pt : TVector) : Single;
var
	d : TVector;
begin
	d:=BarycenterAbsolutePosition;
	Result:=Sqr(d[0]-pt[0])+Sqr(d[1]-pt[1])+Sqr(d[2]-pt[2]);
end;

// AxisAlignedDimensions
//
function TGLBaseSceneObject.AxisAlignedDimensions : TVector;
begin
   VectorScale(Scale.AsVector, 0.5, Result);
end;

// BoundingSphereRadius
//
function TGLBaseSceneObject.BoundingSphereRadius : Single;
begin
   Result:=MaxXYZComponent(AxisAlignedDimensions);
end;

// PointInObject
//
function TGLBaseSceneObject.PointInObject(const point : TVector) : Boolean;
var
   localPt, dim : TVector;
begin
   dim:=AxisAlignedDimensions;
   localPt:=VectorTransform(point, InvAbsoluteMatrix);
   Result:=(Abs(localPt[0])<=dim[0]) and (Abs(localPt[1])<=dim[1])
           and (Abs(localPt[2])<=dim[2]);
end;

// RayCastIntersect
//
function TGLBaseSceneObject.RayCastIntersect(const rayStart, rayVector : TAffineVector;
                                 intersectPoint : PAffineVector = nil;
                                 intersectNormal : PAffineVector = nil) : Boolean;
var
   localStart, localVector : TAffineVector;
   m : TMatrix;
begin
   m:=InvAbsoluteMatrix;
   localStart:=VectorTransform(rayStart, m);
   localVector:=VectorTransform(rayVector, m);
   Result:=RayCastTriangleIntersect(rayStart, rayVector,
                                    NullVector,
                                    XVector,
                                    ZVector,
                                    intersectPoint,
                                    intersectNormal);
end;

// Assign
//
procedure TGLBaseSceneObject.Assign(Source: TPersistent);
var
	i : Integer;
	child, newChild : TGLBaseSceneObject;
begin
	if Source is TGLBaseSceneObject then begin
      if Assigned(FScene) then begin
         FScene.ActivateDefaultRenderingContext;
         if CurrentRenderingContextDC<>0 then begin
            FreeList(False);
            DeactivateRenderingContext;
         end;
      end else FreeList(True);
		FPosition.Assign(TGLBaseSceneObject(Source).FPosition);
      FDirection.Assign(TGLBaseSceneObject(Source).FDirection);
      FUp.Assign(TGLBaseSceneObject(Source).FUp);
      FScaling.Assign(TGLBaseSceneObject(Source).FScaling);
		FChanges:=[ocTransformation, ocStructure];
		FVisible:=TGLBaseSceneObject(Source).FVisible;
		FLocalMatrix:=TGLBaseSceneObject(Source).FLocalMatrix;
		FLocalMatrixDirty:=TGLBaseSceneObject(Source).FLocalMatrixDirty;
		FAbsoluteMatrix:=TGLBaseSceneObject(Source).FAbsoluteMatrix;
		FAbsoluteMatrixDirty:=TGLBaseSceneObject(Source).FAbsoluteMatrixDirty;
		FInvAbsoluteMatrix:=TGLBaseSceneObject(Source).FInvAbsoluteMatrix;
		FInvAbsoluteMatrixDirty:=TGLBaseSceneObject(Source).FInvAbsoluteMatrixDirty;
		SetMatrix(TGLCustomSceneObject(Source).FLocalMatrix);
		FShowAxes:=TGLBaseSceneObject(Source).FShowAxes;
		FObjectsSorting:=TGLBaseSceneObject(Source).FObjectsSorting;
      FRotation.Assign(TGLBaseSceneObject(Source).FRotation);
		FTransMode:=TGLBaseSceneObject(Source).FTransMode;
		DeleteChildren;
      SetScene(TGLBaseSceneObject(Source).FScene);
		if Assigned(Scene) then Scene.BeginUpdate;
		for i:=0 to TGLBaseSceneObject(Source).FChildren.Count-1 do begin
			child:=TGLBaseSceneObject(Source).FChildren[I];
			newChild:=AddNewChild(TGLSceneObjectClass(child.ClassType));
			newChild.Assign(child);
		end;
		if Assigned(Scene) then Scene.EndUpdate;
		OnProgress:=TGLBaseSceneObject(Source).OnProgress;
		FBehaviours.Assign(TGLBaseSceneObject(Source).FBehaviours);
      FObjectEffects.Assign(TGLBaseSceneObject(Source).FObjectEffects);
		Tag:=TGLBaseSceneObject(Source).Tag;
		FTagFloat:=TGLBaseSceneObject(Source).FTagFloat;
   end else inherited Assign(Source);
end;

// Insert
//
procedure TGLBaseSceneObject.Insert(aIndex : Integer; aChild : TGLBaseSceneObject);
begin
	with FChildren do begin
		if Assigned(aChild.FParent) then
			aChild.FParent.Remove(aChild, False);
		Insert(aIndex, aChild);
	end;
	aChild.FParent:=Self;
	if AChild.FScene<>FScene then AChild.FreeList(False);
	AChild.SetScene(FScene);
	if Assigned(FScene) and (AChild is TGLLightSource) then
		FScene.AddLight(TGLLightSource(AChild));
  	TransformationChanged;
end;

//------------------------------------------------------------------------------

function TGLBaseSceneObject.IsUpdating: Boolean;

begin
  Result:=(FUpdateCount <> 0) or (csReading in ComponentState);
end;

//------------------------------------------------------------------------------

function TGLBaseSceneObject.GetIndex: Integer;

begin
  Result:=-1;
  if assigned(FParent) then Result:=FParent.FChildren.IndexOf(Self);
end;

// GetOrientationVectors
//
procedure TGLBaseSceneObject.GetOrientationVectors(var Up, Direction: TAffineVector);
begin
   if (FTransMode<>tmLocal) and Assigned(FParent) then begin
      SetVector(Up, FParent.FUp.AsVector);
      SetVector(Direction, FParent.FDirection.AsVector);
   end else begin
      SetVector(Up, FUp.AsVector);
      SetVector(Direction, FDirection.AsVector);
   end;
end;

// GetParentComponent
//
function TGLBaseSceneObject.GetParentComponent: TComponent;
begin
   Result:=FParent;
end;

// HasParent
//
function TGLBaseSceneObject.HasParent: Boolean;
begin
   Result:=assigned(FParent);
end;

// Lift
//
procedure TGLBaseSceneObject.Lift(ADistance: Single);
var
   Up, Dir: TAffineVector;
begin
   if FTransMode = tmParentWithPos then begin
      GetOrientationVectors(Up, Dir);
      FPosition.AddScaledVector(ADistance, Up);
   end else FPosition.AddScaledVector(ADistance, FUp.AsVector);
   TransformationChanged;
end;

// Move
//
procedure TGLBaseSceneObject.Move(ADistance: Single);
var
   Up, Dir: TAffineVector;
begin
   if FTransMode = tmParentWithPos then begin
      GetOrientationVectors(Up, Dir);
      FPosition.AddScaledVector(ADistance, Dir);
   end else FPosition.AddScaledVector(ADistance, FDirection.AsVector);
   TransformationChanged;
end;

// Slide
//
procedure TGLBaseSceneObject.Slide(ADistance: Single);
var
   RightVector: TAffineVector;
   Up, Dir: TAffineVector;
begin
   if FTransMode = tmParentWithPos then
      GetOrientationVectors(Up, Dir)
   else begin
      SetVector(Up, FUp.AsVector);
      SetVector(Dir, FDirection.AsVector);
   end;
   VectorCrossProduct(Dir, Up, RightVector);
   FPosition.AddScaledVector(ADistance, RightVector);
   TransformationChanged;
end;

// Pitch
//
procedure TGLBaseSceneObject.Pitch(Angle: Single);
var
   RightVector: TAffineVector;
   Up, Dir: TAffineVector;
   r : Single;
begin
   FIsCalculating:=True;
   try
      GetOrientationVectors(Up, Dir);
      RightVector:=VectorCrossProduct(Dir, Up);
      Angle:=DegToRad(Angle);
      FUp.Rotate(RightVector, -Angle);
      FUp.Normalize;
      FDirection.Rotate(RightVector, -Angle);
      FDirection.Normalize;
      if FTransMode = tmParentWithPos then
         FPosition.Rotate(RightVector, Angle);
      r:=-RadToDeg(arctan2(FDirection.Y, Sqrt(Sqr(FDirection.X) + Sqr(FDirection.Z))));
      if FDirection.X < 0 then
         if FDirection.Y < 0 then
            r:=180-r
         else r:=-180-r;
      FRotation.X:=r;
   finally
      FIsCalculating:=False;
   end
end;

// SetPitchAngle
//
procedure TGLBaseSceneObject.SetPitchAngle(AValue: Single);
var
   RightVector: TAffineVector;
   Up, Dir: TAffineVector;
   Diff: Single;
   rotMatrix : TMatrix;
begin
   if AValue<>FRotation.X then begin
      if not (csLoading in ComponentState) then begin
         GetOrientationVectors(Up, Dir);
         Diff:=DegToRad(FRotation.X-AValue);
         RightVector:=VectorCrossProduct(Dir, Up);
         rotMatrix:=CreateRotationMatrix(RightVector, Diff);
         FUp.DirectVector:=VectorTransform(FUp.AsVector, rotMatrix);
         FUp.Normalize;
         FDirection.DirectVector:=VectorTransform(FDirection.AsVector, rotMatrix);
         FDirection.Normalize;
         if FTransMode=tmParentWithPos then
            FPosition.DirectVector:=VectorTransform(FPosition.AsVector, rotMatrix);
         TransformationChanged;
      end;
      FRotation.DirectX:=NormalizeAngle(AValue);
   end;
end;

// Roll
//
procedure TGLBaseSceneObject.Roll(Angle: Single);
var
   RightVector: TVector;
   Up, Dir: TAffineVector;
   r : Single;
begin
   GetOrientationVectors(Up, Dir);
   Angle:=DegToRad(Angle);
   FUp.Rotate(Dir, Angle);
   FUp.Normalize;
   FDirection.Rotate(Dir, Angle);
   FDirection.Normalize;
   if FTransMode = tmParentWithPos then
      FPosition.Rotate(Dir, Angle);

   // calculate new rotation angle from vectors
   RightVector:=VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
   r:=-RadToDeg(arctan2(Rightvector[1], Sqrt(Sqr(RightVector[0]) + Sqr(RightVector[2]))));
   if RightVector[0] < 0 then
      if RightVector[1] < 0 then
         r:=180-r
      else r:=-180-r;
   FRotation.Z:=r;
end;

// SetRollAngle
//
procedure TGLBaseSceneObject.SetRollAngle(AValue: Single);
var
   Up, Dir: TAffineVector;
   Diff: Single;
   rotMatrix : TMatrix;
begin
   if AValue <> FRotation.Z then begin
      if not (csLoading in ComponentState) then begin
         GetOrientationVectors(Up, Dir);
         Diff:=DegToRad(FRotation.Z - AValue);
         rotMatrix:=CreateRotationMatrix(Dir, Diff);
         FUp.DirectVector:=VectorTransform(FUp.AsVector, rotMatrix);
         FUp.Normalize;
         FDirection.DirectVector:=VectorTransform(FDirection.AsVector, rotMatrix);
         FDirection.Normalize;
         if FTransMode = tmParentWithPos then
            FPosition.DirectVector:=VectorTransform(FPosition.AsVector, rotMatrix);
         TransformationChanged;
      end;
      FRotation.DirectZ:=NormalizeAngle(AValue);
  end;
end;

// Turn
//
procedure TGLBaseSceneObject.Turn(Angle: Single);
var
   Up, Dir: TAffineVector;
   r : Single;
begin
   GetOrientationVectors(Up, Dir);
   Angle:=DegToRad(Angle);
   FUp.Rotate(Up, Angle);
   FUp.Normalize;
   FDirection.Rotate(Up, Angle);
   FDirection.Normalize;
   if FTransMode = tmParentWithPos then
      FPosition.Rotate(Up, Angle);
   r:=-RadToDeg(arctan2(FDirection.X, Sqrt(Sqr(FDirection.Y) + Sqr(FDirection.Z))));
   if FDirection.X < 0 then
      if FDirection.Y < 0 then
         r:=180-r
      else r:=-180-r;
   FRotation.Y:=r;
end;

// SetTurnAngle
//
procedure TGLBaseSceneObject.SetTurnAngle(AValue: Single);
var
   up, dir : TAffineVector;
   diff : Single;
   rotMatrix : TMatrix;
begin
   if AValue<>FRotation.Y then begin
      if not (csLoading in ComponentState) then begin
         GetOrientationVectors(Up, Dir);
         Diff:=DegToRad(FRotation.Y-AValue);
         rotMatrix:=CreateRotationMatrix(Up, Diff);
         FUp.DirectVector:=VectorTransform(FUp.AsVector, rotMatrix);
         FUp.Normalize;
         FDirection.DirectVector:=VectorTransform(FDirection.AsVector, rotMatrix);
         FDirection.Normalize;
         if FTransMode = tmParentWithPos then
            FPosition.DirectVector:=VectorTransform(FPosition.AsVector, rotMatrix);
         TransformationChanged;
      end;
      FRotation.DirectY:=NormalizeAngle(AValue);
   end;
end;

// SetRotation
//
procedure TGLBaseSceneObject.SetRotation(aRotation : TGLCoordinates);
begin
   FRotation.Assign(aRotation);
   TransformationChanged;
end;

// GetPitchAngle
//
function TGLBaseSceneObject.GetPitchAngle : Single;
begin
  Result:=FRotation.X;
end;

// GetTurnAngle
//
function TGLBaseSceneObject.GetTurnAngle : Single;
begin
  Result:=FRotation.Y;
end;

// GetRollAngle
//
function TGLBaseSceneObject.GetRollAngle : Single;
begin
  Result:=FRotation.Z;
end;

// SetShowAxes
//
procedure TGLBaseSceneObject.SetShowAxes(AValue: Boolean);
begin
   if FShowAxes <> AValue then begin
      FShowAxes:=AValue;
      NotifyChange(Self);
   end;
end;

// SetScaling
//
procedure TGLBaseSceneObject.SetScaling(AValue: TGLCoordinates);
begin
   FScaling.Assign(AValue);
   TransformationChanged;
end;

// SetName
//
procedure TGLBaseSceneObject.SetName(const NewName: TComponentName);
begin
	if Name <> NewName then begin
      inherited SetName(NewName);
		if Assigned(vGLBaseSceneObjectNameChangeEvent) then
			vGLBaseSceneObjectNameChangeEvent(Self);
	end;
end;

// SetParent
//
procedure TGLBaseSceneObject.SetParent(const val : TGLBaseSceneObject);
begin
   MoveTo(val);
end;

// SetIndex
//
procedure TGLBaseSceneObject.SetIndex(AValue: Integer);
var
   Count: Integer;
   AParent: TGLBaseSceneObject;
begin
   if Assigned(FParent) then begin
      Count:=FParent.Count;
      AParent:=FParent;
      if AValue<0 then AValue:=0;
      if AValue>=Count then AValue:=Count-1;
      if AValue<>Index then begin
         if Assigned(FScene) then FScene.BeginUpdate;
         FParent.Remove(Self, False);
         AParent.Insert(AValue, Self);
         if Assigned(FScene) then FScene.EndUpdate;
      end;
   end;
end;

// SetParentComponent
//
procedure TGLBaseSceneObject.SetParentComponent(Value: TComponent);
var
   topGuy : TComponent;
begin
	if Assigned(FParent) then begin
		FParent.Remove(Self, False);
		FParent:=nil;
	end;
   if Assigned(Value) then begin
   	// first level object?
   	if Value is TGLScene then
	   	if Self is TGLCamera then
		   	TGLScene(Value).Cameras.AddChild(Self)
   		else TGLScene(Value).Objects.AddChild(Self)
	   else begin
         if Assigned(FParent) and (FParent is TGLBaseSceneObject) then
            topGuy:=FParent
         else if Value is TGLBaseSceneObject then
            topGuy:=Value
         else topGuy:=nil;
         if Assigned(topGuy) then
            TGLBaseSceneObject(Value).AddChild(Self)
         else Assert(False);
      end;
   end;
end;

// StructureChanged
//
procedure TGLBaseSceneObject.StructureChanged;
begin
   if not (ocStructure in FChanges) then begin
      Include(FChanges, ocStructure);
      NotifyChange(Self);
   end;
end;

// TransformationChanged
//
procedure TGLBaseSceneObject.TransformationChanged;
begin
   FLocalMatrixDirty:=True;
   FAbsoluteMatrixDirty:=True;
   if not (csLoading in ComponentState) then
      if not (ocTransformation in FChanges) then begin
         Include(FChanges, ocTransformation);
         NotifyChange(Self);
      end;
end;

// MoveTo
//
procedure TGLBaseSceneObject.MoveTo(newParent : TGLBaseSceneObject);
begin
	if Assigned(FParent) then begin
		FParent.Remove(Self, False);
		FParent:=nil;
	end;
	if Assigned(newParent) then
		newParent.AddChild(Self)
	else SetScene(nil);
end;

// MoveUp
//
procedure TGLBaseSceneObject.MoveUp;
begin
   if Assigned(parent) then
      parent.MoveChildUp(parent.IndexOfChild(Self));
end;

// MoveDown
//
procedure TGLBaseSceneObject.MoveDown;
begin
   if Assigned(parent) then
      parent.MoveChildDown(parent.IndexOfChild(Self));
end;

// EndUpdate
//
procedure TGLBaseSceneObject.EndUpdate;
begin
   if FUpdateCount>0 then begin
      Dec(FUpdateCount);
      if FUpdateCount=0 then NotifyChange(Self);
   end else Assert(False, glsUnBalancedBeginEndUpdate);
end;

// CoordinateChanged
//
procedure TGLBaseSceneObject.CoordinateChanged(Sender: TGLCoordinates);
var
   rightVector : TVector;
begin
  if FIsCalculating then Exit;
  FIsCalculating:=True;
  try
      if Sender = FDirection then begin
         if FDirection.VectorLength = 0 then
            FDirection.DirectVector:=ZHmgVector;
         FDirection.Normalize;
         // adjust up vector
         rightVector:=VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
         // Rightvector is zero if direction changed exactly by 90 degrees,
         // in this case assume a default vector
         if VectorLength(rightVector) = 0 then with FDirection do
            Geometry.SetVector(rightVector, X+1, Y+2, Z+3);
         FUp.DirectVector:=VectorCrossProduct(RightVector, FDirection.AsVector);
         FUp.Normalize;
      end else if Sender = FUp then begin
         if FUp.VectorLength = 0 then
            FUp.DirectVector:=YHmgVector;
         FUp.Normalize;
         // adjust up vector
         rightVector:=VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
         // Rightvector is zero if direction changed exactly by 90 degrees,
         // in this case assume a default vector
         if VectorLength(rightVector) = 0 then with FUp do
            Geometry.SetVector(rightVector, X+1, Y+2, Z+3);
         FDirection.DirectVector:=VectorCrossProduct(FUp.AsVector, RightVector);
         FDirection.Normalize;
      end;
      TransformationChanged;
   finally
      FIsCalculating:=False;
   end;
end;

// DoProgress
//
procedure TGLBaseSceneObject.DoProgress(const deltaTime, newTime : Double);
var
   i : Integer;
begin
	for i:=FChildren.Count-1 downto 0 do
		TGLBaseSceneObject(FChildren[i]).DoProgress(deltaTime, newTime);
   if Behaviours.Count>0 then
   	Behaviours.DoProgress(deltaTime, newTime);
   if Effects.Count>0 then
      Effects.DoProgress(deltaTime, newTime);
   if Assigned(FOnProgress) then
		FOnProgress(Self, deltaTime, newTime);
end;

// Remove
//
procedure TGLBaseSceneObject.Remove(AChild: TGLBaseSceneObject; KeepChildren: Boolean);
begin
	if Assigned(FScene) and (AChild is TGLLightSource) then
		FScene.RemoveLight(TGLLightSource(AChild));
	FChildren.Remove(AChild);
	AChild.FParent:=nil;
	if KeepChildren then begin
		BeginUpdate;
		with AChild do while Count>0 do
			Children[0].MoveTo(Self);
		EndUpdate;
	end else NotifyChange(Self);
end;

// IndexOfChild
//
function TGLBaseSceneObject.IndexOfChild(AChild: TGLBaseSceneObject) : Integer;
begin
	Result:=FChildren.IndexOf(AChild);
end;

// FindChild
//
function TGLBaseSceneObject.FindChild(const aName : String;
                                      ownChildrenOnly : Boolean) : TGLBaseSceneObject;
var
   i : integer;
   res: TGLBaseSceneObject;
begin
   res:=nil;
   Result:=nil;
   for i:=0 to FChildren.Count-1 do begin
      if CompareText(TGLBaseSceneObject(FChildren[i]).Name, aName)=0 then begin
         res:=TGLBaseSceneObject(FChildren[i]);
         Break;
      end;
   end;
   if not ownChildrenOnly then begin
      for i:=0 to FChildren.Count-1 do with TGLBaseSceneObject(FChildren[i]) do begin
         Result:=FindChild(aName, ownChildrenOnly);
         if Assigned(Result) then Break;
      end;
   end;
   if not Assigned(Result) then
     Result:=res;
end;

// MoveChildUp
//
procedure TGLBaseSceneObject.MoveChildUp(anIndex : Integer);
begin
   if anIndex>0 then begin
      FChildren.Exchange(anIndex, anIndex-1);
      NotifyChange(Self);
   end;
end;

// MoveChildDown
//
procedure TGLBaseSceneObject.MoveChildDown(anIndex : Integer);
begin
   if anIndex<FChildren.Count-1 then begin
      FChildren.Exchange(anIndex, anIndex+1);
      NotifyChange(Self);
   end;
end;

// PrepareObject
//
procedure TGLBaseSceneObject.PrepareObject(var rci : TRenderContextInfo);
begin
   glPushMatrix;
   glMultMatrixf(@FLocalMatrix);
	if FScene.FCurrentViewer.State=dsPicking then
      if rci.proxySubObject then
         glPushName(Integer(Self))
      else glLoadName(Integer(Self));
end;

// FinishObject
//
procedure TGLBaseSceneObject.FinishObject(var rci : TRenderContextInfo);
begin
   glPopMatrix;
end;

// Render
//
procedure TGLBaseSceneObject.Render(var rci : TRenderContextInfo);
begin
	if FVisible then begin
   	PrepareObject(rci);
      if FShowAxes then
         DrawAxes($CCCC);
      if Effects.Count>0 then begin
         glPushMatrix;
         Effects.RenderPreEffects(Scene.CurrentViewer, rci);
         glPopMatrix;
         glPushMatrix;
         DoRender(rci);
         Effects.RenderPostEffects(Scene.CurrentViewer, rci);
         glPopMatrix;
      end else DoRender(rci);
      if osDoesTemperWithColorsOrFaceWinding in ObjectStyle then begin
         ResetGLPolygonMode;
         ResetGLMaterialColors;
      end;
      FinishObject(rci);
      Exclude(FChanges, ocStructure);
   end;
end;

// DoRender
//
procedure TGLBaseSceneObject.DoRender(var rci : TRenderContextInfo);
begin
   if osDirectDraw in ObjectStyle then
      BuildList(rci)
   else glCallList(GetHandle(rci));
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// RenderChildren
//
procedure TGLBaseSceneObject.RenderChildren(firstChildIndex, lastChildIndex : Integer;
                                            var rci : TRenderContextInfo);
var
	i : Integer;
	distList : TSingleList;
   objList : TList;
	obj : TGLBaseSceneObject;
   oldSorting : TGLObjectsSorting;
begin
   if lastChildIndex=firstChildIndex then
      Get(firstChildIndex).Render(rci)
   else if lastChildIndex>firstChildIndex then begin
      oldSorting:=rci.objectsSorting;
	   if Self.ObjectsSorting<>osInherited then
		   rci.objectsSorting:=Self.ObjectsSorting;
	   case rci.objectsSorting of
         osNone : begin
            for i:=firstChildIndex to lastChildIndex do
               Self[i].Render(rci);
         end;
         osRenderFarthestFirst, osRenderBlendedLast : begin
            distList:=TSingleList.Create;
            objList:=TList.Create;
            try
               if objectsSorting=osRenderBlendedLast then begin
                  // render opaque stuff
                  for i:=firstChildIndex to lastChildIndex do begin
                     obj:=Get(i);
                     if (not (obj is TGLCustomSceneObject)) or (TGLCustomSceneObject(obj).Material.BlendingMode=bmOpaque) then
                        obj.Render(rci)
                     else if obj.Visible then begin
                        objList.Add(obj);
                        distList.Add(obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                     end;
                  end;
               end else for i:=firstChildIndex to lastChildIndex do begin
                  obj:=Get(i);
                  if obj.Visible then begin
                     objList.Add(obj);
                     distList.Add(obj.BarycenterSqrDistanceTo(rci.cameraPosition));
                  end;
               end;
               if distList.Count>1 then
                  QuickSortLists(0, distList.Count-1, distList, objList);
               for i:=objList.Count-1 downto 0 do
                  TGLBaseSceneObject(objList[i]).Render(rci);
            finally
               distList.Free;
               objList.Free;
            end;
         end;
      else
         Assert(False);
      end;
      rci.objectsSorting:=oldSorting;
   end;
end;

// NotifyChange
//
procedure TGLBaseSceneObject.NotifyChange(Sender : TObject);
begin
   if Assigned(FScene) and (not IsUpdating) then
      FScene.NotifyChange(Self);
end;

// ValidateTransformation
//
procedure TGLBaseSceneObject.ValidateTransformation;
var
   i : Integer;
begin
   // determine predecessor in transformation pipeline
   if not Assigned(FParent) then begin
      Include(FChanges, ocTransformation);
      if ocTransformation in Changes then begin
         RebuildMatrix;
         FAbsoluteMatrixDirty:=True;
      end;
   end else begin
      if ocTransformation in Changes then begin
         RebuildMatrix;
         FAbsoluteMatrixDirty:=True;
      end else FAbsoluteMatrixDirty:=FParent.FAbsoluteMatrixDirty;
   end;
   // validate for children
   for i:=0 to Count-1 do
      Self[i].ValidateTransformation;
   // all done
   Exclude(FChanges, ocTransformation);
end;

// GetMatrix
//
function TGLBaseSceneObject.GetMatrix : TMatrix;
begin
   RebuildMatrix;
   Result:=FLocalMatrix;
end;

// SetMatrix
//
procedure TGLBaseSceneObject.SetMatrix(AValue: TMatrix);
var
   Temp: TAffineVector;
begin
   FLocalMatrix:=AValue;
   FDirection.DirectVector:=FLocalMatrix[2];
   FUp.DirectVector:=FLocalMatrix[1];
   SetVector(Temp, FLocalMatrix[0]);
   Scale.SetVector(VectorLength(Temp), FUp.VectorLength, FDirection.VectorLength, 0);
   FPosition.DirectVector:=FLocalMatrix[3];
   TransformationChanged;
end;

procedure TGLBaseSceneObject.SetPosition(APosition: TGLCoordinates);
begin
   FPosition.SetPoint(APosition.X, APosition.Y, APosition.Z);
end;

procedure TGLBaseSceneObject.SetDirection(AVector: TGLCoordinates);
begin
   if not VectorIsNull(AVector.DirectVector) then begin
      FDirection.SetVector(AVector.X, AVector.Y, AVector.Z);
      TransformationChanged;
   end;
end;

procedure TGLBaseSceneObject.SetUp(AVector: TGLCoordinates);
begin
   if not VectorIsNull(AVector.DirectVector) then begin
      FUp.SetVector(AVector.X, AVector.Y, AVector.Z, 0);
      TransformationChanged;
   end;
end;

procedure TGLBaseSceneObject.SetVisible(AValue: Boolean);
begin
   if FVisible <> AValue then begin
      FVisible:=AValue;
      NotifyChange(Self);
   end;
end;

// SetObjectsSorting
//
procedure TGLBaseSceneObject.SetObjectsSorting(const val : TGLObjectsSorting);
begin
	if FObjectsSorting<>val then begin
		FObjectsSorting:=val;
		NotifyChange(Self);
	end;
end;

// SetBehaviours
//
procedure TGLBaseSceneObject.SetBehaviours(const val : TGLBehaviours);
begin
	FBehaviours.Assign(val);
end;

// SetEffects
//
procedure TGLBaseSceneObject.SetEffects(const val : TGLObjectEffects);
begin
   FObjectEffects.Assign(val);
end;

// SetScene
//
procedure TGLBaseSceneObject.SetScene(const value : TGLScene);
var
  i : Integer;
begin
	if value<>FScene then begin
      // must be freed, the new scene may be using a non-compatible RC
      if Assigned(FScene) then
         FreeList(False);
		FScene:=value;
      // propagate for childs
      for i:=0 to FChildren.Count-1 do
		   Children[I].SetScene(FScene);
	end;
end;

// Translate
//
procedure TGLBaseSceneObject.Translate(tx, ty, tz : TGLFloat);
begin
	FPosition.Translate(AffineVectorMake(tx, ty, tz));
end;

// ------------------
// ------------------ TGLBaseBehaviour ------------------
// ------------------

// Create
//
constructor TGLBaseBehaviour.Create(aOwner : TXCollection);
begin
	inherited Create(aOwner);
   // nothing more, yet
end;

// Destroy
//
destructor TGLBaseBehaviour.Destroy;
begin
   // nothing more, yet
	inherited Destroy;
end;

// SetName
//
procedure TGLBaseBehaviour.SetName(const val : String);
begin
   inherited SetName(val);
   if Assigned(vGLBehaviourNameChangeEvent) then
      vGLBehaviourNameChangeEvent(Self);
end;

// WriteToFiler
//
procedure TGLBaseBehaviour.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing more, yet
   end;
end;

// ReadFromFiler
//
procedure TGLBaseBehaviour.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      Assert(ReadInteger=0);
      // nothing more, yet
   end;
end;

// OwnerBaseSceneObject
//
function TGLBaseBehaviour.OwnerBaseSceneObject : TGLBaseSceneObject;
begin
	Result:=TGLBaseSceneObject(Owner.Owner);
end;

// DoProgress
//
procedure TGLBaseBehaviour.DoProgress(const deltaTime, newTime : Double);
begin
	// does nothing
end;

// ------------------
// ------------------ TGLBehaviours ------------------
// ------------------

// Create
//
constructor TGLBehaviours.Create(aOwner : TPersistent);
begin
   Assert(aOwner is TGLBaseSceneObject);
	inherited Create(aOwner);
end;

// ItemsClass
//
class function TGLBehaviours.ItemsClass : TXCollectionItemClass;
begin
   Result:=TGLBehaviour;
end;

// GetBehaviour
//
function TGLBehaviours.GetBehaviour(index : Integer) : TGLBehaviour;
begin
	Result:=TGLBehaviour(Items[index]);
end;

// CanAdd
//
function TGLBehaviours.CanAdd(aClass : TXCollectionItemClass) : Boolean;
begin
   Result:=(not aClass.InheritsFrom(TGLObjectEffect)) and (inherited CanAdd(aClass));
end;

// DoProgress
//
procedure TGLBehaviours.DoProgress(const deltaTime, newTime : Double);
var
	i : Integer;
begin
	for i:=0 to Count-1 do
		TGLBehaviour(Items[i]).DoProgress(deltaTime, newTime);
end;

//------------------ TGLObjectEffect -------------------------------------------

// WriteToFiler
//
procedure TGLObjectEffect.WriteToFiler(writer : TWriter);
begin
   with writer do begin
      WriteInteger(0); // Archive Version 0
      // nothing more, yet
   end;
end;

// ReadFromFiler
//
procedure TGLObjectEffect.ReadFromFiler(reader : TReader);
begin
   with reader do begin
      Assert(ReadInteger=0);
      // nothing more, yet
   end;
end;

// Render
//
procedure TGLObjectEffect.Render(sceneViewer : TGLSceneViewer;
                                 var rci : TRenderContextInfo);
begin
   // nothing here, this implem is just to avoid "abstract error"
end;

//------------------ TGLObjectEffects ------------------------------------------

// Create
//
constructor TGLObjectEffects.Create(aOwner : TPersistent);
begin
   Assert(aOwner is TGLBaseSceneObject);
	inherited Create(aOwner);
end;

// ItemsClass
//
class function TGLObjectEffects.ItemsClass : TXCollectionItemClass;
begin
   Result:=TGLObjectEffect;
end;

// GetEffect
//
function TGLObjectEffects.GetEffect(index : Integer) : TGLObjectEffect;
begin
	Result:=TGLObjectEffect(Items[index]);
end;

// CanAdd
//
function TGLObjectEffects.CanAdd(aClass : TXCollectionItemClass) : Boolean;
begin
   Result:=(aClass.InheritsFrom(TGLObjectEffect)) and (inherited CanAdd(aClass));
end;

// DoProgress
//
procedure TGLObjectEffects.DoProgress(const deltaTime, newTime : Double);
var
	i : Integer;
begin
	for i:=0 to Count-1 do
		TGLBehaviour(Items[i]).DoProgress(deltaTime, newTime);
end;

// RenderPreEffects
//
procedure TGLObjectEffects.RenderPreEffects(sceneViewer : TGLSceneViewer;
                                            var rci : TRenderContextInfo);
var
	i : Integer;
   effect : TGLObjectEffect;
begin
	for i:=0 to Count-1 do begin
      effect:=TGLObjectEffect(Items[i]);
      if effect is TGLObjectPreEffect then
   		effect.Render(sceneViewer, rci);
   end;
end;

// RenderPostEffects
//
procedure TGLObjectEffects.RenderPostEffects(sceneViewer : TGLSceneViewer;
                                             var rci : TRenderContextInfo);
var
	i : Integer;
   effect : TGLObjectEffect;
begin
	for i:=0 to Count-1 do begin
      effect:=TGLObjectEffect(Items[i]);
      if effect is TGLObjectPostEffect then
   		effect.Render(sceneViewer, rci)
      else if Assigned(sceneViewer) and (effect is TGLObjectAfterEffect) then
         sceneViewer.afterRenderEffects.Add(effect);
   end;
end;

//------------------ TGLCustomSceneObject --------------------------------------

// Create
//
constructor TGLCustomSceneObject.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FHandle:=0;
   FMaterial:=TGLMaterial.Create(Self);
end;

// Destroy
//
destructor TGLCustomSceneObject.Destroy;
begin
   inherited Destroy;
   FMaterial.Free;
end;

// Assign
//
procedure TGLCustomSceneObject.Assign(Source: TPersistent);
begin
	if Source is TGLCustomSceneObject then begin
		FMaterial.Assign(TGLCustomSceneObject(Source).FMaterial);
      FHint:=TGLCustomSceneObject(Source).FHint;
	end;
	inherited Assign(Source);
end;

// SetGLMaterial
//
procedure TGLCustomSceneObject.SetGLMaterial(AValue: TGLMaterial);
begin
   FMaterial.Assign(AValue);
   NotifyChange(Self);
end;

// DoDestroyList
//
procedure TGLCustomSceneObject.DoDestroyList(glsceneOnly : Boolean);
begin
   FMaterial.DestroyHandle(glsceneOnly);
   inherited;
end;

// DoRender
//
procedure TGLCustomSceneObject.DoRender(var rci : TRenderContextInfo);
begin
  	FMaterial.Apply(rci);
   if osDirectDraw in ObjectStyle then
      BuildList(rci)
   else glCallList(GetHandle(rci));
   FMaterial.UnApply(rci);
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// ------------------
// ------------------ TGLSceneRootObject ------------------
// ------------------

// Create
//
constructor TGLSceneRootObject.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

//----------------- TGLCamera --------------------------------------------------

// Create
//
constructor TGLCamera.Create(aOwner : TComponent);
begin
   inherited Create(aOwner);
   FFocalLength:=50;
   FDepthOfView:=100;
   FDirection.Initialize(VectorMake(0, 0, -1, 0));
   FCameraStyle:=csPerspective;
end;

// destroy
//
destructor TGLCamera.Destroy;
begin
   inherited;
end;

procedure TGLCamera.Apply;
var
	v, d : TVector;
	absPos : TVector;
   mat : TMatrix;
begin
	if Assigned(FTargetObject) then begin
      // get our target's absolute coordinates
      v:=TargetObject.AbsolutePosition;
   	absPos:=AbsolutePosition;
      VectorSubtract(v, absPos, d);
      NormalizeVector(d);
		FLastDirection:=d;
		gluLookAt(absPos[0], absPos[1], absPos[2],
					 v[0], v[1], v[2],
					 Up.X, Up.Y, Up.Z);
	end else begin
      mat:=Parent.AbsoluteMatrix;
      absPos:=AbsolutePosition;
      v:=VectorTransform(Direction.AsVector, mat);
      FLastDirection:=v;
      d:=VectorTransform(Up.AsVector, mat);
      gluLookAt(absPos[0], absPos[1], absPos[2],
                absPos[0]+v[0],
                absPos[1]+v[1],
                absPos[2]+v[2],
                d[0], d[1], d[2]);
	end;
   Exclude(FChanges, ocStructure);
end;

procedure TGLCamera.ApplyPerspective(Viewport: TRectangle; Width, Height: Integer; DPI: Integer);
var
	Left, Right, Top, Bottom, zFar, MaxDim, Ratio, f: Double;
begin
   // determine biggest dimension and resolution (height or width)
   MaxDim:=Width;
   if Height > MaxDim then
      MaxDim:=Height;

   // calculate near plane distance and extensions;
   // Scene ratio is determined by the window ratio. The viewport is just a
   // specific part of the entire window and has therefore no influence on the
   // scene ratio. What we need to know, though, is the ratio between the window
   // borders (left, top, right and bottom) and the viewport borders.
   // Note: viewport.top is actually bottom, because the window (and viewport) origin
   // in OGL is the lower left corner

   if CameraStyle=csPerspective then
      f:=1/Width
   else f:=100/(focalLength*Width);

   // calculate window/viewport ratio for right extent
   Ratio:=(2 * Viewport.Width + 2 * Viewport.Left - Width) * f;
   // calculate aspect ratio correct right value of the view frustum and take
   // the window/viewport ratio also into account
   Right:=Ratio * Width / (2 * MaxDim);

   // the same goes here for the other three extents
   // left extent:
   Ratio:=(Width - 2 * Viewport.Left) * f;
   Left:=-Ratio * Width / (2 * MaxDim);

   if CameraStyle=csPerspective then
      f:=1/Height
   else f:=100/(focalLength*Height);

   // top extent (keep in mind the origin is left lower corner):
   Ratio:=(2 * Viewport.Height + 2 * Viewport.Top - Height) * f;
   Top:=Ratio * Height / (2 * MaxDim);

   // bottom extent:
   Ratio:=(Height - 2 * Viewport.Top) * f;
   Bottom:=-Ratio * Height / (2 * MaxDim);

   FNearPlane:=FFocalLength * 2 * DPI / (25.4 * MaxDim);
   zFar:=FNearPlane + FDepthOfView;

   // finally create view frustum (perspective or orthogonal)
   case CameraStyle of
      csPerspective :
         glFrustum(Left, Right, Bottom, Top, FNearPlane, zFar);
      csOrthogonal :
         glOrtho(Left, Right, Bottom, Top, FNearPlane, zFar);
   else
      Assert(False);
   end;

   FViewPortRadius:=VectorLength(Right, Top)/FNearPlane;
end;

//------------------------------------------------------------------------------

procedure TGLCamera.AutoLeveling(Factor: Single);
var
  rightVector, rotAxis: TVector;
  angle: Single;
begin
   angle:=RadToDeg(arccos(VectorDotProduct(FUp.AsVector, YVector)));
   rotAxis:=VectorCrossProduct(YHmgVector, FUp.AsVector);
   if (angle > 1) and (VectorLength(rotAxis) > 0) then begin
      rightVector:=VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      FUp.Rotate(AffineVectorMake(rotAxis), Angle / (10*Factor));
      FUp.Normalize;
      // adjust local coordinates
      FDirection.DirectVector:=VectorCrossProduct(FUp.AsVector, rightVector);
      FRotation.Z:=-RadToDeg(arctan2(Rightvector[1], Sqrt(Sqr(RightVector[0])+Sqr(RightVector[2]))));
   end;
end;

procedure TGLCamera.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if (Operation = opRemove) and (AComponent = FTargetObject) then
      TargetObject:=nil;
end;

// SetTargetObject
//
procedure TGLCamera.SetTargetObject(const val : TGLBaseSceneObject);
begin
   if (FTargetObject<>val) then begin
      FTargetObject:=val;
      if not (csLoading in Scene.ComponentState) then
         TransformationChanged;
   end;
end;

procedure TGLCamera.Reset;
var
   Extent: Single;
begin
	FRotation.Z:=0;
   FFocalLength:=50;
   with FScene.CurrentViewer do begin
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity;
      ApplyPerspective(FViewport, Width, Height, GetDeviceCaps(Canvas.Handle, LOGPIXELSX));
      FUp.DirectVector:=YHmgVector;
      if FViewport.Height<FViewport.Width then
         Extent:=FViewport.Height * 0.25
      else Extent:=FViewport.Width * 0.25;
	end;
   FPosition.SetVector(0, 0, FNearPlane * Extent, 1);
   FDirection.SetVector(0, 0, -1, 0);
   TransformationChanged;
end;

// ZoomAll
//
procedure TGLCamera.ZoomAll;
var
	extent: Single;
begin
	with Scene.CurrentViewer do begin
      if FViewport.Height<FViewport.Width then
         Extent:=FViewport.Height * 0.25
      else Extent:=FViewport.Width * 0.25;
		FPosition.DirectVector:=NullHmgPoint;
		Move(-FNearPlane * Extent);
		// let the camera look at the scene center
		FDirection.SetVector(-FPosition.X, -FPosition.Y, -FPosition.Z, 0);
  	end;
end;

// MoveAroundTarget
//
procedure TGLCamera.MoveAroundTarget(pitchDelta, turnDelta : Single);
var
   originalT2C, normalT2C, normalCameraRight : TVector;
   pitchNow, dist: Single;
begin
   if Assigned(FTargetObject) then begin
      // normalT2C points away from the direction the camera is looking
      originalT2C:=VectorSubtract(AbsolutePosition,
                                  TargetObject.AbsolutePosition);
      SetVector(normalT2C, originalT2C);
      dist:=VectorLength(normalT2C);
      NormalizeVector(normalT2C);
      // normalRight points to the camera's right
      // the camera is pitching around this axis.
      normalCameraRight:=VectorCrossProduct(Up.AsVector, normalT2C);
      NormalizeVector(normalCameraRight);
      // calculate the current pitch.
      // 0 is looking down and PI is looking up
      pitchNow:=ArcCos(VectorDotProduct(Up.AsVector, normalT2C));
      pitchNow:=ClampValue(pitchNow+DegToRad(pitchDelta), 0+0.001, PI-0.001);
      // create a new vector pointing up and then rotate it down
      // into the new position
      SetVector(normalT2C, Up.AsVector);
      RotateVector(normalT2C, normalCameraRight, -pitchNow);
      RotateVector(normalT2C, Up.AsVector, -DegToRad(turnDelta));
      ScaleVector(normalT2C, dist);
      Position.AsVector:=VectorAdd(Position.AsVector,
                                   VectorSubtract(normalT2C, originalT2C));
   end;
end;

// AdjustDistanceToTarget
//
procedure TGLCamera.AdjustDistanceToTarget(distanceRatio : Single);
var
	vect : TVector;
begin
	if Assigned(FTargetObject) then begin
		// calculate vector from target to camera in absolute coordinates
		vect:=VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
		// ratio -> translation vector
		ScaleVector(vect, -(1-distanceRatio));
		Position.Translate(vect);
	end;
end;

// DistanceToTarget
//
function TGLCamera.DistanceToTarget : Single;
var
	vect : TVector;
begin
	if Assigned(FTargetObject) then begin
		vect:=VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
		Result:=VectorLength(vect);
	end else Result:=1;
end;

// ScreenDeltaToVector
//
function TGLCamera.ScreenDeltaToVector(deltaX, deltaY : Integer; ratio : Single;
												 const planeNormal : TVector) : TVector;
var
	screenY, screenX : TVector;
	screenYoutOfPlaneComponent : Single;
begin
	// calculate projection of direction vector on the plane
	if Assigned(FTargetObject) then
		screenY:=VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
	else screenY:=Direction.AsVector;
	screenYoutOfPlaneComponent:=VectorDotProduct(screenY, planeNormal);
	screenY:=VectorCombine(screenY, planeNormal, 1, -screenYoutOfPlaneComponent);
	NormalizeVector(screenY);
	// calc the screenX vector
	screenX:=VectorCrossProduct(screenY, planeNormal);
	// and here, we're done
	Result:=VectorCombine(screenX, screenY, deltaX*ratio, deltaY*ratio);
end;

// ScreenDeltaToVectorXY
//
function TGLCamera.ScreenDeltaToVectorXY(deltaX, deltaY : Integer; ratio : Single) : TVector;
var
	screenY : TVector;
	dxr, dyr, d : Single;
begin
	// calculate projection of direction vector on the plane
	if Assigned(FTargetObject) then
		screenY:=VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
	else screenY:=Direction.AsVector;
	d:=VectorLength(screenY[0], screenY[1]);
	if d<=1e-10 then d:=ratio else d:=ratio/d;
	// and here, we're done
	dxr:=deltaX*d;
	dyr:=deltaY*d;
	Result[0]:=screenY[1]*dxr+screenY[0]*dyr;
	Result[1]:=screenY[1]*dyr-screenY[0]*dxr;
	Result[2]:=0;
	Result[3]:=0;
end;

// ScreenDeltaToVectorXZ
//
function TGLCamera.ScreenDeltaToVectorXZ(deltaX, deltaY : Integer; ratio : Single) : TVector;
var
   screenY : TVector;
   d, dxr, dzr : Single;
begin
   // calculate the projection of direction vector on the plane
   if Assigned(fTargetObject) then
      screenY:=VectorSubtract(TargetObject.AbsolutePosition, AbsolutePosition)
   else screenY:=Direction.AsVector;
	d:=VectorLength(screenY[0], screenY[2]);
   if d<=1e-10 then d:=ratio else d:=ratio/d;
   dxr:=deltaX*d;
   dzr:=deltaY*d;
   Result[0]:=-screenY[2]*dxr+screenY[0]*dzr;
   Result[1]:=0;
   Result[2]:=screenY[2]*dzr+screenY[0]*dxr;
   Result[3]:=0;
end;

// ScreenDeltaToVectorYZ
//
function TGLCamera.ScreenDeltaToVectorYZ(deltaX, deltaY : Integer; ratio : Single) : TVector;
var
   screenY : TVector;
   d, dyr, dzr : single;
begin
   // calculate the projection of direction vector on the plane
   if Assigned(fTargetObject) then
      screenY:=VectorSubtract(TargetObject.AbsolutePosition,AbsolutePosition)
   else screenY:=Direction.AsVector;
   d:=VectorLength(screenY[1], screenY[2]);
   if d<=1e-10 then d:=ratio else d:=ratio/d;
   dyr:=deltaX*d;
   dzr:=deltaY*d;
   Result[0]:=0;
   Result[1]:=screenY[2]*dyr+screenY[1]*dzr;
   Result[2]:=screenY[2]*dzr-screenY[1]*dyr;
   Result[3]:=0;
end;

// SetDepthOfView
//
procedure TGLCamera.SetDepthOfView(AValue: Single);
begin
  if FDepthOfView <> AValue then
  begin
	 FDepthOfView:=AValue;
	 if not (csLoading in Scene.ComponentState) then
      TransformationChanged;
  end;
end;

// SetFocalLength
//
procedure TGLCamera.SetFocalLength(AValue: Single);
begin
   if AValue < 1 then AValue:=1;
   if FFocalLength <> AValue  then begin
	   FFocalLength:=AValue;
      if not (csLoading in Scene.ComponentState) then
         TransformationChanged;
   end;
end;

// SetCameraStyle
//
procedure TGLCamera.SetCameraStyle(const val : TGLCameraStyle);
begin
   if FCameraStyle<>val then begin
      FCameraStyle:=val;
      NotifyChange(Self);
   end;
end;

// DoRender
//
procedure TGLCamera.DoRender(var rci : TRenderContextInfo);
begin
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// DoDestroyList
//
procedure TGLCamera.DoDestroyList(glsceneOnly : Boolean);
begin
   // nothing
end;

// ------------------
// ------------------ TDirectOpenGL ------------------
// ------------------

// Create
//
constructor TDirectOpenGL.Create(AOwner: TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw, osDoesTemperWithColorsOrFaceWinding];
end;

// Assign
//
procedure TDirectOpenGL.Assign(Source: TPersistent);
begin
   if Source is TDirectOpenGL then begin
      UseBuildList:=TDirectOpenGL(Source).UseBuildList;
   end;
   inherited Assign(Source);
end;

// BuildList
//
procedure TDirectOpenGL.BuildList(var rci : TRenderContextInfo);
begin
   if Assigned(FOnRender) then
      OnRender(rci);
end;

// SetUseBuildList
//
procedure TDirectOpenGL.SetUseBuildList(const val : Boolean);
begin
   if val<>FUseBuildList then begin
      FUseBuildList:=val;
      if val then
         ObjectStyle:=ObjectStyle-[osDirectDraw]
      else ObjectStyle:=ObjectStyle+[osDirectDraw];
   end;
end;

// ------------------
// ------------------ TGLProxyObject ------------------
// ------------------

// Create
//
constructor TGLProxyObject.Create(AOwner: TComponent);
begin
   inherited;
   FProxyOptions:=cDefaultProxyOptions;
end;

// Destroy
//
destructor TGLProxyObject.Destroy;
begin
   SetMasterObject(nil);
   inherited;
end;

// Assign
//
procedure TGLProxyObject.Assign(Source: TPersistent);
begin
   if Source is TGLProxyObject then begin
      SetMasterObject(TGLProxyObject(Source).MasterObject);
   end;
   inherited Assign(Source);
end;

// Render
//
procedure TGLProxyObject.DoRender(var rci : TRenderContextInfo);
var
   gotMaster, masterGotEffects, oldProxySubObject : Boolean;
begin
   gotMaster:=Assigned(FMasterObject);
   masterGotEffects:=gotMaster and (pooEffects in FProxyOptions)
                     and (FMasterObject.Effects.Count>0);
   if gotMaster then begin
      if pooObjects in FProxyOptions then begin
         oldProxySubObject:=rci.proxySubObject;
         rci.proxySubObject:=True;
         FMasterObject.DoRender(rci);
         rci.proxySubObject:=oldProxySubObject;
      end;
   end;
   // now render self stuff (our children, our effects, etc.)
   if Count>0 then
      RenderChildren(0, Count-1, rci);
   if masterGotEffects then
      FMasterObject.Effects.RenderPostEffects(Scene.CurrentViewer, rci);
end;

// Notification
//
procedure TGLProxyObject.Notification(AComponent: TComponent; Operation: TOperation);
begin
	if (Operation = opRemove) and (AComponent = FMasterObject) then
{$ifdef DFS_COMPILER_4}
		FMasterObject:=nil;
		StructureChanged;
{$else}
		MasterObject:=nil;
{$endif}
   inherited;
end;

// SetMasterObject
//
procedure TGLProxyObject.SetMasterObject(const val : TGLBaseSceneObject);
begin
   if FMasterObject<>val then begin
		if Assigned(FMasterObject) then
			FMasterObject.RemoveFreeNotification(Self);
		FMasterObject:=val;
		if Assigned(FMasterObject) then
         FMasterObject.FreeNotification(Self);
      StructureChanged;
   end;
end;

// SetProxyOptions
//
procedure TGLProxyObject.SetProxyOptions(const val : TGLProxyObjectOptions);
begin
   if FProxyOptions<>val then begin
      FProxyOptions:=val;
      StructureChanged;
   end;
end;

// ------------------
// ------------------ TGLLightSource ------------------
// ------------------

// Create
//
constructor TGLLightSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShining:=True;
  FSpotDirection:=TGLCoordinates.CreateInitialized(Self, VectorMake(0, 0, -1, 0),
                                                   csVector);
  FConstAttenuation:=1;
  FLinearAttenuation:=0;
  FQuadraticAttenuation:=0;
  FSpotCutOff:=180;
  FSpotExponent:=0;
  FLightStyle:=lsSpot;
  FAmbient:=TGLColor.Create(Self);
  FDiffuse:=TGLColor.Create(Self);
  FDiffuse.Initialize(clrWhite);
  FSpecular:=TGLColor.Create(Self);
  FVisible:=False;
  FChanges:=[];
end;

// Destroy
//
destructor TGLLightSource.Destroy;
begin
   FSpotDirection.Free;
   FAmbient.Free;
   FDiffuse.Free;
   FSpecular.Free;
   inherited Destroy;
end;

// DoRender
//
procedure TGLLightSource.DoRender(var rci : TRenderContextInfo);
begin
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// DoDestroyList
//
procedure TGLLightSource.DoDestroyList;
begin
   if FShining and IsOpenGLInitialized then
      glDisable(FLightID);
end;

// CoordinateChanged
//
procedure TGLLightSource.CoordinateChanged(Sender: TGLCoordinates);
begin
   inherited;
   if Sender = FSpotDirection then
      Include(FChanges, ocSpot);
   TransformationChanged;
end;

// GetHandle
//
function TGLLightSource.GetHandle(var rci : TRenderContextInfo) : TObjectHandle;
begin
   Result:=0;
end;

// SetShining
//
procedure TGLLightSource.SetShining(AValue: Boolean);
begin
   if AValue<>FShining then begin
      FShining:=AValue;
      NotifyChange(Self);
   end;
end;

// SetSpotDirection
//
procedure TGLLightSource.SetSpotDirection(AVector: TGLCoordinates);
begin
   FSpotDirection.DirectVector:=AVector.AsVector;
   FSpotDirection.W:=0;
   Include(FChanges, ocSpot);
   NotifyChange(Self);
end;

// SetSpotExponent
//
procedure TGLLightSource.SetSpotExponent(AValue: TGLFloat);
begin
   if FSpotExponent <> AValue then begin
      FSpotExponent:=AValue;
      Include(FChanges, ocSpot);
      NotifyChange(Self);
   end;
end;

// SetSpotCutOff
//
procedure TGLLightSource.SetSpotCutOff(AValue: TGLFloat);
begin
   if FSpotCutOff <> AValue then begin
      FSpotCutOff:=AValue;
      Include(FChanges, ocSpot);
      NotifyChange(Self);
   end;
end;

// SetLightStyle
//
procedure TGLLightSource.SetLightStyle(const val : TLightStyle);
begin
   if FLightStyle<>val then begin
      FLightStyle:=val;
      Include(FChanges, ocSpot);
      NotifyChange(Self);
   end;
end;

// SetAmbient
//
procedure TGLLightSource.SetAmbient(AValue: TGLColor);
begin
   FAmbient.Color:=AValue.Color;
   NotifyChange(Self);
end;

// SetDiffuse
//
procedure TGLLightSource.SetDiffuse(AValue: TGLColor);
begin
   FDiffuse.Color:=AValue.Color;
   NotifyChange(Self);
end;

// SetSpecular
//
procedure TGLLightSource.SetSpecular(AValue: TGLColor);
begin
   FSpecular.Color:=AValue.Color;
   NotifyChange(Self);
end;

// SetConstAttenuation
//
procedure TGLLightSource.SetConstAttenuation(AValue: TGLFloat);
begin
   if FConstAttenuation <> AValue then begin
      FConstAttenuation:=AValue;
      Include(FChanges, ocAttenuation);
      NotifyChange(Self);
   end;
end;

// SetLinearAttenuation
//
procedure TGLLightSource.SetLinearAttenuation(AValue: TGLFloat);
begin
   if FLinearAttenuation <> AValue then begin
      FLinearAttenuation:=AValue;
      Include(FChanges, ocAttenuation);
      NotifyChange(Self);
   end;
end;

// SetQuadraticAttenuation
//
procedure TGLLightSource.SetQuadraticAttenuation(AValue: TGLFloat);
begin
   if FQuadraticAttenuation <> AValue then begin
      FQuadraticAttenuation:=AValue;
      Include(FChanges, ocAttenuation);
      NotifyChange(Self);
   end;
end;

//------------------------------------------------------------------------------

{procedure TGLLightSource.RenderLensFlares(from, at: TAffineVector; near_clip: TGLFloat);
const
  global_scale = 0.5;
  MinDot = 1e-20;
var
   view_dir, tmp, light_dir, pos, LightPos : TAffineVector;
   dx, dy, center, axis, sx, sy: TAffineVector;
   dot: Extended;
   I: Integer;
   NewFrom, NewAt: TAffineVector;
   LightColor: TAffineVector;
begin
   // determine current light position
   LightPos:=MakeAffineVector([FGLobalMatrix[3, 0], FGLobalMatrix[3, 1], FGLobalMatrix[3, 2]]);
   // take out camera influence
   Newat:=VectorAffineSubtract(at, from);
   Newfrom:=NullVector;

   // view_dir = normalize(at-from)
   view_dir:=VectorAffineSubtract(Newat, NewFrom);
   VectorNormalize(view_dir);

   // center = from + near_clip * view_dir
   tmp:=view_dir;
   VectorScale(tmp, near_clip);
   center:=VectorAffineAdd(Newfrom, tmp);

   // light_dir = normalize(light-from)
   light_dir:=VectorAffineSubtract(LightPos, Newfrom);
   VectorNormalize(light_dir);

   // light = from + dot(light, view_dir) * near_clip * light_dir
   dot:=VectorAffineDotProduct(light_dir, view_dir);
   tmp:=light_dir;
   if Abs(Dot) < MinDot then
      if Dot < 0 then Dot:=-MinDot else Dot:=MinDot;
	VectorScale(tmp, near_clip / dot);
	LightPos:=VectorAffineAdd(Newfrom, tmp);

	// axis = light - center
	axis:=VectorAffineSubtract(LightPos, center);

	// dx = normalize(axis)
	dx:=axis;
	VectorNormalize(dx);

	// dy = cross(dx, view_dir)
	dy:=VectorCrossProduct(dx, view_dir);

	//glPushAttrib(GL_ENABLE_BIT or GL_CURRENT_BIT or GL_LIGHTING_BIT or GL_LINE_BIT
	//  or GL_COLOR_BUFFER_BIT or GL_TEXTURE_BIT);
	glPushAttrib(GL_ALL_ATTRIB_BITS);
	Scene.CurrentViewer.UnnecessaryState([stDepthTest, stLighting]);
	Scene.CurrentViewer.RequestedState([stBlend, sTGLTexture2D]);
	glBlendFunc(GL_ONE, GL_ONE);
	glLoadIdentity;

	for I:=0 to LensFlares.Count - 1 do with LensFlares do begin
		sx:=dx;
		VectorScale(sx, Flare[I].Scale * global_scale*10);
		sy:=dy;
		VectorScale(sy, Flare[I].Scale * global_scale*10);
		//sx:=MakeAffineVector(1, 0, 0);
		//sy:=MakeAffineVector(0, 1, 0);

      //glColor3fv(LensFlares.Flare[I].ColorAddr);
      LightColor:=MakeAffineVector(Diffuse.Color);
      LightColor:=VectorAffineAdd(LightColor, LensFlares.Flare[I].Color);;
      VectorScale(LightColor, 0.5);
      glColor3fv(@LightColor);
       glEnable(GL_TEXTURE_2D);;
      if Flare[I].FlareType < 0 then begin
         glBindTexture(GL_TEXTURE_2D, ShineTexture[FlareTic]);
         FlareTic:=(FlareTic + 1) mod 10;
      end else glBindTexture(GL_TEXTURE_2D, FlareTexture[Flare[I].FlareType]);

      // position = center + flare[i].loc * axis
      tmp:=axis;
      VectorScale(tmp, Flare[I].Location);
      Pos:=VectorAffineAdd(center, tmp);

      glBegin(GL_QUADS);
        xglTexCoord2f(0, 0);
        tmp:=VectorAffineCombine3(Pos, sx, sy, 1, -1, -1);
        glVertex3fv(@tmp);

        xglTexCoord2f(128, 0);
        tmp:=VectorAffineCombine3(Pos, sx, sy, 1, 1, -1);
        glVertex3fv(@tmp);

        xglTexCoord2f(128, 128);
        tmp:=VectorAffineCombine3(Pos, sx, sy, 1, 1, 1);
        glVertex3fv(@tmp);

        xglTexCoord2f(0, 128);
        tmp:=VectorAffineCombine3(Pos, sx, sy, 1, -1, 1);
        glVertex3fv(@tmp);
      glEnd;
   end;
   Scene.CurrentViewer.RequestedState([stDepthTest, stLighting]);
   Scene.CurrentViewer.UnnecessaryState([stBlend, sTGLTexture2D]);
   glPopAttrib;
end;
}

// ------------------
// ------------------ TGLScene ------------------
// ------------------

// Create
//
constructor TGLScene.Create(AOwner: TComponent);
begin
  inherited;
  // root creation
  FObjects:=TGLSceneRootObject.Create(Self);
  FObjects.Name:='ObjectRoot';
  FObjects.FScene:=Self;
  FCameras:=TGLBaseSceneObject.Create(Self);
  FCameras.Name:='CameraRoot';
  FCameras.FScene:=Self;
  FLights:=TList.Create;
  FObjectsSorting:=osRenderBlendedLast;
  // actual maximum number of lights is stored in TGLSceneViewer
  FLights.Count:=8;
end;

// Destroy
//
destructor TGLScene.Destroy;
begin
   FObjects.FreeList(False);
	FCameras.Free;
	FLights.Free;
	FObjects.Free;
	inherited Destroy;
end;

{$ifndef DFS_DELPHI_5_UP}
// Notification
//
procedure TGLScene.Notification(AComponent: TComponent; Operation: TOperation);
begin
	// nothing more, here, this is just a workaround the lack of a decent
	// 'RemoveFreeNotification' under Delphi 4
	inherited Notification(AComponent, Operation);
end;
{$endif}

// AddLight
//
procedure TGLScene.AddLight(ALight: TGLLightSource);
var
	i : Integer;
begin
   for i:=0 to FLights.Count - 1 do
		if FLights[i] = nil then begin
			FLights[i]:=ALight;
         ALight.FLightID:=GL_LIGHT0 + i;
         Break;
      end;
end;

// ShutdownAllLights
//
procedure TGLScene.ShutdownAllLights;

   procedure DoShutdownLight(Obj: TGLBaseSceneObject);
   var
      i : integer;
   begin
      if Obj is TGLLightSource then
         TGLLightSource(Obj).Shining:=False;
      for i:=0 to Obj.Count-1 do
         DoShutDownLight(Obj[I]);
   end;

begin
   DoShutdownLight(FObjects);
end;

// AddViewer
//
procedure TGLScene.AddViewer(AViewer: TGLSceneViewer);
begin
   if not Assigned(FViewers) then
      FViewers:=TList.Create;
   if FViewers.IndexOf(AViewer)<0 then begin
      FViewers.Add(AViewer);
      if FBaseContext=0 then
         FBaseContext:=TGLSceneViewer(FViewers[0]).RenderingContext;
      if FViewers.Count>1 then
         OpenGL12.wglShareLists(FBaseContext, AViewer.RenderingContext);
   end;
end;

// RemoveViewer
//
procedure TGLScene.RemoveViewer(AViewer: TGLSceneViewer);
var
   i : Integer;
begin
   if Assigned(FViewers) then begin
      i:=FViewers.IndexOf(AViewer);
      if i>=0 then begin
         if FViewers.Count=1 then begin
            // if AViewer is the last one in the list then remove other
            // shared stuff before (!) the viewer is deleted
            FObjects.FreeList(True);
            FViewers.Free;
            FViewers:=nil;
            FBaseContext:=0;
         end else begin
            FViewers.Delete(i);
            FBaseContext:=TGLSceneViewer(FViewers[0]).RenderingContext;
         end;
      end;
   end;
end;

// GetChildren
//
procedure TGLScene.GetChildren(AProc: TGetChildProc; Root: TComponent);
begin
   FObjects.GetChildren(AProc, Root);
   FCameras.GetChildren(AProc, Root);
end;

//------------------------------------------------------------------------------

procedure TGLScene.RemoveLight(ALight: TGLLightSource);

var LIndex: Integer;

begin
  LIndex:=FLights.IndexOf(ALight);
  if LIndex > -1 then FLights[LIndex]:=nil;
end;

//------------------------------------------------------------------------------

procedure TGLScene.SetChildOrder(AChild: TComponent; Order: Integer);

begin
  (AChild as TGLBaseSceneObject).Index:=Order;
end;

//------------------------------------------------------------------------------

procedure TGLScene.Loaded;

begin
  inherited Loaded;
end;

// IsUpdating
//
function TGLScene.IsUpdating: Boolean;
begin
  Result:=(FUpdateCount <> 0) or (csLoading in ComponentState) or (csDestroying in ComponentState);
end;

// BeginUpdate
//
procedure TGLScene.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

// EndUpdate
//
procedure TGLScene.EndUpdate;
begin
   Assert(FUpdateCount>0);
   Dec(FUpdateCount);
   if FUpdateCount = 0 then NotifyChange(Self);
end;

// SetObjectsSorting
//
procedure TGLScene.SetObjectsSorting(const val : TGLObjectsSorting);
begin
   if FObjectsSorting<>val then begin
      FObjectsSorting:=val;
      NotifyChange(Self);
   end;
end;

// RenderScene
//
procedure TGLScene.RenderScene(aViewer: TGLSceneViewer;
                               const viewPortSizeX, viewPortSizeY : Integer);
var
   i : Integer;
   rci : TRenderContextInfo;
begin
   ResetGLPolygonMode;
   ResetGLMaterialColors;
   AViewer.afterRenderEffects.Clear;
   FCurrentViewer:=AViewer;
   rci.objectsSorting:=FObjectsSorting;
   with AViewer.Camera do begin
      rci.cameraPosition:=aViewer.FCameraAbsolutePosition;
      rci.cameraDirection:=FLastDirection;
      NormalizeVector(rci.cameraDirection);
      rci.cameraDirection[3]:=0;
      with rci.rcci do begin
         origin:=rci.cameraPosition;
         clippingDirection:=rci.cameraDirection;
         viewPortRadius:=FViewPortRadius;
         farClippingDistance:=FNearPlane+FDepthOfView;
      end;
   end;
   rci.viewPortSize.cx:=viewPortSizeX;
   rci.viewPortSize.cy:=viewPortSizeY;
   rci.currentStates:=AViewer.FCurrentStates;
   rci.materialLibrary:=nil;
   rci.fogDisabledCounter:=0;
   rci.proxySubObject:=False;
   FObjects.Render(rci);
   with AViewer.afterRenderEffects do if Count>0 then
      for i:=0 to Count-1 do
         TGLObjectAfterEffect(Items[i]).Render(AViewer, rci);
   AViewer.FCurrentStates:=rci.currentStates;
end;

// ValidateTransformation
//
procedure TGLScene.ValidateTransformation(ACamera: TGLCamera);
begin
   FCurrentGLCamera:=ACamera;
   FCameras.ValidateTransformation;
   FObjects.ValidateTransformation;
   ACamera.Apply;
   FLastGLCamera:=FCurrentGLCamera;
end;

// ActivateDefaultRenderingContext
//
function TGLScene.ActivateDefaultRenderingContext : Boolean;
var
   hdc : Integer;
begin
   Result:=(Assigned(FViewers) and (FViewers.Count>0));
   if Result and (TGLSceneViewer(FViewers[0]).FCanvas<>nil) then begin
      hdc:=TGLSceneViewer(FViewers[0]).FCanvas.Handle;
      ActivateRenderingContext(hdc, FBaseContext);
   end;
end;

// Progress
//
procedure TGLScene.Progress(const deltaTime, newTime : Double);
begin
   FObjects.DoProgress(deltaTime, newTime);
   FCameras.DoProgress(deltaTime, newTime);
end;

// SaveToFile
//
procedure TGLScene.SaveToFile(const fileName : String);
var
   list : TList;
begin
   list:=TList.Create;
   WriteComponentResFile(fileName, Self);
   list.free;
end;

// LoadFromFile
//
procedure TGLScene.LoadFromFile(const fileName : String);
var
   i : integer;
   backupName : string;
begin
   ShutdownAllLights;
   if Assigned(FViewers) then
      for i:=FViewers.Count-1 downto 0 do
         TGLSceneViewer(FViewers[i]).Camera:=nil;
   Cameras.DeleteChildren;
   Objects.DeleteChildren;
   { TODO : Camera & Targets are lost, some persistence upgraded is needed here }
   backupName:=Name;
   Name:='';
   ReadComponentResFile(fileName, Self);
   Name:=backupName;
end;

// SaveToTextFile
//
procedure TGLScene.SaveToTextFile(const fileName : String);
var
   fd : TextFile;

   // TODO : this func will have to disappear or get standardized
   function ComponentToString(Component: TComponent): string;
   var
     BinStream: TMemoryStream;
     StrStream: TStringStream;
     s:         string;
   begin
     BinStream := TMemoryStream.Create;
     try
       StrStream := TStringStream.Create(s);
       try
         BinStream.WriteComponent(Component);
         BinStream.Seek(0, soFromBeginning);
         ObjectBinaryToText(BinStream, StrStream);
         StrStream.Seek(0, soFromBeginning);
         Result := StrStream.DataString;
       finally
         StrStream.Free;
       end;
     finally
       BinStream.Free;
     end;
   end;

begin
   // Initialize the target file
   AssignFile(fd, FileName);
   ReWrite(fd);
   //Save the scene
   Write(fd, ComponentToString(Self));
   CloseFile(fd);
end;

// LoadFromTextFile
//
procedure TGLScene.LoadFromTextFile(const fileName: string);
var
   fileStream : TFileStream;
   memoryStream : TMemoryStream;
   i : Integer;
   backupName : String;
begin
   ShutdownAllLights;
   if Assigned(FViewers) then
      for i:=FViewers.Count-1 downto 0 do
         TGLSceneViewer(FViewers[i]).Camera:=nil;
   Cameras.DeleteChildren;
   Objects.DeleteChildren;
   backupName:=Name;
   Name:='';
   fileStream:=TFileStream.Create(fileName, fmOpenRead + fmShareExclusive);
   memoryStream:=TMemoryStream.Create;
   try
      ObjectTextToBinary(FileStream, MemoryStream);
      memoryStream.Seek(0, soFromBeginning);
      memoryStream.ReadComponent(Self);
   finally
      fileStream.Free;
      memoryStream.Free;
   end;
   Name:=backupName;
end;

// FindSceneObject
//
function TGLScene.FindSceneObject(const name : String) : TGLBaseSceneObject;
begin
   Result:=FObjects.FindChild(name, False);
   if not Assigned(Result) then
       Result:=FCameras.FindChild(name, False);
end;

// NotifyChange
//
procedure TGLScene.NotifyChange(Sender : TObject);
var
   i : Integer;
begin
   if (not IsUpdating) and assigned(FViewers) then
      for i:=0 to FViewers.Count-1 do
         TGLSceneViewer(FViewers[i]).Invalidate;
end;

// SetupLights
//
procedure TGLScene.SetupLights(Maximum: Integer);
var
   I: Integer;
   LS: TGLLightSource;
   Max: Integer;
begin
   glPushMatrix;
   // start searching through all light sources
   if Maximum < FLights.Count then
      Max:=Maximum
   else Max:=FLights.Count;
   for I:=0 to Max - 1 do begin
      LS:=TGLLightSource(FLights[I]);
		if Assigned(LS) then with LS do begin
         if Shining then begin
            glEnable(FLightID);
            glPopMatrix;
            glPushMatrix;
            if LightStyle=lsParallel then begin
               glMultMatrixf(PGLFloat(AbsoluteMatrixAsAddress));
               glLightfv(FLightID, GL_POSITION, SpotDirection.AsAddress)
            end else begin
               glMultMatrixf(PGLFloat(Parent.AbsoluteMatrixAsAddress));
               glLightfv(FLightID, GL_POSITION, Position.AsAddress);
            end;
            with FAmbient  do glLightfv(FLightID, GL_AMBIENT, AsAddress);
            with FDiffuse  do glLightfv(FLightID, GL_DIFFUSE, AsAddress);
            with FSpecular do glLightfv(FLightID, GL_SPECULAR, AsAddress);
            case LightStyle of
               lsSpot : begin
                  if FSpotCutOff<>180 then begin
                     glLightfv(FLightID, GL_SPOT_DIRECTION, FSpotDirection.AsAddress);
                     glLightfv(FLightID, GL_SPOT_EXPONENT, @FSpotExponent);
                  end;
                  glLightfv(FLightID, GL_SPOT_CUTOFF, @FSpotCutOff);
               end;
               lsOmni :
                  glLightf(FLightID, GL_SPOT_CUTOFF, 180);
            end;
            Exclude(FChanges, ocSpot);
            glLightfv(FLightID, GL_CONSTANT_ATTENUATION, @FConstAttenuation);
            glLightfv(FLightID, GL_LINEAR_ATTENUATION, @FLinearAttenuation);
            glLightfv(FLightID, GL_QUADRATIC_ATTENUATION, @FQuadraticAttenuation);
            Exclude(FChanges, ocAttenuation);
         end else glDisable(FLightID);
      end else glDisable(GL_LIGHT0+I);
   end;
   glPopMatrix;
end;

//------------------------------------------------------------------------------

procedure TGLScene.DoAfterRender;
{var
   i : Integer;
   light : TGLLightSource;}
begin
{   for I:=0 to FLights.Count-1 do begin
      light:=TGLLightSource(FLights[I]);
      if Assigned(light) and light.Shining then
         light.RenderLensFlares(MakeAffineVector(CurrenTGLCamera.Position.FCoords),
                                MakeAffineVector(CurrenTGLCamera.FDirection.FCoords),
                                CurrentViewer.FCamera.FNearPlane);
   end;}
end;

//------------------ TGLFogEnvironment ------------------------------------------------

// Note: The fog implementation is not conformal with the rest of the scene management
//       because it is viewer bound not scene bound.

constructor TGLFogEnvironment.Create(Owner : TPersistent);
begin
   inherited;
   FSceneViewer:=(Owner as TGLSceneViewer);
   FFogColor:=TGLColor.Create(Self);
	FFogColor.Initialize(clrBlack);
   FFogMode:=fmLinear;
   FFogStart:=10;
   FFogEnd:=1000;
   FFogDistance:=fdDefault;
end;

//------------------------------------------------------------------------------

destructor TGLFogEnvironment.Destroy;

begin
  FFogColor.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
//
procedure TGLFogEnvironment.NotifyChange(Sender : TObject);
begin
   FChanged:=True;
   FSceneViewer.Invalidate;
end;

procedure TGLFogEnvironment.SetFogColor(Value: TGLColor);
begin
   if Assigned(Value) then begin
      FFogColor.Assign(Value);
      NotifyChange(Self);
   end;
end;

// SetFogStart
//
procedure TGLFogEnvironment.SetFogStart(Value: TGLfloat);
begin
   if Value <> FFogStart then begin
      FFogStart:=Value;
      NotifyChange(Self);
   end;
end;

// SetFogEnd
//
procedure TGLFogEnvironment.SetFogEnd(Value: TGLfloat);
begin
   if Value <> FFogEnd then begin
      FFogEnd:=Value;
      NotifyChange(Self);
   end;
end;

// Assign
//
procedure TGLFogEnvironment.Assign(Source: TPersistent);
begin
   if Source is TGLFogEnvironment then begin
      FFogColor.Assign(TGLFogEnvironment(Source).FFogColor);
      FFogStart:=TGLFogEnvironment(Source).FFogStart;
      FFogEnd:=TGLFogEnvironment(Source).FFogEnd;
      FFogMode:=TGLFogEnvironment(Source).FFogMode;
      FFogDistance:=TGLFogEnvironment(Source).FFogDistance;
      FChanged:=True;
   end else inherited Assign(Source);
end;

// SetFogMode
//
procedure TGLFogEnvironment.SetFogMode(Value: TFogMode);
begin
   if Value <> FFogMode then begin
      FFogMode:=Value;
      NotifyChange(Self);
   end;
end;

// SetFogDistance
//
procedure TGLFogEnvironment.SetFogDistance(const val : TFogDistance);
begin
   if val<>FFogDistance then begin
      FFogDistance:=val;
      NotifyChange(Self);
   end;
end;


// ApplyFog
//
var
   vImplemDependantFogDistanceDefault : Integer = -1;
procedure TGLFogEnvironment.ApplyFog;
begin
   if FChanged then with FSceneViewer do begin
      if FRenderingContext > 0 then begin
         ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
         try
            case FFogMode of
               fmLinear : glFogi(GL_FOG_MODE, GL_LINEAR);
               fmExp : begin
                  glFogi(GL_FOG_MODE, GL_EXP);
                  glFogf(GL_FOG_DENSITY, FFogColor.Alpha);
               end;
               fmExp2 : begin
                  glFogi(GL_FOG_MODE, GL_EXP2);
                  glFogf(GL_FOG_DENSITY, FFogColor.Alpha);
               end;
            end;
            glFogfv(GL_FOG_COLOR, FFogColor.AsAddress);
            glFogf(GL_FOG_START, FFogStart);
            glFogf(GL_FOG_END, FFogEnd);
            if GL_NV_fog_distance then begin
               case FogDistance of
                  fdDefault : begin
                     if vImplemDependantFogDistanceDefault=-1 then
                        glGetIntegerv(GL_FOG_DISTANCE_MODE_NV, @vImplemDependantFogDistanceDefault)
                     else glFogi(GL_FOG_DISTANCE_MODE_NV, vImplemDependantFogDistanceDefault);
                  end;
                  fdEyePlane :
                     glFogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_PLANE_ABSOLUTE_NV);
                  fdEyeRadial :
                     glFogi(GL_FOG_DISTANCE_MODE_NV, GL_EYE_RADIAL_NV);
               else
                  Assert(False);
               end;
            end;
         finally
            DeactivateRenderingContext;
            FChanged:=False;
         end;
      end;
   end;
end;

//------------------ TGLSceneViewer --------------------------------------------

// Create
//
constructor TGLSceneViewer.Create(AOwner: TComponent);
begin
   FIsOpenGLAvailable:=InitOpenGL;
   xglMapTexCoordToMain;
   inherited Create(AOwner);
   ControlStyle:=[csClickEvents, csDoubleClicks, csOpaque, csCaptureMouse];
   if csDesigning in ComponentState then ControlStyle:=ControlStyle + [csFramed];
   FCanvas:=TCanvas.Create;
   Width:=100;
   Height:=100;
   FDisplayOptions:=TDisplayOptions.Create;
   FBackground:=TGLTexture.Create(nil);

   // initialize private state variables
   FFogEnvironment:=TGLFogEnvironment.Create(Self);
   FBackgroundColor:=clBtnFace;
   FDepthTest:=True;
   FFaceCulling:=True;
   FLighting:=True;
   FFogEnable:=False;
   afterRenderEffects:=TList.Create;

   FContextOptions:=[roDoubleBuffer, roRenderToWindow];

   // performance check off
   FMonitor:=False;
   ResetPerformanceMonitor;
   FState:=dsNone;
end;

// Destroy
//
destructor TGLSceneViewer.Destroy;
begin
   // clean up and terminate
   if Assigned(FCamera) and Assigned(FCamera.FScene) then begin
      FCamera.FScene.RemoveViewer(Self);
      FCamera:=nil;
   end;
   DestroyRC;
   afterRenderEffects.Free;
   FBackground.Free;
   FDisplayOptions.Free;
   FFogEnvironment.free;
   //FLandScapeOption.free;
   FCanvas.Free;
   inherited Destroy;
end;

// CreateParams
//
procedure TGLSceneViewer.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   with Params do begin
      if (not (csDesigning in ComponentState) and (woDesktop in FDisplayOptions.WindowAttributes))
            or (not assigned(Parent) and (ParentWindow = 0)) then begin
         WndParent:=0;
         Style:=WS_POPUP or WS_VISIBLE;
       end;
      Style:=Style or WS_CLIPCHILDREN or WS_CLIPSIBLINGS;
//      if woTransparent in FDisplayOptions.WindowAttributes then
//         ExStyle:=ExStyle or WS_EX_TRANSPARENT;
      WindowClass.Style:=CS_VREDRAW or CS_HREDRAW;
   end;
end;

// CreateWnd
//
procedure TGLSceneViewer.CreateWnd;
var
   BackColor: TColorVector;
   Options: TRCOptions;
   stencilBits : Integer;
begin
	inherited CreateWnd;
   FCanvas.Handle:=GetDC(Handle);
   if IsOpenGLAvailable then begin
      // initialize and activate the OpenGL rendering context
      // need to do this only once per window creation as we have a private DC
      FState:=dsRendering;
      try
         Options:=[];
         if roDoubleBuffer in ContextOptions then Include(Options, opDoubleBuffered);
         if StencilTest then
            stencilBits:=8
         else stencilBits:=0;
         // will be freed in DestroyWindowHandle
         FRenderingContext:=CreateRenderingContext(Canvas.Handle, Options,
                                                   32, stencilBits, 0, 0, 0);
         Assert(FRenderingContext<>0);
         ActivateRenderingContext(Canvas.Handle, FRenderingContext);
         try
            // this one should NOT be replaced with an assert
            if not GL_VERSION_1_1 then
               raise EOpenGLError.Create(glsWrongVersion);
            if StencilTest then
               FBuffers:=[buColor, buDepth, buStencil]
            else FBuffers:=[buColor, buDepth];
            // define viewport, this is necessary because the first WM_SIZE message
            // is posted before the rendering context has been created
            with FViewPort do begin
               Left:=0;
               Top:=0;
               Width:=Self.Width;
               Height:=Self.Height;
               glViewport(0, 0, Width, Height);
            end;
            // set up initial context states
            ReadContextProperties;
            SetupRenderingContext;
            BackColor:=ConvertWinColor(FBackgroundColor);
            glClearColor(BackColor[0], BackColor[1], BackColor[2], BackColor[3]);
         finally
            DeactivateRenderingContext;
            if woStayOnTop in DisplayOptions.WindowAttributes then
               SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0,
                            SWP_NOCOPYBITS or SWP_NOMOVE or SWP_NOSIZE);
         end;
      finally
         FState:=dsNone;
      end;
   end;
   invalidated:=False;
end;

// DestroyRC
//
procedure TGLSceneViewer.DestroyRC;
begin
   if FRenderingContext<>0 then begin
   	FCanvas.Handle:=0;
      DestroyRenderingContext(FRenderingContext);
      FRenderingContext:=0;
   end;
end;

// DestroyWindowHandle
//
procedure TGLSceneViewer.DestroyWindowHandle;
begin
   inherited;
   DestroyRC;
end;

// WMEraseBkgnd
//
procedure TGLSceneViewer.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
   if IsOpenGLAvailable then
      Message.Result:=1
   else inherited; 
end;

// WMSize
//
procedure TGLSceneViewer.WMSize(var Message: TWMSize);
//var
//   aPoint : TPoint;
begin
	inherited;
	{ !!!!!!!!!!!!!! Disabled !!!!!!!!!!!!!!!
	if (woTransparent in DisplayOptions.WindowAttributes) then begin
		aPoint.X:=Left;
		aPoint.Y:=Top;
		if (not (woDesktop in DisplayOptions.WindowAttributes))
				or (csDesigning in ComponentState) then
			aPoint:=Parent.ClientToScreen(aPoint);
		with FBackground.Image do begin
			TGLCaptureImage(FBackground.Image).Left:=aPoint.X;
			TGLCaptureImage(FBackground.Image).Top:=aPoint.Y;
			Width:=RoundUpToPowerOf2(Message.Width);
			Height:=RoundUpToPowerOf2(Message.Height);
		end;
	end; }
	// define viewport
	if FRenderingContext <> 0 then begin
		ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
      try
         with FViewPort do begin
            Width:=Message.Width;
            Height:=Message.Height;
				if Height = 0 then Height:=1;
				glViewport(0, 0, Width, Height);
			end;
		finally
			DeactivateRenderingContext;
		end;
	end;
end;

// WMPaint
//
procedure TGLSceneViewer.WMPaint(var Message: TWMPaint);
var
   PS : TPaintStruct;
   recreate : Boolean;
begin
   invalidated:=False;
   recreate:=False;
	BeginPaint(Handle, PS);
	try
      if IsOpenGLAvailable then begin
         if csDesigning in ComponentState then begin
            try
               Render;
            except
               // from time to time, Delphi apparently damages or HDC/RC,
               // so we need to recreate it... (at least, try to...)
               on E:EMakeCurrentFaileException do begin
                  recreate:=True;
               end else raise;
            end;
         end else Render;
      end;
	finally
		EndPaint(Handle, PS);
		Message.Result:=0;
   end;
   if recreate then RecreateWnd;
end;

// WMDestroy
//
procedure TGLSceneViewer.WMDestroy(var Message: TWMDestroy);
begin
   if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.RemoveViewer(Self);
   inherited;
end;

//------------------------------------------------------------------------------

function TGLSceneViewer.ObjectInScene(Obj: TGLBaseSceneObject): Boolean;

var
  ModelMatrix: THomogeneousDblMatrix;
  ProjectMatrix: THomogeneousDblMatrix;
  VP: THomogeneousIntVector;
  WinX, WinY, WinZ: Double;
  R: TRect;
  P: TPoint;
  
begin
  Result:=True;
  glGetDoublev(GL_MODELVIEW_MATRIX, @ModelMatrix);
  glGetDoublev(GL_PROJECTION_MATRIX, @ProjectMatrix);
  glGetIntegerv(GL_VIEWPORT, @VP);
  gluProject(Obj.Position.X, Obj.Position.Y, Obj.Position.Z, ModelMatrix, ProjectMatrix, VP, @WinX, @WinY, @WinZ);
  R:=Rect(Vp[0], Vp[1], Vp[2], Vp[3]);
  P.x:=Round(WinX);
  P.y:=Round(WinY);
  if (not PtInRect(R, P)) then
    Result:=False;
end;

//------------------------------------------------------------------------------

procedure TGLSceneViewer.ReadContextProperties;
begin
  FMaxLightSources:=LimitOf[limLights];
  FDoubleBuffered:=LimitOf[limDoubleBuffer] > 0;
  if glIsEnabled(GL_DEPTH_TEST) > 0 then Include(FCurrentStates, stDepthTest);
  if glIsEnabled(GL_CULL_FACE) > 0 then Include(FCurrentStates, stCullFace);
  if glIsEnabled(GL_LIGHTING) > 0 then Include(FCurrentStates, stLighting);
  if glIsEnabled(GL_FOG) > 0 then Include(FCurrentStates, stFog);
end;

// SetupRenderingContext
//
procedure TGLSceneViewer.SetupRenderingContext;
var
   ColorDepth: Cardinal;
begin
   ColorDepth:=GetDeviceCaps(Canvas.Handle, BITSPIXEL) * GetDeviceCaps(Canvas.Handle, PLANES);
   if roTwoSideLighting in FContextOptions then
      glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE)
   else glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_FALSE);
   glEnable(GL_NORMALIZE);
   FCurrentStates:=[stNormalize];
   if DepthTest then begin
      Include(FCurrentStates, stDepthTest);
      glEnable(GL_DEPTH_TEST)
   end else glDisable(GL_DEPTH_TEST);
   if FaceCulling then begin
      Include(FCurrentStates, stDepthTest);
      glEnable(GL_CULL_FACE)
   end else glDisable(GL_CULL_FACE);
   if Lighting then begin
      Include(FCurrentStates, stLighting);
      glEnable(GL_LIGHTING)
   end else glDisable(GL_LIGHTING);
   if FogEnable then begin
      Include(FCurrentStates, stFog);
      glEnable(GL_FOG)
   end else glDisable(GL_FOG);
   if ColorDepth < 24 then begin
      Include(FCurrentStates, stDither);
      glEnable(GL_DITHER);
   end else glDisable(GL_DITHER);
   glDepthFunc(GL_LESS);
   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;

// GetLimit
//
function TGLSceneViewer.GetLimit(Which: TLimitType): Integer;
var
  VP: array[0..1] of Double;
begin
  case Which of
    limClipPlanes:
      glGetIntegerv(GL_MAX_CLIP_PLANES, @Result);
    limEvalOrder:
      glGetIntegerv(GL_MAX_EVAL_ORDER, @Result);
    limLights:
      glGetIntegerv(GL_MAX_LIGHTS, @Result);
    limListNesting:
      glGetIntegerv(GL_MAX_LIST_NESTING, @Result);
    limModelViewStack:
      glGetIntegerv(GL_MAX_MODELVIEW_STACK_DEPTH, @Result);
    limNameStack:
      glGetIntegerv(GL_MAX_NAME_STACK_DEPTH, @Result);
    limPixelMapTable:
      glGetIntegerv(GL_MAX_PIXEL_MAP_TABLE, @Result);
    limProjectionStack:
      glGetIntegerv(GL_MAX_PROJECTION_STACK_DEPTH, @Result);
    limTextureSize:
      glGetIntegerv(GL_MAX_TEXTURE_SIZE, @Result);
    limTextureStack:
      glGetIntegerv(GL_MAX_TEXTURE_STACK_DEPTH, @Result);
    limViewportDims:
      begin
        glGetDoublev(GL_MAX_VIEWPORT_DIMS, @VP);
        if VP[0]>VP[1] then Result:=Round(VP[0]) else Result:=Round(VP[1]);
      end;
    limAccumAlphaBits:
      glGetIntegerv(GL_ACCUM_ALPHA_BITS, @Result);
    limAccumBlueBits:
      glGetIntegerv(GL_ACCUM_BLUE_BITS, @Result);
    limAccumGreenBits:
      glGetIntegerv(GL_ACCUM_GREEN_BITS, @Result);
    limAccumRedBits:
      glGetIntegerv(GL_ACCUM_RED_BITS, @Result);
    limAlphaBits:
      glGetIntegerv(GL_ALPHA_BITS, @Result);
    limAuxBuffers:
      glGetIntegerv(GL_AUX_BUFFERS, @Result);
    limDepthBits:
      glGetIntegerv(GL_DEPTH_BITS, @Result);
    limStencilBits:
      glGetIntegerv(GL_STENCIL_BITS, @Result);
    limBlueBits:
      glGetIntegerv(GL_BLUE_BITS, @Result);
    limGreenBits:
      glGetIntegerv(GL_GREEN_BITS, @Result);
    limRedBits:
      glGetIntegerv(GL_RED_BITS, @Result);
    limIndexBits:
      glGetIntegerv(GL_INDEX_BITS, @Result);
    limStereo:
      glGetIntegerv(GL_STEREO, @Result);
    limDoubleBuffer:
      glGetIntegerv(GL_DOUBLEBUFFER, @Result);
    limSubpixelBits:
      glGetIntegerv(GL_SUBPIXEL_BITS, @Result);
  else
    Result:=0;
  end;
end;

//------------------------------------------------------------------------------

procedure TGLSceneViewer.Loaded;

var
  NewMode: Integer;

begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    // set display mode depending on the different screen options
    // full screen requested?
    if DisplayOptions.FullScreen then begin
      // full screen mode, so check for window fitting
      case DisplayOptions.WindowFitting of
        wfFitWindowToScreen: // set screen to the specified size
          begin
            NewMode:=DisplayOptions.ScreenResolution;
            if NewMode <> 0 then SetFullScreenMode(NewMode);
          end;
        wfFitScreenToWindow: // adjust screen size to window size
          begin
            NewMode:=GetIndexFromResolution(Width, Height, VideoModes[0].ColorDepth);
            SetFullScreenMode(NewMode);
          end;
      end;
      Left:=0;
      Top:=0;
      ShowWindow(Handle, SW_SHOWMAXIMIZED);
    end else
      // no full screen mode for the application, but perhaps
      // a specific resolution or color depth?
      if DisplayOptions.ScreenResolution <> 0 then
        SetFullScreenMode(DisplayOptions.ScreenResolution);
  // initiate window creation
  HandleNeeded;
end;

// RenderToFile
//
procedure TGLSceneViewer.RenderToFile(const AFile: String; DPI: Integer);
var
  ABitmap: TBitmap;
  SaveDialog: TSavePictureDialog;
  SaveAllowed: Boolean;
  FName: String;
begin
	Assert((FState = dsNone), glsAlreadyRendering);
   SaveDialog:=nil;
   ABitmap:=TBitmap.Create;
   try
      ABitmap.Width:=Width;
      ABitmap.Height:=Height;
      ABitmap.PixelFormat:=pf24Bit;
      RenderToBitmap(ABitmap, DPI);
      FName:=AFile;
      SaveAllowed:=True;
      if FName = '' then begin
         SaveDialog:=TSavePictureDialog.Create(Application);
         with SaveDialog do begin
            Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
            SaveAllowed:=Execute;
         end;
      end;
      if SaveAllowed then begin
         if FName = '' then begin
            FName:=SaveDialog.FileName;
            if (FileExists(SaveDialog.FileName)) then
               SaveAllowed:=MessageDlg(Format('Overwrite file %s?', [SaveDialog.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
         end;
         if SaveAllowed then ABitmap.SaveToFile(FName);
      end;
   finally
      SaveDialog.Free;
      ABitmap.Free;
   end;
end;

// RenderToFile
//
procedure TGLSceneViewer.RenderToFile(const AFile: String; bmpWidth, bmpHeight : Integer);
var
   ABitmap: TBitmap;
   SaveDialog: TSavePictureDialog;
   SaveAllowed: Boolean;
   FName: String;
begin
	Assert((FState = dsNone), glsAlreadyRendering);
   SaveDialog:=nil;
   ABitmap:=TBitmap.Create;
   try
      ABitmap.Width:=bmpWidth;
      ABitmap.Height:=bmpHeight;
      ABitmap.PixelFormat:=pf24Bit;
      RenderToBitmap(ABitmap, (GetDeviceCaps(ABitmap.Canvas.Handle, LOGPIXELSX)*bmpWidth) div width);
      FName:=AFile;
      SaveAllowed:=True;
      if FName = '' then begin
         SaveDialog:=TSavePictureDialog.Create(Application);
         with SaveDialog do begin
            Options:=[ofHideReadOnly, ofNoReadOnlyReturn];
            SaveAllowed:=Execute;
         end;
      end;
      if SaveAllowed then begin
         if FName = '' then begin
            FName:=SaveDialog.FileName;
            if (FileExists(SaveDialog.FileName)) then
               SaveAllowed:=MessageDlg(Format('Overwrite file %s?', [SaveDialog.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes;
         end;
         if SaveAllowed then ABitmap.SaveToFile(FName);
      end;
   finally
      SaveDialog.Free;
      ABitmap.Free;
   end;
end;

// TGLBitmap32
//
function TGLSceneViewer.CreateSnapShot : TGLBitmap32;
var
   activatedRC : Boolean;
begin
   Result:=TGLBitmap32.Create;
   Result.Width:=Width;
   Result.Height:=Height;
   if Assigned(Camera) and Assigned(Camera.Scene) then begin
      activatedRC:=(CurrentRC=0);
      if activatedRC then
         Camera.Scene.ActivateDefaultRenderingContext;
      try
         Result.ReadPixels(ClientRect);
      finally
         if activatedRC then
            DeactivateRenderingContext;
      end;
   end;
end;

// Invalidate
//
procedure TGLSceneViewer.Invalidate;
begin
   if not invalidated then begin
	   inherited Invalidate;
      invalidated:=True;
   end;
end;

// SetViewPort
//
procedure TGLSceneViewer.SetViewPort(X, Y, W, H: Integer);
begin
   with FViewPort do begin
      Left:=X;
      Top:=Y;
      Width:=W;
      Height:=H;
   end;
   Perform(WM_SIZE, SIZE_RESTORED, MakeLong(Width, Height));
   if not (csReading in ComponentState) then Invalidate;
end;

// RenderToBitmap
//
procedure TGLSceneViewer.RenderToBitmap(ABitmap: TBitmap; DPI: Integer);
var
   BitmapContext: HGLRC;
   BackColor: TColorVector;
   ColorBits: Integer;
   viewport: TRectangle;
   LastStates: TGLStates;
   Resolution: Integer;
begin
	Assert((FState = dsNone), glsAlreadyRendering);
   FState:=dsPrinting;
   try
      case ABitmap.PixelFormat of
         pfCustom, pfDevice :  // use current color depth
            ColorBits:=VideoModes[CurrentVideoMode].ColorDepth;
         pf1bit, pf4bit : // OpenGL needs at least 4 bits
            ColorBits:=4;
         pf8bit : ColorBits:=8;
         pf15bit : ColorBits:=15;
         pf16bit : ColorBits:=16;
         pf24bit : ColorBits:=24;
         pf32bit : ColorBits:=32;
      else
         ColorBits:=24;
      end;
      BitmapContext:=CreateRenderingContext(ABitmap.Canvas.Handle, [], ColorBits, 0, 0, 0, 0);
      Assert(BitmapContext<>0);
      try
         // save current window context states
         lastStates:=FCurrentStates;
         // we must free the lists before changeing context, or it will have no effect
         if Assigned(FCamera) and Assigned(FCamera.FScene) then
            FCamera.FScene.Objects.FreeList(False);
         ActivateRenderingContext(ABitmap.Canvas.Handle, BitmapContext);
         try
            SetupRenderingContext;
            BackColor:=ConvertWinColor(FBackgroundColor);
            glClearColor(BackColor[0], BackColor[1], BackColor[2], BackColor[3]);
            // set the desired viewport and limit output to this rectangle
            with Viewport do begin
               Left:=0;
               Top:=0;
               Width:=ABitmap.Width;
               Height:=ABitmap.Height;
               glViewport(Left, Top, Width, Height);
            end;
            ClearBuffers;
            ResetGLPolygonMode;
            ResetGLMaterialColors;
            Resolution:=DPI;
            if Resolution=0 then
               Resolution:=GetDeviceCaps(ABitmap.Canvas.Handle, LOGPIXELSX);
            // render
            DoBaseRender(viewport, Resolution);
            // release allocated lists
            if Assigned(FCamera) and Assigned(FCamera.FScene) then
               FCamera.FScene.Objects.FreeList(True);
            glFinish;
         finally
            DeactivateRenderingContext;
            FCurrentStates:=LastStates;
         end;
      finally
         DestroyRenderingContext(BitmapContext);
      end;
   finally
      FState:=dsNone;
   end;
   if Assigned(FAfterRender) then FAfterRender(Self);
end;

// ShowInfo
//
procedure TGLSceneViewer.ShowInfo;
var
   infoForm: TInfoForm;
begin
   Application.CreateForm(TInfoForm, infoForm);
   try
      ActivateRenderingContext(Canvas.Handle, FRenderingContext);
      // most info is available with active context only
      try
         infoForm.GetInfoFrom(Self);
      finally
         DeactivateRenderingContext;
      end;
      infoForm.ShowModal;
   finally
      infoForm.Free;
   end;
end;
(*
// RequestedState
//
procedure TGLSceneViewer.RequestedState(States: TGLStates);
var
   neededStates: TGLStates;
begin
   // create window and rendering context if not yet done
   HandleNeeded;
   // get all states, which are requested but not yet set
   NeededStates:=States-FCurrentStates;
   if NeededStates<>[] then begin
      SetStates(NeededStates);
      FCurrentStates:=FCurrentStates+NeededStates;
   end;
end;

// UnnecessaryState
//
procedure TGLSceneViewer.UnnecessaryState(States: TGLStates);
var
   takeOutStates: TGLStates;
begin
   { TODO : Better and faster version }
   // create window and rendering context if not yet done
   HandleNeeded;
   // get all states, which are to be taken out, but still set
   takeOutStates:=States * FCurrentStates;
   if takeOutStates <> [] then begin
      // now reset all these states
      ResetStates(takeOutStates);
      FCurrentStates:=FCurrentStates-takeOutStates;
   end;
end;
*)
// ResetPerformanceMonitor
//
procedure TGLSceneViewer.ResetPerformanceMonitor;
begin
   FFramesPerSecond:=0;
   FFrames:=0;
   FTicks:=0;
end;

// ScreenToWorld
//
function TGLSceneViewer.ScreenToWorld(const aPoint : TAffineVector) : TAffineVector;
var
   proj, mv : THomogeneousDblMatrix;
   x, y, z : Double;
begin
   if Assigned(FCamera) then begin
      SetMatrix(proj, ProjectionMatrix);
      SetMatrix(mv, ModelViewMatrix);
      gluUnProject(aPoint[0], aPoint[1], aPoint[2],
                   mv, proj, PHomogeneousIntVector(@FViewPort)^,
                   @x, @y, @z);
      SetVector(Result, x, y, z);
   end else Result:=aPoint;
end;

// ScreenToWorld
//
function TGLSceneViewer.ScreenToWorld(screenX, screenY : Integer) : TAffineVector;
begin
   Result:=ScreenToWorld(AffineVectorMake(screenX, Height-screenY, 0));
end;

// WorldToScreen
//
function TGLSceneViewer.WorldToScreen(const aPoint : TAffineVector) : TAffineVector;
var
   proj, mv : THomogeneousDblMatrix;
   x, y, z : Double;
begin
   if Assigned(FCamera) then begin
      SetMatrix(proj, ProjectionMatrix);
      SetMatrix(mv, ModelViewMatrix);
      gluProject(aPoint[0], aPoint[1], aPoint[2],
                 mv, proj, PHomogeneousIntVector(@FViewPort)^,
                 @x, @y, @z);
      SetVector(Result, x, y, z);
   end else Result:=aPoint;
end;

// ScreenToVector
//
function TGLSceneViewer.ScreenToVector(const aPoint : TAffineVector) : TAffineVector;
begin
   Result:=VectorSubtract(ScreenToWorld(aPoint),
                          PAffineVector(@FCameraAbsolutePosition)^);
end;

// ScreenVectorIntersectWithPlane
//
function TGLSceneViewer.ScreenVectorIntersectWithPlane(
      const aScreenPoint : TAffineVector;
      const planePoint, planeNormal : TAffineVector;
      var intersectPoint : TAffineVector) : Boolean;
var
   v : TAffineVector;
begin
   if Assigned(FCamera) then begin
      v:=ScreenToVector(aScreenPoint);
      Result:=RayCastPlaneIntersect(PAffineVector(@FCameraAbsolutePosition)^,
                                    v, planePoint, planeNormal, @intersectPoint);
   end else Result:=False;
end;

// ScreenVectorIntersectWithPlaneXY
//
function TGLSceneViewer.ScreenVectorIntersectWithPlaneXY(
   const aScreenPoint : TAffineVector; const z : TGLFloat;
   var intersectPoint : TAffineVector) : Boolean;
begin
   Result:=ScreenVectorIntersectWithPlane(aScreenPoint, AffineVectorMake(0, 0, z),
                                          ZVector, intersectPoint);
end;

// ScreenVectorIntersectWithPlaneYZ
//
function TGLSceneViewer.ScreenVectorIntersectWithPlaneYZ(
   const aScreenPoint : TAffineVector; const x : TGLFloat;
   var intersectPoint : TAffineVector) : Boolean;
begin
   Result:=ScreenVectorIntersectWithPlane(aScreenPoint, AffineVectorMake(x, 0, 0),
                                          XVector, intersectPoint);
end;

// ScreenVectorIntersectWithPlaneXZ
//
function TGLSceneViewer.ScreenVectorIntersectWithPlaneXZ(
   const aScreenPoint : TAffineVector; const y : TGLFloat;
   var intersectPoint : TAffineVector) : Boolean;
begin
   Result:=ScreenVectorIntersectWithPlane(aScreenPoint, AffineVectorMake(0, y, 0),
                                          YVector, intersectPoint);
end;

// ClearBuffers
//
procedure TGLSceneViewer.ClearBuffers;
type
   PPixelArray  = ^TByteVector;
var
   bufferBits : TGLBitfield;
begin
   // handle transparency simulation
{   if (woTransparent in DisplayOptions.WindowAttributes) then begin
     glPushAttrib(GL_ENABLE_BIT);
     glEnable(GL_TEXTURE_2D);
     glDisable(GL_LIGHTING);
     glDisable(GL_DITHER);
     glDisable(GL_DEPTH_TEST);
     glDisable(GL_BLEND);
     // Invalidate initiated by the scene itself?
     FBackground.DisableAutoTexture;
     FBackground.Apply;
     glMatrixMode(GL_MODELVIEW);
     glPushMatrix;
     glLoadIdentity;
     glMatrixMode(GL_PROJECTION);
     glPushMatrix;
     glLoadIdentity;
     glOrtho(0, Width - 1, 0, Height - 1, 0, 100);
     glFrontFace(GL_CCW);
     glBegin(GL_QUADS);
       xglTexCoord2f(0, 1 - Height / FBackground.Image.Height);
       glVertex3f(0, 0, 0);

       xglTexCoord2f(Width / FBackground.Image.Width, 1 - Height / FBackground.Image.Height);
       glVertex3f(Width - 1, 0, 0);

       xglTexCoord2f(Width/FBackground.Image.Width, 1);
       glVertex3f(Width - 1, Height - 1, 0);

       xglTexCoord2f(0, 1);
       glVertex3f(0, Height - 1, 0);
     glEnd;
     glMatrixMode(GL_MODELVIEW);
     glPopMatrix;
     glMatrixMode(GL_PROJECTION);
     glPopMatrix;
     glPopAttrib;
   end;}

   // Convert our buXXX in Buffers to the GL boolean set
   if (buColor in Buffers) then //and not (woTransparent in DisplayOptions.WindowAttributes) then
      bufferBits:=GL_COLOR_BUFFER_BIT
   else bufferBits:=0;
   if buDepth in Buffers then
      bufferBits:=bufferBits or GL_DEPTH_BUFFER_BIT;
   if buStencil in Buffers then
      bufferBits:=bufferBits or GL_STENCIL_BUFFER_BIT;
   if buAccum in Buffers then
      bufferBits:=bufferBits or GL_ACCUM_BUFFER_BIT;
   if bufferBits<>0 then glClear(BufferBits);
end;

// Notification
//
procedure TGLSceneViewer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   inherited;
   if (Operation = opRemove) and (AComponent = FCamera) then
      Camera:=nil;
end;

// PickObjects
//
procedure TGLSceneViewer.PickObjects(const Rect: TRect; PickList: TGLPickList;
												 objectCountGuess: Integer);
var
   buffer : PCardinalVector;
   hits : Integer;
   i : Integer;
   current, next : Cardinal;
   szmin, szmax : Single;
   subObj : TPickSubObjects;
   subObjIndex : Cardinal;
begin
   if not Assigned(FCamera) then Exit;
	Assert((FState = dsNone), glsAlreadyRendering);
   Assert(Assigned(PickList));
   ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
   FState:=dsPicking;
   try
      buffer:=nil;
      try
         PrepareRenderingMatrices(FViewPort, GetDeviceCaps(Canvas.Handle, LOGPIXELSX),
                                  @Rect);
			// check countguess, memory waste is not an issue here
         if objectCountGuess<8 then objectCountGuess:=8;
         hits:=-1;
         repeat
				if hits < 0 then begin
               // Allocate 4 integers per row (EG : dunno why 4)
               // Add 32 integers of slop (an extra cache line) to end for buggy
					// hardware that uses DMA to return select results but that sometimes
               // overrun the buffer.  Yuck.
               ReallocMem(buffer, objectCountGuess * 4 * SizeOf(Integer) + 32 * 4);
               // increase buffer by 50% if we get nothing
               Inc(objectCountGuess, objectCountGuess shr 1);
            end;
            // pass buffer to opengl and prepare render
            glSelectBuffer(objectCountGuess*4, @Buffer^);
            glRenderMode(GL_SELECT);
				glInitNames;
            glPushName(0);
            // render the scene (in select mode, nothing is drawn)
            if Assigned(FCamera) and Assigned(FCamera.FScene) then
               FCamera.FScene.RenderScene(Self, Width, Height);
            glFlush;
            Hits:=glRenderMode(GL_RENDER);
			until Hits>-1; // try again with larger selection buffer
         next:=0;
			PickList.Clear;
         PickList.Capacity:=Hits;
         for I:=0 to Hits-1 do begin
            current:=next;
            next:=current + buffer[current] + 3;
            szmin:=(buffer[current + 1] shr 1) / MaxInt;
            szmax:=(buffer[current + 2] shr 1) / MaxInt;
            subObj:=nil;
            subObjIndex:=current+4;
            if subObjIndex<next then begin
               SetLength(subObj, buffer[current]-1);
               while subObjIndex<next do begin
                  subObj[subObjIndex-current-4]:=buffer[subObjIndex];
                  inc(subObjIndex);
               end;
            end;
				PickList.AddHit(TGLCustomSceneObject(buffer[current+3]),
                            subObj, szmin, szmax);
         end;
      finally
         FreeMem(Buffer);
      end;
   finally
      FState:=dsNone;
      DeactivateRenderingContext;
   end;
end;

// GetPickedObjects
//
function TGLSceneViewer.GetPickedObjects(const Rect: TRect; objectCountGuess : Integer = 64) : TGLPickList;
begin
	Result:=TGLPickList.Create(psMinDepth);
	PickObjects(Rect, Result, objectCountGuess);
end;

// GetPickedObject
//
function TGLSceneViewer.GetPickedObject(x, y : Integer) : TGLBaseSceneObject;
var
	pkList : TGLPickList;
begin
	pkList:=GetPickedObjects(Rect(x-1, y-1, x+1, y+1));
	try
		if pkList.Count>0 then
			Result:=pkList.Hit[0]
		else Result:=nil;
	finally
		pkList.Free;
	end;
end;

// GetPixelColor
//
function TGLSceneViewer.GetPixelColor(x, y : Integer) : TColor;
var
   buf : array [0..2] of Byte;
begin
   if not Assigned(FCamera) then begin
      Result:=0;
      Exit;
   end;
   ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
   try
      glReadPixels(x, Height-y, 1, 1, GL_RGB, GL_UNSIGNED_BYTE, @buf[0]);
   finally
      DeactivateRenderingContext;
   end;
   Result:=RGB(buf[0], buf[1], buf[2]);
end;

// GetPixelDepth
//
function TGLSceneViewer.GetPixelDepth(x, y : Integer) : Single;
//var
//   v : TAffineVector;
begin
   if not Assigned(FCamera) then begin
      Result:=0;
      Exit;
   end;
   ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
   try
      glReadPixels(x, Height-y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @Result);
//      v:=ScreenToWorld(AffineVectorMake(x, Height-y, Result));
//      Result:=v[2];
   finally
      DeactivateRenderingContext;
   end;
end;

// PrepareRenderingMatrices
//
procedure TGLSceneViewer.PrepareRenderingMatrices(const aViewPort : TRectangle;
                           resolution : Integer; pickingRect : PRect = nil);
begin
   // setup projection matrix
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity;
   if Assigned(pickingRect) then
      gluPickMatrix((pickingRect.Left+pickingRect.Right) div 2,
                    Height-((pickingRect.Top+pickingRect.Bottom) div 2),
						  Abs(pickingRect.Right - pickingRect.Left),
                    Abs(pickingRect.Bottom - pickingRect.Top),
						  TVector4i(FViewport));
   glGetFloatv(GL_PROJECTION_MATRIX, @FBaseProjectionMatrix);
   FCamera.ApplyPerspective(aViewport, Width, Height, resolution);
   glGetFloatv(GL_PROJECTION_MATRIX, @FProjectionMatrix);

   // setup model view matrix
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity;
   FCamera.FScene.ValidateTransformation(FCamera);
   glGetFloatv(GL_MODELVIEW_MATRIX, @FModelViewMatrix);
   FCameraAbsolutePosition:=FCamera.AbsolutePosition;
end;

// DoBaseRender
//
procedure TGLSceneViewer.DoBaseRender(const aViewPort : TRectangle; resolution : Integer);
begin
   if (not Assigned(FCamera)) or (not Assigned(FCamera.FScene)) then Exit;
   PrepareRenderingMatrices(aViewPort, resolution);
   if Assigned(FBeforeRender) then FBeforeRender(Self);
   with FCamera.FScene do begin
      SetupLights(FMaxLightSources);
      if FogEnable then begin
         glEnable(GL_FOG);
         FogEnvironment.ApplyFog;
      end else glDisable(GL_FOG);
      RenderScene(Self, Width, Height);
   end;
   if Assigned(FPostRender) then FPostRender(Self);
end;

// Render
//
procedure TGLSceneViewer.Render;
var
   Counter1, Counter2: TLargeInteger;
begin
	if ((not Visible) and (not (csDesigning in ComponentState))) or (FState<>dsNone) then Exit;
   if Assigned(FCamera) and Assigned(FCamera.FScene) then
      FCamera.FScene.AddViewer(Self);
   ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
   FState:=dsRendering;
   try
      // performance data demanded?
      if FMonitor then QueryPerformanceCounter(Counter1);
      // clear the buffers
      ClearBuffers;
      // render
      DoBaseRender(FViewport, GetDeviceCaps(Canvas.Handle, LOGPIXELSX));
      glFlush;
      if FDoubleBuffered then SwapBuffers(Canvas.Handle);

      // performance data demanded?
      if FMonitor then begin
         // yes, calculate average frames per second...
         Inc(FFrames);
         if FFrames > 1 then begin // ...but leave out the very first frame
            QueryPerformanceCounter(Counter2);
            // in second run take an 'average' value for the first run into account
            // by simply using twice the time from this run
            if FFrames = 2 then
               FTicks:=FTicks + 2 * (Counter2 - Counter1)
            else FTicks:=FTicks + Counter2 - Counter1;
            if FTicks > 0 then
					FFramesPerSecond:=FFrames * vCounterFrequency / FTicks;
         end;
      end;
   finally
      FState:=dsNone;
      DeactivateRenderingContext;
   end;
   if Assigned(FAfterRender) then FAfterRender(Self);
end;

// SetBackgroundColor
//
procedure TGLSceneViewer.SetBackgroundColor(AColor: TColor);
var
   backColor: TColorVector;
begin
   if FBackgroundColor <> AColor then begin
      FBackgroundColor:=AColor;
      if not (csReading in ComponentState) then begin
         backColor:=ConvertWinColor(FBackgroundColor);
         ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
         try
            glClearColor(BackColor[0], BackColor[1], BackColor[2], BackColor[3]);
         finally
            DeactivateRenderingContext;
         end;
         Invalidate;
      end;
   end;
end;

// SetCamera
//
procedure TGLSceneViewer.SetCamera(ACamera: TGLCamera);
begin
   if FCamera <> ACamera then begin
      if Assigned(FCamera) then begin
         if Assigned(FCamera.FScene) then
            FCamera.FScene.RemoveViewer(Self);
         FCamera:=nil;
      end;
      if Assigned(ACamera) and Assigned(ACamera.FScene) then begin
         FCamera:=ACamera;
         Include(FCamera.FChanges, ocTransformation);
         if not (csLoading in ComponentState) then begin
            RecreateWnd;
            HandleNeeded;
         end;
      end;
      Invalidate;
   end;
end;

// SetContextOptions
//
procedure TGLSceneViewer.SetContextOptions(Options: TContextOptions);
begin
   if FContextOptions<>Options then begin
      FContextOptions:=Options;
      Invalidate;
   end;
end;

// SetDepthTest
//
procedure TGLSceneViewer.SetDepthTest(AValue: Boolean);
begin
   if FDepthTest<>AValue then begin
      FDepthTest:=AValue;
      Invalidate;
  end;
end;

// SetStencilTest
//
procedure TGLSceneViewer.SetStencilTest(val : Boolean);
begin
   if val<>FStencilTest then begin
      FStencilTest:=val;
      RecreateWnd;
   end;
end;

// SetFaceCulling
//
procedure TGLSceneViewer.SetFaceCulling(AValue: Boolean);
begin
   if FFaceCulling <> AValue then begin
      FFaceCulling:=AValue;
      Invalidate;
   end;
end;

// SetLighting
//
procedure TGLSceneViewer.SetLighting(AValue: Boolean);
begin
   if FLighting <> AValue then begin
      FLighting:=AValue;
      Invalidate;
   end;
end;

// SetFogEnable
//
procedure TGLSceneViewer.SetFogEnable(AValue: Boolean);
begin
   if FFogEnable <> AValue then begin
      FFogEnable:=AValue;
      Invalidate;
   end;
end;

//------------------------------------------------------------------------------

{procedure TGLSceneViewer.SetSpecials(Value: TSpecials);
begin
   if FSpecials <> Value then begin
      FSpecials:=Value;
      Invalidate;
   end;
end; }

// SetGLFogEnvironment
//
procedure TGLSceneViewer.SetGLFogEnvironment(AValue: TGLFogEnvironment);
begin
   ActivateRenderingContext(FCanvas.Handle, FRenderingContext);
   try
      FFogEnvironment.Assign(AValue);
      FFogEnvironment.NotifyChange(Self);
   finally
      DeactivateRenderingContext;
   end;
   Invalidate;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

	RegisterClasses([TGLLightSource, TGLCamera, TGLProxyObject, TGLSceneViewer,
                    TGLScene, TDirectOpenGL]);

	// preparation for high resolution timer
	if not QueryPerformanceFrequency(vCounterFrequency) then
		vCounterFrequency:=0;

end.
