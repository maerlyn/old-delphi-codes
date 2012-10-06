// GLSceneRegister
{: Registration unit for GLScene library components, property editors and
      IDE experts.<p>

	<b>History : </b><font size=-1><ul>
      <li>18/02/01 - EG - Added Terrain/HeightData objects
      <li>21/01/01 - EG - Enhanced GetAttributes for some property editors
      <li>09/10/00 - EG - Added registration for TMultiPolygon
      <li>09/06/00 - EG - Added TSoundFileProperty & TSoundNameProperty
      <li>23/05/00 - EG - Added GLCollision
      <li>16/05/00 - EG - Delphi 4 Compatibility
      <li>28/04/00 - EG - Removed ObjectStock in TObjectManager (was useless)
      <li>26/04/00 - EG - Added Categories in ObjectManager,
                          enhanced GetRegisteredSceneObjects
      <li>16/04/00 - EG - Objects icons are now loaded from ressources using
                          ClassName (more VCL-like)
		<li>11/04/00 - EG - Components now install under 'GLScene',
								  Fixed DestroySceneObjectList (thanks Uwe Raabe)
		<li>06/04/00 - EG - Added TGLBehavioursProperty
		<li>18/03/00 - EG - Added TGLImageClassProperty
		<li>13/03/00 - EG - Updated TGLTextureImageProperty
      <li>14/02/00 - EG - Added MaterialLibrary editor and picker
      <li>09/02/00 - EG - ObjectManager moved in, ObjectManager is now fully
                          object-oriented and encapsulated
      <li>06/02/00 - EG - Fixed TGLScenedEditor logic
                          (was causing Delphi IDE crashes on package unload)
      <li>05/02/00 - EG - Added TGLColorProperty and TGLCoordinatesProperty
	</ul></font>
}
unit GLSceneRegister;

// Registration unit for GLScene library
// 30-DEC-99 ml: scene editor added, structural changes

interface

{$I DFS.inc}

uses Windows, PlugInManager, GLScene, Controls, Classes;

type

	PSceneObjectEntry = ^TGLSceneObjectEntry;
	// holds a relation between an scene object class, its global identification,
	// its location in the object stock and its icon reference
	TGLSceneObjectEntry = record
								  ObjectClass : TGLSceneObjectClass;
								  Name : String[32];     // type name of the object
                          Category : String[32]; // category of object
								  Index,                 // index into "FObjectStock"
								  ImageIndex : Integer;  // index into "FObjectIcons"
								end;

	// TObjectManager
   //
   TObjectManager = class(TResourceManager)
      private
         { Private Declarations }
         FSceneObjectList : TList;
         FObjectIcons : TImageList;       // a list of icons for scene objects
         FOverlayIndex,                   // indices into the object icon list
         FSceneRootIndex,
         FCameraRootIndex,
         FLightsourceRootIndex,
         FObjectRootIndex,
         FStockObjectRootIndex : Integer;

      protected
			{ Protected Declarations }
         procedure CreateDefaultObjectIcons;
         procedure DestroySceneObjectList;
         function FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                                       const ASceneObject: String = '') : PSceneObjectEntry;

      public
         { Public Declarations }
         constructor Create(Aowner: TComponent); override;
			destructor Destroy; override;

			function GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
			function GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
         function GetCategory(ASceneObject: TGLSceneObjectClass) : String;
			procedure GetRegisteredSceneObjects(ObjectList: TStringList);
			//: Registers a stock object and adds it to the stock object list
			procedure RegisterSceneObject(ASceneObject: TGLSceneObjectClass; const aName, aCategory : String);
         //: Unregisters a stock object and removes it from the stock object list
			procedure UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
         procedure Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer); override;

         property ObjectIcons: TImageList read FObjectIcons;
         property SceneRootIndex: Integer read FSceneRootIndex;
         property LightsourceRootIndex: Integer read FLightsourceRootIndex;
			property CameraRootIndex: Integer read FCameraRootIndex;
         property ObjectRootIndex: Integer read FObjectRootIndex;

      end;

procedure Register;

//: Auto-create for object manager
function ObjectManager : TObjectManager;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

{$R GLSceneRegister.res}

uses
  DsgnIntf, GLScreen, GLObjects, GLTexture, SysUtils, Dialogs, GLVectorFileObjects,
  ExtDlgs, Forms, GLSceneEdit, Graphics, AsyncTimer, FVectorEditor, Geometry,
{$ifdef DFS_DELPHI_5_UP}
  FMaterialEditorForm, FLibMaterialPicker,
{$endif}
  TypInfo, GLParticles, GLCadencer, GLStrings, GLCollision, GLSound, GLPortal,
  GLSoundFileObjects, GLMesh, GLGraph, GLMisc, GLExtrusion, GLFireFX, GLThorFX,
  GLMultiPolygon, GLSkyDome, GLHUDObjects, GLBitmapFont, GLHeightData,
  GLTerrainRenderer;

var
	vObjectManager : TObjectManager;

function ObjectManager : TObjectManager;
begin
   if not Assigned(vObjectManager) then
      vObjectManager:=TObjectManager.Create(nil);
   Result:=vObjectManager;
end;

{ TODO : Moving property editors to the public interface }

type

   // TGLSceneViewerEditor
   //
   TGLSceneViewerEditor = class(TComponentEditor)
      public
			{ Public Declarations }
			procedure ExecuteVerb(Index: Integer); override;
			function GetVerb(Index: Integer): String; override;
			function GetVerbCount: Integer; override;
	end;

   // TGLSceneEditor
   //
   TGLSceneEditor = class(TComponentEditor)
      public
         { Public Declarations }
         procedure Edit; override;
   end;

  TPlugInProperty = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: String; override;
  end;

  TResolutionProperty = class(TPropertyEditor)
	 function GetAttributes: TPropertyAttributes; override;
	 function GetValue : String; override;
	 procedure GetValues(Proc: TGetStrProc); override;
	 procedure SetValue(const Value: String); override;
  end;

  TGLTextureProperty = class(TClassProperty)
  protected
	 function GetAttributes: TPropertyAttributes; override;
  end;

	// TGLTextureImageProperty
	//
	TGLTextureImageProperty = class(TClassProperty)
		protected
			{ Protected Declarations }
			function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;

	// TGLImageClassProperty
	//
	TGLImageClassProperty = class(TClassProperty)
		protected
			{ Protected Declarations }
			function GetAttributes : TPropertyAttributes; override;
			procedure GetValues(proc : TGetStrProc); override;

		public
			{ Public Declarations }
			function GetValue : String; override;
			procedure SetValue(const value : String); override;
	end;

  TGLColorProperty = class(TClassProperty)
  private
  protected
	 function GetAttributes: TPropertyAttributes; override;
	 procedure GetValues(Proc: TGetStrProc); override;
	 procedure Edit; override;
  public
	 {$ifdef DFS_COMPILER_5_UP}
	 procedure ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	 procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); override;
	 {$endif}
	 function GetValue: String; override;
	 procedure SetValue(const Value: string); override;
  end;

   // TVectorFileProperty
   //
   TVectorFileProperty = class (TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
         procedure SetValue(const Value: string); override;
   end;

   // TSoundFileProperty
   //
   TSoundFileProperty = class (TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
         function GetValue: String; override;
         procedure Edit; override;
   end;

   // TSoundNameProperty
   //
   TSoundNameProperty = class (TStringProperty)
      protected
         { Protected Declarations }
         function GetAttributes : TPropertyAttributes; override;
      	procedure GetValues(Proc: TGetStrProc); override;
   end;

   // TGLCoordinatesProperty
   //
   TGLCoordinatesProperty = class(TClassProperty)
      protected
         { Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

{$ifdef DFS_DELPHI_5_UP}
	// TGLMaterialProperty
	//
	TGLMaterialProperty = class(TClassProperty)
		protected
			{ Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
         procedure Edit; override;
   end;

   // TReuseableDefaultEditor
   //
   {: Editor copied from DsgnIntf.<p>
      Could have been avoided, if only that short-sighted guy at Borland didn't
      chose to publish only half of the stuff (and that's not the only class with
      that problem, most of the subitems handling code in TGLSceneBaseObject is
      here for the same reason...), the "protected" wasn't meant just to lure
      programmers into code they can't reuse... }
   TReuseableDefaultEditor = class (TComponentEditor)
      protected
			{ Protected Declarations }
         FFirst: TPropertyEditor;
			FBest: TPropertyEditor;
         FContinue: Boolean;
         procedure CheckEdit(PropertyEditor: TPropertyEditor);
         procedure EditProperty(PropertyEditor: TPropertyEditor;
                                var Continue, FreeEditor: Boolean); virtual;
      public
         { Public Declarations }
         procedure Edit; override;
   end;

   // TGLMaterialLibraryEditor
   //
   {: Editor for material library.<p> }
   TGLMaterialLibraryEditor = class(TReuseableDefaultEditor)
      protected
         procedure EditProperty(PropertyEditor: TPropertyEditor;
										  var Continue, FreeEditor: Boolean); override;
	end;

	// TGLLibMaterialNameProperty
	//
	TGLLibMaterialNameProperty = class(TStringProperty)
		protected
			{ Protected Declarations }
         function GetAttributes: TPropertyAttributes; override;
			procedure Edit; override;
	end;
{$endif}

//----------------- TObjectManager ---------------------------------------------

// Create
//
constructor TObjectManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSceneObjectList:=TList.Create;
  CreateDefaultObjectIcons;
end;

// Destroy
//
destructor TObjectManager.Destroy;
begin
	DestroySceneObjectList;
   FObjectIcons.Free;
   inherited Destroy;
end;

// Notify
//
procedure TObjectManager.Notify(Sender: TPlugInManager; Operation: TOperation; PlugIn: Integer);
begin
end;

// FindSceneObjectClass
//
function TObjectManager.FindSceneObjectClass(AObjectClass: TGLSceneObjectClass;
                           const aSceneObject: String = '') : PSceneObjectEntry;
var
   I     : Integer;
   Found : Boolean;
begin
   Result:=nil;
   Found:=False;
   with FSceneObjectList do begin
      for I:=0 to Count-1 do
         with TGLSceneObjectEntry(Items[I]^) do
         if (ObjectClass = AObjectClass) and (Length(ASceneObject) = 0)
               or (CompareText(Name, ASceneObject) = 0) then begin
            Found:=True;
            Break;
         end;
      if Found then Result:=Items[I];
   end;
end;

// GetClassFromIndex
//
function TObjectManager.GetClassFromIndex(Index: Integer): TGLSceneObjectClass;
begin
   if Index<0 then
      Index:=0;
   if Index>FSceneObjectList.Count-1 then
      Index:=FSceneObjectList.Count-1;
  Result:=TGLSceneObjectEntry(FSceneObjectList.Items[Index+1]^).ObjectClass;
end;

// GetImageIndex
//
function TObjectManager.GetImageIndex(ASceneObject: TGLSceneObjectClass) : Integer;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.ImageIndex
   else Result:=0;
end;

// GetCategory
//
function TObjectManager.GetCategory(ASceneObject: TGLSceneObjectClass) : String;
var
   classEntry : PSceneObjectEntry;
begin
   classEntry:=FindSceneObjectClass(ASceneObject);
   if Assigned(classEntry) then
      Result:=classEntry^.Category
   else Result:='';
end;

// GetRegisteredSceneObjects
//
procedure TObjectManager.GetRegisteredSceneObjects(objectList: TStringList);
var
   i : Integer;
begin
   if Assigned(objectList) then with objectList do begin
      Clear;
      for i:=1 to FSceneObjectList.Count-1 do
         with TGLSceneObjectEntry(FSceneObjectList.Items[I]^) do
            AddObject(Name, Pointer(ObjectClass));
   end;
end;

// RegisterSceneObject
//
procedure TObjectManager.RegisterSceneObject(ASceneObject: TGLSceneObjectClass;
                                             const aName, aCategory : String);
var
   newEntry  : PSceneObjectEntry;
   pic       : TPicture;
   resBitmapName : String;
   bmp : TBitmap;
begin
   RegisterNoIcon([aSceneObject]);
   with FSceneObjectList do begin
      // make sure no class is registered twice
      if Assigned(FindSceneObjectClass(ASceneObject, AName)) then Exit;
      New(NewEntry);
      pic:=TPicture.Create;
      try
         with NewEntry^ do begin
            // object stock stuff
            // registered objects list stuff
            ObjectClass:=ASceneObject;
            NewEntry^.Name:=aName;
            NewEntry^.Category:=aCategory;
            Index:=FSceneObjectList.Count;
            resBitmapName:=ASceneObject.ClassName;
            Pic.Bitmap.Handle:=LoadBitmap(HInstance, PChar(resBitmapName));
            bmp:=TBitmap.Create;
            bmp.PixelFormat:=pf24bit;
            bmp.Width:=24; bmp.Height:=24;
            bmp.Canvas.Draw(0, 0, Pic.Bitmap);
            Pic.Bitmap:=bmp;
            bmp.Free;
            if Pic.Bitmap.Handle<>0 then begin
               FObjectIcons.AddMasked(Pic.Bitmap, Pic.Bitmap.Canvas.Pixels[0, 0]);
               ImageIndex:=FObjectIcons.Count-1;
            end else ImageIndex:=0;
		   end;
         Add(NewEntry);
      finally
         pic.Free;
      end;
   end;
end;

// UnRegisterSceneObject
//
procedure TObjectManager.UnRegisterSceneObject(ASceneObject: TGLSceneObjectClass);
var
   oldEntry : PSceneObjectEntry;
begin
   // find the class in the scene object list
   OldEntry:=FindSceneObjectClass(ASceneObject);
   // found?
   if assigned(OldEntry) then begin
	   // remove its entry from the list of registered objects
	   FSceneObjectList.Remove(OldEntry);
	   // finally free the memory for the entry
	   Dispose(OldEntry);
   end;
end;

// CreateDefaultObjectIcons
//
procedure TObjectManager.CreateDefaultObjectIcons;
var
   pic : TPicture;
begin
   pic:=TPicture.Create;
   // load first pic to get size
   pic.Bitmap.Handle:=LoadBitmap(HInstance, 'GLS_CROSS_16');
   FObjectIcons:=TImageList.CreateSize(Pic.Width, Pic.height);
   with FObjectIcons, pic.Bitmap.Canvas do begin
      try
         // There's a more direct way for loading images into the image list, but
         // the image quality suffers too much
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FOverlayIndex:=Count-1;
         Overlay(FOverlayIndex, 0); // used as indicator for disabled objects
         Pic.Bitmap.Handle:=LoadBitmap(HInstance, 'GLS_UNIVERSE2_16');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FSceneRootIndex:=Count-1;
         Pic.Bitmap.Handle:=LoadBitmap(HInstance, 'GLS_CAMERA2_16');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FCameraRootIndex:=Count-1;
         Pic.Bitmap.Handle:=LoadBitmap(HInstance, 'GLS_LAMPS2_16');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FLightsourceRootIndex:=Count-1;
         Pic.Bitmap.Handle:=LoadBitmap(HInstance, 'GLS_OBJECTS2_16');
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FObjectRootIndex:=Count-1;
         AddMasked(Pic.Bitmap, Pixels[0, 0]); FStockObjectRootIndex:=Count-1;
      finally
         Pic.Free;
      end;
   end;
end;

// DestroySceneObjectList
//
procedure TObjectManager.DestroySceneObjectList;
var
	i : Integer;
begin
	with FSceneObjectList do begin
		for i:=0 to Count-1 do
			Dispose(PSceneObjectEntry(Items[I]));
		Free;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TGLSceneViewerEditor.ExecuteVerb(Index: Integer);

var
  Source: TGLSceneViewer;

begin
  Source:=Component as TGLSceneViewer;
  case Index of
    0:
      Source.ShowInfo;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGLSceneViewerEditor.GetVerb(Index: Integer): string;

begin
  case Index of
    0:
      Result:='Show context info';
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

function TGLSceneViewerEditor.GetVerbCount: Integer;

begin
  Result:=1;
end;

//----------------- TGLSceneEditor -------------------------------------------------------------------------------------

procedure TGLSceneEditor.Edit;
begin
   with GLSceneEditorForm do begin
      SetScene(Self.Component as TGLScene, Self.Designer);
      Show;
   end;
end;

//----------------- TPlugInProperty ------------------------------------------------------------------------------------

procedure TPlugInProperty.Edit;
var
   Manager: TPlugInManager;
begin
   Manager:=TPlugInList(GetOrdValue).Owner;
   Manager.EditPlugInList;
end;

// GetAttributes
//
function TPlugInProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValues
//
function TPlugInProperty.GetValue: String;
begin
   Result:='registered : ' + IntToStr(TStringList(GetOrdValue).Count);
end;

//----------------- TResolutionProperty --------------------------------------------------------------------------------

function TResolutionProperty.GetAttributes: TPropertyAttributes;

begin
  Result:=[paValueList];
end;

//----------------------------------------------------------------------------------------------------------------------

function TResolutionProperty.GetValue : String;

begin
  Result:=VideoModes[GetOrdValue].Description;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TResolutionProperty.GetValues(Proc: TGetStrProc);

var
  I: Integer;

begin
  for I:=0 to NumberVideoModes-1 do Proc(VideoModes[I].Description);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TResolutionProperty.SetValue(const Value: String);

const Nums = ['0'..'9'];

var XRes,YRes,BPP : Integer;
    Pos, SLength  : Integer;
    TempStr       : String;

begin
  if CompareText(Value,'default') <> 0 then
  begin
    // initialize scanning
    TempStr:=Trim(Value)+'|'; // ensure at least one delimiter
    SLength:=Length(TempStr);
    XRes:=0; YRes:=0; BPP:=0;
    // contains the string something?
    if SLength > 1 then
    begin
      // determine first number
      for Pos:=1 to SLength do
        if not (TempStr[Pos] in Nums) then Break;
      if Pos <= SLength then
      begin
        // found a number?
        XRes:=StrToInt(Copy(TempStr,1,Pos-1));
        // search for following non-numerics
        for Pos:=Pos to SLength do
          if TempStr[Pos] in Nums then Break;
        Delete(TempStr,1,Pos-1); // take it out of the String
        SLength:=Length(TempStr); // rest length of String
        if SLength > 1 then // something to scan?
        begin
          // determine second number
          for Pos:=1 to SLength do
            if not (TempStr[Pos] in Nums) then Break;
          if Pos <= SLength then
          begin
            YRes:=StrToInt(Copy(TempStr,1,Pos-1));
            // search for following non-numerics
            for Pos:=Pos to SLength do
              if TempStr[Pos] in Nums then Break;
            Delete(TempStr,1,Pos-1); // take it out of the String
            SLength:=Length(TempStr); // rest length of String
            if SLength > 1 then
            begin
              for Pos:=1 to SLength do
                if not (TempStr[Pos] in Nums) then Break;
              if Pos <= SLength then BPP:=StrToInt(Copy(TempStr,1,Pos-1));
            end;
          end;
        end;
      end;
    end;
    SetOrdValue(GetIndexFromResolution(XRes,YRes,BPP));
  end
  else SetOrdValue(0);
end;
                           
//----------------- TGLTextureProperty -----------------------------------------

function TGLTextureProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties];
end;

//----------------- TGLTextureImageProperty ------------------------------------

// GetAttributes
//
function TGLTextureImageProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paDialog];
end;

// Edit
//
procedure TGLTextureImageProperty.Edit;
var
	ownerTexture : TGLTexture;
begin
	ownerTexture:=TGLTextureImage(GetOrdValue).OwnerTexture;
	if ownerTexture.Image.Edit then
		Designer.Modified;
end;

//----------------- TGLImageClassProperty --------------------------------------

// GetAttributes
//
function TGLImageClassProperty.GetAttributes: TPropertyAttributes;
begin
	Result:=[paValueList];
end;

// GetValues
//
procedure TGLImageClassProperty.GetValues(proc: TGetStrProc);
var
	i : Integer;
	sl : TStrings;
begin
	sl:=GetGLTextureImageClassesAsStrings;
	try
		for i:=0 to sl.Count-1 do proc(sl[i]);
	finally
		sl.Free;
	end;
end;

// GetValue
//
function TGLImageClassProperty.GetValue : String;
begin
	Result:=FindGLTextureImageClass(GetStrValue).FriendlyName;
end;

// SetValue
//
procedure TGLImageClassProperty.SetValue(const value : String);
var
	tic : TGLTextureImageClass;
begin
	tic:=FindGLTextureImageClassByFriendlyName(value);
	if Assigned(tic) then
		SetStrValue(tic.ClassName)
	else SetStrValue('');
	Modified;
end;

//----------------- TGLColorproperty -----------------------------------------------------------------------------------

procedure TGLColorProperty.Edit;
var
	colorDialog : TColorDialog;
   glColor : TGLColor;
begin
   colorDialog:=TColorDialog.Create(nil);
   try
      glColor:=TGLColor(GetOrdValue);
      colorDialog.Options:=[cdFullOpen];
      colorDialog.Color:=ConvertColorVector(glColor.Color);
      if colorDialog.Execute then begin
         glColor.Color:=ConvertWinColor(colorDialog.Color);
         Modified;
      end;
   finally
      colorDialog.Free;
   end;
end;

function TGLColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paSubProperties, paValueList, paDialog];
end;

procedure TGLColorProperty.GetValues(Proc: TGetStrProc);
begin
  ColorManager.EnumColors(Proc);
end;

function TGLColorProperty.GetValue: String;
begin
  Result:=ColorManager.GetColorName(TGLColor(GetOrdValue).Color);
end;

procedure TGLColorProperty.SetValue(const Value: string);
begin
  TGLColor(GetOrdValue).Color:=ColorManager.GetColor(Value);
  Modified;
end;

{$ifdef DFS_COMPILER_5_UP}
// Owner draw color values, only available in D5 and higher
procedure TGLColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);

   function ColorToBorderColor(AColor: TColorVector): TColor;
   begin
      if (AColor[0] > 0.75) or (AColor[1] > 0.75) or (AColor[2] > 0.75) then
         Result:=clBlack
      else if ASelected then
         Result:=clWhite
      else Result:=ConvertColorVector(AColor);
   end;

var
  vRight: Integer;
  vOldPenColor,
  vOldBrushColor: TColor;
  Color: TColorVector;
begin
  vRight:=(ARect.Bottom - ARect.Top) + ARect.Left;
  with ACanvas do
  try
    vOldPenColor:=Pen.Color;
    vOldBrushColor:=Brush.Color;

    Pen.Color:=Brush.Color;
    Rectangle(ARect.Left, ARect.Top, vRight, ARect.Bottom);

    Color:=ColorManager.GetColor(Value);
    Brush.Color:=ConvertColorVector(Color);
    Pen.Color:=ColorToBorderColor(Color);

    Rectangle(ARect.Left + 1, ARect.Top + 1, vRight - 1, ARect.Bottom - 1);

    Brush.Color:=vOldBrushColor;
    Pen.Color:=vOldPenColor;
  finally
    inherited ListDrawValue(Value, ACanvas, Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
  end;
end;

procedure TGLColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
   // draws the small color rectangle in the object inspector
   if GetVisualValue<>'' then
      ListDrawValue(GetVisualValue, ACanvas, ARect, True)
   else inherited PropDrawValue(ACanvas, ARect, ASelected);
end;
{$endif}

//----------------- TVectorFileProperty ----------------------------------------

// GetAttributes
//
function TVectorFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TVectorFileProperty.GetValue: String;
begin
   Result:=GetStrValue;
end;

// Edit
//
procedure TVectorFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   Component : TFreeForm;
   Desc, F    : String;
begin
   Component:=GetComponent(0) as TFreeForm;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetVectorFileFormats.BuildFilterStrings(TVectorFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         Component.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

// SetValue
//
procedure TVectorFileProperty.SetValue(const Value: string);
begin
   SetStrValue(Value);
end;

//----------------- TSoundFileProperty -----------------------------------------

// GetAttributes
//
function TSoundFileProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// GetValue
//
function TSoundFileProperty.GetValue: String;
var
   sample : TGLSoundSample;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   if sample.Data<>nil then
      Result:='('+sample.Data.ClassName+')'
   else Result:='(empty)';
end;

// Edit
//
procedure TSoundFileProperty.Edit;
var
   ODialog   : TOpenDialog;
   sample : TGLSoundSample;
   Desc, F    : String;
begin
   sample:=GetComponent(0) as TGLSoundSample;
   ODialog:=TOpenDialog.Create(nil);
   try
      GetGLSoundFileFormats.BuildFilterStrings(TGLSoundFile, Desc, F);
      ODialog.Filter:=Desc;
      if ODialog.Execute then begin
         sample.LoadFromFile(ODialog.FileName);
         Modified;
      end;
   finally
      ODialog.Free;
   end;
end;

//----------------- TSoundNameProperty -----------------------------------------

// GetAttributes
//
function TSoundNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paValueList];
end;

// GetValues
//
procedure TSoundNameProperty.GetValues(Proc: TGetStrProc);
var
   i : Integer;
   source : TGLBaseSoundSource;
begin
   source:=(GetComponent(0) as TGLBaseSoundSource);
   if Assigned(source.SoundLibrary) then with source.SoundLibrary do
      for i:=0 to Samples.Count-1 do Proc(Samples[i].Name);
end;

//----------------- TGLCoordinatesProperty -------------------------------------

// GetAttributes
//
function TGLCoordinatesProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit;
//
procedure TGLCoordinatesProperty.Edit;
var
   glc : TGLCoordinates;
   x, y, z : Single;
begin
   glc:=TGLCoordinates(GetOrdValue);
	x:=glc.x;
	y:=glc.y;
	z:=glc.z;
	if VectorEditorForm.Execute(x, y, z) then begin
		glc.AsVector:=VectorMake(x, y, z);
		Modified;
	end;
end;

{$ifdef DFS_DELPHI_5_UP}

//----------------- TGLMaterialProperty --------------------------------------------------------------------------------

// GetAttributes
//
function TGLMaterialProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog, paSubProperties];
end;

// Edit
//
procedure TGLMaterialProperty.Edit;
begin
	if MaterialEditorForm.Execute(TGLMaterial(GetOrdValue)) then
		Modified;
end;

//----------------- TReuseableDefaultEditor --------------------------------------------------------------------------------

// CheckEdit
//
procedure TReuseableDefaultEditor.CheckEdit(PropertyEditor: TPropertyEditor);
var
  FreeEditor: Boolean;
begin
  FreeEditor:=True;
  try
    if FContinue then EditProperty(PropertyEditor, FContinue, FreeEditor);
  finally
    if FreeEditor then PropertyEditor.Free;
  end;
end;

// EditProperty
//
procedure TReuseableDefaultEditor.EditProperty(PropertyEditor: TPropertyEditor;
															  var Continue, FreeEditor: Boolean);
var
  PropName: string;
  BestName: string;

  procedure ReplaceBest;
  begin
    FBest.Free;
	 FBest:=PropertyEditor;
    if FFirst = FBest then FFirst:=nil;
    FreeEditor:=False;
  end;

begin
  if not Assigned(FFirst) and (PropertyEditor is TMethodProperty) then
  begin
    FreeEditor:=False;
    FFirst:=PropertyEditor;
  end;
  PropName:=PropertyEditor.GetName;
  BestName:='';
  if Assigned(FBest) then BestName:=FBest.GetName;
  if CompareText(PropName, 'ONCREATE') = 0 then
    ReplaceBest
  else if CompareText(BestName, 'ONCREATE') <> 0 then
    if CompareText(PropName, 'ONCHANGE') = 0 then
		ReplaceBest
    else if CompareText(BestName, 'ONCHANGE') <> 0 then
      if CompareText(PropName, 'ONCLICK') = 0 then
        ReplaceBest;
end;

// Edit
//
procedure TReuseableDefaultEditor.Edit;
var
  Components: TDesignerSelectionList;
begin
  Components:=TDesignerSelectionList.Create;
  try
    FContinue:=True;
    Components.Add(Component);
    FFirst:=nil;
    FBest:=nil;
	 try
      GetComponentProperties(Components, tkAny, Designer, CheckEdit);
      if FContinue then
        if Assigned(FBest) then
          FBest.Edit
        else if Assigned(FFirst) then
          FFirst.Edit;
    finally
      FFirst.Free;
      FBest.Free;
	 end;
  finally
    Components.Free;
  end;
end;

//----------------- TGLMaterialLibraryEditor --------------------------------------------------------------------------------

// EditProperty
//
procedure TGLMaterialLibraryEditor.EditProperty(PropertyEditor: TPropertyEditor;
                                                var Continue, FreeEditor: Boolean);
begin
   if CompareText(PropertyEditor.GetName, 'MATERIALS') = 0 then begin
      FBest.Free;
      FBest:=PropertyEditor;
      FreeEditor:=False;
   end;
end;

//----------------- TGLLibMaterialNameProperty --------------------------------------------------------------------------------

// GetAttributes
//
function TGLLibMaterialNameProperty.GetAttributes: TPropertyAttributes;
begin
   Result:=[paDialog];
end;

// Edit
//
procedure TGLLibMaterialNameProperty.Edit;
var
   buf : String;
   ml : TGLMaterialLibrary;
   obj : TPersistent;
begin
	buf:=GetStrValue;
   obj:=GetComponent(0);
   if obj is TGLMaterial then
   	ml:=TGLMaterial(obj).MaterialLibrary
   else if obj is TGLLibMaterial then
      ml:=(TGLLibMaterials(TGLLibMaterial(obj).Collection).Owner as TGLMaterialLibrary)
   else begin
      ml:=nil;
      Assert(False, 'oops, unsupported...');
   end;
	if not Assigned(ml) then
		ShowMessage('Select the material library first.')
	else if LibMaterialPicker.Execute(buf, ml) then
		SetStrValue(buf);
end;

{$endif}

//----------------------------------------------------------------------------------------------------------------------

procedure Register;
begin
   RegisterComponents('GLScene',
                      [TGLScene, TGLSceneViewer, TGLMaterialLibrary, TGLCadencer,
                       TPlugInManager, TAsyncTimer, TCollisionManager,
                       TGLFireFXManager, TBitmapFont, TGLBitmapHDS,
                       TGLThorFXManager]);

   RegisterComponentEditor(TGLSceneViewer, TGLSceneViewerEditor);
   RegisterComponentEditor(TGLScene, TGLSceneEditor);
{$ifdef DFS_DELPHI_5_UP}
	RegisterComponentEditor(TGLMaterialLibrary, TGLMaterialLibraryEditor);
{$endif}

	RegisterPropertyEditor(TypeInfo(TPlugInList), TPlugInManager, 'PlugIns', TPlugInProperty);
	RegisterPropertyEditor(TypeInfo(TResolution), nil, '', TResolutionProperty);
	RegisterPropertyEditor(TypeInfo(TGLColor), nil, '', TGLColorProperty);
	RegisterPropertyEditor(TypeInfo(TGLTexture), TGLMaterial, '', TGLTextureProperty);
	RegisterPropertyEditor(TypeInfo(TGLTextureImage), TGLTexture, '', TGLTextureImageProperty);
	RegisterPropertyEditor(TypeInfo(String), TGLTexture, 'ImageClassName', TGLImageClassProperty);
	RegisterPropertyEditor(TypeInfo(TGLSoundFile), TGLSoundSample, '', TSoundFileProperty);
	RegisterPropertyEditor(TypeInfo(String), TGLBaseSoundSource, 'SoundName', TSoundNameProperty);
	RegisterPropertyEditor(TypeInfo(TGLCoordinates), nil, '', TGLCoordinatesProperty);
{$ifdef DFS_DELPHI_5_UP}
	RegisterPropertyEditor(TypeInfo(TGLMaterial), nil, '', TGLMaterialProperty);
	RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLMaterial, '', TGLLibMaterialNameProperty);
	RegisterPropertyEditor(TypeInfo(TGLLibMaterialName), TGLLibMaterial, '', TGLLibMaterialNameProperty);
{$endif}
   RegisterPropertyEditor(TypeInfo(TFileName), TFreeForm, 'FileName', TVectorFileProperty);

end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   with ObjectManager do begin
      RegisterSceneObject(TGLCamera, 'Camera', '');
      RegisterSceneObject(TGLLightSource, 'LightSource', '');
      RegisterSceneObject(TDummyCube, 'DummyCube', '');
      RegisterSceneObject(TSprite, 'Sprite', glsOCBasicGeometry);
      RegisterSceneObject(TLines, 'Lines', glsOCBasicGeometry);
      RegisterSceneObject(TPlane, 'Plane', glsOCBasicGeometry);
      RegisterSceneObject(TPolygon, 'Polygon', glsOCBasicGeometry);
      RegisterSceneObject(TCube, 'Cube', glsOCBasicGeometry);
      RegisterSceneObject(TFrustrum, 'Frustrum', glsOCBasicGeometry);
      RegisterSceneObject(TSphere, 'Sphere', glsOCBasicGeometry);
      RegisterSceneObject(TDisk, 'Disk', glsOCBasicGeometry);
      RegisterSceneObject(TCone, 'Cone', glsOCBasicGeometry);
      RegisterSceneObject(TCylinder, 'Cylinder', glsOCBasicGeometry);
      RegisterSceneObject(TDodecahedron, 'Dodecahedron', glsOCBasicGeometry);
      RegisterSceneObject(TAnnulus, 'Annulus', glsOCAdvancedGeometry);
      RegisterSceneObject(TMultiPolygon, 'MultiPolygon', glsOCAdvancedGeometry);
      RegisterSceneObject(TPipe, 'Pipe', glsOCAdvancedGeometry);
      RegisterSceneObject(TRevolutionSolid, 'RevolutionSolid', glsOCAdvancedGeometry);
      RegisterSceneObject(TTorus, 'Torus', glsOCAdvancedGeometry);
      RegisterSceneObject(TActor, 'Actor', glsOCMeshObjects);
      RegisterSceneObject(TFreeForm, 'FreeForm', glsOCMeshObjects);
      RegisterSceneObject(TMesh, 'Mesh', glsOCMeshObjects);
      RegisterSceneObject(TPortal, 'Portal', glsOCMeshObjects);
      RegisterSceneObject(THeightField, 'HeightField', glsOCProceduralObjects);
      RegisterSceneObject(TGLParticles, 'Particles', glsOCProceduralObjects);
      RegisterSceneObject(TArrowLine, 'ArrowLine', glsOCSpecialObjects);
      RegisterSceneObject(TSkyDome, 'SkyDome', glsOCSpecialObjects);
      RegisterSceneObject(TEarthSkyDome, 'EarthSkyDome', glsOCSpecialObjects);
      RegisterSceneObject(TSpaceText, 'SpaceText', glsOCSpecialObjects);
      RegisterSceneObject(TTerrainRenderer, 'TerrainRenderer', glsOCSpecialObjects);
      RegisterSceneObject(TTeapot, 'Teapot', glsOCSpecialObjects);
      RegisterSceneObject(TXYZGrid, 'XYZGrid', glsOCSpecialObjects);
      RegisterSceneObject(THUDSprite, 'HUDSprite', glsOCHUDObjects);
      RegisterSceneObject(THUDText, 'HUDText', glsOCHUDObjects);
      RegisterSceneObject(TDirectOpenGL, 'Direct OpenGL', '');
      RegisterSceneObject(TGLProxyObject, 'ProxyObject', '');

   end;

finalization

   ObjectManager.Free;

end.
