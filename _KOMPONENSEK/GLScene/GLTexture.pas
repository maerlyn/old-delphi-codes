{: GLTexture<p>

	Handles all the color and texture stuff.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>08/03/01 - Egg - TGLPicFileImage.GetBitmap32 now resets filename if not found
      <li>01/03/01 - Egg - Fixed TGLMaterial.DestroyHandle,
                           Added Texture2 notifications and material cacheing
      <li>26/02/01 - Egg - Added support for GL_EXT_texture_filter_anisotropic
      <li>23/02/01 - Egg - Fixed texture matrix messup (second was using first)
      <li>21/02/01 - Egg - Minor fix for TextureImageRequiredMemory,
                           TexGen calls now based on XOpenGL
      <li>14/02/01 - Egg - Added support for texture format & texture compression
      <li>31/01/01 - Egg - Added Multitexture support
      <li>28/01/01 - Egg - Added MaterialOptions
      <li>15/01/01 - Egg - Enhanced TGLPicFileImage.LoadFromFile
      <li>13/01/01 - Egg - New helper functions for TGLMaterialLibrary
      <li>08/01/01 - Egg - Not-so-clean fix for TGLTexture.Destroy... better fix
                           will require awareness of rendering contexts...
      <li>06/12/00 - Egg - Added PrepareBuildList mechanism
      <li>16/10/00 - Egg - Fix in TGLPictureImage.Assign
      <li>25/09/00 - Egg - New texture management implemented
      <li>13/08/00 - Egg - Added AddTextureMaterial
      <li>06/08/00 - Egg - File not found error now happens only once per texture,
                           also added some more doc and texture transforms support
                           to TGLLibMaterial
      <li>27/07/00 - Egg - TGLPictureImage.Assign now accepts TGraphic & TPicture,
                           Added max texture size clamping
      <li>15/07/00 - Egg - Upgrade for new list/handle destruction scheme
      <li>05/07/00 - Egg - Added tiaTopLeftPointColorTransparent
      <li>28/06/00 - Egg - Added asserts for missing texture files
      <li>01/06/00 - Egg - Added ReloadTexture (support for texture library),
                           Fixed persistence of material names in a texture library
      <li>28/05/00 - Egg - TGLColor now has NotifyChange support for TGLBaseSceneObject
      <li>23/04/00 - Egg - Fixed bugs with TGLPicFileImage & TGLPersistentImage,
                           Added tiaOpaque
		<li>17/04/00 - Egg - Added Assign to DummyCube and Sprite 
      <li>16/04/00 - Egg - Added TGLPicFileImage.Assign
      <li>26/03/00 - Egg - Finally fixed nasty bug in TGLMaterial.Free
		<li>22/03/00 - Egg - Added BeginUpdate/EndUpdate to TGLPictureImage,
									Made use of [Un]SetGLState in TGLMaterial
									(gain = 7-10% on T&L intensive rendering),
                           TGLTexBaseClass is no more (RIP)
		<li>21/03/00 - Egg - TGLMaterial props are now longer stored when it is
									linked to a material library entry,
									Added TGLPictureImage (split from TGLPersistentImage),
									TGLPicFileImage has been updated and reactivated,
									ColorManager is now autocreated and non longer force-linked.
      <li>19/03/00 - Egg - Added SaveToXxxx & LoadFromXxxx to TGLMaterialLibrary
		<li>18/03/00 - Egg - Added GetGLTextureImageClassesAsStrings,
									Added FindGLTextureImageClassByFriendlyName,
									FChanges states now ignored in TGLTexture.GetHandle,
									Added SaveToFile/LoadFromFile to TextureImage
		<li>17/03/00 - Egg - Added tiaLuminance
		<li>14/03/00 - Egg - Added RegisterGLTextureImageClass stuff,
									Added ImageAlpha
		<li>13/03/00 - Egg - Changed TGLTextureImage image persistence again,
									Added "Edit" method for texture image classes, 
									TMagFilter/TMinFilter -> TGLMagFilter/TGLMinFilter
		<li>03/03/00 - Egg - Removed TImagePath,
									Started major rework of the whole TGLTextureImage stuff,
									Fixed and optimized TGLTexture.PrepareImage
		<li>12/02/00 - Egg - Added Material Library
      <li>10/02/00 - Egg - Fixed crash when texture is empty
		<li>08/02/00 - Egg - Added AsWinColor & DeclareCurrentAsDefault to TGLColor,
									fixed notification on material property setXxx methods,
									Objects now begin with 'TGL'
		<li>07/02/00 - Egg - "Update"s renamed to "NotifyChange"s
		<li>06/02/00 - Egg - RoundUpToPowerOf2, RoundDownToPowerOf2 and
                           IsPowerOf2 moved to GLMisc, added TGLPersistentImage.Assign,
                           fixed TGLMaterial.Assign,
                           disable inheritance stuff in TGLFaceProperties.Apply (needs fixing),
                           Diffuse & ambient color now default to openGL values
      <li>05/02/00 - Egg - Javadocisation, fixes and enhancements :<br>
                           TGLColor.Update, ConvertWinColor, TPicImage,
									TGLMaterial.Apply
   </ul></font>
}
unit GLTexture;

// GLTexture   - This unit handles all the color and texture stuff.
// Version     - 0.5.1
// 07-JAN-2000 ml: minor changes in TGLColor
// 04-JAN-2000 ml: minor changes in TGLColor
// 30-DEC-99 ml: bug fixes

interface

uses
  Windows, Classes, OpenGL12, Graphics, Geometry, SysUtils, GLMisc, GLGraphics;

type

	PColorVector = ^TColorVector;
   TColorVector = TVector;

const // color definitions

      // Window's colors (must be filled at program
      // startup, since they depend on the desktop scheme)

      {$J+ - allow change of the following typed constants}

      clrScrollBar           : TColorVector = (0,0,0,1);
      clrBackground          : TColorVector = (0,0,0,1);
      clrActiveCaption       : TColorVector = (0,0,0,1);
      clrInactiveCaption     : TColorVector = (0,0,0,1);
      clrMenu                : TColorVector = (0,0,0,1);
      clrWindow              : TColorVector = (0,0,0,1);
      clrWindowFrame         : TColorVector = (0,0,0,1);
      clrMenuText            : TColorVector = (0,0,0,1);
      clrWindowText          : TColorVector = (0,0,0,1);
      clrCaptionText         : TColorVector = (0,0,0,1);
      clrActiveBorder        : TColorVector = (0,0,0,1);
      clrInactiveBorder      : TColorVector = (0,0,0,1);
      clrAppWorkSpace        : TColorVector = (0,0,0,1);
      clrHighlight           : TColorVector = (0,0,0,1);
      clrHighlightText       : TColorVector = (0,0,0,1);
      clrBtnFace             : TColorVector = (0,0,0,1);
      clrBtnShadow           : TColorVector = (0,0,0,1);
      clrGrayText            : TColorVector = (0,0,0,1);
      clrBtnText             : TColorVector = (0,0,0,1);
		clrInactiveCaptionText : TColorVector = (0,0,0,1);
      clrBtnHighlight        : TColorVector = (0,0,0,1);
      clr3DDkShadow          : TColorVector = (0,0,0,1);
      clr3DLight             : TColorVector = (0,0,0,1);
      clrInfoText            : TColorVector = (0,0,0,1);
      clrInfoBk              : TColorVector = (0,0,0,1);
      
      {$J- - disable change of other typed constants}

      // 'static' color definitions
      // sort of grays
      clrBlack               : TColorVector = (0,    0,    0,    1);
      clrGray05              : TColorVector = (0.05, 0.05, 0.05, 1);
      clrGray10              : TColorVector = (0.10, 0.10, 0.10, 1);
      clrGray15              : TColorVector = (0.15, 0.15, 0.15, 1);
      clrGray20              : TColorVector = (0.20, 0.20, 0.20, 1);
      clrGray25              : TColorVector = (0.25, 0.25, 0.25, 1);
      clrGray30              : TColorVector = (0.30, 0.30, 0.30, 1);
      clrGray35              : TColorVector = (0.35, 0.35, 0.35, 1);
      clrGray40              : TColorVector = (0.40, 0.40, 0.40, 1);
		clrGray45              : TColorVector = (0.45, 0.45, 0.45, 1);
      clrGray50              : TColorVector = (0.50, 0.50, 0.50, 1);
      clrGray55              : TColorVector = (0.55, 0.55, 0.55, 1);
      clrGray60              : TColorVector = (0.60, 0.60, 0.60, 1);
      clrGray65              : TColorVector = (0.65, 0.65, 0.65, 1);
      clrGray70              : TColorVector = (0.70, 0.70, 0.70, 1);
      clrGray75              : TColorVector = (0.75, 0.75, 0.75, 1);
      clrGray80              : TColorVector = (0.80, 0.80, 0.80, 1);
      clrGray85              : TColorVector = (0.85, 0.85, 0.85, 1);
      clrGray90              : TColorVector = (0.90, 0.90, 0.90, 1);
      clrGray95              : TColorVector = (0.95, 0.95, 0.95, 1);
      clrWhite               : TColorVector = (1,    1,    1,    1);

      // other grays
      clrDimGray             : TColorVector = (0.329412, 0.329412, 0.329412, 1);
      clrGray                : TColorVector = (0.752941, 0.752941, 0.752941, 1);
      clrLightGray           : TColorVector = (0.658824, 0.658824, 0.658824, 1);

      // colors en masse
      clrAquamarine          : TColorVector = (0.439216, 0.858824, 0.576471, 1);
		clrBlueViolet          : TColorVector = (0.62352,  0.372549, 0.623529, 1);
      clrBrown               : TColorVector = (0.647059, 0.164706, 0.164706, 1);
      clrCadetBlue           : TColorVector = (0.372549, 0.623529, 0.623529, 1);
      clrCoral               : TColorVector = (1,        0.498039, 0.0,      1);
      clrCornflowerBlue      : TColorVector = (0.258824, 0.258824, 0.435294, 1);
      clrDarkGreen           : TColorVector = (0.184314, 0.309804, 0.184314, 1);
      clrDarkOliveGreen      : TColorVector = (0.309804, 0.309804, 0.184314, 1);
      clrDarkOrchid          : TColorVector = (0.6,      0.196078, 0.8,      1);
      clrDarkSlateBlue       : TColorVector = (0.419608, 0.137255, 0.556863, 1);
      clrDarkSlateGray       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
      clrDarkSlateGrey       : TColorVector = (0.184314, 0.309804, 0.309804, 1);
      clrDarkTurquoise       : TColorVector = (0.439216, 0.576471, 0.858824, 1);
      clrFirebrick           : TColorVector = (0.556863, 0.137255, 0.137255, 1);
      clrForestGreen         : TColorVector = (0.137255, 0.556863, 0.137255, 1);
      clrGold                : TColorVector = (0.8,      0.498039, 0.196078, 1);
      clrGoldenrod           : TColorVector = (0.858824, 0.858824, 0.439216, 1);
      clrGreenYellow         : TColorVector = (0.576471, 0.858824, 0.439216, 1);
      clrIndian              : TColorVector = (0.309804, 0.184314, 0.184314, 1);
      clrKhaki               : TColorVector = (0.623529, 0.623529, 0.372549, 1);
      clrLightBlue           : TColorVector = (0.74902,  0.847059, 0.847059, 1);
		clrLightSteelBlue      : TColorVector = (0.560784, 0.560784, 0.737255, 1);
      clrLimeGreen           : TColorVector = (0.196078, 0.8,      0.196078, 1);
      clrMaroon              : TColorVector = (0.556863, 0.137255, 0.419608, 1);
      clrMediumAquamarine    : TColorVector = (0.196078, 0.8,      0.6,      1);
      clrMediumBlue          : TColorVector = (0.196078, 0.196078, 0.8,      1);
      clrMediumForestGreen   : TColorVector = (0.419608, 0.556863, 0.137255, 1);
      clrMediumGoldenrod     : TColorVector = (0.917647, 0.917647, 0.678431, 1);
      clrMediumOrchid        : TColorVector = (0.576471, 0.439216, 0.858824, 1);
      clrMediumSeaGreen      : TColorVector = (0.258824, 0.435294, 0.258824, 1);
      clrMediumSlateBlue     : TColorVector = (0.498039, 0,        1,        1);
      clrMediumSpringGreen   : TColorVector = (0.498039, 1,        0,        1);
      clrMediumTurquoise     : TColorVector = (0.439216, 0.858824, 0.858824, 1);
      clrMediumViolet        : TColorVector = (0.858824, 0.439216, 0.576471, 1);
      clrMidnightBlue        : TColorVector = (0.184314, 0.184314, 0.309804, 1);
      clrNavy                : TColorVector = (0.137255, 0.137255, 0.556863, 1);
      clrNavyBlue            : TColorVector = (0.137255, 0.137255, 0.556863, 1);
      clrOrange              : TColorVector = (1,        0.5,      0.0,      1);
      clrOrangeRed           : TColorVector = (1,        0.25,     0,        1);
      clrOrchid              : TColorVector = (0.858824, 0.439216, 0.858824, 1);
      clrPaleGreen           : TColorVector = (0.560784, 0.737255, 0.560784, 1);
		clrPink                : TColorVector = (0.737255, 0.560784, 0.560784, 1);
      clrPlum                : TColorVector = (0.917647, 0.678431, 0.917647, 1);
      clrSalmon              : TColorVector = (0.435294, 0.258824, 0.258824, 1);
      clrSeaGreen            : TColorVector = (0.137255, 0.556863, 0.419608, 1);
      clrSienna              : TColorVector = (0.556863, 0.419608, 0.137255, 1);
      clrSkyBlue             : TColorVector = (0.196078, 0.6,      0.8,      1);
      clrSlateBlue           : TColorVector = (0,        0.498039, 1,        1);
      clrSpringGreen         : TColorVector = (0,        1,        0.498039, 1);
      clrSteelBlue           : TColorVector = (0.137255, 0.419608, 0.556863, 1);
      clrTan                 : TColorVector = (0.858824, 0.576471, 0.439216, 1);
      clrThistle             : TColorVector = (0.847059, 0.74902,  0.847059, 1);
      clrTurquoise           : TColorVector = (0.678431, 0.917647, 0.917647, 1);
      clrViolet              : TColorVector = (0.309804, 0.184314, 0.309804, 1);
      clrVioletRed           : TColorVector = (0.8,      0.196078, 0.6,      1);
      clrWheat               : TColorVector = (0.847059, 0.847059, 0.74902,  1);
      clrYellowGreen         : TColorVector = (0.6,      0.8,      0.196078, 1);
      clrSummerSky           : TColorVector = (0.22,     0.69,     0.87,     1);
      clrRichBlue            : TColorVector = (0.35,     0.35,     0.67,     1);
      clrBrass               : TColorVector = (0.71,     0.65,     0.26,     1);
      clrCopper              : TColorVector = (0.72,     0.45,     0.20,     1);
		clrBronze              : TColorVector = (0.55,     0.47,     0.14,     1);
      clrBronze2             : TColorVector = (0.65,     0.49,     0.24,     1);
      clrSilver              : TColorVector = (0.90,     0.91,     0.98,     1);
      clrBrightGold          : TColorVector = (0.85,     0.85,     0.10,     1);
      clrOldGold             : TColorVector = (0.81,     0.71,     0.23,     1);
      clrFeldspar            : TColorVector = (0.82,     0.57,     0.46,     1);
      clrQuartz              : TColorVector = (0.85,     0.85,     0.95,     1);
      clrNeonPink            : TColorVector = (1.00,     0.43,     0.78,     1);
      clrDarkPurple          : TColorVector = (0.53,     0.12,     0.47,     1);
      clrNeonBlue            : TColorVector = (0.30,     0.30,     1.00,     1);
      clrCoolCopper          : TColorVector = (0.85,     0.53,     0.10,     1);
      clrMandarinOrange      : TColorVector = (0.89,     0.47,     0.20,     1);
      clrLightWood           : TColorVector = (0.91,     0.76,     0.65,     1);
      clrMediumWood          : TColorVector = (0.65,     0.50,     0.39,     1);
      clrDarkWood            : TColorVector = (0.52,     0.37,     0.26,     1);
      clrSpicyPink           : TColorVector = (1.00,     0.11,     0.68,     1);
      clrSemiSweetChoc       : TColorVector = (0.42,     0.26,     0.15,     1);
      clrBakersChoc          : TColorVector = (0.36,     0.20,     0.09,     1);
      clrFlesh               : TColorVector = (0.96,     0.80,     0.69,     1);
      clrNewTan              : TColorVector = (0.92,     0.78,     0.62,     1);
		clrNewMidnightBlue     : TColorVector = (0.00,     0.00,     0.61,     1);
		clrVeryDarkBrown       : TColorVector = (0.35,     0.16,     0.14,     1);
		clrDarkBrown           : TColorVector = (0.36,     0.25,     0.20,     1);
		clrDarkTan             : TColorVector = (0.59,     0.41,     0.31,     1);
		clrGreenCopper         : TColorVector = (0.32,     0.49,     0.46,     1);
		clrDkGreenCopper       : TColorVector = (0.29,     0.46,     0.43,     1);
		clrDustyRose           : TColorVector = (0.52,     0.39,     0.39,     1);
		clrHuntersGreen        : TColorVector = (0.13,     0.37,     0.31,     1);
		clrScarlet             : TColorVector = (0.55,     0.09,     0.09,     1);
		clrMediumPurple        : TColorVector = (0.73,     0.16,     0.96,     1);
		clrLightPurple         : TColorVector = (0.87,     0.58,     0.98,     1);
		clrVeryLightPurple     : TColorVector = (0.94,     0.81,     0.99,     1);
		clrGreen               : TColorVector = (0,        1,        0,        1);
		clrOlive               : TColorVector = (0,        1,        1,        1);
		clrPurple              : TColorVector = (1,        0,        1,        1);
		clrTeal                : TColorVector = (0,        1,        1,        1);
		clrRed                 : TColorVector = (1,        0,        0,        1);
		clrLime                : TColorVector = (0,        1,        0,        1);
		clrYellow              : TColorVector = (1,        1,        0,        1);
		clrBlue                : TColorVector = (0,        0,        1,        1);
		clrFuchsia             : TColorVector = (1,        0,        1,        1);
		clrAqua                : TColorVector = (0,        1,        1,        1);

type
	PRGBColor    = ^TRGBColor;
	TRGBColor    = TAffineByteVector;

	TGLTextureMode = (tmDecal, tmModulate, tmBlend, tmReplace);
	TGLTextureWrap = (twBoth, twNone, twVertical, twHorizontal);

	TGLFaceProperties  = class;
	TGLTexture         = class;
	TGLMaterial        = class;
   TGLMaterialLibrary = class;

   // TRenderContextInfo
   //
   {: Stores contextual info useful during rendering methods. }
   TRenderContextInfo = record
      objectsSorting : TGLObjectsSorting;
      cameraPosition : TVector;
      cameraDirection : TVector;
      rcci : TRenderContextClippingInfo;
      viewPortSize : TSize;
      currentStates : TGLStates;
      materialLibrary : TGLMaterialLibrary;
      fogDisabledCounter : Integer;
      proxySubObject : Boolean;
   end;
   PRenderContextInfo = ^TRenderContextInfo;

   // TGLColor
	//
   TGLColor = class(TGLUpdateAbleObject)
      private
			FColor, FDefaultColor : TColorVector;
			procedure SetColor(AColor: TColorVector);
			procedure SetColorComponent(Index: Integer; Value: TGLFloat);
			procedure SetAsWinColor(const val : TColor);
			function GetAsWinColor : TColor;

		protected
			procedure DefineProperties(Filer: TFiler); override;
			procedure ReadData(Stream: TStream);
			procedure WriteData(Stream: TStream);


		public
         { Public Properties }
			constructor Create(AOwner : TPersistent); override;
			constructor CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);

         procedure NotifyChange(Sender : TObject); override;
			procedure Assign(Source : TPersistent); override;
			procedure Initialize(const color : TColorVector);
			function AsAddress : PGLFloat;

			property Color : TColorVector read FColor write SetColor;
			property AsWinColor : TColor read GetAsWinColor write SetAsWinColor;

		published
			property Red:   TGLFloat index 0 read FColor[0] write SetColorComponent stored False;
			property Green: TGLFloat index 1 read FColor[1] write SetColorComponent stored False;
			property Blue:  TGLFloat index 2 read FColor[2] write SetColorComponent stored False;
			property Alpha: TGLFloat index 3 read FColor[3] write SetColorComponent stored False;
	end;

   // TTextureNeededEvent
   //
   TTextureNeededEvent = procedure (Sender : TObject; var textureFileName : String) of object;

	TGLTextureChange  = (tcImage, tcParams);
	TGLTextureChanges = set of TGLTextureChange;

	{: Defines how and if Alpha channel is defined for a texture image.<p>
		+ tiaDefault : uses the alpha channel in the image if any<br>
		+ tiaAlphaFromIntensity : the alpha channel value is deduced from other
			RGB components intensity (the brighter, the more opaque)<br>
		+ tiaSuperBlackTransparent : pixels with a RGB color of (0, 0, 0) are
			completely transparent, others are completely opaque<br>
		+ tiaLuminance : the luminance value is calculated for each pixel
			and used for RGB and Alpha values<br>
		+ tiaLuminanceSqrt : same as tiaLuminance but with an Sqrt(Luminance)<br>
      + tiaOpaque : alpha channel is uniformously set to 1.0<br>
      + tiaTopLeftPointColorTransparent : points of the same color as the
         top left point of the bitmap are transparent, others are opaque.<br>
	}
	TGLTextureImageAlpha = (tiaDefault, tiaAlphaFromIntensity,
									tiaSuperBlackTransparent, tiaLuminance,
									tiaLuminanceSqrt, tiaOpaque,
                           tiaTopLeftPointColorTransparent);

	// TGLTextureImage
	//
	{: Base class for texture image data.<p>
		Basicly, subclasses are to be considered as different ways of getting
		a HBitmap (interfacing the actual source).<br>
		SubClasses should be registered using RegisterGLTextureImageClass to allow
		proper persistence and editability in the IDE experts. }
	TGLTextureImage = class(TGLUpdateAbleObject)
		private
			FOwnerTexture : TGLTexture;
         FOnTextureNeeded : TTextureNeededEvent;

		protected
			function GetHeight: Integer; virtual; abstract;
			function GetWidth: Integer; virtual; abstract;

         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;

		public
         { Public Properties }
			constructor Create(AOwner : TPersistent); override;
			destructor Destroy; override;

			property OwnerTexture : TGLTexture read FOwnerTexture write FOwnerTexture;
			procedure NotifyChange(Sender : TObject); override;

			{: Request to edit the textureImage.<p>
				Returns True if changes have been made.<br>
				This method may be invoked from the IDE or at run-time. }
			function Edit : Boolean; dynamic; abstract;
			{: Save textureImage to file.<p>
				This may not save a picture, but for instance, parameters, if the
				textureImage is a procedural texture. }
			procedure SaveToFile(const fileName : String); dynamic; abstract;
			{: Load textureImage from a file.<p>
				This may not load a picture, but for instance, parameters, if the
				textureImage is a procedural texture.<br>
            Subclasses should invoke inherited which will take care of the
            "OnTextureNeeded" stuff. }
			procedure LoadFromFile(const fileName : String); dynamic;
			{: Returns a user-friendly denomination for the class.<p>
				This denomination is used for picking a texture image class
				in the IDE expert. }
			class function FriendlyName : String; virtual; abstract;
			{: Returns a user-friendly description for the class.<p>
				This denomination is used for helping the user when picking a
				texture image class in the IDE expert. If it's not overriden,
				takes its value from FriendlyName. }
			class function FriendlyDescription : String; virtual;

			{: Request reload/refresh of data upon next use. }
			procedure Invalidate; dynamic;

			{: Returns image's bitmap handle.<p>
				If the actual image is not a windows bitmap (BMP), descendants should
				take care of properly converting to bitmap. }
			function GetBitmap32: TGLBitmap32; virtual; abstract;
			{: Request for unloading bitmapData, to free some memory.<p>
				This one is invoked when GLScene no longer needs the Bitmap data
				it got through a call to GetHBitmap.<br>
				Subclasses may ignore this call if the HBitmap was obtained at
				no particular memory cost. }
			procedure ReleaseBitmap32; virtual;

			property Width: Integer read GetWidth;
			property Height: Integer read GetHeight;
	end;

	TGLTextureImageClass = class of TGLTextureImage;

	// TGLPictureImage
	//
	{: Base class for image data classes internally based on a TPicture ;}
	TGLPictureImage = class(TGLTextureImage)
		private
			FBitmap : TGLBitmap32;
			FPicture : TPicture;
			FUpdateCounter : Integer;

		protected
			function GetHeight: Integer; override;
			function GetWidth: Integer; override;

			procedure SetPicture(const aPicture : TPicture);
			procedure PictureChanged(Sender: TObject);

		public
         { Public Properties }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			procedure Assign(Source: TPersistent); override;

			{: Use this function if you are going to modify the Picture directly.<p>
				Each invokation MUST be balanced by a call to EndUpdate. }
			procedure BeginUpdate;
			procedure EndUpdate;
			function GetBitmap32 : TGLBitmap32; override;
			procedure ReleaseBitmap32; override;

			property Picture : TPicture read FPicture write SetPicture;
	end;

	// TGLPersistentImage
	//
	{: Stores any image compatible with Delphi's TPicture mechanism.<p>
		The picture's data is actually stored into the DFM, the original
		picture name or path is not remembered. It is similar in behaviour
		to Delphi's TImage.<p>
		Note that if original image is for instance JPEG format, only the JPEG
		data will be stored in the DFM (compact) }
	TGLPersistentImage = class(TGLPictureImage)
		public
         { Public Properties }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

			function Edit : Boolean; override;
			procedure SaveToFile(const fileName : String); override;
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

		published
         { Published Properties }
			property Picture;
	end;

	// TGLPicFileImage
	//
	{: Uses a picture whose data is found in a file (only filename is stored). }
	TGLPicFileImage = class(TGLPictureImage)
		private
			FPictureFileName : String;

		protected
			procedure SetPictureFileName(const val : String);

		public
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

  			procedure Assign(Source: TPersistent); override;

			function Edit : Boolean; override;
			//: Only picture file name is saved
			procedure SaveToFile(const fileName : String); override;
         {: Load picture file name or use fileName as picture filename.<p>
            The autodetection is based on the filelength and presence of zeros. }
			procedure LoadFromFile(const fileName : String); override;
			class function FriendlyName : String; override;
			class function FriendlyDescription : String; override;

			function GetBitmap32 : TGLBitmap32; override;
			procedure Invalidate; override;

		published
			property PictureFileName : String read FPictureFileName write SetPictureFileName;
	end;

	{ Disabled stuff


	  TGLCaptureImage = class(TGLTextureImage)
	  private
		 FBitmap : TBitmap;
		 FLeft, FTop : Integer;
	  protected
		 function GetBitmap: HBitmap; override;
		 function GetHeight: Integer; override;
		 function GetWidth: Integer; override;
		 procedure PictureChanged(Sender: TObject);
		 procedure SetHeight(AValue: Integer); override;
		 procedure SetLeft(AValue: Integer);
		 procedure SetTop(AValue: Integer);
		 procedure SetWidth(AValue: Integer); override;
	  public
		 constructor Create(AOwner: TPersistent); override;
		 destructor Destroy; override;
		 procedure DataNeeded; override;
	  published
		 property Left: Integer read FLeft write SetLeft default 32;
		 property Top: Integer read FTop write SetTop default 32;
	  end;
	  }

   TGLLibMaterial = Class;

   // TGLTextureFormat
   //
   {: Texture format for OpenGL (rendering) use.<p>
      Internally, GLScene handles all "base" images as 32 Bits RGBA, but you can
      specify a generic format to reduce OpenGL texture memory use:<ul>
      <li>tfDefault : uses global default format
      <li>tfRGB : 24 bits RGB, 8 bits per component
      <li>tfRGBA : 32 bits RGBA, 8 bits per component
      <li>tfRGB16 : 16 bits RGB (5, 5, 5)
      <li>tfRGBA16 : 16 bits RGBA (4, 4, 4, 4)
      <li>tfAlpha : 8 Bits Alpha-channel only
      <li>tfLuminance : 8 bits Luminance only
      <li>tfLuminanceAlpha : 16 bits Luminance and Alpha channel (8, 8)
      <li>tfIntensity : 8 bits Intensity only
      </ul><br>The actual representation may differ, f.i. old 16bits boards
      will convert everything to 16bit formats, GeForce boards don't have
      a 24 bits format and will convert to 32 bits, etc. }
   TGLTextureFormat = (tfDefault, tfRGB, tfRGBA, tfRGB16, tfRGBA16, tfAlpha,
                       tfLuminance, tfLuminanceAlpha, tfIntensity);

   // TGLTextureCompression
   //
   {: Texture compression option.<p>
      If OpenGL supports it, this will activate a compressed texture format:<ul>
      <li>tcDefault : uses global default compression option
      <li>tcNone : do not use compression
      <li>tcStandard : use standard compression, average quality, average rate
      <li>tcHighQuality : choose a high-quality, low-speed compression
      <li>tcHighSpeed : choose a high-speed, low-quality compression
      </ul>. }
   TGLTextureCompression = (tcDefault, tcNone, tcStandard, tcHighQuality, tcHighSpeed);

   // TGLTextureFilteringQuality
   //
   TGLTextureFilteringQuality = (tfIsotropic, tfAnisotropic);

	// TGLTexture
	//
   {: Defines basic texturing properties.<p>
      You can control texture wrapping, smoothing/filtering and of course define
      the texture map (note that texturing is disabled by default).<p>
      A built-in mechanism (through ImageAlpha) allows auto-generation of an
      Alpha channel for all bitmaps (see TGLTextureImageAlpha). }
	TGLTexture = class (TGLUpdateAbleObject)
		private
			FHandle      : TGLuint;
			FTextureMode : TGLTextureMode;
			FTextureWrap : TGLTextureWrap;
         FTextureFormat : TGLTextureFormat;
			FMinFilter   : TGLMinFilter;
			FMagFilter   : TGLMagFilter;
			FChanges     : TGLTextureChanges;
			FDisabled    : Boolean;
			FImage       : TGLTextureImage;
			FImageAlpha  : TGLTextureImageAlpha;
         FOnTextureNeeded : TTextureNeededEvent;
         FLastRC, FLastDC : Integer;
         FCompression : TGLTextureCompression;
         FRequiredMemorySize : Integer;
         FFilteringQuality : TGLTextureFilteringQuality;

		protected
			procedure SetImage(AValue: TGLTextureImage);
			procedure SetImageAlpha(const val : TGLTextureImageAlpha);
			procedure SetMagFilter(AValue: TGLMagFilter);
			procedure SetMinFilter(AValue: TGLMinFilter);
			procedure SetTextureMode(AValue: TGLTextureMode);
			procedure SetTextureWrap(AValue: TGLTextureWrap);
         procedure SetTextureFormat(const val : TGLTextureFormat);
         procedure SetCompression(const val : TGLTextureCompression);
         procedure SetFilteringQuality(const val : TGLTextureFilteringQuality);
			procedure SetDisabled(AValue: Boolean);

         function StoreImageClassName : Boolean;

			function GetHandle: TGLuint; virtual;
			//: Load texture to OpenGL subsystem
			procedure PrepareImage; virtual;
			//: Setup OpenGL texture parameters
			procedure PrepareParams; virtual;

         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;

		public
			constructor Create(AOwner: TPersistent); override;
			destructor  Destroy; override;

         procedure PrepareBuildList;
			procedure Apply(var currentStates : TGLStates);
         procedure ApplyAsTexture2(libMaterial : TGLLibMaterial);
         procedure UnApplyAsTexture2(libMaterial : TGLLibMaterial);

			procedure Assign(Source: TPersistent); override;

			procedure DestroyHandle(glsceneOnly : Boolean);
			procedure DisableAutoTexture;
			procedure InitAutoTexture(const texRep : TTexPoint); overload;
			procedure InitAutoTexture(texRep : PTexPoint); overload;

			procedure SetImageClassName(const val : String);
			function GetImageClassName : String;

         {: Returns the OpenGL memory used by the texture.<p>
            The compressed size is returned if, and only if texture compression
            if active and possible, and the texture has been allocated (Handle
            is defined), otherwise the estimated size (from TextureFormat
            specification) is returned. }
         function TextureImageRequiredMemory : Integer;

			property  Handle: TGLuint read GetHandle;

		published

			{: Image ClassName for enabling True polymorphism.<p>
				This is ugly, but since the default streaming mechanism does a
				really bad job at storing	polymorphic owned-object properties,
				and neither TFiler nor TPicture allow proper use of the built-in
				streaming, that's the only way I found to allow a user-extensible
				mechanism. }
			property ImageClassName : String read GetImageClassName write SetImageClassName stored StoreImageClassName;
			{: Image data for the texture.<p> }
			property Image: TGLTextureImage read FImage write SetImage;

			property ImageAlpha : TGLTextureImageAlpha read FImageAlpha write SetImageAlpha default tiaDefault;

			property MagFilter: TGLMagFilter read FMagFilter write SetMagFilter default maNearest;
			property MinFilter: TGLMinFilter read FMinFilter write SetMinFilter default miNearest;

			property TextureMode: TGLTextureMode read FTextureMode write SetTextureMode default tmDecal;
			property TextureWrap: TGLTextureWrap read FTextureWrap write SetTextureWrap default twBoth;
         {: Texture format for use by the renderer.<p>
            See TGLTextureFormat for details. }
         property TextureFormat : TGLTextureFormat read FTextureFormat write SetTextureFormat default tfDefault;
         {: Texture compression control.<p>
            If True the compressed TextureFormat variant (the OpenGL ICD must
            support GL_ARB_texture_compression, or this option is ignored). }
         property Compression : TGLTextureCompression read FCompression write SetCompression default tcDefault;
         {: Specifies texture filtering quality.<p>
            You can choose between bilinear and trilinear filetring (anisotropic).<p>
            The OpenGL ICD must support GL_EXT_texture_filter_anisotropic or
            this property is ignored. }
         property FilteringQuality : TGLTextureFilteringQuality read FFilteringQuality write SetFilteringQuality default tfIsotropic;

			property Disabled: Boolean read FDisabled write SetDisabled default True;
	end;

	TShininess = 0..128;
   TPolygonMode = (pmFill, pmLines, pmPoints);

   // TGLFaceProperties
   //
   {: Stores basic face lighting properties.<p>
      The lighting is described with the standard ambient/diffuse/emission/specular
      properties that behave like those of most rendering tools.<br>
      You also have control over shininess (governs specular lighting) and
      polygon mode (lines / fill). } 
	TGLFaceProperties = class (TGLUpdateAbleObject)
	   private
         FAmbient, FDiffuse, FSpecular, FEmission  : TGLColor;
         FPolygonMode : TPolygonMode;
         FShininess : TShininess;

      protected
         procedure SetAmbient(AValue: TGLColor);
         procedure SetDiffuse(AValue: TGLColor);
         procedure SetEmission(AValue: TGLColor);
         procedure SetSpecular(AValue: TGLColor);
         procedure SetPolygonMode(AValue: TPolygonMode);
         procedure SetShininess(AValue: TShininess);

	   public
         constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;
         procedure Apply(AFace: TGLEnum);
         procedure Assign(Source: TPersistent); override;

      published
         property Ambient: TGLColor read FAmbient write SetAmbient;
         property Diffuse: TGLColor read FDiffuse write SetDiffuse;
         property Emission: TGLColor read FEmission write SetEmission;
         property Shininess: TShininess read FShininess write SetShininess default 0;
         property PolygonMode: TPolygonMode read FPolygonMode write SetPolygonMode default pmFill;
         property Specular: TGLColor read FSpecular write SetSpecular;
   end;

   TGLLibMaterialName = String;

   // TBlendingMode
   //
   {: Simplified blending options.<p>
      bmOpaque : disable blending<br>
      bmTransparency : uses standard alpha blending<br>
      bmAdditive : activates additive blending (with saturation) }
   TBlendingMode = (bmOpaque, bmTransparency, bmAdditive);

   // TMaterialOptions
   //
   {: Control special rendering options for a material.<p>
      moIgnoreFog : fog is deactivated when the material is rendered }
   TMaterialOption = (moIgnoreFog);
   TMaterialOptions = set of TMaterialOption;

	// TGLMaterial
   //
   {: Describes a rendering material.<p>
      A material is basicly a set of face properties (front and back) that take
      care of standard material rendering parameters (diffuse, ambient, emission
      and specular) and texture mapping.<br>
      An instance of this class is available for almost all objects in GLScene
      to allow quick definition of material properties. It can link to a
      TGLLibMaterial (taken for a material library).<p>
      The TGLLibMaterial has more adavanced properties (like texture transforms)
      and provides a standard way of sharing definitions and texture maps. }
	TGLMaterial = class (TGLUpdateAbleObject)
      private
	      { Private Declarations }
         FFrontProperties, FBackProperties : TGLFaceProperties;
			FBlendingMode : TBlendingMode;
         FTexture : TGLTexture;
         FMaterialLibrary : TGLMaterialLibrary;
         FLibMaterialName : TGLLibMaterialName;
         FMaterialOptions : TMaterialOptions;
         currentLibMaterial : TGLLibMaterial;

	   protected
	      { Protected Declarations }
         procedure SetBackProperties(Values: TGLFaceProperties);
         procedure SetFrontProperties(Values: TGLFaceProperties);
         procedure SetBlendingMode(const val : TBlendingMode);
         procedure SetMaterialOptions(const val : TMaterialOptions);
         procedure SetTexture(ATexture: TGLTexture);
         procedure SetMaterialLibrary(const val : TGLMaterialLibrary);
         procedure SetLibMaterialName(const val : TGLLibMaterialName);

			procedure NotifyLibMaterialDestruction;
			//: Back, Front, Texture and blending not stored if linked to a LibMaterial
			function StoreMaterialProps : Boolean;

		public
			{ Public Declarations }
			constructor Create(AOwner: TPersistent); override;
			destructor Destroy; override;

         procedure PrepareBuildList;
			procedure Apply(var rci : TRenderContextInfo);
         //: Restore non-standard material states that were altered
         procedure UnApply(var rci : TRenderContextInfo);
			procedure Assign(Source: TPersistent); override;
			procedure NotifyChange(Sender : TObject); override;
         procedure DestroyHandle(glsceneOnly : Boolean);

		published
			{ Published Declarations }
			property BackProperties: TGLFaceProperties read FBackProperties write SetBackProperties stored StoreMaterialProps;
			property FrontProperties: TGLFaceProperties read FFrontProperties write SetFrontProperties stored StoreMaterialProps;
			property BlendingMode : TBlendingMode read FBlendingMode write SetBlendingMode stored StoreMaterialProps default bmOpaque;
         property MaterialOptions : TMaterialOptions read FMaterialOptions write SetMaterialOptions;
			property Texture: TGLTexture read FTexture write SetTexture stored StoreMaterialProps;

			property MaterialLibrary : TGLMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
			property LibMaterialName : TGLLibMaterialName read FLibMaterialName write SetLibMaterialName;
	  end;

	// TGLLibMaterial
	//
   {: Material in a material library.<p>
      Introduces Texture transformations (offset and scale). Those transformations
      are available only for lib materials to minimize the memory cost of basic
      materials (which are used in almost all objects). }
	TGLLibMaterial = class (TCollectionItem)
	   private
	      { Private Declarations }
         userList : TList;
         FName : TGLLibMaterialName;
         FMaterial : TGLMaterial;
         FTextureOffset, FTextureScale : TGLCoordinates;
         FTextureMatrixIsIdentity : Boolean;
         FTextureMatrix : TMatrix;
         FTexture2Name : TGLLibMaterialName;
         notifying : Boolean; // used for recursivity protection
         libMatTexture2 : TGLLibMaterial; // internal cache

	   protected
	      { Protected Declarations }
         function GetDisplayName : String; override;
         procedure Loaded;

         procedure SetName(const val : TGLLibMaterialName);
         procedure SetMaterial(const val : TGLMaterial);
         procedure SetTextureOffset(const val : TGLCoordinates);
         procedure SetTextureScale(const val : TGLCoordinates);
         procedure SetTexture2Name(const val : TGLLibMaterialName);

         procedure CalculateTextureMatrix;
         procedure DestroyHandle(glsceneOnly : Boolean);
         procedure OnNotifyChange(Sender : TObject);
         procedure DoOnTextureNeeded(Sender : TObject; var textureFileName : String);

      public
	      { Public Declarations }
	      constructor Create(Collection : TCollection); override;
	      destructor Destroy; override;

	      procedure Assign(Source: TPersistent); override;

         procedure PrepareBuildList;
			procedure Apply(var rci : TRenderContextInfo);
         //: Restore non-standard material states that were altered
         procedure UnApply(var rci : TRenderContextInfo);

         procedure RegisterUser(material : TGLMaterial); overload;
			procedure UnregisterUser(material : TGLMaterial); overload;
         procedure RegisterUser(libMaterial : TGLLibMaterial); overload;
			procedure UnregisterUser(libMaterial : TGLLibMaterial); overload;
         procedure NotifyUsers;

	   published
	      { Published Declarations }
         property Name : TGLLibMaterialName read FName write SetName;
         property Material : TGLMaterial read FMaterial write SetMaterial;
         {: Texture offset in texture coordinates.<p>
            The offset is applied <i>after</i> scaling. }
         property TextureOffset : TGLCoordinates read FTextureOffset write SetTextureOffset;
         {: Texture coordinates scaling.<p>
            Scaling is applied <i>before</i> applying the offset, and is applied
            to the texture coordinates, meaning that a scale factor of (2, 2, 2)
            will make your texture look twice <i>smaller</i>. }
         property TextureScale : TGLCoordinates read FTextureScale write SetTextureScale;

         {: Reference to the second texture.<p>
            The referred LibMaterial *must* be in the same material library.<p>
            Second textures are supported only through ARB multitexturing (ignored
            if not supported). }
         property Texture2Name : TGLLibMaterialName read FTexture2Name write SetTexture2Name;

	end;

	// TGLLibMaterials
	//
   {: A collection of materials, mainly used in material libraries. }
	TGLLibMaterials = class (TCollection)
	   private
	      { Protected Declarations }
	      FOwner : TComponent;

	   protected
	      { Protected Declarations }
	      function GetOwner: TPersistent; override;
         procedure Loaded;

         procedure SetItems(index : Integer; const val : TGLLibMaterial);
	      function GetItems(index : Integer) : TGLLibMaterial;
         procedure DestroyHandles(glsceneOnly : Boolean);

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent);

         property Owner : TComponent read FOwner;

         function Add: TGLLibMaterial;
	      function FindItemID(ID: Integer): TGLLibMaterial;
	      property Items[index : Integer] : TGLLibMaterial read GetItems write SetItems; default;
         function MakeUniqueName(const nameRoot : TGLLibMaterialName) : TGLLibMaterialName;
         function GetLibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;
         procedure SetNamesToTStrings(aStrings : TStrings);
   end;

   // TGLMaterialLibrary
   //
   {: Stores a set of materials, to be used and shared by scene objects.<p>
      Use a material libraries for storing commonly used materials, it provides
      an efficient way to share texture and material data among many objects,
      thus reducing memory needs and rendering time.<p>
      Materials in a material library also feature advanced control properties
      like texture coordinates transforms. }  
   TGLMaterialLibrary = class (TGLCadenceAbleComponent)
	   private
	      { Protected Declarations }
         FMaterials : TGLLibMaterials;
         FTexturePaths : String;
         FOnTextureNeeded : TTextureNeededEvent;
         FTexturePathList : TStringList;
         FLastAppliedMaterial : TGLLibMaterial;

	   protected
			{ Protected Declarations }
         procedure Loaded; override;
         procedure SetMaterials(const val : TGLLibMaterials);
         procedure SetTexturePaths(const val : String);

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;
         procedure DestroyHandles(glsceneOnly : Boolean);

	      procedure SaveToStream(aStream : TStream); dynamic;
	      procedure LoadFromStream(aStream : TStream); dynamic;
         {: Recommended extension : .GLL }
	      procedure SaveToFile(const fileName : String);
	      procedure LoadFromFile(const fileName : String);

         {: Add a "standard" texture material.<p>
            "standard" means linear texturing mode with mipmaps and texture
            modulation mode with default-strength color components. }
         function AddTextureMaterial(const materialName, fileName : String) : TGLLibMaterial;
         {: Applies the material of given name.<p>
            Returns False if the material could not be found. ake sure this
            call is balanced with a corresponding UnApplyMaterial (or an
            assertion will be triggered in the destructor).<br>
            If a material is already applied, and has not yet been unapplied,
            an assertion will be triggered. }
         function ApplyMaterial(const materialName : String; var rci : TRenderContextInfo) : Boolean;
         {: Un-applies the last applied material.<p>
            Use this function in conjunction with ApplyMaterial.<br>
            If no material was applied, an assertion will be triggered. }
         procedure UnApplyMaterial(var rci : TRenderContextInfo);

      published
	      { Published Declarations }
         {: The materials collection. }
         property Materials : TGLLibMaterials read FMaterials write SetMaterials;
         {: Paths to lookup when attempting to load a texture.<p>
            You can specify multiple paths when loading a texture, the separator
            being the semi-colon ';' character. Directories are looked up from
            first to last, the first file name match is used.<br>
            The current directory is always implicit and checked last.<p>
            Note that you can also use the OnTextureNeeded event to provide a
            filename. }
         property TexturePaths : String read FTexturePaths write SetTexturePaths;
         {: This event is fired whenever a texture needs to be loaded from disk.<p>
            The event is triggered before even attempting to load the texture,
            and before TexturePaths is used. }
         property OnTextureNeeded : TTextureNeededEvent read FOnTextureNeeded write FOnTextureNeeded;

   end;

     PColorEntry = ^TColorEntry;
	  TColorEntry = record
                     Name  : String[31];
                     Color : TColorVector;
                   end;

     TGLColorManager = class(TList)
     public
       destructor Destroy; override;
       procedure AddColor(AName: String; AColor: TColorVector);
       procedure EnumColors(Proc: TGetStrProc);
       function  FindColor(AName: String): TColorVector;
       {: Convert a clrXxxx or a '<red green blue alpha> to a color vector }
		 function  GetColor(AName: String): TColorVector;
       function  GetColorName(AColor: TColorVector): String;
       procedure RegisterDefaultColors;
       procedure RemoveColor(AName: String);
     end;

function ColorManager: TGLColorManager;

//: Converts a color vector (containing float values)
function ConvertColorVector(AColor: TColorVector): TColor;
//: Converts RGB components into a color vector with correct range
function ConvertRGBColor(AColor: array of Byte): TColorVector;
//: Converts a delphi color into its RGB fragments and correct range
function ConvertWinColor(aColor: TColor; alpha : Single = 1) : TColorVector;
procedure RegisterColor(AName: String; AColor: TColorVector);
procedure UnregisterColor(AName: String);

//: Register a TGLTextureImageClass (used for persistence and IDE purposes)
procedure RegisterGLTextureImageClass(textureImageClass : TGLTextureImageClass);
//: Finds a registerer TGLTextureImageClass using its classname
function FindGLTextureImageClass(const className : String) : TGLTextureImageClass;
//: Finds a registerer TGLTextureImageClass using its FriendlyName
function FindGLTextureImageClassByFriendlyName(const friendlyName : String) : TGLTextureImageClass;
//: Defines a TStrings with the list of registered TGLTextureImageClass.
procedure SetGLTextureImageClassesToStrings(aStrings : TStrings);
{: Creates a TStrings with the list of registered TGLTextureImageClass.<p>
	To be freed by caller. }
function GetGLTextureImageClassesAsStrings : TStrings;

// Global texturing defaults
//
var
   vDefaultTextureFormat : TGLTextureFormat = tfRGBA;
   vDefaultTextureCompression : TGLTextureCompression = tcNone;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses Dialogs, GLScene, GLScreen, GLStrings, ExtDlgs, XOpenGL;

var
	vGLTextureImageClasses : TList;
	vColorManager: TGLColorManager;

const
	cTextureMode : array [tmDecal..tmReplace] of TGLEnum =
							( GL_DECAL, GL_MODULATE, GL_BLEND, GL_REPLACE );
   
// ColorManager
//
function ColorManager : TGLColorManager;
begin
	if not Assigned(vColorManager) then begin
		vColorManager:=TGLColorManager.Create;
		vColorManager.RegisterDefaultColors;
	end;
	Result:=vColorManager;
end;

// RegisterGLTextureImageClass
//
procedure RegisterGLTextureImageClass(textureImageClass : TGLTextureImageClass);
begin
	if not Assigned(vGLTextureImageClasses) then
		vGLTextureImageClasses:=TList.Create;
	vGLTextureImageClasses.Add(textureImageClass);
end;

// FindGLTextureImageClass
//
function FindGLTextureImageClass(const className : String) : TGLTextureImageClass;
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	Result:=nil;
	if Assigned(vGLTextureImageClasses) then
		for i:=0 to vGLTextureImageClasses.Count-1 do begin
			tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
			if tic.ClassName=className then begin
				Result:=tic;
				Break;
			end;
		end;

end;

// FindGLTextureImageClassByFriendlyName
//
function FindGLTextureImageClassByFriendlyName(const friendlyName : String) : TGLTextureImageClass;
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	Result:=nil;
	if Assigned(vGLTextureImageClasses) then
		for i:=0 to vGLTextureImageClasses.Count-1 do begin
			tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
			if tic.FriendlyName=friendlyName then begin
				Result:=tic;
				Break;
			end;
		end;
end;

// SetGLTextureImageClassesToStrings
//
procedure SetGLTextureImageClassesToStrings(aStrings : TStrings);
var
	i : Integer;
	tic : TGLTextureImageClass;
begin
	with aStrings do begin
		BeginUpdate;
		Clear;
		if Assigned(vGLTextureImageClasses) then
			for i:=0 to vGLTextureImageClasses.Count-1 do begin
				tic:=TGLTextureImageClass(vGLTextureImageClasses[i]);
				AddObject(tic.FriendlyName, Pointer(tic));
			end;
		EndUpdate;
	end;
end;

// GetGLTextureImageClassesAsStrings
//
function GetGLTextureImageClassesAsStrings : TStrings;
begin
	Result:=TStringList.Create;
	SetGLTextureImageClassesToStrings(Result);
end;

//---------------------- TGLColor ----------------------------------------------

constructor TGLColor.Create(AOwner: TPersistent);
begin
   inherited;
	FColor:=clrBlack;
	FDefaultColor:=FColor;
end;

// CreateInitialized
//
constructor TGLColor.CreateInitialized(AOwner : TPersistent; const color : TColorVector;
                                       changeEvent : TNotifyEvent = nil);
begin
   Create(AOwner);
   Initialize(color);
   OnNotifyChange:=changeEvent;
end;

procedure TGLColor.SetColor(AColor: TColorVector);
begin
   FColor:=AColor;
	NotifyChange(Self);
end;

procedure TGLColor.SetColorComponent(Index: Integer; Value: TGLFloat);
begin
	if FColor[Index]<>Value then begin
		FColor[Index]:=Value;
		NotifyChange(Self);
	end;
end;

// SetAsWinColor
//
procedure TGLColor.SetAsWinColor(const val : TColor);
begin
	FColor:=ConvertWinColor(val);
	NotifyChange(Self);
end;

// GetAsWinColor
//
function TGLColor.GetAsWinColor : TColor;
begin
	Result:=ConvertColorVector(FColor);
end;

procedure TGLColor.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLColor) then begin
		FColor:=TGLColor(Source).FColor;
      NotifyChange(Self);
   end else inherited;
end;

procedure TGLColor.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Color', ReadData, WriteData,
                             not VectorEquals(FColor, FDefaultColor));
end;

// ReadData
//
procedure TGLColor.ReadData(Stream: TStream);
begin
  Stream.Read(FColor, SizeOf(FColor));
end;

// WriteData
//
procedure TGLColor.WriteData(Stream: TStream);
begin
  Stream.Write(FColor, SizeOf(FColor));
end;

// NotifyChange
//
procedure TGLColor.NotifyChange(Sender : TObject);
begin
   if Assigned(Owner) then begin
      if Owner is TGLBaseSceneObject then
         TGLBaseSceneObject(Owner).StructureChanged;
      inherited;
   end;
end;

// AsAddress
//
function TGLColor.AsAddress: PGLFloat;
begin
	Result:=@FColor;
end;

procedure TGLColor.Initialize(const color : TColorVector);
begin
   FColor:=color;
	FDefaultColor:=color;
end;

//----------------- TGLFaceProperties --------------------------------------------

constructor TGLFaceProperties.Create(AOwner: TPersistent);
begin
  inherited;
  // OpenGL default colors
  FAmbient:=TGLColor.Create(Self);
  FAmbient.Initialize(clrGray20);
  FDiffuse:=TGLColor.Create(Self);
  FDiffuse.Initialize(clrGray80);
  FEmission:=TGLColor.Create(Self);
  FSpecular:=TGLColor.Create(Self);
  FShininess:=0;
end;

destructor TGLFaceProperties.Destroy;
begin
   FAmbient.Free;
   FDiffuse.Free;
   FEmission.Free;
   FSpecular.Free;
   inherited Destroy;
end;

// Apply
//
procedure TGLFaceProperties.Apply(AFace: TGLEnum);
const
   cPolygonMode : array [pmFill..pmPoints] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);
begin
   SetGLMaterialColors(AFace, @Emission.FColor, @Ambient.FColor, @Diffuse.FColor,
                              @Specular.FColor,
                       FShininess);
   SetGLPolygonMode(AFace, cPolygonMode[FPolygonMode]);
end;

// Assign
//
procedure TGLFaceProperties.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLFaceProperties) then begin
      FAmbient.FColor:=TGLFaceProperties(Source).FAmbient.FColor;
      FDiffuse.FColor:=TGLFaceProperties(Source).FDiffuse.FColor;
      FSpecular.FColor:=TGLFaceProperties(Source).FSpecular.FColor;
      FShininess:=TGLFaceProperties(Source).FShininess;
      FPolygonMode:=TGLFaceProperties(Source).FPolygonMode;
		FEmission.FColor:=TGLFaceProperties(Source).FEmission.FColor;
		NotifyChange(Self);
   end;
end;

// SetAmbient
//
procedure TGLFaceProperties.SetAmbient(AValue: TGLColor);
begin
   FAmbient.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetDiffuse
//
procedure TGLFaceProperties.SetDiffuse(AValue: TGLColor);
begin
   FDiffuse.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetEmission
//
procedure TGLFaceProperties.SetEmission(AValue: TGLColor);
begin
   FEmission.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetSpecular
//
procedure TGLFaceProperties.SetSpecular(AValue: TGLColor);
begin
   FSpecular.FColor:=AValue.FColor;
   NotifyChange(Self);
end;

// SetPolygonMode
//
procedure TGLFaceProperties.SetPolygonMode(AValue: TPolygonMode);
begin
   if AValue<>FPolygonMode then begin
      FPolygonMode := AValue;
      NotifyChange(Self);
   end;
end;

// SetShininess
//
procedure TGLFaceProperties.SetShininess(AValue: TShininess);
begin
	if FShininess<>AValue then begin
		FShininess:=AValue;
		NotifyChange(Self);
	end;
end;

// ------------------
// ------------------ TGLTextureImage ------------------
// ------------------

// Create
//
constructor TGLTextureImage.Create(AOwner: TPersistent);
begin
	inherited;
	FOwnerTexture:=(AOwner as TGLTexture);
end;

// Destroy
//
destructor TGLTextureImage.Destroy;
begin
	inherited Destroy;
end;

// FriendlyDescription
//
class function TGLTextureImage.FriendlyDescription : String;
begin
	Result:=FriendlyName;
end;

// Invalidate
//
procedure TGLTextureImage.Invalidate;
begin
	ReleaseBitmap32;
	Include(FOwnerTexture.FChanges, tcImage);
   NotifyChange(Self);
end;

// ReleaseBitmap32
//
procedure TGLTextureImage.ReleaseBitmap32;
begin
	// nothing here.
end;

// NotifyChange
//
procedure TGLTextureImage.NotifyChange;
begin
	Include(FOwnerTexture.FChanges, tcImage);
	FOwnerTexture.NotifyChange(Self);
end;

// LoadFromFile
//
procedure TGLTextureImage.LoadFromFile(const fileName : String);
var
   buf : String;
begin
   if Assigned(FOnTextureNeeded) then begin
      buf:=fileName;
      FOnTextureNeeded(Self, buf);
   end;
end;

// ------------------
// ------------------ TGLPictureImage ------------------
// ------------------

// Create
//
constructor TGLPictureImage.Create(AOwner: TPersistent);
begin
	inherited;
	FPicture:=TPicture.Create;
	FPicture.OnChange:=PictureChanged;
end;

// Destroy
//
destructor TGLPictureImage.Destroy;
begin
	ReleaseBitmap32;
	FPicture.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLPictureImage.Assign(Source: TPersistent);
begin
	if Assigned(Source) then begin
      if (Source is TGLPersistentImage) then
   		FPicture.Assign(TGLPersistentImage(Source).FPicture)
      else if (Source is TGraphic) then
         FPicture.Assign(Source)
      else if (Source is TPicture) then
         FPicture.Assign(Source)
      else inherited;
	end else inherited;
end;

// BeginUpdate
//
procedure TGLPictureImage.BeginUpdate;
begin
	Inc(FUpdateCounter);
	FPicture.OnChange:=nil;
end;

// EndUpdate
//
procedure TGLPictureImage.EndUpdate;
begin
	Assert(FUpdateCounter>0, ClassName+': Unbalanced Begin/EndUpdate');
	Dec(FUpdateCounter);
	FPicture.OnChange:=PictureChanged;
	if FUpdateCounter=0 then
		PictureChanged(FPicture);
end;

// GetHeight
//
function TGLPictureImage.GetHeight: Integer;
begin
	Result:=FPicture.Height;
end;

// GetWidth
//
function TGLPictureImage.GetWidth: Integer;
begin
	Result:=FPicture.Width;
end;

// GetBitmap32
//
function TGLPictureImage.GetBitmap32 : TGLBitmap32;
begin
	if not Assigned(FBitmap) then begin
      FBitmap:=TGLBitmap32.Create;
      // we need to deactivate OnChange, due to a "glitch" in some TGraphics,
      // for instance, TJPegImage triggers an OnChange when it is drawn...
      FPicture.OnChange:=nil;
      try
         FBitmap.Assign(FPicture.Graphic);
      finally
         FPicture.OnChange:=PictureChanged;
      end;
	end;
	Result:=FBitmap;
end;

// ReleaseBitmap32
//
procedure TGLPictureImage.ReleaseBitmap32;
begin
	if Assigned(FBitmap) then begin
   	FBitmap.Free;
		FBitmap:=nil;
	end;
end;

// PictureChanged
//
procedure TGLPictureImage.PictureChanged(Sender: TObject);
begin
	Invalidate;
end;

// SetPicture
//
procedure TGLPictureImage.SetPicture(const aPicture : TPicture);
begin
	FPicture.Assign(aPicture);
end;

// ------------------
// ------------------ TGLPersistentImage ------------------
// ------------------

// Create
//
constructor TGLPersistentImage.Create(AOwner: TPersistent);
begin
	inherited;
end;

// Destroy
//
destructor TGLPersistentImage.Destroy;
begin
	inherited Destroy;
end;

// Edit
//
function TGLPersistentImage.Edit : Boolean;
var
	opd : TOpenPictureDialog;
begin
	opd:=TOpenPictureDialog.Create(nil);
	try
		Result:=opd.Execute;
		if Result then begin
			LoadFromFile(opd.FileName);
			NotifyChange(Self);
		end;
	finally
		opd.Free;
	end;
end;

// SaveToFile
//
procedure TGLPersistentImage.SaveToFile(const fileName : String);
begin
	Picture.SaveToFile(fileName);
end;

// LoadFromFile
//
procedure TGLPersistentImage.LoadFromFile(const fileName : String);
begin
   inherited;
   if FileExists(fileName) then
      Picture.LoadFromFile(fileName)
   else begin
      Picture.Graphic:=nil;
      Assert(False, Format(glsFailedOpenFile, [fileName]));
   end;
end;

// FriendlyName
//
class function TGLPersistentImage.FriendlyName : String;
begin
	Result:='Persistent Image';
end;

// FriendlyDescription
//
class function TGLPersistentImage.FriendlyDescription : String;
begin
	Result:='Image data is stored in its original format with other form resources,'
			 +'ie. in the DFM at design-time, and embedded in the EXE at run-time.';
end;

// ------------------
// ------------------ TGLPicFileImage ------------------
// ------------------

// Create
//
constructor TGLPicFileImage.Create(AOwner: TPersistent);
begin
	inherited;
end;

// Destroy
//
destructor TGLPicFileImage.Destroy;
begin
	inherited;
end;

// Assign
//
procedure TGLPicFileImage.Assign(Source: TPersistent);
begin
   if Source is TGLPicFileImage then begin
      FPictureFileName:=TGLPicFileImage(Source).FPictureFileName
  	end else inherited;
end;

// SetPictureFileName
//
procedure TGLPicFileImage.SetPictureFileName(const val : String);
begin
	if val<>FPictureFileName then begin
		FPictureFileName:=val;
		Invalidate;
	end;
end;

// Invalidate
//
procedure TGLPicFileImage.Invalidate;
begin
	Picture.OnChange:=nil;
	try
		Picture.Assign(nil);
		FBitmap:=nil;
	finally
		Picture.OnChange:=PictureChanged;
	end;
	inherited;
end;

// GetBitmap32
//
function TGLPicFileImage.GetBitmap32 : TGLBitmap32;
begin
	if (GetWidth<=0) and (PictureFileName<>'') then begin
		Picture.OnChange:=nil;
		try
         if FileExists(PictureFileName) then
   			Picture.LoadFromFile(PictureFileName)
         else begin
            Picture.Graphic:=nil;
            FPictureFileName:='';
            Assert(False, Format(glsFailedOpenFile, [PictureFileName]));
         end;
		finally
			Picture.OnChange:=PictureChanged;
		end;
	end;
  	Result:=inherited GetBitmap32;
end;

// Edit
//
function TGLPicFileImage.Edit : Boolean;
var
	newName : String;
begin
{ TODO : A better TGLPicFileImage.Edit is needed... }
	newName:=InputBox('PicFile Image', 'Enter filename', PictureFileName);
	Result:=(PictureFileName<>newName);
	if Result then
		PictureFileName:=newName
end;

// SaveToFile
//
procedure TGLPicFileImage.SaveToFile(const fileName : String);
begin
	SaveStringToFile(fileName, PictureFileName);
end;

// LoadFromFile
//
procedure TGLPicFileImage.LoadFromFile(const fileName : String);
var
   buf : String;
begin
   inherited;
   // attempt to autodetect if we are pointed to a file containing
   // a filename or directly to an image
   if SizeOfFile(fileName)<512 then begin
   	buf:=LoadStringFromFile(fileName);
      if Pos(#0, buf)>0 then
         PictureFileName:=fileName
      else PictureFileName:=buf;
   end else PictureFileName:=fileName;
end;

// FriendlyName
//
class function TGLPicFileImage.FriendlyName : String;
begin
	Result:='PicFile Image';
end;

// FriendlyDescription
//
class function TGLPicFileImage.FriendlyDescription : String;
begin
	Result:='Image data is retrieved from a file.';
end;

{ !!!!!!!!!!! Disabled !!!!!!!!!!!!!!!!

//----------------- TGLCaptureImage ----------------------------------------------

constructor TGLCaptureImage.Create(AOwner: TPersistent);
begin
  inherited;
  FBitmap:=TBitmap.Create;
  FBitmap.Width:=32;
  FBitmap.Height:=32;
  //FBitmap.OnChange:=PictureChanged;
end;

destructor TGLCaptureImage.Destroy;
begin
   FBitmap.Free;
   inherited Destroy;
end;

function TGLCaptureImage.GetHeight: Integer;
begin
	Result:=FBitmap.Height;
end;

function TGLCaptureImage.GetWidth: Integer;
begin
  Result:=FBitmap.Width;
end;

function TGLCaptureImage.GetBitmap: HBitmap;
begin
  if not Valid then
	 raise Exception.Create(glsImageInvalid);
  Result:=FBitmap.Handle;
end;

procedure TGLCaptureImage.DataNeeded;
var
   Rect : TRectangle;
begin
   if not Valid then begin
		Rect.Left:=FLeft;
      Rect.Top:=FTop;
      Rect.Width:=FBitmap.Width;
      Rect.Height:=FBitmap.Height;
		ReadScreenImage(FBitmap.Canvas.Handle,0,0,Rect);
		Validate;
   end;
end;

procedure TGLCaptureImage.PictureChanged(Sender: TObject);
begin
	if Valid then NotifyChange;
end;

procedure TGLCaptureImage.SetHeight(AValue: Integer);
begin
	if FBitmap.Height <> AValue then begin
		FBitmap.Height:=AValue;
		Invalidate;
	end;
end;

procedure TGLCaptureImage.SetLeft(AValue: Integer);
begin
	if FLeft <> AValue then begin
		FLeft:=AValue;
		Invalidate;
	end;
end;

procedure TGLCaptureImage.SetTop(AValue: Integer);
begin
	if FTop <> AValue then begin
		FTop:=AValue;
		Invalidate;
	end;
end;

procedure TGLCaptureImage.SetWidth(AValue: Integer);
begin
	if FBitmap.Width <> AValue then begin
		FBitmap.Width:=AValue;
		Invalidate;
	end;
end;
}

// ------------------
// ------------------ TGLTexture ------------------
// ------------------

// Create
//
constructor TGLTexture.Create(AOwner: TPersistent);
begin
	inherited;
	FDisabled:=True;
	FChanges:=[tcImage, tcParams];
	FImage:=TGLPersistentImage.Create(Self);
	FImageAlpha:=tiaDefault;
	FMagFilter:=maNearest;
	FMinFilter:=miNearest;
   FFilteringQuality:=tfIsotropic;
   FRequiredMemorySize:=-1;
end;

// Destroy
//
destructor TGLTexture.Destroy;
begin
	DestroyHandle(False);
	FImage.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLTexture.Assign(Source: TPersistent);
begin
  if Assigned(Source) then begin
      if (Source is TGLTexture) then begin
		   if Source<>Self then begin
			   FImageAlpha:=TGLTexture(Source).FImageAlpha;
   			FTextureMode:=TGLTexture(Source).FTextureMode;
	   		FTextureWrap:=TGLTexture(Source).FTextureWrap;
      		FTextureFormat:=TGLTexture(Source).FTextureFormat;
      		FCompression:=TGLTexture(Source).FCompression;
		   	FMinFilter:=TGLTexture(Source).FMinFilter;
			   FMagFilter:=TGLTexture(Source).FMagFilter;
   			FDisabled:=TGLTexture(Source).FDisabled;
	   		SetImage(TGLTexture(Source).FImage);
		   	FChanges:=[tcParams, tcImage];
   		end;
      end else if (Source is TGraphic) then begin
         Image.Assign(Source);
      end;
	end else inherited Assign(Source);
end;

// SetImage
//
procedure TGLTexture.SetImage(AValue: TGLTextureImage);
begin
	if FImage.ClassType<>AValue.ClassType then begin
		FImage.Free;
		FImage:=TGLTextureImageClass(AValue.ClassType).Create(Self);
      FImage.OnTextureNeeded:=OnTextureNeeded;
	end;
	FImage.Assign(AValue);
end;

// SetImageClassName
//
procedure TGLTexture.SetImageClassName(const val : String);
begin
	if val<>'' then if FImage.ClassName<>val then begin
		FImage.Free;
		FImage:=TGLTextureImageClass(FindGLTextureImageClass(val)).Create(Self);
      FImage.OnTextureNeeded:=OnTextureNeeded;
		Include(FChanges, tcImage);
		NotifyChange(Self);
	end;
end;

// GetImageClassName
//
function TGLTexture.GetImageClassName : String;
begin
	Result:=FImage.ClassName;
end;

// TextureImageRequiredMemory
//
function TGLTexture.TextureImageRequiredMemory : Integer;
const
   cTextureFormatToPixelSize : array [tfRGB..tfIntensity] of Integer = (
                                                     3, 4, 2, 2, 1, 1, 2, 1);
var
   tf : TGLTextureFormat;
begin
   if FRequiredMemorySize>0 then
      Result:=FRequiredMemorySize
   else begin
      if TextureFormat=tfDefault then
         if vDefaultTextureFormat=tfDefault then
            tf:=tfRGBA
         else tf:=vDefaultTextureFormat
      else tf:=TextureFormat;
      Result:=cTextureFormatToPixelSize[tf]*Image.Width*Image.Height;
   end;
end;

// SetImageAlpha
//
procedure TGLTexture.SetImageAlpha(const val : TGLTextureImageAlpha);
begin
	FImageAlpha:=val;
	Include(FChanges, tcImage);
	NotifyChange(Self);
end;

// SetMagFilter
//
procedure TGLTexture.SetMagFilter(AValue: TGLMagFilter);
begin
	if AValue <> FMagFilter then begin
		FMagFilter:=AValue;
		Include(FChanges, tcParams);
		NotifyChange(Self);
	end;
end;

// SetMinFilter
//
procedure TGLTexture.SetMinFilter(AValue: TGLMinFilter);
begin
	if AValue <> FMinFilter then begin
		FMinFilter:=AValue;
		Include(FChanges,tcParams);
		NotifyChange(Self);
	end;
end;

// SetTextureMode
//
procedure TGLTexture.SetTextureMode(AValue: TGLTextureMode);
begin
	if AValue <> FTextureMode then begin
		FTextureMode:=AValue;
		Include(FChanges, tcParams);
		NotifyChange(Self);
	end;
end;

// SetDisabled
//
procedure TGLTexture.SetDisabled(AValue: Boolean);
begin
	if AValue <> FDisabled then begin
		FDisabled:=AValue;
		NotifyChange(Self);
	end;
end;

// SetTextureWrap
//
procedure TGLTexture.SetTextureWrap(AValue: TGLTextureWrap);
begin
	if AValue <> FTextureWrap then begin
		FTextureWrap:=AValue;
		Include(FChanges,tcParams);
		NotifyChange(Self);
	end;
end;

// SetTextureFormat
//
procedure TGLTexture.SetTextureFormat(const val : TGLTextureFormat);
begin
	if val <> FTextureFormat then begin
		FTextureFormat:=val;
		Include(FChanges, tcParams);
		NotifyChange(Self);
	end;
end;

// SetCompression
//
procedure TGLTexture.SetCompression(const val : TGLTextureCompression);
begin
	if val <> FCompression then begin
		FCompression:=val;
		Include(FChanges, tcParams);
		NotifyChange(Self);
	end;
end;

// SetFilteringQuality
//
procedure TGLTexture.SetFilteringQuality(const val : TGLTextureFilteringQuality);
begin
	if val <> FFilteringQuality then begin
		FFilteringQuality:=val;
		Include(FChanges, tcParams);
		NotifyChange(Self);
	end;
end;

// StoreImageClassName
//
function TGLTexture.StoreImageClassName : Boolean;
begin
   Result:=(FImage.ClassName<>TGLPersistentImage.ClassName);
end;

// PrepareBuildList
//
procedure TGLTexture.PrepareBuildList;
begin
   GetHandle;
end;

// Apply
//
procedure TGLTexture.Apply(var currentStates : TGLStates);
begin
	if not Disabled then begin
		SetGLState(currentStates, stTexture2D);
	   glBindTexture(GL_TEXTURE_2D, Handle);
   	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
	end else begin
      UnSetGLState(currentStates, stTexture2D);
   end;
end;

// ApplyAsTexture2
//
procedure TGLTexture.ApplyAsTexture2(libMaterial : TGLLibMaterial);
begin
   if not Disabled then begin
      glActiveTextureARB(GL_TEXTURE1_ARB);
      glEnable(GL_TEXTURE_2D);
      glBindTexture(GL_TEXTURE_2D, Handle);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, cTextureMode[FTextureMode]);
      if not libMaterial.FTextureMatrixIsIdentity then begin
         glMatrixMode(GL_TEXTURE);
         glLoadMatrixf(PGLFloat(@libMaterial.FTextureMatrix[0][0]));
         glMatrixMode(GL_MODELVIEW);
      end;
      xglMapTexCoordToDual;
      glActiveTextureARB(GL_TEXTURE0_ARB);
   end;
end;

// UnApplyAsTexture2
//
procedure TGLTexture.UnApplyAsTexture2(libMaterial : TGLLibMaterial);
begin
   xglMapTexCoordToMain;
   glActiveTextureARB(GL_TEXTURE1_ARB);
   if not libMaterial.FTextureMatrixIsIdentity then begin
      glMatrixMode(GL_TEXTURE);
      glLoadIdentity;
      glMatrixMode(GL_MODELVIEW);
   end;
   glDisable(GL_TEXTURE_2D);
   glActiveTextureARB(GL_TEXTURE0_ARB); 
end;

// GetHandle
//
function TGLTexture.GetHandle : TGLuint;
begin
	if (FHandle = 0) or (FChanges <> []) then begin
		if FHandle = 0 then begin
			glGenTextures(1, @FHandle);
			Assert(FHandle<>0);
		end;
      // used for last ditch attempt to free texture library
      FLastDC:=CurrentRenderingContextDC;
      FLastRC:=CurrentRC;
      // bind texture
		glBindTexture(GL_TEXTURE_2D, FHandle);
		PrepareParams;
		PrepareImage;
		FChanges:=[];
	end;
	Result:=FHandle;
end;

// DestroyHandle
//
procedure TGLTexture.DestroyHandle(glsceneOnly : Boolean);
begin
	if FHandle <> 0 then begin
      if not glsceneOnly then begin
         if CurrentRenderingContextDC<=0 then begin
            // attempt to restore DC/RC
            ActivateRenderingContext(FLastDC, FLastRC);
            Assert(CurrentRenderingContextDC>0);
   	   	glDeleteTextures(1, @FHandle);
            DeactivateRenderingContext;
	   	end else glDeleteTextures(1, @FHandle);
      end;
      FHandle:=0;
		FChanges:=[tcParams,tcImage];
      FRequiredMemorySize:=-1;
	end;
end;

// PrepareImage
//
procedure TGLTexture.PrepareImage;
const
   cTextureFormatToOpenGL : array [tfRGB..tfIntensity] of Integer =
      (GL_RGB, GL_RGBA, GL_RGB5, GL_RGBA4, GL_ALPHA, GL_LUMINANCE,
       GL_LUMINANCE_ALPHA, GL_INTENSITY);
   cCompressedTextureFormatToOpenGL : array [tfRGB..tfIntensity] of Integer =
      (GL_COMPRESSED_RGB_ARB, GL_COMPRESSED_RGBA_ARB, GL_COMPRESSED_RGB_ARB,
       GL_COMPRESSED_RGBA_ARB, GL_COMPRESSED_ALPHA_ARB, GL_COMPRESSED_LUMINANCE_ARB,
       GL_COMPRESSED_LUMINANCE_ALPHA_ARB, GL_COMPRESSED_INTENSITY_ARB);
var
	alphaChannelRequired : Boolean;
   bitmap32 : TGLBitmap32;
   targetFormat : Integer;
   texForm : TGLTextureFormat;
   texComp : TGLTextureCompression;
begin
   bitmap32:=Image.GetBitmap32;
   if (bitmap32=nil) or bitmap32.IsEmpty then Exit;
   // select targetFormat from texture format & compression options
   if TextureFormat=tfDefault then
      if vDefaultTextureFormat=tfDefault then
         texForm:=tfRGBA
      else texForm:=vDefaultTextureFormat
   else texForm:=TextureFormat;
   if GL_ARB_texture_compression then begin
      if Compression=tcDefault then
         if vDefaultTextureCompression=tcDefault then
            texComp:=tcNone
         else texComp:=vDefaultTextureCompression
      else texComp:=Compression;
   end else texComp:=tcNone;
   if texComp<>tcNone then begin
      case texComp of
         tcStandard : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_DONT_CARE);
         tcHighQuality : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_NICEST);
         tcHighSpeed : glHint(GL_TEXTURE_COMPRESSION_HINT_ARB, GL_FASTEST);
      else
         Assert(False);
      end;
      targetFormat:=cCompressedTextureFormatToOpenGL[texForm];
   end else targetFormat:=cTextureFormatToOpenGL[texForm];
   // prepare AlphaChannel
	alphaChannelRequired:=(ImageAlpha<>tiaDefault);
   if alphaChannelRequired then begin
      case ImageAlpha of
         tiaAlphaFromIntensity :
            bitmap32.SetAlphaFromIntensity;
         tiaSuperBlackTransparent :
            bitmap32.SetAlphaTransparentForColor($000000);
         tiaLuminance :
            bitmap32.SetAlphaFromIntensity;
         tiaLuminanceSqrt : begin
            bitmap32.SetAlphaFromIntensity;
            bitmap32.SqrtAlpha;
         end;
         tiaOpaque :
            bitmap32.SetAlphaToValue(255);
         tiaTopLeftPointColorTransparent :
            bitmap32.SetAlphaTransparentForColor(bitmap32.Data[0]);
      else
         Assert(False);
      end;
   end else bitmap32.SetAlphaToValue(255);
   bitmap32.RegisterAsOpenGLTexture(MinFilter, targetFormat);
   if texComp<>tcNone then
      glGetTexLevelParameteriv(GL_TEXTURE_2D, 0, GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB,
                               @FRequiredMemorySize)
   else FRequiredMemorySize:=-1;
   image.ReleaseBitmap32;
end;

// PrepareParams
//
procedure TGLTexture.PrepareParams;
const
	cTextureSWrap : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP, GL_CLAMP, GL_REPEAT );
	cTextureTWrap : array [twBoth..twHorizontal] of TGLEnum =
							( GL_REPEAT, GL_CLAMP, GL_REPEAT, GL_CLAMP );
	cTextureMagFilter : array [maNearest..maLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR );
	cTextureMinFilter : array [miNearest..miLinearMipmapLinear] of TGLEnum =
							( GL_NEAREST, GL_LINEAR, GL_NEAREST_MIPMAP_NEAREST,
							  GL_LINEAR_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_LINEAR,
							  GL_LINEAR_MIPMAP_LINEAR );
   cFilteringQuality : array [tfIsotropic..tfAnisotropic] of Integer = (1, 2);
begin
	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
	glPixelStorei(GL_UNPACK_ROW_LENGTH, 0);
	glPixelStorei(GL_UNPACK_SKIP_ROWS, 0);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, 0);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, cTextureSWrap[FTextureWrap]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, cTextureTWrap[FTextureWrap]);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, cTextureMinFilter[FMinFilter]);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, cTextureMagFilter[FMagFilter]);

   if GL_EXT_texture_filter_anisotropic then
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT,
                      cFilteringQuality[FFilteringQuality]);
end;

// DisableAutoTexture
//
procedure TGLTexture.DisableAutoTexture;
begin
	xglDisable(GL_TEXTURE_GEN_S);
	xglDisable(GL_TEXTURE_GEN_T);
end;

// InitAutoTexture
//
procedure TGLTexture.InitAutoTexture(const texRep : TTexPoint);
begin
   InitAutoTexture(@texRep);
end;

// InitAutoTexture
//
procedure TGLTexture.InitAutoTexture(texRep : PTexPoint);
var
	sGenParams, tGenParams  : TVector;
begin
	sGenParams:=XHmgVector;
	tGenParams:=YHmgVector;

	xglTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
	if Assigned(TexRep) then
		sGenparams[0]:=TexRep.S;
	xglTexGenfv(GL_S, GL_OBJECT_PLANE, @SGenParams);
	xglTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
	if Assigned(TexRep) then
		tGenparams[1]:=TexRep.T;
	xglTexGenfv(GL_T, GL_OBJECT_PLANE, @TGenparams);

	xglEnable(GL_TEXTURE_GEN_S);
  	xglEnable(GL_TEXTURE_GEN_T);
end;

//----------------- TGLMaterial --------------------------------------------------

// Create
//
constructor TGLMaterial.Create(AOwner: TPersistent);
begin
  inherited;
  FBackProperties:=TGLFaceProperties.Create(Self);
  FFrontProperties:=TGLFaceProperties.Create(Self);
  FTexture:=TGLTexture.Create(Self);
end;

// Destroy
//
destructor TGLMaterial.Destroy;
begin
   if Assigned(currentLibMaterial) then
      currentLibMaterial.UnregisterUser(Self);
   FBackProperties.Free;
   FFrontProperties.Free;
   FTexture.Free;
   inherited Destroy;
end;

// SetBackProperties
//
procedure TGLMaterial.SetBackProperties(Values: TGLFaceProperties);
begin
	FBackProperties.Assign(Values);
	NotifyChange(Self);
end;

// SetFrontProperties
//
procedure TGLMaterial.SetFrontProperties(Values: TGLFaceProperties);
begin
	FFrontProperties.Assign(Values);
	NotifyChange(Self);
end;

// SetBlendingMode
//
procedure TGLMaterial.SetBlendingMode(const val : TBlendingMode);
begin
   if val <> FBlendingMode then begin
      FBlendingMode := val;
   	NotifyChange(Self);
   end;
end;

// SetMaterialOptions
//
procedure TGLMaterial.SetMaterialOptions(const val : TMaterialOptions);
begin
   if val <> FMaterialOptions then begin
      FMaterialOptions := val;
   	NotifyChange(Self);
   end;
end;

// SetTexture
//
procedure TGLMaterial.SetTexture(ATexture: TGLTexture);
begin
	FTexture.Assign(ATexture);
end;

// SetMaterialLibrary
//
procedure TGLMaterial.SetMaterialLibrary(const val : TGLMaterialLibrary);
begin
   FMaterialLibrary:=val;
   SetLibMaterialName(LibMaterialName);
end;

// SetLibMaterialName
//
procedure TGLMaterial.SetLibMaterialName(const val : TGLLibMaterialName);
var
   newLibMaterial : TGLLibMaterial;
begin
   // locate new libmaterial
   if Assigned(FMaterialLibrary) then
      newLibMaterial:=MaterialLibrary.Materials.GetLibMaterialByName(val)
   else newLibMaterial:=nil;
   FLibMaterialName:=val;
   // unregister if required
   if newLibMaterial<>currentLibMaterial then begin
      if Assigned(currentLibMaterial) then
         currentLibMaterial.UnregisterUser(Self);
      currentLibMaterial:=newLibMaterial;
      // register with new
      if Assigned(currentLibMaterial) then
         currentLibMaterial.RegisterUser(Self);
      NotifyChange(Self);
   end;
end;

// NotifyLibMaterialDestruction
//
procedure TGLMaterial.NotifyLibMaterialDestruction;
begin
   FMaterialLibrary:=nil;
   FLibMaterialName:='';
   currentLibMaterial:=nil;
end;

// StoreMaterialProps
//
function TGLMaterial.StoreMaterialProps : Boolean;
begin
	Result:=not Assigned(currentLibMaterial);
end;

// PrepareBuildList
//
procedure TGLMaterial.PrepareBuildList;
begin
   if not FTexture.Disabled then
      Texture.PrepareBuildList;
end;

// Apply
//
procedure TGLMaterial.Apply(var rci : TRenderContextInfo);
begin
	if Assigned(currentLibMaterial) then
		currentLibMaterial.Apply(rci)
	else begin
		FFrontProperties.Apply(GL_FRONT);
		if not (stCullFace in rci.currentStates) then
			FBackProperties.Apply(GL_BACK);
		case FBlendingMode of
			bmOpaque : UnSetGLState(rci.currentStates, stBlend);
			bmTransparency : begin
				SetGLState(rci.currentStates, stBlend);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
			end;
			bmAdditive : begin
				SetGLState(rci.currentStates, stBlend);
				glBlendFunc(GL_SRC_ALPHA, GL_ONE);
			end;
      else
         Assert(False);
		end;
      if moIgnoreFog in MaterialOptions then begin
         if stFog in rci.currentStates then begin
            UnSetGLState(rci.currentStates, stFog);
            Inc(rci.fogDisabledCounter);
         end;
      end;
   	FTexture.Apply(rci.currentStates);
	end;
end;

// UnApply
//
procedure TGLMaterial.UnApply(var rci : TRenderContextInfo);
begin
	if Assigned(currentLibMaterial) then
		currentLibMaterial.UnApply(rci)
   else begin
      if (moIgnoreFog in MaterialOptions) then begin
         if rci.fogDisabledCounter>0 then begin
            Dec(rci.fogDisabledCounter);
            if rci.fogDisabledCounter=0 then
               SetGLState(rci.currentStates, stFog);
         end;
      end;
   end;
end;

// Assign
//
procedure TGLMaterial.Assign(Source: TPersistent);
begin
   if Assigned(Source) and (Source is TGLMaterial) then begin
      FBackProperties.Assign(TGLMaterial(Source).FBackProperties);
      FFrontProperties.Assign(TGLMaterial(Source).FFrontProperties);
		FBlendingMode:=TGLMaterial(Source).FBlendingMode;
      FMaterialOptions:=TGLMaterial(Source).FMaterialOptions;
      FTexture.Assign(TGLMaterial(Source).FTexture);
		FMaterialLibrary:=TGLMaterial(Source).MaterialLibrary;
      SetLibMaterialName(TGLMaterial(Source).LibMaterialName);
   	NotifyChange(Self);
   end else inherited;
end;

// NotifyChange
//
procedure TGLMaterial.NotifyChange;
begin
   if Assigned(Owner) then
      if Owner is TGLBaseSceneObject then
         TGLBaseSceneObject(Owner).NotifyChange(Self)
      else if Owner is TGLLibMaterial then
         TGLLibMaterial(Owner).NotifyUsers;
end;

// DestroyHandle
//
procedure TGLMaterial.DestroyHandle(glsceneOnly : Boolean);
begin
	if Assigned(currentLibMaterial) then
		currentLibMaterial.DestroyHandle(glsceneOnly);
   Texture.DestroyHandle(glsceneOnly);
end;

// ------------------
// ------------------ TGLLibMaterial ------------------
// ------------------

// Create
//
constructor TGLLibMaterial.Create(Collection : TCollection);
begin
	inherited Create(Collection);
   userList:=TList.Create;
   FName:=TGLLibMaterials(Collection).MakeUniqueName('LibMaterial');
   FMaterial:=TGLMaterial.Create(Self);
   FMaterial.Texture.OnTextureNeeded:=DoOnTextureNeeded;
   FTextureOffset:=TGLCoordinates.CreateInitialized(Self, NullHmgVector);
   FTextureOffset.OnNotifyChange:=OnNotifyChange;
   FTextureScale:=TGLCoordinates.CreateInitialized(Self, XYZHmgVector);
   FTextureScale.OnNotifyChange:=OnNotifyChange;
   FTextureMatrixIsIdentity:=True;
end;

// Destroy
//
destructor TGLLibMaterial.Destroy;
var
   i : Integer;
begin
   Texture2Name:='';
   for i:=0 to userList.Count-1 do
      TGLMaterial(userList[i]).NotifyLibMaterialDestruction;
   userList.Free;
   FMaterial.Free;
   FTextureOffset.Free;
   FTextureScale.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLLibMaterial.Assign(Source: TPersistent);
begin
	if Source is TGLLibMaterial then begin
      FName:=TGLLibMaterials(Collection).MakeUniqueName(TGLLibMaterial(Source).Name);
      FMaterial.Assign(TGLLibMaterial(Source).Material);
      FTextureOffset.Assign(TGLLibMaterial(Source).TextureOffset);
      FTextureScale.Assign(TGLLibMaterial(Source).TextureScale);
      FTexture2Name:=TGLLibMaterial(Source).Texture2Name;
      CalculateTextureMatrix;
	end else inherited;
end;

// PrepareBuildList
//
procedure TGLLibMaterial.PrepareBuildList;
begin
   Material.PrepareBuildList;
end;

// Apply
//
procedure TGLLibMaterial.Apply(var rci : TRenderContextInfo);
begin
   if not FTextureMatrixIsIdentity then
      SetGLTextureMatrix(FTextureMatrix);
   Material.Apply(rci);
   if (Texture2Name<>'') and GL_ARB_multitexture then begin
      if not Assigned(libMatTexture2) then begin
         libMatTexture2:=TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
         libMatTexture2.RegisterUser(Self);
      end;
      if Assigned(libMatTexture2) then
         libMatTexture2.Material.Texture.ApplyAsTexture2(libMatTexture2);
   end;
end;

// UnApply
//
procedure TGLLibMaterial.UnApply(var rci : TRenderContextInfo);
var
   libMat : TGLLibMaterial;
begin
   if (Texture2Name<>'') and GL_ARB_multitexture then begin
      libMat:=TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMat) then
         libMat.Material.Texture.UnApplyAsTexture2(Self);
   end;
   Material.UnApply(rci);
   if not FTextureMatrixIsIdentity then
      ResetGLTextureMatrix;
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(material : TGLMaterial);
begin
   Assert(userList.IndexOf(material)<0);
   userList.Add(material);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(material : TGLMaterial);
begin
   userList.Remove(material);
end;

// RegisterUser
//
procedure TGLLibMaterial.RegisterUser(libMaterial : TGLLibMaterial);
begin
   Assert(userList.IndexOf(libMaterial)<0);
   userList.Add(libMaterial);
end;

// UnregisterUser
//
procedure TGLLibMaterial.UnRegisterUser(libMaterial : TGLLibMaterial);
begin
   userList.Remove(material);
end;

// NotifyUsers
//
procedure TGLLibMaterial.NotifyUsers;
var
   i : Integer;
   obj : TObject;
begin
   if notifying then Exit;
   notifying:=True;
   try
      for i:=0 to userList.Count-1 do begin
         obj:=TObject(userList[i]);
         if obj is TGLMaterial then
            TGLMaterial(userList[i]).NotifyChange(Self)
         else begin
            Assert(obj is TGLLibMaterial);
            TGLLibMaterial(userList[i]).NotifyUsers;
         end;
      end;
   finally
      notifying:=False;
   end;
end;

// GetDisplayName
//
function TGLLibMaterial.GetDisplayName : String;
begin
	Result:=Name;
end;

// Loaded
//
procedure TGLLibMaterial.Loaded;
begin
   CalculateTextureMatrix;
end;

// SetName
//
procedure TGLLibMaterial.SetName(const val : TGLLibMaterialName);
begin
   if val<>FName then begin
      if not (csLoading in TComponent(TGLLibMaterials(Collection).GetOwner).ComponentState) then begin
         if TGLLibMaterials(Collection).GetLibMaterialByName(val)<>Self then
            FName:=TGLLibMaterials(Collection).MakeUniqueName(val)
         else FName:=val;
      end else FName:=val;
   end;
end;

// SetMaterial
//
procedure TGLLibMaterial.SetMaterial(const val : TGLMaterial);
begin
   FMaterial.Assign(val);
end;

// SetTextureOffset
//
procedure TGLLibMaterial.SetTextureOffset(const val : TGLCoordinates);
begin
   FTextureOffset.AsVector:=val.AsVector;
   CalculateTextureMatrix;
end;

// SetTextureScale
//
procedure TGLLibMaterial.SetTextureScale(const val : TGLCoordinates);
begin
   FTextureScale.AsVector:=val.AsVector;
   CalculateTextureMatrix;
end;

// SetTexture2
//
procedure TGLLibMaterial.SetTexture2Name(const val : TGLLibMaterialName);
begin
   if val<>Texture2Name then begin
      if Assigned(libMatTexture2) then begin
         libMatTexture2.UnregisterUser(Self);
         libMatTexture2:=nil;
      end;
      FTexture2Name:=val;
      NotifyUsers;
   end;
end;

// CalculateTextureMatrix
//
procedure TGLLibMaterial.CalculateTextureMatrix;
begin
   if TextureOffset.Equals(NullHmgVector) and TextureScale.Equals(XYZHmgVector) then
      FTextureMatrixIsIdentity:=True
   else begin
      FTextureMatrixIsIdentity:=False;
      FTextureMatrix:=CreateScaleAndTranslationMatrix(TextureScale.AsVector,
                                                      TextureOffset.AsVector);                     
   end;
   NotifyUsers;
end;

// DestroyHandle
//
procedure TGLLibMaterial.DestroyHandle(glsceneOnly : Boolean);
var
   libMat : TGLLibMaterial;
begin
	FMaterial.DestroyHandle(glsceneOnly);
   if FTexture2Name<>'' then begin
      libMat:=TGLLibMaterials(Collection).GetLibMaterialByName(Texture2Name);
      if Assigned(libMat) then
         libMat.DestroyHandle(glsceneOnly);
   end;
end;

// OnNotifyChange
//
procedure TGLLibMaterial.OnNotifyChange(Sender : TObject);
begin
   CalculateTextureMatrix;
end;

// DoOnTextureNeeded
//
procedure TGLLibMaterial.DoOnTextureNeeded(Sender : TObject; var textureFileName : String);
var
   mLib : TGLMaterialLibrary;
   i : Integer;
   tryName : String;
begin
   mLib:=TGLMaterialLibrary((Collection as TGLLibMaterials).Owner);
   if mLib is TGLMaterialLibrary then with mLib do
      if Assigned(FOnTextureNeeded) then
         FOnTextureNeeded(mLib, textureFileName);
   // if a ':' is present, or if it starts with a '\', consider it as an absolute path
   if (Pos(':', textureFileName)>0) or (Copy(textureFileName, 1, 1)='\') then Exit;
   // ok, not an absolute path, try given paths
   with mLib do begin
      if FTexturePathList<>nil then for i:=0 to FTexturePathList.Count-1 do begin
         tryName:=FTexturePathList[i]+textureFileName;
         if FileExists(tryName) then begin
            textureFileName:=tryName;
            Break;
         end;
      end;
   end;
end;

// ------------------
// ------------------ TGLLibMaterials ------------------
// ------------------

// Create
//
constructor TGLLibMaterials.Create(AOwner : TComponent);
begin
	FOwner:=AOwner;
	inherited Create(TGLLibMaterial);
end;

// GetOwner
//
function TGLLibMaterials.GetOwner: TPersistent;
begin
	Result:=Owner;
end;

// Loaded
//
procedure TGLLibMaterials.Loaded;
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].Loaded;
end;

// SetItems
//
procedure TGLLibMaterials.SetItems(index : Integer; const val : TGLLibMaterial);
begin
	inherited Items[index]:=val;
end;

// GetItems
//
function TGLLibMaterials.GetItems(index : Integer) : TGLLibMaterial;
begin
	Result:=TGLLibMaterial(inherited Items[index]);
end;

// DestroyHandles
//
procedure TGLLibMaterials.DestroyHandles(glsceneOnly : Boolean);
var
   i : Integer;
begin
   for i:=0 to Count-1 do
      Items[i].DestroyHandle(glsceneOnly);
end;

// Add
//
function TGLLibMaterials.Add: TGLLibMaterial;
begin
	Result:=(inherited Add) as TGLLibMaterial;
end;

// FindItemID
//
function TGLLibMaterials.FindItemID(ID: Integer): TGLLibMaterial;
begin
	Result:=(inherited FindItemID(ID)) as TGLLibMaterial;
end;

// MakeUniqueName
//
function TGLLibMaterials.MakeUniqueName(const nameRoot : TGLLibMaterialName) : TGLLibMaterialName;
var
   i : Integer;
begin
   Result:=nameRoot;
   i:=1;
   while GetLibMaterialByName(Result)<>nil do begin
      Result:=nameRoot+IntToStr(i);
      Inc(i);
   end;
end;

// GetLibMaterialByName
//
function TGLLibMaterials.GetLibMaterialByName(const name : TGLLibMaterialName) : TGLLibMaterial;
var
   i : Integer;
   lm : TGLLibMaterial;
begin
   Result:=nil;
   for i:=0 to Count-1 do begin
      lm:=TGLLibMaterial(inherited Items[i]);
      if lm.Name=name then begin
         Result:=lm;
         Break;
      end;
   end;
end;

// SetNamesToTStrings
//
procedure TGLLibMaterials.SetNamesToTStrings(aStrings : TStrings);
var
   i : Integer;
   lm : TGLLibMaterial;
begin
   with aStrings do begin
      BeginUpdate;
      Clear;
      for i:=0 to Self.Count-1 do begin
         lm:=TGLLibMaterial(inherited Items[i]);
         AddObject(lm.Name, lm);
      end;
      EndUpdate;
   end;
end;

// ------------------
// ------------------ TGLMaterialLibrary ------------------
// ------------------

// Create
//
constructor TGLMaterialLibrary.Create(AOwner : TComponent);
begin
   inherited;
   FMaterials:=TGLLibMaterials.Create(Self);
end;

// Destroy
//
destructor TGLMaterialLibrary.Destroy;
begin
   Assert(FLastAppliedMaterial=nil, 'Unbalanced material application');
   FTexturePathList.Free;
   FMaterials.Free;
   FMaterials:=nil;
   inherited;
end;

// DestroyHandles
//
procedure TGLMaterialLibrary.DestroyHandles(glsceneOnly : Boolean);
begin
   if Assigned(FMaterials) then
      FMaterials.DestroyHandles(glsceneOnly);
end;

// Loaded
//
procedure TGLMaterialLibrary.Loaded;
begin
   FMaterials.Loaded;
   inherited;
end;

// SetMaterials
//
procedure TGLMaterialLibrary.SetMaterials(const val : TGLLibMaterials);
begin
   FMaterials.Assign(val);
end;

// SetTexturePaths
//
procedure TGLMaterialLibrary.SetTexturePaths(const val : String);
var
   i, lp : Integer;

   procedure AddCurrent;
   var
      buf : String;
   begin
      buf:=Trim(Copy(val, lp+1, i-lp-1));
      if Length(buf)>0 then begin
         // make sure '\' is the terminator
         if buf[Length(buf)]<>'\' then buf:=buf+'\';
         FTexturePathList.Add(buf);
      end;
   end;

begin
	FTexturePathList.Free;
	FTexturePathList:=nil;
	FTexturePaths:=val;
   if val<>'' then begin
      FTexturePathList:=TStringList.Create;
      lp:=0;
      for i:=1 to Length(val) do begin
         if val[i]=';' then begin
            AddCurrent;
            lp:=i;
         end;
      end;
      i:=Length(val)+1;
      AddCurrent;
   end;
end;

// SaveToStream
//
procedure TGLMaterialLibrary.SaveToStream(aStream : TStream);
var
   wr : TWriter;
begin
   wr:=TWriter.Create(aStream, 16384);
   try
      wr.WriteComponent(Self);
   finally
      wr.Free;
   end;
end;

// LoadFromStream
//
procedure TGLMaterialLibrary.LoadFromStream(aStream : TStream);
var
   rd : TReader;
begin
   rd:=TReader.Create(aStream, 16384);
   try
      rd.ReadComponent(Self);
   finally
      rd.Free;
   end;
end;

// SaveToFile
//
procedure TGLMaterialLibrary.SaveToFile(const fileName : String);
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
procedure TGLMaterialLibrary.LoadFromFile(const fileName : String);
var
   fs : TFileStream;
begin
   fs:=TFileStream.Create(fileName, fmOpenRead+fmShareDenyNone);
   try
      LoadFromStream(fs);
   finally
      fs.Free;
   end;
end;

// AddTextureMaterial
//
function TGLMaterialLibrary.AddTextureMaterial(const materialName, fileName : String) : TGLLibMaterial;
begin
   Result:=Materials.Add;
   with Result do begin
      Name:=materialName;
      with Material.Texture do begin
         MinFilter:=miLinearMipmapLinear;
         MagFilter:=maLinear;
         TextureMode:=tmModulate;
         Disabled:=False;
         Image.LoadFromFile(fileName);
      end;
   end;
end;

// ApplyMaterial
//
function TGLMaterialLibrary.ApplyMaterial(const materialName : String; var rci : TRenderContextInfo) : Boolean;
begin
//   Assert(FLastAppliedMaterial=nil, 'Unbalanced material application');
   FLastAppliedMaterial:=Materials.GetLibMaterialByName(materialName);
   Result:=Assigned(FLastAppliedMaterial);
   if Result then
      FLastAppliedMaterial.Apply(rci);
end;

// UnApplyMaterial
//
procedure TGLMaterialLibrary.UnApplyMaterial(var rci : TRenderContextInfo);
begin
   if Assigned(FLastAppliedMaterial) then begin
      FLastAppliedMaterial.UnApply(rci);
      FLastAppliedMaterial:=nil;
   end;// else Assert(False, 'Unbalanced material un-application');
end;

// ------------------
// ------------------ TGLColorManager ------------------
// ------------------

// Find Color
//
function TGLColorManager.FindColor(AName: String): TColorVector;
var
   i : Integer;
begin
   Result:=clrBlack;
   for i:=0 to Count-1 do
      if CompareText(TColorEntry(Items[i]^).Name, AName)=0 then begin
         Result:=TColorEntry(Items[i]^).Color;
         Break;
      end;
end;

//------------------------------------------------------------------------------

function TGLColorManager.GetColor(AName: String): TColorVector;
var
   WorkCopy  : String;
   Delimiter : Integer;
begin
  WorkCopy:=Trim(AName);
  if AName[1] in ['(','[','<'] then WorkCopy:=Copy(WorkCopy,2,Length(AName)-2);
  if CompareText(Copy(WorkCopy,1,3),'clr') = 0 then Result:=FindColor(WorkCopy)
                                               else
  try
    // initialize result
    Result:=clrBlack;
    WorkCopy:=Trim(WorkCopy);
    Delimiter:=Pos(' ',WorkCopy);
    if (Length(WorkCopy) > 0) and (Delimiter > 0) then
    begin
      Result[0]:=StrToFloat(Copy(WorkCopy,1,Delimiter-1));
      System.Delete(WorkCopy,1,Delimiter);
      WorkCopy:=TrimLeft(WorkCopy);
      Delimiter:=Pos(' ',WorkCopy);
      if (Length(WorkCopy) > 0) and (Delimiter > 0) then
      begin
        Result[1]:=StrToFloat(Copy(WorkCopy,1,Delimiter-1));
        System.Delete(WorkCopy,1,Delimiter);
        WorkCopy:=TrimLeft(WorkCopy);
        Delimiter:=Pos(' ',WorkCopy);
        if (Length(WorkCopy) > 0) and (Delimiter > 0) then
        begin
          Result[2]:=StrToFloat(Copy(WorkCopy,1,Delimiter-1));
          System.Delete(WorkCopy,1,Delimiter);
          WorkCopy:=TrimLeft(WorkCopy);
          Result[3]:=StrToFloat(WorkCopy);
        end
        else Result[2]:=StrToFloat(WorkCopy);
      end
      else Result[1]:=StrToFloat(WorkCopy);
    end
    else Result[0]:=StrToFloat(WorkCopy);
  except
    ShowMessage('Wrong vector format. Use: ''<red green blue alpha>''!');
    Abort;
  end;
end;

//------------------------------------------------------------------------------

function TGLColorManager.GetColorName(AColor: TColorVector): String;

const MinDiff = 1e-6;

var I : Integer;

begin
  for I:=0 to Count-1 do
    with TColorEntry(Items[I]^) do
      if (Abs(Color[0]-AColor[0]) < MinDiff) and
         (Abs(Color[1]-AColor[1]) < MinDiff) and
         (Abs(Color[2]-AColor[2]) < MinDiff) and
         (Abs(Color[3]-AColor[3]) < MinDiff) then Break;
  if I < Count then Result:=TColorEntry(Items[I]^).Name
               else
      Result:=Format('<%.3f %.3f %.3f %.3f>',[AColor[0],AColor[1],AColor[2],AColor[3]]);
end;

//------------------------------------------------------------------------------

destructor TGLColorManager.Destroy;

var I : Integer;

begin
  for I:=0 to Count-1 do FreeMem(Items[I],SizeOf(TColorEntry));
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TGLColorManager.AddColor(AName: String; AColor: TColorVector);

var NewEntry : PColorEntry;

begin
  New(NewEntry);
  if NewEntry = nil then raise Exception.Create('Could not allocate memory for color registration!');
  with NewEntry^ do
  begin
    Name:=AName;
    Color:=AColor;
  end;
  Add(NewEntry);
end;

//------------------------------------------------------------------------------

procedure TGLColorManager.EnumColors(Proc: TGetStrProc);

var I : Integer;

begin
  for I:=0 to Count-1 do Proc(TColorEntry(Items[I]^).Name);
end;

//------------------------------------------------------------------------------

procedure TGLColorManager.RegisterDefaultColors;

begin
  Capacity:=150;
  AddColor('clrBlack',clrBlack);
  AddColor('clrGray05',clrGray05);
  AddColor('clrGray10',clrGray10);
  AddColor('clrGray15',clrGray15);
  AddColor('clrGray20',clrGray20);
  AddColor('clrGray25',clrGray25);
  AddColor('clrGray30',clrGray30);
  AddColor('clrGray35',clrGray35);
  AddColor('clrGray40',clrGray40);
  AddColor('clrGray45',clrGray45);
  AddColor('clrGray50',clrGray50);
  AddColor('clrGray55',clrGray55);
  AddColor('clrGray60',clrGray60);
  AddColor('clrGray65',clrGray65);
  AddColor('clrGray70',clrGray70);
  AddColor('clrGray75',clrGray75);
  AddColor('clrGray80',clrGray80);
  AddColor('clrGray85',clrGray85);
  AddColor('clrGray90',clrGray90);
  AddColor('clrGray95',clrGray95);
  AddColor('clrWhite',clrWhite);
  AddColor('clrDimGray',clrDimGray);
  AddColor('clrGray',clrGray);
  AddColor('clrLightGray',clrLightGray);
  AddColor('clrAquamarine',clrAquamarine);
  AddColor('clrBakersChoc',clrBakersChoc);
  AddColor('clrBlueViolet',clrBlueViolet);
  AddColor('clrBrass',clrBrass);
  AddColor('clrBrightGold',clrBrightGold);
  AddColor('clrBronze',clrBronze);
  AddColor('clrBronze2',clrBronze2);
  AddColor('clrBrown',clrBrown);
  AddColor('clrCadetBlue',clrCadetBlue);
  AddColor('clrCoolCopper',clrCoolCopper);
  AddColor('clrCopper',clrCopper);
  AddColor('clrCoral',clrCoral);
  AddColor('clrCornflowerBlue',clrCornflowerBlue);
  AddColor('clrDarkBrown',clrDarkBrown);
  AddColor('clrDarkGreen',clrDarkGreen);
  AddColor('clrDarkOliveGreen',clrDarkOliveGreen);
  AddColor('clrDarkOrchid',clrDarkOrchid);
  AddColor('clrDarkPurple',clrDarkPurple);
  AddColor('clrDarkSlateBlue',clrDarkSlateBlue);
  AddColor('clrDarkSlateGray',clrDarkSlateGray);
  AddColor('clrDarkSlateGrey',clrDarkSlateGrey);
  AddColor('clrDarkTan',clrDarkTan);
  AddColor('clrDarkTurquoise',clrDarkTurquoise);
  AddColor('clrDarkWood',clrDarkWood);
  AddColor('clrDkGreenCopper',clrDkGreenCopper);
  AddColor('clrDustyRose',clrDustyRose);
  AddColor('clrFeldspar',clrFeldspar);
  AddColor('clrFirebrick',clrFirebrick);
  AddColor('clrFlesh',clrFlesh);
  AddColor('clrForestGreen',clrForestGreen);
  AddColor('clrGold',clrGold);
  AddColor('clrGoldenrod',clrGoldenrod);
  AddColor('clrGreenCopper',clrGreenCopper);
  AddColor('clrGreenYellow',clrGreenYellow);
  AddColor('clrHuntersGreen',clrHuntersGreen);
  AddColor('clrIndian',clrIndian);
  AddColor('clrKhaki',clrKhaki);
  AddColor('clrLightBlue',clrLightBlue);
  AddColor('clrLightPurple',clrLightPurple);
  AddColor('clrLightSteelBlue',clrLightSteelBlue);
  AddColor('clrLightWood',clrLightWood);
  AddColor('clrLimeGreen',clrLimeGreen);
  AddColor('clrMandarinOrange',clrMandarinOrange);
  AddColor('clrMaroon',clrMaroon);
  AddColor('clrMediumAquamarine',clrMediumAquamarine);
  AddColor('clrMediumBlue',clrMediumBlue);
  AddColor('clrMediumForestGreen',clrMediumForestGreen);
  AddColor('clrMediumGoldenrod',clrMediumGoldenrod);
  AddColor('clrMediumOrchid',clrMediumOrchid);
  AddColor('clrMediumPurple',clrMediumPurple);
  AddColor('clrMediumSeaGreen',clrMediumSeaGreen);
  AddColor('clrMediumSlateBlue',clrMediumSlateBlue);
  AddColor('clrMediumSpringGreen',clrMediumSpringGreen);
  AddColor('clrMediumTurquoise',clrMediumTurquoise);
  AddColor('clrMediumViolet',clrMediumViolet);
  AddColor('clrMediumWood',clrMediumWood);
  AddColor('clrMidnightBlue',clrMidnightBlue);
  AddColor('clrNavy',clrNavy);
  AddColor('clrNavyBlue',clrNavyBlue);
  AddColor('clrNeonBlue',clrNeonBlue);
  AddColor('clrNeonPink',clrNeonPink);
  AddColor('clrNewMidnightBlue',clrNewMidnightBlue);
  AddColor('clrNewTan',clrNewTan);
  AddColor('clrOldGold',clrOldGold);
  AddColor('clrOrange',clrOrange);
  AddColor('clrOrangeRed',clrOrangeRed);
  AddColor('clrOrchid',clrOrchid);
  AddColor('clrPaleGreen',clrPaleGreen);
  AddColor('clrPink',clrPink);
  AddColor('clrPlum',clrPlum);
  AddColor('clrQuartz',clrQuartz);
  AddColor('clrRichBlue',clrRichBlue);
  AddColor('clrSalmon',clrSalmon);
  AddColor('clrScarlet',clrScarlet);
  AddColor('clrSeaGreen',clrSeaGreen);
  AddColor('clrSemiSweetChoc',clrSemiSweetChoc);
  AddColor('clrSienna',clrSienna);
  AddColor('clrSilver',clrSilver);
  AddColor('clrSkyBlue',clrSkyBlue);
  AddColor('clrSlateBlue',clrSlateBlue);
  AddColor('clrSpicyPink',clrSpicyPink);
  AddColor('clrSpringGreen',clrSpringGreen);
  AddColor('clrSteelBlue',clrSteelBlue);
  AddColor('clrSummerSky',clrSummerSky);
  AddColor('clrTan',clrTan);
  AddColor('clrThistle',clrThistle);
  AddColor('clrTurquoise',clrTurquoise);
  AddColor('clrViolet',clrViolet);
  AddColor('clrVioletRed',clrVioletRed);
  AddColor('clrVeryDarkBrown',clrVeryDarkBrown);
  AddColor('clrVeryLightPurple',clrVeryLightPurple);
  AddColor('clrWheat',clrWheat);
  AddColor('clrYellowGreen',clrYellowGreen);
  AddColor('clrGreen',clrGreen);
  AddColor('clrOlive',clrOlive);
  AddColor('clrPurple',clrPurple);
  AddColor('clrTeal',clrTeal);
  AddColor('clrRed',clrRed);
  AddColor('clrLime',clrLime);
  AddColor('clrYellow',clrYellow);
  AddColor('clrBlue',clrBlue);
  AddColor('clrFuchsia',clrFuchsia);
  AddColor('clrAqua',clrAqua);

  AddColor('clrScrollBar',clrScrollBar);
  AddColor('clrBackground',clrBackground);
  AddColor('clrActiveCaption',clrActiveCaption);
  AddColor('clrInactiveCaption',clrInactiveCaption);
  AddColor('clrMenu',clrMenu);
  AddColor('clrWindow',clrWindow);
  AddColor('clrWindowFrame',clrWindowFrame);
  AddColor('clrMenuText',clrMenuText);
  AddColor('clrWindowText',clrWindowText);
  AddColor('clrCaptionText',clrCaptionText);
  AddColor('clrActiveBorder',clrActiveBorder);
  AddColor('clrInactiveBorder',clrInactiveBorder);
  AddColor('clrAppWorkSpace',clrAppWorkSpace);
  AddColor('clrHighlight',clrHighlight);
  AddColor('clrHighlightText',clrHighlightText);
  AddColor('clrBtnFace',clrBtnFace);
  AddColor('clrBtnShadow',clrBtnShadow);
  AddColor('clrGrayText',clrGrayText);
  AddColor('clrBtnText',clrBtnText);
  AddColor('clrInactiveCaptionText',clrInactiveCaptionText);
  AddColor('clrBtnHighlight',clrBtnHighlight);
  AddColor('clr3DDkShadow',clr3DDkShadow);
  AddColor('clr3DLight',clr3DLight);
  AddColor('clrInfoText',clrInfoText);
  AddColor('clrInfoBk',clrInfoBk);
end;

//------------------------------------------------------------------------------

procedure TGLColorManager.RemoveColor(AName: String);
var
  I : Integer;
begin
  for I:=0 to Count-1 do
    if CompareText(TColorEntry(Items[I]^).Name,AName) = 0 then
    begin
      Delete(I);
      Break;
	 end;
end;

// ConvertWinColor
//
function ConvertWinColor(aColor : TColor; alpha : Single = 1) : TColorVector;
var
   winColor : Integer;
begin
	// Delphi color to Windows color
   winColor:=ColorToRGB(AColor);
   // convert 0..255 range into 0..1 range
   Result[0]:=(winColor and $FF)/255;
   Result[1]:=((winColor shr 8) and $FF)/255;
   Result[2]:=((winColor shr 16) and $FF)/255;
   Result[3]:=alpha;
end;

//------------------------------------------------------------------------------

function ConvertColorVector(AColor: TColorVector): TColor;
begin
  Result := RGB(Round(255 * AColor[0]), Round(255 * AColor[1]), Round(255 * AColor[2]));
end;

//------------------------------------------------------------------------------

function ConvertRGBColor(AColor: array of Byte): TColorVector;
begin
  // convert 0..255 range into 0..1 range
  Result[0] := AColor[0] / 255;
  if High(AColor) > 0 then Result[1] := AColor[1] / 255
							 else Result[1] := 0;
  if High(AColor) > 1 then Result[2] := AColor[2] / 255
							 else Result[2] := 0;
  if High(AColor) > 2 then Result[3] := AColor[3] / 255
							 else Result[3] := 1;
end;

procedure InitWinColors;
begin
  clrScrollBar:=ConvertWinColor(clScrollBar);
  clrBackground:=ConvertWinColor(clBackground);
  clrActiveCaption:=ConvertWinColor(clActiveCaption);
  clrInactiveCaption:=ConvertWinColor(clInactiveCaption);
  clrMenu:=ConvertWinColor(clMenu);
  clrWindow:=ConvertWinColor(clWindow);
  clrWindowFrame:=ConvertWinColor(clWindowFrame);
  clrMenuText:=ConvertWinColor(clMenuText);
  clrWindowText:=ConvertWinColor(clWindowText);
  clrCaptionText:=ConvertWinColor(clCaptionText);
  clrActiveBorder:=ConvertWinColor(clActiveBorder);
  clrInactiveBorder:=ConvertWinColor(clInactiveBorder);
  clrAppWorkSpace:=ConvertWinColor(clAppWorkSpace);
  clrHighlight:=ConvertWinColor(clHighlight);
  clrHighlightText:=ConvertWinColor(clHighlightText);
  clrBtnFace:=ConvertWinColor(clBtnFace);
  clrBtnShadow:=ConvertWinColor(clBtnShadow);
  clrGrayText:=ConvertWinColor(clGrayText);
  clrBtnText:=ConvertWinColor(clBtnText);
  clrInactiveCaptionText:=ConvertWinColor(clInactiveCaptionText);
  clrBtnHighlight:=ConvertWinColor(clBtnHighlight);
  clr3DDkShadow:=ConvertWinColor(cl3DDkShadow);
  clr3DLight:=ConvertWinColor(cl3DLight);
  clrInfoText:=ConvertWinColor(clInfoText);
  clrInfoBk:=ConvertWinColor(clInfoBk);
end;

procedure RegisterColor(AName: String; AColor: TColorVector);
begin
  ColorManager.AddColor(AName,AColor);
end;

procedure UnregisterColor(AName: String);
begin
  ColorManager.RemoveColor(AName);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	InitWinColors;
	RegisterGLTextureImageClass(TGLPersistentImage);
	RegisterGLTextureImageClass(TGLPicFileImage);
   RegisterClasses([TGLMaterialLibrary]);

finalization

	vColorManager.Free;
	vGLTextureImageClasses.Free;

end.
