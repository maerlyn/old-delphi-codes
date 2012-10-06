// GLHUDObjects
{: GLScene objects that get rendered in 2D coordinates<p>

	<b>History : </b><font size=-1><ul>
      <li>21/02/01 - Egg - Now XOpenGL based (multitexture)
	   <li>15/01/01 - EG - Creation
	</ul></font>
}
unit GLHUDObjects;

interface

uses Classes, GLScene, Graphics, Geometry, GLMisc, StdCtrls, GLObjects,
   GLBitmapFont, GLTexture;

type

   // THUDSprite
   //
	{: A rectangular area, NOT perspective projected.<p>
      (x, y) coordinates map directly to the viewport (in pixels) and refer
      the center of the area.<br>
      The coordinate system is that of an equivalent TCanvas, ie. top-left
      point is the origin (0, 0).<p>
      The z component is ignored and Z-Buffer is disabled when rendering.<p>
      <b>Using THUDSprite in 2D only scenes :</b><br>
      The most convenient way to use a THUDSprite as a simple 2D sprite with
      blending capabilities (transparency or additive), is to set the texture
      mode to tmModulate, in FrontProperties, to use the Emission color to
      control coloring/intensity, and finally use the Diffuse color's alpha
      to control transparency (while setting the other RGB components to 0).<br>
      You can also control aplha-blending by defining a <1 value in the sprite's
      AlphaChannel field. This provides you with hardware accelerated,
      alpha-blended blitting.<p>
      Note : since THUDSprite works in absolute coordinates, TGLProxyObject
      can't be used to duplicate an hud sprite. }
	THUDSprite = class (TSprite)
		public
			{ Public Declarations }
         procedure DoRender(var rci : TRenderContextInfo); override;
   end;

   // THUDText
   //
	THUDText = class (TGLImmaterialSceneObject)
	   private
	      { Private Declarations }
         FBitmapFont : TBitmapFont;
         FText : String;
         FRotation : Single;
         FAlignment : TAlignment;
         FLayout : TTextLayout;

	   protected
	      { Protected Declarations }
         procedure SetBitmapFont(const val : TBitmapFont);
         procedure SetText(const val : String);
         procedure SetRotation(const val : Single);
         procedure SetAlignment(const val : TAlignment);
         procedure SetLayout(const val : TTextLayout);

         procedure Notification(AComponent: TComponent; Operation: TOperation); override;

		public
			{ Public Declarations }
         constructor Create(AOwner : TComponent); override;
         destructor Destroy; override;

         procedure DoRender(var rci : TRenderContextInfo); override;

	   published
	      { Published Declarations }
         property BitmapFont : TBitmapFont read FBitmapFont write SetBitmapFont;
         property Text : String read FText write SetText;
         property Rotation : Single read FRotation write SetRotation;
         property Alignment : TAlignment read FAlignment write SetAlignment;
         property Layout : TTextLayout read FLayout write SetLayout;
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
// ------------------ THUDSprite ------------------
// ------------------

// DoRender
//
procedure THUDSprite.DoRender(var rci : TRenderContextInfo);
var
	vx, vy, vx1, vy1 : Single;
begin
  	Material.Apply(rci);
   if AlphaChannel<>1 then
      SetGLMaterialAlphaChannel(GL_FRONT, AlphaChannel);
   // Prepare matrices
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentViewer.BaseProjectionMatrix);
   glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
   glTranslatef(Position.X-rci.viewPortSize.cx/2,
                rci.viewPortSize.cy/2-Position.Y, Position.Z);
   if Rotation<>0 then
      glRotatef(Rotation, 0, 0, 1);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_DEPTH_TEST);
   // precalc coordinates
   vx:=-Width/2;    vx1:=vx+Width;
   vy:=+Height/2;   vy1:=vy-Height;
   // issue quad
	glBegin(GL_QUADS);
      glNormal3fv(@YVector);
      xglTexCoord2f(0, 0);  glVertex2f( vx, vy1);
      xglTexCoord2f(1, 0);  glVertex2f(vx1, vy1);
      xglTexCoord2f(1, 1);  glVertex2f(vx1,  vy);
      xglTexCoord2f(0, 1);  glVertex2f( vx,  vy);
	glEnd;
   // restore state
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
   Material.UnApply(rci);
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// ------------------
// ------------------ THUDText ------------------
// ------------------

// Create
//
constructor THUDText.Create(AOwner : TComponent);
begin
   inherited;
   ObjectStyle:=ObjectStyle+[osDirectDraw];
end;

// Destroy
//
destructor THUDText.Destroy;
begin
   BitmapFont:=nil;
   inherited;
end;

// Notification
//
procedure THUDText.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if (Operation=opRemove) and (AComponent=FBitmapFont) then
      BitmapFont:=nil;
end;

// SetBitmapFont
//
procedure THUDText.SetBitmapFont(const val : TBitmapFont);
begin
   if val<>FBitmapFont then begin
      if Assigned(FBitmapFont) then
         FBitmapFont.UnRegisterUser(Self);
      FBitmapFont:=val;
      if Assigned(FBitmapFont) then begin
         FBitmapFont.RegisterUser(Self);
         FBitmapFont.FreeNotification(Self);
      end;
      StructureChanged;
   end;
end;

// SetText
//
procedure THUDText.SetText(const val : String);
begin
   FText:=val;
   StructureChanged;
end;

// SetRotation
//
procedure THUDText.SetRotation(const val : Single);
begin
   FRotation:=val;
   StructureChanged;
end;

// SetAlignment
//
procedure THUDText.SetAlignment(const val : TAlignment);
begin
   FAlignment:=val;
   StructureChanged;
end;

// SetLayout
//
procedure THUDText.SetLayout(const val : TTextLayout);
begin
   FLayout:=val;
   StructureChanged;
end;

// DoRender
//
procedure THUDText.DoRender(var rci : TRenderContextInfo);
begin
   if (not Assigned(FBitmapFont)) or (Text='') then Exit;
   // Prepare matrices
   glMatrixMode(GL_MODELVIEW);
   glPushMatrix;
   glLoadMatrixf(@Scene.CurrentViewer.BaseProjectionMatrix);
   glScalef(2/rci.viewPortSize.cx, 2/rci.viewPortSize.cy, 1);
   glTranslatef(Position.X-rci.viewPortSize.cx/2,
                rci.viewPortSize.cy/2-Position.Y, Position.Z);
   if FRotation<>0 then
      glRotatef(FRotation, 0, 0, 1);
   glScalef(Scale.DirectX, Scale.DirectY, 1);
   glMatrixMode(GL_PROJECTION);
   glPushMatrix;
   glLoadIdentity;
   glPushAttrib(GL_ENABLE_BIT);
   glDisable(GL_DEPTH_TEST);
   // render text
   FBitmapFont.RenderString(Text, FAlignment, FLayout);
   // restore state
   glPopAttrib;
   glPopMatrix;
   glMatrixMode(GL_MODELVIEW);
   glPopMatrix;
   if Count>0 then
      RenderChildren(0, Count-1, rci);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
   RegisterClasses([THUDText, THUDSprite]);

end.

