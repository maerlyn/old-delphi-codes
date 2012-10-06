{: Using materials in a TDirectOpenGL OnRender.<p>

   This demo shows how to dynamically create materials in a material library
   and use them in a TDirectOpenGL to render your own stuff.<br>
   The render is quite simple: two quads, each with its own texture. The
   TDirectOpenGL is placed in a small hierarchy with a torus and dummy cube,
   and the rotation animation are handled by those two object to show that
   the OnRender code uses the hierarchy.<p>

   Pay attention to the need to free the materials we allocated *before* the
   TGLSceneViewer (here, in FormCloseQuery).
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLCadencer, GLScene, GLObjects, GLTexture, GLMisc, GLBehaviours;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    Torus1: TTorus;
    DirectOpenGL1: TDirectOpenGL;
    GLLightSource1: TGLLightSource;
    GLCadencer1: TGLCadencer;
    procedure DirectOpenGL1Render(var rci: TRenderContextInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses OpenGL12, JPeg;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // dynamically create 2 materials and load 2 textures
   with GLMaterialLibrary do begin
      with AddTextureMaterial('wood', '..\..\media\ashwood.jpg') do
         Material.FrontProperties.Emission.Color:=clrGray50;
      with AddTextureMaterial('stone', '..\..\media\walkway.jpg') do
         Material.FrontProperties.Emission.Color:=clrGray50;
   end;
end;

procedure TForm1.DirectOpenGL1Render(var rci: TRenderContextInfo);
var
   material : TGLLibMaterial;
begin
   // disable face culling
   glDisable(GL_CULL_FACE);
   // 1st quad, textured with 'wood', using standard method
   GLMaterialLibrary.ApplyMaterial('wood', rci);
   glBegin(GL_QUADS);
      glVertex3f(0.5, 0.5, -0.5);   glTexCoord2f(0, 1);
      glVertex3f(-0.5, 0.5, -0.5);  glTexCoord2f(0, 0);
      glVertex3f(-0.5, 0, 0.5);     glTexCoord2f(1, 0);
      glVertex3f(0.5, 0, 0.5);      glTexCoord2f(1, 1);
   glEnd;
   GLMaterialLibrary.UnApplyMaterial(rci);
   // 2nd quad, textured with 'stone'
   // we "manually" apply the material, this can be usefull if you want to have
   // some dynamic material control
   material:=GLMaterialLibrary.Materials.GetLibMaterialByName('stone');
   material.Material.Apply(rci);
   glBegin(GL_QUADS);
      glVertex3f(0.5, -0.5, -0.5);  glTexCoord2f(0, 1);
      glVertex3f(0.5, 0, 0.5);      glTexCoord2f(0, 0);
      glVertex3f(-0.5, 0, 0.5);     glTexCoord2f(1, 0);
      glVertex3f(-0.5, -0.5, -0.5); glTexCoord2f(1, 1);
   glEnd;
   material.Material.UnApply(rci);
   // enable face culling again
   glEnable(GL_CULL_FACE);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   // This is required to allow proper cleanup of the materials
   // (texture handles etc.) since the library and its material is never
   // referred by GLScene objects, only our code.
   // Whenever you happen to use a library in similar conditions, make sure
   // the library materials are freed *before* the TGLSceneViewer
   GLMaterialLibrary.DestroyHandles(False);
end;

end.
