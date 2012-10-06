{: Basic particle system.<p>

   This is a very basic use of the particle systems in GLScene : colored
   alos (sprites) are created randomly with fade-in and fade-out effects, and
   the whole particle system rotates slowly (particles do not move in this
   sample). Particles live for 10 seconds, and are created every 300 ms.
   An inertia behaviour takes care of the rotation and cadencer makes the whole
   thing move.<p>

   TGLParticles works with a "template", this the mother of all particles, and
   it is duplicated when a new particle is requested. The template is the
   first (top) child of TGLParticles, other children are considered to be
   particles (don't temper directly with TGLParticles children !). In this
   sample, a sprite is the only child, and as such make a simple particle
   template, particles can be very complex : if the sprite was having children,
   these would be part of the particle too, and their children and the children
   of their children and... you got it.<p>

   Some eye candy here, but if you don't have a 3D hardware, reduce the window
   size to avoid slowdown. This one could make a nice screen-saver, this is
   left as an exercice to reader (hint : you just need to drop 1 component,
   type in 3 characters and press CTRL+F9).
}
unit Unit1;

interface

uses
  Forms, GLScene, GLMisc, GLObjects, GLParticles, StdCtrls, GLCadencer, ExtCtrls,
  GLBehaviours, Classes, Controls, Geometry, SysUtils;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLParticles1: TGLParticles;
    Sprite1: TSprite;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject);
    procedure Sprite1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   // if we don't do this, our random won't look like random
   Randomize;
end;

procedure TForm1.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
begin
   // this event is called when a particle is activated,
   // ie. just before it will be rendered
   with TSprite(particle) do begin
      with Material.FrontProperties do begin
         // we pick a random color
         Emission.Color:=PointMake(Random, Random, Random);
         // our halo starts transparent
         Diffuse.Alpha:=0;
      end;
      // this is our "birth time"
      TagFloat:=GLCadencer1.CurrentTime;
   end;
end;

procedure TForm1.Sprite1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   life : Double;
begin
   with TSprite(Sender) do begin
      // calculate for how long we've been living
      life:=(newTime-TagFloat);
      if life>10 then
         // old particle to kill
         GLParticles1.KillParticle(TSprite(Sender))
      else if life<1 then
         // baby particles become brighter in their 1st second of life...
         Material.FrontProperties.Diffuse.Alpha:=life
      else // ...and slowly disappear in the darkness
         Material.FrontProperties.Diffuse.Alpha:=(9-life)/9;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // every timer, we create a particle at a random position
   with TSprite(GLParticles1.CreateParticle).Position do begin
      X:=3*(Random-0.5);
      Y:=3*(Random-0.5);
      Z:=3*(Random-0.5);
   end;
   // infos for the user
   Caption:=Format('%d particles, %.1f FPS',
                   [GLParticles1.Count-1, GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   // change focal so the view will shrink and not just get clipped
   GLCamera1.FocalLength:=50*Width/280;
end;

end.
