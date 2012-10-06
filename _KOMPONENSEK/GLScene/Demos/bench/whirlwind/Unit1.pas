{: This bench stresses particle systems management in GLScene.<p>

	This proggy basicly creates and moves the highest quantity its framerate
	allows (one particle is created after each render, particles live for 3 sec).
	Particles are made of a dummycube (used as axis, hosting an inertia) and a
	child Sprite (untextured, unblended). Depth test is disabled.<p>

	CPU	 	 OS		Renderer				Colors 	 Particles	 FPS		Rating
	K7-500   Win98    GeForce-5.33      32 bits      255    120.2      30600
	------ 13/01/00 : Another Matrix setup change
	K6-400 	NT4	   V3-2000           16 bits		 151	  125.6      18965
	------ 22/12/00 : Matrix setup change (gluProject compatibility)
	K7-500   Win98    GeForce-5.22      32 bits      330    172.2      56826
	K6-400 	NT4	   V3-2000           16 bits		 207	  117.6      24343
	K6-400 	NT4	   Software OpenGL   24 bits		 220	  101.9      22418
	------ 28/06/00 : BuildList optimization (osDirectDraw)
	K6-400   Win98    V3-2000           16 bits		 161     71.2	    11463
	------ 20/06/00 : Geometry.pas optimizations
	K7-500   Win98    V3-2000           16 bits      183     68.8      12590
	K6-400   Win98    V3-3000           16 bits		 154     59.1	     9101
	K6-400 	NT4	   Software OpenGL   24 bits		 152		51.6       7843
	------ 18/04/00 : DummyCube and GLParticles Render optimization (glCallList)
	K7-500   Win98    V3-2000           16 bits      175     65.8      11515
	------ 18/04/00 : various profiling optimizations
	K6-400 	NT4	   Software OpenGL   24 bits		  98		32.5       3185
	K6-400 	NT4	   V3-3000   		   16 bits		 148		53.9       7977
	K7-500   Win98    V3-2000           16 bits      150     55.0       8250
   ------ 17/04/00 : base stats
}
unit Unit1;

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, GLScene, GLTexture,
  GLCadencer, GLMisc, GLObjects, GLParticles, ExtCtrls, GLBehaviours, Geometry;

type
  TForm1 = class(TForm)
	 GLSceneViewer1: TGLSceneViewer;
	 GLScene1: TGLScene;
	 GLCadencer1: TGLCadencer;
	 GLCamera1: TGLCamera;
	 GLParticles1: TGLParticles;
    Sprite1: TSprite;
    Timer1: TTimer;
    DummyCube1: TDummyCube;
    procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DummyCube1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
var
	r, alpha, cr, sr : Single;
begin
	with particle do begin
		alpha:=Random*2*PI;
		r:=2*Random;
      SinCos(alpha, r*r, sr, cr);
		Children[0].Position.SetPoint(sr, 3*r-3, cr);
		GetOrCreateInertia(particle).TurnSpeed:=Random(30);
		TGLCustomSceneObject(particle).TagFloat:=GLCadencer1.CurrentTime;
	end;
end;

procedure TForm1.DummyCube1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	with TGLCustomSceneObject(Sender) do begin
		if newTime-TagFloat>3 then
			GLParticles1.KillParticle(TGLCustomSceneObject(Sender));
	end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	Caption:=Format('%d particles, %.1f FPS',
						 [GLParticles1.Count, GLSceneViewer1.FramesPerSecond]);
	GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	GLParticles1.CreateParticle;
end;

end.
