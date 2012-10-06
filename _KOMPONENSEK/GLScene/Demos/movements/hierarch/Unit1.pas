{: This Form demonstrates basic "hierarchical" movements.<p>

	Our visible (cube) objects are children of TDummyCube object and we move
	them through rotations of the DummyCubes.<br>
	Movements in this demo differs from the "manual" demo : "earth" now spins,
	"moon" has an inclined orbit and spins too.<p>

	You may use any GLScene object when building hierarchical scenes, but it
	is recommended to use TDummyCube for structural (regroupment only) objects,
	since dummycube are just "virtual" at run-time and cost no OpenGL setup or
	rendering time.
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, ComCtrls, GLMisc, ExtCtrls, StdCtrls,
  AsyncTimer, Classes, Controls, GLCadencer;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar: TTrackBar;
    Cube1: TCube;
    Cube3: TCube;
    Cube2: TCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    CBPlay: TCheckBox;
    StaticText1: TStaticText;
    DummyCube1: TDummyCube;
    DummyCube2: TDummyCube;
    GLCadencer1: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

uses Math, SysUtils;

procedure TForm1.TrackBarChange(Sender: TObject);
var
   t : Integer;
begin
	t:=TrackBar.Position;
	// the "sun" spins slowly
	Cube1.TurnAngle:=t/4;
	// "earth" rotates around the sun and spins
	DummyCube1.TurnAngle:=-t;
	Cube2.TurnAngle:=t*2;
	// "moon" rotates around earth and spins
	DummyCube2.RollAngle:=3*t;
	Cube3.TurnAngle:=4*t;
   // update FPS count
   StaticText1.Caption:=IntToStr(Trunc(GLSceneViewer1.FramesPerSecond))+' FPS';
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	if CBPlay.Checked and Visible then begin
		// simulate a user action on the trackbar...
		TrackBar.Position:=((TrackBar.Position+1) mod 360);
   end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
	GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	// We need to stop playing here :
	// 	since the timer is asynchronous, if we don't stop play,
	// 	it may get triggered during the form's destruction
	CBPlay.Checked:=False;
end;

end.
