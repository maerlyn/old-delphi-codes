{: Fading interface sample.<p>

	This is a smoother (but more CPU and framerate intensive) approach to
	picking objects : when the pointer hovers on an object, it smoothly becomes
	red, and when it moves away it progressively turns back to grey.<p>

	It is implemented here using a shared field, "currentPick" (by shared,
	I mean it's a form field used in more than one event) indicating the
	object the mouse is currently hovering, a classic timer and the "Progress"
	chain of events.<p>

	Using a timer is convenient and, in this case, serves the sample well,
	but be aware this is no "perfect" solution : if the cpu is not able to
	keep up with the timer, your interface will lag, if the cpu/3Ddevice is
	faster, you'll have an interface that is not as smooth as it could be.<br>
	Alas, it keeps the code simple and allows me to drop a line on this
	commonly seen problem ;).<br>
	Check other samples for better framerate independance techniques
   (and use the TGLCadencer !).<p>

	Note that all objects (sphere, torus...) share the same event.
}
unit Unit1;

interface

uses
  Forms, GLScene, GLObjects, GLMisc, GLTexture, Classes, Controls, Dialogs,
  ExtCtrls, SysUtils, Geometry;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TSphere;
	 Cylinder: TCylinder;
    Torus: TTorus;
    Cone: TCone;
	 Timer1: TTimer;
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
		X, Y: Integer);
	 procedure GLSceneViewer1MouseDown(Sender: TObject;
		Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure Timer1Timer(Sender: TObject);
	 procedure SphereProgress(Sender: TObject; const deltaTime,
		newTime: Double);
  private
	 { Déclarations privées }
	 currentPick : TGLCustomSceneObject;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
	// get what is under the mouse
	currentPick:=(GLSceneViewer1.GetPickedObject(x, y) as TGLCustomSceneObject);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	pick : TGLCustomSceneObject;
begin
	// if an object is picked...
	pick:=(GLSceneViewer1.GetPickedObject(x, y) as TGLCustomSceneObject);
	if Assigned(pick) then begin
		// ...turn it to yellow and show its name
		pick.Material.FrontProperties.Emission.Color:=clrYellow;
		ShowMessage('You clicked the '+pick.Name);
	end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	// trigger progression (we don't use time in this sample)
	GLScene1.Progress(0, 0);
end;

procedure TForm1.SphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
	targetColor : TColorVector;
begin
	with Sender as TGLCustomSceneObject do begin
		// if we are picked, target color is red, else it is black (no emission)
		if Sender=currentPick then
			targetColor:=clrRed
		else targetColor:=clrBlack;
		// Set new color at 66% between current and target color
		with Material.FrontProperties.Emission do
			Color:=VectorLerp(targetColor, Color, 0.66)
	end;
end;

end.
