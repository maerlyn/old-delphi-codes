{: This sample illustrates basic user-driven camera movements.<p>

	I'm using the GLScene built-in camera movement methods. The camera object is
	a child of its target dummy cube (this means that the camera is translated
	when its target is translate, which is good for flyover/scrolling movements).<p>

	Movements in this sample are done by moving the mouse with a button
	pressed, left button will translate the dummy cube (and the camera),
	right button will rotate the camera around the target.<br>
	Mouse Wheel allows zooming in/out.<br>
	Alternately, plus/minus and '8', '4', '6', '2' keys can be used for
	camera movements.
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, GLMisc, Classes, Controls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    Teapot1: TTeapot;
    GLLightSource1: TGLLightSource;
    DummyCube1: TDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
	 mdx, mdy : Integer;
  public
	 { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Geometry, Math;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	// store mouse coordinates when a button went down
	mdx:=x; mdy:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
	dx, dy : Integer;
	v : TVector;
begin
	// calculate delta since last move or last mousedown
	dx:=mdx-x; dy:=mdy-y;
	mdx:=x; mdy:=y;
	if Shift=[ssRight] then
		// right button changes camera angle
		// (we're moving around the parent and target dummycube)
		GLCamera1.MoveAroundTarget(dy, dx)
	else if Shift=[ssLeft] then begin
		// left button moves our target and parent dummycube
		v:=GLCamera1.ScreenDeltaToVectorXY(dx, -dy,
							0.12*GLCamera1.DistanceToTarget/GLCamera1.FocalLength);
		DummyCube1.Position.Translate(v);
		// notify camera that its position/target has been changed
		GLCamera1.TransformationChanged;
	end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	// Note that 1 wheel-step induces a WheelDelta of 120,
	// this code adjusts the distance to target with a 10% per wheel-step ratio
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
	case Key of
		'2' : GLCamera1.MoveAroundTarget( 3,  0);
		'4' : GLCamera1.MoveAroundTarget( 0, -3);
		'6' : GLCamera1.MoveAroundTarget( 0,  3);
		'8' : GLCamera1.MoveAroundTarget(-3,  0);
		'-' : GLCamera1.AdjustDistanceToTarget(1.1);
		'+' : GLCamera1.AdjustDistanceToTarget(1/1.1);
	end;
end;

end.
