{: This Form demonstrates basic "manual" movements.<p>

	Positions are computed directly using Sin/Cos functions.<p>

	A cadencer is used to "play" the animation, it is not used as a time-controler,
   but just as a way to push the animation as fast as possible. See further
   samples on framerate independance to see how it can be better used.<br>

	Note : when using 3Dfx OPENGL and a Voodoo3 on Win9x in 24bits resolution,
	the driver always uses internal double-buffering (since it can only render
	in 16bits), and keeping the requesting double-buffering in the TGLSceneViewer
	actually results in a "quadruple-buffering"...
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, ComCtrls, GLMisc, ExtCtrls, StdCtrls,
  Classes, Controls, Dialogs, GLCadencer;

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
    GLCadencer1: TGLCadencer;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
	// the "sun" turns slowly around Y axis
	Cube1.TurnAngle:=t/4;
	// "earth" rotates around the sun on the Y axis
	with Cube2.Position do begin
		X:=3*cos(DegToRad(t));
		Z:=3*sin(DegToRad(t));
	end;
	// "moon" rotates around earth on the X axis
	with Cube3.Position do begin
		X:=Cube2.Position.X;
		Y:=Cube2.Position.Y+1*cos(DegToRad(3*t));
		Z:=Cube2.Position.Z+1*sin(DegToRad(3*t));
   end;
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

end.
