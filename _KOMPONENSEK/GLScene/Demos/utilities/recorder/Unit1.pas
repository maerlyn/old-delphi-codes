{: This sample demonstrates use of the TAVIRecorder to create an AVI file.<p>

   The animation is taken from the "Hierarchy" sample, all the recording takes
   place in Button1Click.<p>

   Be aware that if you use default compression, you will likely get a lossless,
   low compression codec (which may be good if you want the highest quality),
   but you can specify a codec, for instance DiVX if you installed it, for high
   compression video.<br>
   The codec can be choosed with the Compressor property of TAVIRecorder.
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, ComCtrls, GLMisc, ExtCtrls, StdCtrls,
  AsyncTimer, Classes, Controls, GLCadencer, AVIRecorder;

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
    Button1: TButton;
    AVIRecorder1: TAVIRecorder;
    procedure TrackBarChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Button1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Déclarations privées }
     UserAbort : boolean;
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
        AVIRecorder1.Width:=GLSceneViewer1.Width;
        AVIRecorder1.Height:=GLSceneViewer1.Height;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	// We need to stop playing here :
	// 	since the timer is asynchronous, if we don't stop play,
	// 	it may get triggered during the form's destruction
	CBPlay.Checked:=False;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
    SavedCap : string;
begin
   if not AVIRecorder1.CreateAVIFile then Exit;
   // if AVIRecorder1.filename is empty, a dialog box will appear asking
   // for the filename. CreateAVIFile() will return a bool
   // indicating if user presses "cancel" in the dialog box.

   SavedCap:=caption;
   caption:='Press ESC to abort';
   UserAbort:=false;
   StaticText1.Visible:=false; // the FPS shown is not correct now,
                               // so just hide it for the time being.
   i:=0;

   Button1.enabled:=false;
   TrackBar.enabled:=false;

   try
      while (i<360) and not UserAbort do
      begin
         TrackBar.Position:=i;
         TrackBarChange(self);

         AVIRecorder1.AddAVIFrame;

         // you might want to update your progress bar here.

         Application.ProcessMessages; // so that our app. is not freezed,
                                      // and will accept user abort.
         inc(i);
      end;
   finally
      AVIRecorder1.CloseAVIFile(UserAbort); // if UserAbort, CloseAVIFile will
                                            // also delete the unfinished file.
      caption:=SavedCap;
      StaticText1.Visible:=true;
      Button1.enabled:=true;
      TrackBar.enabled:=true;
   end;

end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  UserAbort:=key=#27;
end;

end.
