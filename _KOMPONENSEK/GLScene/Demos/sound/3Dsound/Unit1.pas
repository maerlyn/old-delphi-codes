{: 3D Sound sample (FMOD and BASS managers are used in this sample).<p>

   This sample has a moving red sound source with a looping sound, and a "mickey"
   listener that you can move around using the trackbars.<p>

   You already know the TGLScene, TGLSceneViewer, the TTimer is just used for
   regularly updating the Form's caption with FPS and CPU usage stats. You also
   know the TGLCadencer, but here, it not only cadenced the scene, but is also
   referred and used by the sound managers TGLSMFMOD and TGLSMBASS.<p>

   A TGLSoundLibrary is used to load and store the sample, a 44kHz WAV file. The
   sound library can be used to embed sound files in the application, you just
   have to add a sample at design-time and this removes the need for an external
   file, but for our samples, we share a single wav files among all demos.
   Sound libraries are used to store sound samples, you may only play a sample
   that is available in library (you can add/remove sample dynamically).<p>

   We also have sound manager. There can only be one *active* sound manager in
   any app at any time, it serves as an interface to a low-level sound API.
   3D sounds are dynamic things, the easiest way to achieve this is to connect
   the manager to the cadencer by setting its "Cadencer" property, this way,
   it will get updated regularly.<p>

   And now, the last part : a sound emitter behaviour has been attached to the
   red sphere, in this behaviour we specify a sample (by selecting a sound
   library, and a sample in the sound library). The "NbLoops" property was also
   adjusted, to keep our sound looping (playing again and again).<p>

   That's basicly all you need to use GLScene Sound System. Note however, that
   depending on the low-level API you chose (ie. sound manager), some features
   amy or may not be available, but you don't need to worry about that, if
   a feature is unavailable on a particular driver, it will just be ignored.<p>

   Raw performance comparisons between FMOD and BASS on this sample would be
   unfair because:<ul>
   <li>BASS 3D sound quality is higher (and BASS support sound cones)
   <li>BASS uses DirectSound, while FMOD uses WinMM output (couldn't get FMOD
       to work reliably in DirectSound mode)
   </ul>However, CPU use stays extremely low in both cases.
}
unit Unit1;

interface

uses
  Classes, Forms, ExtCtrls, GLCadencer, GLScene, GLObjects, GLMisc,
  GLSound, GLSMFMOD, ComCtrls, Controls, GLSMBASS, StdCtrls;

type
  TForm1 = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube: TDummyCube;
    Sphere: TSphere;
    GLLightSource: TGLLightSource;
    GLSMFMOD: TGLSMFMOD;
    GLSoundLibrary: TGLSoundLibrary;
    GLCadencer1: TGLCadencer;
    Timer: TTimer;
    Mickey: TSphere;
    Sphere2: TSphere;
    Sphere3: TSphere;
    Cone1: TCone;
    TrackBar: TTrackBar;
    Plane1: TPlane;
    Disk1: TDisk;
    Torus1: TTorus;
    TrackBar1: TTrackBar;
    GLSMBASS: TGLSMBASS;
    Panel1: TPanel;
    Label1: TLabel;
    RBBass: TRadioButton;
    RBFMOD: TRadioButton;
    Button1: TButton;
    procedure SphereProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TimerTimer(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RBFMODClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Geometry, SysUtils;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Load our sound sample
   GLSoundLibrary.Samples.AddFile('..\..\media\drumloop.wav');
   GLSoundLibrary.Samples.AddFile('..\..\media\chimes.wav');
end;

procedure TForm1.SphereProgress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   alpha : Single;
begin
   // Move the red sphere (sound source) along an elliptic path
   alpha:=60*DegToRad(newTime);
   TSphere(Sender).Position.SetPoint(sin(alpha)*2, 0.5, cos(alpha)*5);
end;

procedure TForm1.TrackBarChange(Sender: TObject);
begin
   // Rotate the listener around the vertical axis
   DummyCube.TurnAngle:=TrackBar.Position;
   Application.ProcessMessages;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
   // Move the listener forward/back
   Mickey.Position.Z:=TrackBar1.Position/10;
   Application.ProcessMessages;
end;

procedure TForm1.TimerTimer(Sender: TObject);
var
   mngName : String;
begin
   // some stats
   if ActiveSoundManager is TGLSMBASS then
      mngName:='BASS'
   else if ActiveSoundManager is TGLSMBASS then
      mngName:='FMOD'
   else mngName:='DSound';
   Caption:=Format('%.2f FPS, %s CPU use : %.2f%%',
                   [GLSceneViewer.FramesPerSecond, mngName,
                    ActiveSoundManager.CPUUsagePercent]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

procedure TForm1.RBFMODClick(Sender: TObject);
var
   newManager : TGLSoundManager;
begin
   // This method switches managers. On a real world project, this would never
   // happen: you would choose and API and then cling to it, but the GLSS
   // completely wraps the underlying complexity and makes it a snap
   if RBFMOD.Checked then
      newManager:=GLSMFMOD
   else newManager:=GLSMBASS;
   if newManager<>ActiveSoundManager then begin
      // shut down current one, and activate the new one
      if ActiveSoundManager<>nil then
         ActiveSoundManager.Active:=False;
      newManager.Active:=True;
      // restart sound
      GetOrCreateSoundEmitter(Sphere).Playing:=True;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   with TGLBSoundEmitter.Create(Mickey.Behaviours) do begin
      Source.SoundLibrary:=GLSoundLibrary;
      Source.SoundName:='chimes.wav';
      Playing:=True;
   end;
end;

end.
