{: Actor movement with two cameras (first-person and third-person)<p>

   The movement control is a little "doom-like" and keyboard only.<br>
   This demos mainly answers to "doom-like" movement questions and keyboard
   handling in GLScene.<p>
   The basic principle is to check which key are pressed, and for each movement
   key, multiply the movement by the deltaTime and use this value as delta
   position or angle.<p>
   The frame rate may not be that good on non-T&L accelerated board, mainly due
   to the mushrooms that are light on fillrate needs, but heavy on the polygons.<br>
   This demonstrates how badly viewport object-level clipping is needed in
   GLScene :), a fair share of rendering power is lost in projecting
   objects that are out of the viewing frustum.<p>

   TODO : 3rd person view with quaternion interpolation (smoother mvt)
          More mvt options (duck, jump...)
          Smooth animation transition for TActor
          HUD in 1st person view

   Carlos Arteaga Rivero <carteaga@superele.gov.bo>
}
unit Unit1;

interface

uses
  Windows, GLCadencer, GLVectorFileObjects, GLScene, GLObjects, GLMisc,
  StdCtrls, Buttons, Controls, ExtCtrls, ComCtrls, Classes, Forms, Graphics,
  GLSkydome;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    Disk1: TDisk;
    GLSceneViewer1: TGLSceneViewer;
    Actor1: TActor;
    Actor2: TActor;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    Timer1: TTimer;
    GLCamera2: TGLCamera;
    Label3: TLabel;
    Label4: TLabel;
    DummyCube2: TDummyCube;
    FreeForm1: TFreeForm;
    GLLightSource2: TGLLightSource;
    DummyCube3: TDummyCube;
    Label1: TLabel;
    SkyDome1: TSkyDome;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HandleKeys(const deltaTime: Double);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    procedure AddMushrooms;
  public
    { Déclarations privées }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Geometry, SysUtils, Jpeg, Keyboard;

const
  cWalkStep = 6;   // this is our walking speed, in 3D units / second
  cStrafeStep = 6; // this is our strafing speed, in 3D units / second
  cRotAngle = 60;  // this is our turning speed, in degrees / second
  cRunBoost = 2;   // speed boost when running
  cSpread = 90;
  cNbMushrooms = 15;

procedure TForm1.FormCreate(Sender: TObject);
begin
   // Load mushroom mesh
   FreeForm1.LoadFromFile('..\..\media\mushroom.3ds');

   // Duplicate our reference mushroom (but not its mesh data !)
   AddMushrooms;

   // Load Actor into GLScene
   Actor1.LoadFromFile('..\..\media\waste.md2');
   Actor1.Material.Texture.Image.LoadFromFile('..\..\media\waste.jpg');
   Actor1.Animations.LoadFromFile('..\..\media\Quake2Animations.aaf');
   Actor1.Scale.SetVector(0.04, 0.04, 0.04, 0);
   // Load weapon model and texture
   Actor2.LoadFromFile('..\..\media\WeaponWaste.md2');
   Actor2.Material.Texture.Image.LoadFromFile('..\..\media\WeaponWaste.jpg');
   Actor2.Animations.Assign(Actor1.Animations);

   // Define animation properties
   Actor1.AnimationMode:=aamLoop;
   Actor1.SwitchToAnimation('stand');
   Actor1.FrameInterpolation:=afpLinear;
   Actor2.Synchronize(Actor1);

   // Load Texture for ground disk
   Disk1.Material.Texture.Image.LoadFromFile('..\..\media\clover.jpg');
end;

procedure TForm1.HandleKeys(const deltaTime: Double);
var
   moving : String;
   boost : Single;
begin
   // This function uses asynchronous keyboard check (see Keyboard.pas)

   //Change Cameras
   if IsKeyDown(VK_F7) then begin
      GLSceneViewer1.Camera:=GLCamera1;
      Actor1.Visible:=True;
      Label4.Font.Style:=Label4.Font.Style-[fsBold];
      Label3.Font.Style:=Label3.Font.Style+[fsBold];
   end;
   if IsKeyDown(VK_F8) then begin
      GLSceneViewer1.Camera:=GLCamera2;
      Actor1.Visible:=False;
      Label4.Font.Style:=Label4.Font.Style+[fsBold];
      Label3.Font.Style:=Label3.Font.Style-[fsBold];
   end;

   // Move Actor in the scene

   // if nothing specified, we are standing
   moving:='stand';

   // first, are we running ? if yes give animation & speed a boost
   if IsKeyDown(VK_SHIFT) then begin
      Actor1.Interval:=100;
      boost:=cRunBoost*deltaTime
   end else begin
      Actor1.Interval:=150;
      boost:=deltaTime;
   end;
   Actor2.Interval:=Actor1.Interval;

   // are we advaning/backpedaling ?
   if IsKeyDown(VK_UP) then begin
      DummyCube2.Move(cWalkStep*boost);
      moving:='run';
   end;
   if IsKeyDown(VK_DOWN) then begin
      DummyCube2.Move(-cWalkStep*boost);
      moving:='run';
   end;

   // slightly more complex, depending on CTRL key, we either turn or strafe
   if IsKeyDown(VK_LEFT) then begin
      if IsKeyDown(VK_CONTROL) then
          DummyCube2.Slide(-cStrafeStep*boost)
      else DummyCube2.Turn(-cRotAngle*boost);
      moving:='run';
   end;
   if IsKeyDown(VK_RIGHT) then begin
      if IsKeyDown(VK_CONTROL) then
          DummyCube2.Slide(cStrafeStep*boost)
      else DummyCube2.turn(cRotAngle*boost);
      moving:='run';
   end;

   // update animation (if required)
   // you can use faster methods (such as storing the last value of "moving")
   // but this ones shows off the brand new "CurrentAnimation" function :)
   if Actor1.CurrentAnimation<>moving then begin
      Actor1.SwitchToAnimation(moving);
      Actor2.Synchronize(Actor1);
   end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   HandleKeys(deltaTime);
   GLSceneViewer1.Invalidate;
end;

// add a few mushrooms to make the "landscape"

procedure TForm1.AddMushrooms;
var
   i : Integer;
   proxy : TGLProxyObject;
   s : TVector;
   f : Single;
begin
   // spawn some more mushrooms using proxy objects
   for i:=0 to cNbMushrooms-1 do begin
      // create a new proxy and set its MasterObject property
      proxy:=TGLProxyObject(DummyCube1.AddNewChild(TGLProxyObject));
      with proxy do begin
         MasterObject:=FreeForm1;
         // retrieve reference attitude
         Direction:=FreeForm1.Direction;
         Up:=FreeForm1.Up;
         // randomize scale
         s:=FreeForm1.Scale.AsVector;
         f:=(1*Random+1);
         ScaleVector(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           FreeForm1.Position.z+0.8*f,
                           Random(cSpread)-(cSpread/2));
         // randomize orientation
         RollAngle:=Random(360);
         TransformationChanged;
      end;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
