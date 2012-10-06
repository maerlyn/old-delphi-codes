{: Mushroom frenzy : demonstrates loading 3DS files and using proxy objects.<p>

   In this sample, we have a single 3DS mesh (a mushroom), and we want to display
   a whole bunch of mushrooms. To reach this goal, we use a TFreeForm and load
   the 3DS mesh with its "LoadFromFile" method.<p>

   The other mushrooms are obtained with proxy objects (see "AddMushrooms"),
   our freeform is used as MasterObject, the scale and position are then randomized
   and scattered around our ground (a textured disk).<p>

   This results could also have been obtained by creating FreeForms instead of
   ProxyObjects, but using ProxyObjects avoids duplicating mesh data and helps
   in sustaining better framerates (the same data and build list is shared among
   all mushrooms).
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLVectorFileObjects, GLMisc, GLObjects, GLBehaviours, GLCadencer,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    FreeForm1: TFreeForm;
    DummyCube1: TDummyCube;
    Disk1: TDisk;
    Button1: TButton;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private
    { Déclarations privées }
    procedure AddMushrooms;
  public
    { Déclarations publiques }
    mx, my : Integer;
    mushroomCounter : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Geometry, GLTexture, JPeg;

const
   cSpread = 90;
   cNbMushrooms = 10;

procedure TForm1.FormCreate(Sender: TObject);
begin
//   Randomize;
   // Load mushroom mesh
   FreeForm1.LoadFromFile('..\..\media\mushroom.3ds');
   // Load ground texture
   Disk1.Material.Texture.Image.LoadFromFile('..\..\media\clover.jpg');
   // Duplicate our reference mushroom (but not its mesh data !)
   AddMushrooms;
end;

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
         f:=(Random+0.2);
         VectorScale(s, f);
         Scale.AsVector:=s;
         // randomize position
         Position.SetPoint(Random(cSpread)-(cSpread/2),
                           f*FreeForm1.Position.Y,
                           Random(cSpread)-(cSpread/2));
         // randomize orientation
         RollAngle:=Random(360);
      end;
   end;
   Inc(mushroomCounter, cNbMushrooms);
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x; my:=y;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   AddMushrooms;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('Mushroom Counter : %d (%.1f FPS)',
                   [mushroomCounter, GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
   // adjust focal Length
   GLCamera1.FocalLength:=GLSceneViewer1.Width/8;
   // keep "add mushrooms" centered
   Button1.Left:=(Width-Button1.Width) div 2;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // keep it rendering, we want FPS stats !
   GLSceneViewer1.Invalidate;
end;

end.
