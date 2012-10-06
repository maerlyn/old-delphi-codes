{: This Form demonstrates basic "Pathcontrol" movements.<p>
   You can modified the Looped property of the path to enable the path-looping.
   Set ShowPath property to turn on or turn off the path-displaying 
}
unit Unit1;

interface

uses
  Windows, Forms, GLScene, GLObjects, ComCtrls, GLMisc, ExtCtrls, StdCtrls,
  Classes, Controls, GLCadencer, GLBehaviours, Buttons, GLGraph, GLMovement;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Cube2: TCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    CBPlay: TCheckBox;
    DummyCube1: TDummyCube;
    GLCadencer1: TGLCadencer;
    MoveBtn: TBitBtn;
    Timer1: TTimer;
    Sphere1: TSphere;
    procedure FormResize(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormActivate(Sender: TObject);
    procedure MoveBtnClick(Sender: TObject);
    procedure CBPlayClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure PathTravelStop(Sender: TObject; Path: TGLMovementPath; var Looped: Boolean);
    procedure PathAllTravelledOver(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  Math, SysUtils, Geometry;

procedure TForm1.FormResize(Sender: TObject);
begin
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  // We need to stop playing here :
  // 	since the timer is asynchronous, if we don't stop play,
  // 	it may get triggered during the form's destruction
  CBPlay.Checked := False;
end;

procedure TForm1.PathTravelStop(Sender: TObject; Path: TGLMovementPath; var Looped: Boolean);
begin
  MessageBox(0, 'Path Traveled Once', 'Infomation', MB_OK);
end;

procedure TForm1.PathAllTravelledOver(Sender: TObject);
begin
  MessageBox(0, 'All Path(es) Traveled Over', 'Infomation', MB_OK);
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  Movement: TGLMovement;
  Path:     TGLMovementPath;
  Node:     TGLPathNode;
begin
  //Create a movement, a path and the first node of the path
  Movement   := GetOrCreateMovement(Cube2);
  Movement.OnPathTravelStop := PathTravelStop;
  Movement.OnAllPathTravelledOver := PathAllTravelledOver;
  Path       := Movement.AddPath;
  Path.ShowPath := True;
  //Path.Looped := True;
  Node       := Path.AddNodeFromObject(Cube2);
  Node.Speed := 4.0;

  //Add a node
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(-10, 0, 0, 1);
  Node.RotationAsVector := VectorMake(0, 0, 0);

  //Add a node
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(0, 5, - 5);
  Node.RotationAsVector := VectorMake(0, 90, 0);

  //Add a node
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(6, - 5, 2);
  Node.RotationAsVector := VectorMake(0, 180, 0);

  //Add a node
  Node       := Path.AddNode;
  Node.Speed := 4.0;
  Node.PositionAsVector := VectorMake(-6, 0, 0);
  Node.RotationAsVector := VectorMake(0, 259, 0);

  //Activatived the current path
  Movement.ActivePathIndex := 0;
end;

procedure TForm1.MoveBtnClick(Sender: TObject);
var
  Movement: TGLMovement;
begin
  Movement := GetMovement(Cube2);
  if Assigned(Movement) then
    Movement.StartPathTravel;
end;

procedure TForm1.CBPlayClick(Sender: TObject);
begin
  GLCadencer1.Enabled := TCheckBox(Sender).Checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
