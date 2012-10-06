{: Moving objects with the mouse.<p>

   In this demo you can move the two cubes around by picking and dragging
   them. This showcases the use of ScreenVectorIntersectXxxx functions.<p>

   You can also use the numeric keypad to move/zoom the camera and the arrow
   to move the selected object around.<p>

   (Based on Rado Stoyanov's test project)
}
unit Unit1;

interface

uses
  Windows, Forms, Dialogs, SysUtils, GLScene, GLObjects, GLMisc, Classes, Controls, GLGraph,
  GLCollision, GLTexture, OpenGL12, StdCtrls, ExtCtrls, Geometry, Graphics,
  GLVectorFileObjects;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    Scn: TGLSceneViewer;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    ZArrow: TArrowLine;
    XArrow: TArrowLine;
    YArrow: TArrowLine;
    Cube1: TCube;
    TopLight1: TGLLightSource;
    Cube2: TCube;
    Floor: TCube;
    Panel1: TPanel;
    Button1: TButton;
    Label2: TLabel;
    Label1: TLabel;
    TxtX: TSpaceText;
    TxtY: TSpaceText;
    procedure ScnMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	 procedure ScnMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ScnAfterRender(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    lastMouseWorldPos : TAffineVector;
    movingOnZ : Boolean;
    CurrentPick: TGLCustomSceneObject;
    function MouseWorldPos(x, y : Integer) : TAffineVector;
  end;

const
  SelectionColor: TColorVector = (0.243, 0.243, 0.243, 1.000);

var
  Form1: TForm1;

implementation

{$R *.DFM}

function TForm1.MouseWorldPos(x, y : Integer) : TAffineVector;
var
   v : TAffineVector;
begin
   y:=Scn.Height-y;
   if Assigned(currentPick) then begin
      SetVector(v, x, y, 0);
      if movingOnZ then
         Scn.ScreenVectorIntersectWithPlaneXZ(v, currentPick.Position.Y, Result)
      else Scn.ScreenVectorIntersectWithPlaneXY(v, currentPick.Position.Z, Result);
   end else SetVector(Result, NullVector);
end;

procedure TForm1.ScnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   pick : TGLBaseSceneObject;
begin
   movingOnZ:=(ssShift in Shift);
   // If an object is picked...
   pick:=(Scn.GetPickedObject(x, y) as TGLCustomSceneObject);
   if Assigned(Pick) then begin
      // Only Cube1 and Cube2 can be selected
      if (pick.Name <> 'Cube1') and (pick.Name <> 'Cube2') then
         pick := nil;
   end;
   if pick<>currentPick then begin
      if Assigned(currentPick) then
         currentPick.Material.FrontProperties.Emission.Color := clrBlack;
      currentPick:=TGLCustomSceneObject(pick);
      if Assigned(currentPick) then
         CurrentPick.Material.FrontProperties.Emission.Color := SelectionColor;
   end;
   // store mouse pos
   if Assigned(currentPick) then
      lastMouseWorldPos:=MouseWorldPos(x, y);
end;

procedure TForm1.ScnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   newPos : TAffineVector;
begin
   if ssLeft in Shift then begin
      // handle hold/unhold of shift
      if (ssShift in Shift)<>movingOnZ then begin
         movingOnZ:=(ssShift in Shift);
         lastMouseWorldPos:=MouseWorldPos(x, y);
      end;
      newPos:=MouseWorldPos(x, y);
      if Assigned(currentPick) and (VectorNorm(lastMouseWorldPos)<>0) then
         currentPick.Position.Translate(VectorSubtract(newPos, lastMouseWorldPos));
      lastMouseWorldPos:=newPos;
   end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
   with GLCamera1 do case Key of
      '2': MoveAroundTarget(3, 0);
      '4': MoveAroundTarget(0, -3);
      '6': MoveAroundTarget(0, 3);
      '8': MoveAroundTarget(-3, 0);
      '-': AdjustDistanceToTarget(1.1);
      '+': AdjustDistanceToTarget(1 / 1.1);
   end;
end;

procedure TForm1.ScnAfterRender(Sender: TObject);
var
   objPos, winPos : TAffineVector;
begin
   Scn.Canvas.Brush.Style := bsClear;
   Scn.Canvas.Font.Name := 'Verdana';
   Scn.Canvas.Font.Color := clWhite;

   if Assigned(currentPick) then begin
      objPos:=currentPick.Position.AsAffineVector;
      Scn.Canvas.TextOut(3, 3 + 1 * Scn.Canvas.TextHeight('A'),
                         Format('New Object Position: Xn: %4.4f, Yn: %4.4f, Zn: %4.4f',
                                [objPos[0], objPos[1], objPos[2]]));
      winPos:=Scn.WorldToScreen(objPos);
      Scn.Canvas.TextOut(Round(winPos[0]), Scn.Height-Round(winPos[1]),
                         currentPick.Name);
   end else begin
      Scn.Canvas.TextOut(3, 3+Scn.Canvas.TextHeight('A'), 'No selected object');
   end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if Assigned(currentPick) then with currentPick do case Key of
      VK_UP :     if ssShift in Shift then
                     Translate(0, 0, 0.3)
                  else Translate(-0.3, 0, 0);
      VK_DOWN :   if ssShift in Shift then
                     Translate(0, 0, -0.3)
                  else Translate(0.3, 0, 0);
      VK_LEFT :   Translate(0, -0.3, 0);
      VK_RIGHT :  Translate(0, 0.3, 0);
    end;
end;

end.

