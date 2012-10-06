{: A Nut and Bolt sample, 100% defined at design-time.<p>

   Both use two revolution solids, one for the head/pans, another for the thread.
   Additionnally, a cylinder and an annulus help finish up by providing the
   shafts.<p>

   The head/pans are defined by simple rotated on 360° in 6 steps, the threads
   are a simpler curve : two segments in triangular shape, that are rotated and
   extruded in the y axis.<p>

   Smoothing is used in the head to make smoother edges (along with a bevel in
   the curve), while the threads are unsmoothed, to get a sharp edge effect.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLMisc, GLObjects, GLExtrusion;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TDummyCube;
    RSBoltThreads: TRevolutionSolid;
    CYBoltShaft: TCylinder;
    RSBoltHead: TRevolutionSolid;
    Bolt: TDummyCube;
    Nut: TDummyCube;
    RSNutThreads: TRevolutionSolid;
    RSNutPans: TRevolutionSolid;
    Annulus1: TAnnulus;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then
      GLCamera1.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

end.
