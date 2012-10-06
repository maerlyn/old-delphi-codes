{: Generates a 3D mesh from a height-field function.<p>

   The left viewer shows a triangle-based mesh, the right viewer uses a
   triangle-strip-based mesh, both mesh are based on the same height-field and
   have the same resolution.<p>

   This sample wants to demonstrate a few things :<ul>
   <li>how to define a mesh (triangle & triangle strip)
   <li>what the default normal calculations looks like
   <li>TriangleStrips are faster, but slightly more complex to use
   </ul>

   In triangle mode, the normals are computed on a triangle basis, hence the
   facetted look, for triangle-strip, they are computed for each vertex based
   on the triangle it completes (smooth along strip-direction).<br>

   The reader may make the "good" looking version (ie. smooth aspect in all
   direction) by calculating the proper normal from the formula instead of
   using standard normal calculations.<br>

   Sample framerates (K6-400 + Sofware OpenGL), 5000 triangles (cResolution=25) :<br>
   -  mmTriangles : 9.6 FPS<br>
   -  mmTriangleStrip : 17.2 FPS<p>
   Sample framerates (K7-500 + GeForce 256), 20000 triangles (cResolution=50) :<br>
   -  mmTriangles : 53 FPS<br>
   -  mmTriangleStrip : 202 FPS
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLObjects, GLMisc, Geometry, GLTexture, ExtCtrls, GLCadencer,
  StdCtrls, GLMesh;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    Mesh1: TMesh;
    DummyCube1: TDummyCube;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Timer1: TTimer;
    GLSceneViewer2: TGLSceneViewer;
    Panel1: TPanel;
    Label1: TLabel;
    GLScene2: TGLScene;
    DummyCube2: TDummyCube;
    Mesh2: TMesh;
    GLLightSource2: TGLLightSource;
    GLCamera2: TGLCamera;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Déclarations privées }
    mx, my : Integer;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

const
   // half-grid resolution, grid width is actually cResolution*2 of "quads"
   cResolution = 50;

procedure TForm1.FormCreate(Sender: TObject);
var
   x, y : Integer;
   pTopLeft, pTopRight, pBottomRight, pBottomLeft : TAffineVector;
   invRes, invRes2 : Single;

   function MakeVect(const x, y : Single) : TAffineVector;
   begin
      SetVector(Result, x*invRes, sin((x*x+y*y)*invRes2), y*invRes);
   end;

   procedure AddTriangle(const p1, p2, p3 : TAffineVector;
                         const color : TColorVector);
   begin
      with Mesh1.Vertices do begin
         AddVertex(p1, NullVector, color);
         AddVertex(p2, NullVector, color);
         AddVertex(p3, NullVector, color);
      end;
   end;

begin
   // scaling precalcs for our math func
   invRes:=10/cResolution;
   invRes2:=0.1*Sqr(invRes);
   //
   // Triangles
   //
   // this one is basic : we calculate the corner points for each grid quad and
   // add the two triangles that make it
   with Mesh1 do begin
      Mode:=mmTriangles;
      Vertices.Clear;
      for y:=-cResolution to cResolution do begin
         for x:=-cResolution to cResolution do begin
            pTopLeft:=MakeVect(x, y+1);
            pTopRight:=MakeVect(x+1, y+1);
            pBottomRight:=MakeVect(x+1, y);
            pBottomLeft:=MakeVect(x, y);
            // top left triangle
            AddTriangle(pBottomLeft, pTopLeft, pTopRight, clrBlue);
            // bottom right triangle
            AddTriangle(pTopRight, pBottomRight, pBottomLeft, clrBlue);
         end;
      end;
      CalcNormals(fwCounterClockWise);
      Vertices.Locked:=True;
   end;
   //
   // TriangleStrip
   //
   // Same as triangle, however trianglestrips are continuous, and to cover
   // the grid, "null" segments are used at both ends of a strip (to avoid a
   // visible triangle that would stretch for the full width of the grid).
   // Note : this can be avoided by reversing grid traversing direction (one line
   // from left to right, one from right to left, etc.)
   with Mesh2 do begin
      Mode:=mmTriangleStrip;
      Vertices.Clear;
      for y:=-cResolution to cResolution do begin
         pTopLeft:=MakeVect(-cResolution, y+1);
         Vertices.AddVertex(pTopLeft, NullVector, clrBlue);
         Vertices.AddVertex(pTopLeft, NullVector, clrBlue);
         for x:=-cResolution to cResolution do begin
            pTopRight:=MakeVect(x+1, y+1);
            pBottomLeft:=MakeVect(x, y);
            with Vertices do begin
               AddVertex(pBottomLeft, NullVector, clrBlue);
               AddVertex(pTopRight, NullVector, clrBlue);
            end;
         end;
         pBottomRight:=MakeVect(cResolution+1, y);
         Vertices.AddVertex(pBottomRight, NullVector, clrBlue);
         Vertices.AddVertex(pBottomRight, NullVector, clrBlue);
      end;
      CalcNormals(fwClockWise);
      Vertices.Locked:=True;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   // nb of triangles in scene
   Caption:=Format('%d Triangles', [2*(cResolution*2)*(cResolution*2)]);
   // calculate & display triangles framerate
   with GLSceneViewer1 do begin
      // we render twice to get a fair FPS rating
      ResetPerformanceMonitor;
      Render;
      Render;
      Label1.Caption:=Format('%.2f FPS (mmTriangles)', [FramesPerSecond]);
   end;
   // calculate & display trianglestrip framerate
   with GLSceneViewer2 do begin
      // we render twice to get a fair FPS rating
      ResetPerformanceMonitor;
      Render;
      Render;
      Label2.Caption:=Format('%.2f FPS (mmTriangleStrip)', [FramesPerSecond]);
   end;
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
      TGLSceneViewer(Sender).Camera.MoveAroundTarget(my-y, mx-x);
      my:=y; mx:=x;
   end;
end;

end.
