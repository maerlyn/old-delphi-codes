{: This sample is basicly a bench for GLScene.<p>

	A fair number of TCube are created and rendered (for cSize=5, that's about
	1331 cubes, 7986 polygons or 15972 triangles). Only one light and basic
   shading is used, no texture here, the aim is to test the performance of
   GLScene and T&L, not	the fillrate.<p>

	Results :

	Size	 Triangles     FPS	    CPU      OpenGL     ColorDepth
	  5      15972      36.4     K7-500   GeForce-6.50    32Bits
   --- 05/02/01 --- Cube build list and SetGLMaterialColors boost
	  5      15972      25.8     K7-500   GeForce-5.33    32Bits
   --- 13/01/01 --- Long time no bench
	  5      15972       7.7     K6-400     V3 2000       16Bits
   --- 20/06/00 --- Geometry.pas optimizations
	  5      15972      17.2     K7-500     V3 2000       16Bits
   --- 24/03/00 --- PolygonMode & Color cacheing optimization
	  5      15972      15.5     K7-500     V3 2000       16Bits
	  5      15972       5.9     K6-400     V3 3000       16Bits
	  5      15972       4.0     K6-400      MS NT4       24Bits
   --- 22/03/00 --- Set/UnSet states optimization
	  5      15972       5.5     K6-400     V3 3000       16Bits
	  5      15972       3.6     K6-400      MS NT4       24Bits
   --- 22/03/00 --- Created the bench
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  GLScene, GLObjects, GLMisc, Geometry, ExtCtrls, GLTexture, GLCadencer;

type
  TForm1 = class(TForm)
	 GLSceneViewer1: TGLSceneViewer;
	 GLScene1: TGLScene;
	 GLCamera1: TGLCamera;
	 DummyCube1: TDummyCube;
	 GLLightSource1: TGLLightSource;
	 Timer1: TTimer;
    GLCadencer1: TGLCadencer;
	 procedure FormCreate(Sender: TObject);
	 procedure Timer1Timer(Sender: TObject);
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

const
	cSize = 5;

procedure TForm1.FormCreate(Sender: TObject);
var
	x, y, z : Integer;
	cube : TCube;
	factor, cubeSize : Single;
begin
	// bench only creation and 1st render (with lists builds, etc...)
	factor:=70/(cSize*2+1);
	cubeSize:=0.4*factor;
	for x:=-cSize to cSize do for y:=-cSize to cSize do for z:=-cSize to cSize do begin
		cube:=TCube(DummyCube1.AddNewChild(TCube));
		cube.Position.AsVector:=PointMake(factor*x, factor*y, factor*z);
		cube.CubeWidth:=cubeSize;
		cube.CubeHeight:=cubeSize;
		cube.CubeDepth:=cubeSize;
      with cube.Material.FrontProperties do begin
         Diffuse.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));

         // uncomment following lines to stress OpenGL with more color changes calls

//         Ambient.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
//         Emission.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
//         Specular.Color:=VectorLerp(clrYellow, clrRed, (x*x+y*y+z*z)/(cSize*cSize*3));
      end;
	end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
	DummyCube1.TurnAngle:=90*newTime; // 90° per second
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
	Caption:=Format('%.2f FPS', [GLSceneViewer1.FramesPerSecond]);
	GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
