{: Basic terrain rendering demo.<p>

   The terrain HeightData is provided by a TGLBitmapHDS (HDS stands for
   "Height Data Source"), and displayed by a TTerrainRenderer.<p>

   The base terrain renderer uses a brute-force approach to rendering
   terain, by requesting height data tiles, and rendering them using
   triangle strips.<p>

   Known issue : multitexturing ain't working in this case with 3Dfx OpenGL
      (the texture mode apparently isn't honoured), and perhaps other ICDs,
      if it happens to you, set Texture2Name to blank for the "ground" material.
}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GLScene, GLTerrainRenderer, GLObjects, GLMisc, jpeg, GLHeightData,
  ExtCtrls, GLCadencer, StdCtrls, GLTexture, GLHUDObjects, GLBitmapFont,
  GLSkydome;

type
  TForm1 = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLBitmapHDS1: TGLBitmapHDS;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TDummyCube;
    GLLightSource1: TGLLightSource;
    TerrainRenderer1: TTerrainRenderer;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    BitmapFont1: TBitmapFont;
    HUDText1: THUDText;
    SkyDome1: TSkyDome;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    mx, my : Integer;
    fullScreen : Boolean;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses Keyboard, OpenGL12;

procedure TForm1.FormCreate(Sender: TObject);
begin
   SetCurrentDir(ExtractFilePath(Application.ExeName)+'..\..\media');
   // 8 MB height data cache
   GLBitmapHDS1.MaxPoolSize:=8*1024*1024;
   // specify height map data
   GLBitmapHDS1.Picture.LoadFromFile('terrain.bmp');
   // load the texture maps
   GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile('snow512.jpg');
   GLMaterialLibrary1.Materials[1].Material.Texture.Image.LoadFromFile('detailmap.jpg');
   // apply texture map scale (our heightmap size is 256)
   TerrainRenderer1.TilesPerTexture:=256/TerrainRenderer1.TileSize;
   // load Bitmap Font
   BitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=3*deltaTime
   else speed:=deltaTime;
   with GLCamera1.Position do begin
      if IsKeyDown(VK_UP) then
         DummyCube1.Translate(-X*speed, 0, -Z*speed);
      if IsKeyDown(VK_DOWN) then
         DummyCube1.Translate(X*speed, 0, Z*speed);
      if IsKeyDown(VK_LEFT) then
         DummyCube1.Translate(-Z*speed, 0, X*speed);
      if IsKeyDown(VK_RIGHT) then
         DummyCube1.Translate(Z*speed, 0, -X*speed);
      if IsKeyDown(VK_ESCAPE) then Close;
   end;
   // don't drop through terrain!
   with DummyCube1.Position do
      Y:=TerrainRenderer1.InterpolatedHeight(AsVector);
end;

// Standard mouse rotation & FPS code below

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   HUDText1.Text:=Format('%.1f FPS - %d',
                         [GLSceneViewer1.FramesPerSecond, TerrainRenderer1.LastTriangleCount]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
