unit TNT_3D;

interface

uses
  TNT_Timer, TNT_Scene, TNT_Camera, TNT_Console;

type
  TNT3D = class
  private
    Timer: TTime;
  public
    FPS: Single;
    Tris: Cardinal;
    constructor Create;
    procedure Resize(Width, Height: Cardinal);
    procedure Render;
    destructor Destroy; override;
  end;

var
  TNT: TNT3D;
  Scene: TScene;
  Camera: TCamera;
  Console: TConsole;

implementation

uses
  OpenGL12, SysUtils, TNT_Font;

constructor TNT3D.Create;
begin
  inherited Create;
  Console := TConsole.Create(NB_LINES, PROMPT, LOG_NAME);
  Scene := TScene.Create;
  Camera := TCamera.Create(0,0,0, 0,0,0);
  Font := TFont.Create('font.tex');
  Timer := TTime.Create;
end;

procedure TNT3D.Render;
begin
  Scene.Render(Timer.Delta);
  glFlush;
  FPS := Timer.Refresh;
end;

procedure TNT3D.Resize(Width, Height: Cardinal);
begin
  glViewPort(0, 0, Width, Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(45, Width/Height, 1, 3000);
  Console.Log('OpenGL display resized to '+IntToStr(Width)+'x'+IntToStr(Height));
end;

destructor TNT3D.Destroy;
begin
  Timer.Free;
  Camera.Free;
  Scene.Free;
  Font.Free;
  Console.Free;
  inherited Destroy;
end;

end.

