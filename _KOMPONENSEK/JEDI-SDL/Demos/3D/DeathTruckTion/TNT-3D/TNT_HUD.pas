unit TNT_HUD;

interface

uses
  TNT_Entity;

type
  THUD = class(TEntity)
  protected
    procedure OnRender; virtual; abstract;
    procedure Begin3D;
    procedure End3D;
  public
    constructor Create; virtual;
    procedure Render; override;
  end;

implementation

uses
  OpenGL12, TNT_Vector;

constructor THUD.Create;
begin
  inherited EntityCreate(TYPE_HUD, Vector(0,0,0), Vector(0,0,0), 0);
end;

procedure THUD.Render;
begin
  glDisable(GL_DEPTH_TEST);
  End3D;
  OnRender;
  Begin3D;
end;

procedure THUD.Begin3D;
begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure THUD.End3D;
begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;
  glOrtho(0, 640, 480, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

end.

