unit TNT_Camera;

// Gestion de la camera
// --------------------

interface

uses
  TNT_Entity, TNT_Vector, TNT_Object;

type
  TCamera = class(TEntity)
  private
    ObjFollow: TObj;
    DistFollow: Single;
  public
    Orientation: TVector;
    ModelView: array [0..15] of Single;
    constructor Create(x,y,z, ax,ay,az: Single);
    procedure Render; override;
    procedure Follow(Obj: TObj; Dist: Single);
    procedure Billboard;
    procedure Release;
    procedure Rotation;
  end;

implementation

uses
  OpenGL12, TNT_Frustum;

constructor TCamera.Create(x,y,z, ax,ay,az: Single);
begin
  inherited EntityCreate(TYPE_CAMERA, Vector(x, y, z), Vector(0,0,0), 0);
  Orientation := Vector(ax, ay, az);
end;

procedure TCamera.Rotation;
begin
  with Orientation do
  begin
    glRotatef(-x, 1,0,0);
    glRotatef(-y, 0,1,0);
    glRotatef(-z, 0,0,1);
  end;
end;

procedure TCamera.Render;
begin
  if ObjFollow <> nil then
  begin
    Orientation.y := ObjFollow.Heading;
    Position.x := ObjFollow.Position.x;
    Position.z := ObjFollow.Position.z;
  end;

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  glTranslatef(0,0,-DistFollow);
  Rotation;
  with Position do
    glTranslatef(-x, -y, -z);
  glGetFloatv(GL_MODELVIEW_MATRIX, @ModelView[0]);
  ExtractFrustum;
end;

procedure TCamera.Follow(Obj: TObj; Dist: Single);
begin
  ObjFollow := Obj;
  DistFollow := Dist;
end;

procedure TCamera.Billboard;
begin
  with Orientation do
  begin
    glRotatef(z, 0,0,1);
    glRotatef(y, 0,1,0);
    glRotatef(x, 1,0,0);
  end;
end;

procedure TCamera.Release;
begin
  ObjFollow := nil;
end;

end.

