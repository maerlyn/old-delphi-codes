unit TNT_Object;

interface

uses
  TNT_Entity, TNT_Vector, TNT_Model, OpenGL12;

type
  TObj = class(TEntity)
  private
    r, g, b, a: Single;
    Quadratic: PGLUquadricObj;
  public
    ModelID: Integer;
    Heading: Single;
    Frame: Integer;
    constructor Create(Model: Integer; Pos: TVector);
    procedure Render; override;
    procedure SetColor(cr, cg, cb, ca: Single);
    procedure Animate(Time: Single); override;
    function Collision: TEntity;
    procedure Transform(P: TVector; var Q: TVector);
    destructor Destroy; override;
  end;

var
  ShowBSpheres: Boolean;

implementation

uses
  TNT_3D, TNT_Frustum, TNT_Collisions, TNT_Landscape;

constructor TObj.Create(Model: Integer; Pos: TVector);
begin
  inherited EntityCreate(TYPE_OBJECT, Pos, Vector(0,0,0), 0);
  ModelID := Model;
  SetColor(1,1,1,1);
  Quadratic := gluNewQuadric;
  gluQuadricTexture(Quadratic, ByteBool(GL_FALSE ));
end;

procedure TObj.SetColor(cr, cg, cb, ca: Single);
begin
  r := cr;
  g := cg;
  b := cb;
  a := ca;
end;

procedure TObj.Render;
begin
  if a <> 1 then
    glEnable(GL_BLEND);
  glPushMatrix;
  glTranslatef(Position.x, Position.y, Position.z);
  glColor4f(r, g, b, a);
  glRotatef(Heading+180, 0,1,0);
  Scene.Models[ModelID].Render(Frame);

  if ShowBSpheres then
    with Scene.Models[ModelID] do
    begin
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glDisable(GL_TEXTURE_2D);
      glColor3f(0,1,0);
      gluSphere(Quadratic, BVisSphereRadius, 8, 8);
      glColor3f(1,1,1);
      gluSphere(Quadratic, BColSphereRadius, 8, 8);
      glEnable(GL_TEXTURE_2D);
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    end;

  glPopMatrix;
  if a <> 1 then
    glDisable(GL_BLEND);
end;

procedure TObj.Transform(P: TVector; var Q: TVector);
var
  SinT, CosT: Single;
begin
  SinT := -sin(Heading*(PI/180));
  CosT := -cos(Heading*(PI/180));

  P.x := P.x - Position.x;
  P.z := P.z - Position.z;

  Q.x := - P.x*CosT + P.z*SinT;
  Q.z := P.x*SinT + P.z*CosT;
  Q.y := P.y;
end;

procedure TObj.Animate(Time: Single);
begin
  inherited;
  with Scene.Models[ModelID] do
    Visible := SphereInFrustum(Position.X, Position.Y, Position.Z, BVisSphereRadius) > 0;
end;

function TObj.Collision: TEntity;
var
  PtrObj: ^TObj;
  PtrLand: ^TLandscape;
  Yes: Boolean;
begin
  Yes := False;
  Result := Scene.Entities;
  while not Yes and (Result <> nil) do
  begin
    case Result.Typ of
      TYPE_LANDSCAPE:
        begin
          PtrLand := @Result;
          Yes := PtrLand^.Collision(Position.X, Position.Y, Position.Z);
        end;
      TYPE_OBJECT:
        begin
          PtrObj := @Result;
          Yes := ObjectCollision(Self, PtrObj^);
        end;
    end;
    Result := Result.Next;
  end;
end;

destructor TObj.Destroy;
begin
  gluDeleteQuadric(Quadratic);
  inherited Destroy;
end;

end.

