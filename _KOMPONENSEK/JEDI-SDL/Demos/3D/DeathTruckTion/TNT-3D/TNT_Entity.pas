unit TNT_Entity;

// Gestion des entites
// -------------------
//  Tous les elements du moteur sont des entites: objets, particules, landscape,
//  skybox, lights, camera. Les entites sont controlees par le moteur physique,
//  sauf si Mass=0. Une entite peut etre liee à une autre entitee (utile pour
//  la camera, les particules qui suivent un objet, etc.) via le champs Ref.
//  Les entitees ne sont pas utilisees directement, mais tous les elements de la
//  scene sont des descendants de cette classe. Les entitees sont chainees entre
//  elles, elles sont ajoutees a la scene a leur creation, et enlevees a leur
//  destruction.

interface

uses
  TNT_Vector;

const
  TYPE_CAMERA = 0;
  TYPE_LIGHT = 1;
  TYPE_SKYBOX = 2;
  TYPE_LANDSCAPE = 3;
  TYPE_OBJECT = 4;
  TYPE_PARTICLES = 5;
  TYPE_HUD = 6;

type
  TEntitySet = set of 0..6;
  PEntity = ^TEntity;
  TEntity = class
  protected
    Ref: TEntity;
    Mass: Single;
    constructor EntityCreate(ETyp: Integer; Pos, Vel: TVector; sMass: Single;
      ERef: TEntity = nil);
  public
    Next: TEntity;
    Visible: Boolean;
    Typ: Integer;
    Position, Velocity, Force: TVector;
    Dead: Boolean;
    procedure Animate(Time: Single); virtual;
    procedure Render; virtual; abstract;
    procedure Release;
    destructor Destroy; override;
  end;

implementation

uses
  TNT_3D;

constructor TEntity.EntityCreate(ETyp: Integer; Pos, Vel: TVector;
  sMass: Single; ERef: TEntity = nil);
begin
  inherited Create;
  Ref := ERef;
  Typ := ETyp;
  Position := Pos;
  Velocity := Vel;
  Mass := sMass;
  Force := Vector(0,0,0);
  Visible := True;
  Dead := False;
  Scene.Add(Self);
end;

procedure TEntity.Release;
begin
  Ref := nil;
end;

procedure TEntity.Animate(Time: Single);
begin
  if Mass <> 0 then
  begin
    Velocity.x := Velocity.x + (Force.x/Mass) * Time;
    Velocity.y := Velocity.y + (Force.y/Mass) * Time;
    Velocity.z := Velocity.z + (Force.z/Mass) * Time;
    Position.x := Position.x + Velocity.x * Time;
    Position.y := Position.y + Velocity.y * Time;
    Position.z := Position.z + Velocity.z * Time;
  end;
  if Ref <> nil then
  begin
    Position := Ref.Position;
    Velocity := Ref.Velocity;
  end;
end;

destructor TEntity.Destroy;
begin
  Scene.Remove(Self);
  inherited Destroy;
end;

end.

