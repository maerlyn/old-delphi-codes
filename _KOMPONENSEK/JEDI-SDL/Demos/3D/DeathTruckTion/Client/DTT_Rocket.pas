unit DTT_Rocket;

// Gestion des missiles
// --------------------

interface

uses
  OpenGL12, TNT_Object, TNT_Vector, TNT_ParticleSystem, DTT_RocketTrail,
  DTT_Truck, DTT_Sound;

type
  TRocket = class(TObj)
  private
    Owner: TTruck;
    Trail: TParticleSystem;
  public
    constructor New(Truck: TTruck);
    procedure Animate(Time: Single); override;
  end;

var
  MissileSound: TSample;

implementation

uses
  TNT_3D, SysUtils, TNT_Collisions, TNT_Texture, DTT_Explosion, TNT_Entity,
  DTT_Client;

constructor TRocket.New(Truck: TTruck);
var
  Pos: TVector;
begin
  MissileSound.Play;
  Pos := Truck.Position;
  Pos.y := 8;
  inherited Create(4, Pos);
  Owner := Truck;
  Truck.NbRocket := Truck.NbRocket + 1;
  Heading := Truck.Heading;
  Velocity.x := -sin(Heading*(PI/180))*100;
  Velocity.z := -cos(Heading*(PI/180))*100;
  Position.x := Position.x + Velocity.x/10;
  Position.z := Position.z + Velocity.z/10;
  Mass := 1;
  Trail := TParticleSystem.Create(TRocketTrail, 40,
                                  Vector(0,0,0), Vector(0,0,0), 0,
                                  3, Self);
end;

procedure TRocket.Animate(Time: Single);
begin
  inherited;
  if Collision <> nil then
  begin
    if ObjectCollision(Client.PlayerTruck, Self) then
      Client.PlayerTruck.Life := Client.PlayerTruck.Life - 5;
    Dead := True;
    Explosion(Position, Random(2));
    Trail.Kill;
  end;
end;

end.

