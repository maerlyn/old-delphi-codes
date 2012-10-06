unit TNT_ParticleSystem;

interface

uses
  TNT_Entity, TNT_Timer, TNT_Vector;

type
  TParticleSystem = class;

  TParticle = class
  protected
    System: TParticleSystem;
    procedure Die;
  public
    Alive: Boolean;
    constructor Create(Owner: TParticleSystem); virtual;
    procedure Spawn; virtual;
    procedure Animate(Time: Single); virtual; abstract;
    procedure Render; virtual; abstract;
    procedure OnBeforeRender; virtual; abstract;
    procedure OnAfterRender; virtual; abstract;
  end;

  TParticleType = class of TParticle;

  TParticleSystem = class(TEntity)
  private
    Particles: array of TParticle;
    Timer: TTime;
    NbAlive: Integer;
    SpawnInterval: Single;
  public
    constructor Create(ParticleType: TParticleType; n: Integer;
                       vPosition, vVelocity: TVector; sMass: Single;
                       Freq: Cardinal = 0; Ref: TEntity = nil);
    procedure Spawn(n: Integer);
    procedure Animate(Time: Single); override;
    procedure Render; override;
    procedure IncAlive;
    procedure DecAlive;
    procedure Kill;
    destructor Destroy; override;
  end;

implementation

constructor TParticleSystem.Create(ParticleType: TParticleType; n: Integer;
                                   vPosition, vVelocity: TVector; sMass: Single;
                                   Freq: Cardinal = 0; Ref: TEntity = nil);
var
  i: Integer;
begin
  inherited EntityCreate(TYPE_PARTICLES, vPosition, vVelocity, Mass, Ref);
  if Freq<>0 then
    SpawnInterval := 1/Freq
  else
    SpawnInterval := 0;

  SetLength(Particles, n);
  for i := 0 to n-1 do
    Particles[i] := ParticleType.Create(Self);
  Timer := TTime.Create;
end;

procedure TParticleSystem.Spawn(n: Integer);
var
  i, j: Integer;
begin
  for j := 1 to n do
  begin
    i := 0;
    while (i < Length(Particles)) and Particles[i].Alive do
      Inc(i);
    if i < Length(Particles) then
      Particles[i].Spawn;
  end;
end;

procedure TParticleSystem.Render;
var
  i: Integer;
begin
  Particles[0].OnBeforeRender;
  for i := 0 to Length(Particles)-1 do
    if Particles[i].Alive then
      Particles[i].Render;
  Particles[0].OnAfterRender;
end;

procedure TParticleSystem.Animate(Time: Single);
var
  i: Integer;
begin
  inherited;
  if (SpawnInterval<>0) and Timer.Diff(SpawnInterval) then
    Spawn(1);
  for i:=0 to Length(Particles)-1 do
    if Particles[i].Alive then
      Particles[i].Animate(Time);
end;

procedure TParticleSystem.IncAlive;
begin
  Inc(NbAlive);
end;

procedure TParticleSystem.DecAlive;
begin
  Dec(NbAlive);
  if SpawnInterval=0 then
    Dead := NbAlive = 0;
end;

procedure TParticleSystem.Kill;
begin
  SpawnInterval := 0;
end;

destructor TParticleSystem.Destroy;
var
  i: Integer;
begin
  for i:=0 to Length(Particles)-1 do
    Particles[i].Free;
  Particles := nil;
  inherited Destroy;
end;

constructor TParticle.Create(Owner: TParticleSystem);
begin
  inherited Create;
  System := Owner;
end;

procedure TParticle.Spawn;
begin
  Alive := True;
  System.IncAlive;
end;

procedure TParticle.Die;
begin
  Alive := False;
  System.DecAlive;
end;

end.

