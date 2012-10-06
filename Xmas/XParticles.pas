unit XParticles;

interface

{ This is a cut-down version of Untitled-3D's particle engine, ported to CgLib.
  The TParticleSystem class can manage any number of particles of any type. The
  TParticle class has abstract methods to handle updating and rendering. The
  system uses metaclasses to add particles to a system, so you can mix several
  particle types in the same particle system.
  This version of the system requires each particle to define its own parameters
  in the Reset() method. The full Untitled-3D system adds some functionality
  to read parameters from a file stream, so particle classes don't have to be
  as specific as they are here (particle classes for this program are defined
  in XParticleClasses.pas). For the latest news on Untitled-3D, keep an eye on
    http://www.gamedeveloper.org/delphi3d/projects.shtml }

uses
  CgTypes;

{$Z4}

type
  TParticleSystem = class;  // Forward.

  TParticle = class
  public
    // Predefined properties:
    Pos: TCGVector;
    Owner: TParticleSystem;
    // User code:
    procedure Reset; virtual; abstract;
    procedure Step(time: Single); virtual; abstract;
    procedure Render; virtual; abstract;
    constructor Create(AOwner: TParticleSystem); virtual;
  end;
  TParticleType = class of TParticle;

  TParticleSystem = class
  public
    Particles: array of TParticle;
    function AddParticles(pType: TParticleType; n: Integer): Integer;
    procedure DeleteParticles(start, n: Integer);
    procedure Render;
    procedure Step(time: Single);
    procedure Reset;
    destructor Destroy; override;
  end;

implementation

constructor TParticle.Create(AOwner: TParticleSystem);
begin

  inherited Create;
  Owner := AOwner;

end;

{******************************************************************************}
{ TPARTICLESYSTEM                                                              }
{******************************************************************************}

function TParticleSystem.AddParticles(pType: TParticleType; n: Integer): Integer;
var
  x, i: Integer;
begin

  // Add n particles of the given type to the system.
  x := Length(Particles);
  SetLength(Particles, x + n);
  for i := x to x + n - 1 do
  begin
    Particles[i] := pType.Create(Self);
    Particles[i].Reset;
  end;
  Result := x;

end;

procedure TParticleSystem.DeleteParticles(start, n: Integer);
var
  i: Integer;
begin

  // Delete some particles.
  for i := start to start + n - 1 do
  begin
    Particles[i].Free;
    Particles[i] := Particles[i+n];
  end;
  SetLength(Particles, Length(Particles) - n);

end;

destructor TParticleSystem.Destroy;
var
  i: Integer;
begin

  // Delete all particles
  for i := 0 to High(Particles) do Particles[i].Free;
  inherited Destroy;

end;

procedure TParticleSystem.Render;
var
  i: Integer;
begin

  for i := 0 to High(Particles) do Particles[i].Render;

end;

procedure TParticleSystem.Reset;
var
  i: Integer;
begin

  for i := 0 to High(Particles) do Particles[i].Reset;

end;

procedure TParticleSystem.Step(time: Single);
var
  i: Integer;
begin

  for i := 0 to High(Particles) do Particles[i].Step(time);

end;

end.
