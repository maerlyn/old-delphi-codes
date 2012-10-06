unit XParticleClasses;

interface

{ Particle classes for this demo. }

uses
  XParticles, CgTexture, CgTypes, CgGeometry, XTexObject, GL;

type
  TSnowFlake = class(TParticle)
  private
    FTexture: TTexObject;
    FSize: Single;
    FSpeed: TCGVector;
    FRot: Single;
  public
    procedure Reset; override;
    procedure Step(time: Single); override;
    procedure Render; override;
    constructor Create(AOwner: TParticleSystem); override;
  end;

  TTwinkle = class(TParticle)
  private
    FTimer: Single;
    FSize: Single;
    FRot: Single;
  public
    procedure Reset; override;
    procedure Step(time: Single); override;
    procedure Render; override;
    constructor Create(AOwner: TParticleSystem); override;
  end;

implementation

uses
  XMain;

{ TSnowFlake }

constructor TSnowFlake.Create(AOwner: TParticleSystem);
begin

  inherited Create(AOwner);
  Reset;

end;

procedure TSnowFlake.Render;
begin

  // Draw an additively blended, textured quad. Disable depth buffer writing.
  FTexture.Enable;
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glDepthMask(GL_FALSE);
  glPushMatrix;
    glTranslatef(Pos.x, Pos.y, Pos.z);
    glRotatef(FRot, 0, 0, 1);
    glBegin(GL_QUADS);
      with Pos do
      begin
        glTexCoord2f(0, 0);   glVertex3f(-FSize, -FSize, z);
        glTexCoord2f(1, 0);   glVertex3f(FSize, -FSize, z);
        glTexCoord2f(1, 1);   glVertex3f(FSize, FSize, z);
        glTexCoord2f(0, 1);   glVertex3f(-FSize, FSize, z);
      end;
    glEnd;
  glPopMatrix;
  glDepthMask(GL_TRUE);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

end;

procedure TSnowFlake.Reset;
begin

  // Pick a random texture from the ones loaded by the program:
  FTexture := ParticleTex[Random(4) + 1];
  // Set some parameters:
  FSize := (Random + 1) / 2;
  Pos := cgVector((Random - 0.5) * 128, 48 + Random*32, -8 - Random*24);
  FSpeed := cgVector(5*(Random-0.5), -10 - Random*10, 5*(Random-0.5));
  FRot := Random * pi;

end;

procedure TSnowFlake.Step(time: Single);
var
  dP: TCGVector;
begin

  // Update animation parameters.
  FRot := FRot + 50*time;
  dP := FSpeed;
  cgScale(dP, time, time, time);
  Pos := cgVecAdd(Pos, dP);
  // Kill and respawn particle when it falls too deep.
  if Pos.y < -32 then Reset;

end;

{ TTwinkle }

constructor TTwinkle.Create(AOwner: TParticleSystem);
begin

  inherited Create(AOwner);
  Reset;

end;

procedure TTwinkle.Render;
begin

  { Another textured quad. This one briefly fades in and back out when the
    FTimer parameter approaches zero. It is not rendered at all if FTimer is
    above 1. } 
  if FTimer > 1 then Exit;
  StarTex.Enable;
  // Determine color, for fading in and out:
  if FTimer < 0.5 then glColor4f(1, 1, 1, FTimer*2)
  else glColor4f(1, 1, 1, (1-FTimer)*2);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE);
  glDisable(GL_DEPTH_TEST);
  glPushMatrix;
    glTranslatef(Pos.x, Pos.y, Pos.z);
    glRotatef(FRot, 0, 0, 1);
    glBegin(GL_QUADS);
      glNormal3f(0, 0, 1);
      with Pos do
      begin
        glTexCoord2f(0, 0);   glVertex3f(-FSize, -FSize, z);
        glTexCoord2f(1, 0);   glVertex3f(FSize, -FSize, z);
        glTexCoord2f(1, 1);   glVertex3f(FSize, FSize, z);
        glTexCoord2f(0, 1);   glVertex3f(-FSize, FSize, z);
      end;
    glEnd;
  glPopMatrix;
  glEnable(GL_DEPTH_TEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glColor3f(1, 1, 1);

end;

procedure TTwinkle.Reset;
begin

  // Start a random timer countdown to trigger the twinkle.
  FTimer := 1 + Random * 6;
  // Pick a position, size and orientation.
  FRot := Random * 2 * pi;
  FSize := 1 + Random * 4;
  Pos := cgVector(Random * 192 - 96, 8 + Random * 64, -50);

end;

procedure TTwinkle.Step(time: Single);
begin

  { Decrease the countdown timer. If it reaches 0, the particle has been rendered
    and can be reset. }
  FTimer := FTimer - time;
  if FTimer <= 0 then Reset;

end;

end.
