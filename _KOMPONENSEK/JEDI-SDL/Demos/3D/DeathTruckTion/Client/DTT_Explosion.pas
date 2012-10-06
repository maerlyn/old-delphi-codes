unit DTT_Explosion;

interface

uses
  OpenGL12, TNT_ParticleSystem, TNT_Vector, TNT_Texture, TNT_Timer, DTT_Sound;

type
  TExplosion = class(TParticle)
  private
    Position: TVector;
    Frame: Cardinal;
    Timer: TTime;
    ExpTexture: TTexture;
  public
    constructor Create(Owner: TParticleSystem); override;
    procedure Spawn; override;
    procedure Render; override;
    procedure Animate(Time: Single); override;
    procedure OnBeforeRender; override;
    procedure OnAfterRender; override;
    destructor Destroy; override;
  end;

procedure InitExplosions;
procedure Explosion(Pos: TVector; n: Byte);
procedure FlushExplosions;

var
  ExplosionSound: TSample;

implementation

uses
  TNT_3D;

var
  Texture: TTexture;
  Texture1: TTexture;
  Texture2: TTexture;
  PartSys: TParticleSystem;
  xx,xy,xz: GLfloat;
  yx,yy,yz: GLfloat;

procedure InitExplosions;
begin
  Texture1 := TTexture.Create('explosion.tex', GL_CLAMP, GL_LINEAR, True);
  Texture2 := TTexture.Create('explosion2.tex', GL_CLAMP, GL_LINEAR, True);
  PartSys := TParticleSystem.Create(TExplosion, 64, Vector(0,0,0), Vector(0,0,0), 0);
end;

procedure Explosion(Pos: TVector; n: Byte);
begin
  ExplosionSound.Play;
  PartSys.Position := Pos;
  if n = 0 then
    Texture := Texture1
  else
    Texture := Texture2;
  PartSys.Spawn(1);
end;

procedure FlushExplosions;
begin
  PartSys.Free;
  Texture1.Free;
  Texture2.Free;
end;

constructor TExplosion.Create(Owner: TParticleSystem);
begin
  inherited;
  Timer := TTime.Create;
  Position := Owner.Position;
end;

procedure TExplosion.OnBeforeRender;
begin
  glEnable(GL_BLEND);
  glDepthMask(FALSE);
  with Camera do
  begin
    xx := ModelView[0]; xy := ModelView[4]; xz := ModelView[8];
    yx := ModelView[1]; yy := ModelView[5]; yz := ModelView[9];
  end;
end;

procedure TExplosion.Render;
const
  size = 20;
var
	x,y,z: GLfloat;
  ax,ay,az: GLfloat;
  bx,by,bz: GLfloat;
  cx,cy,cz: GLfloat;
  dx,dy,dz: GLfloat;
begin
	ax := (-xx-yx)*size;
	ay := (-xy-yy)*size;
	az := (-xz-yz)*size;
	bx := (xx-yx)*size;
	by := (xy-yy)*size;
	bz := (xz-yz)*size;
	cx := (xx+yx)*size;
	cy := (xy+yy)*size;
	cz := (xz+yz)*size;
	dx := (-xx+yx)*size;
	dy := (-xy+yy)*size;
	dz := (-xz+yz)*size;

  x := Position.x;
  y := Position.y;
  z := Position.z;

  ExpTexture.UseFrame(Frame, GL_REPLACE);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2d(1,1); glVertex3f(x+cx, y+cy, z+cz);
    glTexCoord2d(0,1); glVertex3f(x+dx, y+dy, z+dz);
    glTexCoord2d(1,0); glVertex3f(x+bx, y+by, z+bz);
    glTexCoord2d(0,0); glVertex3f(x+ax, y+ay, z+az);
  glEnd;
  TNT.Tris := TNT.Tris + 2;
end;

procedure TExplosion.OnAfterRender;
begin
  glDepthMask(TRUE);
  glDisable(GL_BLEND);
end;

procedure TExplosion.Spawn;
begin
  inherited;
  Position := System.Position;
  Timer.Create;
  Frame := 0;
  ExpTexture := Texture;
end;

procedure TExplosion.Animate(Time: Single);
begin
  if Timer.Diff(1/10) then
    Inc(Frame);
  if Frame >= ExpTexture.Frames then
    Alive := False;
end;

destructor TExplosion.Destroy;
begin
  Timer.Free;
  inherited;
end;

end.

