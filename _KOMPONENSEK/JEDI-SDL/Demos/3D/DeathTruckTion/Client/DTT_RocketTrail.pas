unit DTT_RocketTrail;

interface

uses
  OpenGL12, TNT_ParticleSystem, TNT_Vector, TNT_Texture, TNT_Timer;

type
  TRocketTrail = class(TParticle)
  private
    Position: TVector;
    Frame: Cardinal;
    Timer: TTime;
  public
    constructor Create(Owner: TParticleSystem); override;
    procedure Spawn; override;
    procedure Render; override;
    procedure Animate(Time: Single); override;
    procedure OnBeforeRender; override;
    procedure OnAfterRender; override;
    destructor Destroy; override;
  end;

procedure InitTrails;
procedure FlushTrails;

implementation

uses
  TNT_3D;

var
  xx,xy,xz: GLfloat;
  yx,yy,yz: GLfloat;
  TrailTexture: TTexture;

procedure InitTrails;
begin
  TrailTexture := TTexture.Create('smoke.tex', GL_CLAMP, GL_LINEAR, True);
end;

procedure FlushTrails;
begin
  TrailTexture.Free;
end;

constructor TRocketTrail.Create(Owner: TParticleSystem);
begin
  inherited;
  Timer := TTime.Create;
//  Position := Owner.Position;
end;

procedure TRocketTrail.OnBeforeRender;
begin
  glEnable(GL_BLEND);
  glDepthMask(FALSE);
  with Camera do
  begin
    xx := ModelView[0]; xy := ModelView[4]; xz := ModelView[8];
    yx := ModelView[1]; yy := ModelView[5]; yz := ModelView[9];
  end;
end;

procedure TRocketTrail.Render;
const
  size = 10;
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

  TrailTexture.UseFrame(Frame, GL_REPLACE);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2d(1,1); glVertex3f(x+cx, y+cy, z+cz);
    glTexCoord2d(0,1); glVertex3f(x+dx, y+dy, z+dz);
    glTexCoord2d(1,0); glVertex3f(x+bx, y+by, z+bz);
    glTexCoord2d(0,0); glVertex3f(x+ax, y+ay, z+az);
  glEnd;
  TNT.Tris := TNT.Tris + 2;
end;

procedure TRocketTrail.OnAfterRender;
begin
  glDepthMask(TRUE);
  glDisable(GL_BLEND);
end;

procedure TRocketTrail.Spawn;
begin
  inherited;
  Position := System.Position;
  TTime.Create;
end;

procedure TRocketTrail.Animate(Time: Single);
begin
  if Timer.Diff(1/10) then
    Inc(Frame);
  if Frame >= TrailTexture.Frames then
    Die;
end;

destructor TRocketTrail.Destroy;
begin
  Timer.Free;
  inherited;
end;

end.

