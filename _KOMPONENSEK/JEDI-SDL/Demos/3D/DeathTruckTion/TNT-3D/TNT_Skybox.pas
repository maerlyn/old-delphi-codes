unit TNT_Skybox;

interface

uses
  OpenGL12, TNT_Entity, TNT_Texture;

type
  TSkybox = class(TEntity)
  private
    Tex: array [0..5] of TTexture;
  public
    constructor Create(Name: String);
    procedure Render; override;
    destructor Destroy; override;
  end;

implementation

uses
  TNT_3D, TNT_Vector;

constructor TSkybox.Create(Name: String);
begin
  inherited EntityCreate(TYPE_SKYBOX, Vector(0,0,0), Vector(0,0,0), 0);
  Name := 'skybox/' + Name + '/' + Name;
  Tex[0] := TTexture.Create(Name + '_rt.tex', GL_CLAMP, GL_LINEAR, False);
  Tex[1] := TTexture.Create(Name + '_ft.tex', GL_CLAMP, GL_LINEAR, False);
  Tex[2] := TTexture.Create(Name + '_lf.tex', GL_CLAMP, GL_LINEAR, False);
  Tex[3] := TTexture.Create(Name + '_bk.tex', GL_CLAMP, GL_LINEAR, False);
  Tex[4] := TTexture.Create(Name + '_up.tex', GL_CLAMP, GL_LINEAR, False);
  Tex[5] := TTexture.Create(Name + '_dn.tex', GL_CLAMP, GL_LINEAR, False);
end;

procedure TSkybox.Render;
const
  Size = 10;
begin
  glPushMatrix;
  glLoadIdentity;
  Camera.Rotation;

    Tex[0].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(-Size, Size, -Size);
      glTexCoord2f(0, 1); glVertex3f(-Size, -Size, -Size);
      glTexCoord2f(1, 1); glVertex3f(Size, -Size, -Size);
      glTexCoord2f(1, 0); glVertex3f(Size, Size, -Size);
    glEnd;
    Tex[1].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(Size, Size, -Size);
      glTexCoord2f(0, 1); glVertex3f(Size, -Size, -Size);
      glTexCoord2f(1, 1); glVertex3f(Size, -Size, Size);
      glTexCoord2f(1, 0); glVertex3f(Size, Size, Size);
    glEnd;
    Tex[2].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(Size, Size, Size);
      glTexCoord2f(0, 1); glVertex3f(Size, -Size, Size);
      glTexCoord2f(1, 1); glVertex3f(-Size, -Size, Size);
      glTexCoord2f(1, 0); glVertex3f(-Size, Size, Size);
    glEnd;
    Tex[3].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(-Size, Size, Size);
      glTexCoord2f(0, 1); glVertex3f(-Size, -Size, Size);
      glTexCoord2f(1, 1); glVertex3f(-Size, -Size, -Size);
      glTexCoord2f(1, 0); glVertex3f(-Size, Size, -Size);
    glEnd;
{    Tex[4].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(x-Size, y+Size, z+Size);
      glTexCoord2f(0, 1); glVertex3f(x-Size, y+Size, z-Size);
      glTexCoord2f(1, 1); glVertex3f(x+Size, y+Size, z-Size);
      glTexCoord2f(1, 0); glVertex3f(x+Size, y+Size, z+Size);
    glEnd;
    Tex[5].Use(GL_REPLACE);
    glBegin(GL_QUADS);
      glTexCoord2f(0, 0); glVertex3f(x-Size, y-Size, z-Size);
      glTexCoord2f(0, 1); glVertex3f(x-Size, y-Size, z+Size);
      glTexCoord2f(1, 1); glVertex3f(x+Size, y-Size, z+Size);
      glTexCoord2f(1, 0); glVertex3f(x+Size, y-Size, z-Size);
    glEnd;}
  glPopMatrix;
  glClear(GL_DEPTH_BUFFER_BIT);
  glEnable(GL_DEPTH_TEST);
  TNT.Tris := TNT.Tris + 4;
end;

destructor TSkybox.Destroy;
var
  i: Integer;
begin
  for i := 0 to 5 do
    Tex[i].Free;
  inherited Destroy;
end;

end.

