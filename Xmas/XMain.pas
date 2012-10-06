unit XMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, CgWindow, CgTexture, XParticles, XParticleClasses,
  GL, GLu, XTexObject, MPlayer;

type
  TXForm = class(TCGForm)
    Timer: TTimer;
    MP: TMediaPlayer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MPNotify(Sender: TObject);
  private
  public
    procedure LoadMedia;
    procedure DisposeMedia;
    procedure RenderTerrain;
  end;

var
  XForm: TXForm;
  Terrain: TCGImage;
  TerrainTex, SkyTex, StarTex, MoonTex, TreeTex, WishesTex: TTexObject;
  ParticleTex: array [1..4] of TTexObject;
  CloudTex: array [1..2] of TTexObject;
  Snow: TParticleSystem;
  Twinkle: TParticleSystem;
  T: Single = 0;
  T_LIST: Cardinal;

implementation

{$R *.DFM}

procedure TXForm.FormCreate(Sender: TObject);
const
  fogC: array [0..3] of Single = (0, 0, 0.025, 1);
begin

  Show;
  Cursor := crNone;
  InitGL;

  Randomize;

  // Load textures and stuff:
  LoadMedia;

  // Start the music!
  MP.Play;

  // Perspective and viewport setup:
  glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(60, 4/3, 1, 150);
  glMatrixMode(GL_MODELVIEW);
  OnResize(Self);

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glAlphaFunc(GL_GREATER, 0.1);     // Used for billboard textures
  glEnable(GL_FOG);
  glFogf(GL_FOG_DENSITY, 0.025);
  glFogfv(GL_FOG_COLOR, @fogC);

  // Create a display list for the terrain:
  T_LIST := glGenLists(1);
  glNewList(T_LIST, GL_COMPILE);
    RenderTerrain;
  glEndList;

end;

procedure TXForm.FormResize(Sender: TObject);
begin

  glViewport(0, 0, ClientWidth, ClientHeight);

end;

procedure TXForm.LoadMedia;
var
  i: Integer;
begin

  // Load terrain map:
  Terrain := TCGImage.Create;
  Terrain.LoadFromFile('data\terrain.cgi');
  // Load textures:
  TerrainTex := TTexObject.Create;
  TerrainTex.Image.LoadFromFile('data\terrain_tex.cgi');
  TerrainTex.Upload;
  SkyTex := TTexObject.Create;
  SkyTex.Image.LoadFromFile('data\sky_bg.cgi');
  SkyTex.Upload;
  MoonTex := TTexObject.Create;
  MoonTex.Image.LoadFromFile('data\moon.cgi');
  MoonTex.Upload;
  TreeTex := TTexObject.Create;
  TreeTex.Image.LoadFromFile('data\tree.cgi');
  TreeTex.Upload;
  WishesTex := TTexObject.Create;
  WishesTex.Image.LoadFromFile('data\merryxmas.cgi');
  WishesTex.Upload;
  for i := 1 to 2 do
  begin
    CloudTex[i] := TTexObject.Create;
    CloudTex[i].Image.LoadFromFile('data\cloud' + IntToStr(i) + '.cgi');
    CloudTex[i].Upload;
  end;
  for i := 1 to 4 do
  begin
    ParticleTex[i] := TTexObject.Create;
    ParticleTex[i].Image.LoadFromFile('data\particle' + IntToStr(i) + '.cgi');
    ParticleTex[i].Upload;
  end;
  StarTex := TTexObject.Create;
  StarTex.Image.LoadFromFile('data\star.cgi');
  StarTex.Upload;
  // Create particle systems:
  Snow := TParticleSystem.Create;
  Snow.AddParticles(TSnowFlake, 800);
  Twinkle := TParticleSystem.Create;
  Twinkle.AddParticles(TTwinkle, 4);

end;

procedure TXForm.DisposeMedia;
var
  i: Integer;
begin

  Snow.Free;
  Twinkle.Free;
  Terrain.Free;
  // Textures:
  TerrainTex.Free;
  SkyTex.Free;
  MoonTex.Free;
  TreeTex.Free;
  WishesTex.Free;
  for i := 1 to 2 do CloudTex[i].Free;
  for i := 1 to 4 do ParticleTex[i].Free;
  StarTex.Free;

end;

procedure TXForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin

  DisposeMedia;

end;

procedure TXForm.FormPaint(Sender: TObject);
var
  i: Integer;
begin

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glDisable(GL_FOG);
  // Starfield:
  SkyTex.Enable;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);    glVertex3f(-128, -128, -132);
    glTexCoord2f(8, 0);    glVertex3f(128, -128, -132);
    glTexCoord2f(8, 8);    glVertex3f(128, 128, -132);
    glTexCoord2f(0, 8);    glVertex3f(-128, 128, -132);
  glEnd;
  // Moon:
  MoonTex.Enable;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);    glVertex3f(16, 20, -130);
    glTexCoord2f(1, 0);    glVertex3f(42, 20, -130);
    glTexCoord2f(1, 1);    glVertex3f(42, 46, -130);
    glTexCoord2f(0, 1);    glVertex3f(16, 46, -130);
  glEnd;
  // Clouds:
  glColor4f(1, 1, 1, 0.7);
    CloudTex[1].Enable;
    glBegin(GL_QUADS);
      glTexCoord2f(T, 0);    glVertex3f(-128, -20, -128);
      glTexCoord2f(T+1, 0);  glVertex3f(128, -20, -128);
      glTexCoord2f(T+1, 1);  glVertex3f(128, 96, -128);
      glTexCoord2f(T, 1);    glVertex3f(-128, 96, -128);
     glEnd;
    CloudTex[2].Enable;
    glBegin(GL_QUADS);
      glTexCoord2f(T/4, 0);    glVertex3f(-128, -20, -100);
      glTexCoord2f(T/4+1, 0);  glVertex3f(128, -20, -100);
      glTexCoord2f(T/4+1, 1);  glVertex3f(128, 96, -100);
      glTexCoord2f(T/4, 1);    glVertex3f(-128, 96, -100);
    glEnd;
  glColor3f(1, 1, 1);
  // Twinkling stars:
  Twinkle.Render;
  glEnable(GL_FOG);
  // Terrain:
  glPushMatrix;
    glTranslatef(0, -20, 0);
    glScalef(3, 2, 2);
    glCallList(T_LIST);
  glPopMatrix;
  // Tree drop shadows:
  TreeTex.Disable;
  glDisable(GL_DEPTH_TEST);
  // VERY fake, but pretty effective!
  glBegin(GL_TRIANGLE_FAN);
    glColor4f(0, 0, 0, 1);
    glVertex3f(8.5, -8, -25);
    glColor4f(1, 1, 1, 0);
    for i := 0 to 16 do glVertex3f(8.5 + 5*cos(i*pi/8), -8, -25 + 5*sin(i*pi/8));
  glEnd;
  glBegin(GL_TRIANGLE_FAN);
    glColor4f(0, 0, 0, 0.8);
    glVertex3f(-17.5, -8.5, -35);
    glColor4f(1, 1, 1, 0);
    for i := 0 to 8 do glVertex3f(-17.5 + 5*cos(i*pi/4), -8.5, -35 + 5*sin(i*pi/4));
  glEnd;
  glEnable(GL_DEPTH_TEST);
  glColor3f(1, 1, 1);
  // Trees:
  glEnable(GL_ALPHA_TEST);
  TreeTex.Enable;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);    glVertex3f(2, -8.5, -25);
    glTexCoord2f(1, 0);    glVertex3f(15, -8.5, -25);
    glTexCoord2f(1, 1);    glVertex3f(15, 4, -25);
    glTexCoord2f(0, 1);    glVertex3f(2, 4, -25);

    glTexCoord2f(0, 0);    glVertex3f(-10, -9, -35);
    glTexCoord2f(1, 0);    glVertex3f(-25, -9, -35);
    glTexCoord2f(1, 1);    glVertex3f(-25, 2, -35);
    glTexCoord2f(0, 1);    glVertex3f(-10, 2, -35);
  glEnd;
  // "Merry Xmas from Delphi3D":
  WishesTex.Enable;
  glBegin(GL_QUADS);
    glTexCoord2f(0, 0);    glVertex3f(-6, -5, -10);
    glTexCoord2f(1, 0);    glVertex3f(-2, -5, -10);
    glTexCoord2f(1, 1);    glVertex3f(-2, -4, -10);
    glTexCoord2f(0, 1);    glVertex3f(-6, -4, -10);
  glEnd;
  glDisable(GL_ALPHA_TEST);
  // Snow:
  Snow.Render;

  PageFlip;

end;

procedure TXForm.TimerTimer(Sender: TObject);
begin

  // Update particle systems and texture scrolling:
  Snow.Step(50/1000);      // Timer interval is 50 msec, or 50/1000ths of a second.
  Twinkle.Step(50/1000);
  T := T + 0.0025;  if T > 4 then T := 0;
  Paint;

end;

procedure TXForm.RenderTerrain;
var
  x, y: Integer;
  W, H: Integer;
begin

  // Just turn the height map into a bunch of quads.
  TerrainTex.Enable;
  W := Terrain.Width;
  H := Terrain.Height;
  for x := 0 to W-2 do
  begin
    for y := 0 to H-2 do
    begin
      glBegin(GL_QUADS);
        glTexCoord2f(8*x/Terrain.Width, 8*y/Terrain.Height);
        glVertex3f((x*(128 div W))-64, Terrain[x,y].R/8, -y*(128 div H));

        glTexCoord2f(8*(x+1)/Terrain.Width, 8*y/Terrain.Height);
        glVertex3f(((x+1)*(128 div W))-64, Terrain[x+1,y].R/8, -y*(128 div H));

        glTexCoord2f(8*(x+1)/Terrain.Width, 8*(y+1)/Terrain.Height);
        glVertex3f(((x+1)*(128 div W))-64, Terrain[x+1,y+1].R/8, -(y+1)*(128 div H));

        glTexCoord2f(8*x/Terrain.Width, 8*(y+1)/Terrain.Height);
        glVertex3f((x*(128 div W))-64, Terrain[x,y+1].R/8, -(y+1)*(128 div H));
      glEnd;
    end;
  end;

end;

procedure TXForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if Key = VK_ESCAPE then Close;

end;

procedure TXForm.MPNotify(Sender: TObject);
begin

  // Loop the music.
  MP.Play;

end;

end.
