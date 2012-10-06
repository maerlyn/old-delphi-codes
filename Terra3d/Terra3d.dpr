{
Terra3D - A 3D OpenGL terrain generator
Copyright (C) 2001  Steve Wortham

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation version 2

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

 *  TERRA3D
 *   by: Steve Wortham
 *   E-Mail: steve.wortham@juno.com
 *   website: www.stevescomp.atfreeweb.com
 -
 +  Translated into Delphi5 by Tele von Zsinór
 +   eMail: TeleVonZsinor@webmail.hu
 +   website: http://TeleVonZsinor.ini.hu
}
program Terra3d;
{$APPTYPE CONSOLE}
uses
  SysUtils,OpenGL,Windows,Messages,Classes,Graphics;

type Timer = record
      frequency: Int64;
      resolution: double;
      mm_timer_start: longint;
      mm_timer_elapsed: longint;
      performance_timer: boolean;
      performance_timer_start: Int64;
      performance_timer_elapsed: Int64;
     end;

     vertex = record
      x,
      y,
      z: double;
     end;

const LightAmbient: array[0..3] of GLfloat = (0.35,0.75,0.55,1.0);
      LightDiffuse: array[0..3] of GLfloat = (1.0,1.0,1.0,1.0);
      LightSpecular: array[0..3] of GLfloat = (1.0,1.0,1.0,1.0);
      LightPosition: array[0..3] of GLfloat = (0.0,0.0,0.0,1.0);

      fogMode: array[0..2] of GLuint = (GL_EXP,GL_EXP2,GL_LINEAR);
      fogFilter: GLuint = 0;
      fogColor: array[0..3] of GLfloat = (0.9019,0.8588,0.7882,1);

      MAX: integer = 1000;
      skyMAX: integer = 50;

      piover180: double = 0.0174535925;

var v_hDC: HDC;
    v_hRC: HGLRC;
    v_hWnd: HWND;
    DIFF: integer = 1;
    length: double;
    keys: array[0..255] of boolean;
    active: boolean = true;
    fullscreen: boolean = true;
    light: boolean;
    wireframe: boolean = false;
    water: boolean = true;
    multitexture: boolean = true;
    lp,fp,sp,wp,mp,aq,sq: boolean;

    quadratic: GLUquadricObj;
    texture: array[0..4] of GLuint;

    base: GLuint;
    filter: GLuint;
    v_object: GLuint = 0;

    v1,v2,t1,t2,t3: vertex;
    n1: array[0..MAX-1,0..MAX-1] of vertex;
    n2: array[0..MAX-1,0..MAX-1] of vertex;
    n3: array[0..MAX-1,0..MAX-1] of vertex;
    n4: array[0..MAX-1,0..MAX-1] of vertex;
    nfield: array[0..MAX-1,0..MAX-1] of vertex;
    sky: array[0..skyMAX-1,0..skyMAX-1] of vertex;
    alpha: array[0..MAX-1,0..MAX-1] of double;

    xtrans: GLfloat = MAX/2;
    xptrans: GLfloat = 0;
    ytrans: GLfloat = 0;
    yptrans: GLfloat = 0;
    ztrans: GLfloat = MAX/2;
    zptrans: GLFloat = 0;

    Progress: double = 0;

    xrot,yrot,zrot: GLfloat = 0;
    Speed: GLfloat;

    XPOS: GLfloat = -MAX/2;
    ZPOS: GLfloat = -MAX/2;
    XP,ZP: GLfloat = 0;

    sceneroty,heading,zprot: GLfloat;
    walkbias,walkbiasangle,yptrans2: GLfloat = 0;

    quality: integer = 4;
    H: GLfloat = 0;
    angle: GLfloat;
    xdist,zdist,Hypoy: GLfloat;

    frames: integer = 0;
    Time1,Time2,DiffTime: double;
    FPS: double = 0;
    multiplier: double = 360/(3.14159*2);

function ABS(A: GLfloat): GLfloat;
begin
 if A < 0 then
  Result := -A
 else
  Result := A;
end;

procedure WndProc(var Message: TMessage);
begin
//
end;

procedure TimerInit(Melyik: Timer);
begin
 if not QueryPerformanceFrequency(Melyik.frequency) then
 begin
  Melyik.performance_timer := false;
  Melyik.mm_timer_start := integer(Now);
  Melyik.resolution := 1.0 / 1000.0;
  Melyik.frequency := 1000;
  Melyik.mm_timer_elapsed := Melyik.mm_timer_start;
 end
 else
 begin
  QueryPerformanceCounter(Melyik.performance_timer_start);
  Melyik.performance_timer := true;
  Melyik.resolution := 1.0 / Melyik.frequency;
  Melyik.performance_timer_elapsed := Melyik.performance_timer_start;
 end;
end;

function TimerGetTime(Melyik: Timer): double;
var time: int64;
begin
 if Melyik.performance_timer then
 begin
  QueryPerformanceCounter(time);
  Result := ((time - Melyik.performance_timer_start) * Melyik.resolution) * 1000.0;
 end
 else
 begin
  Result := ((Now - Melyik.mm_timer_start) * Melyik.resolution) * 1000.0;
 end;
end;

procedure BuildFont;
var font: HFONT;
begin
 base := glGenLists(96);
 font := CreateFont(-16,0,0,0,FW_BOLD,false,0,0,ANSI_CHARSET,OUT_TT_PRECIS,CLIP_DEFAULT_PRECIS,ANTIALIASED_QUALITY,FF_DONTCARE+DEFAULT_PITCH,'Verdana');
 SelectObject(v_hDC,font);
 wglUseFontBitmaps(v_hDC,32,96,base);
end;

procedure KillFont;
begin
 glDeleteLists(base,96);
end;

procedure glPrint(const fmt: char);
var text: array[0..255] of char;
    ap: va_list;
begin
 if fmt = null then Exit;

 va_start(ap,fmt);
  vsprintf(text,fmt,ap);
 va_end(ap);

 glPushAttrib(GL_LIST_BIT);
 glListBase(base - 32);
 glCallLists(Length(text),GL_UNSIGNED_BYTE,text);
 glPopAttrib;
end;

function LoadBMP(FileName: char): TGraphic;
var v_File: FILE;
    ize: TGraphic;
begin
 if Filename = '' then
  Result := null;

 v_File := FileOpen(FileName,fmOpenRead);
 if v_File <> 0 then
 begin
  FileClose(v_File);
  ize.LoadFromFile(Filename);
  Result := ize;
 end
 else
  Result := null;
end;

function LoadGLTextures: integer;
var Status: integer = false;
    TextureImage: array[0..0] of TGraphics;
begin
 //memset(TextureImage,0,sizeuf(void *)*1);
 if (TextureImage[0] := LoadBMP('texture/asphalt.bmp')) then
 begin
  Status := true;
  glGenTextures(1,Texture[0]);
  glBindTexture(GL_TEXURE_2D,Texture[0]);
  glTextParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTextParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  glBuild2DMipmaps(GL_TEXTURE_2D,3,TextureImage[0]->sizeX,TextureImage[0]->sizeY,GL_RGB,GL_UNSIGNED_BYTE,TextureImage[0]->data);
 end;
 if (TextureImage[0] := LoadBMP('texture/sky.bmp')) then
 begin
  Status := true;
  glGenTextures(1,Texture[1]);
  glBlindTexture(GL_TEXTURE_2D,Texture[1]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST); end;
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);
 end;
 if (TextureImage[0] := LoadBMP('texture/stone.bmp')) then
 begin
  Status := true;
  glGenTextures(1,Texture[2]);
  glBindTexture(GL_TEXTURE_2D, texture[2]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);
 end;
 if (TextureImage[0] := LaodBmp('texture/Loading.bmp')) then
 begin
  Status := true;
  glGenTextures(1,Texture[3]);
  glBindTexture(GL_TEXTURE_2D, texture[3]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);
 end;
 if (TextureImage[0] := LoadBMP('texture/Water.bmp')) then
 begin
  Status := true;
  glGenTextures(1,Texture[4]);
  glBindTexture(GL_TEXTURE_2D, texture[4]);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR_MIPMAP_NEAREST);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage[0]->sizeX, TextureImage[0]->sizeY, GL_RGB, GL_UNSIGNED_BYTE, TextureImage[0]->data);
 end;
 TextureImage[0].Free;
 Result := Status;
end;

procedure ReSizeGLScene(width,height: GLsizei);
begin
 if height = 0 then
  height := 1;
 glViewPort(0,0,width,height);
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity;
 gluPerspective(45.0,width/height,0.1,50000.0);
 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity;
end;

procedure DrawProgress;
begin
 glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
 glLoadIdentity;
 glTranslatef(0,0,0);
 Progress := Progress + 0.25;
 glColor4f(1,1,1,1);
 glBindTexture(GLTEXTURE_2D,Texture[3]);
 glBegin(GL_TRIANGLE_STRIP);
 glLoadIdentity();
 glTexCoord2f(1,1);
 glVertex3f(1,0-4,-15);
 glTexCoord2f(1,0);
 glVertex3f(1,-.5-4,-15);
 glTexCoord2f(0,1);
 glVertex3f(-1,0-4,-15);
 glTexCoord2f(0,0);
 glVertex3f(-1,-.5-4,-15);
 glEnd();

 glColor4f(0,0,1,1);
 glBegin(GL_QUADS);
 glVertex3f(-1.8f,-1.55f,-5.0f);
 glVertex3f(-1.8f,-1.7f,-5.0f);
 glVertex3f(-1.8f+Progress,-1.7f,-5.0f);
 glVertex3f(-1.8f+Progress,-1.55f,-5.0f);
 glEnd();
end;

function InitGL: integer;
var i,i2,cnt,t,t2: integer;
    hskyMAX: double;
begin
 if not LoadTextures then
 begin
  Result := false;
  Exit;
 end;
 TimerInit;
 glClearColor(0.9019,0.8588,0.7882,1);
 glClearDepth(1.0);
 glEnable(GL_TEXTURE_2D);
 glShadeModel(GL_SMOOTH);
 glEnable(GL_DEPTH_TEST);
 glDepthFunc(GL_LEQUAL);
 glEnable(GL_BLEND);
 glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
 glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);
 glFogi(GL_FOG_MODE,fogMode[2]);
 glFogvf(GL_FOG_COLOR,fogColor);
 glFogf(GL_FOG_DENSITY,0.594);
 glHint(GL_FOG_HINT,GL_NICEST);
 glFogf(GL_FOG_START,10.0);
 glFogf(GL_FOG_END,60.0);
 glEnable(GL_FOG);
 quadratic := gluNewQuadratic;
 gluQuadraticNormals(quadratic,GLU_SMOOTH);
 gluQuadraticTexture(quadratic,GL_TRUE);
 BuildFont;
 wglUseFontBitmaps(v_hDC,32,96,base);
 glClear(GL_COLOR_BUFFER_BI + GL_DEPTH_BUFFER_LIST);
 SwapBuffers(v_hDC);
 SwapBuffers(v_hDC);
 DrawProgress;
 hskyMAX := hypot(skyMAX,skyMAX);
 for i := 0 to skyMAX - 1 do
  for i2 := 0 to skyMax - 1 do
   sky[i,i2].y := (hskyMAX/2-hypoy(i-skyMAX/2,i2-skyMAX/2))/400-13200;
//GENERATE LANDSCAPE;
 Randomize;
 for i := 0 to MAX - 1 do
  for i2 := 0 to MAX - 1 do
  begin
   if ((i<75)or(i2<75)or(i>MAX-75)or(i2>MAX-75) then
    field[i,i2].y := 15
   else
    field[i,i2].y := (Random(100)-50)/4;
  end;
//SMOOTH LANDSCAPE
 for cnt := 0 to 7 do
 begin
  SwapBuffers(v_hDC);
  DrawProgress;
  for t := 1 to MAX - 2 do
   for t2 := 1 to MAX - 2 do
   begin
    field[t,t2].y := (field[t+1,t2].y+field[t,t2-1].y+field[t-1,t2].y+field[t,t2+1].y)/4;
    if (Random div 500 = 1)and(field[t,t2].y > 0)and(cnd = 0) then
    begin
     field[t-1,t2].y := field[t,t2].y/2;
     field[t+1,t2].y := field[t,t2].y/2;
     field[t,t2-1].y := field[t,t2].y/2;
     field[t,t2+1].y := field[t,t2].y/2;
    end;
   end;
 end;
 for i := 0 to MAX - 1 do
  for i2 := 0 to MAX - 1 do
//GENERATE ALPHA INTENSITIES FOR MULTITEXTURE
   if filed[i,i2].y > 0 then
    alpha[i,i2] := field[i,i2].y
   else
    alpha[i,i2] := 0;
//SMOOTH NORMALS AND ALPHA INTENSITIES
 for cnt := 0 to 2 do
 begin
  SwapBuffers(v_hDC);
  DrawProgress;
  for t := 1 to MAX - 2 do
   for t2 := 1 to MAX - 2 do
    alpha[t,t2] := (alpha[t+1,t2]+alpha[t,t2-1]+alpha[t-1,t2]+alpha[t,t2+1])/4;
 end;
 Result := true;
end;



{------------------------------------------------------------------------------}
{--------------------------------FÕPROGRAM-------------------------------------}
{------------------------------------------------------------------------------}

begin
 writeln('proba');
 readln;
end.

