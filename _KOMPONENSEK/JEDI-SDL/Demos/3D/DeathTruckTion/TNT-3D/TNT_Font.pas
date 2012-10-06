unit TNT_Font;

interface

uses
  OpenGL12, TNT_Texture;

type
  TFont = class
  private
    Base: GLuint;
    Texture: TTexture;
  public
    constructor Create(FileName: String);
    procedure Print(X, Y, Size: Single; FontSet: Cardinal; Str: String);
    destructor Destroy; override;
  end;

var
  Font: TFont;

implementation

uses
  TNT_3D, TNT_File;

constructor TFont.Create(FileName: String);
var
  i: GLuint;
  x,y: GLfloat;
  MetricsName: String;
  Metrics: TFile;
begin
  inherited Create;
  Console.Log('Creating font: ' + FileName);
  Texture := TTexture.Create(FileName, GL_CLAMP, GL_LINEAR, True);
  MetricsName := Copy(FileName, 0, Length(FileName)-3);
  Console.Log('Reading metrics from ' + MetricsName + 'dat');
  Metrics := TFile.Open(MetricsName + 'dat');

  Base := glGenLists(256);
  for i := 0 to 255 do
  begin
    x := (i mod 16)/16;
    y := (i div 16)/16;
    glNewList(Base+i, GL_COMPILE);
      glBegin(GL_QUADS);
        glTexCoord2f(x, y + 0.0625);
        glVertex2d(0, 16);
        glTexCoord2f(x + 0.0625, y + 0.0625);
        glVertex2i(16, 16);
				glTexCoord2f(x + 0.0625, y);
        glVertex2i(16, 0);
				glTexCoord2f(x, y);
        glVertex2i(0, 0);
      glEnd;
      glTranslatef(Metrics.ReadByte+1, 0, 0);
    glEndList;
  end;
  Metrics.Close;
end;

procedure TFont.Print(X, Y, Size: Single; FontSet: Cardinal; Str: String);
begin
  Texture.Use(GL_MODULATE);
  glPushMatrix;
    glLoadIdentity;
    glTranslatef(X, Y, 0);
    glScalef(Size, Size, 1);
    glListBase(Base-32 + 128*FontSet);
    glCallLists(Length(Str), GL_BYTE, PChar(Str));
  glPopMatrix;
end;

destructor TFont.Destroy;
begin
  glDeleteLists(Base, 256);
  Texture.Free;
  inherited Destroy;
end;

end.

