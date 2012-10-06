unit XTexObject;

interface

{ Wrapper class for OpenGL texture objects. Call Upload() to bind the image to
  the texture object, then call Enable() or use glBindTexture() to enable the
  texture. }

uses
  CgTexture, GL;

type
  TTexObject = class
  public
    Image: TCGTexture;
    TexObject: Cardinal;
    constructor Create;
    destructor Destroy; override;
    procedure Enable;
    procedure Disable;
    procedure Upload;
  end;

implementation

constructor TTexObject.Create;
begin

  inherited Create;
  glGenTextures(1, @TexObject);
  glBindTexture(GL_TEXTURE_2D, TexObject);
  Image := TCGTexture.Create;

end;

destructor TTexObject.Destroy;
begin

  glDeleteTextures(1, @TexObject);
  inherited Destroy;

end;

procedure TTexObject.Disable;
begin

  glBindTexture(GL_TEXTURE_2D, 0);

end;

procedure TTexObject.Enable;
begin

  glBindTexture(GL_TEXTURE_2D, TexObject);

end;

procedure TTexObject.Upload;
begin

  Image.HTile := TRUE;
  Image.VTile := TRUE;
  Image.Enable;
  Disable;

end;

end.
