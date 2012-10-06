unit TNT_Texture;

// Gestion des textures
// --------------------
//  TTexture.Create:
//    TexWrap: GL_REPEAT or GL_CLAMP
//    TexFilter: GL_NEAREST or GL_LINEAR
//    Mipmap: True or False
//  TTexture.Use:
//    TexEnv: GL_MODULATE, GL_DECAL, or GL_REPLACE

//  .TEX:
//
//    Width: Integer
//    Height: Integer
//    Bpp(BytesPerPixel): Integer
//    NumFrames: Integer
//    Speed: Integer (images/sec)
//    Frame0 data
//    Frame1 data
//    Frame2 data
//    ...

interface

uses
  OpenGL12, TNT_Timer;

type
  TTexture = class
  private
    CurrFrame: Cardinal;
    Name: array of GLuint;
    Time: TTime;
    Freq: Single;
  public
    Frames, Width, Height, Bpp: Cardinal;
    Loop: Boolean;
    constructor Create(FileName: String; TexWrap, TexFilter: GLint; Mipmap: Boolean);
    procedure Use(TexEnv: GLint);
    procedure UseFrame(n: Cardinal; TexEnv: GLint);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, TNT_3D, TNT_File;

var
  Current: GLuint;
  CurrTexEnv: GLint;

constructor TTexture.Create(FileName: String; TexWrap, TexFilter: GLint; Mipmap: Boolean);
var
  TexFile: TFile;
  ImageData: PChar;
  Size, i: Integer;
  glType: GLuint;
begin
  inherited Create;
  TexFile := TFile.Open(FileName);
  Width := TexFile.ReadInt;
  Height := TexFile.ReadInt;
  Bpp := TexFile.ReadInt;
  glType := 0;
  case Bpp of
    1: glType := GL_LUMINANCE;
    3: glType := GL_RGB;
    4: glType := GL_RGBA;
  end;
  Size := Width*Height*Bpp;
  Frames := TexFile.ReadInt;
  SetLength(Name, Frames);
  glGenTextures(Frames, @Name[0]); // Crashes here in Delphi 4
  Freq := 1/TexFile.ReadInt;
  GetMem(ImageData, Size);
  for i:=0 to Frames-1 do
  begin
    TexFile.Read(ImageData^, Size);

    glBindTexture(GL_TEXTURE_2D, Name[i]);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, TexWrap);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, TexWrap);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, TexFilter);

    if Mipmap then
    begin
      if TexFilter = GL_NEAREST then
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST)
      else
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      gluBuild2DMipmaps(GL_TEXTURE_2D, Bpp, Width, Height, glType, GL_UNSIGNED_BYTE, ImageData);
    end
    else
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, TexFilter);
      glTexImage2D(GL_TEXTURE_2D, 0, Bpp, Width, Height, 0, glType, GL_UNSIGNED_BYTE, ImageData);
    end;
  end;
  FreeMem(ImageData);
  TexFile.Close;
  Console.Log('Load texture: ' + FileName + ' ID: ' + IntToStr(Name[0]));
  Current := Frames-1;
  CurrTexEnv := 0;
  Time := TTime.Create;
  CurrFrame := 0;
end;

procedure TTexture.Use(TexEnv: GLint);
begin
  UseFrame(CurrFrame, TexEnv);
  if Time.Diff(Freq) then
    Inc(CurrFrame);
  if CurrFrame=Frames then
  begin
    CurrFrame := 0;
    Loop := True;
  end;
end;

procedure TTexture.UseFrame(n: Cardinal; TexEnv: GLint);
begin
  if Name[n] <> Current then
  begin
    if TexEnv <> CurrTexEnv then
    begin
      CurrTexEnv := TexEnv;
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, TexEnv);
    end;
    glBindTexture(GL_TEXTURE_2D, Name[n]);
    Current := Name[n];
  end;
end;

destructor TTexture.Destroy;
begin
  Console.Log('Unload texture ' + IntToStr(Name[0]));
  glDeleteTextures(Frames, @Name[0]);
  Time.Free;
  Name := nil;
  inherited Destroy;
end;

end.

