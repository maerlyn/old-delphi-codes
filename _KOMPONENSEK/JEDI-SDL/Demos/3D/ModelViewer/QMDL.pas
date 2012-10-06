unit QMDL;

{*******************************************************}
{                                                       }
{       Quake Model routines v1.0                       }
{                                                       }
{       Copyright (c) 1999 Ilkka Tuomioja               }
{                                                       }
{       Currently Supported:                            }
{       Quake 2 Model - .MD2                            }
{                                                       }
{       Future support:                                 }
{       Quake 1 Model - .MDL                            }
{       Kingpin Model - .MDX                            }
{                                                       }
{       .MD2 based on format specifications by          }
{       Daniel E. Schoenblum                            }
{                                                       }
{      (.MDL based on format specifications by          }
{       Olivier Montanuy)                               }
{                                                       }
{       If someone knows Kinpin's MDX specs,            }
{       I would be very insterested in it...            }
{                                                       }
{       Ported to JEDI-SDL by Dominique Louis on        }
{       5th of September                                }
{*******************************************************}

interface

uses
  SysUtils,
  Classes,
  OpenGL12,
  Logger,
  SDL,
  SDL_Image;

const
  // file ID
  MagicMDL = $4F504449; // = 'IDPO';
  MagicMD2 = $32504449;	// = 'IDP2';
  MagicMDX = $58504449; // = 'IDPX';

  // Versions for different MD_s (are they always the same?)
  VersionMDL = 6;
  VersionMD2 = 8;
  VersionMDX = 4;

  // Pre-defined limits of Q2
  MaxTriangles = 4096;
  MaxVertices = 2048;
  MaxTextureCoordinates = 2048;
  MaxFrames = 512;
  MaxSkins = 32;

type
  TModelType = (mtNone, mtMDL, mtMD2, mtMDx); // MDx = Kingpin

  TMD2Header = record
    Magic: Integer;
    Version: Integer;
    SkinWidth: Integer;
    SkinHeight: Integer;
    FrameSize: Integer;
    NumSkins: Integer;
    NumVertices: Integer;
    NumTexCoords: Integer;
    NumTriangles: Integer;
    NumGlCommands: Integer;
    NumFrames: Integer;
    OffsetSkins: Integer;
    OffsetTexCoords: Integer;
    OffsetTriangles: Integer;
    OffsetFrames: Integer;
    OffsetGlCommands: Integer;
    OffsetEnd: Integer;
  end;

  TTriangleVertex = record
    Vertex: array [0..2] of Single;  //Actually Bytes. Mul by Frame.Scale
    LightNormalIndex: Byte;
  end;
  TTriangleVertexArray = array [0..MaxVertices-1] of TTriangleVertex;
  PTriangleVertex = ^TTriangleVertexArray;

  TTriangle = record
    VertexIndices: array [0..2] of SmallInt;
    TextureIndices: array [0..2] of SmallInt;
  end;
  TTriangleArray = array [0..MaxTriangles-1] of TTriangle;
  PTriangle = ^TTriangleArray;

  TFrame = record
    Scale: array [0..2] of Single;
    Translate: array [0..2] of Single;
    Name: array [0..15] of Char;
    Vertices: PTriangleVertex;    // Actually 0..NumVertices. Use GetMem
  end;
  TFrames = array [0..MaxFrames-1] of TFrame;
  PFrame = ^TFrames;

  TFrameArray = record
    Frame: PFrame;
  end;

  TSkinName = array [0..63] of Char;

  TTextureCoordinateIndex = record
    S: SmallInt;
    T: SmallInt;
  end;
  TTextureCoordinateIndexArray = array [0..MaxTextureCoordinates-1] of TTextureCoordinateIndex;
  PTextureCoordinateIndex = ^TTextureCoordinateIndexArray;

  TGlCommandVertex = record
    S, T: Single;
    VertexIndex: Integer;
  end;
  TGlCommandVertexArray = array [0..100000] of TGlCommandVertex;
  PGlCommandVertex = ^TGlCommandVertexArray;

  TGlCommand = record
    NumCommands: Integer;
    Commands: PGlCommandVertex;
  end;
  TGlCommandArray = array [0..100000] of TGlCommand;
  PGlCommand = ^TGlCommandArray;

  TQuakeModel = class
  private
    FIsLoaded: Boolean;
    FFileName: TFileName;
    FHeader: TMD2Header;
//    FSkinID: array [0..MaxSkins-1] of TGLuInt;
    FSkinID : TGLuInt;
    FTexCoords: PTextureCoordinateIndex;
//    FTriangles: PTriangle;
    FCommands: PGlCommand;
    FNumCommands: Integer;
    FFrames: TFrameArray;
    FModelType: TModelType;
    FSkin: tFileName;
    procedure LoadFromMD2;
    procedure ShowMD2(Index: Integer);
    function LoadTexture(FileName: String): TGLuInt;
    procedure LoadFromFile;
    procedure SetFileName(const Value: TFileName);
  public
    property IsLoaded: Boolean read FIsLoaded;
    property NumSkins: Integer read FHeader.NumSkins;
    property NumVertices: Integer read FHeader.NumVertices;
    property NumTexCoords: Integer read FHeader.NumTexCoords;
    property NumTriangles: Integer read FHeader.NumTriangles;
    property NumGlCommands: Integer read FHeader.NumGlCommands;
    property NumCommandArrays: Integer read FNumCommands;
    property NumFrames: Integer read FHeader.NumFrames;
    property Skin : TFileName read FSkin write FSkin;
    property Model : TFileName read FFileName Write SetFileName;
    procedure SaveToFile(FileName: string);
    procedure Show(Index: Integer);
    // Needs a free routine!
    constructor Create;
  end;

implementation

{ TQuakeModel }
procedure TQuakeModel.LoadFromFile;
var
  FHandle: Integer;
  Magic: Integer;
begin
  If not FileExists(FFileName) then
    exit;
  FHandle := FileOpen(FFileName, fmOpenRead or fmShareDenyNone);
  FileRead(FHandle, Magic, SizeOf(Integer));
  FileClose(FHandle);
  case Magic of
//    MagicMDL:
//      LoadFromMDL(FileName);
    MagicMD2:
      LoadFromMD2;
//    MagicMDX:
//      LoadFromMDXL(FileName);
  end;
end;

procedure TQuakeModel.LoadFromMD2;
var
  Loader: TMemoryStream;
  i, j, k: Integer;
//  Skin: TSkinName;
  GLCommands: Integer;
  TempVertex: Integer;
begin
  Loader := TMemoryStream.Create;
  Loader.LoadFromFile(FFileName);
  If Loader.Size = 0 then exit;
  Loader.ReadBuffer(FHeader, SizeOf(TMD2Header));
  If FHeader.Magic <> MagicMD2 then exit;

  // Load Skins (PCX)
//  Loader.Seek(FHeader.OffsetSkins, soFromBeginning);
//  for i:= 0 to FHeader.NumSkins-1 do
//  begin
//    Loader.ReadBuffer(Skin, 64);
//    FSkinID[i] := LoadTexture(Skin);
//  end;
  //i := 0;
  LoadTexture(FSkin);
  //FSkinID[i] :=
// TexCoords and triangles are not necessary...
{
  // Load TexCoords
  Loader.Seek(FHeader.OffsetTexCoords, soFromBeginning);
  GetMem(FTexCoords, FHeader.NumTexCoords*SizeOf(TTextureCoordinate));
  for i:= 0 to FHeader.NumTexCoords-1 do
  begin
    Loader.ReadBuffer(FTexCoords[i], SizeOf(TTextureCoordinate));
  end;
}
{
  // Load Tris
  Loader.Seek(FHeader.OffsetTriangles, soFromBeginning);
  GetMem(FTriangles, FHeader.NumTriangles*SizeOf(TTriangle));
  for i:= 0 to FHeader.NumTriangles-1 do
  begin
    Loader.ReadBuffer(FTriangles[i], SizeOf(TTriangle));
  end;
}
  // Load Frames
  Loader.Seek(FHeader.OffsetFrames, soFromBeginning);
  GetMem(FFrames.Frame, FHeader.NumFrames*SizeOf(TFrame));
  for i:= 0 to FHeader.NumFrames-1 do
  begin
    GetMem(FFrames.Frame[i].Vertices, FHeader.NumVertices*SizeOf(TTriangleVertex));
    Loader.ReadBuffer(FFrames.Frame[i].Scale, 3*SizeOf(Single));
    Loader.ReadBuffer(FFrames.Frame[i].Translate, 3*SizeOf(Single));
    Loader.ReadBuffer(FFrames.Frame[i].Name, 16*SizeOf(Char));
    for j:= 0 to FHeader.NumVertices-1 do
    begin
      for k:= 0 to 2 do
      begin
        // Scale and translate
        Loader.ReadBuffer(TempVertex, SizeOf(Byte));
        FFrames.Frame[i].Vertices[j].Vertex[k] := TempVertex;
        FFrames.Frame[i].Vertices[j].Vertex[k] :=
          FFrames.Frame[i].Vertices[j].Vertex[k] * FFrames.Frame[i].Scale[k];
        FFrames.Frame[i].Vertices[j].Vertex[k] :=
          FFrames.Frame[i].Vertices[j].Vertex[k] + FFrames.Frame[i].Translate[k];
      end;
      Loader.ReadBuffer(FFrames.Frame[i].Vertices[j].LightNormalIndex, SizeOf(byte));
    end;
  end;

  // Load glCommands
  Loader.Seek(FHeader.OffsetGlCommands, soFromBeginning);
  i := 0;
  GLCommands := 0;
  // Check the number of arrays.
  while i < FHeader.NumGlCommands do
  begin
    Loader.ReadBuffer(j, SizeOf(Integer));
    j := Abs(j);
    Loader.Seek(j*SizeOf(TGLCommandVertex), soFromCurrent);
    Inc(i, j*3+1);
    Inc(GLCommands)
  end;
  // Load the arrays.
  GetMem(FCommands, GLCommands*SizeOf(TGlCommand));
  Loader.Seek(FHeader.OffsetGlCommands, soFromBeginning);
  for i:= 0 to GLCommands-1 do
  begin
    Loader.ReadBuffer(FCommands[i].NumCommands, SizeOf(Integer));
    GetMem(FCommands[i].Commands, Abs(FCommands[i].NumCommands)*SizeOf(TGlCommandVertex));
    for j:= 0 to Abs(FCommands[i].NumCommands)-1 do
      Loader.ReadBuffer(FCommands[i].Commands[j], SizeOf(TGlCommandVertex));
  end;
  FNumCommands := GLCommands;

  Loader.Free;
  FIsLoaded := True;
  FModelType := mtMD2;
end;

procedure TQuakeModel.SaveToFile(FileName: string);
var
  FOut: TFileStream;
begin
  FOut := TFileStream.Create(FileName, fmOpenWrite or fmShareDenyNone);
  FOut.Write(FHeader.NumVertices, 4);
  FOut.Write(FFrames.Frame[0].Vertices, FHeader.NumVertices*SizeOf(TTriangleVertex));
  FOut.Free
end;
procedure TQuakeModel.Show(Index: Integer);
begin
  case FModelType of
//    mtMDL:
//      ShowMDL(Index);
    mtMD2:
      ShowMD2(Index);
//    mtMDX:
//      ShowMDX(Index);
  end;
end;

procedure TQuakeModel.ShowMD2(Index: Integer);
var
  i, j: Integer;
begin
  glTexEnvf(GL_TEXTURE_ENV,GL_TEXTURE_ENV_MODE,GL_MODULATE);
  glBindTexture(GL_TEXTURE_2D, FSkinID);
  glEnable(GL_TEXTURE_2D);

  for i := 0 to FNumCommands-1 do
  begin
    // Neg -> Fans, Pos -> Strips.
    if FCommands[i].NumCommands > 0 then
      glBegin(GL_TRIANGLE_STRIP)
    else
      glBegin(GL_TRIANGLE_FAN);
      
    for j:= 0 to Abs(FCommands[i].NumCommands)-1 do
    begin
      with FCommands[i].Commands[j] do
        glTexCoord2f(S, T);
      with FFrames.Frame[Index].Vertices[FCommands[i].Commands[j].VertexIndex] do
        glVertex3f(Vertex[0], -Vertex[1], Vertex[2]);
    end;
    glEnd; // End Strip or Fan
  end;
  glDisable(GL_TEXTURE_2D);
end;

function TQuakeModel.LoadTexture(FileName: String): TGLuInt;
var
  // Create storage space for the texture
  TextureImage: PSDL_Surface;
  FileExtention : string;
begin
  TextureImage := IMG_Load(PChar( FileName ));
  if (TextureImage <> nil) then
  begin
    glGenTextures(1, @FSkinID);

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glBindTexture(GL_TEXTURE_2D, FSkinID);

    // Generate The Texture
    FileExtention := ExtractFileExt(FileName);
    if FileExtention = '.bmp' then
      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
                    TextureImage.h, 0, GL_BGR,
                    GL_UNSIGNED_BYTE, TextureImage.pixels )
    else if FileExtention = '.pcx' then
      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
                    TextureImage.h, 0, GL_BGRA,
                    GL_UNSIGNED_BYTE, TextureImage.pixels );

  end
  else
    raise Exception.Create('Unable to Load Texture');



  Result := 0;//TexID;
end;

constructor TQuakeModel.Create;
begin
  inherited create;
  FIsLoaded := False;
end;

procedure TQuakeModel.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile;
end;

end.

