unit TNT_Model;

// Gestion des modeles 3D
// ----------------------
//  Les modeles sont chargés à partir d'un fichier .tnt
//  Chaque modele possede un ou plusieurs skins (textures mappées sur
//  le modele)
//  Les modeles peuvent etre constitués de plusieurs sous-objets et de
//  plusieurs frames

interface

uses
  OpenGL12, TNT_Texture, TNT_Vector;

type
  TMesh = record
    Texture: TTexture;
    Start: GLint;
    Count: GLsizei;
  end;

  TModel = class
    NumFrames: Integer;
    NumMeshs: Integer;
    Vertices: array of array of GLfloat;
    Mesh: array of TMesh;
    BColSphereRadius: Single;
    BVisSphereRadius: Single;
    constructor Create(FileName: String);
    procedure Render(Frame: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils, TNT_File, TNT_3D;

constructor TModel.Create(FileName: String);
var
  F: TFile;
  i, NumVerts: Integer;
begin
  inherited Create;
  FileName := 'models/' + FileName;
  F := TFile.Open(FileName);
  F.ReadStr;                      // TNT-MDL,0
  NumMeshs := F.ReadInt;
  SetLength(Mesh, NumMeshs);
  for i := 0 to NumMeshs-1 do
    with Mesh[i] do
    begin
      Texture := TTexture.Create('models/' + F.ReadStr, GL_CLAMP, GL_LINEAR, True);
      Start := F.ReadInt;
      Count := F.ReadInt;
    end;
  NumFrames := F.ReadInt;
  SetLength(Vertices, NumFrames);
  NumVerts := F.ReadInt;
  for i := 0 to NumFrames-1 do
  begin
    SetLength(Vertices[i], NumVerts);
    F.Read(Vertices[i, 0], NumVerts * SizeOf(Single));
  end;
  BVisSphereRadius := F.ReadFloat;
  BColSphereRadius := F.ReadFloat;
  F.Close;
end;

procedure TModel.Render(Frame: Integer);
var
  i: Integer;
begin
  glInterleavedArrays(GL_T2F_N3F_V3F, 0, @Vertices[Frame, 0]);
  for i := 0 to NumMeshs-1 do
    with Mesh[i] do
    begin
      Texture.Use(GL_MODULATE);
      glDrawArrays(GL_TRIANGLES, Start, Count);
      TNT.Tris := TNT.Tris + Cardinal(Count div 3);
    end;
end;

destructor TModel.Destroy;
var
  i: Integer;
begin
  for i := 0 to NumFrames-1 do
    Vertices[i] := nil;
  Vertices := nil;
  for i := 0 to NumMeshs-1 do
    with Mesh[i] do
      Texture.Destroy;
  Mesh := nil;
end;

end.

