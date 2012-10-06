unit TNT_Landscape;

interface

uses
  OpenGL12, TNT_Texture, TNT_Entity;

type
  TVertex = record
    u,v: GLfloat;
    nx,ny,nz: GLfloat;
    x,y,z: GLfloat;
  end;

  TFace = record
    v0, v1, v2: Word;
  end;

  TLandscape = class(TEntity)
  private
    Texture: TTexture;
    Vertices: array of TVertex;
    Faces: array of TFace;
  public
    Map: array of array of Byte;
    constructor Create(MapName, TexName: String);
    function GetHeight(X, Z: Single): Single;
    function Collision(X, Y, Z: Single): Boolean;
    procedure Render; override;
    destructor Destroy; override;
  end;

implementation

uses
  Math, TNT_File, TNT_Vector, TNT_3D, TNT_Console, SysUtils;

const
  HEIGHT_SCALE = 2;
  SIZE_SCALE = 16;

var
  Strip: array [0..65000] of Word;

constructor TLandscape.Create(MapName, TexName: String);
var
  MapFile: TFile;
  FaceNormals: array of TVector;
  Width, Height: Integer;
  u0, u1, u2, u3, u4: TVector;
  i, j{, k}: Integer;
begin
  inherited EntityCreate(TYPE_LANDSCAPE, Vector(0,0,0), Vector(0,0,0), 0);
  MapFile := TFile.Open(MapName);
  Width := MapFile.ReadInt;
  Height := MapFile.ReadInt;
  MapFile.ReadInt;
  SetLength(Map, Width, Height);
  SetLength(Vertices, Width*Height);
  for j := 0 to Height-1 do
    for i := 0 to Width-1 do
    begin
      Map[i,j] := MapFile.ReadByte;
      with Vertices[j*Width+i] do
      begin
        x := i*SIZE_SCALE;
        y := map[i,j]/HEIGHT_SCALE;
        z := j*SIZE_SCALE;
        u := i/Width*8;
        v := j/Height*8;
      end;
    end;
  MapFile.Close;
  Texture := TTexture.Create(TexName, GL_REPEAT, GL_LINEAR, True);

  SetLength(Faces, (Width-1)*(Height-1)*2);
  SetLength(FaceNormals, (Width-1)*(Height-1)*2);
  for i := 0 to Width-2 do
    for j := 0 to Height-2 do
    begin
      with Faces[(i*(Width-1)+j)*2] do
      begin
        v0 := i*Width+j;
        v1 := i*Width+j+1;
        v2 := (i+1)*Width+j+1;
        u0.x := Vertices[v0].x;
        u0.y := Vertices[v0].y;
        u0.z := Vertices[v0].z;
        u1.x := Vertices[v1].x;
        u1.y := Vertices[v1].y;
        u1.z := Vertices[v1].z;
        u2.x := Vertices[v2].x;
        u2.y := Vertices[v2].y;
        u2.z := Vertices[v2].z;
        u3 := Sub(u1, u0);
        u4 := Sub(u2, u0);
        u0 := Cross(u3, u4);
        Normalize(u0);
        FaceNormals[(i*(Width-1)+j)*2] := u0;
      end;
      with Faces[(i*(Width-1)+j)*2+1] do
      begin
        v0 := i*Width+j;
        v1 := (i+1)*Width+j+1;
        v2 := (i+1)*Width+j;
        u0.x := Vertices[v0].x;
        u0.y := Vertices[v0].y;
        u0.z := Vertices[v0].z;
        u1.x := Vertices[v1].x;
        u1.y := Vertices[v1].y;
        u1.z := Vertices[v1].z;
        u2.x := Vertices[v2].x;
        u2.y := Vertices[v2].y;
        u2.z := Vertices[v2].z;
        u3 := Sub(u1, u0);
        u4 := Sub(u2, u0);
        u0 := Cross(u3, u4);
        Normalize(u0);
        FaceNormals[(i*(Width-1)+j)*2+1] := u0;
      end;
    end;
{                                    // calcul des normales de chaque face
  for i := 0 to Length(Vertices)-1 do
  begin
    u1 := Vector(0, 0, 0);
    k := 0;
    for j := 0 to Length(Faces)-1 do
      with Faces[j] do
        if (v0 = i) or (v1 = i) or (v2 = i) then
        begin
          u1 := Add(u1, FaceNormals[j]);
          Inc(k);
        end;
    u1.x := u1.x/k;
    u1.y := u1.y/k;
    u1.z := u1.z/k;
    Normalize(u1);
    Vertices[i].nx := u1.x;
    Vertices[i].ny := u1.y;
    Vertices[i].nz := u1.z;
  end;
}
  FaceNormals := nil;
end;

procedure TLandscape.Render;
const
  DeepView = 1000;
var
  i, j, wi: Integer;
  k: Cardinal;
  Loc1, Loc2, Loc3: TVector;
  Up, Down, Left, Right: Integer;
begin
//  glColor3f(1,1,1);

  Texture.Use(GL_REPLACE);
  glInterleavedArrays(GL_T2F_N3F_V3F, 0, @Vertices[0]);
 // glDrawElements(GL_TRIANGLES, Length(Faces)*3, GL_UNSIGNED_SHORT, @Faces[0]);
{
  glBegin(GL_TRIANGLES);
  for i := 0 to Length(Faces)-1 do
  begin
    with Vertices[Faces[i].v0] do
    begin
      glTexCoord2f(u, v);
      glNormal3f(nx, ny, nz);
      glVertex3f(x, y, z);
    end;
    with Vertices[Faces[i].v1] do
    begin
      glTexCoord2f(u, v);
      glNormal3f(nx, ny, nz);
      glVertex3f(x, y, z);
    end;
    with Vertices[Faces[i].v2] do
    begin
      glTexCoord2f(u, v);
      glNormal3f(nx, ny, nz);
      glVertex3f(x, y, z);
    end;
  end;
  glEnd;}

  with Camera do
  begin
    Loc1.x := Position.x;
    Loc1.z := Position.z;
    Loc2.x := Loc1.x - sin((Orientation.y-45)*0.0174532925)*DeepView;
    Loc2.z := Loc1.z - cos((Orientation.y-45)*0.0174532925)*DeepView;
    Loc3.x := Loc1.x - sin((Orientation.y+45)*0.0174532925)*DeepView;
    Loc3.z := Loc1.z - cos((Orientation.y+45)*0.0174532925)*DeepView;
  end;
  wi := Length(Map[0]);
  Up := Round(Min(Min(Loc1.z, Loc2.z), Loc3.z)/SIZE_SCALE)-1;
  if Up < 0 then Up := 0;
  Right := Round(Max(Max(Loc1.x, Loc2.x), Loc3.x)/SIZE_SCALE)+1;
  if Right > wi-1 then Right := wi-1;
  Down := Round(Max(Max(Loc1.z, Loc2.z), Loc3.z)/SIZE_SCALE)+1;
  if Down > wi-1 then Down := wi-1;
  Left := Round(Min(Min(Loc1.x, Loc2.x), Loc3.x)/SIZE_SCALE)-1;
  if Left < 0 then Left := 0;

  k := 0;
  for j := Up to Down-1 do
  begin
    if (j mod 2) = 0 then
      for i := Left to Right do
      begin
        Strip[k] := j*wi+i;
        Inc(k);
        Strip[k] := (j+1)*wi+i;
        Inc(k);
      end
    else
      for i := Right downto Left do
      begin
        Strip[k] := (j+1)*wi+i;
        Inc(k);
        Strip[k] := j*wi+i;
        Inc(k);
      end;
  end;
  glDrawElements(GL_TRIANGLE_STRIP, k, GL_UNSIGNED_SHORT, @Strip);
  if k>2 then
    TNT.Tris := TNT.Tris + k-2;
end;

function TLandscape.GetHeight(X, Z: Single): Single;
var
  mx, my: Integer;
begin
  mx := Round(X/SIZE_SCALE);
  my := Round(Z/SIZE_SCALE);
  if (mx>=0) and (my>=0) and (mx<Length(Map)) and (my<Length(Map[0])) then
    Result := map[mx,my]/HEIGHT_SCALE
  else
    Result := 255;
end;

function TLandscape.Collision(X, Y, Z: Single): Boolean;
begin
  Result := GetHeight(X, Z) > Y;
end;

destructor TLandscape.Destroy;
begin
  Texture.Free;
  Faces := nil;
  Vertices := nil;
  SetLength(Map, 0, 0);
  inherited Destroy;
end;

end.

