unit TNT_Vector;

// Gestion du calcul vectoriel
// ---------------------------
//  TVector: vecteur (x,y,z)
//  Add, Sub, Dot, Cross, Norm, Normalize

interface

uses
  OpenGL12;

type
  TVector = record
    x, y, z: GLfloat;
  end;

function Vector(px, py, pz: GLfloat): TVector;
function Add(u, v: TVector): TVector;
function Sub(u, v: TVector): TVector;
function Dot(u, v: TVector): GLfloat;
function Cross(u, v: TVector): TVector;
function Norm(u: TVector): GLfloat;
procedure Normalize(var u: TVector);

implementation

function Vector(px, py, pz: GLfloat): TVector;
begin
  Result.x := px;
  Result.y := py;
  Result.z := pz;
end;

function Add(u, v: TVector): TVector;
begin
  Result.x := u.x + v.x;
  Result.y := u.y + v.y;
  Result.z := u.z + v.z;
end;

function Sub(u, v: TVector): TVector;
begin
  Result.x := u.x - v.x;
  Result.y := u.y - v.y;
  Result.z := u.z - v.z;
end;

function Dot(u, v: TVector): GLfloat;
begin
  Result := u.x*v.x + u.y*v.y + u.z*v.z;
end;

function Cross(u, v: TVector): TVector;
begin
  Result.x := u.y*v.z - u.z*v.y;
  Result.y := u.z*v.x - u.x*v.z;
  Result.z := u.x*v.y - u.y*v.x;
end;

function Norm(u: TVector): GLfloat;
begin
  Result := sqrt(u.x*u.x + u.y*u.y + u.z*u.z);
end;

procedure Normalize(var u: TVector);
var
  t: GLfloat;
begin
  t := Norm(u);
  u.x := u.x/t;
  u.y := u.y/t;
  u.z := u.z/t;
end;

end.

