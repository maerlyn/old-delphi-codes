unit TNT_Frustum;

// Frustum culling
// ---------------

interface

procedure ExtractFrustum;
function PointInFrustum(X, Y, Z: Single): Boolean;
function SphereInFrustum(X, Y, Z, Radius: Single): Single;

implementation

uses
  OpenGL12, TNT_3D;

var
  Frustum: array [0..5, 0..3] of Single;

procedure ExtractFrustum;
var
  proj: array [0..15] of Single;
  modl: array [0..15] of Single;
  clip: array [0..15] of Single;
  t: Single;
  i: Integer;
begin
  glGetFloatv(GL_PROJECTION_MATRIX, @proj[0]);
  for i:=0 to 15 do
    modl[i] := Camera.ModelView[i];

  clip[ 0] := modl[ 0] * proj[ 0] + modl[ 1] * proj[ 4] + modl[ 2] * proj[ 8] + modl[ 3] * proj[12];
  clip[ 1] := modl[ 0] * proj[ 1] + modl[ 1] * proj[ 5] + modl[ 2] * proj[ 9] + modl[ 3] * proj[13];
  clip[ 2] := modl[ 0] * proj[ 2] + modl[ 1] * proj[ 6] + modl[ 2] * proj[10] + modl[ 3] * proj[14];
  clip[ 3] := modl[ 0] * proj[ 3] + modl[ 1] * proj[ 7] + modl[ 2] * proj[11] + modl[ 3] * proj[15];

  clip[ 4] := modl[ 4] * proj[ 0] + modl[ 5] * proj[ 4] + modl[ 6] * proj[ 8] + modl[ 7] * proj[12];
  clip[ 5] := modl[ 4] * proj[ 1] + modl[ 5] * proj[ 5] + modl[ 6] * proj[ 9] + modl[ 7] * proj[13];
  clip[ 6] := modl[ 4] * proj[ 2] + modl[ 5] * proj[ 6] + modl[ 6] * proj[10] + modl[ 7] * proj[14];
  clip[ 7] := modl[ 4] * proj[ 3] + modl[ 5] * proj[ 7] + modl[ 6] * proj[11] + modl[ 7] * proj[15];

  clip[ 8] := modl[ 8] * proj[ 0] + modl[ 9] * proj[ 4] + modl[10] * proj[ 8] + modl[11] * proj[12];
  clip[ 9] := modl[ 8] * proj[ 1] + modl[ 9] * proj[ 5] + modl[10] * proj[ 9] + modl[11] * proj[13];
  clip[10] := modl[ 8] * proj[ 2] + modl[ 9] * proj[ 6] + modl[10] * proj[10] + modl[11] * proj[14];
  clip[11] := modl[ 8] * proj[ 3] + modl[ 9] * proj[ 7] + modl[10] * proj[11] + modl[11] * proj[15];

  clip[12] := modl[12] * proj[ 0] + modl[13] * proj[ 4] + modl[14] * proj[ 8] + modl[15] * proj[12];
  clip[13] := modl[12] * proj[ 1] + modl[13] * proj[ 5] + modl[14] * proj[ 9] + modl[15] * proj[13];
  clip[14] := modl[12] * proj[ 2] + modl[13] * proj[ 6] + modl[14] * proj[10] + modl[15] * proj[14];
  clip[15] := modl[12] * proj[ 3] + modl[13] * proj[ 7] + modl[14] * proj[11] + modl[15] * proj[15];

// RIGHT plane
  Frustum[0,0] := clip[ 3] - clip[ 0];
  Frustum[0,1] := clip[ 7] - clip[ 4];
  Frustum[0,2] := clip[11] - clip[ 8];
  Frustum[0,3] := clip[15] - clip[12];
  t := sqrt(Frustum[0,0] * Frustum[0,0] + Frustum[0,1] * Frustum[0,1] + Frustum[0,2] * Frustum[0,2]);
  Frustum[0,0] := Frustum[0,0] / t;
  Frustum[0,1] := Frustum[0,1] / t;
  Frustum[0,2] := Frustum[0,2] / t;
  Frustum[0,3] := Frustum[0,3] / t;

// LEFT plane
   frustum[1,0] := clip[ 3] + clip[ 0];
   frustum[1,1] := clip[ 7] + clip[ 4];
   frustum[1,2] := clip[11] + clip[ 8];
   frustum[1,3] := clip[15] + clip[12];
   t := sqrt(frustum[1,0] * frustum[1,0] + frustum[1,1] * frustum[1,1] + frustum[1,2] * frustum[1,2]);
   frustum[1,0] := frustum[1,0] / t;
   frustum[1,1] := frustum[1,1] / t;
   frustum[1,2] := frustum[1,2] / t;
   frustum[1,3] := frustum[1,3] / t;

// BOTTOM plane
   frustum[2,0] := clip[ 3] + clip[ 1];
   frustum[2,1] := clip[ 7] + clip[ 5];
   frustum[2,2] := clip[11] + clip[ 9];
   frustum[2,3] := clip[15] + clip[13];
   t := sqrt(frustum[2,0] * frustum[2,0] + frustum[2,1] * frustum[2,1] + frustum[2,2] * frustum[2,2]);
   frustum[2,0] := frustum[2,0] / t;
   frustum[2,1] := frustum[2,1] / t;
   frustum[2,2] := frustum[2,2] / t;
   frustum[2,3] := frustum[2,3] / t;

// TOP plane
   frustum[3,0] := clip[ 3] - clip[ 1];
   frustum[3,1] := clip[ 7] - clip[ 5];
   frustum[3,2] := clip[11] - clip[ 9];
   frustum[3,3] := clip[15] - clip[13];
   t := sqrt(frustum[3,0] * frustum[3,0] + frustum[3,1] * frustum[3,1] + frustum[3,2] * frustum[3,2]);
   frustum[3,0] := frustum[3,0] / t;
   frustum[3,1] := frustum[3,1] / t;
   frustum[3,2] := frustum[3,2] / t;
   frustum[3,3] := frustum[3,3] / t;

// FAR plane
   frustum[4,0] := clip[ 3] - clip[ 2];
   frustum[4,1] := clip[ 7] - clip[ 6];
   frustum[4,2] := clip[11] - clip[10];
   frustum[4,3] := clip[15] - clip[14];
   t := sqrt(frustum[4,0] * frustum[4,0] + frustum[4,1] * frustum[4,1] + frustum[4,2] * frustum[4,2]);
   frustum[4,0] := frustum[4,0] / t;
   frustum[4,1] := frustum[4,1] / t;
   frustum[4,2] := frustum[4,2] / t;
   frustum[4,3] := frustum[4,3] / t;

// NEAR plane
  Frustum[5,0] := clip[ 3] + clip[ 2];
  Frustum[5,1] := clip[ 7] + clip[ 6];
  Frustum[5,2] := clip[11] + clip[10];
  Frustum[5,3] := clip[15] + clip[14];
  t := sqrt(Frustum[5,0] * Frustum[5,0] + Frustum[5,1] * Frustum[5,1] + Frustum[5,2] * Frustum[5,2]);
  Frustum[5,0] := Frustum[5,0] / t;
  Frustum[5,1] := Frustum[5,1] / t;
  Frustum[5,2] := Frustum[5,2] / t;
  Frustum[5,3] := Frustum[5,3] / t;
end;

function PointInFrustum(X, Y, Z: Single): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to 5 do
  begin
    if Frustum[i,0]*X + Frustum[i,1]*Y + Frustum[i,2]*Z + Frustum[i,3] <= 0 then
      Exit;
  end;
  Result := True;
end;

function SphereInFrustum(X, Y, Z, Radius: Single): Single;
var
  i: Integer;
  d: Single;
begin
  Result := 0;
  for i := 0 to 5 do
  begin
    d := Frustum[i,0]*X + Frustum[i,1]*Y + Frustum[i,2]*Z + Frustum[i,3];
    if d <= -Radius then
      Exit;
  end;
  Result := d + Radius;
end;

end.

