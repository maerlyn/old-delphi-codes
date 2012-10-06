unit TNT_Collisions;

// Gestion des collisions
// ----------------------

interface

uses
  TNT_3D, TNT_Object;

function ObjectCollision(Obj1, Obj2: TObj): Boolean;

implementation

uses
  TNT_Vector;

function ObjectCollision(Obj1, Obj2: TObj): Boolean;
var
  r1, r2: Single;
begin
  Result := False;
  if Obj1 = Obj2 then
    Exit;
  r1 := Scene.Models[Obj1.ModelID].BColSphereRadius;
  r2 := Scene.Models[Obj2.ModelID].BColSphereRadius;
  Result := Norm(Sub(Obj1.Position, Obj2.Position)) < r1+r2;
end;

end.

