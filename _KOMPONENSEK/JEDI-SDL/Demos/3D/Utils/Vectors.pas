unit Vectors;

interface

uses
  Math;

type
  P3DVector = ^T3DVector;
  T3DVector = record
    X, Y, Z : Single;
  end;

  // Arithmetic Functions
function Vector( x, y, z : Single ) : T3DVector;
function VectorAdd( var v1, v2 : T3DVector ) : T3DVector;
function VectorDivide( var v1, v2 : T3DVector ) : T3DVector;
function VectorMultiply( var v1, v2 : T3DVector ) : T3DVector;
function VectorSubtract( var v1, v2 : T3DVector ) : T3DVector;


// Transformation functions
procedure VectorRotateAroundX( ang : Single; var dest : T3DVector );
procedure VectorRotateAroundY( ang : Single; var dest : T3DVector );
procedure VectorRotateAroundZ( ang : Single; var dest : T3DVector );

function VectorNormalize( v : T3DVector ): T3DVector;
function VectorLength( v : T3DVector ) : Single;
function VectorScale( v : T3DVector; val : Single ) : T3DVector;

function VectorCrossProduct( v1, v2 : T3DVector ) : T3DVector;
function VectorDotProduct( v1, v2 : T3DVector ) : single;
function VectorInterpolate( V1, V2 : T3DVector; Amt : Single ) : T3DVector;

function RandomRange( lo, hi : Single ) : Single;

implementation
{ -- Vectors Math ------------------------------------------------------------- }

function Vector( x, y, z : Single ) : T3DVector;
begin
  result.X := x;
  result.Y := y;
  result.Z := z;
end;

function VectorAdd( var v1, v2 : T3DVector ) : T3DVector;
begin
  result.X := v1.X + v2.X;
  result.Y := v1.Y + v2.Y;
  result.Z := v1.Z + v2.Z;
end;

function VectorSubtract( var v1, v2 : T3DVector ) : T3DVector;
begin
  result.X := v1.X - v2.X;
  result.Y := v1.Y - v2.Y;
  result.Z := v1.Z - v2.Z;
end;

function VectorMultiply( var v1, v2 : T3DVector ) : T3DVector;
begin
  result.X := v1.X * v2.X;
  result.Y := v1.Y * v2.Y;
  result.Z := v1.Z * v2.Z;
end;

function VectorDivide( var v1, v2 : T3DVector ) : T3DVector;
begin
  result.X := v1.X / v2.X;
  result.Y := v1.Y / v2.Y;
  result.Z := v1.Z / v2.Z;
end;

procedure VectorRotateAroundX( ang : Single; var dest : T3DVector );
var
  y0, z0 : Single;
  radAng : Single;
begin
  y0 := dest.Y;
  z0 := dest.Z;
  radAng := DegToRad( ang );

  dest.Y := ( y0 * cos( radAng ) ) - ( z0 * sin( radAng ) );
  dest.Z := ( y0 * sin( radAng ) ) + ( z0 * cos( radAng ) );
end;

procedure VectorRotateAroundY( ang : Single; var dest : T3DVector );
var
  x0, z0 : Single;
  radAng : Single;
begin
  x0 := dest.X;
  z0 := dest.Z;
  radAng := DegToRad( ang );

  dest.X := ( x0 * cos( radAng ) ) + ( z0 * sin( radAng ) );
  dest.Z := ( z0 * cos( radAng ) ) - ( x0 * sin( radAng ) );
end;

procedure VectorRotateAroundZ( ang : Single; var dest : T3DVector );
var
  x0, y0 : Single;
  radAng : Single;
begin
  x0 := dest.X;
  y0 := dest.Y;
  radAng := DegToRad( ang );

  dest.X := ( x0 * cos( radAng ) ) - ( y0 * sin( radAng ) );
  dest.Y := ( y0 * cos( radAng ) ) + ( x0 * sin( radAng ) );
end;

function VectorNormalize( v : T3DVector ) : T3DVector;
var
  Scale, Len : Single;
begin
  Len := VectorLength( v );
  if Len = 0.0 then
    Exit;

  scale := 1.0 / Len;

  Result.X := v.X * Scale;
  Result.Y := v.Y * Scale;
  Result.Z := v.Z * Scale;
end;

function VectorLength( v : T3DVector ) : Single;
var
  size : real;
begin
  size := sqr( v.X ) + sqr( v.Y ) + sqr( v.Z );
  result := sqrt( size );
end;

function VectorScale( v : T3DVector; val : Single ) : T3DVector;
begin
  Result.X := v.X * val;
  Result.Y := v.Y * val;
  Result.Z := v.Z * val;
end;

function VectorCrossProduct( v1, v2 : T3DVector ) : T3DVector;
begin
  Result.x := v1.y * v2.z - v2.y * v1.z;
  Result.y := v1.z * v2.x - v2.z * v1.x;
  Result.z := v1.x * v2.y - v2.x * v1.y;
end;

function VectorDotProduct( v1, v2 : T3DVector ) : single;
begin
  result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function VectorInterpolate( V1, V2 : T3DVector; Amt : Single ) : T3DVector;
begin
  Result.X := V1.X + ( V2.X - V1.X ) * Amt;
  Result.Y := V1.Y + ( V2.Y - V1.Y ) * Amt;
  Result.Z := V1.Z + ( V2.Z - V1.Z ) * Amt;
end;

function RandomRange( lo, hi : Single ) : Single;
begin
  result := ( random * ( hi - lo ) + lo );
end;

end.

