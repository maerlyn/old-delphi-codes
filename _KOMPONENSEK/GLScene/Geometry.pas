{: Geometry<p>

	Base classes and structures for GLScene.<p>

   Most common functions/procedures come in various flavours (using overloads),
   the naming convention is :<ul>
   <li>TypeOperation: functions returning a result, or accepting a "var" as last
      parameter to place result (VectorAdd, VectorCrossProduct...)
   <li>OperationType : procedures taking as first parameter a "var" that will be
      used as operand and result (AddVector, CombineVector...)
   </ul><p>
   As a general rule, procedures implementations (asm or not) are the fastest
   (up to 800% faster than function equivalents), due to reduced return value
   duplication overhead (the exception being the matrix operations).<p>

   For better performance, it is recommended not to use the "Math" unit that
   comes with Delphi, and only use functions/procedures from this unit
   (the single-based functions have been optimized and are up to 100% faster,
   than extended-based ones from "Math").<p>

   3DNow! SIMD instructions are automatically detected and used in *some* of the
   functions/procedures, typical gains (over FPU implementation) are approx a
   100% speed increase on K6-2/3, and 20-60% on K7, and sometimes more
   (f.i. 650% on 4x4 matrix multiplication for the K6).<p>

	<b>Historique : </b><font size=-1><ul>
      <li>06/03/01 - EG - Fix in PointInPolygon by Pavel Vassiliev
      <li>04/03/01 - EG - Added NormalizeVectorArray
      <li>03/03/01 - EG - Added MakeReflectionMatrix 
      <li>02/03/01 - EG - New PointInPolygon code by Pavel Vassiliev
      <li>25/02/01 - EG - Fixed 'VectorSubstract', added VectorArrayLerp and a few minors
      <li>22/02/01 - EG - Added MinXYZ/MaxXYZ variants and Plane-Line intersection
      <li>21/02/01 - EG - Added Sign, MinFloat & MaxFloat
      <li>15/02/01 - EG - Faster Vector Transforms (3DNow! optimizations)
      <li>14/02/01 - EG - Faster Matrix multiplications (3DNow! & FPU optimizations),
                          Added support for FPU-only sections
      <li>05/02/01 - EG - Faster VectorEquals
      <li>21/01/01 - EG - Fixed MakePoint/Vector affine variants (thx Jacques Tur)
      <li>17/01/00 - EG - VectoAdd return type fix (thx Jacques Tur),
                          also added a few new overloads
      <li>05/11/00 - EG - Added RayCastPlaneIntersect
      <li>08/10/00 - EG - Added SetMatrix
      <li>13/08/00 - EG - Added Plane geometry support
      <li>06/08/00 - EG - Various minor additions
      <li>16/07/00 - EG - Added some new mixed vector/scalar funcs and new overloads
      <li>12/07/00 - EG - New overloads and replacements for Power, Trunc, Frac & Round
      <li>25/06/00 - EG - End of major update
      <li>13/06/00 - EG - Start of major update
      <li>09/06/00 - EG - Some additions and fixes in preparation for major changes
      <li>05/06/00 - EG - Added VectorLength overloads
      <li>26/05/00 - EG - [0..0] arrays changed to [0..cMaxArray]
      <li>23/05/00 - EG - Added intersection functions,
                          Replaced some xxxAffinexxx funcs with overloads
      <li>22/03/00 - EG - Added MakeShadowMatrix (adapted from "OpenGL SuperBible" book)
      <li>21/03/00 - EG - Removed PWordArray (was a SysUtils's duplicate)
      <li>06/02/00 - EG - Added VectorEquals
      <li>05/02/00 - EG - Added some "const", more still needed,
                          Added overloads for some of the MakeXXXVector funcs,
                          Added homogeneous vector consts, VectorSpacing
   </ul>
}
unit Geometry;

// This unit contains many needed types, functions and procedures for
// quaternion, vector and matrix arithmetics. It is specifically designed
// for geometric calculations within R3 (affine vector space)
// and R4 (homogeneous vector space).
//
// Note: The terms 'affine' or 'affine coordinates' are not really correct here
//       because an 'affine transformation' describes generally a transformation which leads
//       to a uniquely solvable system of equations and has nothing to do with the dimensionality
//       of a vector. One could use 'projective coordinates' but this is also not really correct
//       and since I haven't found a better name (or even any correct one), 'affine' is as good
//       as any other one.
//
// Identifiers containing no dimensionality (like affine or homogeneous)
// and no datatype (integer..extended) are supposed as R4 representation
// with 'single' floating point type (examples are TVector, TMatrix,
// and TQuaternion). The default data type is 'single' ('GLFloat' for OpenGL)
// and used in all routines (except conversions and trigonometric functions).
//
// Routines with an open array as argument can either take Func([1,2,3,4,..]) or Func(Vect).
// The latter is prefered, since no extra stack operations is required.
// Note: Be careful while passing open array elements! If you pass more elements
//       than there's room in the result the behaviour will be unpredictable.
//
// If not otherwise stated, all angles are given in radians
// (instead of degrees). Use RadToDeg or DegToRad to convert between them.
//
// Geometry.pas was assembled from different sources (like GraphicGems)
// and relevant books or based on self written code, respectivly.
//
// Note: Some aspects need to be considered when using Delphi and pure
//       assembler code. Delphi esnures that the direction flag is always
//       cleared while entering a function and expects it cleared on return.
//       This is in particular important in routines with (CPU) string commands (MOVSD etc.)
//       The registers EDI, ESI and EBX (as well as the stack management
//       registers EBP and ESP) must not be changed! EAX, ECX and EDX are
//       freely available and mostly used for parameter.
//
// Version 2.5
// last change : 04. January 2000
//
// (c) Copyright 1999, Dipl. Ing. Mike Lischke (public@lischke-online.de)

interface

const
   cMaxArray = (MaxInt shr 4);

type
   // data types needed for 3D graphics calculation,
   // included are 'C like' aliases for each type (to be
   // conformal with OpenGL types)

   PByte = ^Byte;
   PWord = ^Word;
   PInteger = ^Integer;
   PFloat = ^Single;
   PDouble = ^Double;
   PExtended = ^Extended;
   PPointer = ^Pointer;

  	PTexPoint    = ^TTexPoint;
	TTexPoint    = packed record
						  S,T : Single;
						end;

   // types to specify continous streams of a specific type
   // switch off range checking to access values beyond the limits
   PByteVector = ^TByteVector;
   PByteArray = PByteVector;
   TByteVector = array[0..cMaxArray] of Byte;

   PWordVector = ^TWordVector;
   TWordVector = array[0..cMaxArray] of Word;

   PIntegerVector = ^TIntegerVector;
   PIntegerArray = PIntegerVector;
   TIntegerVector = array[0..cMaxArray] of Integer;

   PFloatVector = ^TFloatVector;
   PFloatArray = PFloatVector;
   PSingleArray = PFloatArray;
   TFloatVector = array[0..cMaxArray] of Single;

   PDoubleVector = ^TDoubleVector;
   PDoubleArray = PDoubleVector;
   TDoubleVector = array[0..cMaxArray] of Double;

   PExtendedVector = ^TExtendedVector;
   PExtendedArray = PExtendedVector;
   TExtendedVector = array[0..cMaxArray] of Extended;

   PPointerVector = ^TPointerVector;
   PPointerArray = PPointerVector;
   TPointerVector = array[0..cMaxArray] of Pointer;

   PCardinalVector = ^TCardinalVector;
   PCardinalArray = PCardinalVector;
   TCardinalVector = array[0..cMaxArray] of Cardinal;

   // common vector and matrix types with predefined limits
   // indices correspond like: x -> 0
   //                          y -> 1
   //                          z -> 2
   //                          w -> 3

   PHomogeneousByteVector = ^THomogeneousByteVector;
   THomogeneousByteVector = array[0..3] of Byte;
   TVector4b = THomogeneousByteVector;

   PHomogeneousWordVector = ^THomogeneousWordVector;
   THomogeneousWordVector = array[0..3] of Word;
   TVector4w = THomogeneousWordVector;

   PHomogeneousIntVector = ^THomogeneousIntVector;
   THomogeneousIntVector = array[0..3] of Integer;
   TVector4i = THomogeneousIntVector;

   PHomogeneousFltVector = ^THomogeneousFltVector;
   THomogeneousFltVector = array[0..3] of Single;
   TVector4f = THomogeneousFltVector;

   PHomogeneousDblVector = ^THomogeneousDblVector;
   THomogeneousDblVector = array[0..3] of Double;
   TVector4d = THomogeneousDblVector;

   PHomogeneousExtVector = ^THomogeneousExtVector;
   THomogeneousExtVector = array[0..3] of Extended;
   TVector4e = THomogeneousExtVector;

   PHomogeneousPtrVector = ^THomogeneousPtrVector;
   THomogeneousPtrVector = array[0..3] of Pointer;
   TVector4p = THomogeneousPtrVector;

   PAffineByteVector = ^TAffineByteVector;
   TAffineByteVector = array[0..2] of Byte;
   TVector3b = TAffineByteVector;

   PAffineWordVector = ^TAffineWordVector;
   TAffineWordVector = array[0..2] of Word;
   TVector3w = TAffineWordVector;

   PAffineIntVector = ^TAffineIntVector;
   TAffineIntVector = array[0..2] of Integer;
   TVector3i = TAffineIntVector;

   PAffineFltVector = ^TAffineFltVector;
   TAffineFltVector = array[0..2] of Single;
   TVector3f = TAffineFltVector;

   PAffineDblVector = ^TAffineDblVector;
   TAffineDblVector = array[0..2] of Double;
   TVector3d = TAffineDblVector;

   PAffineExtVector = ^TAffineExtVector;
   TAffineExtVector = array[0..2] of Extended;
   TVector3e = TAffineExtVector;

   PAffinePtrVector = ^TAffinePtrVector;
   TAffinePtrVector = array[0..2] of Pointer;
   TVector3p = TAffinePtrVector;

   // some simplified names
   PVector = ^TVector;
   TVector = THomogeneousFltVector;

   PHomogeneousVector = ^THomogeneousVector;
   THomogeneousVector = THomogeneousFltVector;

   PAffineVector = ^TAffineVector;
   TAffineVector = TAffineFltVector;

   PVertex    = ^TVertex;
	TVertex    = TAffineVector;

   // arrays of vectors
   PAffineVectorArray = ^TAffineVectorArray;
   TAffineVectorArray = array[0..MAXINT shr 4] of TAffineVector;

   PVectorArray = ^TVectorArray;
   TVectorArray = array[0..MAXINT shr 5] of TVector;

	PTexPointArray = ^TTexPointArray;
	TTexPointArray = array [0..MaxInt shr 4] of TTexPoint;

   // matrices
   THomogeneousByteMatrix = array[0..3] of THomogeneousByteVector;
   TMatrix4b = THomogeneousByteMatrix;

   THomogeneousWordMatrix = array[0..3] of THomogeneousWordVector;
   TMatrix4w = THomogeneousWordMatrix;

   THomogeneousIntMatrix = array[0..3] of THomogeneousIntVector;
   TMatrix4i = THomogeneousIntMatrix;

   THomogeneousFltMatrix  = array[0..3] of THomogeneousFltVector;
   TMatrix4f = THomogeneousFltMatrix;

   THomogeneousDblMatrix = array[0..3] of THomogeneousDblVector;
   TMatrix4d = THomogeneousDblMatrix;

   THomogeneousExtMatrix = array[0..3] of THomogeneousExtVector;
   TMatrix4e = THomogeneousExtMatrix;

   TAffineByteMatrix = array[0..2] of TAffineByteVector;
   TMatrix3b = TAffineByteMatrix;

   TAffineWordMatrix = array[0..2] of TAffineWordVector;
   TMatrix3w = TAffineWordMatrix;

   TAffineIntMatrix = array[0..2] of TAffineIntVector;
   TMatrix3i = TAffineIntMatrix;

   TAffineFltMatrix = array[0..2] of TAffineFltVector;
   TMatrix3f = TAffineFltMatrix;

   TAffineDblMatrix = array[0..2] of TAffineDblVector;
   TMatrix3d = TAffineDblMatrix;

   TAffineExtMatrix = array[0..2] of TAffineExtVector;
   TMatrix3e = TAffineExtMatrix;

   // some simplified names
   PMatrix = ^TMatrix;
   TMatrix = THomogeneousFltMatrix;

   PHomogeneousMatrix = ^THomogeneousMatrix;
   THomogeneousMatrix = THomogeneousFltMatrix;

   PAffineMatrix = ^TAffineMatrix;
   TAffineMatrix = TAffineFltMatrix;

   {: A plane equation.<p>
      Defined by its equation A.x+B.y+C.z+D<p>, a plane can be mapped to the
      homogeneous space coordinates, and this is what we are doing here.<br>
      The typename is just here for easing up data manipulation. }
   THmgPlane = TVector;

   // q = ([x, y, z], w)
   TQuaternion = record
     case Integer of
       0:
         (ImagPart: TAffineVector;
          RealPart: Single);
       1:
         (Vector: TVector4f);
   end;

   TRectangle = record
     Left,
     Top,
     Width,
     Height: Integer;
   end;

   TTransType = (ttScaleX, ttScaleY, ttScaleZ,
                 ttShearXY, ttShearXZ, ttShearYZ,
                 ttRotateX, ttRotateY, ttRotateZ,
                 ttTranslateX, ttTranslateY, ttTranslateZ,
                 ttPerspectiveX, ttPerspectiveY, ttPerspectiveZ, ttPerspectiveW);

   // used to describe a sequence of transformations in following order:
   // [Sx][Sy][Sz][ShearXY][ShearXZ][ShearZY][Rx][Ry][Rz][Tx][Ty][Tz][P(x,y,z,w)]
   // constants are declared for easier access (see MatrixDecompose below)
   TTransformations  = array [TTransType] of Single;

   // TRenderContextClippingInfo
   //
   TRenderContextClippingInfo = record
      origin : TVector;
      clippingDirection : TVector;
      viewPortRadius : Single; // viewport bounding radius per distance unit
      farClippingDistance : Single;
   end;

const
  // useful constants

  // TexPoints (2D space)
  XTexPoint    : TTexPoint = (S:1; T:0);
  YTexPoint    : TTexPoint = (S:0; T:1);
  XYTexPoint   : TTexPoint = (S:1; T:1);
  NullTexPoint : TTexPoint = (S:0; T:0);

  // standard vectors
  XVector :    TAffineVector = (1, 0, 0);
  YVector :    TAffineVector = (0, 1, 0);
  ZVector :    TAffineVector = (0, 0, 1);
  XYZVector :  TAffineVector = (1, 1, 1);
  NullVector : TAffineVector = (0, 0, 0);
  // standard homogeneous vectors
  XHmgVector : THomogeneousVector = (1, 0, 0, 0);
  YHmgVector : THomogeneousVector = (0, 1, 0, 0);
  ZHmgVector : THomogeneousVector = (0, 0, 1, 0);
  WHmgVector : THomogeneousVector = (0, 0, 0, 1);
  XYZHmgVector  : THomogeneousVector = (1, 1, 1, 0);
  XYZWHmgVector : THomogeneousVector = (1, 1, 1, 1);
  NullHmgVector : THomogeneousVector = (0, 0, 0, 0);
  // standard homogeneous points
  XHmgPoint :  THomogeneousVector = (1, 0, 0, 1);
  YHmgPoint :  THomogeneousVector = (0, 1, 0, 1);
  ZHmgPoint :  THomogeneousVector = (0, 0, 1, 1);
  WHmgPoint :  THomogeneousVector = (0, 0, 0, 1);
  NullHmgPoint : THomogeneousVector = (0, 0, 0, 1);

  IdentityMatrix: TAffineMatrix = ((1, 0, 0),
                                   (0, 1, 0),
                                   (0, 0, 1));
  IdentityHmgMatrix: TMatrix = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));
  IdentityHmgDblMatrix: THomogeneousDblMatrix = ((1, 0, 0, 0),
                                                 (0, 1, 0, 0),
                                                 (0, 0, 1, 0),
                                                 (0, 0, 0, 1));
  EmptyMatrix: TAffineMatrix = ((0, 0, 0),
                                (0, 0, 0),
                                (0, 0, 0));
  EmptyHmgMatrix: TMatrix = ((0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0),
                             (0, 0, 0, 0));

  // some very small numbers
  EPSILON  : Single = 1e-100;
  EPSILON2 : Single = 1e-50;

//------------------------------------------------------------------------------
// Vector functions
//------------------------------------------------------------------------------

function TexPointMake(const s, t : Single) : TTexPoint;
function AffineVectorMake(const x, y, z : Single) : TAffineVector; overload;
function AffineVectorMake(const v : TVector) : TAffineVector; overload;
procedure SetAffineVector(var v : TAffineVector; const x, y, z : Single); overload;
procedure SetVector(var v : TAffineVector; const x, y, z : Single); overload;
procedure SetVector(var v : TAffineVector; const vSrc : TVector); overload;
procedure SetVector(var v : TAffineVector; const vSrc : TAffineVector); overload;
procedure SetVector(var v : TAffineDblVector; const vSrc : TAffineVector); overload;
procedure SetVector(var v : TAffineDblVector; const vSrc : TVector); overload;
function VectorMake(const v : TAffineVector; w : Single = 0) : TVector; overload;
function VectorMake(const x, y, z: Single; w : Single = 0) : TVector; overload;
function PointMake(const x, y, z: Single) : TVector; overload;
procedure SetVector(var v : TVector; const x, y, z : Single; w : Single = 0); overload;
procedure SetVector(var v : TVector; const av : TAffineVector; w : Single = 0); overload;
procedure SetVector(var v : TVector; const vSrc : TVector); overload;
procedure MakePoint(var v : TVector; const x, y, z: Single); overload;
procedure MakePoint(var v : TVector; const av : TAffineVector); overload;
procedure MakeVector(var v : TAffineVector; const x, y, z: Single); overload;
procedure MakeVector(var v : TVector; const x, y, z: Single); overload;
procedure MakeVector(var v : TVector; const av : TAffineVector); overload;

//: Returns the sum of two affine vectors
function VectorAdd(const V1, V2 : TAffineVector) : TAffineVector; overload;
//: Adds two vectors and places result in vr
procedure VectorAdd(const V1, V2 : TAffineVector; var vr : TAffineVector); overload;
//: Returns the sum of two homogeneous vectors
function VectorAdd(const V1, V2 : TVector) : TVector; overload;
//: Sums up f to each component of the vector
function VectorAdd(const v : TAffineVector; const f : Single) : TAffineVector; overload;
//: Sums up f to each component of the vector
function VectorAdd(const v : TVector; const f : Single) : TVector; overload;
//: Adds V2 to V1, result is placed in V1
procedure AddVector(var V1 : TAffineVector; const V2 : TAffineVector); overload;
//: Adds V2 to V1, result is placed in V1
procedure AddVector(var V1 : TVector; const V2 : TVector); overload;
//: Sums up f to each component of the vector
procedure AddVector(var v : TAffineVector; const f : Single); overload;
//: Sums up f to each component of the vector
procedure AddVector(var v : TVector; const f : Single); overload;

//: Returns V1-V2
function VectorSubtract(const V1, V2 : TAffineVector) : TAffineVector; overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TAffineVector); overload;
//: Returns V1-V2
function VectorSubtract(const V1, V2 : TVector) : TVector; overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TVector; var result : TVector); overload;
//: Subtracts V2 from V1 and return value in result
procedure VectorSubtract(const v1, v2 : TVector; var result : TAffineVector); overload;
//: Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1 : TAffineVector; const V2 : TAffineVector); overload;
//: Subtracts V2 from V1, result is placed in V1
procedure SubtractVector(var V1 : TVector; const V2 : TVector); overload;

//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; var f : Single); overload;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TAffineVector; const F1, F2: Single): TAffineVector; overload;
//: Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single): TAffineVector; overload;
//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TVector; const v : TVector; var f : Single); overload;
//: Combine the first vector with the second : vr:=vr+v*f
procedure CombineVector(var vr : TVector; const v : TAffineVector; var f : Single); overload;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1, V2: TVector; const F1, F2: Single): TVector; overload;
//: Makes a linear combination of two vectors and return the result
function VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single): TVector; overload;
//: Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single; var vr : TVector); overload;
//: Makes a linear combination of two vectors and place result in vr
procedure VectorCombine(const V1, V2: TVector; const F1, F2: Single; var vr : TVector); overload;
//: Makes a linear combination of three vectors and return the result
function VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single): TVector; overload;
//: Makes a linear combination of three vectors and return the result
procedure VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single; var vr : TVector); overload;

{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2 : TAffineVector) : Single; overload;
{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1, V2 : TVector) : Single; overload;
{: Calculates the dot product between V1 and V2.<p>
   Result:=V1[X] * V2[X] + V1[Y] * V2[Y] + V1[Z] * V2[Z] }
function VectorDotProduct(const V1 : TVector; const V2 : TAffineVector) : Single; overload;

//: Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2 : TAffineVector): TAffineVector; overload;
//: Calculates the cross product between vector 1 and 2
function VectorCrossProduct(const V1, V2 : TVector): TVector; overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TAffineVector); overload;
//: Calculates the cross product between vector 1 and 2, place result in vr
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;

//: Calculates linear interpolation between start and stop at point t
function Lerp(const start, stop, t : Single) : Single;
//: Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const v1, v2 : TAffineVector; t : Single) : TAffineVector; overload;
//: Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const v1, v2 : TAffineVector; t : Single; var vr : TAffineVector); overload;
//: Calculates linear interpolation between vector1 and vector2 at point t
function VectorLerp(const v1, v2 : TVector; t : Single) : TVector; overload;
//: Calculates linear interpolation between vector1 and vector2 at point t, places result in vr
procedure VectorLerp(const v1, v2 : TVector; t : Single; var vr : TVector); overload;

//: Calculates linear interpolation between vector arrays
procedure VectorArrayLerp(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray); overload;
procedure VectorArrayLerp(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray); overload;

{: Calculates the length of a vector following the equation sqrt(x*x+y*y). }
function VectorLength(const x, y : Single) : Single; overload;
{: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z). }
function VectorLength(const x, y, z : Single) : Single; overload;
//: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z).
function VectorLength(const v : TAffineVector) : Single; overload;
//: Calculates the length of a vector following the equation sqrt(x*x+y*y+z*z+w*w).
function VectorLength(const v : TVector) : Single; overload;
{: Calculates the length of a vector following the equation: sqrt(x*x+y*y+...).<p>
   Note: The parameter of this function is declared as open array. Thus
   there's no restriction about the number of the components of the vector. }
function VectorLength(v : array of Single) : Single; overload;

{: Calculates norm of a vector which is defined as norm = x * x + y * y<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const x, y : Single) : Single; overload;
{: Calculates norm of a vector which is defined as norm = x * x + y * y + ...<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v : TAffineVector) : Single; overload;
{: Calculates norm of a vector which is defined as norm = x * x + y * y + ...<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(const v : TVector) : Single; overload;
{: Calculates norm of a vector which is defined as norm = x * x + y * y + ...<p>
   Also known as "Norm 2" in the math world, this is sqr(VectorLength). }
function VectorNorm(var V: array of Single) : Single; overload;

//: Transforms a vector to unit length
procedure NormalizeVector(var v : TAffineVector); overload;
//: Transforms a vector to unit length
procedure NormalizeVector(var v : TVector); overload;
//: Returns the vector transformed to unit length
function VectorNormalize(const v : TAffineVector) : TAffineVector; overload;

//: Transforms vectors to unit length
procedure NormalizeVectorArray(list : PAffineVectorArray; n : Integer); overload;

{: Calculates the cosine of the angle between Vector1 and Vector2.<p>
   Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
function VectorAngle(const V1, V2: TAffineVector) : Single;

//: Negates the vector
procedure NegateVector(var V : TAffineVector); overload;
//: Negates the vector
procedure NegateVector(var V : TVector); overload;
//: Negates the vector
procedure NegateVector(V : array of Single); overload;

//: Scales given vector by a factor
procedure ScaleVector(var v : TAffineVector; factor : Single); overload;
{: Scales given vector by another vector.<p>
   v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v : TAffineVector; const factor : TAffineVector); overload;
//: Scales given vector by a factor
procedure ScaleVector(var v : TVector; factor : Single); overload;
{: Scales given vector by another vector.<p>
   v[x]:=v[x]*factor[x], v[y]:=v[y]*factor[y] etc. }
procedure ScaleVector(var v : TVector; const factor : TVector); overload;
//: Returns a vector scaled by a factor
function VectorScale(const v : TAffineVector; factor : Single) : TAffineVector; overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TAffineVector; factor : Single; var vr : TAffineVector); overload;
//: Returns a vector scaled by a factor
function VectorScale(const v : TVector; factor : Single) : TVector; overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TVector; factor : Single; var vr : TVector); overload;
//: Scales a vector by a factor and places result in vr
procedure VectorScale(const v : TVector; factor : Single; var vr : TAffineVector); overload;

{: Divides given vector by another vector.<p>
   v[x]:=v[x]/divider[x], v[y]:=v[y]/divider[y] etc. }
procedure DivideVector(var v : TVector; const divider : TVector); overload;

//: True if all components are equal.
function VectorEquals(const V1, V2: TVector) : Boolean; overload;
//: True if all components are equal.
function VectorEquals(const V1, V2: TAffineVector) : Boolean; overload;
//: True if x=y=z=0, w ignored
function VectorIsNull(const v : TVector) : Boolean; overload;
//: True if x=y=z=0, w ignored
function VectorIsNull(const v : TAffineVector) : Boolean; overload;

{: Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const v1, v2 : TAffineVector): Single; overload;
{: Calculates Abs(v1[x]-v2[x])+Abs(v1[y]-v2[y])+..., also know as "Norm1".<p> }
function VectorSpacing(const v1, v2 : TVector): Single; overload;

{: Calculates distance between two vectors.<p>
   ie. sqrt(sqr(v1[x]-v2[x])+...) }
function VectorDistance(const v1, v2 : TAffineVector): Single; overload;
{: Calculates distance between two vectors.<p>
   ie. sqrt(sqr(v1[x]-v2[x])+...) }
function VectorDistance(const v1, v2 : TVector): Single; overload;

{: Calculates the "Norm 2" between two vectors.<p>
   ie. sqr(v1[x]-v2[x])+... }
function VectorDistance2(const v1, v2 : TAffineVector): Single; overload;
{: Calculates the "Norm 2" between two vectors.<p>
   ie. sqr(v1[x]-v2[x])+... }
function VectorDistance2(const v1, v2 : TVector): Single; overload;

{: Calculates a vector perpendicular to N.<p>
   N is assumed to be of unit length, subtract out any component parallel to N }
function VectorPerpendicular(const V, N: TAffineVector): TAffineVector;
//: Reflects vector V against N (assumes N is normalized)
function VectorReflect(const V, N: TAffineVector): TAffineVector;
//: Rotates Vector about Axis with Angle radiants
procedure RotateVector(var vector : TVector; const axis : TAffineVector; angle : Single); overload;
//: Rotates Vector about Axis with Angle radiants
procedure RotateVector(var vector : TVector; const axis : TVector; angle : Single); overload;

//: Rotate given vector around the Y axis (alpha is in rad)
procedure RotateVectorAroundY(var v : TAffineVector; alpha : Single);
//: Returns given vector rotated around the Y axis (alpha is in rad)
function VectorRotateAroundY(const v : TAffineVector; alpha : Single) : TAffineVector; overload;
//: Returns given vector rotated around the Y axis in vr (alpha is in rad)
procedure VectorRotateAroundY(const v : TAffineVector; alpha : Single; var vr : TAffineVector); overload;

//: Vector components are replaced by their Abs() value. }
procedure AbsVector(var v : TVector); overload;
//: Vector components are replaced by their Abs() value. }
procedure AbsVector(var v : TAffineVector); overload;

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

procedure SetMatrix(var dest : THomogeneousDblMatrix; const src : TMatrix); overload;

//: Creates scale matrix
function CreateScaleMatrix(const v : TAffineVector) : TMatrix; overload;
//: Creates scale matrix
function CreateScaleMatrix(const v : TVector) : TMatrix; overload;
//: Creates translation matrix
function CreateTranslationMatrix(const V : TAffineVector): TMatrix; overload;
//: Creates translation matrix
function CreateTranslationMatrix(const V : TVector): TMatrix; overload;
{: Creates a scale+translation matrix.<p>
   Scale is applied BEFORE applying offset }
function CreateScaleAndTranslationMatrix(const scale, offset : TVector): TMatrix; overload;
//: Creates matrix for rotation about x-axis
function CreateRotationMatrixX(const Sine, Cosine: Single) : TMatrix;
//: Creates matrix for rotation about y-axis
function CreateRotationMatrixY(const Sine, Cosine: Single) : TMatrix;
//: Creates matrix for rotation about z-axis
function CreateRotationMatrixZ(const Sine, Cosine: Single) : TMatrix;
//: Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateRotationMatrix(Axis: TAffineVector; Angle: Single): TMatrix;
//: Creates a rotation matrix along the given Axis by the given Angle in radians.
function CreateAffineRotationMatrix(Axis: TAffineVector; Angle: Single): TAffineMatrix;

//: Multiplies two 3x3 matrices
function MatrixMultiply(const M1, M2 : TAffineMatrix) : TAffineMatrix; overload
//: Multiplies two 4x4 matrices
function MatrixMultiply(const M1, M2 : TMatrix) : TMatrix; overload
//: Multiplies M1 by M2 and places result in MResult
procedure MatrixMultiply(const M1, M2 : TMatrix; var MResult : TMatrix); overload

//: Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TVector; const M: TMatrix): TVector; overload;
//: Transforms a homogeneous vector by multiplying it with a matrix
function VectorTransform(const V: TVector; const M: TAffineMatrix): TVector; overload;
//: Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TMatrix): TAffineVector; overload;
//: Transforms an affine vector by multiplying it with a matrix
function VectorTransform(const V: TAffineVector; const M: TAffineMatrix): TAffineVector; overload;

//: Determinant of a 3x3 matrix
function MatrixDeterminant(const M: TAffineMatrix): Single; overload;
//: Determinant of a 4x4 matrix
function MatrixDeterminant(const M: TMatrix): Single; overload;

{: Adjoint of a 4x4 matrix.<p>
   used in the computation of the inverse of a 4x4 matrix }
procedure AdjointMatrix(var M : TMatrix);

//: Multiplies all elements of a 3x3 matrix with a factor
procedure ScaleMatrix(var M : TAffineMatrix; const factor : Single); overload;
//: Multiplies all elements of a 4x4 matrix with a factor
procedure ScaleMatrix(var M : TMatrix; const factor : Single); overload;

//: Computes transpose of 3x3 matrix
procedure TransposeMatrix(var M: TAffineMatrix); overload;
//: Computes transpose of 4x4 matrix
procedure TransposeMatrix(var M: TMatrix); overload;

//: Finds the inverse of a 4x4 matrix
procedure InvertMatrix(var M: TMatrix);
{: Decompose a non-degenerated 4x4 transformation matrix into the sequence of transformations that produced it.<p>
   Modified by ml then eg, original Author: Spencer W. Thomas, University of Michigan<p>
   The coefficient of each transformation is returned in the corresponding
   element of the vector Tran.<p>
   Returns true upon success, false if the matrix is singular. }
function  MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean;

//------------------------------------------------------------------------------
// Matrix functions
//------------------------------------------------------------------------------

//: Calculates the parameters of a plane defined by three points.
function PlaneMake(const p1, p2, p3 : TAffineVector) : THmgPlane;

{: Calculates the cross-product between the plane normal and plane to point vector.<p>
   This functions gives an hint as to were the point is, if the point is in the
   half-space pointed by the vector, result is positive.<p>
   This function performs an homogeneous space dot-product. }
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TAffineVector) : Single;

{: Calculate the normal of a plane defined by three points. }
function CalcPlaneNormal(const p1, p2, p3 : TAffineVector) : TAffineVector; overload;
procedure CalcPlaneNormal(const p1, p2, p3 : TAffineVector; var vr : TAffineVector); overload;
procedure CalcPlaneNormal(const p1, p2, p3 : TVector; var vr : TAffineVector); overload;

//------------------------------------------------------------------------------
// Quaternion functions
//------------------------------------------------------------------------------

//: Creates a quaternion from the given values
function QuaternionMake(Imag: array of Single; Real: Single): TQuaternion;
//: Returns the conjugate of a quaternion
function QuaternionConjugate(const Q: TQuaternion): TQuaternion;
//: Constructs a unit quaternion from two points on unit sphere
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion;
{: Returns quaternion product qL * qR.<p>
   Note: order is important!<p>
   To combine rotations, use the product QuaternionMuliply(qSecond, qFirst),
   which gives the effect of rotating by qFirst then qSecond. }
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
{: Constructs rotation matrix from (possibly non-unit) quaternion.<p>
   Assumes matrix is used to multiply column vector on the left:<br>
   vnew = mat vold.<p>
   Works correctly for right-handed coordinate system and right-handed rotations. }
function QuaternionToMatrix(const Q: TQuaternion): TMatrix;
{: Spherical linear interpolation of unit quaternions with spins.<p>
   QStart, QEnd - start and end unit quaternions<br>
   t            - interpolation parameter (0 to 1)<br>
   Spin         - number of extra spin rotations to involve<br> }
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;
//: Converts a unit quaternion into two points on a unit sphere
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector);

//------------------------------------------------------------------------------
// Logarithmic and exponential functions
//------------------------------------------------------------------------------

{: Return ln(1 + X),  accurate for X near 0. }
function LnXP1(X: Extended): Extended;
{: Log base 10 of X}
function Log10(X: Extended): Extended;
{: Log base 2 of X }
function Log2(X: Extended): Extended; overload;
{: Log base 2 of X }
function Log2(X: Single): Single; overload;
{: Log base N of X }
function LogN(Base, X: Extended): Extended;
{: Raise base to an integer. }
function IntPower(Base: Extended; Exponent: Integer): Extended;
{: Raise base to any power.<p>
   For fractional exponents, or |exponents| > MaxInt, base must be > 0. }
function Power(const Base, Exponent: Single): Single; overload;
{: Raise base to an integer. }
function Power(Base: Single; Exponent: Integer): Single; overload;

//------------------------------------------------------------------------------
// Trigonometric functions
//------------------------------------------------------------------------------

function DegToRad(const Degrees: Extended): Extended; overload;
function DegToRad(const Degrees: Single): Single; overload;
function RadToDeg(const Radians: Extended): Extended; overload;
function RadToDeg(const Radians: Single): Single; overload;

//: Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Extended; var Sin, Cos: Extended); overload;
//: Calculates sine and cosine from the given angle Theta
procedure SinCos(const Theta: Single; var Sin, Cos: Single); overload;
{: Calculates sine and cosine from the given angle Theta and Radius.<p>
   sin and cos values calculated from theta are multiplicated by radius. }
procedure SinCos(const theta, radius : Single; var Sin, Cos: Single); overload;

//: Fills up the two given dynamic arrays with sin cos values
procedure PrepareSinCosCache(var s, c : array of Single;
                             startAngle, stopAngle : Single);

function  ArcCos(const X: Extended) : Extended; overload;
function  ArcCos(const x : Single) : Single; overload;
function  ArcSin(const X : Extended) : Extended; overload;
function  ArcSin(const X : Single) : Single; overload;
function  ArcTan2(const Y, X : Extended) : Extended; overload;
function  ArcTan2(const Y, X : Single) : Single; overload;
function  Tan(const X : Extended) : Extended; overload;
function  Tan(const X : Single) : Single; overload;
function  CoTan(const X : Extended) : Extended; overload;
function  CoTan(const X : Single) : Single; overload;

//------------------------------------------------------------------------------
// Miscellanious math functions
//------------------------------------------------------------------------------

function Trunc(x : Extended) : Int64; overload;
function Trunc(x : Single) : Integer; overload;
function Frac(v : Extended) : Extended; overload;
function Frac(v : Single) : Single; overload;
function Round(v : Extended) : Int64; overload;
function Round(v : Single) : Integer; overload;

{: Returns the sign of the x value using the (-1, 0, +1) convention }
function Sign(x : Single) : Integer;

{: Returns True if x is in [a; b] }
function IsInRange(const x, a, b : Single) : Boolean;

{: Returns True if p is in the cube defined by d. }
function IsInCube(const p, d : TAffineVector) : Boolean; overload;
function IsInCube(const p, d : TVector) : Boolean; overload;

{: Returns the minimum value of the array. }
function MinFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
function MinFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
function MinFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;
{: Returns the maximum value of the array. }
function MaxFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
function MaxFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
function MaxFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;

{: Returns the max of the X, Y and Z components of a vector (W is ignored). }
function MaxXYZComponent(const v : TVector) : Single;
{: Returns the min of the X, Y and Z components of a vector (W is ignored). }
function MinXYZComponent(const v : TVector) : Single;
{: Returns the max of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MaxAbsXYZComponent(v : TVector) : Single;
{: Returns the min of the Abs(X), Abs(Y) and Abs(Z) components of a vector (W is ignored). }
function MinAbsXYZComponent(v : TVector) : Single;

{: Clamps aValue in the aMin-aMax interval.<p> }
function ClampValue(const aValue, aMin, aMax : Single) : Single; overload;
{: Clamps aValue in the aMin-INF interval.<p> }
function ClampValue(const aValue, aMin : Single) : Single; overload;

{: Returns the detected optimization mode.<p>
   Returned values is either 'FPU', '3DNow!' or 'SSE'. }
function GeometryOptimizationMode : String;

{: Begins a FPU-only section.<p>
   You can use a FPU-only section to force use of FPU versions of the math
   functions, though typically slower than their SIMD counterparts, they have
   a higher precision (80 bits internally) that may be required in some cases.<p>
   Each BeginFPUOnlySection call must be balanced by a EndFPUOnlySection (calls
   can be nested). }
procedure BeginFPUOnlySection;
{: Ends a FPU-only section.<p>
   See BeginFPUOnlySection. }
procedure EndFPUOnlySection;

//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines
//--------------------- Unstandardized functions after these lines

// mixed functions

{: Turn a triplet of rotations about x, y, and z (in that order) into an equivalent rotation around a single axis (all in radians).<p> }
function  ConvertRotation(const Angles: TAffineVector): TVector;

// miscellaneous functions

function  MakeAffineDblVector(var V: array of Double): TAffineDblVector;
function  MakeDblVector(var v : array of Double) : THomogeneousDblVector;
function  VectorAffineDblToFlt(const V: TAffineDblVector): TAffineVector;
function  VectorDblToFlt(const V: THomogeneousDblVector): THomogeneousVector;
function  VectorAffineFltToDbl(const V: TAffineVector): TAffineDblVector;
function  VectorFltToDbl(const V: TVector): THomogeneousDblVector;

function  PointInPolygon(var xp, yp : array of Single; x, y: Single): Boolean;

// coordinate system manipulation functions

//: Rotates the given coordinate system (represented by the matrix) around its Y-axis
function Turn(const Matrix: TMatrix; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterUp
function Turn(const Matrix: TMatrix; const MasterUp: TAffineVector; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around its X-axis
function Pitch(const Matrix: TMatrix; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterRight
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around its Z-axis
function Roll(const Matrix: TMatrix; Angle: Single): TMatrix; overload;
//: Rotates the given coordinate system (represented by the matrix) around MasterDirection
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;

// intersection functions

{: Calculates the intersection point "res" of a line with a plane.<p>
   Return value:<ul>
   <li>0 : no intersection, line parallel to plane
   <li>1 : res is valid
   <li>-1 : line is inside plane
   </ul><br>
   Adapted from:<br>
      E.Hartmann, Computerunterstützte Darstellende Geometrie, B.G. Teubner Stuttgart 1988 }
function IntersectLinePlane(const point, direction : TAffineVector;
                            const plane : THmgPlane;
                            intersectPoint : PAffineVector = nil) : Integer;

                            {: Calculate intersection between a ray and a plane.<p>
   Returns True if an intersection was found, the intersection point is placed
   in intersectPoint is the reference is not nil. }
function RayCastPlaneIntersect(const rayStart, rayVector : TAffineVector;
                               const planePoint, planeNormal : TAffineVector;
                               intersectPoint : PAffineVector = nil) : Boolean; overload;
{: Calculate intersection between a ray and a triangle. }
function RayCastTriangleIntersect(const rayStart, rayVector : TAffineVector;
                                  const p1, p2, p3 : TAffineVector;
                                  intersectPoint : PAffineVector = nil;
                                  intersectNormal : PAffineVector = nil) : Boolean;

//: Determines if volume is clipped or not
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean; overload;

// misc funcs

// Creates a shadow projection matrix out of the plane equation
// coefficients and the position of the light. The return value is stored
// in destMat[][]
function MakeShadowMatrix(const planePoint, planeNormal, lightPos : TVector) : TMatrix;

{: Builds a reflection matrix for the given plane.<p>
   Reflection matrix allow implementing planar reflectors in OpenGL (mirrors). }
function MakeReflectionMatrix(const planePoint, planeNormal : TAffineVector) : TMatrix;

const
   cPIdiv180 : Single = 0.017453292;
   c180divPI : Single = 57.29577951;

var
   // this var is adjusted during "initialization", current values are
   // + 0 : use standard optimized FPU code
   // + 1 : use 3DNow! optimized code (requires K6-2/3 CPU)
   // + 2 : use Intel SSE code (Pentium III, NOT IMPLEMENTED YET !)
   vSIMD : Byte = 0;

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
implementation
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

uses SysUtils;

const
  // FPU status flags (high order byte)
  C0 = 1;
  C1 = 2;
  C2 = 4;
  C3 = $40;
  cwChop : Word = $1F3F;


  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

  cOne : Single = 1.0;

type

   TProcVarVectConstVect = procedure (var v1 : TVector; const v2 : TVector);

// OptimizationMode
//
function GeometryOptimizationMode : String;
begin
   case vSIMD of
      0 : Result:='FPU';
      1 : Result:='3DNow!';
      2 : Result:='SSE';
   else
      Result:='*ERR*';
   end;
end;

// BeginFPUOnlySection
//
var
   vOldSIMD : Byte;
   vFPUOnlySectionCounter : Integer;
procedure BeginFPUOnlySection;
begin
   if vFPUOnlySectionCounter=0 then
      vOldSIMD:=vSIMD;
   Inc(vFPUOnlySectionCounter);
   vSIMD:=0;
end;

// EndFPUOnlySection
//
procedure EndFPUOnlySection;
begin
   Dec(vFPUOnlySectionCounter);
   Assert(vFPUOnlySectionCounter>=0);
   if vFPUOnlySectionCounter=0 then
      vSIMD:=vOldSIMD;
end;

//------------------------------------------------------------------------------
//----------------- vector functions -------------------------------------------
//------------------------------------------------------------------------------

// TexPointMake
//
function TexPointMake(const s, t : Single) : TTexPoint;
begin
   Result.S:=s;
   Result.T:=t;
end;

// AffineVectorMake
//
function AffineVectorMake(const x, y, z : Single) : TAffineVector; overload;
begin
   Result[0]:=x;
   Result[1]:=y;
   Result[2]:=z;
end;

// AffineVectorMake
//
function AffineVectorMake(const v : TVector) : TAffineVector;
begin
   Result[0]:=v[0];
   Result[1]:=v[1];
   Result[2]:=v[2];
end;

// SetAffineVector
//
procedure SetAffineVector(var v : TAffineVector; const x, y, z : Single); overload;
begin
   v[0]:=x;
   v[1]:=y;
   v[2]:=z;
end;

// SetVector (affine)
//
procedure SetVector(var v : TAffineVector; const x, y, z : Single);
begin
   v[0]:=x;
   v[1]:=y;
   v[2]:=z;
end;

// SetVector (affine-hmg)
//
procedure SetVector(var v : TAffineVector; const vSrc : TVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine-affine)
//
procedure SetVector(var v : TAffineVector; const vSrc : TAffineVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine double - affine single)
//
procedure SetVector(var v : TAffineDblVector; const vSrc : TAffineVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// SetVector (affine double - hmg single)
//
procedure SetVector(var v : TAffineDblVector; const vSrc : TVector);
begin
   v[0]:=vSrc[0];
   v[1]:=vSrc[1];
   v[2]:=vSrc[2];
end;

// VectorMake
//
function VectorMake(const v : TAffineVector; w : Single = 0) : TVector;
begin
	Result[0]:=v[0];
	Result[1]:=v[1];
	Result[2]:=v[2];
	Result[3]:=w;
end;

// VectorMake
//
function VectorMake(const x, y, z : Single; w : Single = 0) : TVector;
begin
	Result[0]:=x;
	Result[1]:=y;
	Result[2]:=z;
	Result[3]:=w;
end;

// PointMake
//
function PointMake(const x, y, z: Single) : TVector;
begin
	Result[0]:=x;
	Result[1]:=y;
	Result[2]:=z;
   Result[3]:=1;
end;

// SetVector
//
procedure SetVector(var v : TVector; const x, y, z : Single; w : Single = 0);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=w;
end;

// SetVector
//
procedure SetVector(var v : TVector; const av : TAffineVector; w : Single = 0);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=w;
end;

// SetVector
//
procedure SetVector(var v : TVector; const vSrc : TVector);
begin
   // faster than memcpy or move...
	v[0]:=vSrc[0];
	v[1]:=vSrc[1];
	v[2]:=vSrc[2];
	v[3]:=vSrc[3];
end;

// MakePoint
//
procedure MakePoint(var v : TVector; const x, y, z: Single);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=1.0;
end;

// MakePoint
//
procedure MakePoint(var v : TVector; const av : TAffineVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=1.0;
end;

// MakeVector
//
procedure MakeVector(var v : TAffineVector; const x, y, z: Single); overload;
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
end;

// MakeVector
//
procedure MakeVector(var v : TVector; const x, y, z: Single);
begin
	v[0]:=x;
	v[1]:=y;
	v[2]:=z;
	v[3]:=0.0;
end;

// MakeVector
//
procedure MakeVector(var v : TVector; const av : TAffineVector);
begin
	v[0]:=av[0];
	v[1]:=av[1];
	v[2]:=av[2];
	v[3]:=0.0;
end;

// VectorAdd (func, affine)
//
function VectorAdd(const V1, V2 : TAffineVector) : TAffineVector; register;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
end;

// VectorAdd (proc, affine)
//
procedure VectorAdd(const V1, V2 : TAffineVector; var vr : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
end;

// VectorAdd (hmg)
//
function VectorAdd(const V1, V2: TVector): TVector; register;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
         FLD  DWORD PTR [EAX]
         FADD DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FADD DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FADD DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
         FLD  DWORD PTR [EAX+12]
         FADD DWORD PTR [EDX+12]
         FSTP DWORD PTR [ECX+12]
end;

// VectorAdd (affine, single)
//
function VectorAdd(const v : TAffineVector; const f : Single) : TAffineVector;
begin
   Result[0]:=v[0]+f;
   Result[1]:=v[1]+f;
   Result[2]:=v[2]+f;
end;

// VectorAdd (hmg, single)
//
function VectorAdd(const v : TVector; const f : Single) : TVector;
begin
   Result[0]:=v[0]+f;
   Result[1]:=v[1]+f;
   Result[2]:=v[2]+f;
   Result[3]:=v[3]+f;
end;

// AddVector (affine)
//
procedure AddVector(var V1 : TAffineVector; const V2 : TAffineVector); register;
// EAX contains address of V1
// EDX contains address of V2
asm
      FLD  DWORD PTR [EAX]
      FADD DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FADD DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FADD DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
end;

//
// AddVector (hmg)
//
procedure AddVector(var v1 : TVector; const v2 : TVector); register;
// EAX contains address of V1
// EDX contains address of V2
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9E       /// PFADD MM0, [EDX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9E   /// PFADD MM1, [EDX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FADD DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FADD DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FADD DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FADD DWORD PTR [EDX+12]
      FSTP DWORD PTR [EAX+12]
end;

// AddVector (affine)
//
procedure AddVector(var v : TAffineVector; const f : Single);
begin
   v[0]:=v[0]+f;
   v[1]:=v[1]+f;
   v[2]:=v[2]+f;
end;

// AddVector (hmg)
//
procedure AddVector(var v : TVector; const f : Single);
begin
   v[0]:=v[0]+f;
   v[1]:=v[1]+f;
   v[2]:=v[2]+f;
   v[3]:=v[3]+f;
end;

// VectorSubtract (func, affine)
//
function VectorSubtract(const V1, V2: TAffineVector): TAffineVector; register;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
         FLD  DWORD PTR [EAX]
         FSUB DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FSUB DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FSUB DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const v1, v2 : TAffineVector; var result : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
end;

// VectorSubtract (hmg)
//
function VectorSubtract(const V1, V2: TVector): TVector; register;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [ECX+12]
end;

// VectorSubtract (proc, hmg)
//
procedure VectorSubtract(const v1, v2 : TVector; var result : TVector); register;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [ECX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [ECX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [ECX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [ECX+12]
end;

// VectorSubtract (proc, affine)
//
procedure VectorSubtract(const v1, v2 : TVector; var result : TAffineVector); overload;
// EAX contains address of V1
// EDX contains address of V2
// ECX contains the result
asm
         FLD  DWORD PTR [EAX]
         FSUB DWORD PTR [EDX]
         FSTP DWORD PTR [ECX]
         FLD  DWORD PTR [EAX+4]
         FSUB DWORD PTR [EDX+4]
         FSTP DWORD PTR [ECX+4]
         FLD  DWORD PTR [EAX+8]
         FSUB DWORD PTR [EDX+8]
         FSTP DWORD PTR [ECX+8]
end;

// SubtractVector (affine)
//
procedure SubtractVector(var V1 : TAffineVector; const V2 : TAffineVector); register;
// EAX contains address of V1
// EDX contains address of V2
asm
         FLD  DWORD PTR [EAX]
         FSUB DWORD PTR [EDX]
         FSTP DWORD PTR [EAX]
         FLD  DWORD PTR [EAX+4]
         FSUB DWORD PTR [EDX+4]
         FSTP DWORD PTR [EAX+4]
         FLD  DWORD PTR [EAX+8]
         FSUB DWORD PTR [EDX+8]
         FSTP DWORD PTR [EAX+8]
end;

// SubtractVector (hmg)
//
procedure SubtractVector(var V1 : TVector; const V2 : TVector); register;
// EAX contains address of V1
// EDX contains address of V2
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
      db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FSTP DWORD PTR [EAX+12]
end;

// CombineVector
//
procedure CombineVector(var vr : TAffineVector; const v : TAffineVector; var f : Single); register;
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
asm
         FLD  DWORD PTR [EDX]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX]
         FSTP DWORD PTR [EAX]
         FLD  DWORD PTR [EDX+4]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+4]
         FSTP DWORD PTR [EAX+4]
         FLD  DWORD PTR [EDX+8]
         FMUL DWORD PTR [ECX]
         FADD DWORD PTR [EAX+8]
         FSTP DWORD PTR [EAX+8]
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TAffineVector; const F1, F2: Single): TAffineVector; register;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
end;

// VectorCombine3
//
function VectorCombine3(const V1, V2, V3: TAffineVector; const F1, F2, F3: Single): TAffineVector;
begin
  Result[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
  Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
  Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
end;

// CombineVector
//
procedure CombineVector(var vr : TVector; const v : TVector; var f : Single); overload;
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:
      db $0F,$6E,$11           /// MOVD  MM2, [ECX]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
      db $0F,$6F,$02           /// MOVQ  MM0, [EDX]
      db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
      db $0F,$0F,$00,$9E       /// PFADD MM0, [EAX]
      db $0F,$7F,$00           /// MOVQ  [EAX], MM0
      db $0F,$6F,$4A,$08       /// MOVQ  MM1, [EDX+8]
      db $0F,$0F,$CA,$B4       /// PFMUL MM1, MM2
      db $0F,$0F,$48,$08,$9E   /// PFADD MM1, [EAX+8]
      db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
      db $0F,$0E               /// FEMMS
      ret
@@FPU:
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EDX+12]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+12]
      FSTP DWORD PTR [EAX+12]
end;

// CombineVector
//
procedure CombineVector(var vr : TVector; const v : TAffineVector; var f : Single); overload;
// EAX contains address of vr
// EDX contains address of v
// ECX contains address of f
asm
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [ECX]
      FADD DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
end;

// VectorCombine
//
function VectorCombine(const V1, V2: TVector; const F1, F2: Single): TVector;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   Result[W]:=(F1 * V1[W]) + (F2 * V2[W]);
end;

// VectorCombine
//
function VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single): TVector; overload;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   Result[W]:=F1*V1[W];
end;

// VectorCombine
//
procedure VectorCombine(const V1, V2: TVector; const F1, F2: Single; var vr : TVector); overload;
// EAX contains address of v1
// EDX contains address of v2
// ECX contains address of vr
// ebp+$c points to f1
// ebp+$8 points to f2
asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 246354
      db $0F,$6E,$4D,$0C       /// MOVD  MM1, [EBP+$0C]
      db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
      db $0F,$6E,$55,$08       /// MOVD  MM2, [EBP+$08]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2

      db $0F,$6F,$18           /// MOVQ  MM3, [EAX]
      db $0F,$0F,$D9,$B4       /// PFMUL MM3, MM1
      db $0F,$6F,$22           /// MOVQ  MM4, [EDX]
      db $0F,$0F,$E2,$B4       /// PFMUL MM4, MM2
      db $0F,$0F,$DC,$9E       /// PFADD MM3, MM4
      db $0F,$7F,$19           /// MOVQ  [ECX], MM3

      db $0F,$6F,$68,$08       /// MOVQ  MM5, [EAX+8]
      db $0F,$0F,$E9,$B4       /// PFMUL MM5, MM1
      db $0F,$6F,$72,$08       /// MOVQ  MM6, [EDX+8]
      db $0F,$0F,$F2,$B4       /// PFMUL MM6, MM2
      db $0F,$0F,$EE,$9E       /// PFADD MM5, MM6
      db $0F,$7F,$69,$08       /// MOVQ  [ECX+8], MM5

      db $0F,$0E               /// FEMMS
      pop ebp
      ret $08

@@FPU:      // 327363
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX]

      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+4]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+4]

      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+8]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+8]

      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+$0C]
      FLD  DWORD PTR [EDX+12]
      FMUL DWORD PTR [EBP+$08]
      FADD
      FSTP DWORD PTR [ECX+12]
end;

// VectorCombine
//
procedure VectorCombine(const V1 : TVector; const V2: TAffineVector; const F1, F2: Single; var vr : TVector);
begin
   vr[X]:=(F1 * V1[X]) + (F2 * V2[X]);
   vr[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]);
   vr[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]);
   vr[W]:=F1*V1[W];
end;

// VectorCombine3
//
function VectorCombine3(const V1, V2, V3 : TVector; const F1, F2, F3 : Single) : TVector;
begin
   Result[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
   Result[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
   Result[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
   Result[W]:=(F1 * V1[W]) + (F2 * V2[W]) + (F3 * V3[W]);
end;

// VectorCombine3
//
procedure VectorCombine3(const V1, V2, V3: TVector; const F1, F2, F3: Single; var vr : TVector);
// EAX contains address of v1
// EDX contains address of v2
// ECX contains address of v3
// EBX contains address of vr
// ebp+$14 points to f1
// ebp+$10 points to f2
// ebp+$0c points to f3
begin
   asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 197
      db $0F,$6E,$4D,$14       /// MOVD  MM1, [EBP+$14]
      db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
      db $0F,$6E,$55,$10       /// MOVD  MM2, [EBP+$10]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
      db $0F,$6E,$5D,$0C       /// MOVD  MM3, [EBP+$0C]
      db $0F,$62,$DB           /// PUNPCKLDQ MM3, MM3

      db $0F,$6F,$20           /// MOVQ  MM4, [EAX]
      db $0F,$0F,$E1,$B4       /// PFMUL MM4, MM1
      db $0F,$6F,$2A           /// MOVQ  MM5, [EDX]
      db $0F,$0F,$EA,$B4       /// PFMUL MM5, MM2
      db $0F,$0F,$E5,$9E       /// PFADD MM4, MM5
      db $0F,$6F,$31           /// MOVQ  MM6, [ECX]
      db $0F,$0F,$F3,$B4       /// PFMUL MM6, MM3
      db $0F,$0F,$E6,$9E       /// PFADD MM4, MM6
      db $0F,$7F,$23           /// MOVQ  [EBX], MM4

      db $0F,$6F,$78,$08       /// MOVQ  MM7, [EAX+8]
      db $0F,$0F,$F9,$B4       /// PFMUL MM7, MM1
      db $0F,$6F,$42,$08       /// MOVQ  MM0, [EDX+8]
      db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
      db $0F,$0F,$F8,$9E       /// PFADD MM7, MM0
      db $0F,$6F,$69,$08       /// MOVQ  MM5, [ECX+8]
      db $0F,$0F,$EB,$B4       /// PFMUL MM5, MM3
      db $0F,$0F,$FD,$9E       /// PFADD MM7, MM5
      db $0F,$7F,$7B,$08       /// MOVQ  [EBX+8], MM7

      db $0F,$0E               /// FEMMS
      pop ebx
      pop ebp
      ret $10
@@FPU:      // 263
   end;
   vr[X]:=(F1 * V1[X]) + (F2 * V2[X]) + (F3 * V3[X]);
   vr[Y]:=(F1 * V1[Y]) + (F2 * V2[Y]) + (F3 * V3[Y]);
   vr[Z]:=(F1 * V1[Z]) + (F2 * V2[Z]) + (F3 * V3[Z]);
   vr[W]:=(F1 * V1[W]) + (F2 * V2[W]) + (F3 * V3[W]);
end;

// VectorDotProduct (affine)
//
function VectorDotProduct(const V1, V2 : TAffineVector): Single; assembler; register;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
asm
         FLD DWORD PTR [EAX]
         FMUL DWORD PTR [EDX]
         FLD DWORD PTR [EAX + 4]
         FMUL DWORD PTR [EDX + 4]
         FADDP
         FLD DWORD PTR [EAX + 8]
         FMUL DWORD PTR [EDX + 8]
         FADDP
end;

// VectorDotProduct (hmg)
//
function VectorDotProduct(const V1, V2 : TVector) : Single; assembler; register;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
asm
         FLD DWORD PTR [EAX]
         FMUL DWORD PTR [EDX]
         FLD DWORD PTR [EAX + 4]
         FMUL DWORD PTR [EDX + 4]
         FADDP
         FLD DWORD PTR [EAX + 8]
         FMUL DWORD PTR [EDX + 8]
         FADDP
         FLD DWORD PTR [EAX + 12]
         FMUL DWORD PTR [EDX + 12]
         FADDP
end;

// VectorDotProduct
//
function VectorDotProduct(const V1 : TVector; const V2 : TAffineVector) : Single; register;
// EAX contains address of V1
// EDX contains address of V2
// result is stored in ST(0)
asm
         FLD DWORD PTR [EAX]
         FMUL DWORD PTR [EDX]
         FLD DWORD PTR [EAX + 4]
         FMUL DWORD PTR [EDX + 4]
         FADDP
         FLD DWORD PTR [EAX + 8]
         FMUL DWORD PTR [EDX + 8]
         FADDP
end;

// VectorCrossProduct
//
function VectorCrossProduct(const V1, V2: TAffineVector): TAffineVector;
// Temp is necessary because
// either V1 or V2 could also be the result vector
//
// EAX contains address of V1
// EDX contains address of V2
// ECX contains address of result
var
   temp: TAffineVector;
asm
  {Temp[X]:=V1[Y] * V2[Z]-V1[Z] * V2[Y];
  Temp[Y]:=V1[Z] * V2[X]-V1[X] * V2[Z];
  Temp[Z]:=V1[X] * V2[Y]-V1[Y] * V2[X];
  Result:=Temp;}

   PUSH EBX                      // save EBX, must be restored to original value
   LEA EBX, [Temp]
   FLD DWORD PTR [EDX + 8]       // first load both vectors onto FPU register stack
   FLD DWORD PTR [EDX + 4]
   FLD DWORD PTR [EDX + 0]
   FLD DWORD PTR [EAX + 8]
   FLD DWORD PTR [EAX + 4]
   FLD DWORD PTR [EAX + 0]

   FLD ST(1)                     // ST(0):=V1[Y]
   FMUL ST, ST(6)                // ST(0):=V1[Y] * V2[Z]
   FLD ST(3)                     // ST(0):=V1[Z]
   FMUL ST, ST(6)                // ST(0):=V1[Z] * V2[Y]
   FSUBP ST(1), ST               // ST(0):=ST(1)-ST(0)
   FSTP DWORD [EBX]              // Temp[X]:=ST(0)
   FLD ST(2)                     // ST(0):=V1[Z]
   FMUL ST, ST(4)                // ST(0):=V1[Z] * V2[X]
   FLD ST(1)                     // ST(0):=V1[X]
   FMUL ST, ST(7)                // ST(0):=V1[X] * V2[Z]
   FSUBP ST(1), ST               // ST(0):=ST(1)-ST(0)
   FSTP DWORD [EBX + 4]          // Temp[Y]:=ST(0)
   FLD ST                        // ST(0):=V1[X]
   FMUL ST, ST(5)                // ST(0):=V1[X] * V2[Y]
   FLD ST(2)                     // ST(0):=V1[Y]
   FMUL ST, ST(5)                // ST(0):=V1[Y] * V2[X]
   FSUBP ST(1), ST               // ST(0):=ST(1)-ST(0)
   FSTP DWORD [EBX + 8]          // Temp[Z]:=ST(0)
   FSTP ST(0)                    // clear FPU register stack
   FSTP ST(0)
   FSTP ST(0)
   FSTP ST(0)
   FSTP ST(0)
   FSTP ST(0)
   MOV EAX, [EBX]                // copy Temp to Result
   MOV [ECX], EAX
   MOV EAX, [EBX + 4]
   MOV [ECX + 4], EAX
   MOV EAX, [EBX + 8]
   MOV [ECX + 8], EAX
   POP EBX
end;

// VectorCrossProduct
//
function VectorCrossProduct(const V1, V2: TVector): TVector;
begin
   Result[X]:=V1[Y] * V2[Z] - V1[Z] * V2[Y];
   Result[Y]:=V1[Z] * V2[X] - V1[X] * V2[Z];
   Result[Z]:=V1[X] * V2[Y] - V1[Y] * V2[X];
   Result[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const V1, V2: TVector; var vr : TVector);
begin
   vr[X]:=V1[Y] * V2[Z] - V1[Z] * V2[Y];
   vr[Y]:=V1[Z] * V2[X] - V1[X] * V2[Z];
   vr[Z]:=V1[X] * V2[Y] - V1[Y] * V2[X];
   vr[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TVector); overload;
begin
   vr[X]:=V1[Y] * V2[Z] - V1[Z] * V2[Y];
   vr[Y]:=V1[Z] * V2[X] - V1[X] * V2[Z];
   vr[Z]:=V1[X] * V2[Y] - V1[Y] * V2[X];
   vr[W]:=0;
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TVector; var vr : TAffineVector); overload;
begin
   vr[X]:=V1[Y] * V2[Z] - V1[Z] * V2[Y];
   vr[Y]:=V1[Z] * V2[X] - V1[X] * V2[Z];
   vr[Z]:=V1[X] * V2[Y] - V1[Y] * V2[X];
end;

// VectorCrossProduct
//
procedure VectorCrossProduct(const v1, v2 : TAffineVector; var vr : TAffineVector); overload;
begin
   vr[X]:=V1[Y] * V2[Z] - V1[Z] * V2[Y];
   vr[Y]:=V1[Z] * V2[X] - V1[X] * V2[Z];
   vr[Z]:=V1[X] * V2[Y] - V1[Y] * V2[X];
end;

// Lerp
//
function Lerp(const start, stop, t : Single) : Single;
begin
   Result:=start + (stop - start) * t;
end;

// VectorAffineLerp
//
function VectorLerp(const V1, V2: TAffineVector; t: Single): TAffineVector;
begin
   Result[X]:=V1[X]+(V2[X]-V1[X])*t;
   Result[Y]:=V1[Y]+(V2[Y]-V1[Y])*t;
   Result[Z]:=V1[Z]+(V2[Z]-V1[Z])*t;
end;

// VectorLerp
//
procedure VectorLerp(const v1, v2 : TAffineVector; t : Single; var vr : TAffineVector); register;
// EAX contains address of v1
// EDX contains address of v2
// EBX contains address of t
// ECX contains address of vr
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 185 !
      db $0F,$6E,$3B           /// MOVD  MM7, [EBX]
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$6F,$0A           /// MOVQ  MM1, [EDX]
      db $0F,$62,$FF           /// PUNPCKLDQ MM7, MM7
      db $0F,$0F,$C8,$9A       /// PFSUB MM1, MM0
      db $0F,$6E,$50,$08       /// MOVD  MM2, [EAX+8]
      db $0F,$0F,$CF,$B4       /// PFMUL MM1, MM7
      db $0F,$0F,$C1,$9E       /// PFADD MM0, MM1
      db $0F,$6F,$5A,$08       /// MOVQ  MM3, [EDX+8]
      db $0F,$0F,$DA,$9A       /// PFSUB MM3, MM2
      db $0F,$0F,$DF,$B4       /// PFMUL MM3, MM7
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$0F,$D3,$9E       /// PFADD MM2, MM3
      db $0F,$7E,$51,$08       /// MOVD  [ECX+8], MM2

      db $0F,$0E               /// FEMMS
      pop ebx
      pop ebp
      ret $04
@@FPU:      // 811 !!!
   end;
   vr[X]:=V1[X]+(V2[X]-V1[X])*pt^;
   vr[Y]:=V1[Y]+(V2[Y]-V1[Y])*pt^;
   vr[Z]:=V1[Z]+(V2[Z]-V1[Z])*pt^;
end;

// VectorLerp
//
function VectorLerp(const V1, V2: TVector; t: Single): TVector;
var
   pt : ^Single;
begin
   pt:=@t;
   Result[X]:=V1[X]+(V2[X]-V1[X])*pt^;
   Result[Y]:=V1[Y]+(V2[Y]-V1[Y])*pt^;
   Result[Z]:=V1[Z]+(V2[Z]-V1[Z])*pt^;
   Result[W]:=V1[W]+(V2[W]-V1[W])*pt^;
end;

// VectorLerp
//
procedure VectorLerp(const v1, v2 : TVector; t : Single; var vr : TVector); register;
// EAX contains address of v1
// EDX contains address of v2
// EBX contains address of t
// ECX contains address of vr
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      test vSIMD, 1
      jz @@FPU
@@3DNow:    // 173
      db $0F,$6E,$3B           /// MOVD  MM7, [EBX]
      db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
      db $0F,$6F,$0A           /// MOVQ  MM1, [EDX]
      db $0F,$62,$FF           /// PUNPCKLDQ MM7, MM7
      db $0F,$0F,$C8,$9A       /// PFSUB MM1, MM0
      db $0F,$0F,$CF,$B4       /// PFMUL MM1, MM7
      db $0F,$6F,$50,$08       /// MOVQ  MM2, [EAX+8]
      db $0F,$6F,$5A,$08       /// MOVQ  MM3, [EDX+8]
      db $0F,$0F,$C1,$9E       /// PFADD MM0, MM1
      db $0F,$0F,$DA,$9A       /// PFSUB MM3, MM2
      db $0F,$0F,$DF,$B4       /// PFMUL MM3, MM7
      db $0F,$7F,$01           /// MOVQ  [ECX], MM0
      db $0F,$0F,$D3,$9E       /// PFADD MM2, MM3
      db $0F,$7F,$51,$08       /// MOVQ  [ECX+8], MM2

      db $0F,$0E               /// FEMMS
      pop ebx
      pop ebp
      ret $04
@@FPU:      // 242
   end;
   vr[X]:=V1[X]+(V2[X]-V1[X])*pt^;
   vr[Y]:=V1[Y]+(V2[Y]-V1[Y])*pt^;
   vr[Z]:=V1[Z]+(V2[Z]-V1[Z])*pt^;
   vr[W]:=V1[W]+(V2[W]-V1[W])*pt^;
end;

// VectorArrayLerp_3DNow (hmg)
//
procedure VectorArrayLerp_3DNow(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray); stdcall; overload;
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      push ebx
      push edi

      mov   eax, src1
      mov   edx, src2
      mov   ecx, n
      mov   ebx, dest
      mov   edi, pt

      db $0F,$0E               /// femms

      db $0F,$6E,$3F           /// movd     mm7, [edi]
      db $0F,$62,$FF           /// punpckldq mm7, mm7

@@Loop:
      db $0F,$6F,$00           /// movq     mm0, [eax]
      db $0F,$6F,$50,$08       /// movq     mm2, [eax+8]
      db $0F,$6F,$C8           /// movq     mm1, mm0
      db $0F,$6F,$DA           /// movq     mm3, mm2
      db $0F,$0F,$02,$AA       /// pfsubr   mm0, [edx]
      db $0F,$0F,$52,$08,$AA   /// pfsubr   mm2, [edx+8]
      db $0F,$0D,$4B,$20       /// prefetchw [ebx+32]
      db $0F,$0F,$C7,$B4       /// pfmul    mm0, mm7
      db $0F,$0F,$D7,$B4       /// pfmul    mm2, mm7
      add   eax, 16
      add   edx, 16
      db $0F,$0D,$40,$20       /// prefetch [eax+32]
      db $0F,$0F,$C1,$9E       /// pfadd    mm0, mm1
      db $0F,$0F,$D3,$9E       /// pfadd    mm2, mm3
      db $0F,$0D,$42,$20       /// prefetch [edx+32]
      db $0F,$7F,$03           /// movq     [ebx], mm0
      db $0F,$7F,$53,$08       /// movq     [ebx+8], mm2

      add   ebx, 16

      dec   ecx
      jnz @@Loop

      db $0F,$0E               /// femms

      pop edi
      pop ebx
   end;
end;

// VectorArrayLerp (hmg)
//
procedure VectorArrayLerp(const src1, src2 : PVectorArray; t : Single; n : Integer; dest : PVectorArray);
var
   i : Integer;
begin
   if vSIMD=1 then
      VectorArrayLerp_3DNow(src1, src2, t, n, dest)
   else for i:=0 to n-1 do begin
      dest[i][0]:=src1[i][0]+(src2[i][0]-src1[i][0])*t;
      dest[i][1]:=src1[i][1]+(src2[i][1]-src1[i][1])*t;
      dest[i][2]:=src1[i][2]+(src2[i][2]-src1[i][2])*t;
      dest[i][3]:=src1[i][3]+(src2[i][3]-src1[i][3])*t;
   end;
end;

// VectorArrayLerp_3DNow (affine)
//
procedure VectorArrayLerp_3DNow(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray); stdcall; overload;
var
   pt : ^Single;
begin
   pt:=@t;
   asm
      push ebx
      push edi

      mov   eax, src1
      mov   edx, src2
      mov   ecx, n
      shr   ecx, 1
      mov   ebx, dest
      mov   edi, pt

      db $0F,$0E               /// femms

      db $0F,$6E,$3F           /// movd     mm7, [edi]
      db $0F,$62,$FF           /// punpckldq mm7, mm7

@@Loop:
      db $0F,$6F,$00           /// movq     mm0, [eax]
      db $0F,$6F,$50,$08       /// movq     mm2, [eax+8]
      db $0F,$6F,$60,$10       /// movq     mm4, [eax+16]
      db $0F,$6F,$C8           /// movq     mm1, mm0
      db $0F,$6F,$DA           /// movq     mm3, mm2
      db $0F,$6F,$EC           /// movq     mm5, mm4
      db $0F,$0F,$02,$AA       /// pfsubr   mm0, [edx]
      db $0F,$0F,$52,$08,$AA   /// pfsubr   mm2, [edx+8]
      db $0F,$0F,$62,$10,$AA   /// pfsubr   mm4, [edx+16]
      db $0F,$0D,$4B,$40       /// prefetchw [ebx+64]
      db $0F,$0F,$C7,$B4       /// pfmul    mm0, mm7
      db $0F,$0F,$D7,$B4       /// pfmul    mm2, mm7
      db $0F,$0F,$E7,$B4       /// pfmul    mm4, mm7
      db $0F,$0D,$40,$40       /// prefetch [eax+64]
      add   eax, 24
      add   edx, 24
      db $0F,$0F,$C1,$9E       /// pfadd    mm0, mm1
      db $0F,$0F,$D3,$9E       /// pfadd    mm2, mm3
      db $0F,$0F,$E5,$9E       /// pfadd    mm4, mm5
      db $0F,$0D,$42,$40       /// prefetch [edx+64]
      db $0F,$7F,$03           /// movq     [ebx], mm0
      db $0F,$7F,$53,$08       /// movq     [ebx+8], mm2
      db $0F,$7F,$63,$10       /// movq     [ebx+16], mm4

      add   ebx, 24

      dec   ecx
      jnz @@Loop

      db $0F,$0E               /// femms

      pop edi
      pop ebx
   end;
   if (n and 1)=1 then
      VectorLerp(src1[n-1], src2[n-1], t, dest[n-1]);
end;

// VectorArrayLerp (affine)
//
procedure VectorArrayLerp(const src1, src2 : PAffineVectorArray; t : Single; n : Integer; dest : PAffineVectorArray);
var
   i : Integer;
begin
   if vSIMD=1 then
      VectorArrayLerp_3DNow(src1, src2, t, n, dest)
   else for i:=0 to n-1 do begin
      dest[i][0]:=src1[i][0]+(src2[i][0]-src1[i][0])*t;
      dest[i][1]:=src1[i][1]+(src2[i][1]-src1[i][1])*t;
      dest[i][2]:=src1[i][2]+(src2[i][2]-src1[i][2])*t;
   end;
end;

// VectorLength (array)
//
function VectorLength(V: array of Single): Single; assembler;
// EAX contains address of V
// EDX contains the highest index of V
// the result is returned in ST(0)
asm
         FLDZ                           // initialize sum
@@Loop:
         FLD  DWORD PTR [EAX  +  4 * EDX] // load a component
         FMUL ST, ST
         FADDP
         SUB  EDX, 1
         JNL  @@Loop
         FSQRT
end;

// VectorLength  (x, y)
//
function VectorLength(const x, y : Single) : Single; assembler;
// Result:=sqrt(x*x+y*y)
asm
         FLD X
         FMUL ST, ST
         FLD Y
         FMUL ST, ST
         FADD
         FSQRT
end;

// VectorLength (x, y, z)
//
function VectorLength(const x, y, z : Single) : Single; assembler;
// Result:=sqrt(x*x+y*y+z*z)
asm
         FLD X
         FMUL ST, ST
         FLD Y
         FMUL ST, ST
         FADD
         FLD Z
         FMUL ST, ST
         FADD
         FSQRT
end;

// VectorLength
//
function VectorLength(const v : TAffineVector) : Single; register;
// EAX contains address of V
// result is passed in ST(0)
asm
       FLD  DWORD PTR [EAX]
       FMUL ST, ST
       FLD  DWORD PTR [EAX+4]
       FMUL ST, ST
       FADDP
       FLD  DWORD PTR [EAX+8]
       FMUL ST, ST
       FADDP
       FSQRT
end;

// VectorLength
//
function VectorLength(const v : TVector) : Single; register;
// EAX contains address of V
// result is passed in ST(0)
asm
       FLD  DWORD PTR [EAX]
       FMUL ST, ST
       FLD  DWORD PTR [EAX+4]
       FMUL ST, ST
       FADDP
       FLD  DWORD PTR [EAX+8]
       FMUL ST, ST
       FADDP
       FLD  DWORD PTR [EAX+12]
       FMUL ST, ST
       FADDP
       FSQRT
end;

// VectorNorm
//
function VectorNorm(const x, y : Single) : Single;
begin
   Result:=Sqr(x)+Sqr(y);
end;

// VectorNorm (affine)
//
function VectorNorm(const v : TAffineVector) : Single; register;
// EAX contains address of V
// result is passed in ST(0)
asm
      FLD DWORD PTR [EAX];
      FMUL ST, ST
      FLD DWORD PTR [EAX+4];
      FMUL ST, ST
      FADD
      FLD DWORD PTR [EAX+8];
      FMUL ST, ST
      FADD
end;

// VectorNorm (hmg)
//
function VectorNorm(const v : TVector) : Single; register;
// EAX contains address of V
// result is passed in ST(0)
asm
      FLD DWORD PTR [EAX];
      FMUL ST, ST
      FLD DWORD PTR [EAX+4];
      FMUL ST, ST
      FADD
      FLD DWORD PTR [EAX+8];
      FMUL ST, ST
      FADD
      FLD DWORD PTR [EAX+12];
      FMUL ST, ST
      FADD
end;

// VectorNorm
//
function VectorNorm(var V: array of Single): Single; assembler; register;
// EAX contains address of V
// EDX contains highest index in V
// result is passed in ST(0)
asm
      FLDZ                           // initialize sum
@@Loop:
      FLD  DWORD PTR [EAX + 4 * EDX] // load a component
      FMUL ST, ST                    // make square
      FADDP                          // add previous calculated sum
      SUB  EDX, 1
      JNL  @@Loop
end;

// NormalizeVector (affine)
//
procedure NormalizeVector(var v : TAffineVector); register;
//   Result:=VectorLength(v);
//   ScaleVector(v, 1/Result);
asm
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
end;

// VectorNormalize
//
function VectorNormalize(const v : TAffineVector) : TAffineVector; register;
//   Result:=VectorLength(v);
//   ScaleVector(v, 1/Result);
asm
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EDX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EDX+4]
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EDX+8]
end;

// NormalizeVectorArray
//
procedure NormalizeVectorArray(list : PAffineVectorArray; n : Integer);
// EAX contains list
// EDX contains n
asm
      OR    EDX, EDX
      JZ    @@End
@@Loop:
      FLD   DWORD PTR [EAX]
      FMUL  ST, ST
      FLD   DWORD PTR [EAX+4]
      FMUL  ST, ST
      FADD
      FLD   DWORD PTR [EAX+8]
      FMUL  ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD   ST
      FMUL  DWORD PTR [EAX]
      FSTP  DWORD PTR [EAX]
      FLD   ST
      FMUL  DWORD PTR [EAX+4]
      FSTP  DWORD PTR [EAX+4]
      FMUL  DWORD PTR [EAX+8]
      FSTP  DWORD PTR [EAX+8]
      ADD   EAX, 12
      TEST  vSIMD, 1           // performance impact is negligible on ATHLON
      JZ @@FPU
      db $0F,$0D,$40,$60       /// PREFETCH    [EAX+96]
@@FPU:
      DEC   EDX
      JNZ   @@LOOP
@@End:
end;

// NormalizeVector (hmg)
//
procedure NormalizeVector(var v : TVector); register;
//   Result:=VectorLength(v);
//   ScaleVector(v, 1/Result);
asm
      FLD  DWORD PTR [EAX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+12]
      FMUL ST, ST
      FADD
      FSQRT
      FLD1
      FDIVR
      FLD  ST
      FMUL DWORD PTR [EAX]
      FSTP DWORD PTR [EAX]
      FLD  ST
      FMUL DWORD PTR [EAX+4]
      FSTP DWORD PTR [EAX+4]
      FLD ST
      FMUL DWORD PTR [EAX+8]
      FSTP DWORD PTR [EAX+8]
      FMUL DWORD PTR [EAX+12]
      FSTP DWORD PTR [EAX+12]
end;

// VectorAngle
//
function VectorAngle(const V1, V2: TAffineVector): Single; assembler;
//   Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
// EAX contains address of Vector1
// EDX contains address of Vector2
asm
      FLD DWORD PTR [EAX]           // V1[0]
      FLD ST                        // double V1[0]
      FMUL ST, ST                   // V1[0]^2 (prep. for divisor)
      FLD DWORD PTR [EDX]           // V2[0]
      FMUL ST(2), ST                // ST(2):=V1[0] * V2[0]
      FMUL ST, ST                   // V2[0]^2 (prep. for divisor)
      FLD DWORD PTR [EAX + 4]       // V1[1]
      FLD ST                        // double V1[1]
      FMUL ST, ST                   // ST(0):=V1[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0]^2 + V1[1] *  * 2
      FLD DWORD PTR [EDX + 4]       // V2[1]
      FMUL ST(1), ST                // ST(1):=V1[1] * V2[1]
      FMUL ST, ST                   // ST(0):=V2[1]^2
      FADDP ST(2), ST               // ST(1):=V2[0]^2 + V2[1]^2
      FADDP ST(3), ST               // ST(2):=V1[0] * V2[0] + V1[1] * V2[1]
      FLD DWORD PTR [EAX + 8]       // load V2[1]
      FLD ST                        // same calcs go here
      FMUL ST, ST                   // (compare above)
      FADDP ST(3), ST
      FLD DWORD PTR [EDX + 8]
      FMUL ST(1), ST
      FMUL ST, ST
      FADDP ST(2), ST
      FADDP ST(3), ST
      FMULP                         // ST(0):=(V1[0]^2 + V1[1]^2 + V1[2]) *
                                    //          (V2[0]^2 + V2[1]^2 + V2[2])
      FSQRT                         // sqrt(ST(0))
      FDIVP                         // ST(0):=Result:=ST(1) / ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
end;

// NegateVector
//
procedure NegateVector(var v : TAffineVector); register;
// EAX contains address of v
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EAX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EAX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EAX+8]
end;

// NegateVector
//
procedure NegateVector(var v : TVector); register;
// EAX contains address of v
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EAX]
      FLD DWORD PTR [EAX+4]
      FCHS
      FSTP DWORD PTR [EAX+4]
      FLD DWORD PTR [EAX+8]
      FCHS
      FSTP DWORD PTR [EAX+8]
      FLD DWORD PTR [EAX+12]
      FCHS
      FSTP DWORD PTR [EAX+12]
end;

// NegateVector
//
procedure NegateVector(V: array of Single); assembler; register;
// EAX contains address of V
// EDX contains highest index in V
asm
  {V[X]:=-V[X];
  V[Y]:=-V[Y];
  V[Z]:=-V[Z];}
@@Loop:
      FLD DWORD PTR [EAX + 4 * EDX]
      FCHS
      WAIT
      FSTP DWORD PTR [EAX + 4 * EDX]
      DEC EDX
      JNS @@Loop
end;

// ScaleVector (affine)
//
procedure ScaleVector(var v : TAffineVector; factor: Single); register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+8]
end;

// ScaleVector (hmg)
//
procedure ScaleVector(var v : TVector; factor: Single); register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+8]
      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EAX+12]
end;

// ScaleVector (affine vector)
//
procedure ScaleVector(var v : TAffineVector; const factor : TAffineVector);
begin
   v[0]:=v[0]*factor[0];
   v[1]:=v[1]*factor[1];
   v[2]:=v[2]*factor[2];
end;

// ScaleVector (hmg vector)
//
procedure ScaleVector(var v : TVector; const factor : TVector);
begin
   v[0]:=v[0]*factor[0];
   v[1]:=v[1]*factor[1];
   v[2]:=v[2]*factor[2];
   v[3]:=v[3]*factor[3];
end;

// VectorScale (affine)
//
function VectorScale(const v : TAffineVector; factor : Single) : TAffineVector; register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
end;

// VectorScale (proc, affine)
//
procedure VectorScale(const v : TAffineVector; factor : Single; var vr : TAffineVector); register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
end;

// VectorScale (hmg)
//
function VectorScale(const v : TVector; factor : Single) : TVector; register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+12]
      FSTP DWORD PTR [EDX+12]
end;

// VectorScale (proc, hmg)
//
procedure VectorScale(const v : TVector; factor : Single; var vr : TVector); register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
      FLD  DWORD PTR [EAX+12]
      FMUL DWORD PTR [EBP+12]
      FSTP DWORD PTR [EDX+12]
end;

// VectorScale (proc, hmg-affine)
//
procedure VectorScale(const v : TVector; factor : Single; var vr : TAffineVector); register;
asm
      FLD  DWORD PTR [EAX]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX]
      FLD  DWORD PTR [EAX+4]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+4]
      FLD  DWORD PTR [EAX+8]
      FMUL DWORD PTR [EBP+8]
      FSTP DWORD PTR [EDX+8]
end;

// DivideVector
//
procedure DivideVector(var v : TVector; const divider : TVector);
begin
   v[0]:=v[0]/divider[0];
   v[1]:=v[1]/divider[1];
   v[2]:=v[2]/divider[2];
   v[3]:=v[3]/divider[3];
end;

// VectorEquals (hmg vector)
//
function VectorEquals(const V1, V2: TVector) : Boolean;
// EAX contains address of v1
// EDX contains highest of v2
asm
      mov ecx, eax
      mov eax, [edx]
      cmp eax, [ecx]
      jne @@Diff
      mov eax, [edx+$4]
      cmp eax, [ecx+$4]
      jne @@Diff
      mov eax, [edx+$8]
      cmp eax, [ecx+$8]
      jne @@Diff
      mov eax, [edx+$C]
      cmp eax, [ecx+$C]
      jne @@Diff
@@Equal:
      mov al, -1
      jmp @@End
@@Diff:
      xor eax, eax
@@End:
end;

// VectorEquals (affine vector)
//
function VectorEquals(const V1, V2: TAffineVector) : Boolean; register;
// EAX contains address of v1
// EDX contains highest of v2
asm
      mov ecx, eax
      mov eax, [edx]
      cmp eax, [ecx]
      jne @@Diff
      mov eax, [edx+$4]
      cmp eax, [ecx+$4]
      jne @@Diff
      mov eax, [edx+$8]
      cmp eax, [ecx+$8]
      jne @@Diff
@@Equal:
      mov al, -1
      jmp @@End
@@Diff:
      xor eax, eax
@@End:
end;

// VectorIsNull (hmg)
//
function VectorIsNull(const v : TVector) : Boolean;
begin
   Result:=((v[0]=0) and (v[1]=0) and (v[2]=0));
end;

// VectorIsNull (affine)
//
function VectorIsNull(const v : TAffineVector) : Boolean; overload;
begin
   Result:=((v[0]=0) and (v[1]=0) and (v[2]=0));
end;

// VectorSpacing (affine)
//
function VectorSpacing(const v1, v2 : TAffineVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FABS
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FABS
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FABS
      FADD
end;

// VectorSpacing (Hmg)
//
function VectorSpacing(const v1, v2 : TVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FABS
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FABS
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FABS
      FADD
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FABS
      FADD
end;

// VectorDistance (affine)
//
function VectorDistance(const v1, v2 : TAffineVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FSQRT
end;

// VectorDistance (hmg)
//
function VectorDistance(const v1, v2 : TVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FMUL ST, ST
      FADD
      FSQRT
end;

// VectorDistance2 (affine)
//
function VectorDistance2(const v1, v2 : TAffineVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
end;

// VectorDistance2 (hmg)
//
function VectorDistance2(const v1, v2 : TVector) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+12]
      FSUB DWORD PTR [EDX+12]
      FMUL ST, ST
      FADD
end;

// VectorPerpendicular
//
function VectorPerpendicular(const V, N : TAffineVector): TAffineVector;
var
   dot : Single;
begin
   dot:=VectorDotProduct(V, N);
   Result[X]:=V[X]-Dot * N[X];
   Result[Y]:=V[Y]-Dot * N[Y];
   Result[Z]:=V[Z]-Dot * N[Z];
end;

// VectorReflect
//
function VectorReflect(const V, N: TAffineVector): TAffineVector; assembler; register;
// EAX contains address of V
// EDX contains address of N
// ECX contains address of the result
//var Dot : Single;
   {Dot:=VectorAffineDotProduct(V, N);
   Result[X]:=V[X]-2 * Dot * N[X];
   Result[Y]:=V[Y]-2 * Dot * N[Y];
   Result[Z]:=V[Z]-2 * Dot * N[Z];}
asm
      CALL VectorDotProduct   // dot is now in ST(0)
      FCHS                          // -dot
      FADD ST, ST                   // -dot * 2
      FLD DWORD PTR [EDX]           // ST:=N[X]
      FMUL ST, ST(1)                // ST:=-2 * dot * N[X]
      FADD DWORD PTR[EAX]           // ST:=V[X] - 2 * dot * N[X]
      FSTP DWORD PTR [ECX]          // store result
      FLD DWORD PTR [EDX + 4]       // etc.
      FMUL ST, ST(1)
      FADD DWORD PTR[EAX + 4]
      FSTP DWORD PTR [ECX + 4]
      FLD DWORD PTR [EDX + 8]
      FMUL ST, ST(1)
      FADD DWORD PTR[EAX + 8]
      FSTP DWORD PTR [ECX + 8]
      FSTP ST                       // clean FPU stack
end;

// RotateVector
//
procedure RotateVector(var vector : TVector; const axis : TAffineVector; angle: Single);
var
   rotMatrix : TMatrix4f;
begin
   rotMatrix:=CreateRotationMatrix(axis, Angle);
   vector:=VectorTransform(vector, rotMatrix);
end;

// RotateVector
//
procedure RotateVector(var vector : TVector; const axis : TVector; angle : Single); overload;
var
   rotMatrix : TMatrix4f;
begin
   rotMatrix:=CreateRotationMatrix(PAffineVector(@axis)^, Angle);
   vector:=VectorTransform(vector, rotMatrix);
end;

// RotateVectorAroundY
//
procedure RotateVectorAroundY(var v : TAffineVector; alpha : Single);
var
   c, s, v0 : Single;
begin
   SinCos(alpha, s, c);
   v0:=v[0];
   v[0]:=c*v0+s*v[2];
   v[2]:=c*v[2]-s*v0;
end;

// VectorRotateAroundY (func)
//
function VectorRotateAroundY(const v : TAffineVector; alpha : Single) : TAffineVector;
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   Result[1]:=v[1];
   Result[0]:=c*v[0]+s*v[2];
   Result[2]:=c*v[2]-s*v[0];
end;

// VectorRotateAroundY (proc)
//
procedure VectorRotateAroundY(const v : TAffineVector; alpha : Single; var vr : TAffineVector);
var
   c, s : Single;
begin
   SinCos(alpha, s, c);
   vr[1]:=v[1];
   vr[0]:=c*v[0]+s*v[2];
   vr[2]:=c*v[2]-s*v[0];
end;

// AbsVector (hmg)
//
procedure AbsVector(var v : TVector);
begin
  v[0]:=Abs(v[0]);
  v[1]:=Abs(v[1]);
  v[2]:=Abs(v[2]);
  v[3]:=Abs(v[3]);
end;

// AbsVector (affine)
//
procedure AbsVector(var v : TAffineVector);
begin
  v[0]:=Abs(v[0]);
  v[1]:=Abs(v[1]);
  v[2]:=Abs(v[2]);
end;

// SetMatrix (single->double)
//
procedure SetMatrix(var dest : THomogeneousDblMatrix; const src : TMatrix);
var
   i : Integer;
begin
   for i:=X to W do begin
      dest[i, X]:=src[i, X];
      dest[i, Y]:=src[i, Y];
      dest[i, Z]:=src[i, Z];
      dest[i, W]:=src[i, W];
   end;
end;

// CreateScaleMatrix (affine)
//
function CreateScaleMatrix(const V: TAffineVector): TMatrix; register;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=V[X];
   Result[Y, Y]:=V[Y];
   Result[Z, Z]:=V[Z];
end;

// CreateScaleMatrix (Hmg)
//
function CreateScaleMatrix(const V: TVector): TMatrix; register;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=V[X];
   Result[Y, Y]:=V[Y];
   Result[Z, Z]:=V[Z];
end;

// CreateTranslationMatrix (affine)
//
function CreateTranslationMatrix(const V: TAffineVector): TMatrix; register;
begin
   Result:=IdentityHmgMatrix;
   Result[W, X]:=V[X];
   Result[W, Y]:=V[Y];
   Result[W, Z]:=V[Z];
end;

// CreateTranslationMatrix (hmg)
//
function CreateTranslationMatrix(const V: TVector): TMatrix; register;
begin
   Result:=IdentityHmgMatrix;
   Result[W, X]:=V[X];
   Result[W, Y]:=V[Y];
   Result[W, Z]:=V[Z];
end;

// CreateScaleAndTranslationMatrix
//
function CreateScaleAndTranslationMatrix(const scale, offset : TVector): TMatrix; register;
begin
   Result:=IdentityHmgMatrix;
   Result[X, X]:=scale[X];   Result[W, X]:=offset[X];
   Result[Y, Y]:=scale[Y];   Result[W, Y]:=offset[Y];
   Result[Z, Z]:=scale[Z];   Result[W, Z]:=offset[Z];
end;

// CreateRotationMatrixX
//
function CreateRotationMatrixX(const Sine, Cosine: Single) : TMatrix; register;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=1;
   Result[Y, Y]:=Cosine;
   Result[Y, Z]:=Sine;
   Result[Z, Y]:=-Sine;
   Result[Z, Z]:=Cosine;
   Result[W, W]:=1;
end;

// CreateRotationMatrixY
//
function CreateRotationMatrixY(const Sine, Cosine: Single): TMatrix; register;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=Cosine;
   Result[X, Z]:=-Sine;
   Result[Y, Y]:=1;
   Result[Z, X]:=Sine;
   Result[Z, Z]:=Cosine;
   Result[W, W]:=1;
end;

// CreateRotationMatrixZ
//
function CreateRotationMatrixZ(const Sine, Cosine: Single): TMatrix; register;
begin
   Result:=EmptyHmgMatrix;
   Result[X, X]:=Cosine;
   Result[X, Y]:=Sine;
   Result[Y, X]:=-Sine;
   Result[Y, Y]:=Cosine;
   Result[Z, Z]:=1;
   Result[W, W]:=1;
end;

// CreateRotationMatrix
//
function CreateRotationMatrix(Axis: TAffineVector; Angle: Single): TMatrix; register;
var
   cosine, sine, one_minus_cosine : Single;
begin
   SinCos(Angle, Sine, Cosine);
   one_minus_cosine:=1 - cosine;
   NormalizeVector(Axis);

   Result[X, X]:=(one_minus_cosine * Sqr(Axis[0])) + Cosine;
   Result[X, Y]:=(one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * Sine);
   Result[X, Z]:=(one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * Sine);
   Result[X, W]:=0;

   Result[Y, X]:=(one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * Sine);
   Result[Y, Y]:=(one_minus_cosine * Sqr(Axis[1])) + Cosine;
   Result[Y, Z]:=(one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * Sine);
   Result[Y, W]:=0;

   Result[Z, X]:=(one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * Sine);
   Result[Z, Y]:=(one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * Sine);
   Result[Z, Z]:=(one_minus_cosine * Sqr(Axis[2])) + Cosine;
   Result[Z, W]:=0;

   Result[W, X]:=0;
   Result[W, Y]:=0;
   Result[W, Z]:=0;
   Result[W, W]:=1;
end;

// CreateAffineRotationMatrix
//
function CreateAffineRotationMatrix(Axis: TAffineVector; Angle: Single): TAffineMatrix;
var
   cosine, sine, one_minus_cosine : Single;
begin
   SinCos(Angle, Sine, Cosine);
   one_minus_cosine:=1 - cosine;
   NormalizeVector(Axis);

   Result[X, X]:=(one_minus_cosine * Sqr(Axis[0])) + Cosine;
   Result[X, Y]:=(one_minus_cosine * Axis[0] * Axis[1]) - (Axis[2] * Sine);
   Result[X, Z]:=(one_minus_cosine * Axis[2] * Axis[0]) + (Axis[1] * Sine);

   Result[Y, X]:=(one_minus_cosine * Axis[0] * Axis[1]) + (Axis[2] * Sine);
   Result[Y, Y]:=(one_minus_cosine * Sqr(Axis[1])) + Cosine;
   Result[Y, Z]:=(one_minus_cosine * Axis[1] * Axis[2]) - (Axis[0] * Sine);

   Result[Z, X]:=(one_minus_cosine * Axis[2] * Axis[0]) - (Axis[1] * Sine);
   Result[Z, Y]:=(one_minus_cosine * Axis[1] * Axis[2]) + (Axis[0] * Sine);
   Result[Z, Z]:=(one_minus_cosine * Sqr(Axis[2])) + Cosine;
end;

// MatrixMultiply (3x3 func)
//
function MatrixMultiply(const M1, M2 : TAffineMatrix) : TAffineMatrix; register;
begin
   if vSIMD=1 then begin
      asm
         db $0F,$0E               /// femms
         xchg eax, ecx

         db $0F,$6E,$7A,$08       /// movd        mm7,[edx+8]
         db $0F,$6E,$72,$20       /// movd        mm6,[edx+32]
         db $0F,$62,$7A,$14       /// punpckldq   mm7,[edx+20]
         db $0F,$6F,$01           /// movq        mm0,[ecx]
         db $0F,$6E,$59,$08       /// movd        mm3,[ecx+8]
         db $0F,$6F,$C8           /// movq        mm1,mm0
         db $0F,$0F,$C7,$B4       /// pfmul       mm0,mm7
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
         db $0F,$6F,$E3           /// movq        mm4,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$E6,$B4       /// pfmul       mm4,mm6
         db $0F,$6F,$69,$0C       /// movq        mm5,[ecx+12]
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         db $0F,$6E,$59,$14       /// movd        mm3,[ecx+20]
         db $0F,$0F,$E0,$9E       /// pfadd       mm4,mm0
         db $0F,$6F,$CD           /// movq        mm1,mm5
         db $0F,$7F,$10           /// movq        [eax],mm2
         db $0F,$0F,$EF,$B4       /// pfmul       mm5,mm7
         db $0F,$7E,$60,$08       /// movd        [eax+8],mm4
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$6F,$41,$18       /// movq        mm0,[ecx+24]
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$ED,$AE       /// pfacc       mm5,mm5
         db $0F,$6F,$E3           /// movq        mm4,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$E6,$B4       /// pfmul       mm4,mm6
         db $0F,$6F,$C8           /// movq        mm1,mm0
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         db $0F,$6E,$59,$20       /// movd        mm3,[ecx+32]
         db $0F,$0F,$E5,$9E       /// pfadd       mm4,mm5
         db $0F,$0F,$C7,$B4       /// pfmul       mm0,mm7
         db $0F,$7F,$50,$0C       /// movq        [eax+12],mm2
         db $0F,$6F,$D1           /// movq        mm2,mm1
         db $0F,$7E,$60,$14       /// movd        [eax+20],mm4
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$0A,$B4       /// pfmul       mm1,[edx]
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$52,$0C,$B4   /// pfmul       mm2,[edx+12]
         db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
         db $0F,$0F,$F3,$B4       /// pfmul       mm6,mm3
         db $0F,$62,$DB           /// punpckldq   mm3,mm3
         db $0F,$0F,$5A,$18,$B4   /// pfmul       mm3,[edx+24]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$F0,$9E       /// pfadd       mm6,mm0
         db $0F,$0F,$D3,$9E       /// pfadd       mm2,mm3
         
         db $0F,$7E,$70,$20       /// movd        [eax+32],mm6
         db $0F,$7F,$50,$18       /// movq        [eax+24],mm2
         db $0F,$0E               /// femms
      end;
   end else begin
      Result[X, X]:= M1[X, X]*M2[X, X]+M1[X, Y]*M2[Y, X]+M1[X, Z]*M2[Z, X];
      Result[X, Y]:= M1[X, X]*M2[X, Y]+M1[X, Y]*M2[Y, Y]+M1[X, Z]*M2[Z, Y];
      Result[X, Z]:= M1[X, X]*M2[X, Z]+M1[X, Y]*M2[Y, Z]+M1[X, Z]*M2[Z, Z];
      Result[Y, X]:= M1[Y, X]*M2[X, X]+M1[Y, Y]*M2[Y, X]+M1[Y, Z]*M2[Z, X];
      Result[Y, Y]:= M1[Y, X]*M2[X, Y]+M1[Y, Y]*M2[Y, Y]+M1[Y, Z]*M2[Z, Y];
      Result[Y, Z]:= M1[Y, X]*M2[X, Z]+M1[Y, Y]*M2[Y, Z]+M1[Y, Z]*M2[Z, Z];
      Result[Z, X]:= M1[Z, X]*M2[X, X]+M1[Z, Y]*M2[Y, X]+M1[Z, Z]*M2[Z, X];
      Result[Z, Y]:= M1[Z, X]*M2[X, Y]+M1[Z, Y]*M2[Y, Y]+M1[Z, Z]*M2[Z, Y];
      Result[Z, Z]:= M1[Z, X]*M2[X, Z]+M1[Z, Y]*M2[Y, Z]+M1[Z, Z]*M2[Z, Z];
   end;
end;

// MatrixMultiply (4x4, func)
//
function MatrixMultiply(const M1, M2: TMatrix): TMatrix; register;
begin
   if vSIMD=1 then begin
      asm
         db $0F,$0E               /// femms
         xchg eax, ecx

         db $0F,$6F,$01           /// movq        mm0,[ecx]
         db $0F,$6F,$49,$08       /// movq        mm1,[ecx+8]
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$10       /// movq        mm0,[ecx+16]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$18       /// movq        mm1,[ecx+24]
         db $0F,$7F,$38           /// movq        [eax],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$08       /// movq        [eax+8],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$20       /// movq        mm0,[ecx+32]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$28       /// movq        mm1,[ecx+40]
         db $0F,$7F,$78,$10       /// movq        [eax+16],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$18       /// movq        [eax+24],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$6F,$41,$30       /// movq        mm0,[ecx+48]
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$6F,$49,$38       /// movq        mm1,[ecx+56]
         db $0F,$7F,$78,$20       /// movq        [eax+32],mm7
         db $0F,$6F,$22           /// movq        mm4,[edx]
         db $0F,$7F,$58,$28       /// movq        [eax+40],mm3

         db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
         db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
         db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
         db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
         db $0F,$62,$C0           /// punpckldq   mm0,mm0
         db $0F,$62,$C9           /// punpckldq   mm1,mm1
         db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
         db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
         db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
         db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
         db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
         db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
         db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
         db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
         db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
         db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
         db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
         db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
         db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
         db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
         db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
         db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
         db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
         db $0F,$7F,$78,$30       /// movq        [eax+48],mm7
         db $0F,$7F,$58,$38       /// movq        [eax+56],mm3
         db $0F,$0E               /// femms
      end;
   end else begin
      Result[X,X]:=M1[X,X]*M2[X,X]+M1[X,Y]*M2[Y,X]+M1[X,Z]*M2[Z,X]+M1[X,W]*M2[W,X];
      Result[X,Y]:=M1[X,X]*M2[X,Y]+M1[X,Y]*M2[Y,Y]+M1[X,Z]*M2[Z,Y]+M1[X,W]*M2[W,Y];
      Result[X,Z]:=M1[X,X]*M2[X,Z]+M1[X,Y]*M2[Y,Z]+M1[X,Z]*M2[Z,Z]+M1[X,W]*M2[W,Z];
      Result[X,W]:=M1[X,X]*M2[X,W]+M1[X,Y]*M2[Y,W]+M1[X,Z]*M2[Z,W]+M1[X,W]*M2[W,W];
      Result[Y,X]:=M1[Y,X]*M2[X,X]+M1[Y,Y]*M2[Y,X]+M1[Y,Z]*M2[Z,X]+M1[Y,W]*M2[W,X];
      Result[Y,Y]:=M1[Y,X]*M2[X,Y]+M1[Y,Y]*M2[Y,Y]+M1[Y,Z]*M2[Z,Y]+M1[Y,W]*M2[W,Y];
      Result[Y,Z]:=M1[Y,X]*M2[X,Z]+M1[Y,Y]*M2[Y,Z]+M1[Y,Z]*M2[Z,Z]+M1[Y,W]*M2[W,Z];
      Result[Y,W]:=M1[Y,X]*M2[X,W]+M1[Y,Y]*M2[Y,W]+M1[Y,Z]*M2[Z,W]+M1[Y,W]*M2[W,W];
      Result[Z,X]:=M1[Z,X]*M2[X,X]+M1[Z,Y]*M2[Y,X]+M1[Z,Z]*M2[Z,X]+M1[Z,W]*M2[W,X];
      Result[Z,Y]:=M1[Z,X]*M2[X,Y]+M1[Z,Y]*M2[Y,Y]+M1[Z,Z]*M2[Z,Y]+M1[Z,W]*M2[W,Y];
      Result[Z,Z]:=M1[Z,X]*M2[X,Z]+M1[Z,Y]*M2[Y,Z]+M1[Z,Z]*M2[Z,Z]+M1[Z,W]*M2[W,Z];
      Result[Z,W]:=M1[Z,X]*M2[X,W]+M1[Z,Y]*M2[Y,W]+M1[Z,Z]*M2[Z,W]+M1[Z,W]*M2[W,W];
      Result[W,X]:=M1[W,X]*M2[X,X]+M1[W,Y]*M2[Y,X]+M1[W,Z]*M2[Z,X]+M1[W,W]*M2[W,X];
      Result[W,Y]:=M1[W,X]*M2[X,Y]+M1[W,Y]*M2[Y,Y]+M1[W,Z]*M2[Z,Y]+M1[W,W]*M2[W,Y];
      Result[W,Z]:=M1[W,X]*M2[X,Z]+M1[W,Y]*M2[Y,Z]+M1[W,Z]*M2[Z,Z]+M1[W,W]*M2[W,Z];
      Result[W,W]:=M1[W,X]*M2[X,W]+M1[W,Y]*M2[Y,W]+M1[W,Z]*M2[Z,W]+M1[W,W]*M2[W,W];
   end;
end;

// MatrixMultiply (4x4, proc)
//
procedure MatrixMultiply(const M1, M2: TMatrix; var MResult: TMatrix); register;
begin
   MResult:=MatrixMultiply(M1, M2);
end;

// VectorTransform
//
function VectorTransform(const V: TVector; const M: TMatrix) : TVector; register;
begin
   if vSIMD=1 then begin
      asm
        db $0F,$0E               /// femms

        db $0F,$6F,$00           /// movq        mm0,[eax]
        db $0F,$6F,$48,$08       /// movq        mm1,[eax+8]
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2

        db $0F,$7F,$39           /// movq        [ecx],mm7
        db $0F,$7F,$59,$08       /// movq        [ecx+8],mm3
        db $0F,$0E               /// femms
      end
   end else begin
      Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + V[W] * M[W, X];
      Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + V[W] * M[W, Y];
      Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + V[W] * M[W, Z];
      Result[W]:=V[X] * M[X, W] + V[Y] * M[Y, W] + V[Z] * M[Z, W] + V[W] * M[W, W];
   end;
end;

// VectorTransform
//
function VectorTransform(const V: TVector; const M: TAffineMatrix): TVector; overload;
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
   Result[W]:=V[W];
end;

// VectorTransform
//
function VectorTransform(const V: TAffineVector; const M: TMatrix): TAffineVector; register;
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
end;

// VectorTransform
//
function VectorTransform(const V: TAffineVector; const M: TAffineMatrix): TAffineVector; register;
begin
   if vSIMD=1 then begin
      asm
        db $0F,$0E               /// femms

        db $0F,$6F,$00           /// movq        mm0,[eax]
        db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
        db $0F,$6E,$62,$08       /// movd        mm4,[edx+8]
        db $0F,$6F,$D8           /// movq        mm3,mm0
        db $0F,$6E,$52,$20       /// movd        mm2,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$62,$14       /// punpckldq   mm4,[edx+20]
        db $0F,$0F,$02,$B4       /// pfmul       mm0,[edx]
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$D1,$B4       /// pfmul       mm2,mm1
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$20,$B4       /// pfmul       mm4,[eax]
        db $0F,$0F,$5A,$0C,$B4   /// pfmul       mm3,[edx+12]
        db $0F,$0F,$4A,$18,$B4   /// pfmul       mm1,[edx+24]
        db $0F,$0F,$E4,$AE       /// pfacc       mm4,mm4
        db $0F,$0F,$D8,$9E       /// pfadd       mm3,mm0
        db $0F,$0F,$E2,$9E       /// pfadd       mm4,mm2
        db $0F,$0F,$D9,$9E       /// pfadd       mm3,mm1

        db $0F,$7E,$61,$08       /// movd        [ecx+8],mm4
        db $0F,$7F,$19           /// movq        [ecx],mm3
        db $0F,$0E               /// femms
      end;
   end else begin
      Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X];
      Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y];
      Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z];
   end;
end;

// MatrixDeterminant (affine)
//
function MatrixDeterminant(const M: TAffineMatrix): Single; register;
begin
  Result:=  M[X, X] * (M[Y, Y] * M[Z, Z] - M[Z, Y] * M[Y, Z])
          - M[X, Y] * (M[Y, X] * M[Z, Z] - M[Z, X] * M[Y, Z])
          + M[X, Z] * (M[Y, X] * M[Z, Y] - M[Z, X] * M[Y, Y]);
end;

// MatrixDetInternal
//
function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single; register;
// internal version for the determinant of a 3x3 matrix
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;

// MatrixDeterminant (hmg)
//
function MatrixDeterminant(const M: TMatrix): Single; register;
begin
  Result:= M[X, X]*MatrixDetInternal(M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          -M[X, Y]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          +M[X, Z]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, W], M[Z, W], M[W, W])
          -M[X, W]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z]);
end;

// AdjointMatrix
//
procedure AdjointMatrix(var M : TMatrix); register;
var
   a1, a2, a3, a4,
   b1, b2, b3, b4,
   c1, c2, c3, c4,
   d1, d2, d3, d4: Single;
begin
    a1:= M[X, X]; b1:= M[X, Y];
    c1:= M[X, Z]; d1:= M[X, W];
    a2:= M[Y, X]; b2:= M[Y, Y];
    c2:= M[Y, Z]; d2:= M[Y, W];
    a3:= M[Z, X]; b3:= M[Z, Y];
    c3:= M[Z, Z]; d3:= M[Z, W];
    a4:= M[W, X]; b4:= M[W, Y];
    c4:= M[W, Z]; d4:= M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X]:= MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X]:=-MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X]:= MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X]:=-MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y]:=-MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y]:= MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y]:=-MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y]:= MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z]:= MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z]:=-MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z]:= MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z]:=-MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W]:=-MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W]:= MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W]:=-MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W]:= MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

// ScaleMatrix (affine)
//
procedure ScaleMatrix(var M : TAffineMatrix; const factor : Single); register;
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      M[I, 0]:=M[I, 0] * Factor;
      M[I, 1]:=M[I, 1] * Factor;
      M[I, 2]:=M[I, 2] * Factor;
   end;
end;

// ScaleMatrix (hmg)
//
procedure ScaleMatrix(var M : TMatrix; const factor : Single); register;
var
   i : Integer;
begin
   for i:=0 to 3 do begin
      M[I, 0]:=M[I, 0] * Factor;
      M[I, 1]:=M[I, 1] * Factor;
      M[I, 2]:=M[I, 2] * Factor;
      M[I, 3]:=M[I, 3] * Factor;
   end;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TAffineMatrix); register;
var
   f : Single;
begin
   f:=M[0, 1]; M[0, 1]:=M[1, 0]; M[1, 0]:=f;
   f:=M[0, 2]; M[0, 2]:=M[2, 0]; M[2, 0]:=f;
   f:=M[1, 2]; M[1, 2]:=M[2, 1]; M[2, 1]:=f;
end;

// TransposeMatrix
//
procedure TransposeMatrix(var M: TMatrix); register;
var
   f : Single;
begin
   f:=M[0, 1]; M[0, 1]:=M[1, 0]; M[1, 0]:=f;
   f:=M[0, 2]; M[0, 2]:=M[2, 0]; M[2, 0]:=f;
   f:=M[0, 3]; M[0, 3]:=M[3, 0]; M[3, 0]:=f;
   f:=M[1, 2]; M[1, 2]:=M[2, 1]; M[2, 1]:=f;
   f:=M[1, 3]; M[1, 3]:=M[3, 1]; M[3, 1]:=f;
   f:=M[2, 3]; M[2, 3]:=M[3, 2]; M[3, 2]:=f;
end;

// InvertMatrix
//
procedure InvertMatrix(var M: TMatrix); register;
var
   det : Single;
begin
   det:=MatrixDeterminant(M);
   if Abs(Det) < EPSILON then
      M:=IdentityHmgMatrix
   else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1 / det);
   end;
end;

// MatrixDecompose
//
function MatrixDecompose(const M: TMatrix; var Tran: TTransformations): Boolean; register;
var
   I, J: Integer;
   LocMat, pmat, invpmat, tinvpmat: TMatrix;
   prhs, psol: TVector;
   row0, row1, row2 : TAffineVector;
   f : Single;
begin
  Result:=False;
  locmat:=M;
  // normalize the matrix
  if locmat[W, W] = 0 then Exit;
  for I:=0 to 3 do
    for J:=0 to 3 do
      locmat[I, J]:=locmat[I, J] / locmat[W, W];

  // pmat is used to solve for perspective, but it also provides
  // an easy way to test for singularity of the upper 3x3 component.

  pmat:=locmat;
  for I:=0 to 2 do pmat[I, W]:=0;
  pmat[W, W]:=1;

  if MatrixDeterminant(pmat) = 0 then Exit;

  // First, isolate perspective.  This is the messiest.
  if (locmat[X, W] <> 0) or (locmat[Y, W] <> 0) or (locmat[Z, W] <> 0) then begin
    // prhs is the right hand side of the equation.
    prhs[X]:=locmat[X, W];
    prhs[Y]:=locmat[Y, W];
    prhs[Z]:=locmat[Z, W];
    prhs[W]:=locmat[W, W];

    // Solve the equation by inverting pmat and multiplying
    // prhs by the inverse.  (This is the easiest way, not
    // necessarily the best.)

    invpmat:=pmat;
    InvertMatrix(invpmat);
    TransposeMatrix(invpmat);
    psol:=VectorTransform(prhs, tinvpmat);

    // stuff the answer away
    Tran[ttPerspectiveX]:=psol[X];
    Tran[ttPerspectiveY]:=psol[Y];
    Tran[ttPerspectiveZ]:=psol[Z];
    Tran[ttPerspectiveW]:=psol[W];

    // clear the perspective partition
    locmat[X, W]:=0;
    locmat[Y, W]:=0;
    locmat[Z, W]:=0;
    locmat[W, W]:=1;
  end else begin
    // no perspective
    Tran[ttPerspectiveX]:=0;
    Tran[ttPerspectiveY]:=0;
    Tran[ttPerspectiveZ]:=0;
    Tran[ttPerspectiveW]:=0;
  end;

  // next take care of translation (easy)
  for I:=0 to 2 do begin
    Tran[TTransType(Ord(ttTranslateX) + I)]:=locmat[W, I];
    locmat[W, I]:=0;
  end;

  // now get scale and shear
  SetVector(row0, locmat[0]);
  SetVector(row1, locmat[1]);
  SetVector(row2, locmat[2]);

  // compute X scale factor and normalize first row
  Tran[ttScaleX]:=VectorNorm(row0);
  VectorScale(row0, 1/sqrt(Tran[ttScaleX]));

  // compute XY shear factor and make 2nd row orthogonal to 1st
  Tran[ttShearXY]:=VectorDotProduct(row0, row1);
  f:=-Tran[ttShearXY];
  CombineVector(row1, row0, f);

  // now, compute Y scale and normalize 2nd row
  Tran[ttScaleY]:=VectorNorm(row1);
  VectorScale(row1, 1/sqrt(Tran[ttScaleY]));
  Tran[ttShearXY]:=Tran[ttShearXY]/Tran[ttScaleY];

  // compute XZ and YZ shears, orthogonalize 3rd row
  Tran[ttShearXZ]:=VectorDotProduct(row0, row2);
  f:=-Tran[ttShearXZ];
  CombineVector(row2, row0, f);
  Tran[ttShearYZ]:=VectorDotProduct(row1, row2);
  f:=-Tran[ttShearYZ];
  CombineVector(row2, row1, f);

  // next, get Z scale and normalize 3rd row
  Tran[ttScaleZ]:=VectorNorm(row2);
  VectorScale(row2, 1/sqrt(Tran[ttScaleZ]));
  Tran[ttShearXZ]:=Tran[ttShearXZ] / tran[ttScaleZ];
  Tran[ttShearYZ]:=Tran[ttShearYZ] / Tran[ttScaleZ];

  // At this point, the matrix (in rows[]) is orthonormal.
  // Check for a coordinate system flip.  If the determinant
  // is -1, then negate the matrix and the scaling factors.
  if VectorDotProduct(row0, VectorCrossProduct(row1, row2)) < 0 then begin
    for I:=0 to 2 do
      Tran[TTransType(Ord(ttScaleX) + I)]:=-Tran[TTransType(Ord(ttScaleX) + I)];
    NegateVector(row0);
    NegateVector(row1);
    NegateVector(row2);
  end;

  // now, get the rotations out, as described in the gem
  Tran[ttRotateY]:=arcsin(-row0[Z]);
  if cos(Tran[ttRotateY]) <> 0 then begin
    Tran[ttRotateX]:=arctan2(row1[Z], row2[Z]);
    Tran[ttRotateZ]:=arctan2(row0[Y], row0[X]);
  end else begin
    tran[ttRotateX]:=arctan2(row1[X], row1[Y]);
    tran[ttRotateZ]:=0;
  end;
  // All done!
  Result:=True;
end;

// CalcPlaneNormal (func, affine)
//
function CalcPlaneNormal(const p1, p2, p3 : TAffineVector) : TAffineVector;
var
   v1, v2 : TAffineVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, Result);
   NormalizeVector(Result);
end;

// CalcPlaneNormal (proc, affine)
//
procedure CalcPlaneNormal(const p1, p2, p3 : TAffineVector; var vr : TAffineVector);
var
   v1, v2 : TAffineVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, vr);
   NormalizeVector(vr);
end;

// CalcPlaneNormal (proc, hmg)
//
procedure CalcPlaneNormal(const p1, p2, p3 : TVector; var vr : TAffineVector); overload;
var
   v1, v2 : TVector;
begin
   VectorSubtract(p2, p1, v1);
   VectorSubtract(p3, p1, v2);
   VectorCrossProduct(v1, v2, vr);
   NormalizeVector(vr);
end;

// PlaneMake
//
function PlaneMake(const p1, p2, p3 : TAffineVector) : THmgPlane;
begin
   CalcPlaneNormal(p1, p2, p3, PAffineVector(@Result)^);
   Result[3]:=-VectorDotProduct(p1, PAffineVector(@Result)^);
end;

// PlaneEvaluatePoint
//
function PlaneEvaluatePoint(const plane : THmgPlane; const point : TAffineVector) : Single;
// EAX contains address of plane
// EDX contains address of point
// result is stored in ST(0)
asm
      FLD DWORD PTR [EAX]
      FMUL DWORD PTR [EDX]
      FLD DWORD PTR [EAX + 4]
      FMUL DWORD PTR [EDX + 4]
      FADDP
      FLD DWORD PTR [EAX + 8]
      FMUL DWORD PTR [EDX + 8]
      FADDP
      FLD DWORD PTR [EAX + 12]
      FADDP
end;

// QuaternionMake
//
function QuaternionMake(Imag: array of Single; Real: Single): TQuaternion; assembler;
// EAX contains address of Imag
// ECX contains address to result vector
// EDX contains highest index of Imag
// Real part is passed on the stack
asm
      PUSH EDI
      PUSH ESI
      MOV EDI, ECX
      MOV ESI, EAX
      MOV ECX, EDX
      INC ECX
      REP MOVSD
      MOV EAX, [Real]
      MOV [EDI], EAX
      POP ESI
      POP EDI
end;

// QuaternionConjugate
//
function QuaternionConjugate(const Q: TQuaternion): TQuaternion; assembler;
// EAX contains address of Q
// EDX contains address of result
asm
      FLD DWORD PTR [EAX]
      FCHS
      FSTP DWORD PTR [EDX]
      FLD DWORD PTR [EAX + 4]
      FCHS
      FSTP DWORD PTR [EDX + 4]
      FLD DWORD PTR [EAX + 8]
      FCHS
      FSTP DWORD PTR [EDX + 8]
      MOV EAX, [EAX + 12]
      MOV [EDX + 12], EAX
end;

// QuaternionFromPoints
//
function QuaternionFromPoints(const V1, V2: TAffineVector): TQuaternion; assembler;
// EAX contains address of V1
// ECX contains address to result
// EDX contains address of V2
begin
   Result.ImagPart:=VectorCrossProduct(V1, V2);
   Result.RealPart:=Sqrt((VectorDotProduct(V1, V2) + 1)/2);
end;

// QuaternionMultiply
//
function QuaternionMultiply(const qL, qR: TQuaternion): TQuaternion;
var
   Temp : TQuaternion;
begin
  Temp.RealPart:=qL.RealPart * qR.RealPart - qL.ImagPart[X] * qR.ImagPart[X] -
                   qL.ImagPart[Y] * qR.ImagPart[Y] - qL.ImagPart[Z] * qR.ImagPart[Z];
  Temp.ImagPart[X]:=qL.RealPart * qR.ImagPart[X] + qL.ImagPart[X] * qR.RealPart +
                      qL.ImagPart[Y] * qR.ImagPart[Z] - qL.ImagPart[Z] * qR.ImagPart[Y];
  Temp.ImagPart[Y]:=qL.RealPart * qR.ImagPart[Y] + qL.ImagPart[Y] * qR.RealPart +
                      qL.ImagPart[Z] * qR.ImagPart[X] - qL.ImagPart[X] * qR.ImagPart[Z];
  Temp.ImagPart[Z]:=qL.RealPart * qR.ImagPart[Z] + qL.ImagPart[Z] * qR.RealPart +
                      qL.ImagPart[X] * qR.ImagPart[Y] - qL.ImagPart[Y] * qR.ImagPart[X];
  Result:=Temp;
end;

// QuaternionToMatrix
//
function QuaternionToMatrix(const Q: TQuaternion): TMatrix;

// Essentially, this function is the same as CreateRotationMatrix and you can consider it as
// being for reference here.

{var Norm, S,
    XS, YS, ZS,
    WX, WY, WZ,
    XX, XY, XZ,
    YY, YZ, ZZ   : Single;

begin
  Norm:=Q.Vector[X] * Q.Vector[X] + Q.Vector[Y] * Q.Vector[Y] + Q.Vector[Z] * Q.Vector[Z] + Q.RealPart * Q.RealPart;
  if Norm > 0 then S:=2 / Norm
              else S:=0;

  XS:=Q.Vector[X] * S;   YS:=Q.Vector[Y] * S;   ZS:=Q.Vector[Z] * S;
  WX:=Q.RealPart * XS;   WY:=Q.RealPart * YS;   WZ:=Q.RealPart * ZS;
  XX:=Q.Vector[X] * XS;  XY:=Q.Vector[X] * YS;  XZ:=Q.Vector[X] * ZS;
  YY:=Q.Vector[Y] * YS;  YZ:=Q.Vector[Y] * ZS;  ZZ:=Q.Vector[Z] * ZS;

  Result[X, X]:=1 - (YY + ZZ); Result[Y, X]:=XY + WZ;       Result[Z, X]:=XZ - WY;       Result[W, X]:=0;
  Result[X, Y]:=XY - WZ;       Result[Y, Y]:=1 - (XX + ZZ); Result[Z, Y]:=YZ + WX;       Result[W, Y]:=0;
  Result[X, Z]:=XZ + WY;       Result[Y, Z]:=YZ - WX;       Result[Z, Z]:=1 - (XX + YY); Result[W, Z]:=0;
  Result[X, W]:=0;             Result[Y, W]:=0;             Result[Z, W]:=0;             Result[W, W]:=1;}

var
  V: TAffineVector;
  SinA, CosA,
  A, B, C: Extended;

begin
  V:=Q.ImagPart;
  NormalizeVector(V);
  SinCos(Q.RealPart / 2, SinA, CosA);
  A:=V[X] * SinA;
  B:=V[Y] * SinA;
  C:=V[Z] * SinA;

  Result:=IdentityHmgMatrix;
  Result[X, X]:=1 - 2 * B * B - 2 * C * C;
  Result[X, Y]:=2 * A * B - 2 * CosA * C;
  Result[X, Z]:=2 * A * C + 2 * CosA * B;

  Result[Y, X]:=2 * A * B + 2 * CosA * C;
  Result[Y, Y]:=1 - 2 * A * A - 2 * C * C;
  Result[Y, Z]:=2 * B * C - 2 * CosA * A;

  Result[Z, X]:=2 * A * C - 2 * CosA * B;
  Result[Z, Y]:=2 * B * C + 2 * CosA * A;
  Result[Z, Z]:=1 - 2 * A * A - 2 * B * B;
end;

// QuaternionToPoints
//
procedure QuaternionToPoints(const Q: TQuaternion; var ArcFrom, ArcTo: TAffineVector); register;
var
   s : Single;
begin
   S:=Sqrt(Q.ImagPart[X] * Q.ImagPart[X] + Q.ImagPart[Y] * Q.ImagPart[Y]);
   if S = 0 then
      SetAffineVector(ArcFrom, 0, 1, 0)
   else SetAffineVector(ArcFrom, -Q.ImagPart[Y] / S, Q.ImagPart[X] / S, 0);
   ArcTo[X]:=Q.RealPart * ArcFrom[X] - Q.ImagPart[Z] * ArcFrom[Y];
   ArcTo[Y]:=Q.RealPart * ArcFrom[Y] + Q.ImagPart[Z] * ArcFrom[X];
   ArcTo[Z]:=Q.ImagPart[X] * ArcFrom[Y] - Q.ImagPart[Y] * ArcFrom[X];
   if Q.RealPart < 0 then
      SetAffineVector(ArcFrom, -ArcFrom[X], -ArcFrom[Y], 0);
end;

// LnXP1
//
function LnXP1(X: Extended): Extended;
asm
        FLDLN2
        MOV     AX,WORD PTR X+8               { exponent }
        FLD     X
        CMP     AX,$3FFD                      { .4225 }
        JB      @@1
        FLD1
        FADD
        FYL2X
        JMP     @@2
@@1:
        FYL2XP1
@@2:
        FWAIT
end;

// Log10
//
function Log10(X: Extended): Extended;
// Log.10(X):=Log.2(X) * Log.10(2)
asm
        FLDLG2     { Log base ten of 2 }
        FLD     X
        FYL2X
end;

// Log2
//
function Log2(X: Extended): Extended;
asm
        FLD1
        FLD     X
        FYL2X
end;

// Log2
//
function Log2(X: Single): Single;
asm
        FLD1
        FLD     X
        FYL2X
end;

// LogN
//
function LogN(Base, X: Extended): Extended;
// Log.N(X):=Log.2(X) / Log.2(N)
asm
        FLD1
        FLD     X
        FYL2X
        FLD1
        FLD     Base
        FYL2X
        FDIV
end;

// IntPower
//
function IntPower(Base: Extended; Exponent: Integer): Extended; register;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result:=1 }
        xor     eax, edx
        sub     eax, edx          { eax:=Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X:=Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result:=Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result:=1 / Result }
@@3:
        fwait
end;

// Power
//
function Power(const Base, Exponent: Single): Single;
begin
  if Exponent = 0.0 then
    Result:=1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result:=0.0               { 0**n = 0, n > 0 }
  else Result:=Exp(Exponent * Ln(Base));
end;

// Power (int exponent)
//
function Power(Base: Single; Exponent: Integer): Single;
asm
        mov     ecx, eax
        cdq
        fld1                      { Result:=1 }
        xor     eax, edx
        sub     eax, edx          { eax:=Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X:=Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result:=Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result:=1 / Result }
@@3:
end;

// DegToRad (extended)
//
function DegToRad(const Degrees: Extended): Extended;
begin
   Result:=Degrees * (PI / 180);
end;

// DegToRad (single)
//
function DegToRad(const Degrees : Single) : Single; register;
//   Result:=Degrees * cPIdiv180;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
asm
      FLD  DWORD PTR [EBP+8]
      FMUL cPIdiv180
end;

// RadToDeg (extended)
//
function RadToDeg(const Radians: Extended): Extended;
begin
   Result:=Radians * (180 / PI);
end;

// RadToDeg (single)
//
function RadToDeg(const Radians: Single): Single;
//   Result:=Radians * c180divPI;
// don't laugh, Delphi's compiler manages to make a nightmare of this one
// with pushs, pops, etc. in its default compile... (this one is twice faster !)
asm
      FLD  DWORD PTR [EBP+8]
      FMUL c180divPI
end;

// SinCos (Extended)
//
procedure SinCos(const Theta: Extended; var Sin, Cos: Extended); register;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP TBYTE PTR [EDX]    // cosine
   FSTP TBYTE PTR [EAX]    // sine
end;

// SinCos (Single)
//
procedure SinCos(const Theta: Single; var Sin, Cos: Single); register;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

// SinCos (Single w radius)
//
procedure SinCos(const theta, radius : Single; var Sin, Cos: Single); register;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  theta
   FSINCOS
   FMUL radius
   FSTP DWORD PTR [EDX]    // cosine
   FMUL radius
   FSTP DWORD PTR [EAX]    // sine
end;

// PrepareSinCosCache
//
procedure PrepareSinCosCache(var s, c : array of Single;
                             startAngle, stopAngle : Single);
var
   i : Integer;
   d : Single;
begin
   Assert((High(s)=High(c)) and (Low(s)=Low(c)));
   stopAngle:=stopAngle+1e-5;
   if High(s)>Low(s) then
      d:=cPIdiv180*(stopAngle-startAngle)/(High(s)-Low(s))
   else d:=0;
   for i:=Low(s) to High(s) do
      SinCos((i-Low(s))*d+startAngle, s[i], c[i]);
end;

// ArcCos (Extended)
//
function ArcCos(const x : Extended): Extended;
begin
   Result:=ArcTan2(Sqrt(1 - X * X), X);
end;

// ArcCos (Single)
//
function ArcCos(const x : Single): Single; register;
// Result:=ArcTan2(Sqrt(c1 - X * X), X);
asm
      FLD   X
      FMUL  ST, ST
      FSUBR cOne
      FSQRT
      FLD   X
      FPATAN
end;

// ArcSin (Extended)
//
function ArcSin(const x : Extended) : Extended;
begin
   Result:=ArcTan2(X, Sqrt(1 - X * X))
end;

// ArcSin (Single)
//
function ArcSin(const x : Single) : Single;
//   Result:=ArcTan2(X, Sqrt(1 - X * X))
asm
      FLD   X
      FLD   ST
      FMUL  ST, ST
      FSUBR cOne
      FSQRT
      FPATAN
end;

// ArcTan2 (Extended)
//
function ArcTan2(const y, x : Extended) : Extended;
asm
      FLD  Y
      FLD  X
      FPATAN
end;

// ArcTan2 (Single)
//
function ArcTan2(const y, x : Single) : Single;
asm
      FLD  Y
      FLD  X
      FPATAN
end;

// Tan (Extended)
//
function Tan(const x : Extended) : Extended;
asm
      FLD  X
      FPTAN
      FSTP ST(0)      // FPTAN pushes 1.0 after result
end;

// Tan (Single)
//
function Tan(const x : Single) : Single;
asm
      FLD  X
      FPTAN
      FSTP ST(0)      // FPTAN pushes 1.0 after result
end;

// CoTan (Extended)
//
function CoTan(const x : Extended) : Extended;
asm
      FLD  X
      FPTAN
      FDIVRP
end;

// CoTan (Single)
//
function CoTan(const x : Single) : Single;
asm
      FLD  X
      FPTAN
      FDIVRP
end;

// Trunc (extended)
//
function Trunc(x : Extended) : Int64; register;
asm
      SUB     ESP,12
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     x
      FISTP   qword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
      POP     EDX
end;

// Trunc (single)
//
function Trunc(x : Single) : Integer; register;
asm
      SUB     ESP,8
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     x
      FISTP   dword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
end;

// Frac (Extended)
//
function Frac(v : Extended) : Extended; register;
begin
   Result:=v-Trunc(v);
end;

// Frac (Extended)
//
function Frac(v : Single) : Single; register;
begin
   Result:=v-Trunc(v);
end;

// Round (Extended);
//
function Round(v : Extended) : Int64; register;
asm
      SUB     ESP,8
      FLD     v
      FISTP   qword ptr [ESP]
      POP     EAX
      POP     EDX
end;

// Round (Single);
//
function Round(v : Single) : Integer; register;
asm
      SUB     ESP,4
      FLD     v
      FISTP   dword ptr [ESP]
      POP     EAX
end;

// Sign
//
function Sign(x : Single) : Integer;
begin
   if x<0 then
      Result:=-1
   else if x>0 then
      Result:=1
   else Result:=0;
end;

// IsInRange
//
function IsInRange(const x, a, b : Single) : Boolean;
begin
   if a<b then
      Result:=(a<=x) and (x<=b)
   else Result:=(b<=x) and (x<=a);
end;

// IsInCube (affine)
//
function IsInCube(const p, d : TAffineVector) : Boolean; overload;
begin
   Result:=    ((p[0]>=-d[0]) and (p[0]<=d[0]))
           and ((p[1]>=-d[1]) and (p[1]<=d[1]))
           and ((p[2]>=-d[2]) and (p[2]<=d[2]));
end;

// IsInCube (hmg)
//
function IsInCube(const p, d : TVector) : Boolean; overload;
begin
   Result:=    ((p[0]>=-d[0]) and (p[0]<=d[0]))
           and ((p[1]>=-d[1]) and (p[1]<=d[1]))
           and ((p[2]>=-d[2]) and (p[2]<=d[2]));
end;

// MinFloat (single)
//
function MinFloat(values : PSingleArray; nbItems : Integer) : Single;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]<Result then Result:=values[i];
   end else Result:=0;
end;

// MinFloat (double)
//
function MinFloat(values : PDoubleArray; nbItems : Integer) : Double;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]<Result then Result:=values[i];
   end else Result:=0;
end;

// MinFloat (extended)
//
function MinFloat(values : PExtendedArray; nbItems : Integer) : Extended;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]<Result then Result:=values[i];
   end else Result:=0;
end;

// MaxFloat (single)
//
function MaxFloat(values : PSingleArray; nbItems : Integer) : Single; overload;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]>Result then Result:=values[i];
   end else Result:=0;
end;

// MaxFloat (double)
//
function MaxFloat(values : PDoubleArray; nbItems : Integer) : Double; overload;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]>Result then Result:=values[i];
   end else Result:=0;
end;

// MaxFloat (extended)
//
function MaxFloat(values : PExtendedArray; nbItems : Integer) : Extended; overload;
var
   i : Integer;
begin
   if nbItems>0 then begin
      Result:=values[0];
      for i:=1 to nbItems-1 do
         if values[i]>Result then Result:=values[i];
   end else Result:=0;
end;

// MaxXYZComponent
//
function MaxXYZComponent(const v : TVector) : Single;
begin
   if v[0]>=v[1] then
      if v[0]>=v[2] then
         Result:=v[0]
      else Result:=v[2]
   else if v[1]>=v[2] then
      Result:=v[1]
   else Result:=v[2];
end;

// MaxXYZComponent
//
function MinXYZComponent(const v : TVector) : Single;
begin
   if v[0]<=v[1] then
      if v[0]<=v[2] then
         Result:=v[0]
      else Result:=v[2]
   else if v[1]<=v[2] then
      Result:=v[1]
   else Result:=v[2];
end;

// MaxAbsXYZComponent
//
function MaxAbsXYZComponent(v : TVector) : Single;
begin
   AbsVector(v);
   Result:=MaxXYZComponent(v);
end;

// MinAbsXYZComponent
//
function MinAbsXYZComponent(v : TVector) : Single;
begin
   AbsVector(v);
   Result:=MinXYZComponent(v);
end;

// ClampValue (min-max)
//
function ClampValue(const aValue, aMin, aMax : Single) : Single; overload;
begin
   if aValue<aMin then
      Result:=aMin
   else if aValue>aMax then
      Result:=aMax
   else Result:=aValue;
end;

// ClampValue (min-)
//
function ClampValue(const aValue, aMin : Single) : Single; overload;
begin
   if aValue<aMin then
      Result:=aMin
   else Result:=aValue;
end;

//----------------- miscellaneous vector functions ---------------------------------------------------------------------

function MakeAffineDblVector(var V: array of Double): TAffineDblVector; assembler;
// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V
asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, 6
              REP MOVSD
              POP ESI
              POP EDI
end;

// MakeDblVector
//
function MakeDblVector(var v : array of Double) : THomogeneousDblVector; assembler;
// creates a vector from given values
// EAX contains address of V
// ECX contains address to result vector
// EDX contains highest index of V
asm
              PUSH EDI
              PUSH ESI
              MOV EDI, ECX
              MOV ESI, EAX
              MOV ECX, 8
              REP MOVSD
              POP ESI
              POP EDI
end;

// PointInPolygon
//
function PointInPolygon(var xp, yp : array of Single; x, y: Single) : Boolean;
// The code below is from Wm. Randolph Franklin <wrf@ecse.rpi.edu>
// with some minor modifications for speed.  It returns 1 for strictly
// interior points, 0 for strictly exterior, and 0 or 1 for points on
// the boundary.
var
   I, J: Integer;
begin
   Result:=False;
   if High(XP)=High(YP) then begin
      J:=High(XP);
      for I:=0 to High(XP) do begin
         if ((((YP[I]<=Y) and (Y<YP[J])) or ((YP[J]<=Y) and (Y<YP[I])) )
             and (X<(XP[J]-XP[I])*(Y-YP[I])/(YP[J]-YP[I])+XP[I])) then
            Result:=not Result;
         J:=I;
      end;
   end;
end;

// ConvertRotation
//
function ConvertRotation(const Angles: TAffineVector): TVector; register;

{   Rotation of the Angle t about the axis (X, Y, Z) is given by:

     | X^2 + (1-X^2) Cos(t),    XY(1-Cos(t))  +  Z Sin(t), XZ(1-Cos(t))-Y Sin(t) |
 M = | XY(1-Cos(t))-Z Sin(t), Y^2 + (1-Y^2) Cos(t),      YZ(1-Cos(t)) + X Sin(t) |
     | XZ(1-Cos(t)) + Y Sin(t), YZ(1-Cos(t))-X Sin(t),   Z^2 + (1-Z^2) Cos(t)    |

   Rotation about the three axes (Angles a1, a2, a3) can be represented as
   the product of the individual rotation matrices:

      | 1  0       0       | | Cos(a2) 0 -Sin(a2) | |  Cos(a3) Sin(a3) 0 |
      | 0  Cos(a1) Sin(a1) | * | 0       1  0       | * | -Sin(a3) Cos(a3) 0 |
      | 0 -Sin(a1) Cos(a1) | | Sin(a2) 0  Cos(a2) | |  0       0       1 |
	     Mx                       My                     Mz

   We now want to solve for X, Y, Z, and t given 9 equations in 4 unknowns.
   Using the diagonal elements of the two matrices, we get:

      X^2 + (1-X^2) Cos(t) = M[0][0]
      Y^2 + (1-Y^2) Cos(t) = M[1][1]
      Z^2 + (1-Z^2) Cos(t) = M[2][2]

   Adding the three equations, we get:

      X^2  +  Y^2  +  Z^2 - (M[0][0]  +  M[1][1]  +  M[2][2]) =
	 - (3 - X^2 - Y^2 - Z^2) Cos(t)

   Since (X^2  +  Y^2  +  Z^2) = 1, we can rewrite as:

      Cos(t) = (1 - (M[0][0]  +  M[1][1]  +  M[2][2])) / 2

   Solving for t, we get:

      t = Acos(((M[0][0]  +  M[1][1]  +  M[2][2]) - 1) / 2)

    We can substitute t into the equations for X^2, Y^2, and Z^2 above
    to get the values for X, Y, and Z.  To find the proper signs we note
    that:

	2 X Sin(t) = M[1][2] - M[2][1]
	2 Y Sin(t) = M[2][0] - M[0][2]
	2 Z Sin(t) = M[0][1] - M[1][0]
}
var
   Axis1, Axis2: TVector3f;
   M, M1, M2: TMatrix;
   cost, cost1, sint, s1, s2, s3: Single;
   I: Integer;
begin
   // see if we are only rotating about a single Axis
   if Abs(Angles[X]) < EPSILON then begin
      if Abs(Angles[Y]) < EPSILON then begin
         SetVector(Result, 0, 0, 1, Angles[Z]);
         Exit;
      end else if Abs(Angles[Z]) < EPSILON then begin
         SetVector(Result, 0, 1, 0, Angles[Y]);
         Exit;
      end
   end else if (Abs(Angles[Y]) < EPSILON) and (Abs(Angles[Z]) < EPSILON) then begin
      SetVector(Result, 1, 0, 0, Angles[X]);
      Exit;
   end;

   // make the rotation matrix
   Axis1:=XVector;
   M:=CreateRotationMatrix(Axis1, Angles[X]);

   Axis2:=YVector;
   M2:=CreateRotationMatrix(Axis2, Angles[Y]);
   M1:=MatrixMultiply(M, M2);

   Axis2:=ZVector;
   M2:=CreateRotationMatrix(Axis2, Angles[Z]);
   M:=MatrixMultiply(M1, M2);

   cost:=((M[X, X] + M[Y, Y] + M[Z, Z])-1) / 2;
   if cost < -1 then
      cost:=-1
   else if cost > 1 - EPSILON then begin
      // Bad Angle - this would cause a crash
      SetVector(Result, XHmgVector);
      Exit;
   end;

   cost1:=1 - cost;
   SetVector(Result, sqrt((M[X, X]-cost) / cost1),
                     sqrt((M[Y, Y]-cost) / cost1),
                     sqrt((M[Z, Z]-cost) / cost1),
                     arccos(cost));

   sint:=2 * Sqrt(1 - cost * cost); // This is actually 2 Sin(t)

   // Determine the proper signs
   for I:=0 to 7 do begin
     if (I and 1) > 1 then s1:=-1 else s1:=1;
     if (I and 2) > 1 then s2:=-1 else s2:=1;
     if (I and 4) > 1 then s3:=-1 else s3:=1;
     if (Abs(s1 * Result[X] * sint-M[Y, Z] + M[Z, Y]) < EPSILON2)
        and (Abs(s2 * Result[Y] * sint-M[Z, X] + M[X, Z]) < EPSILON2)
        and (Abs(s3 * Result[Z] * sint-M[X, Y] + M[Y, X]) < EPSILON2) then begin
           // We found the right combination of signs
           Result[X]:=Result[X] * s1;
           Result[Y]:=Result[Y] * s2;
           Result[Z]:=Result[Z] * s3;
           Exit;
         end;
   end;
end;

// QuaternionSlerp
//
function QuaternionSlerp(const QStart, QEnd: TQuaternion; Spin: Integer; t: Single): TQuaternion;
var
    beta,                   // complementary interp parameter
    theta,                  // Angle between A and B
    sint, cost,             // sine, cosine of theta
    phi: Single;            // theta plus spins
    bflip: Boolean;         // use negativ t?
begin
  // cosine theta
  cost:=VectorAngle(QStart.ImagPart, QEnd.ImagPart);

   // if QEnd is on opposite hemisphere from QStart, use -QEnd instead
   if cost < 0 then begin
      cost:=-cost;
      bflip:=True;
   end else bflip:=False;

   // if QEnd is (within precision limits) the same as QStart,
   // just linear interpolate between QStart and QEnd.
   // Can't do spins, since we don't know what direction to spin.

   if (1 - cost) < EPSILON then
      beta:=1 - t
   else begin
      // normal case
      theta:=arccos(cost);
      phi:=theta + Spin * Pi;
      sint:=sin(theta);
      beta:=sin(theta - t * phi) / sint;
      t:=sin(t * phi) / sint;
   end;

   if bflip then t:=-t;

   // interpolate
   Result.ImagPart[X]:=beta * QStart.ImagPart[X] + t * QEnd.ImagPart[X];
   Result.ImagPart[Y]:=beta * QStart.ImagPart[Y] + t * QEnd.ImagPart[Y];
   Result.ImagPart[Z]:=beta * QStart.ImagPart[Z] + t * QEnd.ImagPart[Z];
   Result.RealPart:=beta * QStart.RealPart + t * QEnd.RealPart;
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorDblToFlt(const V: THomogeneousDblVector): THomogeneousVector; assembler;

// converts a vector containing double sized values into a vector with single sized values

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
              FLD  QWORD PTR [EAX + 24]
              FSTP DWORD PTR [EDX + 12]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineDblToFlt(const V: TAffineDblVector): TAffineVector; assembler;

// converts a vector containing double sized values into a vector with single sized values

asm
              FLD  QWORD PTR [EAX]
              FSTP DWORD PTR [EDX]
              FLD  QWORD PTR [EAX + 8]
              FSTP DWORD PTR [EDX + 4]
              FLD  QWORD PTR [EAX + 16]
              FSTP DWORD PTR [EDX + 8]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorAffineFltToDbl(const V: TAffineVector): TAffineDblVector; assembler;

// converts a vector containing single sized values into a vector with double sized values

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
end;

//----------------------------------------------------------------------------------------------------------------------

function VectorFltToDbl(const V: TVector): THomogeneousDblVector; assembler;

// converts a vector containing single sized values into a vector with double sized values

asm
              FLD  DWORD PTR [EAX]
              FSTP QWORD PTR [EDX]
              FLD  DWORD PTR [EAX + 8]
              FSTP QWORD PTR [EDX + 4]
              FLD  DWORD PTR [EAX + 16]
              FSTP QWORD PTR [EDX + 8]
              FLD  DWORD PTR [EAX + 24]
              FSTP QWORD PTR [EDX + 12]
end;

//----------------- coordinate system manipulation functions -----------------------------------------------------------

// Turn (Y axis)
//
function Turn(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[1][0], Matrix[1][1], Matrix[1][2]), Angle));
end;

// Turn (direction)
//
function Turn(const Matrix: TMatrix; const MasterUp: TAffineVector; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterUp, Angle));
end;

// Pitch (X axis)
//
function Pitch(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[0][0], Matrix[0][1], Matrix[0][2]), Angle));
end;

// Pitch (direction)
//
function Pitch(const Matrix: TMatrix; const MasterRight: TAffineVector; Angle: Single): TMatrix; overload;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterRight, Angle));
end;

// Roll (Z axis)
//
function Roll(const Matrix: TMatrix; Angle: Single): TMatrix;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(AffineVectorMake(Matrix[2][0], Matrix[2][1], Matrix[2][2]), Angle));
end;

// Roll (direction)
//
function Roll(const Matrix: TMatrix; const MasterDirection: TAffineVector; Angle: Single): TMatrix; overload;
begin
   Result:=MatrixMultiply(Matrix, CreateRotationMatrix(MasterDirection, Angle));
end;

// RayCastPlaneIntersect (plane defined by point+normal)
//
function RayCastPlaneIntersect(const rayStart, rayVector : TAffineVector;
                               const planePoint, planeNormal : TAffineVector;
                               intersectPoint : PAffineVector = nil) : Boolean;
var
   sp : TAffineVector;
   t, d : Single;
begin
   d:=VectorDotProduct(rayVector, planeNormal);
   Result:=((d>EPSILON2) or (d<-EPSILON2));
   if Result and Assigned(intersectPoint) then begin
      VectorSubtract(planePoint, rayStart, sp);
      d:=1/d; // will keep one FPU unit busy during dot product calculation
      t:=VectorDotProduct(sp, planeNormal)*d;
      if t>0 then begin
         SetVector(intersectPoint^, rayStart);
         CombineVector(intersectPoint^, rayVector, t);
      end else Result:=False;
   end;
end;

// RayCastTriangleIntersect
//
function RayCastTriangleIntersect(const rayStart, rayVector : TAffineVector;
                                  const p1, p2, p3 : TAffineVector;
                                  intersectPoint : PAffineVector = nil;
                                  intersectNormal : PAffineVector = nil) : Boolean;
var
   v1, v2, n, s : TAffineVector;
   d, t, v, x, y : Single;
begin
   v1:=VectorSubtract(p2, p1);
   v2:=VectorSubtract(p3, p1);
   n:=VectorCrossProduct(v1, v2);
   v:=VectorDotProduct(rayVector, n);
   if Abs(v)<=1e-7 then begin
      Result:=False;
      Exit;
   end;
   s:=VectorSubtract(rayStart, p1);
   d:=VectorDotProduct(s, n);
   t:=d/v;
   s:=VectorSubtract(VectorCombine(rayStart, rayVector, 1, t), p1);
   x:=VectorDotProduct(v1, s);
   y:=VectorDotProduct(v2, s);
   Result:=(x>=0) and (y>=0) and (x+y<1);
   if Result then begin
      if intersectPoint<>nil then
         intersectPoint^:=s;
      if intersectNormal<>nil then begin
         NormalizeVector(n);
         intersectNormal^:=n;
      end;
   end;
end;

// IntersectLinePlane
//
function IntersectLinePlane(const point, direction : TAffineVector;
                            const plane : THmgPlane;
                            intersectPoint : PAffineVector = nil) : Integer;
var
   a, b : Extended;
   t : Single;
begin
   a:=VectorDotProduct(plane, direction);    // direction projected to plane normal
   b:=PlaneEvaluatePoint(plane, point);      // distance to plane
   if a=0 then begin          // direction is parallel to plane
      if b=0 then
         Result:=-1           // line is inside plane
      else Result:=0;         // line is outside plane
   end else begin
      if Assigned(intersectPoint) then begin
         t:=-b/a;                               // parameter of intersection
         intersectPoint^:=point;
         // calculate intersection = p + t*d
         CombineVector(intersectPoint^, direction, t);
      end;
      Result:=1;
   end;
end;

// IsVolumeClipped
//
function IsVolumeClipped(const objPos : TAffineVector; const objRadius : Single;
                         const rcci : TRenderContextClippingInfo) : Boolean;
var
   objCenter, objProj : TAffineVector;
   proj, dist : Single;
begin
   VectorSubtract(objPos, PAffineVector(@rcci.origin)^, objCenter);
   proj:=VectorDotProduct(objCenter, PAffineVector(@rcci.clippingDirection)^);
   if (proj+objRadius>0) and (proj-objRadius<rcci.farClippingDistance) then begin
      if proj>0 then begin
         VectorScale(PAffineVector(@rcci.clippingDirection)^, proj, objProj);
         dist:=proj*1.1*rcci.viewPortRadius;
         Result:=(VectorDistance2(objCenter, objProj)>Sqr(dist+objRadius));
      end else begin
         Result:=(VectorNorm(objCenter)>Sqr(objRadius));
      end;
   end else Result:=True;
end;

// IsVolumeClipped
//
function IsVolumeClipped(const min, max : TAffineVector;
                         const rcci : TRenderContextClippingInfo) : Boolean;
begin
   // change box to sphere
   Result:=IsVolumeClipped(VectorScale(VectorAdd(min, max), 0.5),
                           VectorDistance(min, max)*0.5, rcci);
end;

// MakeShadowMatrix
//
function MakeShadowMatrix(const planePoint, planeNormal, lightPos : TVector) : TMatrix;
var
   planeNormal3, dot : Single;
begin
	// Find the last coefficient by back substitutions
	planeNormal3:=-( planeNormal[0]*planePoint[0]
                   +planeNormal[1]*planePoint[1]
                   +planeNormal[2]*planePoint[2]);
	// Dot product of plane and light position
	dot:= planeNormal[0]*lightPos[0]
        +planeNormal[1]*lightPos[1]
        +planeNormal[2]*lightPos[2]
        +planeNormal3  *lightPos[3];
	// Now do the projection
	// First column
   Result[0][0]:= dot - lightPos[0] * planeNormal[0];
   Result[1][0]:=     - lightPos[0] * planeNormal[1];
   Result[2][0]:=     - lightPos[0] * planeNormal[2];
   Result[3][0]:=     - lightPos[0] * planeNormal3;
	// Second column
	Result[0][1]:=     - lightPos[1] * planeNormal[0];
	Result[1][1]:= dot - lightPos[1] * planeNormal[1];
	Result[2][1]:=     - lightPos[1] * planeNormal[2];
	Result[3][1]:=     - lightPos[1] * planeNormal3;
	// Third Column
	Result[0][2]:=     - lightPos[2] * planeNormal[0];
	Result[1][2]:=     - lightPos[2] * planeNormal[1];
	Result[2][2]:= dot - lightPos[2] * planeNormal[2];
	Result[3][2]:=     - lightPos[2] * planeNormal3;
	// Fourth Column
	Result[0][3]:=     - lightPos[3] * planeNormal[0];
	Result[1][3]:=     - lightPos[3] * planeNormal[1];
	Result[2][3]:=     - lightPos[3] * planeNormal[2];
	Result[3][3]:= dot - lightPos[3] * planeNormal3;
end;

// MakeReflectionMatrix
//
function MakeReflectionMatrix(const planePoint, planeNormal : TAffineVector) : TMatrix;
var
   pv2 : Single;
begin
   // Precalcs
   pv2:=2*VectorDotProduct(planePoint, planeNormal);
   // 1st column
   Result[0][0]:=1-2*Sqr(planeNormal[0]);
   Result[1][0]:=-2*planeNormal[0]*planeNormal[1];
   Result[2][0]:=-2*planeNormal[0]*planeNormal[2];
   Result[3][0]:=0;
   // 2nd column
   Result[0][1]:=-2*planeNormal[0]*planeNormal[1];
   Result[1][1]:=1-2*Sqr(planeNormal[1]);
   Result[2][1]:=-2*planeNormal[1]*planeNormal[2];
   Result[3][1]:=0;
   // 3rd column
   Result[0][2]:=-2*planeNormal[0]*planeNormal[2];
   Result[1][2]:=-2*planeNormal[1]*planeNormal[2];
   Result[2][2]:=1-2*Sqr(planeNormal[2]);
   Result[3][2]:=0;
   // 2nd column
   Result[0][3]:=pv2*planeNormal[0];
   Result[1][3]:=pv2*planeNormal[1];
   Result[2][3]:=pv2*planeNormal[2];
   Result[3][3]:=1;
end;

//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------
initialization
//--------------------------------------------------------------
//--------------------------------------------------------------
//--------------------------------------------------------------

   // detect 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
   asm
      pusha
      mov  eax, $80000000
      db $0F,$A2               /// cpuid
      cmp  eax, $80000000
      jbe @@No3DNow
      mov  eax, $80000001
      db $0F,$A2               /// cpuid
      test edx, $80000000
      jz @@No3DNow
      mov vSIMD, 1
@@No3DNow:
      popa
   end;

end.


