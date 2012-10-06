// Spline
{: Cubic spline interpolation functions<p>

	<b>Historique : </b><font size=-1><ul>
	   <li>28/05/00 - Egg - Javadocisation, minor changes & optimizations,
                           Renamed TSpline to TCubicSpline, added W component
                           and a bunch of helper methods
	   <li>20/05/00 - RoC - Created, based on the C source code from Eric
	</ul></font>
}
unit Spline;

interface

uses Geometry;

type

   // TCubicSpline
   //
   {: 3D cubic spline handler class.<p>
      This class allows to describe and calculate values of a time-based,
      three-dimensionnal cubic spline.<p>
      Cubic spline pass through all given points and tangent on point N is
      given by the (N-1) to (N+1) vector.<p>
      Note : X, Y & Z are actually interpolated independently. }
   TCubicSpline = class (TObject)
      private
         { Private Declarations }
         matX, matY, matZ, matW : Pointer;
         FNb : Integer;

      public
         { Public Declarations }
         {: Creates the spline and declares interpolation points.<p>
            Time references go from 0 (first point) to nb-1 (last point), the
            first and last reference matrices respectively are used when T is
            used beyond this range.<p>
            Note : "nil" single arrays are accepted, in this case the axis is
            disabled and calculus will return 0 (zero) for this component. }
         constructor Create(const X, Y, Z, W : PFloatArray; const nb : Integer);
         destructor Destroy; override;

         {: Calculates X component at time t.<p> }
         function SplineX(const t : Single): Single;
         {: Calculates Y component at time t.<p> }
         function SplineY(const t : single): Single;
         {: Calculates Z component at time t.<p> }
         function SplineZ(const t : single): Single;
         {: Calculates W component at time t.<p> }
         function SplineW(const t : single): Single;

         {: Calculates X and Y components at time t.<p> }
         procedure SplineXY(const t : single; var X, Y : Single);
         {: Calculates X, Y and Z components at time t.<p> }
         procedure SplineXYZ(const t : single; var X, Y, Z : Single);
         {: Calculates X, Y, Z and W components at time t.<p> }
         procedure SplineXYZW(const t : single; var X, Y, Z, W : Single);

         {: Calculates affine vector at time t.<p> }
         function SplineAffineVector(const t : single) : TAffineVector; overload;
         {: Calculates affine vector at time t.<p> }
         procedure SplineAffineVector(const t : single; var vector : TAffineVector); overload;
         {: Calculates vector at time t.<p> }
         function SplineVector(const t : single) : TVector; overload;
         {: Calculates vector at time t.<p> }
         procedure SplineVector(const t : single; var vector : TVector); overload;

         {: Calculates the spline slope at time t. }
         function SplineSlopeVector(const t : single) : TAffineVector; overload;

         {: Calculates the intersection of the spline with the YZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;
         {: Calculates the intersection of the spline with the XZ plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;
         {: Calculates the intersection of the spline with the XY plane.<p>
            Returns True if an intersection was found. }
         function SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

type
   TSingleMatrix = array of PFloatArray;

// VECCholeskyTriDiagResol
//
function VECCholeskyTriDiagResol(const b : PFloatArray; const nb : Integer) : PFloatArray;
var
   Y, LDiag, LssDiag : PFloatArray;
   i, k, Debut, Fin: Integer;
begin
   Debut:=0;
   Fin:=nb-1;
   Assert(Assigned(B));
   GetMem(LDiag, nb*sizeof(Single));
   GetMem(LssDiag, (nb-1)*sizeof(Single));
   try
      LDiag[Debut]:=1.4142135; // = sqrt(2)
      LssDiag[Debut]:=1.0/1.4142135;
      for K:=Debut+1 to Fin-1 do begin
         LDiag[K]:=Sqrt(4-LssDiag[K-1]*LssDiag[K-1]);
         LssDiag[K]:=1.0/LDiag[K];
      end;
      LDiag[Fin]:=Sqrt(2-LssDiag[Fin-1]*LssDiag[Fin-1]);
      GetMem(Y, nb*sizeof(Single));
      try
         Y[Debut]:=B[Debut]/LDiag[Debut];
         for I:=Debut+1 to Fin do
            Y[I]:=(B[I]-Y[I-1]*LssDiag[I-1])/LDiag[I];
         GetMem(Result, nb*sizeof(Single));
         Result[Fin]:=Y[Fin]/LDiag[Fin];
         for i:=Fin-1 downto Debut do
            Result[I]:=(Y[I]-Result[I+1]*LssDiag[I])/LDiag[I];
      finally
         FreeMem(Y);
      end;
   finally
      FreeMem(LDiag);
      FreeMem(LssDiag);
   end;
end;

// MATInterpolationHermite
//
function MATInterpolationHermite(const ordonnees : PFloatArray; const nb : Integer): Pointer;
var
   a, b, c, d : Single;
   m : Pointer;
   i, n : Integer;
   bb, deriv, fa : PFloatArray;
begin
   Result:=nil;
   if Assigned(Ordonnees) and (nb>0) then begin
      n:=nb-1;
      GetMem(bb, nb*sizeof(Single));
      try
         PFloatArray(bb)[0]:=3*(ordonnees[1]-ordonnees[0]);
         PFloatArray(bb)[n]:=3*(ordonnees[n]-ordonnees[n-1]);
         for i:=1 to n-1 do
            bb[I]:=3*(ordonnees[I+1]-ordonnees[I-1]);
         deriv:=VECCholeskyTriDiagResol(bb, nb);
         try
            GetMem(m, n*SizeOf(PFloatArray));
            for i:=0 to n-1 do begin
               GetMem(TSingleMatrix(m)[I], 4*SizeOf(Single));
               a:=ordonnees[I];
               b:=deriv[I];
               c:=3*(ordonnees[I+1]-ordonnees[I])-2*deriv[I]-deriv[I+1];
               d:=-2*(ordonnees[I+1]-ordonnees[I])+deriv[I]+deriv[I+1];
               fa:=TSingleMatrix(m)[I];
               fa[3]:=a+I*(I*(c-I*d)-b);
               fa[2]:=b+I*(3*I*d-2*c);
               fa[1]:=c-3*I*d;
               fa[0]:=d;
            end;
         finally
            FreeMem(Deriv);
         end;
      finally
         FreeMem(BB);
      end;
      Result:=m;
   end;
end;

// MATValeurSpline
//
function MATValeurSpline(const spline : Pointer; const x : Single;
                         const nb : Integer) : Single;
var
   i : Integer;
   sa : PFloatArray;
begin
   if Assigned(Spline) then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Trunc(x);
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      sa:=PFloatArray(TSingleMatrix(spline)[i]);
      Result:=((sa[0]*x+sa[1])*x+sa[2])*x+sa[3];
   end else Result:=0;
end;

// MATValeurSplineSlope
//
function MATValeurSplineSlope(const spline : Pointer; const x : Single;
                              const nb : Integer) : Single;
var
   i : Integer;
   sa : PFloatArray;
begin
   if Assigned(Spline) then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Trunc(x);
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      sa:=PFloatArray(TSingleMatrix(spline)[i]);
      Result:=(3*sa[0]*x+2*sa[1])*x+sa[2];
   end else Result:=0;
end;

// ------------------
// ------------------ TCubicSpline ------------------
// ------------------

// Create
//
constructor TCubicSpline.Create(const X, Y, Z, W: PFloatArray; const nb : Integer);
begin
   MatX:=MATInterpolationHermite(X, nb);
   MatY:=MATInterpolationHermite(Y, nb);
   MatZ:=MATInterpolationHermite(Z, nb);
   MatW:=MATInterpolationHermite(W, nb);
   FNb:=nb;
end;

// Destroy
//
destructor TCubicSpline.Destroy;

   procedure FreeMatrix(matrix : Pointer);
   var
      i : Integer;
   begin
      if Assigned(matrix) then begin
         for i:=0 to FNb-2 do
            FreeMem(PFloatArray(TSingleMatrix(matrix)[I]));
         FreeMem(matrix);
      end;
   end;

begin
   FreeMatrix(MatX);
   FreeMatrix(MatY);
   FreeMatrix(MatZ);
   FreeMatrix(MatW);
   inherited Destroy;
end;

// SplineX
//
function TCubicSpline.SplineX(const t : single): Single;
begin
   Result:=MATValeurSpline(MatX, t, FNb);
end;

// SplineY
//
function TCubicSpline.SplineY(const t : single): Single;
begin
   Result:=MATValeurSpline(MatY, t, FNb);
end;

// SplineZ
//
function TCubicSpline.SplineZ(const t : single): Single;
begin
   Result:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineW
//
function TCubicSpline.SplineW(const t : single): Single;
begin
   Result:=MATValeurSpline(MatW, t, FNb);
end;

// SplineXY
//
procedure TCubicSpline.SplineXY(const t : single; var X, Y : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
end;

// SplineXYZ
//
procedure TCubicSpline.SplineXYZ(const t : single; var X, Y, Z : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
   Z:=MATValeurSpline(MatZ, T, FNb);
end;

// SplineXYZW
//
procedure TCubicSpline.SplineXYZW(const t : single; var X, Y, Z, W : Single);
begin
   X:=MATValeurSpline(MatX, T, FNb);
   Y:=MATValeurSpline(MatY, T, FNb);
   Z:=MATValeurSpline(MatZ, T, FNb);
   W:=MATValeurSpline(MatW, T, FNb);
end;

// SplineAffineVector
//
function TCubicSpline.SplineAffineVector(const t : single) : TAffineVector;
begin
   Result[0]:=MATValeurSpline(MatX, t, FNb);
   Result[1]:=MATValeurSpline(MatY, t, FNb);
   Result[2]:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineAffineVector
//
procedure TCubicSpline.SplineAffineVector(const t : single; var vector : TAffineVector);
begin
   vector[0]:=MATValeurSpline(MatX, t, FNb);
   vector[1]:=MATValeurSpline(MatY, t, FNb);
   vector[2]:=MATValeurSpline(MatZ, t, FNb);
end;

// SplineVector
//
function TCubicSpline.SplineVector(const t : single) : TVector;
begin
   Result[0]:=MATValeurSpline(MatX, t, FNb);
   Result[1]:=MATValeurSpline(MatY, t, FNb);
   Result[2]:=MATValeurSpline(MatZ, t, FNb);
   Result[3]:=MATValeurSpline(MatW, t, FNb);
end;

// SplineVector
//
procedure TCubicSpline.SplineVector(const t : single; var vector : TVector);
begin
   vector[0]:=MATValeurSpline(MatX, t, FNb);
   vector[1]:=MATValeurSpline(MatY, t, FNb);
   vector[2]:=MATValeurSpline(MatZ, t, FNb);
   vector[3]:=MATValeurSpline(MatW, t, FNb);
end;

// SplineSlopeVector
//
function TCubicSpline.SplineSlopeVector(const t : single) : TAffineVector;
begin
   Result[0]:=MATValeurSplineSlope(MatX, t, FNb);
   Result[1]:=MATValeurSplineSlope(MatY, t, FNb);
   Result[2]:=MATValeurSplineSlope(MatZ, t, FNb);
end;

// SplineIntersecYZ
//
function TCubicSpline.SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineX(Sup);
   Sinf:=SplineX(Inf);
   if SSup>Sinf then begin
      if (SSup<X) or (Sinf>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<X) or (SSup>X) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineX(Mid);
         if X<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      Y:=SplineY((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

// SplineIntersecXZ
//
function TCubicSpline.SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineY(Sup);
   Sinf:=SplineY(Inf);
   if SSup>Sinf then begin
      if (SSup<Y) or (Sinf>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Y) or (SSup>Y) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineY(Mid);
         if Y<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Z:=SplineZ((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

// SplineIntersecXY
//
function TCubicSpline.SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;
var
   Sup, Inf, Mid: Double;
   SSup, Sinf, Smid: Single;
begin
   Result:=False;

   Sup:=FNb;
   Inf:=0.0;

   Ssup:=SplineZ(Sup);
   Sinf:=SplineZ(Inf);
   if SSup>Sinf then begin
      if (SSup<Z) or (Sinf>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            SSup:=SMid;
            Sup:=Mid;
         end else begin
            Sinf:=SMid;
            Inf:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end else begin
      if (Sinf<Z) or (SSup>Z) then Exit;
      while Abs(SSup-Sinf)>1e-4 do begin
         Mid:=(Sup+Inf)*0.5;
         SMid:=SplineZ(Mid);
         if Z<SMid then begin
            Sinf:=SMid;
            Inf:=Mid;
         end else begin
            SSup:=SMid;
            Sup:=Mid;
         end;
      end;
      X:=SplineX((Sup+Inf)*0.5);
      Y:=SplineY((Sup+Inf)*0.5);
   end;
   Result:=True;
end;

end.
