{: GLNavigator<p>

   Unit for navigating TGLBaseObjects.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>09/11/00 - JAJ - First submitted. Base Class TGLNavigator included.
	</ul></font>

}
unit GLNavigator;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Geometry, GLScene, GLMisc;

type

	// TGLNavigator
	//
	{: TGLNavigator is the component for moving a TGLBaseSceneObject, and all Classes based on it,
      this includes all the objects from the Scene Editor.<p>

	   The three calls to get you started is
      <ul>
  	   <li>TurnHorisontal : it turns left and right.
	   <li>TurnVertical : it turns up and down.
	   <li>MoveForward :	moves back and forth.
      </ul>
	   The three properties to get you started is
      <ul>
	   <li>MovingObject : The Object that you are moving.
	   <li>UseVirtualUp : When UseVirtualUp is set you navigate Quake style. If it isn't
   		it's more like Descent.
	   <li>AngleLock : Allows you to block the Vertical angles. Should only be used in
			conjunction with UseVirtualUp.
      </ul>
   }
   TGLNavigator = class(TComponent)
      private
         FObject           : TGLBaseSceneObject;
         FVirtualRight     : TVector;
         FVirtualUp        : TGLCoordinates;
         FUseVirtualUp     : Boolean;
         FAutoUpdateObject : Boolean;
         FMaxAngle         : Single;
         FMinAngle         : Single;
         FCurrentAngle     : Single;
         FAngleLock        : Boolean;
      public
         Constructor Create(AOwner : TComponent); override;
         Destructor  Destroy; override;
         Procedure   SetObject(NewObject : TGLBaseSceneObject);
         Procedure   SetUseVirtualUp(UseIt : Boolean);
         Procedure   SetVirtualUp(Up : TGLCoordinates);
         Function    CalcRight : TVector;
      protected
      public
         Procedure   TurnHorizontal(Angle : Single);
         Procedure   TurnVertical(Angle : Single);
         Procedure   MoveForward(Distance : Single);
         Procedure   StrafeHorizontal(Distance : Single);
         Procedure   StrafeVertical(Distance : Single);
         Procedure   Straighten;
      published
         property    VirtualUp    : TGLCoordinates read FVirtualUp write SetVirtualUp;
         property    MovingObject : TGLBaseSceneObject read FObject write SetObject;
         property    UseVirtualUp : Boolean read FUseVirtualUp write SetUseVirtualUp;
         property    AutoUpdateObject : Boolean read FAutoUpdateObject write FAutoUpdateObject;
         property    MaxAngle     : Single read FMaxAngle write FMaxAngle;
         property    MinAngle     : Single read FMinAngle write FMinAngle;
         property    AngleLock    : Boolean read FAngleLock write FAngleLock;
   end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('GLScene', [TGLNavigator]);
end;

Constructor TGLNavigator.Create(AOwner : TComponent);
Begin
  inherited;
  FVirtualUp    := TGLCoordinates.Create(Self);
  FCurrentAngle := 0;
End;

Destructor  TGLNavigator.Destroy;

Begin
  FVirtualUp.Free;
  inherited;
End;

Procedure   TGLNavigator.SetObject(NewObject : TGLBaseSceneObject);

Begin
  FObject := NewObject;
  If not Assigned(FObject) then Exit;
  if csdesigning in componentstate then
  Begin
    If VectorLength(FVirtualUp.AsVector) = 0 then
    Begin
      FVirtualUp.AsVector := FObject.Up.AsVector;
    End;
    Exit;
  End;

  If FUseVirtualUp Then FVirtualRight := CalcRight;
End;

Function    TGLNavigator.CalcRight : TVector;

Begin
  If Assigned(FObject) then
  If FUseVirtualUp Then
  Begin
    VectorCrossProduct(FObject.Direction.AsVector, FVirtualUp.AsVector, Result);
    ScaleVector(Result,1/VectorLength(Result));
  End else VectorCrossProduct(FObject.Direction.AsVector, FObject.Up.AsVector, Result); { automaticly length(1), if not this is a bug }
End;

Procedure   TGLNavigator.TurnHorizontal(Angle : Single);

Var
  T : TVector;
  U : TAffineVector;


Begin

  Angle := DegToRad(Angle); {make it ready for Cos and Sin }
  If FUseVirtualUp Then
  Begin
    SetVector(U, VirtualUp.AsVector);
    T := FObject.Up.AsVector;
    RotateVector(T,U,Angle);
    FObject.Up.AsVector := T;

    T := FObject.Direction.AsVector;
    RotateVector(T,U,Angle);
    FObject.Direction.AsVector := T;
  End else FObject.Direction.AsVector := VectorCombine(FObject.Direction.AsVector,CalcRight,Cos(Angle),Sin(Angle));
End;

Procedure   TGLNavigator.TurnVertical(Angle : Single);

Var
  CosAngle, SinAngle : Single;
  Direction : TVector;

Begin
  If FAngleLock then
  Begin
    CosAngle := FCurrentAngle+Angle; {used as a temp angle, to save stack}
    If CosAngle > FMaxAngle then
    Begin
      If FCurrentAngle = FMaxAngle then Exit;
      CosAngle := FMaxAngle;
    End else
    Begin
      If CosAngle < FMinAngle then
      Begin
        If FCurrentAngle = FMinAngle then Exit;
        CosAngle := FMinAngle;
      End;
    End;
  End;
  FCurrentAngle := CosAngle; {CosAngle temp, use stopped}

  Angle := DegToRad(Angle); {make it ready for Cos and Sin }
  SinCos(Angle,SinAngle,CosAngle);
  Direction := VectorCombine(FObject.Direction.AsVector,FObject.Up.AsVector,CosAngle,SinAngle);
  FObject.Up.AsVector := VectorCombine(FObject.Direction.AsVector,FObject.Up.AsVector,SinAngle,CosAngle);
  FObject.Direction.AsVector := Direction;
End;

Procedure   TGLNavigator.MoveForward(Distance : Single);
Begin
  If FUseVirtualUp Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,VectorCrossProduct(FVirtualUp.AsVector,CalcRight),1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Direction.AsVector,1,Distance);
End;

Procedure   TGLNavigator.StrafeHorizontal(Distance : Single);
Begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,CalcRight,1,Distance);
End;

Procedure   TGLNavigator.StrafeVertical(Distance : Single);
Begin
  If UseVirtualUp Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FVirtualUp.AsVector,1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Up.AsVector,1,Distance);
End;

Procedure   TGLNavigator.Straighten;

Var
  R : TVector;
  D : TVector;
  A : Single;

Begin
  FCurrentAngle     := 0;

  R := CalcRight;
  A := VectorAngle(AffineVectorMake(MovingObject.Up.AsVector), AffineVectorMake(VirtualUp.AsVector));
  MovingObject.Up.AsVector := VirtualUp.AsVector;

  VectorCrossProduct(R, FVirtualUp.AsVector, D);

  If A >= 0 then
    ScaleVector(D,-1/VectorLength(D))
  else
    ScaleVector(D,1/VectorLength(D));

  MovingObject.Direction.AsVector := D;
End;

Procedure   TGLNavigator.SetUseVirtualUp(UseIt : Boolean);

Begin
  FUseVirtualUp := UseIt;
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
End;


Procedure   TGLNavigator.SetVirtualUp(Up : TGLCoordinates);
Begin
  FVirtualUp.Assign(Up);
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
End;


end.
