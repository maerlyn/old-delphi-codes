unit CameraControl;
{******************************************************************************}
{                                                                              }
{                               CameraControl                                  }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{                                                                              }
{ Portions created by Ariel Jacob are                                          }
{ Copyright (C) 2000 - 2001 Ariel Jacob.                                       }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ Many thanks to Ben Humphrey (DigiBen) from www.GameTutorials.com (tutorials) }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   CameraControl                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{                                                                              }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{                                                                              }
{                                                                              }
{                                                                              }
{******************************************************************************}

interface
uses Math3D;

type
  TCammra = Class
    FPosition,
    FView,
    FUpVector   : T3DVector;

    constructor Create;
    procedure SetView( Position, View, UpVector : T3DVector );
    procedure RotateView( valX, valY, valZ : single );
    procedure FPRotateViewX( amount: single );
    procedure RotateAroundPoint( vCenter: T3DVector; valX, valY, valZ: single );
    procedure StrafeCamera( amount: single );
    procedure MoveCamera( amount: single );
  end;

implementation

// -- TCammra -----------------------------------------------------------------
constructor TCammra.Create;
begin
  inherited;
  FPosition:= TVector(0, 0, -1);
  FView    := TVector(0, 0, 0);
  FUpVector:= TVector(0, 0, 1);
end;

// SetView the Cammra
procedure TCammra.SetView( Position, View, UpVector : T3DVector );
begin
  FPosition:= Position;
  FView    := View;
  FUpVector:= UpVector;         
end;

// This rotates the camera's view around the position
procedure TCammra.RotateView( valX, valY, valZ : single );
var
  vVector: T3DVector;
begin
  // Get our view vVector (The direction we are facing)
  vVector.x:= FView.x - FPosition.x;
  vVector.y:= FView.y - FPosition.y;
  vVector.z:= FView.z - FPosition.z;

  if valX <> 0 then
  begin
    FView.z:= (FPosition.z + sin(valX)*vVector.y + cos(valX)*vVector.z);
    FView.y:= (FPosition.y + cos(valX)*vVector.y - sin(valX)*vVector.z);
  end;
  if valY <> 0 then
  begin
    FView.z:= (FPosition.z + sin(valY)*vVector.x + cos(valY)*vVector.z);
    FView.x:= (FPosition.x + cos(valY)*vVector.x - sin(valY)*vVector.z);
  end;
  if valZ <> 0 then
  begin
    FView.x:= (FPosition.x + sin(valZ)*vVector.y + cos(valZ)*vVector.x);
    FView.y:= (FPosition.y + cos(valZ)*vVector.y - sin(valZ)*vVector.x);
  end;
end;

procedure TCammra.FPRotateViewX( amount: single );
begin
  FView.y := FView.y + amount;

  if ( FView.y - FPosition.y ) >  10 then
    FView.y:= FPosition.y + 10;

  // Check if the distance of our view exceeds -60 from our position, if so, stop it. (DOWN)
  if ( FView.y - FPosition.y ) < -10 then
    FView.y:= FPosition.y - 10;
end;

procedure TCammra.RotateAroundPoint( vCenter: T3DVector;
  valX, valY, valZ: single);
var
  vVector: T3DVector;
begin
  vVector.x:= FPosition.x - vCenter.x;
  vVector.y:= FPosition.y - vCenter.y;
  vVector.z:= FPosition.z - vCenter.z;

  if valX <> 0 then
  begin
    FPosition.z:= (vCenter.z + sin(valX)*vVector.y + cos(valX)*vVector.z);
    FPosition.y:= (vCenter.y + cos(valX)*vVector.y - sin(valX)*vVector.z);
  end;
  if valY <> 0 then
  begin
    FPosition.z:= (vCenter.z + sin(valY)*vVector.x + cos(valY)*vVector.z);
    FPosition.x:= (vCenter.x + cos(valY)*vVector.x - sin(valY)*vVector.z);
  end;
  if valZ <> 0 then
  begin
    FPosition.x:= (vCenter.x + sin(valZ)*vVector.y + cos(valZ)*vVector.x);
    FPosition.y:= (vCenter.y + cos(valZ)*vVector.y - sin(valZ)*vVector.x);
  end;
end;

procedure TCammra.StrafeCamera( amount: single );
var
  Cross,
  vVector: T3DVector;
begin
  vVector.x:= FView.x - FPosition.x;
  vVector.y:= FView.y - FPosition.y;
  vVector.z:= FView.z - FPosition.z;

  // The X value for the vVector is:  (V1.y * V2.z) - (V1.z * V2.y)
  Cross.x:= ((FUpVector.y * vVector.z) - (FUpVector.z * vVector.y));
  // The Y value for the vVector is:  (V1.z * V2.x) - (V1.x * V2.z)
  Cross.y:= ((FUpVector.z * vVector.x) - (FUpVector.x * vVector.z));
  // The Z value for the vVector is:  (V1.x * V2.y) - (V1.y * V2.x)
  Cross.z:= ((FUpVector.x * vVector.y) - (FUpVector.y * vVector.x));

  // Add the resultant vVector to our position
  FPosition.x:= FPosition.x + Cross.x * amount;
  FPosition.z:= FPosition.z + Cross.z * amount;
  // Add the resultant vVector to our view
  FView.x:= FView.x + Cross.x * amount;
  FView.z:= FView.z + Cross.z * amount;
end;

procedure TCammra.MoveCamera( amount: single );
var
  vVector: T3DVector;
begin
  vVector.x:= FView.x - FPosition.x;
  vVector.y:= FView.y - FPosition.y;
  vVector.z:= FView.z - FPosition.z;

  FPosition.x:= FPosition.x + vVector.x * amount;
  FPosition.z:= FPosition.z + vVector.z * amount;
  FView.x:= FView.x + vVector.x * amount;
  FView.z:= FView.z + vVector.z * amount;	
end;

end.
