unit SDL_General;
{******************************************************************************}
{                                                                              }
{                     Helper/Common Unit                                       }
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
{   Helper/Common functions...                                                 }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
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

uses
  SDL;

type
  TFPSCalc = class
  protected
    FLastFps : cardinal;
    FInterval : cardinal;
    FLastTime : Uint32;
    FFrames : Cardinal;
    function Calc : cardinal;
  public
    property Interval : Cardinal read FInterval write FInterval;
    property FPS : Cardinal read Calc;
    procedure Count;
    constructor Create( Interval : cardinal );
  end;

  TFixedTime = class
  protected
    TICK_INTERVAL : cardinal;
    Next_Time : cardinal;
    procedure PutFPS( newFPS : cardinal );
    function GetFPS : cardinal;
  public
    property FPS : Cardinal read GetFPS write PutFPS;
    procedure Delay;
    function TimePassed : boolean;
    constructor Create( desiredFPS : cardinal );
  end;

implementation

{ -- TFPSCalc ----------------------------------------------------------------- }

constructor TFPSCalc.Create( Interval : cardinal );
begin
  inherited Create;
  FLastFps := 0;
  FInterval := Interval;
  FLastTime := SDL_GetTicks;
  FFrames := 0;
end;

function TFPSCalc.Calc : cardinal;
var
  DeltaTime : Uint32;
  Currenttime : Uint32;
begin
  result := FLastFps;
  Currenttime := SDL_GetTicks;
  DeltaTime := Currenttime - FLastTime;
  if DeltaTime >= FInterval then
  begin
    // whats faster round or Trunc ?
    FLastFps := Trunc( ( FLastFps * 0.2 ) + ( ( FFrames / DeltaTime ) * 1000 ) * 0.8 );
    result := FLastFps;
    FFrames := 0;
    FLastTime := Currenttime;
  end;
end;

procedure TFPSCalc.Count;
begin
  Inc( FFrames );
end;

{ TFixedTime }

constructor TFixedTime.Create( desiredFPS : cardinal );
begin
  inherited Create;
  PutFPS( desiredFPS );
  Next_Time := 0;
end;

procedure TFixedTime.Delay;
var
  now : UInt32;
begin
  now := SDL_GetTicks;
  if next_time <= now then
  begin
    next_time := now + TICK_INTERVAL;
    exit;
  end;
  SDL_Delay( next_time - now );
end;

function TFixedTime.GetFPS : cardinal;
begin
  result := ( TICK_INTERVAL * 1000 ) div 1;
end;

procedure TFixedTime.PutFPS( newFPS : cardinal );
begin
  TICK_INTERVAL := ( 1000 div newFPS ) * 1;
end;

function TFixedTime.TimePassed : boolean;
var
  now : UInt32;
begin
  now := SDL_GetTicks;
  result := False;
  if next_time <= now then
  begin
    next_time := now + TICK_INTERVAL;
    result := True;
  end;
end;

end.

