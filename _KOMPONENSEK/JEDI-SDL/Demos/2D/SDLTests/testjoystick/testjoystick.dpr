program testjoystick;
{******************************************************************************}
{                                                                              }
{                     Joystick test/example                                    }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{                                                                              }
{ Portions created by Ariel Jacob are                                          }
{ Copyright (C) 2000 - 2002 Ariel Jacob.                                       }
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
{   Joystick test/example                                                      }
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

uses
  SysUtils,
  SDL,
  Logger;

const
  AppTitle = 'Joystick Test (by A.J)';

var
  event : TSDL_Event;
  JS    : PSDL_Joystick;
  X, Y,                           // To store *my*(4 Axes) Joystick info.
  Thrust, Spin  : Integer;
  I, numJS, WJS : Integer;        
  Done          : Boolean = False;

begin
  // Init the SDL
  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_JOYSTICK ) < 0 then
  begin
    Log.LogError(Format('initialization failed: %s\n',
      [SDL_GetError]), 'Start');
    Halt(1);
  end;

  // setup a screen
  if (SDL_SetVideoMode(512, 50, 8, 0) = nil) then
  begin
    Log.LogError(Format('Video initialization failed: %s\n',
      [SDL_GetError]), 'Start');
    Halt(1);
  end;

  SDL_WM_SetCaption(AppTitle, nil);

  // How many JS we have?
  numJS:= SDL_NumJoysticks;
  Log.LogStatus(Format('%d Joystick(s) found',
      [numJS]), 'Start');
  if numJS = 0 then
    Halt(2);

  WJS:= -1;
  // Get the JS info
  for I:= 0 to numJS-1 do
  begin
    JS:= SDL_JoystickOpen(I);
    if js = nil then
      Log.LogWarning(Format('Joystick %d faild',
        [I]), 'loop')
    else begin
      if WJS = -1 then
        WJS:= I;
      Log.LogStatus(Format('Name:        %s',
        [SDL_JoystickName(I)]), 'joy info');
      Log.LogStatus(Format('Axes:        %d',
        [SDL_JoystickNumAxes(JS)]), 'joy info');
      Log.LogStatus(Format('Track-Balls: %d',
        [SDL_JoystickNumBalls(JS)]), 'joy info');
      Log.LogStatus(Format('Hats:        %d',
        [SDL_JoystickNumHats(JS)]), 'joy info');
      Log.LogStatus(Format('Buttons:     %d',
        [SDL_JoystickNumButtons(JS)]), 'joy info');

      SDL_JoystickClose(JS);
    end;
  end;

  // using the first wrking JS
  JS:= SDL_JoystickOpen(WJS);
  X:= 0;
  Y:= 0;
  Thrust:= 0;
  Spin:=0;

  SDL_WM_SetCaption(PChar(Format(AppTitle +
    ' - X: %d, Y: %d, Thrust: %d, Spin: %d', [X, Y, Thrust, Spin])), nil);

  while NOT Done do
  begin
    while (SDL_PollEvent(@event) <> 0) do
    begin
      case (event.type_) of
        SDL_KEYDOWN:
          begin
            case event.key.keysym.sym of
              SDLK_ESCAPE:
                begin
                  done := True;
                end;
            end;
          end;

        SDL_JOYAXISMOTION:
          begin
            case event.jaxis.axis of
              0: X:= event.jaxis.value;
              1: Y:= event.jaxis.value*-1;
              2: Thrust:= event.jaxis.value*-1;
              3: Spin:= event.jaxis.value;
            end;
            SDL_WM_SetCaption(PChar(Format(AppTitle +
              ' - X: %d, Y: %d, Thrust: %d, Spin: %d', [X, Y, Thrust, Spin])), nil);
          end;

        SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP:
          begin
          end;

        SDL_QUITEV:
          done := True;
      end;
    end;

    // don't hang the computer, time here is not critical.
    SDL_Delay(150);
  end;

  SDL_JoystickClose(JS);
  SDL_Quit;
end.
