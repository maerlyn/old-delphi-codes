program ZTest;
{******************************************************************}
{                                                                  }
{       Borland Delphi/Kylix SDL Shooting Example                  }
{                                                                  }
{ Portions created by Róbert Kisnémeth <mikrobi@freemail.hu>, are  }
{ Copyright (C) 2001 Róbert Kisnémeth.                             }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : ZTest.dpr                          }
{ The initial developer of the Pascal code is :                    }
{ Róbert Kisnémeth <mikrobi@freemail.hu>                           }
{                                                                  }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{   ZTest : This testprogram demonstrates the usage of Z coords    }
{   Warning! No any errorcheckings!                                }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, in your path .                     }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This demo shows how to use the SDL, SDL_Mixer and SDL_Image    }
{   libraries. It shows how to do basic collision detection and    }
{   how to handle keyboard events.                                 }
{   YOU MUST HAVE "PLAY_MID" and "WAV_MUSIC" conditionally defined }
{   to be able to compile this demo                                }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{  August   12 2001 - RK : Initial translation.                    }
{  August   17 2001 - DL : Added Kylix Code                        }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  Logger,
  SDL,
  SDLSprites in '../../Pas/SDLSprites.pas';

const
  TICK_INTERVAL = 1000 div 30;

type
  TMySprite = class(TSprite)
    xi: integer;
    AnimCounter: integer;
    constructor Create;
    procedure Move; override;
  end;

constructor TMySprite.Create;
begin
  inherited Create('../images/blocks.bmp', 16, 16);
    { the image is 32 pixel width, the sprite is 16 pixel width, so there are 2 animation phases }
  z := random(100);
  xi := 1 + z div 20;
  y := random(240 - 16);
  x := random(320 - 16);
  AnimCounter := random(30);
end;

procedure TMySprite.Move;
begin
  x := x + xi;
  if x > 320 then
    x := -16;
  if x < -16 then
    x := 320;
  inc(AnimCounter);
  if AnimCounter = 30 then
  begin
    AnimCounter := 0;
    inc(AnimPhase);
    if AnimPhase = NumberOfFrames then
      AnimPhase := 0;
  end;
end;

var
  Screen, Background: PSDL_Surface;
  SpriteEngine: TSpriteEngine;
  i: integer;
  Next_Time: cardinal = 0;
  Quit: boolean = false;
  Event: TSDL_Event;

function TimeLeft: UInt32;
var
  now: UInt32;
begin
  now := SDL_GetTicks;
  if next_time <= now then
  begin
    next_time := now + TICK_INTERVAL;
    result := 0;
    exit;
  end;
  result := next_time - now;
end;

begin
  randomize;
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
    halt;

  // Set the title bar in environments that support it
  SDL_WM_SetCaption('Sprite Engine : Z Order Test', nil);

  Screen := SDL_SetVideoMode(320, 240, 16, SDL_SWSURFACE);

  Background := SDL_LoadBMP('../images/background.bmp');
  
  SpriteEngine := TSpriteEngine.Create(Screen);
  SpriteEngine.BackgroundSurface := Background;
  for i := 0 to 99 do
    SpriteEngine.AddSprite(TMySprite.Create);
  { now we draw the screen : the background and the sprites }
  SDL_BlitSurface(Background, nil, Screen, nil);
  SpriteEngine.Draw;
  { the main cycle }
  repeat
    while SDL_PollEvent(@Event) > 0 do
    begin
      case Event.key.type_ of
        SDL_QUITEV :
        begin
          Quit := true;
        end;
        
        SDL_KeyDown :
        begin
          if Event.key.keysym.sym = SDLK_ESCAPE then
            Quit := true;
        end;
      end;
    end;
    { remove all sprites from the screen and move all sprites }
    SpriteEngine.Move;

    { at this point you may modify the Background surface. If you modify this,
     then you must modify the screen surface too. Now they are the same. }

    { redraw all sprites to the screen in new positions }
    SpriteEngine.Draw;
    { update changes on screen }
    SDL_UpdateRects(Screen, SpriteEngine.NumberOfRects, @SpriteEngine.UpdateRects[0]);
    SDL_Delay(TimeLeft);
  until Quit = true;
  SpriteEngine.Free;
  SDL_FreeSurface(Background);
  SDL_Quit;
end.

