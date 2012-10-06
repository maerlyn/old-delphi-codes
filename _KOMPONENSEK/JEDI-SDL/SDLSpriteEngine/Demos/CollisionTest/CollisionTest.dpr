program CollisionTest;
{******************************************************************}
{                                                                  }
{       Borland Delphi/Kylix SDL Shooting Example                  }
{                                                                  }
{ Portions created by Róbert Kisnémeth <mikrobi@freemail.hu>, are  }
{ Copyright (C) 2001 Róbert Kisnémeth.                             }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : CollisionTest.dpr                  }
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
{   CollisionTest : Collision Test sample code.                    }
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
{  August    12 2001 - RK : Initial code                           }
{  August    17 2001 - DL : Added Kylix Code                       }
{  September 12 2001 - RK : Altered code to show collision better  }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  Logger,
  SDL,
  SDLSprites in '../../Pas/SDLSprites.pas';

const
  TICK_INTERVAL = 1000 div 30;
  idBLOCK = 1;

type
  TMySprite = class(TSprite)
    xi, yi: integer;
    PreviouslyCollided: boolean;
    constructor Create;
    procedure Move; override;
    procedure CollisionTest;
  end;

constructor TMySprite.Create;
begin
  inherited Create('../images/blocks.bmp', 16, 16);
    { the image is 32 pixel width, the sprite is 16 pixel width, so there are 2 animation phases }
  id := idBLOCK;
  xi := random(2);
  if xi = 0 then
    xi := -1;
  yi := random(2);
  if yi = 0 then
    yi := -1;
  y := random(240 - 16);
  x := random(320 - 16);
  PreviouslyCollided := false;
end;

procedure TMySprite.CollisionTest;
var
  Rect1, Rect2: TSDL_Rect;
  i: integer;
  WasCollision: boolean;
begin
  GetCollisionRect(@Rect1);
  WasCollision := false;
  for i := 0 to ParentList.Count - 1 do
    if (ParentList[i] <> self) and (TSprite(ParentList[i]).ID = idBLOCK) then
    begin
      ParentList[i].GetCollisionRect(@Rect2);
      if isCollideRects(@Rect1, @Rect2) then
      begin
        WasCollision := true;
        if PreviouslyCollided = false then
        begin
          AnimPhase := 1 - AnimPhase;
          PreviouslyCollided := true;
        end;
      end;
    end;
  if not WasCollision then
    PreviouslyCollided := false;
end;

procedure TMySprite.Move;
begin
  x := x + xi;
  y := y + yi;
  if x = 0 then
    xi := 1
  else if x = 319 - 16 then
    xi := -1;
  if y = 0 then
    yi := 1
  else if y = 239 - 16 then
    yi := -1;
  CollisionTest;
end;

var
  Screen, Background: PSDL_Surface;
  Next_Time: cardinal = 0;
  Event: TSDL_Event;
  SpriteEngine: TSpriteEngine;
  Quit: boolean = false;
  i: integer;

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
  SDL_WM_SetCaption('Sprite Engine : Collision Test', nil);
  Screen := SDL_SetVideoMode(320, 240, 16, SDL_SWSURFACE);
  Background := SDL_LoadBMP('../images/background.bmp');
  SpriteEngine := TSpriteEngine.Create(Screen);
  SpriteEngine.BackgroundSurface := Background;
  for i := 0 to 5 do
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

    { update changes on screen NEW!!! }
    { If it makes strange flashes and blinks then use SDL_UpdateRect(Screen, 0, 0, 0, 0) ! }
    SDL_UpdateRects(Screen, SpriteEngine.Sprites.Count*2, @SpriteEngine.UpdateRects[0]);
    SDL_Delay(TimeLeft);
  until Quit = true;
  SpriteEngine.Free;
  SDL_FreeSurface(Background);
  SDL_Quit;
end.

