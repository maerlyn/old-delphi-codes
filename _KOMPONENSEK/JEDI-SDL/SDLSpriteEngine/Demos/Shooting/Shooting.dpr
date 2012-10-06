program Shooting;
{******************************************************************}
{                                                                  }
{       Borland Delphi/Kylix SDL Shooting Example                  }
{                                                                  }
{ Portions created by Róbert Kisnémeth <mikrobi@freemail.hu>, are  }
{ Copyright (C) 2001 Róbert Kisnémeth.                             }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : Shooting.dpr                       }
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
{   SHOOTING: Shooting sample code.                                }
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
  idPLAYER = 1;
  idLASER = 2;
  idENEMY = 3;

var
  Screen, Background, Graphics: PSDL_Surface;
  SpriteEngine: TSpriteEngine;
  next_time: cardinal = 0;
  ScrollCounter: cardinal = 0;
  EnemyRate: cardinal = 0;
  Quit: boolean = false;
  Event: TSDL_Event;
  Keys: PKeyStateArr;

type
  TPlayer = class(TSprite)
    Rotor: integer;
    ShootCounter: integer;
    DestRect: TSDL_Rect;
    Rects: array[0..1, 0..6] of TSDL_Rect;
    constructor Create;
    procedure Draw; override;
    procedure Move; override;
    procedure GetCollisionRect(Rect: PSDL_Rect); override;
  end;

  TLaser = class(TSprite)
    DestRect: TSDL_Rect;
    constructor Create(_x, _y: integer);
    procedure Draw; override;
    procedure Move; override;
  end;

  TEnemy = class(TSprite)
    Hull: integer;
    DestRect: TSDL_Rect;
    Speed, SpeedCounter: integer;
    Rotor: integer;
    Rects: array[0..1] of TSDL_Rect;
    constructor Create;
    procedure HitByLaser;
    procedure Draw; override;
    procedure Move; override;
    procedure DetectCollision;
    procedure GetCollisionRect(Rect: PSDL_Rect); override;
  end;

  TExplosion = class(TSprite)
    DestRect: TSDL_Rect;
    Rects: array[0..7] of TSDL_Rect;
    Speed, SpeedCounter: integer;
    constructor Create(_x, _y, _Speed: integer);
    procedure Move; override;
    procedure Draw; override;
  end;

constructor TExplosion.Create(_x, _y, _Speed: integer);
var
  i: integer;
begin
  inherited Create('', 32, 32);
  for i := 0 to 7 do
  begin
    Rects[i].x := i * 32;
    Rects[i].y := 96;
    Rects[i].w := 32;
    Rects[i].h := 32;
  end;
  Speed := _Speed;
  x := _x;
  y := _y;
  z := 200;
  SpeedCounter := 0;
end;

procedure TExplosion.Move;
begin
  inc(SpeedCounter, Speed);
  if SpeedCounter >= 32 then
  begin
    while SpeedCounter >= 32 do
    begin
      dec(SpeedCounter, 32);
      inc(y);
    end;
    if y > 240 then
      Kill;
  end;
  inc(AnimPhase);
  if AnimPhase = 8 * 4 then
    Kill;
end;

procedure TExplosion.Draw;
begin
  DestRect.x := x;
  DestRect.y := y;
  SDL_BlitSurface(Graphics, @Rects[AnimPhase shr 2], Surface, @DestRect);
  PrevRect := DestRect;
end;

constructor TEnemy.Create;
var
  i: integer;
begin
  inherited Create('', 32, 32);
  for i := 0 to 1 do
  begin
    Rects[i].x := 336 + i * 32;
    Rects[i].y := 0;
    Rects[i].w := 32;
    Rects[i].h := 32;
  end;
  Hull := 100;
  id := idENEMY;
  Rotor := 0;
  Speed := 8 + random(56);
  SpeedCounter := 0;
  x := random(320 - 32);
  y := -32;
  z := random(50);
end;

procedure TEnemy.GetCollisionRect(Rect: PSDL_Rect);
begin
  Rect.x := x + 1;
  Rect.y := y + 4;
  Rect.w := 30;
  Rect.h := 23;
end;

procedure TEnemy.Move;
begin
  Rotor := 1 - Rotor;
  inc(SpeedCounter, Speed);
  if SpeedCounter >= 32 then
  begin
    while SpeedCounter >= 32 do
    begin
      dec(SpeedCounter, 32);
      inc(y);
    end;
    if y > 240 then
      Kill;
  end;
  DetectCollision;
end;

procedure TEnemy.Draw;
begin
  DestRect.x := x;
  DestRect.y := y;
  SDL_BlitSurface(Graphics, @Rects[Rotor], Surface, @DestRect);
  PrevRect := DestRect;
end;

procedure TEnemy.DetectCollision;
var
  i: integer;
  MyRect, NMERect: TSDL_Rect;
begin
  GetCollisionRect(@MyRect);
  for i := 0 to ParentList.Count - 1 do
    if ParentList[i] <> Self then
    begin
      ParentList[i].GetCollisionRect(@NMERect);
      if isCollideRects(@MyRect, @NMERect) then
      begin
        if ParentList[i].ID = idLASER then
        begin
          ParentList[i].Kill;
          HitByLaser; { myself }
        end
        else if ParentList[i].ID = idPLAYER then
        begin
          ParentList[i].Kill; { Player }
          SpriteEngine.AddSprite(TExplosion.Create(ParentList[i].x + 8,
            ParentList[i].y + 8, 0));
          Kill; { myself }
          SpriteEngine.AddSprite(TExplosion.Create(x, y, Speed));
        end;
      end;
    end;
end;

procedure TEnemy.HitByLaser;
begin
  Hull := Hull - 30;
  if Hull < 0 then
  begin
    Kill;
    SpriteEngine.AddSprite(TExplosion.Create(x, y, Speed));
  end;
end;

constructor TLaser.Create(_x, _y: integer);
begin
  inherited Create('', 3, 15);
  id := idLASER;
  x := _x;
  y := _y;
  z := 50;
  SrcRect.x := 336;
  SrcRect.y := 64;
  SrcRect.w := 3;
  SrcRect.h := 15;
end;

procedure TLaser.Move;
begin
  if y > -15 then
    dec(y, 2)
  else
    Kill;
end;

procedure TLaser.Draw;
begin
  DestRect.x := x;
  DestRect.y := y;
  SDL_BlitSurface(Graphics, @SrcRect, Surface, @DestRect);
  PrevRect := DestRect;
end;

constructor TPlayer.Create;
var
  i: integer;
begin
  inherited Create('', 48, 48);
  id := idPLAYER;
  z := 100;
  for i := 0 to 6 do
  begin
    Rects[0, i].x := i * 48;
    Rects[0, i].y := 0;
    Rects[0, i].w := 48;
    Rects[0, i].h := 48;
    Rects[1, i].x := i * 48;
    Rects[1, i].y := 48;
    Rects[1, i].w := 48;
    Rects[1, i].h := 48;
  end;
  ShootCounter := 0;
  Rotor := 0;
  AnimPhase := 3 * 4;
  x := 160 - 24;
  y := 239 - 48;
end;

procedure TPlayer.GetCollisionRect(Rect: PSDL_Rect);
begin
  Rect.y := y + 8;
  Rect.h := 24;
  case AnimPhase of
    0..3:
      begin
        Rect.x := x + 10;
        Rect.w := 30;
      end;
    4..7:
      begin
        Rect.x := x + 7;
        Rect.w := 35;
      end;
    8..11, 16..19:
      begin
        Rect.x := x + 5;
        Rect.w := 38;
      end;
    12..15:
      begin
        Rect.x := x + 4;
        Rect.w := 40;
      end;
    20..23:
      begin
        Rect.x := x + 6;
        Rect.w := 35;
      end;
    24..27:
      begin
        Rect.x := x + 8;
        Rect.w := 30;
      end;
  end;
end;

procedure TPlayer.Move;
begin
  Rotor := 1 - Rotor;
  if (Keys[SDLK_LEFT] = 1) and (x > 0) then
  begin
    dec(x);
    if AnimPhase > 0 then
      dec(AnimPhase);
  end
  else if (Keys[SDLK_RIGHT] = 1) and (x < 320 - 48) then
  begin
    inc(x);
    if AnimPhase < 27 then
      inc(AnimPhase);
  end
  else
  begin
    if AnimPhase < 12 then
      inc(AnimPhase)
    else if AnimPhase > 12 then
      dec(AnimPhase);
  end;
  if (Keys[SDLK_UP] = 1) and (y > 0) then
    dec(y)
  else if (Keys[SDLK_DOWN] = 1) and (y < 240 - 48) then
    inc(y);
  if (Keys[SDLK_SPACE] = 1) and (ShootCounter = 0) then
  begin
    SpriteEngine.AddSprite(TLaser.Create(x + 22, y));
    inc(ShootCounter);
  end;
  if ShootCounter > 0 then
  begin
    inc(ShootCounter);
    if ShootCounter = 20 then
      ShootCounter := 0;
  end;
end;

procedure TPlayer.Draw;
begin
  DestRect.x := x;
  DestRect.y := y;
  SDL_BlitSurface(Graphics, @Rects[Rotor, AnimPhase shr 2], Surface, @DestRect);
  PrevRect := DestRect;
end;

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

procedure DrawForest;
var
  x, y: integer;
  Src, Dest: TSDL_Rect;
begin
  Src.w := 32;
  Src.h := 32;
  Src.y := 32;
  for y := 0 to 7 do
    for x := 0 to 9 do
    begin
      if random(2) = 0 then
        Src.x := 336
      else
        Src.x := 368;
      Dest.x := x * 32;
      Dest.y := y * 32;
      SDL_BlitSurface(Graphics, @Src, Background, @Dest);
    end;
  SDL_BlitSurface(Background, nil, Screen, nil);
end;

procedure ScrollForest;
var
  y: integer;
  Row: array[0..319] of byte;
  MustLock: boolean;
  Video1, Video2: cardinal;
begin
  MustLock := SDL_MustLock(Background);
  if MustLock then
    SDL_LockSurface(Background);
  Video1 := cardinal(Background.pixels) + 238 * Background.pitch; { from }
  Video2 := cardinal(Background.pixels) + 239 * Background.pitch; { to }
  { store lowest row }
  Move(pointer(Video2)^, Row[0], 320);
  for y := 0 to 238 do
  begin
    Move(pointer(Video1)^, pointer(Video2)^, 320);
    dec(Video1, Background.pitch);
    dec(Video2, Background.pitch);
  end;
  { restore uppest row }
  Move(Row[0], Background.Pixels^, 320);
  if MustLock then
    SDL_UnlockSurface(Background);
  SDL_BlitSurface(Background, nil, Screen, nil);
end;

begin
  if SDL_Init(SDL_INIT_VIDEO) <> 0 then
    halt;
  // Set the title bar in environments that support it
  SDL_WM_SetCaption('Sprite Engine : Shooting', nil);

  Screen := SDL_SetVideoMode(320, 240, 8, SDL_SWSURFACE or SDL_HWPALETTE);
  if Screen = nil then
  begin
    SDL_Quit;
    halt;
  end;

  Graphics := SDL_LoadBMP('../images/graphics.bmp');
  if Graphics = nil then
  begin
    SDL_Quit;
    halt;
  end;
  
  SDL_SetColorKey(Graphics, SDL_HWACCEL or SDL_RLEACCEL or SDL_SRCCOLORKEY, 0);
  Background := SDL_CreateRGBSurface(SDL_SWSURFACE or SDL_HWPALETTE, 320, 240,
    8, 0, 0, 0, 0);
  if Background = nil then
  begin
    SDL_Quit;
    halt;
  end;
  SDL_SetColors(Screen, @Graphics.format.palette.colors[0], 0, 256);
  SDL_SetColors(Background, @Graphics.format.palette.colors[0], 0, 256);
  SDL_ShowCursor(SDL_DISABLE);
  SpriteEngine := TSpriteEngine.Create(Screen);
  SpriteEngine.BackgroundSurface := Background;
  SpriteEngine.AddSprite(TPlayer.Create);
  { now we draw the screen : the background and the sprites }
  randomize;
  DrawForest;
  SpriteEngine.Draw;
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
    
    keys := PKeyStateArr(SDL_GetKeyState(nil));
    { remove all sprites from the screen and move all sprites }
    SpriteEngine.Move;
    { Changing background }
    inc(ScrollCounter);
    if ScrollCounter = 5 then
    begin
      ScrollForest;
      ScrollCounter := 0;
    end;
    inc(EnemyRate);
    if EnemyRate = 30 then
    begin
      SpriteEngine.AddSprite(TEnemy.Create);
      EnemyRate := 0;
    end;
    { redraw all sprites to the screen in new positions }
    SpriteEngine.Draw;
    { update changes on screen }
    // Do not use SDL_UPDATERECTS() 'coz the we need to refresh the whole screen
    SDL_UpdateRect(Screen, 0, 0, 0, 0);
    SDL_Delay(TimeLeft);
  until Quit = true;
  SpriteEngine.Free;
  SDL_FreeSurface(Graphics);
  SDL_Quit;
end.

