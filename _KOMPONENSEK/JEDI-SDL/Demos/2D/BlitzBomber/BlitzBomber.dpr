program BlitzBomber;
{******************************************************************}
{                                                                  }
{       Borland Delphi/Kylix SDL Blitz Bomber v1.2                 }
{                                                                  }
{ Portions created by Róbert Kisnémeth <mikrobi@freemail.hu>, are  }
{ Copyright (C) 2001 Róbert Kisnémeth.                             }
{ A game written by KiCHY dESiGN on 07/2001.                       }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : BlitzBomber.dpr                    }
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
{   BlitzBomber : This program is a very simple game.              }
{   You must destroy all skyscrapers to land safely. You can       }
{   destroy these buildings with your bomb. You can drop a bomb    }
{   with SPACE key. You can't drop another bomb until the previous }
{   bomb was destroyed. A bomb can't destroy a whole building just }
{   the upper 10 levels including roofs, etc.                      }
{   When you safely landed you completed the game. It's not so     }
{   easy as you think!                                             }
{   During the game you can press ALT+ENTER to toggle              }
{   fullscreen/windowed mode. Any ideas & hints & opinions may go  }
{   to: mikrobi@freemail.hu                                        }
{                                                                  }
{   This game and source code is completely free. Use at your      }
{   own risk. There is no guarantee for any harm caused by this    }
{   program to your computer.                                      }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, in your path .                     }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This demo shows how to use the SDL, Sorry for the chaotic code }
{   and the lack of comments.                                      }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{    July   13 2001 - RK : The first public version v 1.1.         }
{  August   11 2001 - RK : Major speed improvements                }
{                          On a 200MHz Pentium the game runs app.  }
{                           40% faster!                            }
{                          Drawing routines changed                }
{  August   17 2001 - DL : Added Kylix Code                        }
{September  14 2001 - DL : Removed need 4 Windows, QForms unit and }
{                          Added Logging.                          }
{                          Added ability to shut game by closing   }
{                          the window.                             }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  Logger,
  SDL;

const
  ImageStr = 'abcdefghijklmnopqrstuvwxyz0123456789-!.[]}<>B@"#_EZXCV';
  TICK_INTERVAL = trunc(1000 div 20);
  VERSION_NO = '1.3';

type
  TScene = (scTitleProcess, scGameSetup, scGameProcess, scEndProcess, scFinish);
  TCloud = object
    SrcRect, DestRect: TSDL_Rect;
    x, y: integer;
    Type_: integer;
    xi: integer;
    procedure Move;
    procedure Draw;
    procedure Remove;
  end;

  TBomb = object
    DestRect, SrcRect: TSDL_Rect;
    x, y: integer;
    Lifetime: integer;
    Falls: boolean;
    procedure Remove;
    procedure Draw;
    procedure Move;
  end;

  TAirplane = object { This object controls the bomb object! }
    SrcRect, DestRect: TSDL_Rect;
    x, x2, y, y2: integer;
    Rotor: byte;
    Bomb: TBomb;
    Stage: byte;
    procedure Move;
    procedure Remove;
    procedure Draw;
    procedure LaunchBomb;
  end;

var
  CloudArray: array[1..10] of TCloud;
  SurfaceLost: boolean = false;
  Airplane: TAirplane;
  next_time: UInt32 = 0;
  Surface, Background, Lines, Images, Logo, Clouds: PSDL_Surface;
  Event: TSDL_Event;
  colors: array[0..255] of TSDL_Color;
  Map: array[0..1199] of byte;
  Scene: TScene;
  H1Tile, H2Tile, GroundTile: byte;
  ScreenMode: cardinal = 0;

procedure TCloud.Move;
begin
  x := x + xi;
  if x > 320 * 16 then
  begin
    x := x - 320 * 16 - 40 * 16;
    y := random(50);
    Type_ := random(3);
    xi := 1 + random(8);
  end;
end;

procedure TCloud.Remove;
begin
  SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
end;

procedure TCloud.Draw;
begin
  DestRect.x := x div 16;
  DestRect.y := y;
  SrcRect.x := 0;
  SrcRect.w := 40;
  case Type_ of
    0:
      begin
        SrcRect.y := 0;
        SrcRect.h := 16;
      end;
    1:
      begin
        SrcRect.y := 16;
        SrcRect.h := 16;
      end;
    2:
      begin
        SrcRect.y := 32;
        SrcRect.h := 8;
      end;
  end;
  SDL_BlitSurface(Clouds, @SrcRect, Surface, @DestRect);
end;

procedure TBomb.Move;
var
  SrcRect2, DestRect2: TSDL_Rect;
  Position: integer;
  What: byte;
begin
  inc(y);
  if y = 225 then
  begin { The bomb reaches the ground }
    Falls := false;
    exit;
  end;
  if y mod 8 = 0 then
  begin
    Position := (y div 8) * 40 + x div 8;
    What := map[Position];
    map[Position] := 0;
    SrcRect2.x := 0;
    SrcRect2.y := y;
    DestRect2.x := x;
    DestRect2.y := y;
    SDL_BlitSurface(Lines, @SrcRect2, Background, @DestRect2);
      { clear the block behind the bomb }
    if (What = H1Tile) or (What = H2Tile) then
    begin
      inc(LifeTime);
      if LifeTime = 10 then
      begin
        Falls := false;
        if Map[Position + 40] <> GroundTile then
        begin { modify the top level of house }
          Map[Position] := Map[Position + 40] + 1;
          SrcRect2.x := Map[Position] shl 3;
          SrcRect2.y := 0;
          SrcRect2.w := 8;
          SDL_BlitSurface(Images, @SrcRect2, Background, @DestRect2);
            { clear the block behind the bomb }
          SDL_BlitSurface(Images, @SrcRect2, Surface, @DestRect2);
            { clear the block behind the bomb }
        end;
        exit;
      end;
    end;
  end;
end;

procedure TBomb.Draw;
begin
  SrcRect.x := 45 * 8;
  SrcRect.y := 0;
  SrcRect.w := 8;
  SrcRect.h := 8;
  DestRect.x := x;
  DestRect.y := y;
  SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
end;

procedure TBomb.Remove;
begin
  SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
end;

procedure TAirPlane.Remove;
begin
  DestRect.x := x;
  DestRect.y := y;
  DestRect.h := 8;
  if Stage = 0 then
  begin
    DestRect.w := 16;
    SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
  end
  else
  begin
    DestRect.w := 8;
    SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
    DestRect.x := x2;
    DestRect.y := y2;
    SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
  end;
  Bomb.Remove;
end;

procedure TAirplane.Draw;
begin
  SrcRect.y := 0;
  SrcRect.w := 8;
  SrcRect.h := 8;
  DestRect.x := x;
  DestRect.y := y;
  if Stage = 0 then
  begin
    SrcRect.x := 40 * 8;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    SrcRect.x := 41 * 8 + Rotor * 8;
    DestRect.x := x + 8;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    Rotor := 1 - Rotor;
  end
  else
  begin
    SrcRect.x := 43 * 8;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    SrcRect.x := 44 * 8;
    DestRect.x := x2;
    DestRect.y := y2;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
  end;
  if Bomb.Falls then
    Bomb.Draw;
end;

procedure TAirplane.LaunchBomb;
begin
  if Stage = 0 then
  begin
    Bomb.Falls := true;
    Bomb.x := (x and $FFF8) + 8;
    Bomb.y := y;
    Bomb.Lifetime := 0;
  end;
end;

procedure TAirplane.Move;
var
  Position: integer;
begin
  if Bomb.Falls then
    Bomb.Move;
  if Stage > 6 then
    exit;
  if Stage = 0 then
  begin
    inc(x);
    if (x mod 8 = 0) and (x > -1) and (x < 305) then
    begin
      if (x = 304) and (y = 224) then
      begin
        Scene := scEndProcess;
        exit;
      end;
      Position := (y div 8) * 40 + (x div 8) + 2;
      if map[Position] <> 0 then
      begin
        Stage := 1;
        x2 := x + 8;
        y2 := y;
      end;
    end;
    if x = 320 then
    begin
      x := -16;
      y := y + 8;
    end;
  end
  else
  begin
    if y mod 8 = 0 then
    begin
      Position := (y div 8) * 40 + (x div 8);
      if (map[Position] = 0) and (y < 224) then
        inc(y)
      else
        Stage := Stage or 2;
    end
    else
      inc(y);
    if y2 mod 8 = 0 then
    begin
      Position := (y2 div 8) * 40 + (x2 div 8);
      if (map[Position] = 0) and (y2 < 224) then
        inc(y2)
      else
        Stage := Stage or 4;
    end
    else
      inc(y2);
  end;
end;

procedure Initialize(Flag: cardinal);
var
  i: byte;
  Error: boolean;
begin
  Error := false;
  if SDL_Init(SDL_INIT_VIDEO) = -1 then
  begin
    Log.LogError(Format('Could not initialize SDL : %s',
      [SDL_GetError]), 'Initialize');
    SDL_Quit;
    halt(1);
  end;

  Surface := SDL_SetVideoMode(320, 240, 8, SDL_SWSURFACE or SDL_HWPALETTE or
    Flag);
  if Surface = nil then
  begin
    Log.LogError(Format('Couldn''t set 320x240x8 video mode : %s',
      [SDL_GetError]), 'Initialize');
    Error := true;
  end;
  if not Error then
  begin
    Images := SDL_LoadBMP('images/Images256.bmp');
    if Images = nil then
    begin
      Log.LogError(Format('Couldn''t load image : %s',
      [SDL_GetError]), 'Initialize');
      Error := true;
    end;
    SDL_SetColorKey(Images, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(Images.format, 255, 0, 255));
    for i := 0 to 255 do
      Colors[i] := Images.Format.Palette.Colors[i];
    SDL_SetColors(Surface, @Colors, 0, 256);
  end;
  if not Error then
  begin
    Background := SDL_CreateRGBSurface(SDL_SWSURFACE or SDL_HWPALETTE, 320, 240,
      8, 0, 0, 0, 0);
    if Background = nil then
    begin
      Log.LogError(Format('Couldn''t create surface: %s',
      [SDL_GetError]), 'Initialize');
      Error := true;
    end;
    SDL_SetColors(Background, @Colors, 0, 256);
  end;
  if not Error then
  begin
    Lines := SDL_LoadBMP('images/Lines256.bmp');
    if Lines = nil then
    begin
      Log.LogError(Format('Couldn''t load image : %s',
      [SDL_GetError]), 'Initialize');
      Error := true;
    end;
  end;
  if not Error then
  begin
    Logo := SDL_LoadBMP('images/Logo256.bmp');
    if Logo = nil then
    begin
      Log.LogError(Format('Couldn''t load image : %s',
      [SDL_GetError]), 'Initialize');
      Error := true;
    end;
    SDL_SetColorKey(Logo, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,
      SDL_MapRGB(Logo.format, 255, 0, 255));
  end;
  if not Error then
  begin
    Clouds := SDL_LoadBMP('images/Clouds256.bmp');
    if Clouds = nil then
    begin
       Log.LogError(Format('Couldn''t load image : %s',
      [SDL_GetError]), 'Initialize');
      Error := true;
      halt(1);
    end;
  end;
  
  if Error then
  begin
    SDL_FreeSurface(Surface);
    SDL_FreeSurface(Images);
    SDL_FreeSurface(Lines);
    SDL_FreeSurface(Logo);
    SDL_Quit;
    Halt(1);
  end;
  SDL_SetColorKey(Clouds, SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL,

  SDL_MapRGB(Clouds.format, 255, 0, 255));

  SDL_WM_SetCaption('Blitz Bomber v' + VERSION_NO + ' by KiCHY dESiGN', nil);

  GroundTile := pos('E', ImageStr);
  H1Tile := pos('@', ImageStr);
  H2Tile := H1Tile + 2;
  for i := 1 to 6 do
  begin
    CloudArray[i].x := random(320) * 16;
    CloudArray[i].y := random(50);
    CloudArray[i].Type_ := random(3);
    CloudArray[i].xi := 1 + random(8);
  end;
end;

procedure Finalize;
begin
  SDL_FreeSurface(Surface);
  SDL_FreeSurface(Background);
  SDL_FreeSurface(Images);
  SDL_FreeSurface(Lines);
  SDL_FreeSurface(Logo);
  SDL_FreeSurface(Clouds);
  SDL_Quit;
end;

procedure ToggleVideo;
begin
  case ScreenMode of
    0:
      begin
        Finalize;
        ScreenMode := SDL_FULLSCREEN;
        Initialize(ScreenMode);
        SurfaceLost := true;
      end;
    SDL_FULLSCREEN:
      begin
        Finalize;
        ScreenMode := 0;
        Initialize(ScreenMode);
        SurfaceLost := true;
      end;
  end;
end;

procedure DrawBackground;
var
  x: smallint;
  DestRect: TSDL_Rect;
begin
  DestRect.x := 0;
  DestRect.y := 0;
  for x := 0 to 39 do
  begin
    SDL_BlitSurface(Lines, nil, Surface, @DestRect);
    inc(DestRect.x, 8);
  end;
  SDL_BlitSurface(Surface, nil, Background, nil);
end;

procedure DrawClouds;
var
  i: integer;
begin
  for i := 1 to 6 do
    CloudArray[i].Draw;
end;

procedure RemoveClouds;
var
  i: integer;
begin
  for i := 1 to 6 do
    CloudArray[i].Remove;
end;

procedure MoveClouds;
var
  i: integer;
begin
  for i := 1 to 10 do
    CloudArray[i].Move;
end;

procedure DrawImage(x, y: integer; const Text: string);
var
  i, j: integer;
  betu, DestRect: TSDL_Rect;
begin
  Betu.y := 0;
  Betu.w := 8;
  Betu.h := 8;
  DestRect.x := x;
  DestRect.y := y;
  for i := 0 to length(Text) - 1 do
  begin
    j := pos(Text[i + 1], ImageStr);
    if j > 0 then
    begin
      Betu.x := j shl 3;
      SDL_BlitSurface(Images, @Betu, Surface, @DestRect);
    end;
    inc(DestRect.x, 8);
  end;
end;

procedure DrawTitle;
var
  DestRect: TSDL_Rect;
begin
  DrawBackground;
  DestRect.x := (320 - Logo.w) div 2;
  DestRect.y := 30;
  SDL_BlitSurface(Logo, nil, Surface, @DestRect);
  DrawImage(160 - 22 * 4, 80, 'a game by kichy design');
  DrawImage(160 - 11 * 4, 90, 'version '+VERSION_NO );
  DrawImage(160 - 7 * 4, 100, '09-2001');
  DrawImage(160 - 12 * 4, 120, 'instructions');
  DrawImage(160 - 12 * 4, 130, '------------');
  DrawImage(160 - 26 * 4, 140, 'press space to launch a bomb');
  DrawImage(160 - 17 * 4, 150, 'press esc to quit');
  DrawImage(160 - 37 * 4, 160, 'press alt-enter to toggle fullscreen');
  DrawImage(160 - 21 * 4, 210, 'press space to start!');
  SDL_BlitSurface(Surface, nil, Background, nil);
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

procedure TitleProcess;
begin
  { Draw title screen }
  DrawTitle;
  { Draw the clouds }
  DrawClouds;
  repeat
    while SDL_PollEvent(@event) > 0 do
    begin
      case event.type_ of
        SDL_QUITEV :
           Scene := scFinish;
           
        SDL_KEYDOWN :
        begin
          case Event.Key.keysym.sym of
            SDLK_SPACE :
              Scene := scGameSetup;
            SDLK_ESCAPE :
              Scene := scFinish;
            SDLK_RETURN :
            begin
              if (Event.Key.keysym.Modifier and KMOD_ALT <> 0) then
                ToggleVideo;
            end;
          end;
        end;
      end;
    end;
    { Do we redraw the screen? (Because ToggleFullscreen) }
    if SurfaceLost then
    begin
      DrawTitle;
      SurfaceLost := false;
    end;
    { Handle the clouds }
    RemoveClouds;
    MoveClouds;
    DrawClouds;
    SDL_Flip(Surface);
    SDL_Delay(TimeLeft);
  until Scene <> scTitleProcess;
end;

procedure DrawMap;
var
  x, y: integer;
  SrcRect, DestRect: TSDL_Rect;
begin
  SrcRect.y := 0;
  SrcRect.w := 8;
  SrcRect.h := 8;
  for x := 0 to 39 do
    for y := 0 to 29 do
      if map[y * 40 + x] > 0 then
      begin
        DestRect.x := x shl 3;
        DestRect.y := y shl 3;
        SrcRect.x := map[y * 40 + x] shl 3;
        SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
        SDL_BlitSurface(Images, @SrcRect, Background, @DestRect);
      end;
end;

procedure GameSetup;
var
  x, y, h: integer;
  house: byte;
  SrcRect, DestRect: TSDL_Rect;
begin
  { draw gackground lines }
  DrawBackground;
  SDL_BlitSurface(Surface, nil, Background, nil);
  SrcRect.y := 0;
  SrcRect.w := 8;
  SrcRect.h := 8;
  { clear house datas }
  fillchar(Map, sizeof(Map), 0);
  { Airplane to left-upper corner }
  Airplane.x := 0;
  AirPlane.y := 0;
  Airplane.Stage := 0; { The airplane flies }
  { no bomb on the screen }
  AirPlane.Bomb.Falls := false;
  { draw ground }
  DestRect.y := 232;
  SrcRect.x := 400;
  for x := 0 to 39 do
  begin
    map[29 * 40 + x] := 50;
    DestRect.x := x shl 3;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    SDL_BlitSurface(Images, @SrcRect, Background, @DestRect);
  end;
  { draw all houses }
  for x := 2 to 37 do
  begin
    h := 8 + random(21); { House height }
    { Draw a House }
    house := 46 + 2 * random(2); { House Type }
    SrcRect.x := house * 8;
    DestRect.x := x shl 3;
    for y := 28 downto h do
    begin
      map[y * 40 + x] := house;
      DestRect.y := y shl 3;
      SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
      SDL_BlitSurface(Images, @SrcRect, Background, @DestRect);
    end;
    { Top of House }
    map[y * 40 + x] := 51 + random(4);
    SrcRect.x := map[y * 40 + x] shl 3;
    DestRect.y := y shl 3;
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    SDL_BlitSurface(Images, @SrcRect, Background, @DestRect);
    { The cloud stuff }
    RemoveClouds;
    MoveClouds;
    DrawClouds;
    SDL_Flip(Surface);
    SDL_Delay(TimeLeft);
  end;
  Scene := scGameProcess;
end;

procedure GameProcess;
var
  Wait: integer;
begin
  Wait := 0;
  repeat
    while SDL_PollEvent(@event) > 0 do
    begin
      case event.type_ of
        SDL_QUITEV :
          Scene := scFinish;
           
        SDL_KEYDOWN:
        begin
          case Event.Key.keysym.sym of
            SDLK_SPACE :
            begin
             if (Airplane.Bomb.Falls = false) then
              Airplane.LaunchBomb;
            end;
            
            SDLK_ESCAPE :
              Scene := scTitleProcess;

            SDLK_RETURN :
            begin
              if (Event.Key.keysym.Modifier and KMOD_ALT <> 0) then
                ToggleVideo;
            end;
          end;
        end;
      end;
    end;
    if Airplane.Stage > 6 then
    begin { The airplane crashed down }
      inc(Wait);
      if Wait = 100 then
        Scene := scTitleProcess; { Back to title screen }
    end;
    RemoveClouds;
    Airplane.Remove;
    MoveClouds;
    Airplane.Move;
    if SurfaceLost then
    begin
      DrawBackground;
      DrawMap;
      SurfaceLost := false;
    end;
    DrawClouds;
    AirPlane.Draw;
    SDL_Flip(Surface);
    SDL_Delay(TimeLeft);
  until Scene <> scGameProcess;
end;

procedure EndProcess;
const
  Anim: array[0..3] of byte = (0, 1, 2, 1);
var
  SrcRect, DestRect: TSDL_Rect;
  Counter: integer;
  AnimPhase: byte;
  //i: integer;
begin
  Counter := 0;
  AnimPhase := 0;
  SrcRect.w := 8;
  SrcRect.h := 8;
  SrcRect.y := 0;
  DestRect.x := 296;
  DestRect.y := 224;
  repeat
    RemoveClouds;
    MoveClouds;
    DrawClouds;
    inc(Counter);
    inc(AnimPhase);
    if AnimPhase = 16 then
      AnimPhase := 0;
    SrcRect.x := 440 + Anim[AnimPhase shr 2] * 8;
    SDL_BlitSurface(Background, @DestRect, Surface, @DestRect);
    SDL_BlitSurface(Images, @SrcRect, Surface, @DestRect);
    SDL_Flip(Surface);
    SDL_Delay(TimeLeft);
  until Counter = 100;
  Scene := scTitleProcess;
end;

begin
  randomize;
  Initialize(ScreenMode);
  Scene := scTitleProcess;
  repeat
    case Scene of
      scTitleProcess: TitleProcess;
      scGameSetup: GameSetup;
      scGameProcess: GameProcess;
      scEndProcess: EndProcess;
    end;
  until Scene = scFinish;
  Finalize;
end.

