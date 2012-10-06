program JEDISDLAliens;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Aliens Example                          }
{       Conversion of the SDL Alens Demo                           }
{                                                                  }
{ Portions created by Sam Lantinga <slouken@devolution.com>,  are  }
{ Copyright (C) 1998 Sam Lantinga.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : aliens.c                                }
{                                                                  }
{ The original Pascal code is : DelphiSDLMouse.dpr                 }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Matthias Thomas <ma.thoma@gmx.de>                                }
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
{   ALIENS: A silly little game demonstrating the SDL, SDL_Mixer   }
{           and SDL_Image libraries.                               }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, SDL_image and SDL_Mixer somewhere  }
{   in your path .                                                 }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
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
{   April   30 2001 - DL : Initial translation.                    }
{   June    30 2001 - DL : Delphi & Kylix unification of Code      }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  SDL,
  SDL_Mixer,
  SDL_Image,
  smpeg,
  Logger;

const
  WndClassName = 'SDL Aliens Demo'; // The Name Of Window's Class
  TITLE = 'JEDI-SDL Aliens Demo'; // Window Title
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;

{$IFDEF macintosh}
  DIR_SEP = ':';
  DIR_CUR = ':';
{$ELSE}
  DIR_SEP = '/';
  DIR_CUR = '';
{$ENDIF}

  FRAMES_PER_SEC = 50;
  PLAYER_SPEED = 4;
  MAX_SHOTS = 5;
  SHOT_SPEED = 6;
  MAX_ALIENS = 30;
  ALIEN_SPEED = 5;
  ALIEN_ODDS = (1 * FRAMES_PER_SEC);
  EXPLODE_TIME = 4;

  MAX_UPDATES = 3 * (1 + MAX_SHOTS + MAX_ALIENS);

type
  PSDL_SPRITE = ^TSDL_SPRITE;
  TSDL_SPRITE = record
    alive: integer;
    facing: integer;
    x, y: integer;
    image: PSDL_Surface;
  end;

  PBlit = ^TBlit;
  TBlit = record
    src: PSDL_Surface;
    srcrect: PSDL_Rect;
    dstrect: PSDL_Rect;
  end;

  PBlits = ^TBlits;
  TBlits = array[0..MAX_UPDATES] of TBlit;

  TSounds = (
    MUSIC_WAV,
    SHOT_WAV,
    EXPLODE_WAV,
    NUM_WAVES
    );
var
  path: string; // program's path
  screen : PSDL_SURFACE;
  background: PSDL_Surface;
  flags: UInt32;

  reloading: Boolean;
  player: TSDL_SPRITE;
  shots: array[0..MAX_SHOTS - 1] of TSDL_SPRITE;
  aliens: array[0..MAX_ALIENS - 1] of TSDL_SPRITE;
  explosions: array[0..MAX_ALIENS] of TSDL_SPRITE;

  numupdates: integer;
  srcupdate: array[0..MAX_UPDATES - 1] of TSDL_Rect;
  dstupdate: array[0..MAX_UPDATES - 1] of TSDL_Rect;

  Blits: TBlits;

{$IFDEF PLAY_MOD}
  music: PMix_Music;
{$ENDIF}
{$IFDEF PLAY_MID}
  music: PMix_Music;
{$ENDIF}
  sounds: array[0..3] of PMix_Chunk;

  {static variable}
  next_tick: Uint32;

  //---------------------------------------------

function LoadImage(DataFile: string; transparent: boolean): PSDL_Surface;
var
  image, surface: PSDL_Surface;
begin
  image := IMG_Load( PChar( DataFile ) );
  if (image = nil) then
  begin
    Log.LogError(Format('Couldn''t load image %s: %s',
      [DataFile, IMG_GetError]), 'LoadImage');
    result := nil;
    exit;
  end;

  if (transparent) then
  begin
    // Assuming 8-bit BMP image
    SDL_SetColorKey(image, (SDL_SRCCOLORKEY or SDL_RLEACCEL),
      PUint8(image.pixels)^);
  end;
  surface := SDL_DisplayFormat(image);
  SDL_FreeSurface(image);
  result := surface;
end;

function DataFile(filename: string): string;
begin
  result := PChar(DIR_CUR + 'data' + DIR_SEP + filename);
end;

function LoadData: Boolean;
var
  i: integer;
begin
  // Load sounds */
{$IFDEF PLAY_MOD}
  music := Mix_LoadMUS( PChar( DataFile('music.it') ) );
  if (music = nil) then
  begin
    Log.LogWarning(Format('Couldn''t load music: %s',
      [Mix_GetError]), 'LoadData');
  end;
{$ENDIF}
{$IFDEF PLAY_MID}
  music := Mix_LoadMUS( PChar( DataFile('music.mid') ) );
  if (music = nil) then
  begin
    Log.LogWarning(Format('Couldn''t load music: %s',
      [Mix_GetError]), 'LoadData');
  end;
{$ENDIF}


{$IFNDEF PLAY_MOD}
{$IFNDEF PLAY_MID}
  sounds[ord(MUSIC_WAV)] := Mix_LoadWAV( PChar( DataFile( 'music.wav' ) ) );
{$ENDIF}
{$ENDIF}
  sounds[ord(SHOT_WAV)] := Mix_LoadWAV( PChar( DataFile( 'shot.wav' ) ) );
  sounds[ord(EXPLODE_WAV)] := Mix_LoadWAV( PChar( DataFile( 'explode.wav' ) ) );

  // Load graphics
  player.image := LoadImage( DataFile('player.gif'), True);
  if (player.image = nil) then
  begin
    result := False;
    exit;
  end;

  shots[0].image := LoadImage(DataFile('shot.gif'), False);
  if (shots[0].image = nil) then
  begin
    result := False;
    exit;
  end;

  for i := 1 to MAX_SHOTS - 1 do
  begin
    shots[i].image := shots[0].image;
  end;

  aliens[0].image := LoadImage(DataFile('alien.gif'), True);
  if (aliens[0].image = nil) then
  begin
    result := False;
    exit;
  end;

  for i := 1 to MAX_ALIENS - 1 do
  begin
    aliens[i].image := aliens[0].image;
  end;

  explosions[0].image := LoadImage(DataFile('explosion.gif'), True);
  for i := 1 to MAX_ALIENS do
  begin
    explosions[i].image := explosions[0].image;
  end;

  background := LoadImage(DataFile('background.gif'), False);

  // Set up the update rectangle pointers
  for i := 0 to MAX_UPDATES - 1 do
  begin
    Blits[i].srcrect := @srcupdate[i];
    Blits[i].dstrect := @dstupdate[i];
  end;

  result := True;
end;

procedure FreeData;
var
  i: integer;
begin
  // Free sounds
{$IFDEF PLAY_MOD or def PLAY_MID}
  Mix_FreeMusic(music);
{$ENDIF}
  for i := 0 to Ord(NUM_WAVES) - 1 do
  begin
    Mix_FreeChunk(sounds[i]);
  end;

  // Free graphics
  SDL_FreeSurface(player.image);
  SDL_FreeSurface(shots[0].image);
  SDL_FreeSurface(aliens[0].image);
  SDL_FreeSurface(explosions[0].image);
  SDL_FreeSurface(background);
end;

procedure CreateAlien;
var
  i: integer;
begin
  // Look for a free alien slot
  for i := 0 to MAX_ALIENS - 1 do
  begin
    if (aliens[i].alive = 0) then
      break;
  end;

  if (i = MAX_ALIENS) then
    exit;

  // Figure out which direction it travels */
  while (aliens[i].facing = 0) do
  begin
    aliens[i].facing := (random(Round(now)) mod 3) - 1;
  end;

  // Figure out it's initial location */
  aliens[i].y := 0;
  if (aliens[i].facing < 0) then
  begin
    aliens[i].x := screen.w - aliens[i].image.w - 1;
  end
  else
  begin
    aliens[i].x := 0;
  end;

  aliens[i].alive := 1;
end;

procedure DrawObject(sprite: PSDL_SPRITE);
var
  update: PBlit;
begin
  update := @Blits[numupdates];
  inc(numupdates);
  update.src := sprite.image;
  update.srcrect.x := 0;
  update.srcrect.y := 0;
  update.srcrect.w := sprite.image.w;
  update.srcrect.h := sprite.image.h;
  update.dstrect.x := sprite.x;
  update.dstrect.y := sprite.y;
  update.dstrect.w := sprite.image.w;
  update.dstrect.h := sprite.image.h;
end;

procedure EraseObject(sprite: PSDL_SPRITE);
var
  update: PBlit;
  wrap: integer;
begin
  // The background wraps horizontally across the screen */
  update := @Blits[numupdates];
  inc(numupdates);
  update.src := background;
  update.srcrect.x := sprite.x mod background.w;
  update.srcrect.y := sprite.y;
  update.srcrect.w := sprite.image.w;
  update.srcrect.h := sprite.image.h;
  wrap := (update.srcrect.x + update.srcrect.w) - (background.w);
  if (wrap > 0) then
  begin
    update.srcrect.w := update.srcrect.w - wrap;
  end;

  update.dstrect.x := sprite.x;
  update.dstrect.y := sprite.y;
  update.dstrect.w := update.srcrect.w;
  update.dstrect.h := update.srcrect.h;

  // Assuming sprites can only wrap across one background tile */
  if (wrap > 0) then
  begin
    update := @Blits[numupdates];
    inc(numupdates);
    update.src := background;
    update.srcrect.x := 0;
    update.srcrect.y := sprite.y;
    update.srcrect.w := wrap;
    update.srcrect.h := sprite.image.h;
    update.dstrect.x := ((sprite.x div background.w) + 1) * background.w;
    update.dstrect.y := sprite.y;
    update.dstrect.w := update.srcrect.w;
    update.dstrect.h := update.srcrect.h;
  end;
end;

procedure UpdateScreen;
var
  i: integer;
begin
  for i := 0 to numupdates - 1 do
  begin
    SDL_LowerBlit(Blits[i].src, Blits[i].srcrect, screen, Blits[i].dstrect);
  end;
  SDL_UpdateRects(screen, numupdates, @dstupdate);
  numupdates := 0;
end;

function Collide(sprite1: PSDL_SPRITE; sprite2: PSDL_SPRITE): Boolean;
begin
  if ((sprite1.y >= (sprite2.y + sprite2.image.h)) or
    (sprite1.x >= (sprite2.x + sprite2.image.w)) or
    (sprite2.y >= (sprite1.y + sprite1.image.h)) or
    (sprite2.x >= (sprite1.x + sprite1.image.w))) then
  begin
    result := False;
    exit;
  end;
  result := True;
end;

procedure WaitFrame;
var
  this_tick: Uint32;
begin

  // Wait for the next frame */
  this_tick := SDL_GetTicks();
  if (this_tick < next_tick) then
  begin
    SDL_Delay(next_tick - this_tick);
  end;
  next_tick := this_tick + (1000 div FRAMES_PER_SEC);
end;

// This of course can be optimized :-)

procedure RunGame;
var
  i, j : integer;
  event: TSDL_Event;
  keys: PKeyStateArr;
  dst: TSDL_Rect;
begin
  // Paint the background */
  numupdates := 0;
  i := 0;
  while (i < screen.w - 1) do
  begin
    dst.x := i;
    dst.y := 0;
    dst.w := background.w;
    dst.h := background.h;
    SDL_BlitSurface(background, nil, screen, @dst);
    i := i + background.w;
  end;
  SDL_UpdateRect(screen, 0, 0, 0, 0);

  // Initialize the objects */
  player.alive := 1;
  player.x := (screen.w - player.image.w) div 2;
  player.y := (screen.h - player.image.h) - 1;
  player.facing := 0;
  DrawObject(@player);

  for i := 0 to MAX_SHOTS - 1 do
  begin
    shots[i].alive := 0;
  end;

  for i := 0 to MAX_ALIENS - 1 do
  begin
    aliens[i].alive := 0;
  end;

  CreateAlien;
  DrawObject(@aliens[0]);
  UpdateScreen;

  while (player.alive = 1) do
  begin
    // Wait for the next frame */
    WaitFrame;

    // Poll input queue, run keyboard loop */
    while (SDL_PollEvent(@Event) > 0) do
    begin
      case Event.type_ of
        SDL_MouseMotion:
        begin
          //Draw( Event.motion.X, Event.motion.Y );
        end;

        SDL_QuitEv:
        begin
          Exit;
        end;
      end;
    end;
    keys := PKeyStateArr(SDL_GetKeyState(nil));

    // Erase everything from the screen */
    for i := 0 to MAX_SHOTS - 1 do
    begin
      if (shots[i].alive = 1) then
      begin
        EraseObject(@shots[i]);
      end;
    end;
    for i := 0 to MAX_ALIENS - 1 do
    begin
      if (aliens[i].alive = 1) then
      begin
        EraseObject(@aliens[i]);
      end;
    end;
    EraseObject(@player);
    for i := 0 to MAX_ALIENS do
    begin
      if (explosions[i].alive = 1) then
      begin
        EraseObject(@explosions[i]);
      end;
    end;

    // Decrement the lifetime of the explosions */
    for i := 0 to MAX_ALIENS do
    begin
      if (explosions[i].alive > 0) then
      begin
        dec(explosions[i].alive);
      end;
    end;

    // Create new aliens */
    if ((random(Round(now)) mod ALIEN_ODDS) = 0) then
    begin
      CreateAlien;
    end;

    // Create new shots */
    if (not reloading) then
    begin
      if (keys[SDLK_SPACE] = SDL_PRESSED) then
      begin
        for i := 0 to MAX_SHOTS - 1 do
        begin
          if (shots[i].alive = 0) then
          begin
            break;
          end;
        end;
        if (i <> MAX_SHOTS) then
        begin
          shots[i].x := player.x + (player.image.w - shots[i].image.w) div 2;
          shots[i].y := player.y - shots[i].image.h;
          shots[i].alive := 1;
          Mix_PlayChannel(Ord(SHOT_WAV), sounds[Ord(SHOT_WAV)], 0);
        end;
      end;
    end;
    reloading := (keys[SDLK_SPACE] = SDL_PRESSED);

    // Move the player */
    player.facing := 0;
    if (keys[SDLK_RIGHT] = SDL_PRESSED) then
    begin
      inc(player.facing);
    end;
    if (keys[SDLK_LEFT] = SDL_PRESSED) then
    begin
      dec(player.facing);
    end;

    player.x := player.x + player.facing * PLAYER_SPEED;
    if (player.x < 0) then
    begin
      player.x := 0;
    end
    else if (player.x >= (screen.w - player.image.w)) then
    begin
      player.x := (screen.w - player.image.w) - 1;
    end;

    // Move the aliens */
    for i := 0 to MAX_ALIENS - 1 do
    begin
      if (aliens[i].alive = 1) then
      begin
        aliens[i].x := aliens[i].x + aliens[i].facing * ALIEN_SPEED;
        if (aliens[i].x < 0) then
        begin
          aliens[i].x := 0;
          aliens[i].y := aliens[i].y + aliens[i].image.h;
          aliens[i].facing := 1;
        end
        else if (aliens[i].x >= (screen.w - aliens[i].image.w)) then
        begin
          aliens[i].x := (screen.w - aliens[i].image.w) - 1;
          aliens[i].y := aliens[i].y + aliens[i].image.h;
          aliens[i].facing := -1;
        end;
      end;
    end;

    // Move the shots */
    for i := 0 to MAX_SHOTS - 1 do
    begin
      if (shots[i].alive = 1) then
      begin
        shots[i].y := shots[i].y - SHOT_SPEED;
        if (shots[i].y < 0) then
        begin
          shots[i].alive := 0;
        end;
      end;
    end;

    // Detect collisions */
    for j := 0 to MAX_SHOTS - 1 do
    begin
      for i := 0 to MAX_ALIENS - 1 do
      begin
        if (shots[j].alive = 1) and (aliens[i].alive = 1) and
          Collide(@shots[j], @aliens[i]) then
        begin
          aliens[i].alive := 0;
          explosions[i].x := aliens[i].x;
          explosions[i].y := aliens[i].y;
          explosions[i].alive := EXPLODE_TIME;
          Mix_PlayChannel(Ord(EXPLODE_WAV),
            sounds[Ord(EXPLODE_WAV)], 0);
          shots[j].alive := 0;
          break;
        end;
      end;
    end;

    for i := 0 to MAX_ALIENS - 1 do
    begin
      if (aliens[i].alive = 1) and Collide(@player, @aliens[i]) then
      begin
        aliens[i].alive := 0;
        explosions[i].x := aliens[i].x;
        explosions[i].y := aliens[i].y;
        explosions[i].alive := EXPLODE_TIME;
        player.alive := 0;
        explosions[MAX_ALIENS].x := player.x;
        explosions[MAX_ALIENS].y := player.y;
        explosions[MAX_ALIENS].alive := EXPLODE_TIME;
        Mix_PlayChannel(Ord(EXPLODE_WAV), sounds[Ord(EXPLODE_WAV)], 0);
      end;
    end;

    // Draw the aliens, shots, player, and explosions */
    for i := 0 to MAX_ALIENS - 1 do
    begin
      if (aliens[i].alive = 1) then
      begin
        DrawObject(@aliens[i]);
      end;
    end;

    for i := 0 to MAX_SHOTS - 1 do
    begin
      if (shots[i].alive = 1) then
      begin
        DrawObject(@shots[i]);
      end;
    end;

    if (player.alive = 1) then
    begin
      DrawObject(@player);
    end;

    for i := 0 to MAX_ALIENS do
    begin
      if (explosions[i].alive = 1) then
      begin
        DrawObject(@explosions[i]);
      end;
    end;
    UpdateScreen;

    // Loop the music */
{$IFDEF PLAY_MOD}
    if (not ( Mix_PlayingMusic = 1 ) ) then
    begin
      Mix_PlayMusic(music, 0);
    end;
{$ENDIF}
{$IFDEF PLAY_MID}
    if (not ( Mix_PlayingMusic = 1 ) ) then
    begin
      Mix_PlayMusic(music, 0);
    end;
{$ENDIF}

{$IFNDEF PLAY_MOD}
{$IFNDEF PLAY_MID}
    if (not Mix_Playing(Ord(MUSIC_WAV)) = 0) then
    begin
      Mix_PlayChannel(Ord(MUSIC_WAV), sounds[Ord(MUSIC_WAV)], 0);
    end;
{$ENDIF}
{$ENDIF}

    // Check for keyboard abort */
    if (keys[SDLK_ESCAPE] = SDL_PRESSED) then
    begin
      player.alive := 0;
    end;
  end;

  // Wait for the player to finish exploding */
  while (not Mix_Playing(Ord(EXPLODE_WAV)) = 0) do
  begin
    WaitFrame;
  end;
  Mix_HaltChannel(-1);
end;

procedure TerminateApplication;
begin
  SDL_QUIT;
  Halt(0);
end;

begin
  next_tick := 0;

  path := ExtractFileDir(ParamStr(0)) + '/';
  // Initialize the SDL library
  if (SDL_Init(SDL_INIT_AUDIO or SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Couldn''t initialize SDL : %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
    exit;
  end;

  // Open the audio device
  // NOTE : the call to  Mix_OpenAudio MUST happen before the call to
  //        SDL_SetVideoMode, otherwise you will get a ( sometimes load )
  //        audible pop.
  if (Mix_OpenAudio(11025, AUDIO_U8, 1, 512) < 0) then
  begin
    Log.LogWarning(Format('Couldn''t set 11025 Hz 8-bit audio - Reason : %s',
      [Mix_GetError]), 'Main');
  end;

  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
    flags := SDL_SWSURFACE or SDL_FULLSCREEN
  else
    flags := SDL_SWSURFACE;

  // Set the title bar in environments that support it
  SDL_WM_SetCaption(TITLE, nil);

  screen := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, 0, flags);
  if (screen = nil) then
  begin
    Log.LogError(Format('Could not set video mode : %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
    exit;
  end;

  // Initialize the random number generator
  randomize; //( now );

  // Load the music and artwork
  if (LoadData) then
  begin
    // Run the game
    RunGame;

    // Free the music and artwork
    FreeData;
  end;

  // Quit
  Mix_CloseAudio();
  TerminateApplication;
end.

