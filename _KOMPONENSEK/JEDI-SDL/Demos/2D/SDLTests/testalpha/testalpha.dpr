program testalpha;
{******************************************************************************}
{                                                                              }
{                          Alpha Test                                          }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@Savagesoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2000 - 2002 Dominique Louis.                                   }
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
{     Fill a colormap with gray and stripe it down the screen,                 }
{     Then move an alpha valued sprite around the screen.                      }
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
  FRAME_TICKS = ( 1000 div 30 ); (* 30 frames/second *)

var
  Screen : PSDL_Surface;

  (* Create a 'light' -- a yellowish surface with variable alpha *)

function CreateLight( Screen : PSDL_Surface; radius : integer ) : PSDL_Surface;
var
  trans, alphamask : Uint8;
  range, addition : integer;
  xdist, ydist : integer;
  x, y : Uint16;
  skip : Uint16;
  pixel : Uint32;
  light : PSDL_Surface;
{$IFDEF LIGHT_16BIT}
  buf : PUint16;
{$ELSE}
  buf : PUint32;
{$ENDIF}
begin

{$IFDEF LIGHT_16BIT}
  (* Create a 16 (4/4/4/4) bpp square with a full 4-bit alpha channel *)
  (* Note: this isn't any faster than a 32 bit alpha surface *)
  alphamask := $0000000F;
  light := SDL_CreateRGBSurface( SDL_SWSURFACE, 2 * radius, 2 * radius, 16,
    $0000F000, $00000F00, $000000F0, alphamask );
{$ELSE}
  (* Create a 32 (8/8/8/8) bpp square with a full 8-bit alpha channel *)
  alphamask := $000000FF;
  light := SDL_CreateRGBSurface( SDL_SWSURFACE, 2 * radius, 2 * radius, 32,
    $FF000000, $00FF0000, $0000FF00, alphamask );
{$ENDIF}
  if ( light = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t create light: %s', [ SDL_GetError ] ),
      'CreateLight' );
    result := nil;
    Exit;
  end;
  (* Fill with a light yellow-orange color *)
  skip := light.pitch - ( light.w * light.format.BytesPerPixel );
{$IFDEF LIGHT_16BIT}
  buf := @( light.pixels );
{$ELSE}
  buf := @( light.pixels );
{$ENDIF}

  (* Get a tranparent pixel value - we'll add alpha later *)
  {pixel := SDL_MapRGBA(light.format, $FF, $DD, $88, 0);
  for y := 0 to light.h - 1 do
  begin
    for x := 0 to light.w - 1 do
    begin
      buf^ := pixel;
      Inc(buf);
    end;
    Inc(buf, skip); (* Almost always 0, but just in ... *)
  end; }
  (* Calculate alpha values for the surface. *)
{$IFDEF LIGHT_16BIT}
  //buf := @(light.pixels);
{$ELSE}
  //buf := @(light.pixels);
{$ENDIF}
  {for y := 0 to light.h - 1 do
  begin
    for x := 0 to light.w - 1 do
    begin
      (* Slow distance formula (from center of light) *)
      xdist := x - (light.w div 2);
      ydist := y - (light.h div 2);
      range := Round( Sqrt(xdist * xdist + ydist * ydist) );
      (* Scale distance to range of transparency (0-255) *)
      if (range > radius) then
      begin
        trans := alphamask;
      end
      else
      begin
        (* Increasing transparency with distance *)
        trans := ((range * alphamask) div radius);
        (* Lights are very transparent *)
        addition := (alphamask + 1) div 8;
        if (trans + addition > alphamask) then
        begin
          trans := alphamask;
        end
        else
        begin
          trans := trans + addition;
        end;
      end;
      (* We set the alpha component as the right N bits *)
      buf^ := buf^ or (255 - trans);
      Inc(buf);
    end;
    Inc(buf, skip); (* Almost always 0, but just in ... *)
  end; }
  (* Enable RLE acceleration of this alpha surface *)
  SDL_SetAlpha( light, SDL_SRCALPHA or SDL_RLEACCEL, 0 );
  (* We're donenot  *)
  result := ( light );
end;

var
  flashes : Uint32 = 0;
  flashtime : Uint32 = 0;

procedure FlashLight( Screen : PSDL_Surface; light : PSDL_Surface; x, y : integer );
var
  Position : TSDL_Rect;
  ticks1 : Uint32;
  ticks2 : Uint32;
begin

  (* Easy, center light *)
  position.x := x - ( light.w div 2 );
  position.y := y - ( light.h div 2 );
  position.w := light.w;
  position.h := light.h;
  ticks1 := SDL_GetTicks;
  SDL_BlitSurface( light, nil, screen, @position );
  ticks2 := SDL_GetTicks;
  SDL_UpdateRects( screen, 1, @position );
  Inc( flashes );
  (* Update time spend doing alpha blitting *)
  flashtime := flashtime + ( ticks2 - ticks1 );
end;

var
  sprite_visible : Boolean = false;
  sprite : PSDL_Surface;
  backing : PSDL_Surface;
  position : TSDL_Rect;
  x_vel, y_vel : integer;
  alpha_vel : integer;

function LoadSprite( Screen : PSDL_Surface; filename : PChar ) : integer;
var
  converted : PSDL_Surface;
begin
  (* Load the sprite image *)
  sprite := SDL_LoadBMP( filename );
  if ( sprite = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t load %s: %s', [ filename, SDL_GetError ] ),
      'LoadSprite' );
    result := ( -1 );
    Exit;
  end;

  (* Set transparent pixel as the pixel at (0,0) *)
  if ( sprite.format.palette <> nil ) then
  begin
    SDL_SetColorKey( sprite, SDL_SRCCOLORKEY, {SDL_MapRGB(
      sprite.format, 255, 0, 255 )} PUint8( sprite.pixels )^ );
  end;

  (* Convert sprite to video format *)
  converted := SDL_DisplayFormat( sprite );
  SDL_FreeSurface( sprite );

  if ( converted = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t convert background: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    result := ( -1 );
    Exit;
  end;
  sprite := converted;
  (* Create the background *)
  backing := SDL_CreateRGBSurface( SDL_SWSURFACE, sprite.w, sprite.h, 8,
    0, 0, 0, 0 );
  if ( backing = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t creates background: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    SDL_FreeSurface( sprite );
    result := ( -1 );
    Exit;
  end;
  (* Convert background to video format *)
  converted := SDL_DisplayFormat( backing );
  SDL_FreeSurface( backing );
  if ( converted = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t convert background: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    SDL_FreeSurface( sprite );
    result := ( -1 );
    Exit;
  end;
  backing := converted;
  (* Set the initial position of the sprite *)
  position.x := ( screen.w - sprite.w ) div 2;
  position.y := ( screen.h - sprite.h ) div 2;
  position.w := sprite.w;
  position.h := sprite.h;
  x_vel := 0;
  y_vel := 0;
  alpha_vel := 1;
  (* We're ready to roll. :) *)
  result := ( 0 );
end;

procedure AttractSprite( x : Uint16; y : Uint16 );
begin
  x_vel := ( x - position.x ) div 10;
  y_vel := ( y - position.y ) div 10;
end;

procedure MoveSprite( Screen : PSDL_Surface; light : PSDL_Surface );
var
  Updates : array[ 0..1 ] of TSDL_Rect;
  alpha : integer;
  x, y : integer;
begin

  (* Erase the sprite if it was visible *)
  if ( sprite_visible ) then
  begin
    Updates[ 0 ] := position;
    SDL_BlitSurface( backing, nil, screen, @updates[ 0 ] );
  end
  else
  begin
    updates[ 0 ].x := 0;
    updates[ 0 ].y := 0;
    updates[ 0 ].w := 0;
    updates[ 0 ].h := 0;
    sprite_visible := True;
  end;
  (* Since the sprite is off the screen, we can do other drawing
     without being overwritten by the saved area behind the sprite.
   *)
  if ( light <> nil ) then
  begin
    SDL_GetMouseState( x, y );
    FlashLight( screen, light, x, y );
  end;

  (* Move the sprite, bounce at the wall *)
  position.x := position.x + x_vel;
  if ( ( position.x < 0 ) or ( position.x >= screen.w ) ) then
  begin
    x_vel := -x_vel;
    position.x := position.x + x_vel;
  end;

  position.y := position.y + y_vel;
  if ( ( position.y < 0 ) or ( position.y >= screen.h ) ) then
  begin
    y_vel := -y_vel;
    position.y := position.y + y_vel;
  end;

  (* Update transparency (fade in and out) *)
  alpha := sprite.format.alpha;
  if ( ( alpha + alpha_vel ) < 0 ) then
  begin
    alpha_vel := -alpha_vel;
  end
  else if ( ( alpha + alpha_vel ) > 255 ) then
  begin
    alpha_vel := -alpha_vel;
  end;
  SDL_SetAlpha( sprite, SDL_SRCALPHA, ( alpha + alpha_vel ) );
  (* Save the area behind the sprite *)
  updates[ 1 ] := position;
  SDL_BlitSurface( screen, @Updates[ 1 ], backing, nil );

  (* Blit the sprite onto the screen *)
  updates[ 1 ] := position;
  SDL_BlitSurface( sprite, nil, screen, @updates[ 1 ] );
  (* Make it sonot  *)
  SDL_UpdateRects( screen, 2, @updates );
end;

procedure WarpSprite( Screen : PSDL_Surface; x, y : integer );
var
  Updates : array[ 0..1 ] of TSDL_Rect;
begin
  (* Erase, move, Draw, update *)
  Updates[ 0 ] := position;
  SDL_BlitSurface( backing, nil, screen, @updates[ 0 ] );
  position.x := x - sprite.w div 2; (* Center about X *)
  position.y := y - sprite.h div 2; (* Center about Y *)
  updates[ 1 ] := position;
  SDL_BlitSurface( screen, @Updates[ 1 ], backing, nil );
  updates[ 1 ] := position;
  SDL_BlitSurface( sprite, nil, screen, @updates[ 1 ] );
  SDL_UpdateRects( screen, 2, @updates );
end;

var
  VideoInfo : PSDL_VideoInfo;

  video_bpp : Uint8;
  videoflags : Uint32;
  buffer : PUint8;
  i : integer;
  done : Boolean;
  event : TSDL_Event;
  light : PSDL_Surface;
  mouse_pressed : Boolean;
  ticks, lastticks : Uint32;
  clip, area : TSDL_Rect;
begin

  (* Initialize SDL *)
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Couldn''t initialize SDL: %s', [ SDL_GetError ] ),
      'LoadSprite' );
    halt( 1 );
  end;

  (* Alpha blending doesn't work well at 8-bit color *)
  VideoInfo := SDL_GetVideoInfo;
  if ( VideoInfo.vfmt.BitsPerPixel > 8 ) then
  begin
    video_bpp := VideoInfo.vfmt.BitsPerPixel;
  end
  else
  begin
    video_bpp := 16;
  end;

  videoflags := SDL_SWSURFACE;
  for i := 0 to ParamCount - 1 do
  begin
    if ( ParamStr( i ) = '-bpp' ) then
    begin
      video_bpp := StrToInt( ParamStr( i + 1 ) );
    end
    else if ( ParamStr( i ) = '-hw' ) then
    begin
      videoflags := videoflags or SDL_HWSURFACE;
    end
    else if ( ParamStr( i ) = '-warp' ) then
    begin
      videoflags := videoflags or SDL_HWPALETTE;
    end
    else if ( ParamStr( i ) = '-fullscreen' ) then
    begin
      videoflags := videoflags or SDL_FULLSCREEN;
    end
    else
    begin
      Log.LogWarning( 'Usage : testapha -bpp N -warp -hw -fullscreen',
        'MAIN' );
      halt( 1 );
    end;
  end;
  (* Set 640 x 480 video mode *)
  screen := SDL_SetVideoMode( 640, 480, video_bpp, videoflags );
  if ( Screen = nil ) then
  begin
    Log.LogError( Format( 'Couldn'' t set 640 x 480 %d video mode: %s',
      [ video_bpp, SDL_GetError ] ),
      'MAIN' );
    halt( 2 );
  end;

  // Set the window manager title bar
  SDL_WM_SetCaption( 'JEDI-SDL alpha test', 'testalpha' );

  (* Set the surface pixels and refreshnot  *)
  if ( SDL_LockSurface( screen ) < 0 ) then
  begin
    Log.LogError( Format( 'Couldn'' t lock the display surface: %s',
      [ SDL_GetError ] ),
      'MAIN' );
    halt( 2 );
  end;

  buffer := PUint8( screen.pixels );
  for i := 0 to screen.h - 1 do
  begin
    FillChar( buffer, Screen.pitch, ( i * 255 ) div screen.h );
    //memset(buffer, (i * 255) div screen.h , screen.pitch);
    Inc( buffer, screen.pitch );
  end;
  SDL_UnlockSurface( screen );
  SDL_UpdateRect( screen, 0, 0, 0, 0 );

  (* Create the light *)
  light := CreateLight( screen, 82 );
  if ( light = nil ) then
  begin
    halt( 1 );
  end;

  (* Load the sprite *)
  if ( LoadSprite( screen, '../images/icon.bmp' ) < 0 ) then
  begin
    SDL_FreeSurface( light );
    halt( 1 );
  end;
  (* Set a clipping rectangle to clip the outside edge of the screen *)

  clip.x := 32;
  clip.y := 32;
  clip.w := screen.w - ( 2 * 32 );
  clip.h := screen.h - ( 2 * 32 );
  SDL_SetClipRect( screen, @clip );

  (* Wait for a keystroke *)
  lastticks := SDL_GetTicks;
  done := False;
  mouse_pressed := False;
  while ( not done ) do
  begin
    (* Update the frame -- move the sprite *)
    if ( mouse_pressed ) then
    begin
      MoveSprite( screen, light );
      mouse_pressed := False;
    end
    else
    begin
      MoveSprite( screen, nil );
    end;
    (* Slow down the loop to 30 frames/second *)
    ticks := SDL_GetTicks;
    if ( ( ticks - lastticks ) < FRAME_TICKS ) then
    begin
{$IFDEF CHECK_SLEEP_GRANULARITY}
      fprintf( stderr, 'Sleeping %d ticks', FRAME_TICKS - ( ticks - lastticks ) );
{$ENDIF}
      SDL_Delay( FRAME_TICKS - ( ticks - lastticks ) );
{$IFDEF CHECK_SLEEP_GRANULARITY}
      fprintf( stderr, 'Slept %d ticks', ( SDL_GetTicks - ticks ) );
{$ENDIF}
    end;
    lastticks := ticks;
    (* Check for events *)
    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case ( event.type_ ) of
        (* Attract sprite while mouse is held down *)
        SDL_MOUSEMOTION :
          begin
            if ( event.motion.state <> 0 ) then
            begin
              AttractSprite( event.motion.x,
                event.motion.y );
              mouse_pressed := true;
            end;
          end;
        SDL_MOUSEBUTTONDOWN :
          begin
            if ( event.button.button = 1 ) then
            begin
              AttractSprite( event.button.x,
                event.button.y );
              mouse_pressed := true;
            end
            else
            begin
              area.x := event.button.x - 16;
              area.y := event.button.y - 16;
              area.w := 32;
              area.h := 32;
              SDL_FillRect( screen, @area, 0 );
              SDL_UpdateRects( screen, 1, @area );
            end;
          end;
        SDL_KEYDOWN :
          begin
            (* Any keypress quits the app... *)
            SDL_QUIT;
            done := true;
          end;
      end;
    end;
  end;
  SDL_FreeSurface( light );
  SDL_FreeSurface( sprite );
  SDL_FreeSurface( backing );
  (* Pr : integer out some timing information *)
  {if (flashes > 0) then
  begin
    printf('%d alpha blits, ~mod4.4f ms per blit',
      flashes, (float)flashtime / flashes);
  end;}

  SDL_Quit;
end.

