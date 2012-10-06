program testgamma;
{******************************************************************************}
{                                                                              }
{                     Gamma test/example                                       }
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
  Math,
  SysUtils,
  Logger,
  SDL;

// Turn a normal gamma value into an appropriate gamma ramp

procedure CalculateGamma( gamma : double; var ramp : array of Uint16 );
var
  i, value : integer;
begin
  gamma := 1.0 / gamma;
  for i := 0 to 255 do
  begin
    value := Round( power( i / 256.0, gamma ) * 65535.0 + 0.5 );
    if ( value > 65535 ) then
      value := 65535;
    ramp[ i ] := Uint16( value );
  end;
end;

// This can be used as a general routine for all of the test programs */

procedure get_video_args( var w, h, bpp : integer; var flags : Uint32 );
var
  i : integer;
begin


  w := 640;
  h := 480;
  bpp := 0;
  flags := SDL_SWSURFACE;

  for i := 1 to ParamCount do
  begin
    if ( ParamStr( i ) = '-width' ) then
    begin
      if ( ParamStr( i + 1 ) <> '' ) then
        w := StrToInt( ParamStr( i + 1 ) );
    end
    else if ( ParamStr( i ) = '-height' ) then
    begin
      if ( ParamStr( i + 1 ) <> '' ) then
        h := StrToInt( ParamStr( i + 1 ) );
    end
    else if ( ParamStr( i ) = '-bpp' ) then
    begin
      if ( ParamStr( i + 1 ) <> '' ) then
        bpp := StrToInt( ParamStr( i + 1 ) );
    end
    else if ( ParamStr( i ) = '-fullscreen' ) then
    begin
      flags := flags or SDL_FULLSCREEN;
    end
    else if ( ParamStr( i ) = '-hw' ) then
    begin
      flags := flags or SDL_HWSURFACE;
    end
    else
      break;
  end;
end;

var
  screen : PSDL_Surface;
  image : PSDL_Surface;
  gamma : single;
  i : integer;
  w, h, bpp : integer;
  flags : Uint32;
  ramp : array[ 0..255 ] of Uint16;
  red_ramp : array[ 0..255 ] of Uint16;
  timeout, then_ : Uint32;
  //real : array[0..2] of single;
  dst : TSDL_Rect;
  event : TSDL_Event;
begin
  // Check command line arguments
  get_video_args( w, h, bpp, flags );

  // Initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Couldn''t initialize SDL : %s', [ SDL_GetError ] ), 'Main' );
    halt( 1 );
  end;

  // Initialize the display, always use hardware palette
  screen := SDL_SetVideoMode( w, h, bpp, flags or SDL_HWPALETTE );
  if ( screen = nil ) then
  begin
    //fprintf(stderr, 'Couldn't set %dx%d video mode: %s\n',
    //				w, h, SDL_GetError());
    Log.LogError( Format( 'Couldn''t set %dx%d video mode : %s', [ w, h, SDL_GetError ] ), 'Main' );
    halt( 1 );
  end;

  // Set the window manager title bar
  SDL_WM_SetCaption( 'JEDI-SDL gamma test', 'testgamma' );

  // Set the desired gamma, if any
  gamma := 1.0;
  {if ( *argv ) {
   gamma = (float)atof(*argv);
  }
  if ( SDL_SetGamma( gamma, gamma, gamma ) < 0 ) then
  begin
    //fprintf(stderr, 'Unable to set gamma: %s\n', SDL_GetError());
    Log.LogError( Format( 'Unable to set gamma: %s', [ SDL_GetError ] ), 'Main' );
    halt( 1 );
  end;

  // This isn't supported.  Integrating the gamma ramps isn't exact
// See what gamma was actually set
{if ( SDL_GetGamma(@real[0], @real[1], @real[2]) < 0 ) then
  begin
//	printf('Couldn't get gamma: %s\n', SDL_GetError());
end
  else
  begin
    //printf('Set gamma values: R=%2.2f, G=%2.2f, B=%2.2f\n',
//		real[0], real[1], real[2]);
end;}


// Do all the drawing work
  image := SDL_LoadBMP( '../images/sample.bmp' );
  if ( image <> nil ) then
  begin
    dst.x := ( screen.w - image.w ) div 2;
    dst.y := ( screen.h - image.h ) div 2;
    dst.w := image.w;
    dst.h := image.h;
    SDL_BlitSurface( image, nil, screen, @dst );
    SDL_UpdateRects( screen, 1, @dst );
  end;

  // Wait a bit, handling events
  then_ := SDL_GetTicks;
  timeout := ( 5 * 1000 );
  while ( {( SDL_GetTicks - then_ ) < timeout} timeout <> 0 ) do
  begin
    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case ( event.type_ ) of
        SDL_QUITEV : // Quit now */
          timeout := 0;

        SDL_KEYDOWN :
          case ( event.key.keysym.sym ) of
            SDLK_SPACE : // Go longer..
              timeout := timeout + ( 5 * 1000 );

            SDLK_UP :
              begin
                gamma := gamma + 0.2;
                SDL_SetGamma( gamma, gamma, gamma );
              end;

            SDLK_DOWN :
              begin
                gamma := gamma - 0.2;
                SDL_SetGamma( gamma, gamma, gamma );
              end;

            SDLK_ESCAPE :
              timeout := 0;
          end;
      end;
    end;
  end;

  // Perform a gamma flash to red using color ramps */
  while ( gamma < 10.0 ) do
  begin
    // Increase the red gamma and decrease everything else... */
    gamma := gamma + 0.1;
    CalculateGamma( gamma, red_ramp );
    CalculateGamma( 1.0 / gamma, ramp );
    SDL_SetGammaRamp( @red_ramp, @ramp, @ramp );
  end;
  // Finish completely red */
  FillChar( red_ramp, sizeof( red_ramp ), 255 );
  FillChar( ramp, sizeof( ramp ), 0 );
  SDL_SetGammaRamp( @red_ramp, @ramp, @ramp );

  // Now fade out to black
  for i := ( red_ramp[ 0 ] shr 8 ) downto 0 do
  begin
    FillChar( red_ramp, sizeof( red_ramp ), i );
    SDL_SetGammaRamp( @red_ramp, nil, nil );
  end;
  SDL_Delay( 2 * 1000 );
end.
