program JEDISDLWormhole;
{******************************************************************}
{                 a worm hole using JEDI-SDL                       }
{                                                                  }
{ Source of the worm hole taken from SWAG                          }
{(c) Jaco van Niekerk  sparky@lantic.co.za                         }
{                                                                  }
{ JEDI-SDL version by thomas Bechelot   bech_t@hotmail.com         }
{                                                                  }
{ rest of code taken from JEDI-SDL sample (like putpixel ...)      }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{  Simple wormhole using SDL.                                      }
{  press ESC to stop.                                              }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{ if someone could tell me how to make a real clear screen, or     }
{ just change line 199. thanks.                                    }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   september 23 2001 - TB : just for fun                          }
{   september 27 2001 - DL : Kylix compatability                   }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  SDLUtils,
  Logger,
  SDL;

var
  screen_ : PSDL_Surface;

  circle_x : array[1..80, 0..61] of integer;
  circle_y : array[1..80, 0..61] of integer;
  relpos_x : array[1..80] of integer;
  relpos_y : array[1..80] of integer;

  new_y, new_x : word;
  cx, cy, dx, dy : real;
  tx, ty : integer;
  i : byte;

  done : boolean;
  col{, bl} : UInt32;

  event : TSDL_EVENT;
  keys : PKeyStateArr;

  video_flags : Cardinal;

procedure DrawWormhole( color : UInt8 );
var
  i, j : byte;
begin
  //if ( SDL_MustLock(screen_) ) then
        //if ( SDL_LockSurface(screen_) < 0 ) then

        {plot circles}
  for i := 0 to 80 do
    for j := 0 to 60 do
    begin
      new_x := circle_x[i][j] + relpos_x[i];
      new_y := circle_y[i][j] + relpos_y[i];
      if ( new_x > 0 ) and ( new_x < 640 ) and
        ( new_y > 0 ) and ( new_y < 480 ) then
        SDL_PutPixel( screen_, new_x, new_y, color );

    end;

  //if ( SDL_MustLock(screen_) > 0 ) then
  //     begin
  //     SDL_UnlockSurface(screen_);
  //     end;

  {blast to screen}
  SDL_UpdateRect( screen_, 0, 0, 0, 0 );
end;

procedure calc_circles;
var
  deg, x, y, c : integer;
begin
  for c := 1 to 80 do
  begin
    relpos_x[c] := 0;
    relpos_y[c] := 0;
    for deg := 0 to 60 do
    begin
      x := round( c * 6 * cos( deg * pi / 30 ) );
      y := round( c * 6 * sin( deg * pi / 30 ) );
      circle_x[c, deg] := 320 + x;
      circle_y[c, deg] := 240 + y;
    end;
  end;
end;

begin
  // Initialize defaults, Video
  SDL_Init( SDL_INIT_VIDEO );
  SDL_WM_SetCaption( 'JEDI-SDL - Wormhole', nil );

  if ( ParamStr( 1 ) = '-fullscreen' ) or ( ParamStr( 1 ) = '-fs' ) then
    video_flags := SDL_ANYFORMAT or SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
  else
    video_flags := SDL_ANYFORMAT or SDL_SWSURFACE or SDL_HWPALETTE;
  // Have a preference for 8-bit, but accept any depth
  screen_ := SDL_SetVideoMode( 640, 480, 8, video_flags );
  if ( screen_ = nil ) then
  begin
    Log.LogError( Format(
      'Couldn''t set fullscreen 640x480x8 video mode : %s',
      [SDL_GetError] ), 'Main' );
    SDL_Quit;
    halt( 1 );
  end;

  col := SDL_MapRGB( screen_.format, $FF, $FF, $FF );
  //bl := SDL_MapRGB( screen_.format, $00, $00, $00 );

  randomize;

  calc_circles;
  cx := 0;
  cy := 0;
  dx := 0;
  dy := 0;
  tx := random( 20 ) - 10;
  ty := random( 20 ) - 10;

  done := False;

  while not done do
  begin
    while ( SDL_PollEvent( @event ) = 1 ) do
      case event.type_ of
        SDL_QUITEV : done := true;
      end;

    keys := PKeyStateArr( SDL_GetKeyState( nil ) );

    if keys[SDLK_ESCAPE] = SDL_PRESSED then
      done := True;

    {update offset buffer}
    for i := 80 downto 1 do
    begin
      relpos_x[i] := relpos_x[i - 1];
      relpos_y[i] := relpos_y[i - 1];
    end;

    {create "new" circle}
    if cx > tx then
      dx := dx - 0.55
    else if cx < tx then
      dx := dx + 0.55;
    if cy > ty then
      dy := dy - 0.55
    else if cy < ty then
      dy := dy + 0.55;
    if sqr( cx - tx ) + sqr( cy - ty ) < 480 then
    begin
      tx := random( 80 ) - 30;
      ty := random( 50 ) - 25;
    end;
    cx := cx + dx;
    cy := cy + dy;

    {speed control}
    if dx > 5 then
      dx := 5;
    if dx < -5 then
      dx := -5;
    if dy > 5 then
      dy := 5;
    if dy < -5 then
      dy := -5;

    {update new circle}
    relpos_x[1] := round( cx );
    relpos_y[1] := round( cy );

    DrawWormhole( col );

    SDL_Delay( 20 );

    // Should be replace by a clearscreen but i don't how ... :)
    SDL_FillRect( screen_, nil, 0 );
    //DrawWormhole( bl );
  end;

  // Shutdown all subsystems
  SDL_Quit;
end.

