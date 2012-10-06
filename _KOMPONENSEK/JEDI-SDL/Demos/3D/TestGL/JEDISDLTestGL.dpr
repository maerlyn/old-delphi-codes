program JEDISDLTestGL;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Sam Lantinga <slantinga@blizzard.com>, are   }
{ Copyright (C) 2001 Sam Lantinga.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : testgl.c                                }
{                                                                  }
{ The original Pascal code is : JEDISDLTestGL.dpr                  }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{                                                                  }
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
{  Shows how to use OpenGL to do 2D and 3D with the SDL libraries  }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Also Makes uses of Mike Lischke's Cross-Platform OpenGL header.}
{   You can pick it up from...                                     }
{   http://www.lischke-online.de/Graphics.html#OpenGL12            }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   April   11 2001 - DL : Initial translation.                    }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  OpenGL12,
  SDL,
  Logger;

(* Undefine this if you want a flat cube instead of a rainbow cube *)
{$DEFINE SHADED_CUBE}

(* Define this to be the name of the logo image to use with -logo *)
const
  LOGO_FILE = 'images/icon.bmp';
  (* The SDL_OPENGLBLIT interface is deprecated.
     The code is still available for benchmark purposes though.
  *)
var
  USE_DEPRECATED_OPENGLBLIT : Boolean = FALSE;
  (**********************************************************************)

procedure HotKey_ToggleFullScreen;
var
  screen : PSDL_Surface;
begin
  screen := SDL_GetVideoSurface;
  if ( SDL_WM_ToggleFullScreen( screen ) <> 0 ) then
  begin
    if ( ( screen.flags and SDL_FULLSCREEN ) <> 0  ) then
      Log.LogStatus( 'Toggled fullscreen mode - now fullscreen', 'HotKey_ToggleFullScreen' )
    else
      Log.LogStatus( 'Toggled fullscreen mode - now windowed', 'HotKey_ToggleFullScreen' );
  end
  else
  begin
    Log.LogStatus( 'Unable to toggle fullscreen mode', 'HotKey_ToggleFullScreen' );
  end;
end;

procedure HotKey_ToggleGrab;
var
  mode : TSDL_GrabMode;
begin

  Log.LogStatus( 'Ctrl-G: toggling input grabnot ', 'HotKey_ToggleGrab' );
  mode := SDL_WM_GrabInput( SDL_GRAB_QUERY );
  if ( mode = SDL_GRAB_ON ) then
  begin
    Log.LogStatus( 'Grab was on', 'HotKey_ToggleGrab' );
  end
  else
  begin
    Log.LogStatus( 'Grab was off', 'HotKey_ToggleGrab' );
  end;
  mode := SDL_WM_GrabInput( mode );
  if ( mode = SDL_GRAB_ON ) then
  begin
    Log.LogStatus( 'Grab is now on', 'HotKey_ToggleGrab' );
  end
  else
  begin
    Log.LogStatus( 'Grab is now off', 'HotKey_ToggleGrab' );
  end;
end;

procedure HotKey_Iconify;
begin
  Log.LogStatus( 'Ctrl-Z: iconifying window ', 'HotKey_Iconify' );
  SDL_WM_IconifyWindow;
end;

function HandleEvent( event : PSDL_Event ) : Boolean;
var
  done : Boolean;
begin

  done := False;
  case ( event.type_ ) of
    SDL_ACTIVEEVENT :
    begin
      (* See what happened *)
      if event.active.gain <> 0 then
        Log.LogStatus( 'app gained ', 'HandleEvent' )
      else
        Log.LogStatus( 'app lost', 'HandleEvent' );
      if ( event.active.state and SDL_APPACTIVE ) <> 0 then
      begin
        Log.LogStatus( 'active ', 'HandleEvent' );
      end
      else if ( event.active.state and SDL_APPMOUSEFOCUS ) <> 0 then
      begin
        Log.LogStatus( 'mouse ', 'HandleEvent' );
      end
      else if ( event.active.state and SDL_APPINPUTFOCUS ) <> 0 then
      begin
        Log.LogStatus( 'input ', 'HandleEvent' );
      end;
      Log.LogStatus( 'focus', 'HandleEvent' );
    end;

    SDL_KEYDOWN :
      begin
        case event.key.keysym.sym of
          SDLK_ESCAPE :
            begin
              done := True;
            end;

          SDLK_g :
            begin
              if ( event.key.keysym.modifier and KMOD_CTRL <> 0 ) then
                HotKey_ToggleGrab;
            end;

          SDLK_z :
            begin
              if ( event.key.keysym.modifier and KMOD_CTRL <> 0 ) then
                HotKey_Iconify;
            end;

          SDLK_RETURN :
            begin
              if ( event.key.keysym.modifier and KMOD_ALT <> 0 ) then
                HotKey_ToggleFullScreen;
            end;
        end;
      end;
    SDL_QUITEV :
      done := true;
  end;
  if event.type_ = SDL_KEYDOWN then
    Log.LogStatus( Format( 'key %s pressed', [ SDL_GetKeyName( event.key.keysym.sym ) ] ), 'HandleEvent' );

  result := done;
end;

procedure SDL_GL_Enter2DMode;
var
  screen : PSDL_Surface;
begin
  screen := SDL_GetVideoSurface;
  (* Note, there may be other things you need to change,
     depending on how you have your OpenGL state set up.
  *)
  glPushAttrib( GL_ENABLE_BIT );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  glEnable( GL_TEXTURE_2D );
  glViewport( 0, 0, screen.w, screen.h );
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
  glLoadIdentity;
  glOrtho( 0.0, screen.w, screen.h, 0.0, 0.0, 1.0 );
  glMatrixMode( GL_MODELVIEW );
  glPushMatrix;
  glLoadIdentity;
  glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL );
end;

procedure SDL_GL_Leave2DMode;
begin
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix;
  glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glPopAttrib;
end;
(* Quick utility function for texture creation *)

function power_of_two( input : integer ) : integer;
var
  value : integer;
begin
  value := 1;
  while ( value < input ) do
  begin
    value := value shl 1;
  end;
  result := value;
end;

function SDL_GL_LoadTexture( surface : PSDL_Surface; var texcoord : array of TGlFloat ) :
TGLuint;
var
  texture : TGLuint;
  w, h : integer;
  image : PSDL_Surface;
  area : TSDL_Rect;
  saved_flags : Uint32;
  saved_alpha : Uint8;
begin
  (* Use the surface width and height expanded to powers of 2 *)
  w := power_of_two( surface.w );
  h := power_of_two( surface.h );
  texcoord[ 0 ] := 0.0; (* Min X *)
  texcoord[ 1 ] := 0.0; (* Min Y *)
  texcoord[ 2 ] := surface.w / w; (* Max X *)
  texcoord[ 3 ] := surface.h / h; (* Max Y *)
  image := SDL_CreateRGBSurface(
    SDL_SWSURFACE,
    w, h,
    32,
{$IFDEF IA32} (* OpenGL RGBA masks *)
    $000000FF,
    $0000FF00,
    $00FF0000,
    $FF000000
{$ELSE}
    $FF000000,
    $00FF0000,
    $0000FF00,
    $000000FF
{$ENDIF}
    );
  if ( image = nil ) then
  begin
    result := 0;
    exit;
  end;
  (* Save the alpha blending attributes *)
  saved_flags := surface.flags and ( SDL_SRCALPHA or SDL_RLEACCELOK );
  saved_alpha := surface.format.alpha;
  if ( ( saved_flags and SDL_SRCALPHA ) = SDL_SRCALPHA ) then
  begin
    SDL_SetAlpha( surface, 0, 0 );
  end;
  (* Copy the surface into the GL texture image *)
  area.x := 0;
  area.y := 0;
  area.w := surface.w;
  area.h := surface.h;
  SDL_BlitSurface( surface, @area, image, @area );
  (* Restore the alpha blending attributes *)
  if ( ( saved_flags and SDL_SRCALPHA ) = SDL_SRCALPHA ) then
  begin
    SDL_SetAlpha( surface, saved_flags, saved_alpha );
  end;
  (* Create an OpenGL texture for the image *)
  glGenTextures( 1, @texture );
  glBindTexture( GL_TEXTURE_2D, texture );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  glTexImage2D( GL_TEXTURE_2D,
    0,
    GL_RGBA,
    w, h,
    0,
    GL_RGBA,
    GL_UNSIGNED_BYTE,
    image.pixels );
  SDL_FreeSurface( image ); (* No longer needed *)
  result := texture;
end;

var
  texture : TGLuint;
  texMinX, texMinY : TGLfloat;
  texMaxX, texMaxY : TGLfloat;
  x : integer = 0;
  y : integer = 0;
  w, h : integer;
  delta_x : integer = 1;
  delta_y : integer = 1;
  last_moved : Uint32 = 0;

procedure DrawLogoTexture;
var
  screen : PSDL_Surface;
  image : PSDL_Surface;
  texcoord : array[ 0..3 ] of TGlFloat;
begin
  screen := SDL_GetVideoSurface;
  if ( texture = 0 ) then
  begin

    (* Load the image (could use SDL_image library here) *)
    image := SDL_LoadBMP( LOGO_FILE );
    if ( image = nil ) then
    begin
      exit;
    end;
    w := image.w;
    h := image.h;
    (* Convert the image into an OpenGL texture *)
    texture := SDL_GL_LoadTexture( image, texcoord );
    (* Make texture coordinates easy to understand *)
    texMinX := texcoord[ 0 ];
    texMinY := texcoord[ 1 ];
    texMaxX := texcoord[ 2 ];
    texMaxY := texcoord[ 3 ];
    (* We don't need the original image anymore *)
    SDL_FreeSurface( image );
    (* Make sure that the texture conversion is okay *)
    if ( texture = 0 ) then
    begin
      exit;
    end;
  end;
  (* Move the image around *)
  x := x + delta_x;
  if ( x < 0 ) then
  begin
    x := 0;
    delta_x := -delta_x;
  end
  else if ( ( x + w ) > screen.w ) then
  begin
    x := screen.w - w;
    delta_x := -delta_x;
  end;
  y := y + delta_y;
  if ( y < 0 ) then
  begin
    y := 0;
    delta_y := -delta_y;
  end
  else if ( ( y + h ) > screen.h ) then
  begin
    y := screen.h - h;
    delta_y := -delta_y;
  end;
  (* Show the image on the screen *)
  SDL_GL_Enter2DMode;
  glBindTexture( GL_TEXTURE_2D, texture );
  glBegin( GL_TRIANGLE_STRIP );
    glTexCoord2f( texMinX, texMinY );
    glVertex2i( x, y );
    glTexCoord2f( texMaxX, texMinY );
    glVertex2i( x + w, y );
    glTexCoord2f( texMinX, texMaxY );
    glVertex2i( x, y + h );
    glTexCoord2f( texMaxX, texMaxY );
    glVertex2i( x + w, y + h );
  glEnd;
  SDL_GL_Leave2DMode;
end;

(* This code is deprecated, but available for speed comparisons *)
var
  image : PSDL_Surface = nil;
  dst : TSDL_Rect;

procedure DrawLogoBlit;
var
  screen : PSDL_Surface;
  temp : PSDL_Surface;
begin
  screen := SDL_GetVideoSurface;
  if ( image = nil ) then
  begin
    (* Load the image (could use SDL_image library here) *)
    temp := SDL_LoadBMP( LOGO_FILE );
    if ( temp = nil ) then
    begin
      exit;
    end;
    w := temp.w;
    h := temp.h;
    (* Convert the image into the screen format *)
    image := SDL_CreateRGBSurface(
      SDL_SWSURFACE,
      w, h,
      screen.format.BitsPerPixel,
      screen.format.Rmask,
      screen.format.Gmask,
      screen.format.Bmask,
      screen.format.Amask );
    if ( image <> nil ) then
    begin
      SDL_BlitSurface( temp, nil, image, nil );
    end;
    SDL_FreeSurface( temp );
    (* Make sure that the texture conversion is okay *)
    if ( image = nil ) then
    begin
      exit;
    end;
  end;
  (* Move the image around
            Note that we do not clear the old position.  This is because we
            perform a glClear which clears the framebuffer and then only
            update the new area.
            Note that you can also achieve interesting effects by modifying
            the screen surface alpha channel.  It's set to 255 by default..
          *)
  x := x + delta_x;
  if ( x < 0 ) then
  begin
    x := 0;
    delta_x := -delta_x;
  end
  else if ( ( x + w ) > screen.w ) then
  begin
    x := screen.w - w;
    delta_x := -delta_x;
  end;
  y := y + delta_y;
  if ( y < 0 ) then
  begin
    y := 0;
    delta_y := -delta_y;
  end
  else if ( ( y + h ) > screen.h ) then
  begin
    y := screen.h - h;
    delta_y := -delta_y;
  end;
  dst.x := x;
  dst.y := y;
  dst.w := w;
  dst.h := h;
  SDL_BlitSurface( image, nil, screen, @dst );
  (* Show the image on the screen *)
  SDL_UpdateRects( screen, 1, @dst );
end;

function RunGLTest( logo : Boolean; slowly : Boolean; bpp : integer; gamma : single ) : integer;
const
  color : array[ 0..7, 0..2 ] of single =
  ( ( 1.0, 1.0, 0.0 ),
    ( 1.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 0.0 ),
    ( 0.0, 1.0, 0.0 ),
    ( 0.0, 1.0, 1.0 ),
    ( 1.0, 1.0, 1.0 ),
    ( 1.0, 0.0, 1.0 ),
    ( 0.0, 0.0, 1.0 ) );
  cube : array[ 0..7, 0..2 ] of single =
  ( ( 0.5, 0.5, -0.5 ),
    ( 0.5, -0.5, -0.5 ),
    ( -0.5, -0.5, -0.5 ),
    ( -0.5, 0.5, -0.5 ),
    ( -0.5, 0.5, 0.5 ),
    ( 0.5, 0.5, 0.5 ),
    ( 0.5, -0.5, 0.5 ),
    ( -0.5, -0.5, 0.5 ) );

var
  i : integer;
  rgb_size : array[ 0..2 ] of integer;
  w : integer;
  h : integer;
  done : Boolean;
  frames : integer;
  start_time, this_time : Uint32;
  video_flags : Uint32;
  value : integer;
  gl_error : TGLenum;
  sdl_error : Pchar;
  event : TSDL_Event;
  aspect : single;
begin
  w := 640;
  h := 480;
  done := false;

  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Couldn''t initialize SDL : %s', [ SDL_GetError ] ), 'HandleEvent' );
    SDL_Quit; Halt( 1 );
  end;
  (* See if we should detect the display depth *)
  if ( bpp = 0 ) then
  begin
    if ( SDL_GetVideoInfo.vfmt.BitsPerPixel <= 8 ) then
    begin
      bpp := 8;
    end
    else
    begin
      bpp := 16; (* More doesn't seem to work *)
    end;
  end;

  (* Set the flags we want to use for setting the video mode *)
  if ( logo and USE_DEPRECATED_OPENGLBLIT ) then
  begin
    video_flags := SDL_OPENGLBLIT;
  end
  else
  begin
    video_flags := SDL_OPENGL;
  end;

  for i := 1 to ParamCount - 1 do
  begin
    if ( ParamStr( i ) = '-fullscreen' ) then
    begin
      video_flags := video_flags or SDL_FULLSCREEN;
    end;
  end;
  
  (* Initialize the display *)
  case ( bpp ) of
    8 :
      begin
        rgb_size[ 0 ] := 3;
        rgb_size[ 1 ] := 3;
        rgb_size[ 2 ] := 2;
      end;
    15..16 :
      begin
        rgb_size[ 0 ] := 5;
        rgb_size[ 1 ] := 5;
        rgb_size[ 2 ] := 5;
      end;
  else
    rgb_size[ 0 ] := 8;
    rgb_size[ 1 ] := 8;
    rgb_size[ 2 ] := 8;
  end;
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, rgb_size[ 0 ] );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, rgb_size[ 1 ] );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, rgb_size[ 2 ] );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  if ( SDL_SetVideoMode( w, h, bpp, video_flags ) = nil ) then
  begin
    Log.LogError( Format( 'Couldn''t set GL mode : %s', [ SDL_GetError ] ), 'RunGLTest' );
    SDL_Quit;
    Halt( 1 );
  end;
  Log.LogStatus( Format( 'Screen BPP: %d', [ SDL_GetVideoSurface.format.BitsPerPixel ] ), 'RunGLTest' );
  Log.LogStatus( '', 'RunGLTest' );
  Log.LogStatus( Format( 'Vendor     : %s', [ glGetString( GL_VENDOR ) ] ), 'RunGLTest' );
  Log.LogStatus( Format( 'Renderer   : %s', [ glGetString( GL_RENDERER ) ] ), 'RunGLTest' );
  Log.LogStatus( Format( 'Version    : %s', [ glGetString( GL_VERSION ) ] ), 'RunGLTest' );
  Log.LogStatus( Format( 'Extensions : %s', [ glGetString( GL_EXTENSIONS ) ] ), 'RunGLTest' );
  Log.LogStatus( '', 'RunGLTest' );
  SDL_GL_GetAttribute( SDL_GL_RED_SIZE, value );
  Log.LogStatus( Format( 'SDL_GL_RED_SIZE: requested %d, got %d', [ rgb_size[ 0 ], value ] ), 'RunGLTest' );
  SDL_GL_GetAttribute( SDL_GL_GREEN_SIZE, value );
  Log.LogStatus( Format( 'SDL_GL_GREEN_SIZE: requested %d, got %d', [ rgb_size[ 1 ], value ] ), 'RunGLTest' );
  SDL_GL_GetAttribute( SDL_GL_BLUE_SIZE, value );
  Log.LogStatus( Format( 'SDL_GL_BLUE_SIZE: requested %d, got %d', [ rgb_size[ 2 ], value ] ), 'RunGLTest' );
  SDL_GL_GetAttribute( SDL_GL_DEPTH_SIZE, value );
  Log.LogStatus( Format( 'SDL_GL_DEPTH_SIZE: requested %d, got %d', [ bpp, value ] ), 'RunGLTest' );
  SDL_GL_GetAttribute( SDL_GL_DOUBLEBUFFER, value );
  Log.LogStatus( Format( 'SDL_GL_DOUBLEBUFFER: requested 1, got %d', [ value ] ), 'RunGLTest' );
  (* Set the window manager title bar *)
  SDL_WM_SetCaption( 'JEDI-SDL GL test', 'testgl' );
  (* Set the gamma for the window *)
  if ( gamma <> 0.0 ) then
  begin
    SDL_SetGamma( gamma, gamma, gamma );
  end;
  glViewport( 0, 0, w, h );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity( );
  // This makes the cube rectangular on screen */
  //glOrtho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0 );
  // This makes the cube equisided on screen */
  aspect := 2.0 / ( w / h );
  glOrtho(-2.0, 2.0, -aspect, aspect, -20.0, 20.0);


  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity( );
  glEnable( GL_DEPTH_TEST );
  glDepthFunc( GL_LESS );
  glShadeModel( GL_SMOOTH );
  (* Loop until done. *)
  start_time := SDL_GetTicks;
  frames := 0;
  while ( not done ) do
  begin
    (* Do our drawing, too. *)
    glClearColor( 0.0, 0.0, 0.0, 1.0 );
    glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
    glBegin( GL_QUADS );
{$IFDEF SHADED_CUBE}
    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );
    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );

    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );

    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );

    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
{$ELSE} // flat cube
    glColor3f( 1.0, 0.0, 0.0 );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 1 ] );
    glVertex3fv( @cube[ 2 ] );
    glVertex3fv( @cube[ 3 ] );

    glColor3f( 0.0, 1.0, 0.0 );
    glVertex3fv( @cube[ 3 ] );
    glVertex3fv( @cube[ 4 ] );
    glVertex3fv( @cube[ 7 ] );
    glVertex3fv( @cube[ 2 ] );

    glColor3f( 0.0, 0.0, 1.0 );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 6 ] );
    glVertex3fv( @cube[ 1 ] );

    glColor3f( 0.0, 1.0, 1.0 );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 4 ] );
    glVertex3fv( @cube[ 7 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3f( 1.0, 1.0, 0.0 );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 3 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3f( 1.0, 0.0, 1.0 );
    glVertex3fv( @cube[ 6 ] );
    glVertex3fv( @cube[ 1 ] );
    glVertex3fv( @cube[ 2 ] );
    glVertex3fv( @cube[ 7 ] );
{$ENDIF} (* SHADED_CUBE *)
    glEnd( );

    glMatrixMode( GL_MODELVIEW );
    glRotatef( 5.0, 1.0, 1.0, 1.0 );
    (* Draw 2D logo onto the 3D display *)
    if ( logo ) then
    begin
      if ( USE_DEPRECATED_OPENGLBLIT ) then
      begin
        DrawLogoBlit;
      end
      else
      begin
        DrawLogoTexture;
      end;
    end;
    SDL_GL_SwapBuffers( );
    (* Check for error conditions. *)
    gl_error := glGetError( );
    if ( gl_error <> GL_NO_ERROR ) then
    begin
      Log.LogError( Format( 'testgl: OpenGL error: %d', [ gl_error ] ), 'RunGLTest' );
    end;
    sdl_error := SDL_GetError( );
    if ( Length( sdl_error ) <> 0 ) then
    begin
      Log.LogError( Format( 'testgl: SDL error %s ', [ sdl_error ] ), 'RunGLTest' );
      SDL_ClearError;
    end;
    (* Allow the user to see what's happening *)
    if ( slowly ) then
    begin
      SDL_Delay( 20 );
    end;
    (* Check if there's a pending event. *)
    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      done := HandleEvent( @event );
    end;
    inc( frames );
  end;
  (* Pr : integer out the frames per second *)
  this_time := SDL_GetTicks;
  if ( this_time <> start_time ) then
  begin
    Log.LogStatus( Format( '%2.2f FPS', [ ( frames / ( this_time - start_time ) ) * 1000.0 ] ), 'RunGLTest' );
  end;
  (* Destroy our GL context, etc. *)
  SDL_Quit;
  result := ( 0 );
end;

var
  i : integer;
  numtests : integer;
  bpp : integer = 0;
  logo : Boolean = false;
  slowly : Boolean = false;
  gamma : single = 0.0;
begin
  logo := false;
  slowly := false;
  numtests := 1;
  for i := 1 to ParamCount do
  begin
    if ( ParamStr( i ) = '-twice' ) then
    begin
      inc( numtests );
    end;
    if ( ParamStr( i ) = '-logo' ) then
    begin
      logo := true;
      USE_DEPRECATED_OPENGLBLIT := false;
    end;
    if ( ParamStr( i ) = '-logoblit' ) then
    begin
      logo := true;
      USE_DEPRECATED_OPENGLBLIT := True;
    end;
    if ( ParamStr( i ) = '-slow' ) then
    begin
      slowly := true;
    end;
    if ( ParamStr( i ) = '-bpp' ) then
    begin
      bpp := StrToInt( ParamStr( i + 1 ) );
    end;
    if ( ParamStr( i ) = '-gamma' ) then
    begin
      gamma := StrToFloat( ParamStr( i + 1 ) ); //( float )atof( argv : array[ 0.. + +i - 1 ] of );
    end;
    if ( ParamStr( i ) = '-h' ) then
    begin
      Log.LogStatus( 'Usage : -twice -logo -slow -gamma N -bpp N ', 'MAIN' );
      SDL_Quit;
      Halt( 0 );
    end;
  end;
  LoadOpenGL;
  for i := 0 to numtests - 1 do
  begin
    RunGLTest( logo, slowly, bpp, gamma );
  end;
  SDL_Quit;
  UnLoadOpenGL;
end.


