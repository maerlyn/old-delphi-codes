program glmovie;
{******************************************************************}
{                                                                  }
{       Object Pascal Example of using smpeg with OpenGL           }
{       Conversion of the glmovie Demo                             }
{                                                                  }
{ Portions created by Sam Lantinga <slouken@devolution.com>,  are  }
{ Copyright (C) 1998 Sam Lantinga.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : glmovie.c                               }
{                                                                  }
{ The original Pascal code is : glmovie.dpr                        }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2001 Dominique Louis.                              }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Romi Kuntsman <romik@users.sourceforge.net>                           }
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
{   GLMovie : Shows how to load and play an Mpeg file using OpenGL }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL, smpeg and OpenGL somewhere         }
{   in your path .                                                 }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   This demo shows how to load and play an mpeg file using smpeg  }
{   with OpenGLv                                                   }
{   You will need Smpeg libraris and OpenGL in order for this demo }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{ December  02 2001 - DL : Initial translation.                    }
{   June    21 2002 - RK : Fixed DL's silly mistakes               }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  OpenGL12,
  smpeg,
  SDL,
  Logger;

type
  { Some data is redundant at this stage. }
  PGLMovieTexture = ^TGLMovieTexture;
  TGLMovieTexture = record
    id : TGLuint; (* OpenGL texture id. *)
    poly_width : TGLuint; (* Quad width for tile. *)
    poly_height : TGLuint; (* Quad height for tile. *)
    movie_width : TGLuint; (* Width of movie inside tile. *)
    movie_height : TGLuint; (* Height of movie inside tile. *)
    skip_rows : TGLuint; (* Number of rows of movie to skip *)
    skip_pixels : TGLuint; (* Number of columns of movie to skip *)
    row : TGLuint; (* Row number of tile in scheme. *)
    col : TGLuint; (* Column number of tile in scheme. *)
  end;

type
  TGLuintArray = array of TGLuint;
  PGLuintArray = ^TGLuintArray;
  TGLMovieTextureArray = array of TGLMovieTexture;
  PGLMovieTextureArray = ^TGLMovieTextureArray;

var
  (* Our evil maximum texture size. Boo 3Dfxnot  *)
  texture_size : TGLuint = 256; (* Keep this around for easy freeing later. *)
  texture_ids : TGLuintArray; (* Our main data. *)
  textures : TGLMovieTextureArray;
  num_texture_rows : TGLuint = 0;
  num_texture_cols : TGLuint = 0; (* Width and height of all tiling. *)
  tiled_width : TGLuint = 0;
  tiled_height : TGLuint = 0; (* Width and height of entire movie. *)
  movie_width : TGLuint = 0;
  movie_height : TGLuint = 0;

  (*
  * Draw the frame data.
  *
  * Parameters:
  *    frame: Actual RGBA frame data
  *)

procedure glmovie_draw( frame : PGLubyte );
var
  i : TGLuint;
  shift : TGLdouble;
begin
  glClear( GL_COLOR_BUFFER_BIT );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  shift := 1 / ( texture_size );
  for i := 0 to num_texture_rows * num_texture_cols - 1 do
  begin
    glBindTexture( GL_TEXTURE_2D, textures[ i ].id );
    glPixelStorei( GL_UNPACK_ROW_LENGTH, movie_width );
    glPixelStorei( GL_UNPACK_SKIP_ROWS, textures[ i ].skip_rows );
    glPixelStorei( GL_UNPACK_SKIP_PIXELS, textures[ i ].skip_pixels );
    glTexSubImage2D( GL_TEXTURE_2D, 0, 0, (* offset_x *) 0, (* offset_y *) textures[ i ].movie_width + 2, textures[ i ].movie_height + 2, GL_RGBA, GL_UNSIGNED_BYTE, frame );
    glBegin( GL_QUADS );
      glTexCoord2f( shift, shift );
      glVertex2i( textures[ i ].col * texture_size, textures[ i ].row * texture_size );
      glTexCoord2f( shift, shift + ( textures[ i ].movie_height ) / ( texture_size ) );
      glVertex2i( textures[ i ].col * texture_size, ( textures[ i ].row + 1 ) * texture_size );
      glTexCoord2f( shift + ( textures[ i ].movie_width ) / ( texture_size ), shift + ( textures[ i ].movie_height ) / ( texture_size ) );
      glVertex2i( ( textures[ i ].col + 1 ) * texture_size, ( textures[ i ].row + 1 ) * texture_size );
      glTexCoord2f( shift + ( textures[ i ].movie_width ) / ( texture_size ), shift );
      glVertex2i( ( textures[ i ].col + 1 ) * texture_size, textures[ i ].row * texture_size );
    glEnd;
  end;
end;

{*
 * Calculates the next power of 2 given a particular value.
 * Useful for calculating proper texture sizes for non power-of-2
 * aligned texures.
 * Parameters:
 *     seed: Value to begin from
 * Returns:
 *     Next power of 2 beginning from 'seed'
 *}

function glmovie_next_power_of_2( seed : GLuint ) : GLuint;
var
  i : GLuint;
begin
  i := 1;
  while ( i < seed ) do
  begin
    i := i * 2;
  end;

  result := i;
end;

(*
 * Initialize the movie player subsystem with the width and height
 * of the *movie data* (as opposed to the window).
 *
 * Parameters:
 *     width: Width of movie in pixels
 *     height: Height of movie in pixels
 * result :=s:
 *     GL_NO_ERROR on success
 *     Any of the enumerated GL errors on failure
 *)

function glmovie_init( Width : GLuint; Height : TGLuint ) : TGLenum;
type
  PGLubyteArray = ^TGLubyteArray;
  TGLubyteArray = array of TGLubyte;
var
  (* Initial black texels. *)
  pixels : TGLubyteArray;
  (* Absolute offsets from within tiled frame. *)
  //offset_x: GLuint;
  //offset_y: GLuint;
  skip_rows : GLuint;
  skip_pixels : GLuint;
  i, j, current : GLuint;
begin
  skip_rows := 0;
  current := 0;
  (* Save original movie dimensions. *)
  movie_width := width;
  movie_height := height;
  (* Get the power of 2 dimensions. *)
  tiled_width := glmovie_next_power_of_2( width );
  tiled_height := glmovie_next_power_of_2( height );
  while ( ( texture_size > tiled_width ) or ( texture_size > tiled_height ) ) do
  begin
    texture_size := texture_size div 2;
  end;
  (* Now break it up into quads. *)
  num_texture_rows := tiled_height div texture_size;
  num_texture_cols := tiled_width div texture_size;
  (* Time for fun with data type = record *)
  glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );
  glEnable( GL_TEXTURE_2D );
  glEnable( GL_DITHER );
  
  SetLength( texture_ids, num_texture_rows * num_texture_cols );
  if ( texture_ids = nil ) then
  begin
    result := GL_OUT_OF_MEMORY;
    exit;
  end;

  glGenTextures( num_texture_rows * num_texture_cols, @texture_ids[0] );
  SetLength( textures, num_texture_rows * num_texture_cols );

  if ( textures = nil ) then
  begin
    glDeleteTextures( num_texture_rows * num_texture_cols, @texture_ids[0] );
    SetLength( texture_ids, 0 );
    result := GL_OUT_OF_MEMORY;
    exit;
  end;

  for i := 0 to num_texture_rows - 1 do
  begin
    skip_pixels := 0;
    for j := 0 to num_texture_cols - 1 do
    begin
      current := i * num_texture_cols + j;
      (* Setup texture. *)
      textures[ current ].id := texture_ids[ current ];
      textures[ current ].poly_width := texture_size;
      textures[ current ].poly_height := texture_size;
      textures[ current ].movie_width := ( movie_width - 2 ) * ( j + 1 ) div num_texture_cols - skip_pixels;
      textures[ current ].movie_height := ( movie_height - 2 ) * ( i + 1 ) div num_texture_rows - skip_rows;
      textures[ current ].row := i;
      textures[ current ].col := j;
      textures[ current ].skip_pixels := skip_pixels;
      textures[ current ].skip_rows := skip_rows;
      skip_pixels := skip_pixels + textures[ current ].movie_width;

      SetLength( pixels, textures[ current ].poly_width * textures[ current ].poly_height * 4 );
      if ( pixels = nil ) then
      begin
        glDeleteTextures( num_texture_rows * num_texture_cols, @texture_ids[0] );
        SetLength( texture_ids, 0 );
        SetLength( textures, 0 );
        result := GL_OUT_OF_MEMORY;
        exit;
      end;
      //FillChar( pixels^, textures[ current ].poly_width * textures[ current ].poly_height * 4, 0 );

      (* Do all of our useful binding. *)
      glBindTexture( GL_TEXTURE_2D, textures[ current ].id );
      glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      (* Specify our 256x256 black texture. *)
      glTexImage2D( GL_TEXTURE_2D,
        0,
        GL_RGB,
        textures[ current ].poly_width,
        textures[ current ].poly_height,
        0,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        @pixels[0] );
        SetLength( pixels, 0 );
    end;
    skip_rows := skip_rows + textures[ current ].movie_height;
  end;
  (* Simple state setup at the end. *)
  glClearColor( 0.0, 0.0, 0.0, 0.0 );
  result := glGetError( );
end;

//******************* glmpeg_update *************************

procedure glmpeg_update( surface : PSDL_Surface; x : Sint32; y : Sint32; w : Uint32;
  h : Uint32 ); cdecl;
var
  error : TGLenum;
begin
  glmovie_draw( PGLubyte( surface.pixels ) );
  error := glGetError( );
  if ( error <> GL_NO_ERROR ) then
  begin
    Log.LogError( Format( 'glmovie: GL error: %s', [ gluErrorString( error ) ] ),
      'glmpeg_update' );
    Exit;
  end;
  SDL_GL_SwapBuffers;
end;

{*
 * Here we need to center the OpenGL viewport within the
 * window size that we are given.
 *
 * Parameters:
 *     width: Width of the window in pixels
 *     height: Height of the window in pixels
 *}

procedure glmovie_resize( width : GLuint; height : GLuint );
begin
  glViewport( 0, 0, width, height );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  gluOrtho2D( 0, tiled_width, tiled_height, 0 );
end;

{*
 * Free any resources associated with the movie player.
 *}

procedure glmovie_quit;
begin
  glDeleteTextures( num_texture_rows * num_texture_cols, @texture_ids );
  SetLength( texture_ids, 0 );
  SetLength( textures, 0 );
end;

var
  mpeg : PSMPEG;
  mpeg_info : TSMPEG_Info;
  screen : PSDL_Surface;
  surface : PSDL_Surface;
  event : TSDL_Event;
begin

  if ( ParamCount < 1 ) then
  begin
    Log.LogError( Format( 'Usage: %s file.mpg', [ ParamStr( 0 ) ] ), 'Main' );
    Exit;
  end;

  LoadOpenGL;
  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_AUDIO ) < 0 ) then
  begin
    Log.LogError( 'glmovie: I couldn''t initizlize SDL(shrug)', 'Main' );
    Exit;
  end;

  mpeg := SMPEG_new( PChar( ParamStr( 1 ) ), @mpeg_info, 1 );
  if ( mpeg = nil ) then
  begin
    Log.LogError( Format( 'glmovie: I''m not so sure about this %s file...',
      [ ParamStr( 1 ) ] ), 'Main' );
    SDL_Quit;
    Exit;
  end;
  (* Grab the mouse and input and set the video mode *)
  SDL_ShowCursor( 0 );
  SDL_WM_GrabInput( SDL_GRAB_ON );

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it
  SDL_WM_SetCaption('SDL GLMovie Demo using JEDI-SDL', nil
    );

  screen := SDL_SetVideoMode( 640, 480, 16, SDL_OPENGL { or SDL_FULLSCREEN } );
  if ( Screen = nil ) then
  begin
    Log.LogError( Format( 'glmovie: Couldn''t set 640 x 480 GL video mode : %s',
      [ SDL_GetError ] ), 'Main' );
    SDL_Quit;
    Exit;
  end;

  (* Everything needs to be in RGB for GL, but needs to be 32-bit for SMPEG. *)
  surface := SDL_AllocSurface( SDL_SWSURFACE,
    mpeg_info.width,
    mpeg_info.height,
    32,
    $000000FF,
    $0000FF00,
    $00FF0000,
    $FF000000 );

  if ( surface = nil ) then
  begin
    Log.LogError( 'glmovie: I couldn''t make a surface(boo hoo)', 'Main' );
    SDL_Quit;
    Exit;
  end;

  (* *Initialize* with mpeg size. *)
  if ( glmovie_init( mpeg_info.width, mpeg_info.height ) <> GL_NO_ERROR ) then
  begin
    Log.LogError( 'glmovie: glmovie_init failed ', 'Main' );
    SDL_Quit;
    Exit;
  end;

  (* *Resize* with window size. *)
  glmovie_resize( screen.w, screen.h );
  SMPEG_setdisplay( mpeg, surface, nil, @glmpeg_update );
  SMPEG_play( mpeg );

  while ( SMPEG_status( mpeg ) = SMPEG_PLAYING ) do
  begin

    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case ( event.type_ ) of
        SDL_KEYDOWN :
          begin
            if ( event.key.keysym.sym = SDLK_ESCAPE ) then
            begin
              SMPEG_stop( mpeg );
            end;
          end;
        SDL_MOUSEBUTTONDOWN, SDL_QUITEV :
          SMPEG_stop( mpeg );
      end;
      SDL_Delay( 100 );
    end;
    glmpeg_update(surface,0,0,0,0);
  end;

  glmovie_quit;
  SDL_Quit;
  UnLoadOpenGL;
end.

