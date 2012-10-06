program JEDISDLOpenGLEnvMap;
{******************************************************************}
{                                                                  }
{                  Environment Map Demo                            }
{                                                                  }
{ Portions created by Phil Freeman <phil@philfreeman.org.uk>, are  }
{ Copyright (C) 2001 Phil Freeman.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2002 Dominique Louis.                              }
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
{  Shows how to use OpenGL with the SDL libraries                  }
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
{   May    09 2002 - DL : Initial translation.                     }
{                                                                  }
{******************************************************************}
uses
  OpenGL12,
  SysUtils,
  Vectors in '../Utils/Vectors.pas',
  Logger,
  SDL;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;
  APP_TITLE = 'Phil Freeman''s OpenGL Environment Map Demo using JEDI-SDL';

var
  // This is our SDL surface
  surface : PSDL_Surface;
  Torus : Cardinal; // Torus Display List
  Tex, Envmap : Cardinal; // Texture IDs
  //Status indicator
  Status : Boolean = false;

procedure TerminateApplication;
begin
  glDeleteLists( Torus, 1 );
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;
// Load Bitmaps And Convert To Textures

function LoadGLTexture( fileName : string; var TexID : Cardinal ) : Boolean;
var
  // Create storage space for the texture
  TextureImage : PSDL_Surface;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage := SDL_LoadBMP( PChar( fileName ) );
  if ( TextureImage <> nil ) then
  begin
    // Set the status to true
    Status := true;
    // Delete any existing texture
    glDeleteTextures( 1, @TexID );
    // Create Texture
    glGenTextures( 1, @TexID );
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture( GL_TEXTURE_2D, TexID );
    // Linear Filtering
    // scale linearly when image bigger than texture
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    // scale linearly when image smaller than texture
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    // Generate The Texture
    {glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
     TextureImage.h, 0, GL_BGR,
     GL_UNSIGNED_BYTE, TextureImage.pixels ); }
    gluBuild2DMipmaps( GL_TEXTURE_2D, 3, TextureImage.w, TextureImage.h, GL_BGR, GL_UNSIGNED_BYTE, TextureImage.pixels );
  end
  else
  begin
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    TerminateApplication;
  end;
  // Free up any memory we may have used
  if ( TextureImage <> nil ) then
    SDL_FreeSurface( TextureImage );
  result := Status;
end;
// function to reset our viewport after a window resize

function ResizeWindow( width : integer; height : integer ) : Boolean;
begin
  // Protect against a divide by zero
  if ( height = 0 ) then
    height := 1;
  // Setup our viewport.
  glViewport( 0, 0, width, height );
  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  // Set our perspective
  gluPerspective( 45.0, width / height, 0.1, 100.0 );
  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );
  // Reset The View
  glLoadIdentity;
  result := true;
end;
// function to handle key press events

procedure HandleKeyPress( keysym : PSDL_keysym );
begin
  case keysym.sym of
    SDLK_ESCAPE :
      // ESC key was pressed
      TerminateApplication;
    SDLK_RETURN :
      begin
        if ( keysym.Modifier and KMOD_ALT <> 0 ) then
        begin
          {* Alt+Enter key was pressed
           * this toggles fullscreen mode
           *}
          SDL_WM_ToggleFullScreen( surface );
        end;
      end;
  end;
end;

function BuildTorus( Radius, Width : Single; Stacks, Slices : Integer ) : Cardinal;

  procedure DrawPoint( I, J : Integer );
  var
    Position : T3DVector;
    Normal : T3DVector;
  begin
    // This procedure draws a single point on the torus to
    // OpenGL. The point drawn is based on the two
    // parameters I and J.

    // First, calculate the position vector, by rotating it
    // around the X and Y axes.
    Position.Y := Width * Sin( I / Stacks * 2 * Pi );
    Position.Z := Radius + Width * Cos( I / Stacks * 2 * Pi );
    Position.X := Position.Z * Cos( J / Slices * 2 * Pi );
    Position.Z := Position.Z * Sin( J / Slices * 2 * Pi );

    // Then, calculate the Normal vector in the same way.
    Normal.Y := Sin( I / Stacks * 2 * Pi );
    Normal.Z := Cos( I / Stacks * 2 * Pi );
    Normal.X := Normal.Z * Cos( J / Slices * 2 * Pi );
    Normal.Z := Normal.Z * Sin( J / Slices * 2 * Pi );

    // Send the two vectors to OpenGL.
    glTexCoord2f( I / Stacks, J / Slices );
    glNormal3fv( @Normal );
    glVertex3fv( @Position );
  end;

var
  I, J : Integer;
begin
  // Create and initialize the display list.

  Result := glGenLists( 1 );
  glNewList( Result, GL_COMPILE );

  for I := 0 to Stacks - 1 do
  begin

    // Render the torus using quad strips.

    glBegin( GL_QUAD_STRIP );

    for J := 0 to Slices do
    begin

      DrawPoint( I, J );
      DrawPoint( I + 1, J );

    end;

    glEnd;

  end;

  glEndList;
end;
// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL : Boolean;
begin
  // Load in the texture
  if ( not LoadGLTexture( 'images/tex.bmp', Tex ) ) then
  begin
    result := false;
    exit;
  end;
  if ( not LoadGLTexture( 'images/Envmap.bmp', EnvMap ) ) then
  begin
    result := false;
    exit;
  end;
  // Enable Texture Mapping
  glEnable( GL_TEXTURE_2D );
  // Create the torus object
  Torus := BuildTorus( 1.0, 0.3, 50, 50 );
  // Create lighting
  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );
  // Select the correct blend equation
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE );
  // Enable smooth shading
  glShadeModel( GL_SMOOTH );
  // Set the background black
  glClearColor( 0.0, 0.0, 0.0, 0.0 );
  // Depth buffer setup
  glClearDepth( 1.0 );
  // Enables Depth Testing
  glEnable( GL_DEPTH_TEST );
  // The Type Of Depth Test To Do
  glDepthFunc( GL_LEQUAL );
  // Really Nice Perspective Calculations
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

  result := true;
end;
// The main drawing function.

procedure DrawGLScene;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glLoadIdentity;

  glTranslatef( 0, 0, -5 );
  glRotatef( SDL_GetTicks / 25, 1, 1, 1 );

  // First, render everything as usual, with the
  // normal texture
  glDisable( GL_BLEND );
  glBindTexture( GL_TEXTURE_2D, Tex );
  glCallList( Torus );

  // Enable env mapping
  glEnable( GL_TEXTURE_GEN_S );
  glEnable( GL_TEXTURE_GEN_T );
  glTexGeni( GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP );
  glTexGeni( GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP );

  // Render again, using the envmap.
  glEnable( GL_BLEND );
  glBindTexture( GL_TEXTURE_2D, Envmap );
  glCallList( Torus );

  // Disable env mapping
  glDisable( GL_TEXTURE_GEN_S );
  glDisable( GL_TEXTURE_GEN_T );
  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;
var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
begin
  // Load the appropriate .DLL or .SO
  LoadOpenGL;
  // Initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogError( Format( 'Could not initialize SDL : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;
  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;
  if ( videoInfo = nil ) then
  begin
    Log.LogError( Format( 'Video query failed : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;
  // the flags to pass to SDL_SetVideoMode
  videoFlags := SDL_OPENGL; // Enable OpenGL in SDL
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware
  // This checks to see if surfaces can be stored in memory
  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;
  // This checks if hardware blits can be done * /
  if ( videoInfo.blit_hw <> 0 ) then
    videoFlags := videoFlags or SDL_HWACCEL;
  // Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  // Set the title bar in environments that support it
  SDL_WM_SetCaption( APP_TITLE, nil );
  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing
  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ),
      'Main' );
    TerminateApplication;
  end;
  // Loop, drawing and checking events
  InitGL;
  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );
  Done := False;
  while ( not Done ) do
  begin
    // This could go in a separate function
    while ( SDL_PollEvent( @event ) = 1 ) do
    begin
      case event.type_ of
        SDL_QUITEV :
          begin
            Done := true;
          end;
        SDL_KEYDOWN :
          begin
            // handle key presses
            HandleKeyPress( @event.key.keysym );
          end;
        SDL_VIDEORESIZE :
          begin
            surface := SDL_SetVideoMode( event.resize.w, event.resize.h, SCREEN_BPP, videoflags );
            if ( surface = nil ) then
            begin
              Log.LogError( Format( 'Could not get a surface after resize : %s', [ SDL_GetError ] ),
                'Main' );
              TerminateApplication;
            end;
            InitGL;
            ResizeWindow( event.resize.w, event.resize.h );
          end;
      end;
    end;
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.
