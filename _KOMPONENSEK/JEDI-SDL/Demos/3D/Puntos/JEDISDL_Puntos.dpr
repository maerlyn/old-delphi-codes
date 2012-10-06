program JEDISDL_Puntos;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Pascal code, comments and convertion to JEDI-SDL created by:     }
{   Gustavo Maximo <maximo@barcelona.com>                          }
{ The OpenGL code by:                                              }
{   Carlos Garcia Trujillo <cgar1136@yahoo.com>                    }
{ The JEDI-SDL code by:                                            }
{   Dominique Louis <Dominique@SavageSoftware.com.au>              }
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
{   July    28 2001 : Initial release.                             }
{ September 17 2001 : DL - Converted to nehe style                 }
{                                                                  }
{******************************************************************}

uses
  OpenGL12,
  SysUtils,
  Logger,
  SDL;

var
  surface : PSDL_Surface;
  Angle : TGLInt;
  next_time: UInt32 = 0;

const
  TICK_INTERVAL = 30; // Global constant to add the timer
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
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
      if (keysym.Modifier and KMOD_ALT <> 0) then
      begin
        {* Alt+Enter key was pressed
         * this toggles fullscreen mode
         *}
        SDL_WM_ToggleFullScreen( surface );
      end;
    end;
  end;
end;

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.
function InitGL : Boolean;
begin
  // Enable Texture Mapping
  glEnable( GL_TEXTURE_2D );

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

procedure DrawGLScene;
Var
  X, Y, Z : TGLInt;
begin
  glClearColor(0,0.2,0,0); // seting background like black green
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // clear all

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  gluLookAt(0,0,3,0,0,0,0,1,0); // camera position

  glRotatef(30,1,0,0); //first we make a rotation of 30 grades in relation of X to obtain the perspective
  glRotatef(Angle,0,1,0); // Rotate the picture in Y with Angle variable

  GLPointSize(2.0); // we assign a size of 2 pixels to each point

  // Now with 3 loops we paint a cube with multicolor points
  GLBegin(GL_POINTS);
  for x := -5 to 5 do
   for y := -5 to 5 do
    for z := -5 to 5 do
    begin
      GLColor3f(X,Y,Z);
      GLVertex3f(X/10,Y/10,Z/10);
    end;
  GLEnd;

  SDL_GL_SwapBuffers; // swap buffers to display, since we're double buffered.
end;

// this function seting th SDL Timer
function TimeLeft: UInt32;
var
  now: UInt32;
begin
  now := SDL_GetTicks; // Get the number of milliseconds since the SDL library initialization.
  if (next_time <= now) then
    begin
      next_time := now + TICK_INTERVAL;
      result := 0;
      exit;
    end;
  Result := next_time - now;
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
    Log.LogError( Format( 'Could not initialize SDL : %s', [SDL_GetError] ),
      'Main' );
    TerminateApplication;
  end;

  // Fetch the video info 
  videoInfo := SDL_GetVideoInfo;

  if ( videoInfo = nil ) then
  begin
    Log.LogError( Format( 'Video query failed : %s', [SDL_GetError] ),
      'Main' );
    TerminateApplication;
  end;

  // the flags to pass to SDL_SetVideoMode 
  videoFlags := SDL_OPENGL;                  // Enable OpenGL in SDL 
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

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption( 'Puntos : OpenGL Code using JEDI-SDL by Gustavo Maximo' , nil);

  videoflags := videoFlags or SDL_RESIZABLE;    // Enable window resizing 

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [SDL_GetError]
      ),
      'Main' );
    TerminateApplication;
  end;

  // initialize OpenGL
  InitGL;
  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

  Done := False;
  while (not Done) do
  begin
    Inc(Angle,4); // Rotate the observation angle of the scene
    Angle := Angle mod 360;
    DrawGLScene;
    SDL_Delay(TimeLeft); // wait...
    // This could go in a separate function */
    while (SDL_PollEvent(@event) = 1) do
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
            Log.LogError( Format( 'Could not get a surface after resize : %s', [SDL_GetError] ),
            'Main' );
            TerminateApplication;
          end;
          InitGL;
          ResizeWindow( event.resize.w, event.resize.h );
        end;
      end;
    end;
  end;
  TerminateApplication;
end.
