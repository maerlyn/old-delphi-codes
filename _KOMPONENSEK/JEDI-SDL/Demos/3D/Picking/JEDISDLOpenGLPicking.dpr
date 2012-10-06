program JEDISDLOpenGLPicking;
{******************************************************************}
{                                                                  }
{                       Picking Demo                               }
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
  Logger,
  SDL;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;
  APP_TITLE = 'Phil Freeman''s OpenGL Picking Demo using JEDI-SDL';

type
  PSelection = ^TSelection; // TSelection holds information
  TSelection = record // from the hit record
    Names : Integer; // The number of names in this hit
    zNear : Integer; // Near z value
    zFar : Integer; // Far z value
    ID : Integer; // ID of the item picked
  end;

const
  MapSize = 32; { Lightmap size }
  NumLights = 3; { Number of lights }
  Ambience : Single = 0.2; { Scene ambience }
  MinLight : Single = 0.2; { Inner circle of light }
  MaxLight : Single = 1.0; { Outer circle of light }

var
  // This is our SDL surface
  surface : PSDL_Surface;
  Quadric : PGLUQuadric; // Quadric object
  Shapes : array[ 0..6 ] of Boolean; // Flags for each object
  //Status indicator
  Status : Boolean = false;

procedure TerminateApplication;
begin
  gluDeleteQuadric( Quadric );
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
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


// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL : Boolean;
begin
  // Create the quadric object
  Quadric := gluNewQuadric;

  // Enable lighting
  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );

  // Enable colour tracking
  glEnable( GL_COLOR_MATERIAL );
  glColorMaterial( GL_FRONT, GL_AMBIENT_AND_DIFFUSE );

  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );
  glShadeModel( GL_SMOOTH );
  glClearColor( 0, 0, 0, 0 );

  glEnable( GL_DEPTH_TEST );
  glClearDepth( 1 );
  glDepthFunc( GL_LESS );

  result := true;
end;

(*---

  RenderShapes;
  Renders the objects and sets up the name stack

---*)

procedure RenderShapes;
var
  Time : Single;
begin
  Time := SDL_GetTicks / 100;
  glTranslatef( 0, 0, -3 );
  glRotatef( Time * 7.0, 1, 0, 0 );
  glRotatef( Time * 8.0, 0, 1, 0 );
  glRotatef( Time * 9.0, 0, 0, 1 );

  glLoadName( 0 );
  if Shapes[ 0 ] then
    glColor3f( 0.0, 0.0, 1.0 )
  else
    glColor3f( 0.0, 0.0, 0.5 );
  glTranslatef( -1.0, 0.0, 0.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 1 );
  if Shapes[ 1 ] then
    glColor3f( 0.0, 1.0, 0.0 )
  else
    glColor3f( 0.0, 0.5, 0.0 );
  glTranslatef( 2.0, 0.0, 0.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 2 );
  if Shapes[ 2 ] then
    glColor3f( 0.0, 1.0, 1.0 )
  else
    glColor3f( 0.0, 0.5, 0.5 );
  glTranslatef( -1.0, 0.0, 1.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 3 );
  if Shapes[ 3 ] then
    glColor3f( 1.0, 1.0, 0.0 )
  else
    glColor3f( 0.5, 0.5, 0.0 );
  glTranslatef( 0.0, 0.0, -2.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 4 );
  if Shapes[ 4 ] then
    glColor3f( 1.0, 0.5, 0.0 )
  else
    glColor3f( 0.5, 0.2, 0.0 );
  glTranslatef( 0.0, 1.0, 1.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 5 );
  if Shapes[ 5 ] then
    glColor3f( 1.0, 0.0, 1.0 )
  else
    glColor3f( 0.5, 0.0, 0.5 );
  glTranslatef( 0.0, -2.0, 0.0 );
  gluSphere( Quadric, 0.2, 16, 16 );

  glLoadName( 6 );
  if Shapes[ 6 ] then
    glColor3f( 1.0, 0.0, 0.0 )
  else
    glColor3f( 0.5, 0.0, 0.0 );
  glTranslatef( 0.0, 1.0, 0.0 );
  gluSphere( Quadric, 0.3, 16, 16 );
end;

(*---

  ProcessHits

  Processes the hit record
  Hits is the number of hits, as returned by glRenderMode
  Data is a pointer to the selection buffer

---*)

procedure ProcessHits( Hits : Integer; Data : PSelection );
var
  I : Integer;
  ID : Integer; // Object ID
  zNear : Integer; // Near z value
begin
  if ( Hits = 0 ) then Exit; // Exit if the hit record is empty

  ID := Data.ID; // Set up the initial object ID
  zNear := Data.zNear; // Set up the initial z value

  for I := 0 to Hits - 2 do
  begin // For each hit ...
    Inc( Data ); // Move to the next hit
    if ( Data.zNear < zNear ) then
    begin // Test the near z value in order to find the
      ID := Data.ID; // hit nearest to the camera
      zNear := Data.zNear;
    end;
  end;

  Shapes[ ID ] := not Shapes[ ID ]; // Toggle the flag corresponding to the nearest
  // object.
end;

// function to handle key press events

procedure HandleMouseButtonDown( Mouse : TSDL_MouseButtonEvent );
var
  Hits : Integer; // The number of hits
  Viewport : TVector4i; // The OpenGL viewport
  Data : array[ 0..2 ] of TSelection; // The selection buffer
begin
  case Mouse.button of
    SDL_BUTTON_LMASK :
      begin
        glGetIntegerv( GL_VIEWPORT, @Viewport ); // Get the current viewport

        glSelectBuffer( SizeOf( Data ), @Data ); // Set up the selection buffer
        glRenderMode( GL_SELECT ); // Enter selection mode

        glInitNames; // Initialize the name stack
        glPushName( Longword( -1 ) ); // Push a null name onto the stack

        glMatrixMode( GL_PROJECTION ); // Set up the projection matrix
        glPushMatrix;
        glLoadIdentity;
        gluPickMatrix( Mouse.X, Viewport[ 3 ] - Mouse.Y, 2.0, 2.0, // Set up a 2 pixel picking matrix
          Viewport );
        gluPerspective( 45.0, surface.w / surface.h, // Use a perspective projection
          0.1, 100.0 );

        glMatrixMode( GL_MODELVIEW ); // Reset the modelview matrix
        glLoadIdentity;
        RenderShapes; // Render the scene

        glMatrixMode( GL_PROJECTION ); // Reset the projection matrix
        glPopMatrix;

        glMatrixMode( GL_MODELVIEW ); // Return to the modelview matrix
        Hits := glRenderMode( GL_RENDER ); // Exit selection mode and get the number of hits
        ProcessHits( Hits, @Data ); // Process the selection data
      end;
  end;
end;

// The main drawing function.

procedure DrawGLScene;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glLoadIdentity;

  RenderShapes;
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
        SDL_MOUSEBUTTONDOWN :
          begin
            // handle Mouse presses
            HandleMouseButtonDown( event.button );
          end;
        SDL_VIDEORESIZE :
          begin
            surface := SDL_SetVideoMode( event.resize.w, event.resize.h,
              SCREEN_BPP, videoflags );
            if ( surface = nil ) then
            begin
              Log.LogError( Format( 'Could not get a surface after resize : %s',
                [ SDL_GetError ] ),
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


