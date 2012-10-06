program JEDISDLOpenGLMotionBlur;
{******************************************************************}
{                                                                  }
{                       Motion Blur Demo                           }
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
  APP_TITLE = 'Phil Freeman''s OpenGL Motion Blur Demo using JEDI-SDL';

  Fade            = 0.4;       { Alpha value }

  Trail           = 0.4;       { Trail Length }

  Samples         = 5;         { Number of Samples }

var
  // This is our SDL surface
  surface: PSDL_Surface;

  Scene: GLuInt;               { Display list }
  
  //Status indicator
  Status: Boolean = false;

procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;

// function to reset our viewport after a window resize
function ResizeWindow(width: integer; height: integer): Boolean;
begin
  // Protect against a divide by zero
  if (height = 0) then
    height := 1;
  // Setup our viewport.
  glViewport(0, 0, width, height);
  // change to the projection matrix and set our viewing volume.
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  // Set our perspective
  gluPerspective(60.0, width / height, 0.1, 100.0);
  // Make sure we're changing the model view and not the projection
  glMatrixMode(GL_MODELVIEW);
  // Reset The View
  glLoadIdentity;
  result := true;
end;

// function to handle key press events
procedure HandleKeyPress(keysym: PSDL_keysym);
begin
  case keysym.sym of
    SDLK_ESCAPE:
      // ESC key was pressed
      TerminateApplication;
    SDLK_RETURN:
      begin
        if (keysym.Modifier and KMOD_ALT <> 0) then
        begin
          {* Alt+Enter key was pressed
           * this toggles fullscreen mode
           *}
          SDL_WM_ToggleFullScreen(surface);
        end;
      end;
  end;
end;

procedure RenderCube(Size: Single);
begin
  glBegin(GL_QUADS);

  glNormal3f( 1.0,  0.0,  0.0);
  glVertex3f( Size, -Size, -Size);
  glVertex3f( Size,  Size, -Size);
  glVertex3f( Size,  Size,  Size);
  glVertex3f( Size, -Size,  Size);

  glNormal3f(-1.0,  0.0,  0.0);
  glVertex3f(-Size, -Size,  Size);
  glVertex3f(-Size, -Size, -Size);
  glVertex3f(-Size,  Size, -Size);
  glVertex3f(-Size,  Size,  Size);

  glNormal3f( 0.0,  0.0, -1.0);
  glVertex3f(-Size,  Size, -Size);
  glVertex3f( Size,  Size, -Size);
  glVertex3f( Size, -Size, -Size);
  glVertex3f(-Size, -Size, -Size);

  glNormal3f( 0.0,  0.0,  1.0);
  glVertex3f(-Size, -Size,  Size);
  glVertex3f(-Size,  Size,  Size);
  glVertex3f( Size,  Size,  Size);
  glVertex3f( Size, -Size,  Size);

  glNormal3f( 0.0,  1.0,  0.0);
  glVertex3f( Size,  Size, -Size);
  glVertex3f( Size,  Size,  Size);
  glVertex3f(-Size,  Size,  Size);
  glVertex3f(-Size,  Size, -Size);

  glNormal3f( 0.0, -1.0,  0.0);
  glVertex3f( Size, -Size,  Size);
  glVertex3f(-Size, -Size,  Size);
  glVertex3f(-Size, -Size, -Size);
  glVertex3f( Size, -Size, -Size);

  glEnd;
end;

procedure CreateScene;
begin
  { Create a display list }
  Scene := glGenLists(1);
  glNewList(Scene, GL_COMPILE);

  { Render several coloured cubes }
  glColor3f(0.0, 0.0, 1.0);
  glTranslatef(-1.0, 0.0, 0.0);
  RenderCube(0.2);

  glColor3f(0.0, 1.0, 0.0);
  glTranslatef(2.0, 0.0, 0.0);
  RenderCube(0.2);

  glColor3f(0.0, 1.0, 1.0);
  glTranslatef(-1.0, 0.0, 1.0);
  RenderCube(0.2);

  glColor3f(1.0, 1.0, 0.0);
  glTranslatef(0.0, 0.0, -2.0);
  RenderCube(0.2);

  glColor3f(1.0, 0.5, 0.0);
  glTranslatef(0.0, 1.0, 1.0);
  RenderCube(0.2);

  glColor3f(1.0, 0.0, 1.0);
  glTranslatef(0.0, -2.0, 0.0);
  RenderCube(0.2);

  glColor3f(1.0, 0.0, 0.0);
  glTranslatef(0.0, 1.0, 0.0);
  RenderCube(0.4);

  glEndList;
end;

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.
function InitGL: Boolean;
begin
  { Create the display list }
  CreateScene;

  { Enable lighting }
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);

  { Use glColor to define material colors }
  glEnable(GL_COLOR_MATERIAL);

  { Set up blending }
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glShadeModel(GL_SMOOTH);
  glClearColor(0, 0, 0, 0);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
  glClearDepth(1);

  result := true;
end;

// The main drawing function.
procedure DrawGLScene;
var
  I: Integer;
  T1, T2: Single;          { Frame time }
begin
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity;

  { Get the current time }
  T1 := SDL_GetTicks / 1000;

  for I := 0 to Samples do begin
    { Fade the screen using blending so that older samples
      look faded }
    glLoadIdentity;
    glEnable(GL_BLEND);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    glOrtho(0.0, 1.0, 0.0, 1.0, -1.0, 1.0);

    glColor4f(0.0, 0.0, 0.0, Fade);
    glRectf(0.0, 0.0, 1.0, 1.0);

    glPopMatrix;
    glMatrixMode(GL_MODELVIEW);
    glDisable(GL_BLEND);

    { Clear the depth buffer }
    glClear(GL_DEPTH_BUFFER_BIT);

    { Get the time for this frame }
    T2 := T1 + I / Samples * Trail;

    { Set up rotation for this frame }
    glLoadIdentity;
    glTranslatef(0, 0, -3);
    glRotatef(T2 * 70.0, 1, 0, 0);
    glRotatef(T2 * 80.0, 0, 1, 0);
    glRotatef(T2 * 90.0, 0, 0, 1);

    { Render the scene }
    glCallList(Scene);
  end;
  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done: Boolean;
  event: TSDL_Event;
  videoflags: Uint32;
  videoInfo: PSDL_VideoInfo;
begin
  // Load the appropriate .DLL or .SO
  LoadOpenGL;
  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Could not initialize SDL : %s', [SDL_GetError]),
      'Main');
    TerminateApplication;
  end;
  // Fetch the video info
  videoInfo := SDL_GetVideoInfo;
  if (videoInfo = nil) then
  begin
    Log.LogError(Format('Video query failed : %s', [SDL_GetError]),
      'Main');
    TerminateApplication;
  end;
  // the flags to pass to SDL_SetVideoMode
  videoFlags := SDL_OPENGL; // Enable OpenGL in SDL
  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware
  // This checks to see if surfaces can be stored in memory
  if (videoInfo.hw_available <> 0) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;
  // This checks if hardware blits can be done * /
  if (videoInfo.blit_hw <> 0) then
    videoFlags := videoFlags or SDL_HWACCEL;
  // Set the OpenGL Attributes
  SDL_GL_SetAttribute(SDL_GL_RED_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_GREEN_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_BLUE_SIZE, 5);
  SDL_GL_SetAttribute(SDL_GL_DEPTH_SIZE, 16);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  // Set the title bar in environments that support it
  SDL_WM_SetCaption(APP_TITLE, nil);
  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing
  surface := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags);
  if (surface = nil) then
  begin
    Log.LogError(Format('Unable to create OpenGL screen : %s', [SDL_GetError]
      ),
      'Main');
    TerminateApplication;
  end;
  // Loop, drawing and checking events
  InitGL;
  ReSizeWindow(SCREEN_WIDTH, SCREEN_HEIGHT);
  Done := False;
  while (not Done) do
  begin
    // This could go in a separate function
    while (SDL_PollEvent(@event) = 1) do
    begin
      case event.type_ of
        SDL_QUITEV:
          begin
            Done := true;
          end;
        SDL_KEYDOWN:
          begin
            // handle key presses
            HandleKeyPress(@event.key.keysym);
          end;
        SDL_VIDEORESIZE:
          begin
            surface := SDL_SetVideoMode(event.resize.w, event.resize.h,
              SCREEN_BPP, videoflags);
            if (surface = nil) then
            begin
              Log.LogError(Format('Could not get a surface after resize : %s',
                [SDL_GetError]),
                'Main');
              TerminateApplication;
            end;
            InitGL;
            ResizeWindow(event.resize.w, event.resize.h);
          end;
      end;
    end;
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.


