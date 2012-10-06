program JEDISDLOpenGL37;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Jan Horn <jan@sulaco.co.za>,  are            }
{ Copyright (C) 2001 Jan Horn.                                     }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson37.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL37.dpr                }
{ The initial developer of the Pascal code is :                    }
{ Dean Ellis <dean_ellis@yahoo.com>                                }
{                                                                  }
{ Portions created by Dean Ellis are                               }
{ Copyright (C) 2001 Dean Ellis.                                   }
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
{  October  02 2001 - JH : Initial translation.                    }
{  October  02 2001 - DE : Port to JEDI-SDL                        }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  SDL,
  OpenGL12,
  Logger;

type
  TVector = array[0..2] of TGLFLoat;

var
  textureID: TGLUint;
  texture: PSDL_Surface;
  angle: TGLFloat;
  Vertexes: array[0..3] of TVector;
  normal: TVector;
  elapsedtime: integer;
  screen: PSDL_Surface;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

  globalAmbient: array[0..3] of TGLFloat = (0.2, 0.2, 0.2, 1.0);
  LightPos: array[0..3] of TGLFloat = (0.0, 5.0, 10.0, 1.0);
  LightAmbient: array[0..3] of TGLFloat = (0.2, 0.2, 0.2, 1.0);
  LightDiffuse: array[0..3] of TGLFloat = (0.3, 0.3, 0.3, 1.0);
  LightSpecular: array[0..3] of TGLFloat = (0.8, 0.8, 0.8, 1.0);

  LmodelAmbient: array[0..3] of TGLFloat = (0.2, 0.2, 0.2, 1.0);

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
  gluPerspective(45.0, width / height, 0.1, 100.0);

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
        SDL_WM_ToggleFullScreen(screen);
      end;
    end;
  end;
end;

function EmptyTexture: TGLUint;
begin
  texture := SDL_CreateRGBSurface(SDL_SRCALPHA, 128, 128, 24, 0, 0, 0, 0);
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, 128, 128, 0, GL_BGR, GL_UNSIGNED_BYTE,
    texture.pixels);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure InitGL;
begin
  glClearColor(0.0, 0.0, 0.0, 0.5);
  glShadeModel(GL_SMOOTH);
  glClearDepth(1.0);
  glDepthFunc(GL_LESS);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @LModelAmbient);
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @GLobalAmbient);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPos);
  glLightfv(GL_LIGHT0, GL_AMBIENT, @lightAmbient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, @lightDiffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @lightSpecular);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  TextureID := EmptyTexture;
  glShadeModel(GL_SMOOTH);
  glMateriali(GL_FRONT, GL_SHININESS, 128);
end;

procedure ReduceToUnit(var vector: array of TGLFloat);
var
  length: TGLFLoat;
begin
  // Calculates The Length Of The Vector
  length := sqrt((vector[0] * vector[0]) + (vector[1] * vector[1]) + (vector[2]
    * vector[2]));
  if Length = 0 then
    Length := 1;

  vector[0] := vector[0] / length;
  vector[1] := vector[1] / length;
  vector[2] := vector[2] / length;
end;

procedure calcNormal(const v: array of TVector; var cross: array of TGLFloat);
var
  v1, v2: array[0..2] of TGLFloat;
begin
  // Finds The Vector Between 2 Points By Subtracting
  // The x,y,z Coordinates From One Point To Another.

  // Calculate The Vector From Point 1 To Point 0
  v1[0] := v[0][0] - v[1][0]; // Vector 1.x=Vertex[0].x-Vertex[1].x
  v1[1] := v[0][1] - v[1][1]; // Vector 1.y=Vertex[0].y-Vertex[1].y
  v1[2] := v[0][2] - v[1][2]; // Vector 1.z=Vertex[0].y-Vertex[1].z
  // Calculate The Vector From Point 2 To Point 1
  v2[0] := v[1][0] - v[2][0]; // Vector 2.x=Vertex[0].x-Vertex[1].x
  v2[1] := v[1][1] - v[2][1]; // Vector 2.y=Vertex[0].y-Vertex[1].y
  v2[2] := v[1][2] - v[2][2]; // Vector 2.z=Vertex[0].z-Vertex[1].z
  // Compute The Cross Product To Give Us A Surface Normal
  cross[0] := v1[1] * v2[2] - v1[2] * v2[1]; // Cross Product For Y - Z
  cross[1] := v1[2] * v2[0] - v1[0] * v2[2]; // Cross Product For X - Z
  cross[2] := v1[0] * v2[1] - v1[1] * v2[0]; // Cross Product For X - Y

  ReduceToUnit(cross); // Normalize The Vectors
end;

// Draws A Helix

procedure ProcessHelix;
const
  Twists = 5;
  MaterialColor: array[1..4] of TGLFloat = (0.4, 0.2, 0.8, 1.0);
  Specular: array[1..4] of TGLFloat = (1, 1, 1, 1);
var
  x, y, z: TGLFLoat;
  phi, theta: Integer;
  r, u, v: TGLFLoat;
begin
  glLoadIdentity; // Reset The Modelview Matrix
  gluLookAt(0, 5, 50, 0, 0, 0, 0, 1, 0);
  // Eye Position (0,5,50) Center Of Scene (0,0,0), Up On Y Axis

  glPushMatrix; // Push The Modelview Matrix
  glTranslatef(0, 0, -50); // Translate 50 Units Into The Screen
  glRotatef(angle / 2.0, 1, 0, 0); // Rotate By angle/2 On The X-Axis
  glRotatef(angle / 3.0, 0, 1, 0); // Rotate By angle/3 On The Y-Axis

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, @MaterialColor);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @specular);

  r := 2; // Radius

  glBegin(GL_QUADS); // Begin Drawing Quads
  phi := 0;
  while phi < 360 do
  begin
    theta := 0;
    while theta < 360 * twists do
    begin
      v := phi / 180 * pi; // Calculate Angle Of First Point	(  0 )
      u := theta / 180.0 * pi; // Calculate Angle Of First Point	(  0 )

      x := cos(u) * (2 + cos(v)) * r; // Calculate x Position (1st Point)
      y := sin(u) * (2 + cos(v)) * r; // Calculate y Position (1st Point)
      z := (u - (2 * pi) + sin(v)) * r; // Calculate z Position (1st Point)

      vertexes[0][0] := x; // Set x Value Of First Vertex
      vertexes[0][1] := y; // Set y Value Of First Vertex
      vertexes[0][2] := z; // Set z Value Of First Vertex

      v := (phi / 180 * pi); // Calculate Angle Of Second Point	(  0 )
      u := ((theta + 20) / 180 * pi); // Calculate Angle Of Second Point	( 20 )

      x := cos(u) * (2 + cos(v)) * r; // Calculate x Position (2nd Point)
      y := sin(u) * (2 + cos(v)) * r; // Calculate y Position (2nd Point)
      z := (u - (2 * pi) + sin(v)) * r; // Calculate z Position (2nd Point)

      vertexes[1][0] := x; // Set x Value Of Second Vertex
      vertexes[1][1] := y; // Set y Value Of Second Vertex
      vertexes[1][2] := z; // Set z Value Of Second Vertex

      v := (phi + 20) / 180 * pi; // Calculate Angle Of Third Point	( 20 )
      u := (theta + 20) / 180 * pi; // Calculate Angle Of Third Point	( 20 )

      x := cos(u) * (2 + cos(v)) * r; // Calculate x Position (3rd Point)
      y := sin(u) * (2 + cos(v)) * r; // Calculate y Position (3rd Point)
      z := (u - (2 * pi) + sin(v)) * r; // Calculate z Position (3rd Point)

      vertexes[2][0] := x; // Set x Value Of Third Vertex
      vertexes[2][1] := y; // Set y Value Of Third Vertex
      vertexes[2][2] := z; // Set z Value Of Third Vertex

      v := (phi + 20) / 180 * pi; // Calculate Angle Of Fourth Point	( 20 )
      u := theta / 180 * pi; // Calculate Angle Of Fourth Point	(  0 )

      x := cos(u) * (2 + cos(v)) * r; // Calculate x Position (4th Point)
      y := sin(u) * (2 + cos(v)) * r; // Calculate y Position (4th Point)
      z := (u - (2 * pi) + sin(v)) * r; // Calculate z Position (4th Point)

      vertexes[3][0] := x; // Set x Value Of Fourth Vertex
      vertexes[3][1] := y; // Set y Value Of Fourth Vertex
      vertexes[3][2] := z; // Set z Value Of Fourth Vertex

      calcNormal(vertexes, normal); // Calculate The Quad Normal

      glNormal3f(normal[0], normal[1], normal[2]); // Set The Normal

      // Render The Quad
      glVertex3f(vertexes[0][0], vertexes[0][1], vertexes[0][2]);
      glVertex3f(vertexes[1][0], vertexes[1][1], vertexes[1][2]);
      glVertex3f(vertexes[2][0], vertexes[2][1], vertexes[2][2]);
      glVertex3f(vertexes[3][0], vertexes[3][1], vertexes[3][2]);
      theta := theta + 20;
    end;
    phi := phi + 20;
  end;
  glEnd; // Done Rendering Quads
  glPopMatrix; // Pop The Matrix
end;

// Set Up An Ortho View

procedure ViewOrtho;
begin
  glMatrixMode(GL_PROJECTION); // Select Projection
  glPushMatrix; // Push The Matrix
  glLoadIdentity; // Reset The Matrix
  glOrtho(0, Screen.w, Screen.h, 0, -1, 1); // Select Ortho Mode
  glMatrixMode(GL_MODELVIEW); // Select Modelview Matrix
  glPushMatrix; // Push The Matrix
  glLoadIdentity; // Reset The Matrix
end;

// Set Up A Perspective View

procedure ViewPerspective;
begin
  glMatrixMode(GL_PROJECTION); // Select Projection
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW); // Select Modelview
  glPopMatrix; // Pop The Matrix
end;

// Renders To A Texture

procedure RenderToTexture;
begin
  glViewport(0, 0, 128, 128); // Set Our Viewport (Match Texture Size)
  ProcessHelix; // Render The Helix
  glBindTexture(GL_TEXTURE_2D, TextureID); // Bind To The Blur Texture

  // Copy Our ViewPort To The Blur Texture (From 0,0 To 128,128... No Border)
  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, 0, 0, 128, 128, 0);
  glClearColor(0.0, 0.0, 0.0, 0.5); // Set The Clear Color To Medium Blue
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  // Clear The Screen And Depth Buffer
  glViewport(0, 0, Screen.w, Screen.h); // Set Viewport
end;

// Draw The Blurred Image

procedure DrawBlur(const times: Integer; const inc: TGLFloat);
var
  spost, alpha, alphainc: TGLFloat;
  I: Integer;
begin
  alpha := 0.2;

  glEnable(GL_TEXTURE_2D); // Enable 2D Texture Mapping
  glDisable(GL_DEPTH_TEST); // Disable Depth Testing
  glBlendFunc(GL_SRC_ALPHA, GL_ONE); // Set Blending Mode
  glEnable(GL_BLEND); // Enable Blending
  glBindTexture(GL_TEXTURE_2D, TextureID); // Bind To The Blur Texture
  ViewOrtho; // Switch To An Ortho View

  alphainc := alpha / times; // alphainc=0.2f / Times To Render Blur

  glBegin(GL_QUADS); // Begin Drawing Quads
  // Number Of Times To Render Blur
  for I := 0 to times - 1 do
  begin
    glColor4f(1.0, 1.0, 1.0, alpha); // Set The Alpha Value (Starts At 0.2)
    glTexCoord2f(0 + spost, 1 - spost); // Texture Coordinate	( 0, 1 )
    glVertex2f(0, 0); // First Vertex		(   0,   0 )

    glTexCoord2f(0 + spost, 0 + spost); // Texture Coordinate	( 0, 0 )
    glVertex2f(0, Screen.h); // Second Vertex	(   0, Screen height )

    glTexCoord2f(1 - spost, 0 + spost); // Texture Coordinate	( 1, 0 )
    glVertex2f(Screen.w, Screen.h);
      // Third Vertex  ( Screen width, Screen height )

    glTexCoord2f(1 - spost, 1 - spost); // Texture Coordinate	( 1, 1 )
    glVertex2f(Screen.w, 0); // Fourth Vertex    ( Screen width, 0 )

    spost := spost + inc;
    // Gradually Increase spost (Zooming Closer To Texture Center)
    alpha := alpha - alphainc;
    // Gradually Decrease alpha (Gradually Fading Image Out)
  end;
  glEnd; // Done Drawing Quads

  ViewPerspective; // Switch To A Perspective View

  glEnable(GL_DEPTH_TEST); // Enable Depth Testing
  glDisable(GL_TEXTURE_2D); // Disable 2D Texture Mapping
  glDisable(GL_BLEND); // Disable Blending
  glBindTexture(GL_TEXTURE_2D, 0); // Unbind The Blur Texture
end;

var
  DemoStart, LastTime: LongWord;

  // The main drawing function.

procedure DrawGLScene;
begin
  LastTime := ElapsedTime;
  ElapsedTime := SDL_GetTicks - DemoStart; // Calculate Elapsed Time
  ElapsedTime := (LastTime + ElapsedTime) div 2;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  // Clear The Screen And The Depth Buffer
  glLoadIdentity; // Reset The View
  RenderToTexture; // Render To A Texture
  ProcessHelix; // Draw Our Helix
  DrawBlur(25, 0.02); // Draw The Blur Effect
  angle := ElapsedTime / 5; // Update angle Based On The Clock
  SDL_GL_SwapBuffers;
end;

var
  event: TSDL_Event;
  videoFlags: UInt32;
  videoInfo: PSDL_VideoInfo;
  Done: Boolean = False;

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
  SDL_WM_SetCaption('Jeff Molofee''s OpenGL Code Tutorial 37 : Radial Blur using JEDI-SDL', nil
    );

  //videoflags := videoFlags or SDL_RESIZABLE;    // Enable window resizing

  screen := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags);
  if (screen = nil) then
  begin
    Log.LogError(Format('Unable to create OpenGL screen : %s', [SDL_GetError]
      ),
      'Main');
    TerminateApplication;
  end;

  // Loop, drawing and checking events
  InitGL;
  ReSizeWindow(SCREEN_WIDTH, SCREEN_HEIGHT);

  while not Done do
  begin
    while SDL_PollEvent(@event) = 1 do
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
      end;
    end;
    DrawGlScene;
  end;
  if texture <> nil then
    SDL_FreeSurface(texture);
  SDL_FreeSurface(screen);
  TerminateApplication;
end.

