program JEDISDLOpenGL20;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Ti Leggett <leggett@eecs.tulane.edu>,  are   }
{ Copyright (C) 2001 Ti Leggett.                                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson20.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL20.dpr                }
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
{   April   11 2001 - DL : Initial translation.                    }
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
  MAX_TEXTURES = 5;

var
  // This is our SDL surface
  surface: PSDL_Surface;

  masking: Boolean = true; // Masking On/Off
  scene: Boolean; // Which Scene To Draw

  texture: array[0..MAX_TEXTURES - 1] of TGLuInt; // Storage For 5 Textures

  roll: TGLfloat; // Rolling Texture

  //Status indicator
  Status: Boolean = false;

procedure TerminateApplication;
begin
  glDeleteTextures(MAX_TEXTURES, @texture[0]);
  SDL_FreeSurface(surface);
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;

// Load Bitmaps And Convert To Textures

function LoadGLTextures: Boolean;
var
  // Create storage space for the texture
  TextureImage: array[0..MAX_TEXTURES - 1] of PSDL_Surface;
  loop: integer;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage[0] := SDL_LoadBMP('images/Logo.bmp');
  TextureImage[1] := SDL_LoadBMP('images/Mask1.bmp');
  TextureImage[2] := SDL_LoadBMP('images/Image1.bmp');
  TextureImage[3] := SDL_LoadBMP('images/Mask2.bmp');
  TextureImage[4] := SDL_LoadBMP('images/Image2.bmp');
  
  if (TextureImage[0] <> nil)
    and (TextureImage[1] <> nil)
    and (TextureImage[2] <> nil)
    and (TextureImage[3] <> nil)
    and (TextureImage[4] <> nil) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures(MAX_TEXTURES, @texture[0]);

    for loop := 0 to MAX_TEXTURES - 1 do
    begin
      glBindTexture(GL_TEXTURE_2D, texture[loop]);
      
      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage[loop].w,
				  TextureImage[loop].h, 0, GL_BGR,
				  GL_UNSIGNED_BYTE,
				  TextureImage[loop].pixels );

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end;
  end
  else
  begin
    Log.LogError(Format('Could not Load Image : %s', [SDL_GetError]),
      'LoadGLTextures');
    TerminateApplication;
  end;

  // Free up any memory we may have used
  for loop := 0 to MAX_TEXTURES - 1 do
  begin
    if TextureImage[loop] <> nil then
      SDL_FreeSurface(TextureImage[loop]);
  end;

  result := Status;
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

    SDLK_SPACE:
      begin
        scene := not scene;
      end;

    SDLK_m:
      begin
        masking := not masking;
      end;

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
// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL: Boolean;
begin
  // Load in the texture
  if (not LoadGLTextures) then
  begin
    result := false;
    exit;
  end;
  // Enable Texture Mapping ( NEW )
  glEnable(GL_TEXTURE_2D);
  // Enable smooth shading
  glShadeModel(GL_SMOOTH);
  // Set the background black
  glClearColor(0.0, 0.0, 0.0, 0.0);
  // Depth buffer setup
  glClearDepth(1.0);
  // Enables Depth Testing
  glEnable(GL_DEPTH_TEST);

  result := true;
end;
// The main drawing function.

procedure DrawGLScene;
begin
  // Clear The Screen And The Depth Buffer
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  // Reset the view
  glLoadIdentity;
  glTranslatef(0.0, 0.0, -2.0); // Move Into The Screen 5 Units

  glBindTexture(GL_TEXTURE_2D, texture[0]); // Select Our Logo Texture
  glBegin(GL_QUADS); // Start Drawing A Textured Quad
    glTexCoord2f(0.0, -roll + 3.0);
    glVertex3f(-1.1, -1.1, 0.0); // Bottom Left
    glTexCoord2f(3.0, -roll + 3.0);
    glVertex3f(1.1, -1.1, 0.0); // Bottom Right
    glTexCoord2f(3.0, -roll + 0.0);
    glVertex3f(1.1, 1.1, 0.0); // Top Right
    glTexCoord2f(0.0, -roll + 0.0);
    glVertex3f(-1.1, 1.1, 0.0); // Top Left
  glEnd(); // Done Drawing The Quad}


  glEnable(GL_BLEND); // Enable Blending
  glDisable(GL_DEPTH_TEST); // Disable Depth Testing

  if (masking) then // Is Masking Enabled?
  begin
    glBlendFunc(GL_DST_COLOR, GL_ZERO); // Blend Screen Color With Zero (Black)
  end;

  if (scene) then // Are We Drawing The Second Scene?
  begin
    glTranslatef(0.0, 0.0, -1.0); // Translate Into The Screen One Unit
    glRotatef(roll * 360, 0.0, 0.0, 1.0); // Rotate On The Z Axis 360 Degrees.
    if (masking) then // Is Masking On?
    begin
      glBindTexture(GL_TEXTURE_2D, texture[3]); // Select The Second Mask Texture
      glBegin(GL_QUADS); // Start Drawing A Textured Quad
      glTexCoord2f(0.0, 0.0);
      glVertex3f(-1.1, -1.1, 0.0); // Bottom Left
      glTexCoord2f(1.0, 0.0);
      glVertex3f(1.1, -1.1, 0.0); // Bottom Right
      glTexCoord2f(1.0, 1.0);
      glVertex3f(1.1, 1.1, 0.0); // Top Right
      glTexCoord2f(0.0, 1.0);
      glVertex3f(-1.1, 1.1, 0.0); // Top Left
      glEnd(); // Done Drawing The Quad
    end;

    glBlendFunc(GL_ONE, GL_ONE); // Copy Image 2 Color To The Screen
    glBindTexture(GL_TEXTURE_2D, texture[4]); // Select The Second Image Texture
    glBegin(GL_QUADS); // Start Drawing A Textured Quad
    glTexCoord2f(0.0, 0.0);
    glVertex3f(-1.1, -1.1, 0.0); // Bottom Left
    glTexCoord2f(1.0, 0.0);
    glVertex3f(1.1, -1.1, 0.0); // Bottom Right
    glTexCoord2f(1.0, 1.0);
    glVertex3f(1.1, 1.1, 0.0); // Top Right
    glTexCoord2f(0.0, 1.0);
    glVertex3f(-1.1, 1.1, 0.0); // Top Left
    glEnd(); // Done Drawing The Quad
  end
  else // Otherwise
  begin
    if (masking) then // Is Masking On?
    begin
      glBindTexture(GL_TEXTURE_2D, texture[1]); // Select The First Mask Texture
      glBegin(GL_QUADS); // Start Drawing A Textured Quad
      glTexCoord2f(roll + 0.0, 0.0);
      glVertex3f(-1.1, -1.1, 0.0); // Bottom Left
      glTexCoord2f(roll + 4.0, 0.0);
      glVertex3f(1.1, -1.1, 0.0); // Bottom Right
      glTexCoord2f(roll + 4.0, 4.0);
      glVertex3f(1.1, 1.1, 0.0); // Top Right
      glTexCoord2f(roll + 0.0, 4.0);
      glVertex3f(-1.1, 1.1, 0.0); // Top Left
      glEnd(); // Done Drawing The Quad
    end;

    glBlendFunc(GL_ONE, GL_ONE); // Copy Image 1 Color To The Screen
    glBindTexture(GL_TEXTURE_2D, texture[2]); // Select The First Image Texture
    glBegin(GL_QUADS); // Start Drawing A Textured Quad
    glTexCoord2f(roll + 0.0, 0.0);
    glVertex3f(-1.1, -1.1, 0.0); // Bottom Left
    glTexCoord2f(roll + 4.0, 0.0);
    glVertex3f(1.1, -1.1, 0.0); // Bottom Right
    glTexCoord2f(roll + 4.0, 4.0);
    glVertex3f(1.1, 1.1, 0.0); // Top Right
    glTexCoord2f(roll + 0.0, 4.0);
    glVertex3f(-1.1, 1.1, 0.0); // Top Left
    glEnd(); // Done Drawing The Quad
  end;

  glEnable(GL_DEPTH_TEST); // Enable Depth Testing
  glDisable(GL_BLEND); // Disable Blending

  roll := roll + 0.002; // Increase Our Texture Roll Variable
  if (roll > 1.0) then // Is Roll Greater Than One
  begin
    roll := roll - 1.0; // Subtract 1 From Roll
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
  SDL_WM_SetCaption('Jeff Molofee''s OpenGL Code Tutorial 20 using JEDI-SDL', nil
    );
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


