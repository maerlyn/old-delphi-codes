program JEDISDLOpenGLSkyBox;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Port of the SkyBox Example by Jan Horn                     }
{                                                                  }
{ Portions created by Jan Horn <jhorn@global.co.za>,  are          }
{ Copyright (C) 2001 Jan Horn.                                     }
{ All Rights Reserved.                                             }
{                                                                  }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGLSkyBox.dpr            }
{ The initial developer of the Pascal code is :                    }
{ Jan Horn <jhorn@global.co.za>                                    }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{  Dominique Louis <Dominique@SavageSoftware.com.au>               }
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
{  Shows how to use OpenGL with the SDL libraries and how to       }
{  do SkyBoxs !                                                    }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{  Thanks to Paul Bourke for a excellent explanation on how the    }
{   marching cubes algorithm works. The lookup tables are also     }
{  from his site. If you really want to understand metaballs, go   }
{  read his article. -                                             }
{  http://www.swin.edu.au/astronomy/pbourke/modelling/             }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{  October 03 2001 - JH : Initial demo.                            }
{   August 03 2001 - DL : Port to JEDI-SDL.                        }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  OpenGL12,
  Logger,
  SDL;

const
  WND_TITLE = 'SkyBox demo by Jan Horn, JEDI-SDL version by Dominique Louis';
  FPS_TIMER = 1;                     // Timer to calculate FPS
  FPS_INTERVAL = 1000;               // Calculate FPS every 1000 ms
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 32;

var
  // This is our SDL surface
  surface : PSDL_Surface;

  ElapsedTime : LongWord;             // Elapsed time between frames

  // Textures
  FrontTex : TglUint;
  BackTex : TglUint;
  TopTex : TglUint;
  BottomTex : TglUint;
  LeftTex : TglUint;
  RightTex : TglUint;

  // User vaiables
  xSpeed, ySpeed : TglFloat;
  xAngle, yAngle : TglFloat;

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
  end;
end;

// Based on Jan Horn's Load Texture Routine
function LoadTexture(FileName: string; var TextureID : TGLuInt ): Boolean;
var
  // Create storage space for the texture
  TextureImage: PSDL_Surface;
begin
  TextureImage := SDL_LoadBMP(PChar( FileName ));
  if (TextureImage <> nil) then
  begin
    Result := True;
    glGenTextures(1, @TextureID);

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glBindTexture(GL_TEXTURE_2D, TextureID);

    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}

    // Generate The Texture
    {glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
                  TextureImage.h, 0, GL_BGR,
                  GL_UNSIGNED_BYTE, TextureImage.pixels );}
    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage.w, TextureImage.h, GL_BGR, GL_UNSIGNED_BYTE, TextureImage.pixels);
  end
  else
  begin
    Result := False;
    Log.LogError( Format( 'Unable to Load Texture : %s', [SDL_GetError] ),
      'LoadTexture' );
  end;
end;

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.
function InitGL : Boolean;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
  LoadTexture('images/cliffFront.bmp', FrontTex);    // Load the Texture
  LoadTexture('images/cliffBack.bmp', BackTex );    // Load the Texture
  LoadTexture('images/cliffLeft.bmp', LeftTex );    // Load the Texture
  LoadTexture('images/cliffRight.bmp', RightTex);    // Load the Texture
  LoadTexture('images/cliffBottom.bmp', TopTex );    // Load the Texture
  LoadTexture('images/cliffTop.bmp', BottomTex);    // Load the Texture

  xAngle :=-10;
  ySpeed :=0.1;

  result := true;
end;

// The main drawing function.
procedure DrawGLScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity();                                       // Reset The View

  //glTranslatef(0.0,0.0,-0.5);

  {--- There are two types of movement. Select the type you want to use ---}
  { For movement that requires used input use ... }
  glRotatef(xAngle, 1, 0, 0);
  glRotatef(yAngle, 0, 1, 0);

  glBindTexture(GL_TEXTURE_2D, FrontTex);  // Bind the Texture to the object
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.01, -1.01,  1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.005, -1.005,  1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.005,  1.005,  1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.005,  1.005,  1.0);
  glend;

  glBindTexture(GL_TEXTURE_2D, BackTex);
  glBegin(GL_QUADS);
    glNormal3f( 0.0, 0.0,-1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.005, -1.005, -1.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.005,  1.005, -1.0);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.005,  1.005, -1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.005, -1.005, -1.0);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, TopTex);
  glBegin(GL_QUADS);
    glNormal3f( 0.0, 1.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.005,  1.0, -1.005);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.005,  1.0,  1.005);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.005,  1.0,  1.005);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.005,  1.0, -1.005);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, BottomTex);
  glBegin(GL_QUADS);
    glNormal3f( 0.0,-1.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.005, -1.0, -1.005);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.005, -1.0, -1.005);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.005, -1.0,  1.005);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.005, -1.0,  1.005);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, LeftTex);
  glBegin(GL_QUADS);
    glNormal3f( 1.0, 0.0, 0.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.005, -1.005);
    glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.005, -1.005);
    glTexCoord2f(0.0, 1.0); glVertex3f( 1.0,  1.005,  1.005);
    glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.005,  1.005);
  glEnd;

  glBindTexture(GL_TEXTURE_2D, RightTex);
  glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.005, -1.005);
    glTexCoord2f(1.0, 0.0); glVertex3f(-1.0, -1.005,  1.005);
    glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.005,  1.005);
    glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.005, -1.005);

  glEnd();

  xAngle :=xAngle + xSpeed;
  yAngle :=yAngle + ySpeed;

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  DemoStart, LastTime : LongWord;

  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
  keystate: PKeyStateArr;
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
  SDL_WM_SetCaption( WND_TITLE , nil);

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
  
  DemoStart := SDL_GetTicks;            // Get Time when demo started
  Done := False;
  while (not Done) do
  begin
    LastTime := ElapsedTime;
    ElapsedTime := SDL_GetTicks - DemoStart;     // Calculate Elapsed Time
    ElapsedTime := (LastTime + ElapsedTime) DIV 2; // Average it out for smoother movement
    
    DrawGLScene;
    
    // This could go in a separate function */
    while (SDL_PollEvent(@event) = 1) do
    begin
      case event.type_ of
        SDL_QUITEV :
        begin
          Done := true;
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
        
        SDL_KEYDOWN :
        begin
          // handle key presses
          HandleKeyPress( @event.key.keysym );
        end;
      end;
    end;

    //Handle real-time key events
    keystate := PKeyStateArr(SDL_GetKeyState(nil));
    if (keystate[SDLK_UP] <> 0) then
    begin
      xspeed := xspeed - 0.002;
    end;

    if (keystate[SDLK_DOWN] <> 0) then
    begin
      xspeed := xspeed + 0.002;
    end;

    if (keystate[SDLK_LEFT] <> 0) then
    begin
      yspeed := yspeed - 0.002;
    end;

    if (keystate[SDLK_RIGHT] <> 0) then
    begin
      yspeed := yspeed + 0.002;
    end;
  end;
  TerminateApplication;
end.

