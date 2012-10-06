program JEDISDL_MDLViewer;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of Quake 2 Model viewer                         }
{                                                                  }
{ Portions created by Ilkka Tuomioja,  are                         }
{ Copyright (C) 1999 Ilkka Tuomioja.                               }
{ All Rights Reserved.                                             }
{                                                                  }
{                                                                  }
{ The original Pascal code is : JEDISDL_MDLViewer.dpr              }
{ The initial developer of the Pascal code is :                    }
{ Ilkka Tuomioja < >                                               }
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
{   September 05 2001 - DL : Initial translation ( Texture not     }
{                            working.                              }
{   September 19 2001 - DL : Textures working and weapon added.    }
{                            They need to be powers of 2 ( duh ).  }
{                                                                  }
{******************************************************************}

uses
  OpenGL12,
  SysUtils,
  Logger,
  QMDL,
  SDL;
  
const
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;
  
var
  // This is our SDL surface
  surface : PSDL_Surface;
  
  Wireframe : Boolean = False;

  QModel : TQuakeModel; // Quake model
  QModelFrame: Integer; //Current Frame for Quake Model
  WModel : TQuakeModel; // Weapon model
  WModelFrame: Integer; //Current Frame for Weapon Model

  FModelDirectory : string = 'Models/blade/'; // Path of directory where Models and skins exist;

  Angle: Integer;

procedure TerminateApplication;
begin
  SDL_QUIT;
  // Free outr Models
  WModel.Free;
  QModel.Free;
  
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

    SDLK_W:
    begin
      if Wireframe then // Enable/Disable wireframe draw
      begin
        glPolygonMode(GL_FRONT, GL_FILL);
        glPolygonMode(GL_BACK, GL_FILL);
      end
      else
      begin
        glPolygonMode(GL_FRONT, GL_LINE);
        glPolygonMode(GL_BACK, GL_LINE);
      end;
      WireFrame := not (WireFrame);
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

  if QModel <> nil then
    QModel.Free;

  QModel := TQuakeModel.Create;
  // Load Bitmaps And Convert To Textures
  QModel.Skin := FModelDirectory + 'skin.bmp';
  // Load Character Model
  try
    QModel.Model := FModelDirectory + 'tris.md2';
  except on E: Exception do
    Log.LogWarning( Format( 'Exception : %s', [E.Message] ), 'InitGL' );
  end;


  if WModel <> nil then
    WModel.Free;

  wModel := TQuakeModel.Create;
  // Load Bitmaps And Convert To Textures
  WModel.Skin := FModelDirectory + 'weapon.bmp';
  // Load Weapon Model
  try
    WModel.Model := FModelDirectory + 'weapon.md2';
  except on E: Exception do
    Log.LogWarning( Format( 'Exception : %s', [E.Message] ), 'InitGL' );
  end;

  if Wireframe then
  begin
    glPolygonMode(GL_FRONT, GL_LINE);
    glPolygonMode(GL_BACK, GL_LINE);
  end;

  result := true;
end;


// The main drawing function.
procedure DrawGLScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);     // activate the transformation matrix
  glLoadIdentity;                 // set it to initial state

  Inc(Angle);
  If Angle >= 360 then
    Dec(Angle, 360);

  glTranslatef(0, 0, -75);

   // Just for better view...
  glRotatef(-90, 1, 0, 0);
  glRotatef(-90, 0, 0, 1);

  QModel.Show(QModelFrame);

  WModel.Show(WModelFrame);

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
  //keystate: PKeyStateArr;
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
  {if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else}
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
  SDL_WM_SetCaption( 'Ilkka Tuomioja''s Quake II Model Viewer using JEDI-SDL', nil
    );

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

  // If Model Directory passed in as paramter use it, else use default. 
  if ParamCount > 0 then
  begin
    FModelDirectory := ParamStr( 1 );
    if ( FModelDirectory[Length(FModelDirectory)] <> '/'  ) then
    begin
      FModelDirectory := FModelDirectory + '/';
    end
  end;

  // Loop, drawing and checking events
  InitGL;
  ResizeWindow(SCREEN_WIDTH, SCREEN_HEIGHT);


  Done := False;
  while (not Done) do
  begin
    SDL_Delay( 250 );

    If QModelFrame = QModel.NumFrames then
    QModelFrame := 0;

    If WModelFrame = WModel.NumFrames then
    WModelFrame := 0;
    
    DrawGLScene;

    // This could go in a separate function */
    while (SDL_PollEvent(@event) = 1) do
    begin
      case event.type_ of
        SDL_QUITEV:
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

    {keystate := PKeyStateArr(SDL_GetKeyState(nil));
    if (keystate[SDLK_SPACE] <> 0 ) then
    begin
      Done := true;
    end;}

    Inc(QModelFrame); // move on to the Next Quake Model Frame
    Inc(WModelFrame); // move on to the Next Weapon Model Frame
  end;
  
  TerminateApplication;
end.

