program JEDISDLOpenGL17;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Ti Leggett <leggett@eecs.tulane.edu>,  are   }
{ Copyright (C) 2001 Ti Leggett.                                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson17.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL17.dpr                }
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
{   June  01 2002 - DL : Initial translation.                      }
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

  NUM_TEXTURES = 2;

var
  // This is our SDL surface
  surface : PSDL_Surface;

  base : TGLuint; // Base Display List For The Font           */
  texture : array[ 0..NUM_TEXTURES - 1 ] of TGLuint; // Storage For Our Font Texture             */

  cnt1 : GLfloat; // Counter Used To Move Text & For Coloring */
  cnt2 : GLfloat; // Counter Used To Move Text & For Coloring */

  //Status indicator
  Status : Boolean = false;

  // function to recover memory form our list of characters

procedure KillFont;
begin
  glDeleteLists( base, 256 ); // Delete All 256 Display Lists
end;

procedure TerminateApplication;
begin
  // Clean up our font list */
  KillFont;
  // Clean up our textures
  glDeleteTextures( NUM_TEXTURES, @texture[ 0 ] );
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;

// Load Bitmaps And Convert To Textures

function LoadGLTextures : Boolean;
var
  // Create storage space for the texture
  TextureImage : array[ 0..NUM_TEXTURES - 1 ] of PSDL_Surface;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage[ 0 ] := SDL_LoadBMP( 'images/font.bmp' );
  TextureImage[ 1 ] := SDL_LoadBMP( 'images/bumps.bmp' );
  if ( TextureImage[ 0 ] <> nil )
    and ( TextureImage[ 1 ] <> nil ) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures( NUM_TEXTURES, @texture[ 0 ] );

    // Load in texture 1
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

    // Generate The Texture
    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage[ 0 ].w,
      TextureImage[ 0 ].h, 0, GL_BGR,
      GL_UNSIGNED_BYTE, TextureImage[ 0 ].pixels );

    // Nearest Filtering
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
      GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
      GL_NEAREST );

    // Load in texture 2
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture( GL_TEXTURE_2D, texture[ 1 ] );

    // Linear Filtering
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
      GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
      GL_LINEAR );

    // Generate The Texture
    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage[ 1 ].w,
      TextureImage[ 1 ].h, 0, GL_BGR,
      GL_UNSIGNED_BYTE, TextureImage[ 1 ].pixels );
  end
  else
  begin
    Log.LogError( Format( 'Could not Load Images : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    TerminateApplication;
  end;

  // Free up any memory we may have used
  if ( TextureImage[ 0 ] <> nil ) then
    SDL_FreeSurface( TextureImage[ 0 ] );
  if ( TextureImage[ 1 ] <> nil ) then
    SDL_FreeSurface( TextureImage[ 1 ] );

  result := Status;
end;

// function to build our font list

procedure BuildFont;
var
  loop : TGLuint; // Loop variable               */
  cx : single; // Holds Our X Character Coord */
  cy : single; // Holds Our Y Character Coord */
begin
  // Creating 256 Display List */
  base := glGenLists( 256 );
  /// Select Our Font Texture */
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

  // Loop Through All 256 Lists */
  for loop := 0 to 255 do
  begin
    {* NOTE:
     *  BMPs are stored with the top-leftmost pixel being the
     * last byte and the bottom-rightmost pixel being the first
     * byte. So an image that is displayed as
     *    1 0
     *    0 0
     * is represented data-wise like
     *    0 0
     *    0 1
     * And because SDL_LoadBMP loads the raw data without
     * translating to how it is thought of when viewed we need
     * to start at the bottom-right corner of the data and work
     * backwards to get everything properly. So the below code has
     * been modified to reflect this. Examine how this is done and
     * how the original tutorial is done to grasp the differences.
     *
     * As a side note BMPs are also stored as BGR instead of RGB
     * and that is why we load the texture using GL_BGR. It's
     * bass-ackwards I know but whattaya gonna do?
     *}

    // X Position Of Current Character
    cx := 1 - ( loop mod 16 ) / 16.0;
    // Y Position Of Current Character */
    cy := 1 - ( loop div 16 ) / 16.0;

    // Start Building A List
    glNewList( base + ( 255 - loop ), GL_COMPILE );
    // Use A Quad For Each Character
    glBegin( GL_QUADS );
    // Texture Coord (Bottom Left)
    glTexCoord2f( cx - 0.0625, cy );
    // Vertex Coord (Bottom Left) */
    glVertex2i( 0, 0 );

    // Texture Coord (Bottom Right)
    glTexCoord2f( cx, cy );
    // Vertex Coord (Bottom Right)
    glVertex2i( 16, 0 );

    // Texture Coord (Top Right)
    glTexCoord2f( cx, cy - 0.0625 );
    // Vertex Coord (Top Right)
    glVertex2i( 16, 16 );

    // Texture Coord (Top Left)
    glTexCoord2f( cx - 0.0625, cy - 0.0625 );
    // Vertex Coord (Top Left)
    glVertex2i( 0, 16 );
    glEnd;

    // Move To The Left Of The Character
    glTranslated( 10, 0, 0 );
    glEndList;
  end;
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
  // Load in the texture
  if ( not LoadGLTextures ) then
  begin
    result := false;
    exit;
  end;

  //Build our font list
  BuildFont;

  // Enable Texture Mapping ( NEW )
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

  // Select The Type Of Blending
  glBlendFunc( GL_SRC_ALPHA, GL_ONE );

  result := true;
end;

// Function to print the string

procedure glDrawText( x, y : GLint; Text : string; sett : integer );
begin
  if ( sett > 1 ) then
    sett := 1;

  // Select our texture
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );

  // Disable depth testing */
  glDisable( GL_DEPTH_TEST );

  // Select The Projection Matrix */
  glMatrixMode( GL_PROJECTION );
  // Store The Projection Matrix */
  glPushMatrix;

  // Reset The Projection Matrix */
  glLoadIdentity;
  // Set Up An Ortho Screen */
  glOrtho( 0, SCREEN_WIDTH, 0, SCREEN_HEIGHT, -1, 1 );

  // Select The Modelview Matrix */
  glMatrixMode( GL_MODELVIEW );
  // Store the Modelview Matrix */
  glPushMatrix;
  // Reset The Modelview Matrix */
  glLoadIdentity;

  // Position The Text (0,0 - Bottom Left) */
  glTranslated( x, y, 0 );

  // Choose The Font Set (0 or 1) */
  glListBase( base - 32 + ( 128 * sett ) );

  // Write The Text To The Screen */
  glCallLists( Length( Text ), GL_BYTE, PChar( Text ) );

  // Select The Projection Matrix
  glMatrixMode( GL_PROJECTION );
  // Restore The Old Projection Matrix
  glPopMatrix;

  // Select the Modelview Matrix
  glMatrixMode( GL_MODELVIEW );
  // Restore the Old Projection Matrix
  glPopMatrix;

  // Re-enable Depth Testing
  glEnable( GL_DEPTH_TEST );
end;

// The main drawing function.

procedure DrawGLScene;
begin
  // Clear The Screen And The Depth Buffer
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  // Reset the view
  glLoadIdentity;

  // Select Our Second Texture */
  glBindTexture( GL_TEXTURE_2D, texture[ 1 ] );
  // Move Into The Screen 5 Units */
  glTranslatef( 0.0, 0.0, -5.0 );
  // Rotate On The Z Axis 45 Degrees (Clockwise) */
  glRotatef( 45.0, 0.0, 0.0, 1.0 );
  // Rotate On The X & Y Axis By cnt1 (Left To Right) */
  glRotatef( cnt1 * 30.0, 1.0, 1.0, 0.0 );

  // Disable Blending Before We Draw In 3D */
  glDisable( GL_BLEND );
  glColor3f( 1.0, 1.0, 1.0 ); // Bright White                       */
  glBegin( GL_QUADS ); // Draw Our First Texture Mapped Quad */
  glTexCoord2d( 0.0, 0.0 ); // First Texture Coord                */
  glVertex2f( -1.0, 1.0 ); // First Vertex                       */
  glTexCoord2d( 1.0, 0.0 ); // Second Texture Coord               */
  glVertex2f( 1.0, 1.0 ); // Second Vertex                      */
  glTexCoord2d( 1.0, 1.0 ); // Third Texture Coord                */
  glVertex2f( 1.0, -1.0 ); // Third Vertex                       */
  glTexCoord2d( 0.0, 1.0 ); // Fourth Texture Coord               */
  glVertex2f( -1.0, -1.0 ); // Fourth Vertex                      */
  glEnd;

  // Rotate On The X & Y Axis By 90 Degrees (Left To Right) */
  glRotatef( 90.0, 1.0, 1.0, 0.0 );
  glBegin( GL_QUADS ); // Draw Our Second Texture Mapped Quad */
  glTexCoord2d( 0.0, 0.0 ); // First Texture Coord                 */
  glVertex2f( -1.0, 1.0 ); // First Vertex                        */
  glTexCoord2d( 1.0, 0.0 ); // Second Texture Coord                */
  glVertex2f( 1.0, 1.0 ); // Second Vertex                       */
  glTexCoord2d( 1.0, 1.0 ); // Third Texture Coord                 */
  glVertex2f( 1.0, -1.0 ); // Third Vertex                        */
  glTexCoord2d( 0.0, 1.0 ); // Fourth Texture Coord                */
  glVertex2f( -1.0, -1.0 ); // Fourth Vertex                       */
  glEnd;

  // Re-enable Blending */
  glEnable( GL_BLEND );
  // Reset the view */
  glLoadIdentity;


  // Pulsing Colors Based On Text Position */
  // Draw GL Text To The Screen */
  glColor3f( 1.0 * cos( cnt1 ),
    1.0 * sin( cnt2 ),
    1.0 - 0.5 * cos( cnt1 + cnt2 ) );
  glDrawText( Round( 280 + 250 * cos( cnt1 ) ),
    Round( 235 + 200 * sin( cnt2 ) ),
    'NeHe', 0 );
  glColor3f( 1.0 * sin( cnt2 ),
    1.0 - 0.5 * cos( cnt1 + cnt2 ),
    1.0 * cos( cnt1 ) );
  glDrawText( Round( 280 + 230 * cos( cnt2 ) ),
    Round( 235 + 200 * sin( cnt1 ) ),
    'OpenGL', 1 );

  // Set Color to White */
  glColor3f( 1.0, 0.0, 0.0 );
  // Draw Text To The Screen */
  glDrawText( Round( 240 + 200 * cos( ( cnt2 + cnt1 ) / 5 ) ), 2,
    'JEDI-SDL', 0 );

  // Set Color To Red */
  glColor3f( 1.0, 1.0, 1.0 );
  // Draw Offset Text To The Screen */
  glDrawText( Round( 242 + 200 * cos( ( cnt2 + cnt1 ) / 5 ) ), 2,
    'JEDI-SDL', 0 );

  cnt1 := cnt1 + 0.01; // Increase The First Counter  */
  cnt2 := cnt2 + 0.0081; // Increase The Second Counter */

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
  SDL_WM_SetCaption( 'Jeff Molofee''s OpenGL Code Tutorial 17 using JEDI-SDL', nil
    );

  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP,
    videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ), 'Main' );
    TerminateApplication;
  end;

  // Enable key repeat
  if ( ( SDL_EnableKeyRepeat( 100, SDL_DEFAULT_REPEAT_INTERVAL ) ) < 0 ) then
  begin
    Log.LogError( Format( 'Setting keyboard repeat failed: %s',
      [ SDL_GetError ] ), 'Main' );
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

