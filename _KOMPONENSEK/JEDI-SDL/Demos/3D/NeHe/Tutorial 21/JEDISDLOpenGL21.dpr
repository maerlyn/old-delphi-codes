program JEDISDLOpenGL21;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Ti Leggett <leggett@eecs.tulane.edu>,  are   }
{ Copyright (C) 2001 Ti Leggett.                                   }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : lesson21.c                              }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGL21.dpr                }
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
{   June   06 2002 - DL : Initial translation.                     }
{                                                                  }
{******************************************************************}
uses
  OpenGL12,
  SysUtils,
  Logger,
  SDL,
  SDL_Mixer;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

  MAX_TEXTURES = 2;

  steps : array[ 0..5 ] of byte = ( 1, 2, 4, 5, 10, 20 );

type
  gameobject = record
    fx, fy : integer;
    x, y : integer;
    spin : single;
  end;

var
  // This is our SDL surface
  surface : PSDL_Surface;

  // Our audio chunk
  chunk : PMix_Chunk;
  music : PMix_Music;

  player : gameobject;
  enemies : array[ 0..8 ] of gameobject;
  hourglass : gameobject;

  base : TGLuint; // Base Display List For The Font
  texture : array[ 0..MAX_TEXTURES - 1 ] of TGLuInt; // Storage For 5 Textures

  vline : array[ 0..10, 0..9 ] of boolean;
  hline : array[ 0..9, 0..10 ] of boolean;

  //Status indicator
  Status : Boolean = false;

  anti : boolean = true;
  filled : boolean;
  gameover : boolean;

  adjust : integer = 3;
  lives : integer = 5;
  level : integer = 1;
  level2 : integer = 1;
  stage : integer = 1;

procedure TerminateApplication;
begin
  // Clean up our font list
  glDeleteLists( base, 256 );

  // Clean up our textures
  glDeleteTextures( MAX_TEXTURES, @texture[ 0 ] );

  // Stop playing the music
  Mix_HaltMusic;

  // Free up the memory for the music
  Mix_FreeMusic( music );

  // Free up any memory for the sfx
  Mix_FreeChunk( chunk );

  // Close our audio device
  Mix_CloseAudio;

  // Close up the sound sub system
  SDL_QuitSubSystem( SDL_INIT_AUDIO );

  SDL_FreeSurface( surface );

  // clean up the window
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;

// Load Bitmaps And Convert To Textures

function LoadGLTextures : Boolean;
var
  // Create storage space for the texture
  TextureImage : array[ 0..MAX_TEXTURES - 1 ] of PSDL_Surface;
  loop : integer;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage[ 0 ] := SDL_LoadBMP( 'images/font.bmp' );
  TextureImage[ 1 ] := SDL_LoadBMP( 'images/image.bmp' );

  if ( TextureImage[ 0 ] <> nil )
    and ( TextureImage[ 1 ] <> nil ) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures( MAX_TEXTURES, @texture[ 0 ] );

    for loop := 0 to MAX_TEXTURES - 1 do
    begin
      glBindTexture( GL_TEXTURE_2D, texture[ loop ] );

      glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage[ loop ].w,
        TextureImage[ loop ].h, 0, GL_BGR,
        GL_UNSIGNED_BYTE,
        TextureImage[ loop ].pixels );

      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    end;
  end
  else
  begin
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    TerminateApplication;
  end;

  // Free up any memory we may have used
  for loop := 0 to MAX_TEXTURES - 1 do
  begin
    if TextureImage[ loop ] <> nil then
      SDL_FreeSurface( TextureImage[ loop ] );
  end;

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
    glVertex2i( 0, 16 );

    // Texture Coord (Bottom Right)
    glTexCoord2f( cx, cy );
    // Vertex Coord (Bottom Right)
    glVertex2i( 16, 16 );

    // Texture Coord (Top Right)
    glTexCoord2f( cx, cy - 0.0625 );
    // Vertex Coord (Top Right)
    glVertex2i( 16, 0 );

    // Texture Coord (Top Left)
    glTexCoord2f( cx - 0.0625, cy - 0.0625 );
    // Vertex Coord (Top Left)
    glVertex2i( 0, 0 );
    glEnd;

    // Move To The Left Of The Character
    glTranslated( 15, 0, 0 );
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

  // Set our ortho perspective/view
  glOrtho( 0.0, width, height, 0.0, -1.0, 1.0 );

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

    SDLK_SPACE :
      begin
        if ( gameover ) then
        begin
          gameover := FALSE; // gameover Becomes FALSE
          filled := TRUE; // filled Becomes TRUE
          level := 1; // Starting Level Is Set Back To One
          level2 := 1; // Displayed Level Is Also Set To One
          stage := 1; // Game Stage Is Set To Zero
          lives := 5; // Lives Is Set To Five
        end;
      end;

    SDLK_a :
      begin
        anti := not anti;
      end;

    SDLK_RIGHT :
      begin
        if ( ( player.x < 10 )
          and ( player.fx = player.x * 60 )
          and ( player.fy = player.y * 40 ) ) then
        begin
          // Mark The Current Horizontal Border As Filled
          hline[ player.x ][ player.y ] := true;
          // Move The Player Right
          player.x := player.x + 1;
        end;
      end;

    SDLK_LEFT :
      begin
        if ( ( player.x > 0 )
          and ( player.fx = player.x * 60 )
          and ( player.fy = player.y * 40 ) ) then
        begin
          // Move The Player Left
          player.x := player.x - 1;
          // Mark The Current Horizontal Border As Filled
          hline[ player.x ][ player.y ] := true;
        end;

      end;

    SDLK_UP :
      begin
        if ( ( player.y > 0 )
          and ( player.fx = player.x * 60 )
          and ( player.fy = player.y * 40 ) ) then
        begin
          // Move The Player Up
          player.y := player.y - 1;
          // Mark The Current Verticle Border As Filled
          vline[ player.x ][ player.y ] := true;
        end;
      end;

    SDLK_DOWN :
      begin
        if ( ( player.y < 10 )
          and ( player.fx = player.x * 60 )
          and ( player.fy = player.y * 40 ) ) then
        begin
          // Mark The Current Verticle Border As Filled
          vline[ player.x ][ player.y ] := true;
          // Move The Player Down
          player.y := player.y + 1;
        end;
      end;

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

  // Build The Font
  BuildFont;

  // Enable smooth shading
  glShadeModel( GL_SMOOTH );

  // Set the background black
  glClearColor( 0.0, 0.0, 0.0, 0.5 );

  // Depth buffer setup
  glClearDepth( 1.0 );

  // Set Line Antialiasing
  glHint( GL_LINE_SMOOTH_HINT, GL_NICEST );
  // Enable Blending
  glEnable( GL_BLEND );
  // Type Of Blending To Use
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  result := true;
end;

// Function to draw the string on the screen
procedure glDrawText( x, y : GLint; Text : string; fontset : integer );
begin
   // Did User Choose An Invalid Character Set? */
  if ( fontset > 1 ) then
    fontset := 1;
  // Enable Texture Mapping */
  glEnable( GL_TEXTURE_2D );
  // Select our texture */
  glBindTexture( GL_TEXTURE_2D, texture[ 0 ] );
  // Disable depth testing */
  glDisable( GL_DEPTH_TEST );
  // Reset The Modelview Matrix */
  glLoadIdentity;
  // Position The Text (0,0 - Bottom Left) */
  glTranslated( x, y, 0 );
  // Choose The Font Set (0 or 1) */
  glListBase( base - 32 + ( 128 * fontset ) );
  // If Set 0 Is Being Used Enlarge Font */
  if ( fontset = 0 ) then
    // Enlarge Font Width And Height */
    glScalef( 1.5, 2.0, 1.0 );
  // Write The Text To The Screen */
  glCallLists( Length( Text ), GL_BYTE, PChar( Text ) );
  // Disable Texture Mapping */
  glDisable( GL_TEXTURE_2D );
  // Re-enable Depth Testing */
  glEnable( GL_DEPTH_TEST );
end;

// The main drawing function.

procedure DrawGLScene;
var
  loop1, loop2 : integer;
begin
  // Clear The Screen And The Depth Buffer
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  // Reset the view
  glLoadIdentity;
  // Set Color To Purple
  glColor3f( 1.0, 0.5, 1.0 );
  // Write GRID CRAZY On The Screen
  glDrawText( 207, 24, 'GRID CRAZY', 0 );
  // Set Color To Yellow
  glColor3f( 1.0, 1.0, 0.0 );
  // Write Actual Level Stats
  glDrawText( 20, 20, 'Level:' + IntToStr( level2 ), 1 );
  // Write Stage Stats
  glDrawText( 20, 40, 'Stage:' + IntToStr( stage ), 1 );

  // Is The Game Over?
  if ( gameover ) then
  begin
    // Pick A Random Color
    glColor3ub( random( 255 ), random( 255 ), random( 255 ) );
    // Write GAME OVER To The Screen
    glDrawText( 472, 20, 'GAME OVER', 1 );
    // Write PRESS SPACE To The Screen
    glDrawText( 456, 40, 'PRESS SPACE', 1 );
  end;

  // Loop Through Lives Minus Current Life
  for loop1 := 0 to lives - 2 do
  begin
    // Reset The View
    glLoadIdentity( );
    // Move To The Right Of Our Title Text
    glTranslatef( 490 + ( loop1 * 40.0 ), 40.0, 0.0 );
    // Rotate Counter Clockwise
    glRotatef( -player.spin, 0.0, 0.0, 1.0 );
    // Set Player Color To Light Green
    glColor3f( 0.0, 1.0, 0.0 );

    // Start Drawing Our Player Using Lines
    glBegin( GL_LINES );
    glVertex2d( -5, -5 ); // Top Left Of Player
    glVertex2d( 5, 5 ); // Bottom Right Of Player
    glVertex2d( 5, -5 ); // Top Right Of Player
    glVertex2d( -5, 5 ); // Bottom Left Of Player
    glEnd( );

    // Rotate Counter Clockwise
    glRotatef( -player.spin * 0.5, 0.0, 0.0, 1.0 );
    // Set Player Color To Dark Green
    glColor3f( 0.0, 0.75, 0.0 );

    // Start Drawing Our Player Using Lines
    glBegin( GL_LINES );
    glVertex2d( -7, 0 ); // Left Center Of Player
    glVertex2d( 7, 0 ); // Right Center Of Player
    glVertex2d( 0, -7 ); // Top Center Of Player
    glVertex2d( 0, 7 ); // Bottom Center Of Player
    glEnd( );
  end;

  // Set Filled To True Before Testing
  filled := TRUE;
  // Set Line Width For Cells To 2.0
  glLineWidth( 2.0 );
  // Disable Antialiasing
  glDisable( GL_LINE_SMOOTH );
  // Reset The Current Modelview Matrix
  glLoadIdentity( );

  // Loop From Left To Right
  for loop1 := 0 to 10 do
  begin
    // Loop From Top To Bottom
    for loop2 := 0 to 10 do
    begin
      // Set Line Color To Blue
      glColor3f( 0.0, 0.5, 1.0 );

      // Has The Horizontal Line Been Traced
      if ( hline[ loop1 ][ loop2 ] ) then
        glColor3f( 1.0, 1.0, 1.0 );

      // Dont Draw To Far Right
      if ( loop1 < 10 ) then
      begin
        // If A Horizontal Line Isn't Filled
        if ( not hline[ loop1 ][ loop2 ] ) then
          filled := FALSE;

        // Start Drawing Horizontal Cell Borders
        glBegin( GL_LINES );
        // Left Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        // Right Side Of Horizontal Line
        glVertex2d( 80 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        glEnd( );
      end;

      // Set Line Color To Blue
      glColor3f( 0.0, 0.5, 1.0 );

      // Has The Horizontal Line Been Traced
      if ( vline[ loop1 ][ loop2 ] ) then
        // If So, Set Line Color To White
        glColor3f( 1.0, 1.0, 1.0 );

      // Dont Draw To Far Down
      if ( loop2 < 10 ) then
      begin
        // If A Verticle Line Isn't Filled
        if ( not vline[ loop1 ][ loop2 ] ) then
          filled := FALSE;

        // Start Drawing Verticle Cell Borders
        glBegin( GL_LINES );
        // Left Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          70 + ( loop2 * 40 ) );
        // Right Side Of Horizontal Line
        glVertex2d( 20 + ( loop1 * 60 ),
          110 + ( loop2 * 40 ) );
        glEnd( );
      end;

      // Enable Texture Mapping
      glEnable( GL_TEXTURE_2D );
      // Bright White Color
      glColor3f( 1.0, 1.0, 1.0 );
      // Select The Tile Image
      glBindTexture( GL_TEXTURE_2D, texture[ 1 ] );

      // If In Bounds, Fill In Traced Boxes
      if ( ( loop1 < 10 ) and ( loop2 < 10 ) ) then
      begin
        // Are All Sides Of The Box Traced?
        if ( hline[ loop1 ][ loop2 ] and
          hline[ loop1 ][ loop2 + 1 ] and
          vline[ loop1 ][ loop2 ] and
          vline[ loop1 + 1 ][ loop2 ] ) then
        begin
          // Draw A Textured Quad
          glBegin( GL_QUADS );
          // Top Right
          glTexCoord2f( ( loop1 / 10.0 ) + 0.1, 1.0 - ( ( loop2 / 10.0 ) ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 59,
            70 + loop2 * 40 + 1 );
          // Top Left
          glTexCoord2f( ( loop1 / 10.0 ), 1.0 - ( ( loop2 / 10.0 ) ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 1,
            70 + loop2 * 40 + 1 );
          // Bottom Left
          glTexCoord2f( ( loop1 / 10.0 ), 1.0 - ( ( loop2 / 10.0 ) + 0.1 ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 1,
            ( 70 + loop2 * 40 ) + 39 );
          // Bottom Right
          glTexCoord2f( ( loop1 / 10.0 ) + 0.1, 1.0 - ( ( loop2 / 10.0 ) + 0.1 ) );
          glVertex2d( 20 + ( loop1 * 60 ) + 59,
            ( 70 + loop2 * 40 ) + 39 );
          glEnd( );
        end;
      end;

      // Disable Texture Mapping
      glDisable( GL_TEXTURE_2D );
    end;
  end;

  // Set The Line Width To 1.0
  glLineWidth( 1.0 );

  // Is Anti TRUE?
  if ( anti ) then
    glEnable( GL_LINE_SMOOTH );

  // If fx=1 Draw The Hourglass
  if ( hourglass.fx = 1 ) then
  begin
    // Reset The Modelview Matrix
    glLoadIdentity( );
    // Move To The Fine Hourglass Position
    glTranslatef( 20.0 + ( hourglass.x * 60 ),
      70.0 + ( hourglass.y * 40 ), 0.0 );
    // Rotate Clockwise
    glRotatef( hourglass.spin, 0.0, 0.0, 1.0 );
    // Set Hourglass Color To Random Color
    glColor3ub( random( 255 ), random( 255 ), random( 255 ) );

    // Start Drawing Our Hourglass Using Lines
    glBegin( GL_LINES );
    // Top Left Of Hourglass
    glVertex2d( -5, -5 );
    // Bottom Right Of Hourglass
    glVertex2d( 5, 5 );
    // Top Right Of Hourglass
    glVertex2d( 5, -5 );
    // Bottom Left Of Hourglass
    glVertex2d( -5, 5 );
    // Bottom Left Of Hourglass
    glVertex2d( -5, 5 );
    // Bottom Right Of Hourglass
    glVertex2d( 5, 5 );
    // Top Left Of Hourglass
    glVertex2d( -5, -5 );
    // Top Right Of Hourglass
    glVertex2d( 5, -5 );
    glEnd( );
  end;

  // Reset The Modelview Matrix
  glLoadIdentity( );
  // Move To The Fine Player Position
  glTranslatef( player.fx + 20.0, player.fy + 70.0, 0.0 );
  // Rotate Clockwise
  glRotatef( player.spin, 0.0, 0.0, 1.0 );
  // Set Player Color To Light Green
  glColor3f( 0.0, 1.0, 0.0 );

  // Start Drawing Our Player Using Lines
  glBegin( GL_LINES );
  // Top Left Of Player
  glVertex2d( -5, -5 );
  // Bottom Right Of Player
  glVertex2d( 5, 5 );
  // Top Right Of Player
  glVertex2d( 5, -5 );
  // Bottom Left Of Player
  glVertex2d( -5, 5 );
  glEnd( );

  // Rotate Clockwise
  glRotatef( player.spin * 0.5, 0.0, 0.0, 1.0 );
  // Set Player Color To Dark Green
  glColor3f( 0.0, 0.75, 0.0 );
  // Start Drawing Our Player Using Lines
  glBegin( GL_LINES );
  // Left Center Of Player
  glVertex2d( -7, 0 );
  // Right Center Of Player
  glVertex2d( 7, 0 );
  // Top Center Of Player
  glVertex2d( 0, -7 );
  // Bottom Center Of Player
  glVertex2d( 0, 7 );
  glEnd( );

  // Loop To Draw Enemies
  for loop1 := 0 to ( stage * level ) - 1 do
  begin
    // Reset The Modelview Matrix
    glLoadIdentity( );
    glTranslatef( enemies[ loop1 ].fx + 20.0,
      enemies[ loop1 ].fy + 70.0, 0.0 );
    // Make Enemy Body Pink
    glColor3f( 1.0, 0.5, 0.5 );

    // Start Drawing Enemy
    glBegin( GL_LINES );
    // Top Point Of Body
    glVertex2d( 0, -7 );
    // Left Point Of Body
    glVertex2d( -7, 0 );
    // Left Point Of Body
    glVertex2d( -7, 0 );
    // Bottom Point Of Body
    glVertex2d( 0, 7 );
    // Bottom Point Of Body
    glVertex2d( 0, 7 );
    // Right Point Of Body
    glVertex2d( 7, 0 );
    // Right Point Of Body
    glVertex2d( 7, 0 );
    // Top Point Of Body
    glVertex2d( 0, -7 );
    glEnd( );

    // Rotate The Enemy Blade
    glRotatef( enemies[ loop1 ].spin, 0.0, 0.0, 1.0 );
    // Make Enemy Blade Red
    glColor3f( 1.0, 0.0, 0.0 );

    // Start Drawing Enemy Blade
    glBegin( GL_LINES );
    // Top Left Of Enemy
    glVertex2d( -7, -7 );
    // Bottom Right Of Enemy
    glVertex2d( 7, 7 );
    // Bottom Left Of Enemy
    glVertex2d( -7, 7 );
    // Top Right Of Enemy
    glVertex2d( 7, -7 );
    glEnd( );
  end;

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

procedure PlaySound( sound : string; Loop : integer );
begin
  if ( sound = '' ) then
  begin
    Mix_HaltChannel( 1 );
    Mix_FreeChunk( chunk );
    chunk := nil;
    exit;
  end;

  if ( chunk <> nil ) then
  begin
    Mix_HaltChannel( 1 );
    Mix_FreeChunk( chunk );

    chunk := nil;
  end;

  chunk := Mix_LoadWAV( PChar( sound ) );

  if ( chunk = nil ) then
    //fprintf( stderr, "Failed to load sound: %s\n", sound );

    Mix_PlayChannel( -1, chunk, Loop );

  exit;
end;

procedure ResetObjects;
var
  loop1 : integer;
begin
  player.x := 0;
  player.y := 0;
  player.fx := 0;
  player.fy := 0;

  for loop1 := 0 to ( stage * level ) - 1 do
  begin
    enemies[ loop1 ].x := 5 + random( 6 );
    enemies[ loop1 ].y := random( 11 );
    enemies[ loop1 ].fx := enemies[ loop1 ].x * 60;
    enemies[ loop1 ].fy := enemies[ loop1 ].y * 40;
  end;
end;

var
  Done : Boolean;
  event : TSDL_Event;
  videoflags : Uint32;
  videoInfo : PSDL_VideoInfo;
  loop1, loop2, delay : integer;
begin
  // Load the appropriate .DLL or .SO
  LoadOpenGL;
  // Initialize SDL with Audio
  if ( SDL_Init( SDL_INIT_VIDEO or SDL_INIT_AUDIO ) < 0 ) then
  begin
    Log.LogError( Format( 'Could not initialize SDL : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;

  // Open the sound device
  if ( Mix_OpenAudio( 22060, AUDIO_S16SYS, 2, 512 ) < 0 ) then
  begin
    Log.LogError( Format( 'Unable to open audio : %s', [ SDL_GetError ] ),
      'Main' );
    TerminateApplication;
  end;

  // Load in the music
  music := Mix_LoadMUS( 'sounds/lktheme.mod' );

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
  SDL_WM_SetCaption( 'Jeff Molofee''s OpenGL Code Tutorial 21 using JEDI-SDL', nil
    );
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

  // Enable key repeat */
  if SDL_EnableKeyRepeat( 100, SDL_DEFAULT_REPEAT_INTERVAL ) = -1 then
  begin
    Log.LogWarning( Format( 'Setting keyboard repeat failed: %s',
      [ SDL_GetError ] ), 'Main' );
    TerminateApplication;
  end;

  // Loop, drawing and checking events
  InitGL;

  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

  // reset our objects
  ResetObjects;

  // Start playing the music
  Mix_PlayMusic( music, -1 );

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

    // Waste some cycles
    while ( SDL_GetTicks( ) < time + steps[ adjust ] * 2 ) do
      ;

    // draw the scene
    DrawGLScene;

    if ( not gameover ) then
    begin
      // Move the enemies
      for loop1 := 0 to ( stage * level ) - 1 do
      begin
        // Move The Enemy Right
        if ( ( enemies[ loop1 ].x < player.x ) and
          ( enemies[ loop1 ].fy = enemies[ loop1 ].y * 40 ) ) then
          enemies[ loop1 ].x := enemies[ loop1 ].x + 1;

        // Move The Enemy Left
        if ( ( enemies[ loop1 ].x > player.x ) and
          ( enemies[ loop1 ].fy = enemies[ loop1 ].y * 40 ) ) then
          enemies[ loop1 ].x := enemies[ loop1 ].x - 1;

        // Move The Enemy Down
        if ( ( enemies[ loop1 ].y < player.y ) and
          ( enemies[ loop1 ].fx = enemies[ loop1 ].x * 60 ) ) then
          enemies[ loop1 ].y := enemies[ loop1 ].y + 1;

        // Move The Enemy Up
        if ( ( enemies[ loop1 ].y > player.y ) and
          ( enemies[ loop1 ].fx = enemies[ loop1 ].x * 60 ) ) then
          enemies[ loop1 ].y := enemies[ loop1 ].y - 1;

        // Should the enemies move?
        if ( delay > ( 3 - level ) ) and ( ( hourglass.fx <> 2 ) ) then
        begin
          // Reset The Delay Counter Back To Zero
          delay := 0;

          // Loop Through All The Enemies
          for loop2 := 0 to ( stage * level ) - 1 do
          begin
            // Is Fine Position On X Axis Lower
            // Than Intended Position?

            if ( enemies[ loop2 ].fx <
              enemies[ loop2 ].x * 60 ) then
            begin
              // Increase Fine Position
              // On X Axis
              enemies[ loop2 ].fx := enemies[ loop2 ].fx + steps[ adjust ];
              // Spin Enemy Clockwise
              enemies[ loop2 ].spin := enemies[ loop2 ].spin + steps[ adjust ];
            end;

            // Is Fine Position On X Axis
            // Higher Than Intended Position?

            if ( enemies[ loop2 ].fx >
              enemies[ loop2 ].x * 60 ) then
            begin
              // Decrease Fine Position
              // On X Axis

              enemies[ loop2 ].fx := enemies[ loop2 ].fx - steps[ adjust ];
              // Spin Enemy Counter
              // Clockwise

              enemies[ loop2 ].spin := enemies[ loop2 ].spin - steps[ adjust ];
            end;

            // Is Fine Position On Y Axis Lower
            // Than Intended Position?

            if ( enemies[ loop2 ].fy <
              enemies[ loop2 ].y * 40 ) then
            begin
              // Increase Fine Position
                                                        // On Y Axis

              enemies[ loop2 ].fy := enemies[ loop2 ].fy +
                steps[ adjust ];
              // Spin Enemy Clockwise
              enemies[ loop2 ].spin := enemies[ loop2 ].spin +
                steps[ adjust ];
            end;

            // Is Fine Position On Y Axis
            // Higher Than Intended Position?

            if ( enemies[ loop2 ].fy >
              enemies[ loop2 ].y * 40 ) then
            begin
              // Decrease Fine Position
              // On Y Axis

              enemies[ loop2 ].fy := enemies[ loop2 ].fy -
                steps[ adjust ];
              // Spin Enemy Counter
              // Clockwise

              enemies[ loop2 ].spin := enemies[ loop2 ].spin -
                steps[ adjust ];
            end;
          end;
        end;

        // Are Any Of The Enemies On Top Of The Player?
        if ( ( enemies[ loop1 ].fx = player.fx ) and
          ( enemies[ loop1 ].fy = player.fy ) ) then
        begin
          // Player Loses A Life
          dec( lives );

          // Are We Out Of Lives?
          if ( lives = 0 ) then
            gameover := true;

          // Play The Death Sound
          PlaySound( 'sounds/die.wav', 0 );

          ResetObjects;
        end;
      end;

      // Move the player
      // Is Fine Position On X Axis Lower Than
      // Intended Position?

      if ( player.fx < player.x * 60 ) then
        // Increase The Fine X Position
        player.fx := player.fx + steps[ adjust ];

      // Is Fine Position On X Axis Greater Than
      // Intended Position?

      if ( player.fx > player.x * 60 ) then
        // Decrease The Fine X Position
        player.fx := player.fx - steps[ adjust ];

      // Is Fine Position On Y Axis Lower Than
      // Intended Position?

      if ( player.fy < player.y * 40 ) then
        // Increase The Fine Y Position
        player.fy := player.fy + steps[ adjust ];

      // Is Fine Position On Y Axis Lower Than
      // Intended Position?

      if ( player.fy > player.y * 40 ) then
        // Decrease The Fine Y Position
        player.fy := player.fy - steps[ adjust ];
    end;

    // Is The Grid Filled In?
    if ( filled ) then
    begin
      // Play The Level Complete Sound
      PlaySound( 'sounds/complete.wav', 0 );

      // Increase The Stage
      inc( stage );

      // Is The Stage Higher Than 3?
      if ( stage > 3 ) then
      begin
        stage := 1; // If So, Set The Stage To One
        inc( level ); // Increase The Level
        inc( level2 ); // Increase The Displayed Level

        // Is The Level Greater Than 3?
        if ( level > 3 ) then
        begin
          // Set The Level To 3
          level := 3;
          // Give The Player A Free Life
          inc( lives );

          // Player Have More Than 5 Lives?
          if ( lives > 5 ) then
            lives := 5; // Set Lives To Five
        end;
      end;

      // Reset Player / Enemy Positions
      ResetObjects;

      // Loop Through The Grid X Coordinates
      for loop1 := 0 to 10 do
      begin
        // Loop Through The Grid Y Coordinates
        for loop2 := 0 to 10 do
        begin
          // If X Coordinate Is Less Than 10
          if ( loop1 < 10 ) then
            // Set Horizontal Value To FALSE
            hline[ loop1 ][ loop2 ] := FALSE;

          // If Y Coordinate Is Less Than 10
          if ( loop2 < 10 ) then
            // Set Vertical Value To FALSE
            vline[ loop1 ][ loop2 ] := FALSE;
        end;
      end;
    end;

    // If The Player Hits The Hourglass While
    // It's Being Displayed On The Screen

    if ( ( player.fx = hourglass.x * 60 ) and
      ( player.fy = hourglass.y * 40 ) and
      ( hourglass.fx = 1 ) ) then
    begin
      // Play Freeze Enemy Sound
      PlaySound( 'sounds/freeze.wav', -1 );

      // Set The hourglass fx Variable To Two
      hourglass.fx := 2;
      // Set The hourglass fy Variable To Zero
      hourglass.fy := 0;
    end;

    // Spin The Player Clockwise
    player.spin := player.spin + ( 0.5 * steps[ adjust ] );

    // Is The spin Value Greater Than 360?
    if ( player.spin > 360.0 ) then
      player.spin := player.spin - 360;

    // Spin The Hourglass Counter Clockwise
    hourglass.spin := hourglass.spin - ( 0.25 * steps[ adjust ] );

    // Is The spin Value Less Than 0?
    if ( hourglass.spin < 0.0 ) then
      hourglass.spin := hourglass.spin + 360.0;

    // Increase The hourglass fy Variable
    hourglass.fy := hourglass.fy + steps[ adjust ];

    // Is The hourglass fx Variable Equal To 0 And
    // The fy Variable Greater Than 6000 Divided By The Current Level?

    if ( ( hourglass.fx = 0 ) and ( hourglass.fy > 6000 / level ) ) then
    begin
      // Play The Hourglass Appears Sound
      PlaySound( 'sounds/hourglass.wav', 0 );
      // Give The Hourglass A Random X Value
      hourglass.x := random( 10 ) + 1;
      // Give The Hourglass A Random Y Value
      hourglass.y := random( 11 );
      // Set hourglass fx Variable To One (Hourglass Stage)
      hourglass.fx := 1;
      // Set hourglass fy Variable To Zero (Counter)
      hourglass.fy := 0;

    end;

    // Is The hourglass fx Variable Equal To 1 And
    // The fy Variable Greater Than 6000 Divided By The Current Level?

    if ( ( hourglass.fx = 1 ) and ( hourglass.fy > 6000 / level ) ) then
    begin
      // Set fx To Zero (Hourglass Will Vanish)
      hourglass.fx := 0;
      // Set fy to Zero (Counter Is Reset)
      hourglass.fy := 0;
    end;

    // Is The hourglass fx Variable Equal To 2 And The fy Variable
    // Greater Than 500 Plus 500 Times The Current Level?

    if ( ( hourglass.fx = 2 ) and
      ( hourglass.fy > 500 + ( 500 * level ) ) ) then
    begin
      // Kill The Freeze Sound
      PlaySound( '', 0 );

      // Set hourglass fx Variable To Zero
      hourglass.fx := 0;
      // Set hourglass fy Variable To Zero
      hourglass.fy := 0;
    end;

    inc( delay );

  end;
  TerminateApplication;
end.


