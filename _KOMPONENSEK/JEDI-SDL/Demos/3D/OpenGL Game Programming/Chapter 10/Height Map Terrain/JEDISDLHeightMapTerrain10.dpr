program JEDISDLHeightMapTerrain10;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of an example of OpenGL HeightMap Terrains from }
{       the book OpenGL Game Programming by Kevin Hawkins and      }
{       Dave Astle.                                                }
{                                                                  }
{ Portions created by Hawkins Astle, are  Copyright (C) 2001       }
{ Ti Leggett.                                                      }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : HeightMapTerrain.c                      }
{                                                                  }
{ The original Pascal code is : JEDISDLHeightMapTerrain.dpr        }
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
{  November 30 2001 - DL : Initial translation.                    }
{                                                                  }
{******************************************************************}

uses
  OpenGL12,
  SysUtils,
  Logger,
  SDLUtils,
  SDL;

const
  // screen width, height, and bit depth
  WINDOW_TITLE = 'OpenGL Game Programming Chapter 10 : Terrain Demo using Vertex Arrays';
  SCREEN_WIDTH = 800;
  SCREEN_HEIGHT = 600;
  SCREEN_BPP = 32;
  MAP_X = 32; // size of map along x-axis
  MAP_Z = 32; // size of map along z-axis
  MAP_SCALE = 20.0; // the scale of the terrain map
  PI = 3.14159;


var
  angle : TGLfloat = 0.0; // camera angle
  radians : TGLfloat = 0.0; // camera angle in radians
  waterHeight : TGLfloat = 154.0; // height of water
  waterDir : Boolean = true; // used to animate water; true = up, false = down

  ////// Mouse/Camera Variables
  mouseX, oldMouseX, mouseY, oldMouseY : integer; // mouse coordinates
  cameraPos : array[ 0..2 ] of TGLfloat; // camera coordinates
  lookAt : array[ 0..2 ] of TGLfloat; // camera look-at coordinates

  surface : PSDL_Surface; // main surface
  HeightMap : PSDL_Surface; // the map image data
  LandTextureImage : PSDL_Surface; // land texture data
  WaterTextureImage : PSDL_Surface; // water texture data
  LandTextureID : TGLUInt; // the land texture object
  WaterTextureID : TGLUInt; // the water texture object

  ////// Terrain Data
  indexArray : array[ 0..( MAP_X * MAP_Z * 6 ) - 1 ] of TGLuint; // vertex index array
  terrain : array[ 0..( MAP_X * MAP_Z ) - 1 ] of array[ 0..2 ] of TGLFloat; // heightfield terrain data (0-255); 256x256
  colorArray : array[ 0..( MAP_X * MAP_Z ) - 1 ] of array[ 0..2 ] of TGLFloat; // heightfield terrain data (0-255); 256x256
  texcoordArray : array[ 0..( MAP_X * MAP_Z ) - 1 ] of array[ 0..2 ] of TGLFloat; // heightfield terrain data (0-255); 256x256


  //Status indicator
  Status : Boolean = false;


  // InitializeTerrain
  // desc: initializes the heightfield terrain data

procedure InitializeTerrain;
var
  x, z : Byte;
begin
  // loop through all of the heightfield points, calculating
  // the coordinates for each point
  for z := 0 to MAP_Z - 1 do
  begin
    for x := 0 to MAP_X - 1 do
    begin
      terrain[x + MAP_X * z, 0] := x * MAP_SCALE;
      terrain[x + MAP_X * z, 1] := PByteArray( HeightMap.pixels )[ ( z * MAP_Z + x ) * 3 ];
      terrain[x + MAP_X * z][ 2 ] := -z * MAP_SCALE;
    end;
  end;
end;


procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;

// Load Bitmaps And Convert To Textures

function LoadGLTextures : Boolean;
begin
  // load the land texture data
  LandTextureImage := SDL_LoadBMP( '../../images/green.bmp' );
  if ( LandTextureImage = nil ) then
  begin
    Result := false;
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    exit
  end;

  // load the water texture data
  WaterTextureImage := SDL_LoadBMP( '../../images/water.bmp' );
  if ( WaterTextureImage = nil ) then
  begin
    Result := false;
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'LoadGLTextures' );
    exit
  end;

  // generate the land texture as a mipmap
  glGenTextures( 1, @LandTextureID );
  glBindTexture( GL_TEXTURE_2D, LandTextureID );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  gluBuild2DMipmaps( GL_TEXTURE_2D, 3, LandTextureImage.w, LandTextureImage.h, GL_BGR, GL_UNSIGNED_BYTE, LandTextureImage.pixels );

  // generate the water texture as a mipmap
  glGenTextures( 1, @WaterTextureID );
  glBindTexture( GL_TEXTURE_2D, WaterTextureID );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
  gluBuild2DMipmaps( GL_TEXTURE_2D, 3, WaterTextureImage.w, WaterTextureImage.h, GL_BGR, GL_UNSIGNED_BYTE, WaterTextureImage.pixels );
  Result := true;
end;

procedure CleanUp;
begin
  SDL_FreeSurface( HeightMap );
  SDL_FreeSurface( LandTextureImage );
  SDL_FreeSurface( WaterTextureImage );
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  // if the compiled arrays extension is available, unlock the arrays
  glUnlockArraysEXT;
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
  gluPerspective( 54.0, width / height, 10.0, 1000.0 );

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

{*****************************************************************************
 InitializeArrays()

 Loads the terrain data into 3 vertex arrays (vertex, color, and tex coord),
 as well as creating an index array.
*****************************************************************************}

procedure InitializeArrays;
var
  // used to track current entry in the index array
  index : integer;
  currentVertex : integer;
  x, z : integer;
begin
  index := 0;
  // loop over all vertices in the terrain map
  for z := 0 to MAP_Z - 1 do
  begin
    for x := 0 to MAP_X - 1 do
    begin
      // vertices are numbered left to right, top to bottom
      currentVertex := z * MAP_X + x;

      // set the values in the color array
      colorArray[ currentVertex][ 0 ] := terrain[ x + MAP_X * z, 1 ] / 255.0;
      colorArray[ currentVertex][ 1 ] := terrain[ x + MAP_X * z, 1 ] / 255.0;
      colorArray[ currentVertex][ 2 ] := terrain[ x + MAP_X * z, 1 ] / 255.0;

      // set the values in the texture coordinate array. since the texture
      // is tiled over each "square", we can use texture wrapping
      texcoordArray[ currentVertex][ 0 ] := x;
      texcoordArray[ currentVertex][ 1 ] := z;
    end;
  end;

  for z := 0 to MAP_Z - 2 do
  begin
    for x := 0 to MAP_X - 1 do
    begin
      currentVertex := z * MAP_X + x;
      indexArray[ index ] := currentVertex + MAP_X;
      inc( index );
      indexArray[ index ] := currentVertex;
      inc( index );
    end;
  end;

  // enable the vertex arrays being used
  glEnableClientState( GL_VERTEX_ARRAY );
  glEnableClientState( GL_COLOR_ARRAY );
  glEnableClientState( GL_TEXTURE_COORD_ARRAY );

  // pass the pointers to OpenGL
  glVertexPointer( 3, GL_FLOAT, 0, @terrain );
  glColorPointer( 3, GL_FLOAT, 0, @colorArray );
  glTexCoordPointer( 2, GL_FLOAT, 0, @texcoordArray );
end; // end InitializeArrays()

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.
function InitGL : Boolean;
var
  extList : string;
begin
  glClearColor( 0.0, 0.0, 0.0, 0.0 ); // clear to black

  glShadeModel( GL_SMOOTH ); // use smooth shading
  glEnable( GL_DEPTH_TEST ); // hidden surface removal
  glEnable( GL_CULL_FACE ); // do not calculate inside of poly's
  glFrontFace( GL_CCW ); // counter clock-wise polygons are out

  glEnable( GL_TEXTURE_2D ); // enable 2D texturing

  HeightMap := SDL_LoadBMP( '../../images/heightmap.bmp' );
  if ( HeightMap = nil ) then
  begin
    Result := false;
    Log.LogError( Format( 'Could not Load Image : %s', [ SDL_GetError ] ),
      'InitGL' );
    exit
  end;

  // initialize the terrain data and load the textures
  InitializeTerrain;
  // Load in the texture
  if ( not LoadGLTextures ) then
  begin
    result := false;
    Log.LogError( Format( 'Could not Load Textures : %s', [ SDL_GetError ] ),
      'InitGL' );
    exit;
  end;

  // load terrain data into the arrays
  InitializeArrays;

  // check for the compiled array extensions
  extList := glGetString( GL_EXTENSIONS );

  if not ( Pos( 'GL_EXT_compiled_vertex_array', extList ) > 0 ) then
  begin
    result := false;
    Log.LogError( 'GL_EXT_compiled_vertex_array not supported!', 'InitGL' );
    exit;
  end;

  // if the compiled arrays extension is available, lock the arrays
  glLockArraysEXT( 0, MAP_X * MAP_Z );

  result := true;
end;

// The main drawing function.

procedure DrawGLScene;
var
  {x,} z : Byte;
begin
  radians := PI * ( angle - 90.0 ) / 180.0;

  // calculate the camera's position
  cameraPos[ 0 ] := lookAt[ 0 ] + sin( radians ) * mouseY; // multiplying by mouseY makes the
  cameraPos[ 2 ] := lookAt[ 2 ] + cos( radians ) * mouseY; // camera get closer/farther away with mouseY
  cameraPos[ 1 ] := lookAt[ 1 ] + mouseY / 2.0;

  // calculate the camera look-at coordinates as the center of the terrain map
  lookAt[ 0 ] := ( MAP_X * MAP_SCALE ) / 2.0;
  lookAt[ 1 ] := 150.0;
  lookAt[ 2 ] := -( MAP_Z * MAP_SCALE ) / 2.0;

  // clear screen and depth buffer
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glLoadIdentity;

  // set the camera position
  gluLookAt( cameraPos[ 0 ], cameraPos[ 1 ], cameraPos[ 2 ], lookAt[ 0 ], lookAt[ 1 ], lookAt[ 2 ],
    0.0, 1.0, 0.0 );

  // set the current texture to the land texture
  glBindTexture( GL_TEXTURE_2D, LandTextureID );


  // loop through all the triangle strips
  for z := 0 to MAP_Z - 2 do
  begin
    // draw the triangles in this strip
    glDrawElements( GL_TRIANGLE_STRIP, MAP_X * 2, GL_UNSIGNED_INT, @indexArray[ z * MAP_X * 2 ] );
  end;

  // enable blending
  glEnable( GL_BLEND );

  // enable read-only depth buffer
  glDepthMask( TGLBoolean( GL_FALSE ) );

  // set the blend function to what we use for transparency
  glBlendFunc( GL_SRC_ALPHA, GL_ONE );

  glColor4f( 0.5, 0.5, 1.0, 0.7 ); // set color to a transparent blue
  glBindTexture( GL_TEXTURE_2D, WaterTextureID ); // set texture to the water texture

  // draw water as one large quad surface
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0);     // lower left corner
    glVertex3f(terrain[0][0], waterHeight, terrain[0][2]);

    glTexCoord2f(10.0, 0.0);    // lower right corner
    glVertex3f(terrain[MAP_X-1][0], waterHeight, terrain[MAP_X-1][2]);

    glTexCoord2f(10.0, 10.0);   // upper right corner
    glVertex3f(terrain[MAP_X-1 + MAP_X * (MAP_Z-1)][0], waterHeight, terrain[MAP_X-1 + MAP_X * (MAP_Z-1)][2]);

    glTexCoord2f(0.0, 10.0);    // upper left corner
    glVertex3f(terrain[MAP_X * (MAP_Z-1)][0], waterHeight, terrain[MAP_X * (MAP_Z-1)][2]);
  glEnd;

  // set back to normal depth buffer mode (writable)
  glDepthMask( TGLBoolean( GL_TRUE ) );

  // disable blending
  glDisable( GL_BLEND );

  // animate the water
  if ( waterHeight > 155.0 ) then
    waterDir := false
  else if ( waterHeight < 154.0 ) then
    waterDir := true;

  if ( waterDir ) then
    waterHeight := waterHeight + 0.01
  else
    waterHeight := waterHeight - 0.01;

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
  SDL_WM_SetCaption( WINDOW_TITLE, nil );

  videoflags := videoFlags or SDL_RESIZABLE; // Enable window resizing

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ] ),
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

        SDL_MOUSEMOTION :
          begin
            OldMouseX := mouseX;
            OldMouseY := mouseY;
            mouseX := event.motion.x;
            mouseY := event.motion.y;

            // these lines limit the camera's range
            if ( mouseY < 200 ) then
              mouseY := 200;
            if ( mouseY > 450 ) then
              mouseY := 450;

            if ( ( mouseX - OldMouseX ) > 0 ) then // mouse moved to the right
              angle := angle + 3.0
            else if ( ( mouseX - OldMouseX ) < 0 ) then // mouse moved to the left
              angle := angle - 3.0;
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

