program JEDISDLOpenGLMetaBalls;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Port of the Metaballs Example by Jan Horn                  }
{                                                                  }
{ Portions created by Jan Horn <jhorn@global.co.za>,  are          }
{ Copyright (C) 2001 Jan Horn.                                     }
{ All Rights Reserved.                                             }
{                                                                  }
{                                                                  }
{ The original Pascal code is : JEDISDLOpenGLMetaBalls.dpr         }
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
{  do Metaballs !                                                  }
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
{   August 12 2001 - JH : Initial demo.                            }
{   August 20 2001 - DL : Port to JEDI-SDL.                        }
{ November 29 2001 - DL : Added Environment mapping.               }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  OpenGL12,
  LookUpTable,
  Logger,
  SDL;

type
  TGLCoord = Record
    X, Y, Z : TGLFLoat;
  end;

  TMetaBall = Record
    Radius : TGLFloat;
    X, Y, Z : TGLFLoat;
  end;

  PGridPoint = ^TGridPoint;
  TGridPoint = Record
    Pos : TGLCoord;
    Normal : TGLCoord;
    Value : TGLFLoat;  // Result of the metaball equations at this point
  end;

  TGridCube = Record
    GridPoint : Array [0..7] of PGridPoint; // Points to 8 grid points (cube)
  end;

const
  WND_TITLE = 'Metaballs demo by Jan Horn, JEDI-SDL version by Dominique Louis';
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

var
  // This is our SDL surface
  surface : PSDL_Surface;

  ElapsedTime : LongWord;             // Elapsed time between frames

  // Textures
  EnviroTex : TglUint;
  Background : TglUint;

  // User variables
  Wireframe     : Boolean = False;
  SmoothShading : Boolean = True;
  Textured      : Boolean = True;
  GridSize      : Integer;
  TessTriangles : Integer;           // Number of triangles by metaball tesselation.
  MetaBall : Array[1..3] of TMetaBall;
  Grid  : Array[0..50, 0..50, 0..50] of TGridPoint;  // for this demo set max gridsize = 50
  Cubes : Array[0..49, 0..49, 0..49] of TGridCube;

procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;

procedure NormalizeVector(var V : TGLCoord);
var
  Length : glFloat;
begin
  Length :=Sqrt(V.x*V.x + V.y*V.y + V.z*V.z);
  if Length = 0 then exit;

  V.x :=V.x / Length;
  V.y :=V.y / Length;
  V.z :=V.z / Length;
end;

procedure SetColor(const V : TGLCoord);
var
  C : TglFloat;
begin
  with V do
    C := sqrt(x*x + y*y +z*z);
  glColor3f(C, C, C + 0.1);    // add a hint of blue
end;


procedure InitGrid;
var cx, cy, cz : Integer;
begin
  // Create the grid positions
  for cx := 0 to GridSize do
  begin
    for cy := 0 to GridSize do
    begin
      for cz := 0 to GridSize do
      begin
        Grid[cx, cy, cz].Pos.X := 2*cx/GridSize -1;   // grid from -1 to 1
        Grid[cx, cy, cz].Pos.Y := 2*cy/GridSize -1;   // grid from -1 to 1
        Grid[cx, cy, cz].Pos.Z := 1-2*cz/GridSize;    // grid from -1 to 1
      end;
    end;
  end;

  // Create the cubes. Each cube points to 8 grid points
  for cx := 0 to GridSize-1 do
  begin
    for cy := 0 to GridSize-1 do
    begin
      for cz := 0 to GridSize-1 do
      begin
         Cubes[cx,cy,cz].GridPoint[0] := @Grid[cx,   cy,   cz  ];
        Cubes[cx,cy,cz].GridPoint[1] := @Grid[cx+1, cy,   cz  ];
        Cubes[cx,cy,cz].GridPoint[2] := @Grid[cx+1, cy,   cz+1];
        Cubes[cx,cy,cz].GridPoint[3] := @Grid[cx,   cy,   cz+1];
        Cubes[cx,cy,cz].GridPoint[4] := @Grid[cx,   cy+1, cz  ];
        Cubes[cx,cy,cz].GridPoint[5] := @Grid[cx+1, cy+1, cz  ];
        Cubes[cx,cy,cz].GridPoint[6] := @Grid[cx+1, cy+1, cz+1];
        Cubes[cx,cy,cz].GridPoint[7] := @Grid[cx,   cy+1, cz+1];
      end;
    end;
  end;
end;

{----------------------------------------------------------}
{  Interpolate the position where an metaballs intersects  }
{  the line betweenthe two coordicates, C1 and C2          }
{----------------------------------------------------------}
procedure Interpolate(const C1, C2 : TGridPoint; var CResult, Norm : TGLCoord);
var mu : glFLoat;
begin
  if Abs(C1.Value) = 1 then
  begin
    CResult := C1.Pos;
    Norm := C1.Normal;
  end
  else
  if Abs(C2.Value) = 1 then
  begin
    CResult := C2.Pos;
    Norm := C2.Normal;
  end
  else
  if C1.Value = C2.Value then
  begin
    CResult := C1.Pos;
    Norm := C1.Normal;
  end
  else
  begin
    mu := (1 - C1.Value) / (C2.Value - C1.Value);
    CResult.x := C1.Pos.x + mu * (C2.Pos.x - C1.Pos.x);
    CResult.y := C1.Pos.y + mu * (C2.Pos.y - C1.Pos.y);
    CResult.z := C1.Pos.z + mu * (C2.Pos.z - C1.Pos.z);

    Norm.X := C1.Normal.X + (C2.Normal.X - C1.Normal.X) * mu;
    Norm.Y := C1.Normal.Y + (C2.Normal.Y - C1.Normal.Y) * mu;
    Norm.Z := C1.Normal.Z + (C2.Normal.Z - C1.Normal.Z) * mu;
  end;
end;


{------------------------------------------------------------}
{  Calculate the triangles required to draw a Cube.          }
{  Draws the triangles that makes up a Cube                  }
{------------------------------------------------------------}
procedure CreateCubeTriangles(const GridCube : TGridCube);
var I : Integer;
    CubeIndex: Integer;
    VertList, Norm : Array[0..11] of TGLCoord;
begin
  // Determine the index into the edge table which tells
  // us which vertices are inside/outside the metaballs
  CubeIndex := 0;
  if GridCube.GridPoint[0]^.Value < 1 then CubeIndex := CubeIndex or 1;
  if GridCube.GridPoint[1]^.Value < 1 then CubeIndex := CubeIndex or 2;
  if GridCube.GridPoint[2]^.Value < 1 then CubeIndex := CubeIndex or 4;
  if GridCube.GridPoint[3]^.Value < 1 then CubeIndex := CubeIndex or 8;
  if GridCube.GridPoint[4]^.Value < 1 then CubeIndex := CubeIndex or 16;
  if GridCube.GridPoint[5]^.Value < 1 then CubeIndex := CubeIndex or 32;
  if GridCube.GridPoint[6]^.Value < 1 then CubeIndex := CubeIndex or 64;
  if GridCube.GridPoint[7]^.Value < 1 then CubeIndex := CubeIndex or 128;

  // Check if the cube is entirely in/out of the surface
  if edgeTable[CubeIndex] = 0 then
    Exit;

  // Find the vertices where the surface intersects the cube.
  with GridCube do
  begin
    if (edgeTable[CubeIndex] and 1) <> 0 then
      Interpolate(GridPoint[0]^, GridPoint[1]^, VertList[0], Norm[0]);
    if (edgeTable[CubeIndex] and 2) <> 0 then
      Interpolate(GridPoint[1]^, GridPoint[2]^, VertList[1], Norm[1]);
    if (edgeTable[CubeIndex] and 4) <> 0 then
      Interpolate(GridPoint[2]^, GridPoint[3]^, VertList[2], Norm[2]);
    if (edgeTable[CubeIndex] and 8) <> 0 then
      Interpolate(GridPoint[3]^, GridPoint[0]^, VertList[3], Norm[3]);
    if (edgeTable[CubeIndex] and 16) <> 0 then
      Interpolate(GridPoint[4]^, GridPoint[5]^, VertList[4], Norm[4]);
    if (edgeTable[CubeIndex] and 32) <> 0 then
      Interpolate(GridPoint[5]^, GridPoint[6]^, VertList[5], Norm[5]);
    if (edgeTable[CubeIndex] and 64) <> 0 then
      Interpolate(GridPoint[6]^, GridPoint[7]^, VertList[6], Norm[6]);
    if (edgeTable[CubeIndex] and 128) <> 0 then
      Interpolate(GridPoint[7]^, GridPoint[4]^, VertList[7], Norm[7]);
    if (edgeTable[CubeIndex] and 256) <> 0 then
      Interpolate(GridPoint[0]^, GridPoint[4]^, VertList[8], Norm[8]);
    if (edgeTable[CubeIndex] and 512) <> 0 then
      Interpolate(GridPoint[1]^, GridPoint[5]^, VertList[9], Norm[9]);
    if (edgeTable[CubeIndex] and 1024) <> 0 then
      Interpolate(GridPoint[2]^, GridPoint[6]^, VertList[10], Norm[10]);
    if (edgeTable[CubeIndex] and 2048) <> 0 then
      Interpolate(GridPoint[3]^, GridPoint[7]^, VertList[11], Norm[11]);
  end;

  // Draw the triangles for this cube
  I := 0;
  glColor3f(1, 1, 1);
  while TriangleTable[CubeIndex, i] <> -1 do
  begin
    if Textured then
      glNormal3fv(@Norm[TriangleTable[CubeIndex, i]])
    else
      SetColor(VertList[TriangleTable[CubeIndex][i]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i]]);

    if Textured then
      glNormal3fv(@Norm[TriangleTable[CubeIndex, i+1]])
    else
      SetColor(VertList[TriangleTable[CubeIndex][i+1]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i+1]]);

    if Textured then
      glNormal3fv(@Norm[TriangleTable[CubeIndex, i+2]])
    else
      if SmoothShading then
         SetColor(VertList[TriangleTable[CubeIndex][i+2]]);
    glVertex3fv(@VertList[TriangleTable[CubeIndex][i+2]]);

    Inc(TessTriangles);
    Inc(i, 3);
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
      if (keysym.Modifier and KMOD_ALT <> 0) then
      begin
        {* Alt+Enter key was pressed
         * this toggles fullscreen mode
         *}
        SDL_WM_ToggleFullScreen( surface );
      end;
    end;

    SDLK_T :
    begin
      Textured := not(Textured);
      if Textured then
        glEnable(GL_TEXTURE_2D)
      else
        glDisable(GL_TEXTURE_2D);
    end;

    SDLK_S:
    begin
      SmoothShading := NOT(SmoothShading); // Set Smooth shading or not
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
      WireFrame := NOT(WireFrame);
    end;

    SDLK_MINUS: // decrease grid size / resolution
    begin
      if GridSize > 5 then
        GridSize := GridSize - 1;
      InitGrid;
    end;

    SDLK_PLUS: // decrease grid size / resolution
    begin
      if GridSize < 50 then
        GridSize := GridSize + 1;
      InitGrid;
    end;
  end;
end;

{------------------------------------------------------------------}
{  Load BMP textures                                               }
{------------------------------------------------------------------}
function LoadTexture(Filename: String; var Texture: GLuint): Boolean;
var
  // Create storage space for the texture
  TextureImage: PSDL_Surface;
  ImagePath : string;
begin
  Result := True;
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  ImagePath := 'Data/' + FileName;
  TextureImage := SDL_LoadBMP( PChar( ImagePath ) );
  if ( TextureImage <> nil ) then
  begin
    glGenTextures(1, @Texture);
    glBindTexture(GL_TEXTURE_2D, Texture);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}

    { Select a filtering type. BiLinear filtering produces very good results with little performance impact
      GL_NEAREST               - Basic texture (grainy looking texture)   
      GL_LINEAR                - BiLinear filtering
      GL_LINEAR_MIPMAP_NEAREST - Basic mipmapped texture
      GL_LINEAR_MIPMAP_LINEAR  - BiLinear Mipmapped texture
    }  

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); { only first two can be used }
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); { all of the above can be used }

    gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage.w, TextureImage.H, GL_BGR, GL_UNSIGNED_BYTE, TextureImage.pixels);
  end
  else
  begin
    Result := False;
    Log.LogError( Format( 'Unable to load Image : %s, Error : %s', [FileName, SDL_GetError] ),
      'LoadTexture' );
    TerminateApplication;
  end;
end;


// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.
function InitGL : Boolean;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
  LoadTexture('chrome.bmp', EnviroTex);
  LoadTexture('background.bmp', background);

  // Set up environment mapping
  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
  glTexGeni(GL_S, GL_SPHERE_MAP, 0);
  glTexGeni(GL_T, GL_SPHERE_MAP, 0);

  glEnable(GL_NORMALIZE);

  // initialise the metaball size and positions
  MetaBall[1].Radius :=0.3;
  MetaBall[1].X :=0;
  MetaBall[1].Y :=0;
  MetaBall[1].Z :=0;

  MetaBall[2].Radius :=0.22;
  MetaBall[2].X :=0;
  MetaBall[2].Y :=0;
  MetaBall[2].Z :=0;

  MetaBall[3].Radius :=0.25;
  MetaBall[3].X :=0;
  MetaBall[3].Y :=0;
  MetaBall[3].Z :=0;

  Textured :=TRUE;
  SmoothShading :=TRUE;
  WireFrame :=FALSE;
  GridSize  :=25;
  InitGrid;

  result := true;
end;

// The main drawing function.
procedure DrawGLScene;
var cx, cy, cz : Integer;
    I : Integer;
    c : glFloat;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity();                                       // Reset The View

  glTranslatef(0.0,0.0,-2.5);

  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);
  glBindTexture(GL_TEXTURE_2D, Background);
  glBegin(GL_QUADS);
    glTexCoord2d(0, 0);   glVertex3d(-1.5,-1.1, 0);
    glTexCoord2d(1, 0);   glVertex3d( 1.5,-1.1, 0);
    glTexCoord2d(1, 1);   glVertex3d( 1.5, 1.1, 0);
    glTexCoord2d(0, 1);   glVertex3d(-1.5, 1.1, 0);
  glEnd();
  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);

  glRotatef(ElapsedTime/30, 0, 0, 1);

  c := 0.15*cos(ElapsedTime/600);
  MetaBall[1].X :=-0.3*cos(ElapsedTime/700) - c;
  MetaBall[1].Y :=0.3*sin(ElapsedTime/600) - c;

  MetaBall[2].X :=0.4*sin(ElapsedTime/400) + c;
  MetaBall[2].Y :=0.4*cos(ElapsedTime/400) - c;

  MetaBall[3].X :=-0.4*cos(ElapsedTime/400) - 0.2*sin(ElapsedTime/600);
  MetaBall[3].y :=0.4*sin(ElapsedTime/500) - 0.2*sin(ElapsedTime/400);

  TessTriangles := 0;
  For cx := 0 to GridSize do
    For cy := 0 to GridSize do
      For cz := 0 to GridSize do
        with Grid[cx, cy, cz] do
        begin
          Value :=0;
          for I :=1 to 3 do  // go through all meta balls
          begin
            with Metaball[I] do
               Value := Value + Radius*Radius /((Pos.x-x)*(Pos.x-x) + (Pos.y-y)*(Pos.y-y) + (Pos.z-z)*(Pos.z-z));
          end;
        end;

  // Calculate normals at the grid vertices
  For cx := 1 to GridSize-1 do
  begin
    For cy := 1 to GridSize-1 do
    begin
      For cz := 1 to GridSize-1 do
      begin
        Grid[cx,cy,cz].Normal.X := Grid[cx-1, cy, cz].Value - Grid[cx+1, cy, cz].Value;
        Grid[cx,cy,cz].Normal.Y := Grid[cx, cy-1, cz].Value - Grid[cx, cy+1, cz].Value;
        Grid[cx,cy,cz].Normal.Z := Grid[cx, cy, cz-1].Value - Grid[cx, cy, cz+1].Value;
//        NormalizeVector(Grid[cx,cy,cz].Normal);
      end;
    end;
  end;

  // Draw the metaballs by drawing the triangle in each cube in the grid
  glBindTexture(GL_TEXTURE_2D, EnviroTex);
  glBegin(GL_TRIANGLES);
    For cx := 0 to GridSize-1 do
      for cy := 0 to GridSize-1 do
        for cz := 0 to GridSize-1 do
          CreateCubeTriangles(Cubes[cx, cy, cz]);
  glEnd;

  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  DemoStart, LastTime : LongWord;

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
    LastTime :=ElapsedTime;
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
  end;
  TerminateApplication;
end.

