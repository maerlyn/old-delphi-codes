program JEDISDLShadows;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL with OpenGL Example                     }
{       Conversion of the SDL OpenGL Examples                      }
{                                                                  }
{ Portions created by Jan Horn <jhorn@global.co.za>,  are          }
{ Copyright (C) 2002 Jan Horn.                                     }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original Pascal code is : JEDISDLShadows.dpr                 }
{ The initial developer of the this code is :                      }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
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
{   June   16 2001 - DL : Initial translation.                     }
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
  FPS_TIMER = 1; // Timer to calculate FPS
  FPS_INTERVAL = 500; // Calculate FPS every 500 ms

type
  TVertex = Record
    X, Y, Z : glFloat;
  end;
  
var
  // This is our SDL surface
  surface: PSDL_Surface;
  FPSCount: Integer = 0; // Counter for FPS
  ElapsedTime: Integer; // Elapsed time between frames

  // Textures
  FloorTex, WallTex: TglUint;

  // User variables
  TorusDL: array[0..1] of TglUint; // Torus display lists
  RoomDL: array[0..4] of TglUint; // room wall display lists
  Torus1Position: array[0..2] of TglFLoat = (4, 2, 2);
  Torus2Position: array[0..2] of TglFLoat = (-4, -5, 6);
  ShadowMatrix: array[0..4, 0..15] of TglFloat;
  RoomNormal, RoomPoints: array[0..4] of TVertex;

  // Lights
  LightObj: PgluQuadricObj;
  LightPosition: array[0..3] of glFloat = (-7.0, -2.5, 10, 1.0);

  // Mouse
  MouseButton: Integer = -1; // mouse button down
  xPos, yPos, zPos: glFloat; // Location
  Xcoord : Integer = 0;      // Mouse Coordinates
  Ycoord : Integer = 0;
  Zcoord : Integer = 0;

  //Status indicator
  Status: Boolean = false;

procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;

{------------------------------------------------------------------}
{  Create a torus be giving inner, outer radius and detail level   }
{------------------------------------------------------------------}
procedure CreateTorus(TorusNumber : Integer; TubeRadius, Radius : GLfloat; Sides, Rings : Integer);
var I, J : Integer;
    theta, phi, theta1 : GLfloat;
    cosTheta, sinTheta : GLfloat;
    cosTheta1, sinTheta1 : GLfloat;
    ringDelta, sideDelta : GLfloat;
    cosPhi, sinPhi, dist : GLfloat;
begin
  sideDelta := 2.0 * Pi / Sides;
  ringDelta := 2.0 * Pi / rings;
  theta := 0.0;
  cosTheta := 1.0;
  sinTheta := 0.0;

  TorusDL[TorusNumber] :=glGenLists(1);
  glNewList(TorusDL[TorusNumber], GL_COMPILE);
    for i := rings - 1 downto 0 do
    begin
      theta1 := theta + ringDelta;
      cosTheta1 := cos(theta1);
      sinTheta1 := sin(theta1);
      glBegin(GL_QUAD_STRIP);
        phi := 0.0;
        for j := Sides downto 0 do
        begin
          phi := phi + sideDelta;
          cosPhi := cos(phi);
          sinPhi := sin(phi);
          dist := Radius + (TubeRadius * cosPhi);

          glNormal3f(cosTheta1 * cosPhi, -sinTheta1 * cosPhi, sinPhi);
          glVertex3f(cosTheta1 * dist, -sinTheta1 * dist, TubeRadius * sinPhi);

          glNormal3f(cosTheta * cosPhi, -sinTheta * cosPhi, sinPhi);
          glVertex3f(cosTheta * dist, -sinTheta * dist, TubeRadius * sinPhi);
        end;
      glEnd();
      theta := theta1;
      cosTheta := cosTheta1;
      sinTheta := sinTheta1;
    end;
  glEndList();
end;


{------------------------------------------------------------------}
{  Create a display list object for each wall in the room          }
{------------------------------------------------------------------}
procedure createRoomDisplayLists;
begin
  // Left face
  roomNormal[0].X := 1.0; roomNormal[0].Y := 0.0; roomNormal[0].Z := 0.0;
  roomPoints[0].X := -10; roomPoints[0].Y := -10; roomPoints[0].Z := 10;
  RoomDL[0] :=glGenLists(1);
  glNewList(RoomDL[0], GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, WallTex);
    glBegin(GL_QUADS);
      glNormal3f( 1.0, 0.0, 0.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( -10.0, -10.0, -10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( -10.0,  10.0, -10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f( -10.0,  10.0,  10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( -10.0, -10.0,  10.0);
    glEnd();
  glEndList();

  // Back front facing wall
  roomNormal[1].X := 0.0; roomNormal[1].Y := 0.0; roomNormal[1].Z := 1.0;
  roomPoints[1].X := -10; roomPoints[1].Y := -10; roomPoints[1].Z := -10;
  RoomDL[1] :=glGenLists(1);
  glNewList(RoomDL[1], GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, WallTex);
    glBegin(GL_QUADS);
      glNormal3f( 0.0, 0.0, 1.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(-10.0, -10.0,  -10.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( 10.0, -10.0,  -10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( 10.0,  10.0,  -10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f(-10.0,  10.0,  -10.0);
    glEnd();
  glEndList();

  // Right Face
  roomNormal[2].X := -1.0; roomNormal[2].Y := 0.0; roomNormal[2].Z := 0.0;
  roomPoints[2].X :=  10;  roomPoints[2].Y := -10; roomPoints[2].Z := 10;
  RoomDL[2] :=glGenLists(1);
  glNewList(RoomDL[2], GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, WallTex);
    glBegin(GL_QUADS);
      glNormal3f(-1.0, 0.0, 0.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 10.0, -10.0, -10.0);
      glTexCoord2f(2.0, 0.0); glVertex3f( 10.0, -10.0,  10.0);
      glTexCoord2f(2.0, 2.0); glVertex3f( 10.0,  10.0,  10.0);
      glTexCoord2f(0.0, 2.0); glVertex3f( 10.0,  10.0, -10.0);
    glEnd();
  glEndList();

  // Floor
  roomNormal[3].X := 0.0; roomNormal[3].Y := 1.0; roomNormal[3].Z := 0.0;
  roomPoints[3].X := -10; roomPoints[3].Y := -10; roomPoints[3].Z := -10;
  RoomDL[3] :=glGenLists(1);
  glNewList(RoomDL[3], GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, FloorTex);
    glBegin(GL_QUADS);
      glNormal3f( 0.0, 1.0, 0.0);
      glTexCoord2f(0.0, 1.0); glVertex3f(-10.0,  -10.0, -10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f(-10.0,  -10.0,  10.0);
      glTexCoord2f(1.0, 0.0); glVertex3f( 10.0,  -10.0,  10.0);
      glTexCoord2f(1.0, 1.0); glVertex3f( 10.0,  -10.0, -10.0);
    glEnd();
  glEndList();

  // Ceiling
  roomNormal[4].X := 0.0; roomNormal[4].Y :=-1.0; roomNormal[4].Z := 0.0;
  roomPoints[4].X := -10; roomPoints[4].Y := 10;  roomPoints[4].Z := 10;
  RoomDL[4] :=glGenLists(1);
  glNewList(RoomDL[4], GL_COMPILE);
    glBindTexture(GL_TEXTURE_2D, FloorTex);
    glBegin(GL_QUADS);
      glNormal3f( 0.0,-1.0, 0.0);
      glTexCoord2f(1.0, 1.0); glVertex3f(-10.0, 10.0, -10.0);
      glTexCoord2f(0.0, 1.0); glVertex3f( 10.0, 10.0, -10.0);
      glTexCoord2f(0.0, 0.0); glVertex3f( 10.0, 10.0,  10.0);
      glTexCoord2f(1.0, 0.0); glVertex3f(-10.0, 10.0,  10.0);
    glEnd();
  glEndList();
end;


{---------------------------------------------------------------------}
{  Generates a shadow matrix for a plane with the normal and a point  }
{---------------------------------------------------------------------}
procedure generateShadowMatrix(var ShadowMatrix : Array of glFloat; const normal, point : TVertex; lightX, lightY, lightZ, lightW : glFloat);
var d, dot : Real;
begin
  d := -normal.X*point.X - normal.Y*point.Y - normal.Z*point.Z;
  dot :=normal.X*lightX  + normal.Y*lightY + normal.Z*lightZ + d*lightW;

  ShadowMatrix[0]  := -lightX*normal.X + dot;
  ShadowMatrix[4]  := -lightX*normal.Y;
  ShadowMatrix[8]  := -lightX*normal.Z;
  ShadowMatrix[12] := -lightX*d;
  ShadowMatrix[1]  := -lightY*normal.X;
  ShadowMatrix[5]  := -lightY*normal.Y + dot;
  ShadowMatrix[9]  := -lightY*normal.Z;
  ShadowMatrix[13] := -lightY*d;
  ShadowMatrix[2]  := -lightZ*normal.X;
  ShadowMatrix[6]  := -lightZ*normal.Y;
  ShadowMatrix[10] := -lightZ*normal.Z + dot;
  ShadowMatrix[14] := -lightZ*d;
  ShadowMatrix[3]  := -lightW*normal.X;
  ShadowMatrix[7]  := -lightW*normal.Y;
  ShadowMatrix[11] := -lightW*normal.Z;
  ShadowMatrix[15] := -lightW*d + dot;
end;


{------------------------------------------------------------------}
{  Renders the room with the shadows on the walls                  }
{------------------------------------------------------------------}
procedure RenderRoomAndShadows;
var Surface : Integer;
begin
  glClear(GL_STENCIL_BUFFER_BIT);                 // Clear the stencil buffer the first time
  glEnable(GL_STENCIL_TEST);                      // Turn on the stencil buffer test

  // go through all surfaces that get the shadow
  for Surface :=0 to 4 do
  begin
    glStencilFunc(GL_ALWAYS, 1, 1);               // Setup the stencil buffer to write
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);    // a 1 everywhere the plane is.

    glDisable(GL_LIGHT0);                         // Turn off the light
    glEnable(GL_TEXTURE_2D);
    glCallList(roomDL[surface]);                  // Render the surface into both the stencil and color buffer
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_LIGHT0);                          // Turn on the light

    glDisable(GL_DEPTH_TEST);
    glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);  // Disable the color buffer
    glStencilFunc(GL_EQUAL, 1, 1);                        // Set the stencil buffer to update
    glStencilOp(GL_KEEP, GL_KEEP, GL_INCR);               // only where it finds a 1 from the plane and increment the buffer

    // Go through all lights. Only one in this case
    glPushMatrix();
      generateShadowMatrix( ShadowMatrix[surface], roomNormal[surface], roomPoints[surface],
                            LightPosition[0], LightPosition[1], LightPosition[2], LightPosition[3]);
      glMultMatrixf(@ShadowMatrix[surface]);               // Add the shadow matrix to the ModelView

      // Go through all objects. Two toruses
      glPushMatrix();                                     // Render the shadow generators
        glTranslatef(Torus1Position[0], Torus1Position[1], Torus1Position[2]);
        glRotatef(ElapsedTime/8, 1, 0, 0);
        glRotatef(ElapsedTime/78, 0, 1, 0);
        glCallList(TorusDL[0]);
      glPopMatrix();
      glPushMatrix();                                     // Render the shadow generators
        glTranslatef(Torus2Position[0], Torus2Position[1], Torus2Position[2]);
        glRotatef(ElapsedTime/6, 1, 0, 0);
        glRotatef(ElapsedTime/58, 0, 1, 0);
        glCallList(TorusDL[1]);
      glPopMatrix();
    glPopMatrix();

    glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);      // Turn the color buffer back on

    glEnable(GL_TEXTURE_2D);
    glCallList(roomDL[surface]);                          // Render the plane where ever this is a 1, denoting the plane is in full light
    glDisable(GL_TEXTURE_2D);
    glEnable(GL_DEPTH_TEST);
  end;
  glDisable(GL_STENCIL_TEST);                             // Turn off the stencil test.
end;

// Load Bitmaps And Convert To Textures
function LoadGLTextures(image: string; var TextureID: TglUint): Boolean;
var
  // Create storage space for the texture
  TextureImage: PSDL_Surface;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage := SDL_LoadBMP(PChar(image));
  if (TextureImage <> nil) then
  begin
    // Set the status to true
    Status := true;

    // Create Texture
    glGenTextures(1, @TextureID);
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture(GL_TEXTURE_2D, TextureID);
    // Generate The Texture
    glTexImage2D(GL_TEXTURE_2D, 0, 3, TextureImage.w,
      TextureImage.h, 0, GL_BGR,
      GL_UNSIGNED_BYTE, TextureImage.pixels);
    // Linear Filtering
    // scale linearly when image bigger than texture
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    // scale linearly when image smaller than texture
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end
  else
  begin
    Log.LogError(Format('Could not Load Image : %s', [SDL_GetError]),
      'LoadGLTextures');
    TerminateApplication;
  end;
  // Free up any memory we may have used
  if (TextureImage <> nil) then
    SDL_FreeSurface(TextureImage);
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
const AmbientLight : Array[0..3] of glFloat = (2.2, 2.2, 2.2, 2.2);
begin
  glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

  // Setup lights
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @AmbientLight);
  glEnable(GL_LIGHTING);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
  glEnable(GL_LIGHT0);

  // the light object
  LightObj := gluNewQuadric();
  
  // Load in the texture
  if (not LoadGLTextures( 'images/wood004.bmp', FloorTex ) ) then
  begin
    result := false;
    exit;
  end;
  // Load in the texture
  if (not LoadGLTextures( 'images/brick27.bmp', WallTex ) ) then
  begin
    result := false;
    exit;
  end;
  
  createRoomDisplayLists;
  CreateTorus(0, 0.4, 1.8, 16, 32);
  CreateTorus(1, 0.4, 1.8, 16, 32);

  xPos :=LightPosition[0];
  yPos :=LightPosition[1];
  zPos :=-LightPosition[2];

  result := true;
end;
// The main drawing function.

procedure DrawGLScene;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
  glLoadIdentity();                                       // Reset The View

  glTranslatef(0.0,0.0,-28);

  // if the user has not clicked on the screen, give the light some movement
  if MouseButton = -1 then
  begin
    LightPosition[0] :=9*sin(ElapsedTime/1000);
    LightPosition[1] :=5*sin(ElapsedTime/800) + 3*cos(ElapsedTime/450);
    LightPosition[2] :=4*cos(ElapsedTime/600)+10;
    glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
  end;

  // Draw the light
  glDisable(GL_LIGHTING);
  glPushMatrix();
    glTranslatef(LightPosition[0], LightPosition[1], LightPosition[2]);
    glColor3f(1, 1, 0);
    gluSphere(LightObj, 0.2, 8, 8);
  glPopMatrix();

  // draw the toruses
  glPushMatrix();
    glTranslatef(Torus1Position[0], Torus1Position[1], Torus1Position[2]);
    glRotatef(ElapsedTime/8, 1, 0, 0);
    glRotatef(ElapsedTime/78, 0, 1, 0);
    glColor3f(0.8, 0.3, 0.3);
    glCallList(TorusDL[0]);
  glPopMatrix();
  glPushMatrix();
    glTranslatef(Torus2Position[0], Torus2Position[1], Torus2Position[2]);
    glRotatef(-ElapsedTime/6, 1, 0, 0);
    glRotatef(ElapsedTime/58, 0, 1, 0);
    glColor3f(0.3, 0.3, 0.8);
    glCallList(TorusDL[1]);
  glPopMatrix();
  glEnable(GL_LIGHTING);

  // draw the room and shadows
  RenderRoomAndShadows;
  
  // swap buffers to display, since we're double buffered.
  SDL_GL_SwapBuffers;
end;

var
  Done: Boolean;
  event: TSDL_Event;
  videoflags: Uint32;
  videoInfo: PSDL_VideoInfo;
  DemoStart, LastTime : Cardinal;
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
  SDL_WM_SetCaption('Jan Horn''s Project Shadow Demo using JEDI-SDL', nil
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
  DemoStart := SDL_GetTicks; 
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
        SDL_MOUSEBUTTONDOWN :
          begin
            MouseButton := event.Button.Button;
            case MouseButton of
              1 :
              begin
                Xcoord := event.button.x;
                Ycoord := event.button.y;
              end;
              3 :
              begin
                ZCoord :=event.button.y;
              end;
            end;
          end;
        SDL_MOUSEBUTTONUP :
          begin
            MouseButton := 0;
            if event.Button.Button = 1 then
            begin
              XCoord := 0;
              YCoord := 0;
            end;
          end;
        SDL_MOUSEMOTION :
          begin
            case MouseButton of
              1 :
              begin
                xPos := xPos + (event.motion.x-xCoord)/40;
                yPos := yPos - (event.motion.y-yCoord)/40;
                Xcoord := event.motion.x;
                Ycoord := event.motion.y;
                LightPosition[0] :=xPos;
                LightPosition[1] :=yPos;
                glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
              end;
              3 :
              begin
                zPos :=zPos - (event.motion.y-ZCoord)/10;
                Zcoord := event.motion.y;
                LightPosition[2] :=-zPos;
                glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);
              end;
            end;
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
    LastTime := ElapsedTime;
    ElapsedTime := SDL_GetTicks - DemoStart;     // Calculate Elapsed Time
    ElapsedTime :=(LastTime + ElapsedTime) shr 1; // Average it out for smoother movement
    // draw the scene
    DrawGLScene;
  end;
  TerminateApplication;
end.


