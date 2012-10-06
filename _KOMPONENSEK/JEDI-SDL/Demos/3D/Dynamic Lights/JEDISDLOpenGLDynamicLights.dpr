program JEDISDLOpenGLDynamicLights;
{******************************************************************}
{                                                                  }
{                   Dynamic Lights Demo                            }
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
{   May    11 2002 - PF : Update to make light maps lighter.       }
{                                                                  }
{******************************************************************}

uses
  OpenGL12,
  SysUtils,
  Logger,
  SDL,
  Vectors in '../Utils/Vectors.pas',
  Math;

type
  { RGB Color Triplet }
  TColor = array[0..2] of TGLFloat;
  { Light source }
  TLight = record
    Pos: T3DVector; { Position }
    Color: TColor; { Color }
  end;

  { Object Face }
  TFace = record
    Normal: T3DVector; { Normal }
    Vertices: array[0..3] of T3DVector; { Vertices }
  end;

const
  // screen width, height, and bit depth
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;
  APP_TITLE = 'Phil Freeman''s OpenGL Dynamic Lights Demo using JEDI-SDL';

  MapSize = 32; { Lightmap size }
  NumLights = 3; { Number of lights }
  Ambience: Single = 0.2; { Scene ambience }
  MinLight: Single = 0.2; { Inner circle of light }
  MaxLight: Single = 1.0; { Outer circle of light }
  
var
  // This is our SDL surface
  surface: PSDL_Surface;
  
  TexID : TGLuInt; { Texture ID }
  LightMapID : TGLuInt; { LightMap ID }
  Faces: array[0..2] of TFace; { Object Faces }
  Lights: array[0..NumLights-1] of TLight; { Light Sources }
  
  //Status indicator
  Status: Boolean = false;

procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt(0);
end;
// Load Bitmaps And Convert To Textures

function LoadGLTexture(fileName: string; var TexID: Cardinal): Boolean;
var
  // Create storage space for the texture
  TextureImage: PSDL_Surface;
begin
  // Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
  TextureImage := SDL_LoadBMP(PChar(fileName));
  if (TextureImage <> nil) then
  begin
    // Set the status to true
    Status := true;
    // Delete any existing texture
    glDeleteTextures(1, @TexID);
    // Create Texture
    glGenTextures(1, @TexID);
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture(GL_TEXTURE_2D, TexID);
    // Linear Filtering
    // scale linearly when image bigger than texture
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    // scale linearly when image smaller than texture
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    // Generate The Texture
    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
     TextureImage.h, 0, GL_BGR,
     GL_UNSIGNED_BYTE, TextureImage.pixels );
    {gluBuild2DMipmaps(GL_TEXTURE_2D, 3, TextureImage.w, TextureImage.h, GL_BGR,
      GL_UNSIGNED_BYTE, TextureImage.pixels);}
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

// A general OpenGL initialization function.  Sets all of the initial parameters.
// We call this right after our OpenGL window is created.

function InitGL: Boolean;
begin
  glPointSize(4);
  // Load in the texture
  if (not LoadGLTexture('images/texture.bmp', TexID )) then
  begin
    result := false;
    exit;
  end;
  // Enable Texture Mapping
  glEnable(GL_TEXTURE_2D);
  // Create a second Texture
  glGenTextures(1, @LightMapID );
  // Select the correct blend equation
  //glEnable( GL_BLEND );
  { Set the blend function for light mapping }
  glBlendFunc(GL_DST_COLOR, GL_SRC_COLOR);
  // Enable smooth shading
  glShadeModel(GL_SMOOTH);
  // Set the background black
  glClearColor(0.0, 0.0, 0.0, 0.0);
  // Depth buffer setup
  glClearDepth(1.0);
  // Enables Depth Testing
  glEnable(GL_DEPTH_TEST);
  // The Type Of Depth Test To Do
  glDepthFunc(GL_LEQUAL);
  // Really Nice Perspective Calculations
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  { Initialize the lights }
  Lights[0].Color[0] := 1.0;
  Lights[0].Color[1] := 0.0;
  Lights[0].Color[2] := 0.0;

  Lights[1].Color[0] := 0.0;
  Lights[1].Color[1] := 1.0;
  Lights[1].Color[2] := 0.0;

  Lights[2].Color[0] := 0.0;
  Lights[2].Color[1] := 0.0;
  Lights[2].Color[2] := 1.0;

  { Initialize the faces }
  Faces[0].Normal := Vector(0.0, -1.0, 0.0);
  Faces[0].Vertices[0] := Vector( 0.5, -0.5, -0.5);
  Faces[0].Vertices[1] := Vector(-0.5, -0.5, -0.5);
  Faces[0].Vertices[2] := Vector(-0.5, -0.5,  0.5);
  Faces[0].Vertices[3] := Vector( 0.5, -0.5,  0.5);

  Faces[1].Normal := Vector(0.0, 0.0, -1.0);
  Faces[1].Vertices[0] := Vector(-0.5,  0.5, -0.5);
  Faces[1].Vertices[1] := Vector(-0.5, -0.5, -0.5);
  Faces[1].Vertices[2] := Vector( 0.5, -0.5, -0.5);
  Faces[1].Vertices[3] := Vector( 0.5,  0.5, -0.5);

  Faces[2].Normal := Vector(1.0, 0.0, 0.0);
  Faces[2].Vertices[0] := Vector( 0.5,  0.5, -0.5);
  Faces[2].Vertices[1] := Vector( 0.5, -0.5, -0.5);
  Faces[2].Vertices[2] := Vector( 0.5, -0.5,  0.5);
  Faces[2].Vertices[3] := Vector( 0.5,  0.5,  0.5);

  result := true;
end;
{ Interpolate to find the position vector
  of any point on the surface of a quad }

function InterpolateFace(Face: TFace; u, v: Single): T3DVector;
begin
  Result := VectorInterpolate(
    VectorInterpolate(Face.Vertices[0],
    Face.Vertices[1], v),
    VectorInterpolate(Face.Vertices[3],
    Face.Vertices[2], v), u
    );
end;

{ Render the specified face }

procedure RenderFace(Face: TFace);
begin
  glBegin(GL_QUADS);
    glNormal3fv(@Face.Normal);
    glTexCoord2f(0.0, 0.0);
    glVertex3fv(@Face.Vertices[0]);
    glTexCoord2f(0.0, 1.0);
    glVertex3fv(@Face.Vertices[1]);
    glTexCoord2f(1.0, 1.0);
    glVertex3fv(@Face.Vertices[2]);
    glTexCoord2f(1.0, 0.0);
    glVertex3fv(@Face.Vertices[3]);
  glEnd;
end;

{ Shade a vertex using the specified light source }

function ShadeVertex(Light: TLight; Pos,
  Nrm: T3DVector): Single;
var
  Len: Single;             { Distance from the light source }
  Tmp: T3DVector;
  Angle: Single;           { Angle made with the light source }
  Distance: Single;        { Clamped distance from the light }
begin
  { Get the distance from the point to the
    light source }
  Tmp := VectorSubtract(Pos, Light.Pos);
  Len := VectorLength(Tmp);

  { Calculate the angle between the normal and
    the light source }
  Angle := 1.0 - ArcCos(VectorDotProduct(Tmp, Nrm) /
    Len / VectorLength(Nrm)) / Pi;

  { Clamp the distance to the light source }
  if Len < MinLight then
    Distance := 1.0 else
    if Len > MaxLight then
      Distance := 0.0 else
      Distance := (Len - MaxLight) /
        (MinLight - MaxLight);

  { Return the lighting factor }
  Result := Angle * Distance * 255;
end;

{ Create a texture for the specified face }

procedure CreateTexture(Face: TFace);
var
  Dst: PByte;
  Pos: T3DVector;                     { Interpolated position vector }
  Color: Single;                    { Lighting factor }
  I, J, K: Integer;
  R, G, B: Integer;                 { Temporary RGB values }
  Lightmap: Pointer;                { Lightmap data }
begin
  { Assign texture memory }
  GetMem(Lightmap, MapSize * MapSize * 3);
  Dst := Lightmap;

  for I := 0 to MapSize - 1 do
    for J := 0 to MapSize - 1 do begin

      { Clear the color to black }
      R := 0; G := 0; B := 0;

      for K := 0 to NumLights - 1 do begin
        { Calculate the position vector }
        Pos := InterpolateFace(Face, I / MapSize,
          J / MapSize);

        { Calculate the lighting factor }
        Color := ShadeVertex(Lights[K], Pos,
          Face.Normal);

        { Add the color values to the lightmap }
        Inc(R, Round(Color * Lights[K].Color[0]));
        Inc(G, Round(Color * Lights[K].Color[1]));
        Inc(B, Round(Color * Lights[K].Color[2]));
      end;

      { Clamp large values }
      if R > 255 then R := 255;
      if G > 255 then G := 255;
      if B > 255 then B := 255;

      { Adjust for ambience and set the lightmap values }
      Dst^ := Round(R * (1.0 - Ambience) +
        Ambience * 255);
      Inc(Dst);
      Dst^ := Round(G * (1.0 - Ambience) +
        Ambience * 255);
      Inc(Dst);
      Dst^ := Round(B * (1.0 - Ambience) +
        Ambience * 255);
      Inc(Dst);
    end;

  { Build the lightmap }
  glBindTexture(GL_TEXTURE_2D, LightmapID);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gluBuild2DMipmaps(GL_TEXTURE_2D, 3, MapSize, MapSize, GL_RGB,
    GL_UNSIGNED_BYTE, Lightmap);

  { Free texture memory }
  FreeMem(Lightmap);
end;

// The main drawing function.

procedure DrawGLScene;
var
  I: Integer;
  Time: Single;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  glTranslatef(0.0, 0.0, -1.5);
  glRotatef(30, 0.0, 1.0, 0.0);

  { Disable blending and texturing }
  glDisable(GL_BLEND);
  glDisable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, 0);

  { Get the current time }
  Time := SDL_GetTicks / 1000.0;

  for I := 0 to NumLights - 1 do
    with Lights[I] do begin
      { Calculate the position of the light source }
      Pos.X := Sin(Time * (I + 1) * 0.7) * 0.45;
      Pos.Y := Sin(Time * (I + 1) * 0.8) * 0.45;
      Pos.Z := Sin(Time * (I + 1) * 0.9) * 0.45;

      { Render a sphere to represent the light }
      glColor3fv(@Color);
      glPushMatrix;
      glBegin(GL_POINTS);
      glVertex3f(Pos.X, Pos.Y, Pos.Z);
      glEnd;
      glPopMatrix;
    end;

  { Enable texturing and use the
    normal color values }
  glEnable(GL_TEXTURE_2D);
  glColor3f(1.0, 1.0, 1.0);

  for I := 0 to High(Faces) do begin
    { Render the face as normal }
    glDisable(GL_BLEND);
    glBindTexture(GL_TEXTURE_2D, TexID);
    RenderFace(Faces[I]);

    { Calculate the lightmap, enable blending and
      render the lightmap }
    glEnable(GL_BLEND);
    CreateTexture(Faces[I]);
    RenderFace(Faces[I]);
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


