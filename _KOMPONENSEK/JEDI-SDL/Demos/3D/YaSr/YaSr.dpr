program YASR; // Yet another stupid Renderer
{******************************************************************}
{                                                                  }
{       Yet Another BSP Stupid Renderer                            }
{                                                                  }
{ Portions copyright by Christian Hackbart <chackbart@SQC.de>      }
{ All Rights Reserved.                                             }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
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
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary for SDL & SDL_Image, in your path .                     }
{   The Latest SDL runtimes can be found on http://www.libsdl.org  }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{   It should be easy compileable under Kylix, but I did not found }
{   any time to test this. If you have suggestions, questions or   }
{   something else, you want to tell me, feel free to drop a mail. }
{  Quake 3 Converting Tools can be found at:                       }
{  http://nate.scuzzy.net/                                         }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{  March    04 2002 - CH : The first public version v 0.1.         }
{                                                                  }
{  March    06 2002 - DL : Slight change to make is similar to     }
{                          JEDI-SDL projects                       }
{                                                                  }
{******************************************************************}

{$DEFINE TEXTURE}

uses
  OpenGL12,
  Logger,
  SysUtils,
  BspFile,
{$IFDEF TEXTURE}
  SDL_Image,
{$ENDIF}
  SDL;


const
  TITLE = 'Christian Hackbart''s BSP Renderer using JEDI-SDL';

var
  // screen width, height, and bit depth
  SCREEN_WIDTH : Cardinal = 640;
  SCREEN_HEIGHT : Cardinal = 480;
  SCREEN_BPP : Cardinal = 16;
  Done : Boolean;
  event : TSDL_Event;
  videoFlags : Uint32;
  MouseStatus : UInt32;

  bsp : TBsp;

  CamPos : vec3f = ( x : 0; y : 0; z : 0 );
  CamAng : vec3f = ( x : 0; y : 0; z : 0 );
  CamDir : array[ 0..2 ] of vec3f;
  Wire : Boolean;
  // This is our SDL surface
  surface : PSDL_Surface;


procedure TerminateApplication;
begin
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;

var
  DisplayList, OutlineList : integer;
type
  TRenderType = ( rtTexture, rtLightmap );

procedure Render( render : TRenderType );
var
  i : integer;
begin
  if render = rtTexture then
  begin
    glEnableClientState( GL_VERTEX_ARRAY );
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );

    glVertexPointer( 3, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].pos );
    glTexCoordPointer( 2, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].tv );

    // Draw all of our faces
    for i := 0 to bsp.numFaces - 1 do
    begin
      glBindTexture( GL_TEXTURE_2D, bsp.textures[ bsp.faces[ i ].id ].id );
      glDrawArrays( GL_TRIANGLE_FAN, bsp.faces[ i ].start, bsp.faces[ i ].num );
    end;

    glDisableClientState( GL_TEXTURE_COORD_ARRAY );
    glDisableClientState( GL_VERTEX_ARRAY );
  end
  else
  begin
    glEnableClientState( GL_VERTEX_ARRAY );
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );

    glVertexPointer( 3, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].pos );
    glTexCoordPointer( 2, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].lv );

    // Draw all of our faces
    for i := 0 to bsp.numfaces - 1 do
    begin
      glBindTexture( GL_TEXTURE_2D, bsp.lightmaps[ bsp.faces[ i ].lid ].id );
      glDrawArrays( GL_TRIANGLE_FAN, bsp.faces[ i ].start, bsp.faces[ i ].num );
    end;

    glDisableClientState( GL_TEXTURE_COORD_ARRAY );
    glDisableClientState( GL_VERTEX_ARRAY );
  end;
end;

procedure DrawGLScene( Wire : Boolean = false );
var
  i : GLInt;
  mat : array[ 0..15 ] of glfloat;
begin
  // camera orientation

  glRotatef( -camAng.z, 0, 0, 1 ); // roll
  glRotatef( -camAng.x, 1, 0, 0 ); // pitch
  glRotatef( -camAng.y, 0, 1, 0 ); // yaw
  glTranslatef( -camPos.x, -camPos.y, -camPos.z );

  // -- get resulting matrix and extract view vectors
  glGetFloatv( GL_MODELVIEW_MATRIX, @mat );

  for i := 0 to 2 do
  begin
    camDir[ i ].x := mat[ i + 0 ];
    camDir[ i ].y := mat[ i + 4 ];
    camDir[ i ].z := mat[ i + 8 ];
  end;

  // clear screen
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
  glEnable( GL_DEPTH_TEST );
  glEnable( GL_CULL_FACE );

  if DisplayList = 0 then // render new Display
  begin
    DisplayList := glGenLists( 1 );
    glNewList( DisplayList, GL_COMPILE );

    glColor3f( 1, 1, 1 );
    glEnable( GL_DEPTH_TEST );
    glDepthFunc( GL_LESS );

    glEnable( GL_TEXTURE_2D );
    glEnable( GL_BLEND );

    // First pass, lightmaps
    glBlendFunc( GL_ONE, GL_ZERO );
    Render( rtLightmap );

{$IFDEF TEXTURE}

    glDepthFunc( GL_EQUAL );
    glDepthMask( FALSE );

    //glBlendFunc( GL_DST_COLOR, GL_ONE ); //GL_ZERO
    glBlendFunc( GL_SRC_ALPHA, GL_ONE );
    Render( rtTexture );

    glDisable( GL_BLEND );
    glDepthFunc( GL_LESS );
    glDepthMask( TRUE );

{$ENDIF}

    glDisable( GL_TEXTURE_2D );

    glEndList;


    OutlineList := glGenLists( 1 );
    glNewList( OutlineList, GL_COMPILE );

    glEnableClientState( GL_VERTEX_ARRAY );
    glEnableClientState( GL_TEXTURE_COORD_ARRAY );

    glVertexPointer( 3, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].pos );
    glTexCoordPointer( 2, GL_FLOAT, sizeof( tvertex ), @bsp.vertices[ 0 ].tv );

    glDisable( GL_TEXTURE_2D );
    glColor3f( 0, 1, 0 );
    glDepthFunc( GL_LEQUAL );
    for i := 0 to bsp.NumFaces - 1 do
      glDrawArrays( GL_LINE_LOOP, bsp.faces[ i ].start, bsp.faces[ i ].num );
    glEndList;
  end;

  glCallList( DisplayList );

  if Wire then glCallList( OutlineList );
  SDL_GL_SwapBuffers;
end;

function FindFile( const Filename : string ) : string; // Lame :)
var
  sr : TSearchrec;
begin
  FindFirst( Filename + '.*', faAnyFile, sr );
  Result := Filename + ExtractFileExt( sr.Name );
  FindClose( sr );
end;

{$IFDEF TEXTURE}
procedure LoadImage( var Texture : TTexture; Alpha : Byte = 0 );
var
  TextureImage : PSDL_Surface;
  FileName : string;
begin
  FileName := FindFile( 'images/' + texture.name );
  TextureImage := IMG_Load( PChar( Filename ) );
  if ( TextureImage <> nil ) then
  begin
    try
      SDL_SetAlpha( TextureImage, SDL_SRCALPHA, Alpha );
      glGenTextures( 1, @texture.id );

      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );

      glBindTexture( GL_TEXTURE_2D, texture.id );

      // Generate The Texture
      if (TextureImage.format.BytesPerPixel = 3) then
      begin
        {glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w,
        TextureImage.h, 0, GL_BGR,
        GL_UNSIGNED_BYTE, TextureImage.pixels );}
        gluBuild2DMipmaps( GL_TEXTURE_2D, 3, TextureImage.w, TextureImage.h, GL_BGR, GL_UNSIGNED_BYTE, TextureImage.pixels );
      end
      else
      begin
        {glTexImage2D( GL_TEXTURE_2D, 0, GL_BGRA, TextureImage.w,
        TextureImage.h, 0, GL_BGRA,
        GL_UNSIGNED_BYTE, TextureImage.pixels ); }
        gluBuild2DMipmaps( GL_TEXTURE_2D, GL_BGRA, TextureImage.w, TextureImage.h, GL_BGRA, GL_UNSIGNED_BYTE, TextureImage.pixels );
      end;
    finally
    // Free up any memory we may have used
      SDL_FreeSurface( TextureImage );
    end;
  end
  else
    log.LogError( 'Texture ' + filename + ' not found...', 'Loadtexture' );
end;
{$ENDIF}

procedure LoadTextures;
var
  i : integer;
begin
  glEnable( GL_TEXTURE_2D );
  for i := 0 to bsp.numlightmaps - 1 do
  begin
    glGenTextures( 1, @bsp.lightmaps[ i ].id );
    glBindTexture( GL_TEXTURE_2D, bsp.lightmaps[ i ].id );

    glPixelStorei( GL_UNPACK_ALIGNMENT, 1 );

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

    glTexImage2D( GL_TEXTURE_2D, 0, 3, 128, 128, 0,
      GL_RGB, GL_UNSIGNED_BYTE, @bsp.lightmaps[ i ].data );
  end;
{$IFDEF TEXTURE}
  for i := 0 to bsp.NumTextures - 1 do
    loadimage( bsp.textures[ i ] );
{$ENDIF}
end;

function ResizeWindow( width : integer; height : integer ) : Boolean;
begin
  // Protect against a divide by zero
  if ( height = 0 ) then height := 1;
  // Setup our viewport.
  glViewport( 0, 0, width, height );

  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our perspective
  gluPerspective( 60.0, WIDTH / HEIGHT, 1.0, 4096.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );

  // Reset The View
  glLoadIdentity;

  {if OutLineList<>0 then glDeleteLists (OutLineList, 1);
  if DisplayList<>0 then glDeleteLists (DisplayList, 1);}

  result := true;
end;

procedure WindowProc;
const
  speed = 1; //0.0;

begin
  while ( SDL_PollEvent( @event ) = 1 ) do
  begin
    case event.type_ of
      SDL_QUITEV : Done := true;
      SDL_VIDEORESIZE :
        with event.resize do
        begin
          SCREEN_WIDTH := w;
          if h = 0 then
            h := 1;
          SCREEN_HEIGHT := h;
          SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_WIDTH, 0, videoFlags );
          ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );
        end;
      SDL_MOUSEBUTTONDOWN :
        begin
          if event.button.state = 1 then Inc( MouseStatus );
          if event.button.state = 3 then Inc( MouseStatus, 2 );
        end;
      SDL_MOUSEBUTTONUP :
        begin
          if event.button.state = 1 then MouseStatus := MouseStatus and 2;
          if event.button.state = 3 then MouseStatus := MouseStatus and 1;
        end;
      SDL_MOUSEMOTION :
        begin
          if MouseStatus = 0 then Exit;
        end;

      // Keys
      SDL_KEYDOWN :
        case event.key.keysym.sym of
          SDLK_ESCAPE :
            Done := true;
          SDLK_A :
            begin
              camPos.x := camPos.x - camDir[ 2 ].x * speed;
              camPos.y := camPos.y - camDir[ 2 ].y * speed;
              camPos.z := camPos.z - camDir[ 2 ].z * speed;
            end;
          SDLK_Z :
            begin
              camPos.x := camPos.x + camDir[ 2 ].x * speed;
              camPos.y := camPos.y + camDir[ 2 ].y * speed;
              camPos.z := camPos.z + camDir[ 2 ].z * speed;
            end;
          SDLK_RIGHT : camAng.y := camAng.y - speed;
          SDLK_LEFT : camAng.y := camAng.y + speed;

          SDLK_UP : camPos.y := camPos.y + speed;
          SDLK_DOWN : camPos.y := camPos.y - speed;
          SDLK_SPACE :
            begin
              FillChar( CamPos, sizeof( CamPos ), 0 );
              FillChar( CamAng, sizeof( CamAng ), 0 );
            end;
          SDLK_W : Wire := not Wire;
        end;
    end;
  end;
end;

var
  videoInfo : PSDL_VideoInfo;
  i : Byte;
  LevelName : string;
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

  // Make sure we are ready for OpenGL
  videoFlags := SDL_OPENGL;
  
  // Parse Parameters
  for i := 1 to ParamCount - 1 do
  begin
    if ( ParamStr( i ) = '-fullscreen' ) then
    begin
      videoFlags := videoFlags or SDL_FULLSCREEN // Enable OpenGL in FullScreen
    end
    else if ( ParamStr( i ) = '-level' ) then
    begin
      LevelName := ParamStr( i + 1 );
    end;
  end;
  
  if LevelName = '' then
  begin
    Log.LogError( 'Level Name must be specified. Usage : YASR -fullscreen -level levelname.bsp',
    'Main' );
    TerminateApplication;
  end;


  videoFlags := videoFlags or SDL_DOUBLEBUF; // Enable double buffering
  videoFlags := videoFlags or SDL_HWPALETTE; // Store the palette in hardware

  // This checks to see if surfaces can be stored in memory
  if ( ( videoInfo.hw_available and 1 ) = 1 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;

  // This checks if hardware blits can be done * /
  if ( ( videoInfo.blit_hw and 2 ) = 2 ) then
    videoFlags := videoFlags or SDL_HWACCEL;

  // Set the OpenGL Attributes
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it
  SDL_WM_SetCaption( TITLE, nil );

  surface := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, videoflags );
  if ( surface = nil ) then
  begin
    Log.LogError( Format( 'Unable to create OpenGL screen : %s', [ SDL_GetError ]
      ),
      'Main' );
    TerminateApplication;
  end;

  BSP := TBSP.Create;
  BSP.LoadFromFile( 'levels/' + LevelName ); //t8dm6

  DisplayList := 0;
  OutLineList := 0;
  ReSizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );
  LoadTextures;

  Done := False;

  while ( not Done ) do
  begin
    DrawGLScene( Wire );
    WindowProc;
  end;
  BSP.Free;
  glDeleteLists( OutLineList, 1 );
  glDeleteLists( DisplayList, 1 );
  TerminateApplication;
end.

