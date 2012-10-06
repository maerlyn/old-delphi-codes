program ParticalTest;
{****************************************************************************

 <!!!> ParticleEngine is one of the models of the OpenGL engine.
 <!!!> This is a *development* version of the ParticleEngine, NOT FINAL.
 <!!!> NOT for distribution.

     The contents of this file are subject to the Mozilla Public License
     Version 1.1 (the "License"); you may not use this file except in
     compliance with the License. You may obtain a copy of the License at
     http://www.mozilla.org/MPL/

     Software distributed under the License is distributed on an "AS IS"
     basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
     License for the specific language governing rights and limitations
     under the License.

     The Initial Developer of the Original Code is Ariel Jacob.
     Portions created by Ariel Jacob are Copyright (C) 2002 Ariel Jacob.

     Contributor(s):
       Ariel Jacob;               email: ariel@global-rd.com
       Kisnémeth Róbert.
       (you name might be here)
******************************************************************************}

uses
  SysUtils,
  SDL,
  OpenGL12,
  Logger,
  ParticleEngine,
  Vectors in '../Utils/Vectors.pas',
  SDL_General;

type
  TGLineParticles = class( TParticleGroup )
    G : Single;
    procedure Move; override;
    constructor Create;
  end;

  TBoomParticles = class( TParticleGroup )
    constructor Create( nParticles : cardinal );
  end;

  TSphereParticles = class( TParticleGroup )
    constructor Create( nParticles : cardinal );
  end;

  // Overrides the move procedure, to allow continues particles 'rain'
  // (can't die)
  TRainParticles = class( TParticleGroup )
    G : Single;
    procedure SetParticle( num : cardinal );
    procedure Move; override;
    constructor Create( nParticles : cardinal );
  end;

  // using the Done event
  TNothingToDoParticles = class( TParticleGroup )
    constructor Create( nParticles : cardinal );
  end;

const
  RainGroupID = 10;
  AppTitle = 'Partical engine Test - press [a],[s],[d],[f/r],[g] - Ariel Jacob';

var
  Screen : PSDL_Surface = nil;
  ParticleSystem : TParticleSystem = nil;
  texture1, texture2 : TGLUInt;
  videoFlags : Cardinal;
  ResizeW : Cardinal = 640;
  ResizeH : Cardinal = 480;

  CamPos : T3DVector = ( x : 0; y : 0; z : 0 );
  CamAng : T3DVector = ( x : 0; y : 0; z : 0 );

  // example for creating new types of Particles styles
  { -- TRainParticles ----------------------------------------------------------- }

constructor TGLineParticles.Create;
var
  I : byte;
  S, P : T3DVector;
begin
  inherited create( 30, Texture1 );
  G := 0;
  for I := 0 to 29 do
  begin
    P.X := I - 15;
    P.Y := 15 + I * 0.25;
    P.Z := -45;
    S.X := 0;
    S.Y := 0;
    S.Z := 0;
    AddParticle( 1, 1, 0, P, S, 4, 0.05 );
  end;
end;

procedure TGLineParticles.Move;
var
  I : byte;
begin
  inherited;
  for I := 0 to 29 do
  begin
    Particles[ I ].Position.Y := Particles[ I ].Position.Y + G;
  end;
  G := G - 0.0098;
end;

{ -- TRainParticles ----------------------------------------------------------- }

procedure TRainParticles.SetParticle( num : cardinal );
const
  //size = 50;
  fade = 0.05;
var
  Distance : Single;
begin
  with Particles[ num ] do
  begin
    Distance := size / NumberOfParticles;
    Position := Vectors.Vector( ( num * Distance - ( size / 2 ) ), ( 17 + ( Random( 100 ) + 1 ) /
      50 ) , -45 );
    Vector := Vectors.Vector( 0, 0, 0 );
    Life := 4 + Random( 2 );
    fadeSpeed := fade;
    r := 0;
    g := ( random( 100 ) + 1 ) / 200;
    b := 1;
  end;
end;

constructor TRainParticles.Create( nParticles : cardinal );
var
  I : cardinal;
begin
  inherited create( nParticles, Texture1 );
  NumberOfParticles := nParticles;
  G := -0.0098;
  ID := RainGroupID;
  for I := 0 to NumberOfParticles - 1 do
  begin
    SetParticle( I );
  end;
end;

procedure TRainParticles.Move;
var
  I : cardinal;
begin
  I := 0;
  while I < NumberOfParticles do
  begin
    with Particles[ I ] do
    begin
      if life > 0 then
      begin
        Position.Y := Position.Y - G;
        ;
        life := life - fadeSpeed;
      end
      else
        SetParticle( I );
    end;
    Inc( I );
  end;
  //  G:= G-0.0098;
end;

{ -- TBoomParticles ----------------------------------------------------------- }

constructor TBoomParticles.Create( nParticles : cardinal );
var
  I : cardinal;
  S, P : T3DVector;
begin
  inherited create( nParticles, Texture2 );
  for I := 0 to nParticles - 1 do
  begin
    P.X := 0;
    P.Y := 0;
    P.Z := -50;
    S.X := ( random( 5 ) - 2.5 ) / 45;
    S.Y := ( random( 5 ) - 2.5 ) / 45;
    S.Z := ( random( 2 ) + 1 ) / 5; //random(60) mod 60  - 32.0;

    AddParticle(
      ( random( 100 ) + 1 ) / 100, //r
      ( random( 100 ) + 1 ) / 100, //g
      ( random( 100 ) + 1 ) / 100, //b
      P, S, 1, ( random( 10 ) mod 100 ) / 1000.0 + 0.003 );
  end;
end;

{ -- TSphereParticles ----------------------------------------------------------- }

constructor TSphereParticles.Create( nParticles : cardinal );
const
  R = 5;
  Life = 1;
  Fade = 0.05;
var
  I : cardinal;
  S, P : T3DVector;
  A, B, C : cardinal;
  angle : Single;
begin
  inherited create( nParticles, Texture1 );
  A := nParticles shr 3;
  B := A shl 1;
  C := A shl 2;
  angle := 360 / A;
  for I := 0 to A - 1 do
  begin
    P.X := 0;
    P.Y := 0;
    P.Z := -50;
    S.X := sin( I * angle ) / R * 3;
    S.Y := cos( I * angle ) / R * 3;
    S.Z := 0;

    AddParticle(
      1, //r
      0, //g
      ( random( 100 ) + 1 ) / 100, //b
      P, S, Life * 2, Fade );
  end;
  angle := 360 / B;
  for I := 0 to B - 1 do
  begin
    P.X := 0;
    P.Y := 0;
    P.Z := -50;
    S.X := sin( I * angle ) / R * 2;
    S.Y := cos( I * angle ) / R * 2;
    S.Z := 0;

    AddParticle(
      0, //r
      0.3, //g
      1, //b
      P, S, Life, Fade / 2 );
  end;
  angle := 360 / C;
  for I := 0 to C - 1 do
  begin
    P.X := 0;
    P.Y := 0;
    P.Z := -50;
    S.X := sin( I * angle ) / R;
    S.Y := cos( I * angle ) / R;
    S.Z := 0;

    AddParticle(
      1, //r
      1, //g
      0, //b
      P, S, Life * 2, Fade );
  end;
end;

{ -- TNothingToDoParticles ---------------------------------------------------- }

constructor TNothingToDoParticles.Create( nParticles : cardinal );
const
  R = 10;
  Life = 5.5;
  Fade = 0.25;
var
  I : cardinal;
  S, P : T3DVector;
  angle : Single;
begin
  inherited create( nParticles, Texture1 );
  angle := 360 / nParticles;
  for I := 0 to nParticles - 1 do
  begin
    P.X := sin( I * angle ) / R;
    P.Y := cos( I * angle ) / R;
    P.Z := -50;
    S.X := 0;
    S.Y := 0;
    S.Z := 1;

    AddParticle(
      ( random( 100 ) + 1 ) / 100, //r
      ( random( 100 ) + 1 ) / 50, //g
      ( random( 100 ) + 1 ) / 10, //b
      P, S, Life * 2, Fade );
  end;
end;

{ -- LoadGLTextures ----------------------------------------------------------- }

function LoadGLTextures( var TextureP : TGLUInt; image : string ) : boolean;
var
  Status : boolean;
  TextureImage : PSDL_Surface; // Create Storage Space For The Textures
begin
  Status := FALSE;

  TextureImage := SDL_LoadBMP( PChar( 'Data/' + image ) );

  if ( TextureImage <> nil ) then
  begin
    Status := TRUE;
    // Create Texture
    glGenTextures( 1, @TextureP );
    // Typical Texture Generation Using Data From The Bitmap
    glBindTexture( GL_TEXTURE_2D, TextureP );

    // Generate The Texture
    gluBuild2DMipmaps( GL_TEXTURE_2D, 3, TextureImage.w,
      TextureImage.h, GL_BGR,
      GL_UNSIGNED_BYTE, TextureImage.pixels );

    // Linear Filtering
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
      GL_LINEAR_MIPMAP_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
      GL_LINEAR );

    // Generate The Texture
    glTexImage2D( GL_TEXTURE_2D, 0, 3, TextureImage.w, TextureImage.h,
      0, GL_BGR, GL_UNSIGNED_BYTE, TextureImage.pixels );

    // Linear Filtering
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  end;
  if TextureImage <> nil then
    SDL_FreeSurface( TextureImage );

  Result := Status; // Return The Status
end;

{ -- Finalize, Free&Exit ------------------------------------------------------ }
// !@# NOTE - I have no idea what are those commands.

procedure Finalize;
begin
  SDL_FreeSurface( Screen );
  glDeleteTextures( 1, @texture1 );
  glDeleteTextures( 1, @texture2 );
  ParticleSystem.Free;
  SDL_QUIT;
  UnLoadOpenGL;
  Halt( 0 );
end;

{ -- initGL, Load OGL --------------------------------------------------------- }
// !@# NOTE - I have no idea what are those commands.

function initGL : boolean;
begin
  // Enable smooth shading */
//  glShadeModel(GL_SMOOTH);

  // Set the background black */
  glClearColor( 0.0, 0.0, 0.0, 0.0 );

  // Depth buffer setup */
  glClearDepth( 1.0 );

  // Enables Depth Testing */
//  glDisable(GL_DEPTH_TEST);

  // Enable Blending */
  glEnable( GL_BLEND );
  // Type Of Blending To Perform */
  glBlendFunc( GL_SRC_ALPHA, GL_ONE );

  // singlely Nice Perspective Calculations */
//  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
  // singlely Nice Point Smoothing */
//  glHint(GL_POINT_SMOOTH_HINT, GL_NICEST);

  // ----------------- !@#
  glViewport( 0, 0, ResizeW, ResizeH );

  // change to the projection matrix and set our viewing volume.
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  // Set our perspective
  gluPerspective( 45.0, ResizeW / ResizeH, 0.5, 250.0 );

  // Make sure we're changing the model view and not the projection
  glMatrixMode( GL_MODELVIEW );

  // Reset The View
  glLoadIdentity;

  //----------------- !@#

  LoadGLTextures( texture1, 'Particle.bmp' );
  LoadGLTextures( texture2, 'Face.bmp' );
  // Enable Texture Mapping */
  glEnable( GL_TEXTURE_2D );

  result := True;
end;

{ -- Init, ALL ---------------------------------------------------------------- }

procedure Init( );
var
  videoInfo : PSDL_VideoInfo;
begin
  Randomize;
  // Load the appropriate .DLL or .SO
  LoadOpenGL;

  // initialize SDL
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    Log.LogWarning( Format( 'Video initialization failed: %s\n',
      [ SDL_GetError ] ), 'Start' );
    Halt( 1 );
  end;

  // Fetch the video info
  videoInfo := SDL_GetVideoInfo( );

  if videoInfo = nil then
  begin
    Log.LogWarning( Format( 'Video query failed: %s\n',
      [ SDL_GetError ] ), 'Start' );
    Halt( 1 );
  end;

  // the flags to pass to SDL_SetVideoMode
  // Enable OpenGL in SDL          */
  videoFlags := SDL_OPENGL;
  // Enable double buffering       */
  videoFlags := videoFlags or SDL_DOUBLEBUF;
  // Store the palette in hardware */
  videoFlags := videoFlags or SDL_HWPALETTE;
  // Enable window resizing        */
  videoFlags := videoFlags or SDL_RESIZABLE;

  // This checks to see if surfaces can be stored in memory
  if ( videoInfo.hw_available <> 0 ) then
    videoFlags := videoFlags or SDL_HWSURFACE
  else
    videoFlags := videoFlags or SDL_SWSURFACE;

  // This checks if hardware blits can be done * /
  if ( videoInfo.blit_hw <> 0 ) then
    videoFlags := videoFlags or SDL_HWACCEL;

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  // Sets up OpenGL double buffering */
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  // Set the title bar in environments that support it
  SDL_WM_SetCaption( AppTitle, nil ); //DL

  // get a SDL surface */
  Screen := SDL_SetVideoMode( ResizeW, ResizeH, 16,
    videoFlags );

  // Verify there is a surface */
  if Screen = nil then
  begin
    Log.LogWarning( Format( 'Video mode set failed: %s',
      [ SDL_GetError ] ), 'Main' );
    Finalize;
  end;
  // Enable key repeat */
  if SDL_EnableKeyRepeat( 100, SDL_DEFAULT_REPEAT_INTERVAL ) = -1 then
  begin
    Log.LogWarning( Format( 'Setting keyboard repeat failed: %s',
      [ SDL_GetError ] ), 'Main' );
    Finalize;
  end;

  // initialize OpenGL */
  if not initGL then
  begin
    Log.LogWarning( Format( 'Could not initialize OpenGL. %s',
      [ SDL_GetError ] ), 'Main' );
    Finalize;
  end;
  ParticleSystem := TParticleSystem.Create;
end;

procedure NothingToDoDone;
begin
  ParticleSystem.AddParticleGroup( TSphereParticles.Create( 100 * 8 ) );
end;

{ -- ParticleFun, Draw loop --------------------------------------------------- }

procedure ParticleFun( );
var
  Done : Boolean;
  event : TSDL_Event;
  FPSCalc : TFPSCalc;
  NewCount,
    OldCount : cardinal;

  MouseBnt : array[ 1..3 ] of Boolean;
  MouseSP : TPoint;
  mat : array[ 0..15 ] of glfloat;

begin
  Done := False;
  FPSCalc := TFPSCalc.Create( 1000 );
  OldCount := 0;

  MouseBnt[ 1 ] := False;
  MouseBnt[ 2 ] := False;
  MouseBnt[ 3 ] := False;

  ParticleSystem.AddParticleGroup( TBoomParticles.Create( 300 ) );
  ParticleSystem.AddParticleGroup( TGLineParticles.Create );

  while ( not Done ) do
  begin

    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case ( event.type_ ) of
        SDL_VIDEORESIZE :
          begin
            ResizeW := event.resize.w;
            ResizeH := event.resize.h;
            if ResizeH = 0 then
              ResizeH := 1;
            SDL_SetVideoMode( ResizeW, ResizeH, 0, videoFlags );
            InitGL;
          end;

        SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP :
          begin
            MouseBnt[ event.button.button ] := event.button.state = 1;
            if MouseBnt[ event.button.button ] then
              SDL_GetMouseState( MouseSP.x, MouseSP.y );
          end;

        SDL_MOUSEMOTION :
          begin
            if MouseBnt[ 1 ] and MouseBnt[ 3 ] then
            begin
              // move froward depending on the cam view
              glGetFloatv( GL_MODELVIEW_MATRIX, @mat );

              if MouseSP.y > event.motion.y then
              begin
                camPos.X := camPos.X - ( mat[ 2 + 0 ] ) * 1.7;
                camPos.Y := camPos.Y - ( mat[ 2 + 4 ] ) * 1.7;
                camPos.Z := camPos.Z - ( mat[ 2 + 8 ] ) * 1.7;
              end
              else
              begin
                camPos.X := camPos.X + mat[ 2 + 0 ];
                camPos.Y := camPos.Y + mat[ 2 + 4 ];
                camPos.Z := camPos.Z + mat[ 2 + 8 ];
              end;

              MouseSP.x := event.motion.x;
              MouseSP.y := event.motion.y;
            end
            else
            begin
              if MouseBnt[ 1 ] then
              begin
                camAng.Y := camAng.Y + ( MouseSP.x - event.motion.x ) / 5;
                camAng.X := camAng.X + ( MouseSP.y - event.motion.y ) / 5;
                MouseSP.x := event.motion.x;
                MouseSP.y := event.motion.y;
              end;
              if MouseBnt[ 3 ] then
              begin
                camPos.Z := camPos.Z + ( MouseSP.y - event.motion.y );
                MouseSP.x := event.motion.x;
                MouseSP.y := event.motion.y;
              end;
            end;

          end;

        SDL_KEYDOWN :
          begin
            case event.key.keysym.sym of
              SDLK_ESCAPE :
                begin
                  done := True;
                end;
              SDLK_a :
                begin
                  ParticleSystem.AddParticleGroup( TBoomParticles.Create( 500 ) );
                end;
              SDLK_s :
                begin
                  ParticleSystem.AddParticleGroup( TGLineParticles.Create );
                end;
              SDLK_d :
                begin
                  ParticleSystem.AddParticleGroup( TSphereParticles.Create( 100 *
                    8 ) );
                end;
              SDLK_f :
                begin
                  ParticleSystem.AddParticleGroup( TRainParticles.Create( 100 ) );
                end;
              SDLK_r :
                begin
                  ParticleSystem.RemoveParticleGroup( RainGroupID );
                end;
              SDLK_g :
                begin
                  with ParticleSystem.AddParticleGroup(
                    TNothingToDoParticles.Create( 50 ) ) do
                  begin
                    OnGroupDone := @NothingToDoDone;
                  end;
                end;
            end;
          end;
        SDL_QUITEV :
          done := true;
      end;
    end;

    glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
    glLoadIdentity( );

    ParticleSystem.Move;

    // camera orientation
    glRotatef( -camAng.z, 0, 0, 1 );
    glRotatef( -camAng.y, 0, 1, 0 );
    glRotatef( -camAng.x, 1, 0, 0 );
    glTranslatef( -camPos.x, -camPos.y, -camPos.z );

    ParticleSystem.Draw;
    FPSCalc.Count;

    NewCount := ParticleSystem.ParticlesCount;
    if OldCount <> NewCount then
    begin
      SDL_WM_SetCaption( PChar( AppTitle + ' ' +
        IntToStr( NewCount ) + ' Particles' + ' FPS ' + // Average FPS
        IntToStr( FPSCalc.FPS ) ),
        nil );
      OldCount := NewCount;
    end;

    SDL_GL_SwapBuffers( );
  end;
  FPSCalc.Free;
end;

begin
  Init;
  ParticleFun; // if it works...
  Finalize;
end.

