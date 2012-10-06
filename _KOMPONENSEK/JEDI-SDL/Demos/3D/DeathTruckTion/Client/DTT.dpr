program DTT;

uses
  DTT_Truck in 'DTT_Truck.pas',
  DTT_Explosion in 'DTT_Explosion.pas',
  DTT_Game in 'DTT_Game.pas',
  DTT_GUI in 'DTT_GUI.pas',
  DTT_HUD in 'DTT_HUD.pas',     
  DTT_Input in 'DTT_Input.pas',
  DTT_Rocket in 'DTT_Rocket.pas',
  DTT_RocketTrail in 'DTT_RocketTrail.pas',
  DTT_Sound in 'DTT_Sound.pas',
  DTT_Console in 'DTT_Console.pas',
  TNT_Vector in '../TNT-3D/TNT_Vector.pas',
  TNT_Camera in '../TNT-3D/TNT_Camera.pas',
  TNT_Collisions in '../TNT-3D/TNT_Collisions.pas',
  TNT_Console in '../TNT-3D/TNT_Console.pas',
  TNT_Entity in '../TNT-3D/TNT_Entity.pas',
  TNT_File in '../TNT-3D/TNT_File.pas',
  TNT_Font in '../TNT-3D/TNT_Font.pas',
  TNT_Frustum in '../TNT-3D/TNT_Frustum.pas',
  TNT_GUI in '../TNT-3D/TNT_GUI.pas',
  TNT_HUD in '../TNT-3D/TNT_HUD.pas',
  TNT_Landscape in '../TNT-3D/TNT_Landscape.pas',
  TNT_Light in '../TNT-3D/TNT_Light.pas',
  TNT_Model in '../TNT-3D/TNT_Model.pas',
  TNT_Object in '../TNT-3D/TNT_Object.pas',
  TNT_ParticleSystem in '../TNT-3D/TNT_ParticleSystem.pas',
  TNT_Scene in '../TNT-3D/TNT_Scene.pas',
  TNT_Skybox in '../TNT-3D/TNT_Skybox.pas',
  TNT_Texture in '../TNT-3D/TNT_Texture.pas',
  TNT_Timer in '../TNT-3D/TNT_Timer.pas',
  TNT_3D in '../TNT-3D/TNT_3D.pas',
  DTT_Client in '../Network/DTT_Client.pas',
  DTT_Network in '../Network/DTT_Network.pas',
  Logger,
  OpenGL12,
  SDL;

{$R *.res}

const
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;
  GAME_TITLE = 'DeathTruckTion v1.1';

var
  Flags : Uint32 = SDL_OPENGL;
  Event : TSDL_Event;
  Joystick : PSDL_Joystick = nil;
  Joy, X, Y : Integer;
  DeBounce : Integer = 0;

procedure TerminateApplication( Error : string );
begin
  if Error <> '' then
    Log.LogError( Error, 'Main' );
  UnLoadOpenGL;
  SoundClose;
  SDL_Quit;
  Halt;
end;
var
  i : integer;
begin
  LoadOpenGL;
  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_JOYSTICK ) < 0 then
    TerminateApplication( 'Could not initialize SDL' );
  if not SoundInit then
    TerminateApplication( 'Could not initialize FMod' );
  SDL_WM_SetCaption( GAME_TITLE, nil );
  SDL_ShowCursor( SDL_DISABLE );
  SDL_EnableKeyRepeat( SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL );
  SDL_EnableUNICODE( SDL_ENABLE );
  Joystick := SDL_JoystickOpen( 0 );
  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 5 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 24 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );
  {case MessageDlg('Would You Like To Run In FullScreen Mode?', mtConfirmation,
    mbYesNoCancel, 0) of
    mrCancel: TerminateApplication('');
    mrYes: Flags := Flags or SDL_FULLSCREEN;
  end;}
  for i := 1 to ParamCount - 1 do
  begin
    if ( ParamStr( i ) = '-fullscreen' ) then
    begin
      Flags := Flags or SDL_FULLSCREEN;
    end
    else if ( ParamStr( i ) = '-hwaccel' ) then
    begin
      Flags := Flags or SDL_HWACCEL;
    end;
  end;
  if SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, Flags ) = nil
    then
    TerminateApplication( 'Unable to create OpenGL screen' );
  TNT := TNT3D.Create;
  TNT.Resize( SCREEN_WIDTH, SCREEN_HEIGHT );
  GUI := DTTGUI.Create;
  Done := False;
  Running := False;
  repeat
    TNT.Render;
    SDL_GL_SwapBuffers;
    Input.SetButton( SDL_GetMouseState( X, Y ) );
    Input.SetMouse( X, Y );
    if Joystick <> nil then
    begin
      Joy := SDL_JoystickGetAxis( Joystick, 0 );
      if Joy < -32000 then
        Input.SetKey( SDLK_4 )
      else
        Input.ResetKey( SDLK_4 );
      if Joy > 32000 then
        Input.SetKey( SDLK_6 )
      else
        Input.ResetKey( SDLK_6 );
      Joy := SDL_JoystickGetAxis( Joystick, 1 );
      if Joy < -32000 then
        Input.SetKey( SDLK_8 )
      else
        Input.ResetKey( SDLK_8 );
      if Joy > 32000 then
        Input.SetKey( SDLK_5 )
      else
        Input.ResetKey( SDLK_5 );
      if SDL_JoystickGetButton( Joystick, 0 ) = 1 then
      begin
        if DeBounce = 0 then
        begin
          Input.SetKey( SDLK_TAB );
          DeBounce := 6;
        end
        else if DeBounce > 0 then
          Dec( DeBounce );
      end
      else
        Input.ResetKey( SDLK_TAB );
      if SDL_JoystickGetButton( Joystick, 1 ) = 1 then
        Input.SetKey( SDLK_b )
      else
        Input.ResetKey( SDLK_b );
    end;
    if Running then
    begin
      Client.DoClient;
      HandleInput;
    end
    else
      GUI.guiInput;
    while SDL_PollEvent( @Event ) = 1 do
    begin
      case Event.type_ of
        SDL_QUITEV : Done := True;
        SDL_KEYDOWN :
          begin
            Input.SetKey( Event.key.keysym.sym );
            Input.SetChar( Char( Event.key.keysym.unicode ) );
          end;
        SDL_KEYUP : Input.ResetKey( Event.key.keysym.sym );
      end;
    end;
  until Done;
  TNT.Free;
  SDL_JoystickClose( Joystick );
  TerminateApplication( '' );
end.


