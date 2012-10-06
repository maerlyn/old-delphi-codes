unit DTT_Game;

interface

uses
  DTT_HUD;

procedure DTT_Init(Name, Server, MapName: String);
procedure DTT_Clean;
procedure HandleInput;

var
  Running: Boolean;
  HUD: DTTHUD;

implementation

uses
  SDL, SysUtils, OpenGL12, TNT_3D, DTT_Input, TNT_Skybox, TNT_Vector,
  TNT_Landscape, DTT_Client, DTT_Truck, DTT_Rocket, DTT_Console, TNT_Entity,
  DTT_Explosion, DTT_RocketTrail, DTT_Sound;

var
  Music: TMusic;

procedure DTT_Init(Name, Server, MapName: String);
begin
  ConsoleReset;

  Scene.LoadModel('truck_green.tnt');
  Scene.LoadModel('truck_blue.tnt');
  Scene.LoadModel('truck_red.tnt');
  Scene.LoadModel('truck_yellow.tnt');
  Scene.LoadModel('missile.tnt');
  Scene.LoadFromFile(MapName);

  InitTrucks;
  InitTrails;
  InitExplosions;

  MissileSound := TSample.Create('missile.wav');
  ExplosionSound := TSample.Create('explosion.wav');
  Beep := TSample.Create('beep.wav');
  Music := TMusic.Create('music.s3m');
  Music.Play;

  HUD := DTTHUD.Create;

  Camera.Position.y := 17;
  Camera.Orientation := Vector(0,0,0);

  Client := TClient.Login(Name, Server);
  Camera.Follow(Client.PlayerTruck, 30);

  ExitFlag := False;
  Running := True;
end;

procedure HandleInput;
begin
  with Input, Client.PlayerTruck, Keyboard do
  begin
    if Running then
    begin
      if Key[SDLK_LEFT] or Key[SDLK_4] then Turn(1);
      if Key[SDLK_RIGHT] or Key[SDLK_6] then Turn(-1);
      if Key[SDLK_UP] or Key[SDLK_8] then Accelerate;
      if Key[SDLK_DOWN] or Key[SDLK_5] then Reverse;
      if Key[SDLK_a] or Key[SDLK_b] then CollisionOK := False;
      case NewKey of
        SDLK_TAB: Fire;
      end;
    end;
  end;
  ConsoleInput;
  Input.Keyboard.NewKey := 0;
end;

procedure DTT_Clean;
begin
  Running := False;

  Client.Logout;

  Camera.Release;

  FlushTrails;
  FlushTrucks;
  FlushExplosions;

  Music.Free;
  MissileSound.Free;
  ExplosionSound.Free;
  Beep.Free;

  HUD.Free;

  Scene.FlushEntities([TYPE_LIGHT, TYPE_SKYBOX, TYPE_LANDSCAPE,
                       TYPE_OBJECT, TYPE_PARTICLES]);
  Scene.FlushModels;
end;

end.

