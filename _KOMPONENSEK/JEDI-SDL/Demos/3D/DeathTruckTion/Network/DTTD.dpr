program DTTD;

{$APPTYPE CONSOLE}

uses
  Classes,
  SysUtils,
  IdGlobal,
  IdBaseComponent,
  IdComponent,
  IdUDPBase,
  IdUDPServer,
  IdSocketHandle,
  DTT_Network in 'DTT_Network.pas',
  TNT_Vector in '../TNT-3D/TNT_Vector.pas',
  SDL;

{$R *.res}

const
  CONST_INERTIA = 1.07;

  COLOR_RED = 0;
  COLOR_GREEN = 1;
  COLOR_BLUE = 2;
  COLOR_YELLOW = 3;

type
  TServer = class(TIdUDPServer)
    constructor Create(axOwner: TComponent); override;
    procedure UDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
  end;

var
  Server: TServer;
  Ports: array [1..MAX_PLAYERS] of Integer;
  StartTable: array [1..MAX_PLAYERS] of TPlayerState;
//  Time: Uint32;

procedure Log(Text: String);
begin
  WriteLn(Text);
end;

constructor TServer.Create(axOwner: TComponent);
begin
  inherited;
  DefaultPort := SERVER_PORT;
  ThreadedEvent := True;
  OnUDPRead := UDPRead;
  Active := True;
end;

procedure TServer.UDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
var
  PlayerMsg: TPlayerMsg;
  PlayerState: TPlayerState;
  i: Integer;
//  dt: Uint32;
begin
{  dt := Time;
  Time := SDL_GetTicks;
  dt := Time - dt;

  for i:=1 to MAX_PLAYERS do
    with NetGame.Players[i] do
      if Name <> '' then
      begin
        A := A / CONST_INERTIA;
        V := V / CONST_INERTIA + A*dt;
        Position.x := Position.x - V*sin(T*(PI/180));
        Position.z := Position.z - V*cos(T*(PI/180));
      end;
}
  case AData.Size of
    SizeOf(TPlayerMsg):
      begin
        AData.ReadBuffer(PlayerMsg, SizeOf(TPlayerMsg));
        case PlayerMsg.Msg of

          MSG_JOIN:
            begin
              Log('Join: ' +  PlayerMsg.Name + ' (' + ABinding.PeerIP + ')');
              with NetGame do
              begin
                i := 1;
                while Players[i].Name <> '' do
                  Inc(i);
                Inc(NbPlayers);
                Players[i] := StartTable[i];
                Players[i].Name := PlayerMsg.Name;
                Players[i].ID := i;
                Ports[i] := PlayerMsg.Port;
              end;
              Server.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, NetGame, SizeOf(TGame));
            end;

          MSG_PART:
            begin
              Log('Part: ' +  PlayerMsg.Name + ' (' + ABinding.PeerIP + ')');
              with NetGame do
              begin
                i := 1;
                while Players[i].Name <> PlayerMsg.Name do
                  Inc(i);
                Dec(NbPlayers);
                Players[i].Name := '';
              end;
              Server.SendBuffer(ABinding.PeerIP, ABinding.PeerPort, NetGame, SizeOf(TGame));
            end;
        end;
      end;

    SizeOf(TPlayerState):
      begin
        AData.ReadBuffer(PlayerState, SizeOf(TPlayerState));
        NetGame.Players[PlayerState.ID] := PlayerState;
        Server.SendBuffer(ABinding.PeerIP, Ports[PlayerState.ID], NetGame, SizeOf(TGame));
      end;
  end;
end;

begin
  InitNetGame;
  with StartTable[1] do
  begin
    Name := '';
    Position := Vector(10,0,10);
    T := 225;
    Col := COLOR_RED;
//    V := 0;
//    A := 0;
    NbRocket := 0;
  end;

  with StartTable[2] do
  begin
    Name := '';
    Position := Vector(110,0,110);
    T := 225-180;
    Col := COLOR_GREEN;
//    V := 0;
//    A := 0;
    NbRocket := 0;
  end;

  with StartTable[3] do
  begin
    Name := '';
    Position := Vector(210,0,210);
    T := 225-90;
    Col := COLOR_BLUE;
//    V := 0;
//    A := 0;
    NbRocket := 0;
  end;

  with StartTable[4] do
  begin
    Name := '';
    Position := Vector(310,0,310);
    T := 225+90;
    Col := COLOR_YELLOW;
//    V := 0;
//    A := 0;
    NbRocket := 0;
  end;

  Server := TServer.Create(nil);
  Log('dttd: listening...');

  try
    repeat Sleep(Cardinal(-1)) until False;
  except
    on Exception do Server.Free;
  end;

  Log('dttd: stopped!');
end.

