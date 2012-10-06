unit DTT_Client;

// Gestion de la partie "Client" du reseau
// ---------------------------------------

interface

uses
  Classes, SysUtils, IdUDPClient, IdUDPServer, IdSocketHandle,
  DTT_Network, DTT_Truck, TNT_Timer;

type
  TClient = class(TIdUDPClient)
  private
    Listener: TIdUDPServer;
    ID: Integer;
    Timer: TTime;
    procedure SendGuaranted(Player: TPlayerMsg);
    procedure RemoveParted;
    procedure AddJoined;
    procedure UpdateTrucks;
    procedure UDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
  public
    PlayerTruck: TTruck;
    constructor Login(PlayerName, ServerHost: String);
    procedure DoClient;
    destructor Logout;
  end;

var
  Client: TClient;

implementation

uses
  TNT_3D, TNT_Vector;

var
  OldNetGame: TGame;

constructor TClient.Login(PlayerName, ServerHost: String);
var
  PlayerMsg: TPlayerMsg;
  ok: Boolean;
begin
  inherited Create(nil);
  Host := ServerHost;
  Port := SERVER_PORT;
  Console.Log(PlayerName + ' connected to ' + Host);

  Listener := TIdUDPServer.Create(Owner);
  with Listener do
  begin
    ThreadedEvent := True;
    OnUDPRead := UDPRead;
    DefaultPort := SERVER_PORT + 3;
    ok := True;
    repeat
      try                  // marche pas ??!
        Active := True;
      except
        on Exception do
        begin
          DefaultPort := DefaultPort + 1;
          ok := False;
        end;
      end;
    until ok;
  end;

  InitNetGame;
  OldNetGame := NetGame;
  with PlayerMsg do
  begin
    Name := PlayerName;
    Port := Listener.DefaultPort;
    Msg := MSG_JOIN;
  end;
  SendGuaranted(PlayerMsg);
  AddJoined;
  ID := 1;
  while NetGame.Players[ID].Name <> PlayerName do
    Inc(ID);
  PlayerTruck := TruckTable[ID];
  Timer := TTime.Create;
end;

procedure TClient.SendGuaranted(Player: TPlayerMsg);
var
  Received: Integer;
begin
  Received := 0;
  while Received = 0 do
  begin
    SendBuffer(Player, SizeOf(TPlayerMsg));
    Received := ReceiveBuffer(NetGame, SizeOf(TGame));
  end;
end;

procedure TClient.UDPRead(Sender: TObject; AData: TStream; ABinding: TIdSocketHandle);
begin
  if AData.Size = SizeOf(TGame) then
  begin
    OldNetGame := NetGame;
    AData.ReadBuffer(NetGame, SizeOf(TGame));
    RemoveParted;
    AddJoined;
    UpdateTrucks;
  end;
end;

procedure TClient.DoClient;
begin
  if Timer.Diff(1/20) then
  begin
    with NetGame.Players[ID] do
    begin
      Position := PlayerTruck.Position;
      NbRocket := PlayerTruck.NbRocket;
      Frame := PlayerTruck.Frame;
      T := PlayerTruck.T;
{      V := PlayerTruck.V;
      A := PlayerTruck.A;}
    end;
    SendBuffer(NetGame.Players[ID], SizeOf(TPlayerState));
  end;
end;

procedure TClient.RemoveParted;
var
  i: Integer;
begin
  for i:=1 to MAX_PLAYERS do
    if (OldNetGame.Players[i].Name <> '') and (NetGame.Players[i].Name = '') then
    begin
      TruckTable[i].Free;
      TruckTable[i] := nil;
      Console.Log(OldNetGame.Players[i].Name + ' left the arena');
    end;
end;

procedure TClient.AddJoined;
var
  i: Integer;
begin
  for i:=1 to MAX_PLAYERS do
    with NetGame.Players[i] do
      if (Name <> '') and (TruckTable[i] = nil) then
      begin
        TruckTable[i] := TTruck.New(Name, Position, T, Col);
        TruckTable[i].NbRocket := NbRocket;
        Console.Log(Name + ' joined the arena');
      end;
end;

procedure TClient.UpdateTrucks;
var
  i: Integer;
begin
  for i:=1 to MAX_PLAYERS do
    with NetGame.Players[i] do
      if (Name <> '') and (Name <> PlayerTruck.Name) then
      begin
        TruckTable[i].Position := Position;
        TruckTable[i].Frame := Frame;
        TruckTable[i].T := T;
{        TruckTable[i].V := V;
        TruckTable[i].A := A;}
        if NbRocket > TruckTable[i].NbRocket then
          TruckTable[i].Fire;
      end;
end;

destructor TClient.Logout;
var
  PlayerMsg: TPlayerMsg;
begin
  with PlayerMsg do
  begin
    Name := NetGame.Players[ID].Name;
    Msg := MSG_PART;
  end;
  SendGuaranted(PlayerMsg);
  Listener.Free;
  Timer.Free;
  inherited Destroy;
end;

end.

