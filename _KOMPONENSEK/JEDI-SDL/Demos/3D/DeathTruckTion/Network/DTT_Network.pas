unit DTT_Network;

interface

uses
  TNT_Vector;

const
  SERVER_PORT = 30000;
  MAX_PLAYERS = 4;

  MSG_JOIN = 0;
  MSG_PART = 1;
  MSG_POSITION = 2;

type
  TPlayerState = packed record
    Name: String[16];
    ID: Byte;
    Position: TVector;
    T: Single;
    NbRocket: Byte;
    Col: Byte;
    Frame: Byte;
  end;

  TPlayerMsg = packed record
    Name: String[16];
    Port: Integer;
    Msg: Byte;
  end;

  TGame = packed record
    NbPlayers: Byte;
    Players: packed array [1..MAX_PLAYERS] of TPlayerState;
  end;

var
  NetGame: TGame;

procedure InitNetGame;

implementation

procedure InitNetGame;
var
  i: Integer;
begin
  with NetGame do
  begin
    NbPlayers := 0;
    for i := 1 to MAX_PLAYERS do
      Players[i].Name := '';
  end;
end;

end.

