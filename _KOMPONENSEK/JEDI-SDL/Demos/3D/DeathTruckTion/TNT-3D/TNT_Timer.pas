unit TNT_Timer;

// Gestion des timers du jeu
// -------------------------
//  Tous les temps retournés sont en secondes
//  Create: remise à zero
//  Refresh: actualise la fréquence
//  Delta: temps écoulé depuis le dernier appel à Delta
//  Diff: permet de tester si un delai s'est écoulé
//  *** Note: on ne peut pas utiliser Refresh et Diff d'un meme timer

interface

uses
  SDL;

type
  TTime = class
  private
    Count: Cardinal;
    LastTime: Uint32;
    LastTimeDelta: Uint32;
  public
    Frequency: Single;
    constructor Create;
    function Delta: Single;
    function Refresh: Single;
    function Diff(Interval: Single): Boolean;
  end;

implementation

constructor TTime.Create;
begin
  inherited Create;
  Frequency := 0;
  Count := 0;
  LastTime := SDL_GetTicks;
  LastTimeDelta := LastTime;
end;

function TTime.Diff(Interval: Single): Boolean;
var
  NewTime: Uint32;
begin
  NewTime := SDL_GetTicks;
  Result := False;
  if ((NewTime - LastTime) / 1000) >= Interval then
  begin
    LastTime := NewTime;
    Result := True;
  end;
end;

function TTime.Delta: Single;
var
  NewTime: Uint32;
begin
  NewTime := SDL_GetTicks;
  Result := (NewTime - LastTimeDelta) / 1000;
  LastTimeDelta := NewTime;
end;

function TTime.Refresh: Single;
begin
  Inc(Count);
  if Diff(1) then
  begin
    Frequency := Count;
    Count := 0;
  end;
  Result := Frequency;
end;

end.

