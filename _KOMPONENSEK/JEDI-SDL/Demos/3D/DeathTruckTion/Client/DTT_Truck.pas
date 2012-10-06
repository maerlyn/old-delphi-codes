unit DTT_Truck;

// Gestion des trucks
// ------------------

interface

uses
  OpenGL12, TNT_Object, TNT_Vector, TNT_Timer;

const
  MAX_TRUCK = 4;
  CONST_BRAKE = 8;
  CONST_ACCEL = 4;
  CONST_INERTIA = 1.07;

  FRAME_MOVE = 0;
  FRAME_START_LEFT = 8;
  FRAME_LEFT = 11;
  FRAME_START_RIGHT = 19;
  FRAME_RIGHT = 22;

type
  TTruck = class(TObj)
  public
    Name: String;
    V, A, T: Single;
    NbRocket: Integer;
    Color: Byte;
    Life: Single;
    DeathCnt: Cardinal;
    CurrFrame, ToFrame: Integer;
    Roll: Byte;
    Timer: TTime;
    constructor New(PlayerName: String; Pos: TVector; TT: Single; Col: Byte);
    procedure Accelerate;
    procedure Reverse;
    procedure Turn(Angle: Single);
    procedure Fire;
    procedure Animate(Time: Single); override;
    destructor Destroy; override;
  end;

var
  TruckTable: array [1..MAX_TRUCK] of TTruck;
  CollisionOK: Boolean = True;

procedure InitTrucks;
procedure FlushTrucks;

implementation

uses
  TNT_3D, Math, DTT_Game, DTT_Client, TNT_Collisions, DTT_Rocket, TNT_Font,
  DTT_Explosion, DTT_Console, TNT_Entity;

constructor TTruck.New(PlayerName: String; Pos: TVector; TT: Single; Col: Byte);
begin
  inherited Create(Col, Pos);
  Name := PlayerName;
  T := TT;
  Life := 100;
  Timer := TTime.Create;
end;

procedure TTruck.Accelerate;
begin
  if V < 0 then A := CONST_BRAKE;
  if V >= 0 then A := CONST_ACCEL;
end;

procedure TTruck.Reverse;
begin
  if V < 0 then A := -CONST_ACCEL;
  if V >= 0 then A := -CONST_BRAKE;
end;

procedure TTruck.Turn(Angle: Single);
begin
  if Angle > 0 then
    ToFrame := FRAME_RIGHT
  else
    ToFrame := FRAME_LEFT;

  if V < 0 then
    T := T + Max(-1,V)*2*Angle;
  if V > 0 then
    T := T + Min(V,1)*Angle;

  if T < 0 then T := T + 360;
  if T >= 360 then T := T - 360;
end;

procedure TTruck.Animate(Time: Single);
var
  oldPos: TVector;
begin
  if DeathCnt > 0 then
  begin
    Dec(DeathCnt);
    if DeathCnt = 0 then
      ExitFlag := True;
    if DeathCnt = 100 then
      Explosion(Vector(Position.x, Position.y+5, Position.z),1);
    Exit;
  end;

  if Timer.Diff(1/30) and (Name = Client.PlayerTruck.Name) then
  begin
    if V>0.1 then Inc(Roll);
    if V<-0.1 then Dec(Roll);

    Frame := CurrFrame;
    if CurrFrame = ToFrame then
      Frame := Frame + (Roll and 7)
    else if CurrFrame = FRAME_MOVE then
      CurrFrame := ToFrame - 3
    else if ((ToFrame = FRAME_LEFT) and (CurrFrame >= FRAME_START_LEFT) and (CurrFrame < FRAME_LEFT)) or
            ((ToFrame = FRAME_RIGHT) and (CurrFrame >= FRAME_START_RIGHT) and (CurrFrame < FRAME_RIGHT)) then
      Inc(CurrFrame)
    else
    begin
      Dec(CurrFrame);
      if (CurrFrame = FRAME_START_LEFT-1) or (CurrFrame = FRAME_START_RIGHT-1) then
        CurrFrame := FRAME_MOVE;
    end;
    ToFrame := FRAME_MOVE;
  end;

  inherited;
  A := A / CONST_INERTIA;
  V := V / CONST_INERTIA + A*Time;

  Heading := T;
  oldPos := Position;
  Velocity.x := - V*sin(T*(PI/180));
  Velocity.y := 0;
  Velocity.z := - V*cos(T*(PI/180));
  Position.x := Position.x + Velocity.x;
  Position.z := Position.z + Velocity.z;

  if (Life < 0) and (Client.PlayerTruck.Name = Name) then
  begin
    Explosion(Position,1);
    DeathCnt := 130;
  end
  else
  begin
    if (Collision <> nil) and CollisionOK then
    begin
      Position := oldPos;
      V := 0;
      A := 0;
    end;
  end;
  if Name = Client.PlayerTruck.Name then
    CollisionOK := True;
end;

destructor TTruck.Destroy;
begin
  Timer.Free;
  inherited;
end;

procedure TTruck.Fire;
begin
  if DeathCnt = 0 then
    TRocket.New(Self);
end;

procedure InitTrucks;
var
  i: Integer;
begin
  for i:=1 to MAX_TRUCK do
    TruckTable[i] := nil;
end;

procedure FlushTrucks;
var
  i: Integer;
begin
  for i:=1 to MAX_TRUCK do
    TruckTable[i].Free;
end;

end.

