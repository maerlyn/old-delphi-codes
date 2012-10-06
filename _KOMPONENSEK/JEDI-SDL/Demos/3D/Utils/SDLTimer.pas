unit SDLTimer;

interface

uses
  Timer,
  SDL;

type
  TSDLTimer = class(TTimer)
  private

  protected
    function GetElapsedTime: Cardinal; override;
    function GetFramesPerSecond: Byte; override;
    procedure SetLockedFramesPerSecond(const Value: Byte); override;
  public
    procedure Initialise; override;
  published 

  end;


implementation

{ TSDLTimer }
function TSDLTimer.GetElapsedTime: Cardinal;
begin
  FLastTime := FElapsedTime;
  FElapsedTime := SDL_GetTicks - FStartTime;     // Calculate Elapsed Time
  FElapsedTime :=(FLastTime + ElapsedTime) shr 1; // Average it out for smoother movement
  result := FElapsedTime;
end;

function TSDLTimer.GetFramesPerSecond: Byte;
begin

end;

procedure TSDLTimer.Initialise;
begin
  FStartTime := SDL_GetTicks;
end;

procedure TSDLTimer.SetLockedFramesPerSecond(const Value: Byte);
begin
  FLockedFramesPerSecond := Value;
end;

end.
