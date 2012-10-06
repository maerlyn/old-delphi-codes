unit Timer;

interface

type
  TTimer = class(TObject)
  private

  protected
    FStartTime, FLastTime, FElapsedTime : Cardinal;
    FLockedFramesPerSecond: Byte;
    function GetElapsedTime: Cardinal; virtual;
    function GetFramesPerSecond: Byte; virtual;
    procedure SetLockedFramesPerSecond(const Value: Byte); virtual;
  public
    procedure Initialise; virtual;
    property ElapsedTime : Cardinal read GetElapsedTime;
    property FramesPerSecond : Byte read GetFramesPerSecond;
    property LockedFramesPerSecond : Byte read FLockedFramesPerSecond write SetLockedFramesPerSecond;
  published 

  end;


implementation

{ TTimer }
function TTimer.GetElapsedTime: Cardinal;
begin

end;

function TTimer.GetFramesPerSecond: Byte;
begin

end;

procedure TTimer.Initialise;
begin

end;

procedure TTimer.SetLockedFramesPerSecond(const Value: Byte);
begin
  FLockedFramesPerSecond := Value;
end;

end.
