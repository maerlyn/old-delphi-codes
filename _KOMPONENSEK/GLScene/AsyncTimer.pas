{: Asynchronous timer component (actual 1 ms resolution).<p>

   This component is based on ThreadedTimer by Carlos Barbosa.<p>

   <b>History : </b><font size=-1><ul>
      <li>05/04/00 - GrC - Enabled checks to prevent events after destroy
      <li>01/04/00 - Egg - Re-Creation, minor changes over Carlos's code
   </ul></font>
}
unit AsyncTimer;

interface

uses
  Windows, Classes;

const
  cDEFAULT_TIMER_INTERVAL = 1000;

type
   // TAsyncTimer
   //
   {: Asynchronous timer component (actual 1 ms resolution).<p>
      Keep in mind timer resolution is obtained <i>in-between</i> events, but
      events are not triggered every x ms. For instance if you set the interval to
      5 ms, and your Timer event takes 1 ms to complete, Timer events will actually
      be triggered every 5+1=6 ms (that's why it's "asynchronous").<p>
      This component is based on ThreadedTimer by Carlos Barbosa. }
   TAsyncTimer = class(TComponent)
      private
         FEnabled: Boolean;
         FOnTimer: TNotifyEvent;
         FTimerThread: TThread;

      protected
         procedure SetEnabled(Value: Boolean);
         function GetInterval: Word;
         procedure SetInterval(Value: Word);
         function GetThreadPriority: TThreadPriority;
         procedure SetThreadPriority(Value: TThreadPriority);
         procedure DoTimer;

      public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

      published
         property Enabled: Boolean read FEnabled write SetEnabled default False;
         property Interval: Word read GetInterval write SetInterval  default cDEFAULT_TIMER_INTERVAL;
         property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
         property ThreadPriority: TThreadPriority read GetThreadPriority write SetThreadPriority default tpNormal;
  end;

implementation

uses SysUtils;

type
   // TTimerThread
   //
   TTimerThread = class(TThread)
      private
         FOwner: TAsyncTimer;
         FInterval: Word;
         FStop: THandle;
      protected
         constructor Create(CreateSuspended: Boolean); virtual;
         procedure Execute; override;
   end;

// Create
//
constructor TTimerThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  // create event object for signaling interruptions
  FStop := CreateEvent(nil, False, False, nil);
end;

// Execute
//
procedure TTimerThread.Execute;
begin
   while not Terminated do begin
      // wait for time elapse
      if WaitForSingleObject(FStop, FInterval) = WAIT_TIMEOUT then
      // test for termination here too, odds are termination happenning
      // during WaitSingleObject most of the time !
      if not Terminated then
         // if time elapsed run user-event
         Synchronize(FOwner.DoTimer);
   end;
   // Delete event object
   CloseHandle(FStop);
end;

{ TAsyncTimer }

// Create
//
constructor TAsyncTimer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   // create timer thread
   FTimerThread := TTimerThread.Create(True);
   with TTimerThread(FTimerThread) do begin
      FOwner := Self;
      FreeOnTerminate := False;
      Priority := tpNormal;
      FInterval := cDEFAULT_TIMER_INTERVAL;
   end;
end;

// Destroy
//
destructor TAsyncTimer.Destroy;
begin
   Enabled:=False;
   // Destroy thread
   // signal thread to terminate
   FTimerThread.Terminate;
   SetEvent(TTimerThread(FTimerThread).FStop);
   // resume if stopped
   if FTimerThread.Suspended then FTimerThread.Resume;
   // wait and free
   FTimerThread.WaitFor;
   FTimerThread.Free;
   inherited Destroy;
end;

// DoTimer
//
procedure TAsyncTimer.DoTimer;
begin
   if Enabled and Assigned(FOnTimer) then
      FOnTimer(self);
end;

// SetEnabled
//
procedure TAsyncTimer.SetEnabled(Value: Boolean);
begin
   if Value <> FEnabled then begin
      FEnabled := Value;
      if FEnabled then begin
         // When enabled resume thread
         if TTimerThread(FTimerThread).FInterval > 0 then begin
            SetEvent(TTimerThread(FTimerThread).FStop);
            FTimerThread.Resume;
         end;
      end
   else
      // suspend thread
      FTimerThread.Suspend;
   end;
end;

function TAsyncTimer.GetInterval: Word;
begin
  Result := TTimerThread(FTimerThread).FInterval;
end;

procedure TAsyncTimer.SetInterval(Value: Word);
begin
  if Value <> TTimerThread(FTimerThread).FInterval then begin
    TTimerThread(FTimerThread).FInterval := Value;
  end;
end;

function TAsyncTimer.GetThreadPriority: TThreadPriority;
begin
  Result := FTimerThread.Priority;
end;

procedure TAsyncTimer.SetThreadPriority(Value: TThreadPriority);
begin
  FTimerThread.Priority := Value;
end;

end.
