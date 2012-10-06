// GLCadencer
{: Egg<p>

	Candencing composant for GLScene (ease Progress processing)<p>

	<b>Historique : </b><font size=-1><ul>
      <li>01/02/01 - Egg - Fixed "Freezing" when Enabled set to False
      <li>08/10/00 - Egg - Added TASAPHandler to support multiple ASAP cadencers
      <li>19/06/00 - Egg - Fixed TGLCadencer.Notification
		<li>14/04/00 - Egg - Minor fixes
		<li>13/04/00 - Egg - Creation
	</ul></font>
}
unit GLCadencer;

interface

uses Windows, Classes, Controls, Messages, GLScene, StdCtrls, Forms, GLMisc;

type

	// TGLCadencerMode
	//
	{: Determines how the TGLCadencer operates.<p>
		- cmManual : you must trigger progress manually (in your code)<br>
		- cmASAP : progress is triggered As Soon As Possible after a previous
			progress (uses windows messages). }
	TGLCadencerMode = (cmManual, cmASAP);

	// TGLCadencerTimeReference
	//
	{: Determines which time reference the TGLCadencer should use.<p>
		- cmRTC : the Real Time Clock is used (precise over long periods, but
			not accurate to the millisecond)<br>
		- cmPerformanceCounter : the windows performance counter is used (nice
			precision, may derive over long periods, this is the default option
			as it allows the smoothest animation on fast systems)<br>
		- cmExternal : the CurrentTime property is used }
	TGLCadencerTimeReference = (cmRTC, cmPerformanceCounter, cmExternal);

	// TGLCadencer
	//
	{: This component allows auto-progression of animation.<p>
		Basicly dropping this component and linking it to your TGLScene will send
		it real-time progression events (time will be measured in seconds) while
		keeping the CPU 100% busy if possible (ie. if things change in your scene...).<p>
		The progression time (the one you'll see in you progression events)
		is calculated using  (CurrentTime-OriginTime)*TimeMultiplier,
		CurrentTime being either manually or automatically updated using
		TimeReference (setting CurrentTime does NOT trigger progression). }
	TGLCadencer = class (TComponent)
		private
			{ Private Declarations }
         FSubscribedCadenceableComponents : TList;
			FScene : TGLScene;
			FTimeMultiplier : Double;
			lastTime, downTime : Double;
			FEnabled : Boolean;
			FSleepLength : Integer;
			FMode : TGLCadencerMode;
			FTimeReference : TGLCadencerTimeReference;
			FCurrentTime : Double;
			FOriginTime : Double;
			FOnProgress : TGLProgressEvent;
			progressing : Integer;

		protected
			{ Protected Declarations }
			procedure Notification(AComponent: TComponent; Operation: TOperation); override;
			function StoreTimeMultiplier : Boolean;
			procedure SetEnabled(const val : Boolean);
			procedure SetScene(const val : TGLScene);
			procedure SetMode(const val : TGLCadencerMode);
			procedure SetTimeReference(const val : TGLCadencerTimeReference);
			{: Returns raw ref time (no multiplier, no offset) }
			function GetRawReferenceTime : Double;
         procedure RestartASAP;

		public
			{ Public Declarations }
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;

         procedure Subscribe(aComponent : TGLCadenceAbleComponent);
         procedure UnSubscribe(aComponent : TGLCadenceAbleComponent);

			{: Allows to manually trigger a progression.<p>
				Time stuff is handled automatically.<p>
				If cadencer is disabled, this functions does nothing. }
			procedure Progress;
			{: Adjusts CurrentTime if necessary and returns its value. }
			function GetCurrentTime : Double;

			{: Value soustracted to current time to obtain progression time. }
			property OriginTime : Double read FOriginTime write FOriginTime;
			{: Current time (manually or automatically set, see TimeReference). }
			property CurrentTime : Double read FCurrentTime write FCurrentTime;

		published
			{ Published Declarations }
			{: The TGLScene that will be cadenced (progressed). }
			property Scene : TGLScene read FScene write SetScene;
			{: Enables/Disables cadencing.<p>
				Disabling won't cause a jump when restarting, it is working like
				a play/pause (ie. may modify OriginTime to keep things smooth). }
			property Enabled : Boolean read FEnabled write SetEnabled default True;
			{: Defines how CurrentTime is updated.<p>
				See TGLCadencerTimeReference.<br>
				Dynamically changeing the TimeReference may cause a "jump".  }
			property TimeReference : TGLCadencerTimeReference read FTimeReference write SetTimeReference default cmPerformanceCounter;
			{: Multiplier applied to the time reference.<p>
				Dynamically changeing the TimeMultiplier may cause a "jump". }
			property TimeMultiplier : Double read FTimeMultiplier write FTimeMultiplier stored StoreTimeMultiplier;
			{: Adjusts how progression events are triggered.<p>
				See TGLCadencerMode. }
			property Mode : TGLCadencerMode read FMode write SetMode default cmASAP;
			{: Allows relinquishing time to other threads/processes.<p>
				A "sleep" is issued BEFORE each progress if SleepLength>=0 (see
				help for the "sleep" procedure in delphi for details. }
			property SleepLength : Integer read FSleepLength write FSleepLength default -1;
			{: Happens AFTER scene was progressed. }
			property OnProgress : TGLProgressEvent read FOnProgress write FOnProgress;
	end;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils;

type
   TASAPHandler = class
   	FWindowHandle : HWND;
   	procedure WndProc(var Msg: TMessage);

      constructor Create;
      destructor Destroy; override;
   end;

var
	vCounterFrequency : TLargeInteger;
	vWMTickCadencer : Cardinal;
   vASAPCadencerList : TList;
   vHandler : TASAPHandler;

const
	cTickGLCadencer = 'TickGLCadencer';

// RegisterASAPCadencer
//
procedure RegisterASAPCadencer(aCadencer : TGLCadencer);
begin
   if not Assigned(vASAPCadencerList) then
      vASAPCadencerList:=TList.Create;
   if vASAPCadencerList.IndexOf(aCadencer)<0 then begin
      vASAPCadencerList.Add(aCadencer);
      if not Assigned(vHandler) then
         vHandler:=TASAPHandler.Create;
   end;
end;

// UnRegisterASAPCadencer
//
procedure UnRegisterASAPCadencer(aCadencer : TGLCadencer);
begin
   if Assigned(vASAPCadencerList) then begin
      vASAPCadencerList.Remove(aCadencer);
      if vASAPCadencerList.Count=0 then begin
         vHandler.Free;
         vHandler:=nil;
      end;
   end;
end;

// ------------------
// ------------------ TASAPHandler ------------------
// ------------------

// Create
//
constructor TASAPHandler.Create;
begin
	inherited Create;
   FWindowHandle:=AllocateHWnd(WndProc);
	PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
end;

// Destroy
//
destructor TASAPHandler.Destroy;
begin
 	DeallocateHWnd(FWindowHandle);
   inherited Destroy;
end;

// WndProc
//
var
   vWndProcInLoop : Boolean;
procedure TASAPHandler.WndProc(var Msg: TMessage);
var
   i : Integer;
   cad : TGLCadencer;
begin
   with Msg do begin
      if not vWndProcInLoop then begin
         vWndProcInLoop:=True;
         try
         	if (Msg=vWMTickCadencer) then begin
               for i:=vASAPCadencerList.Count-1 downto 0 do begin
                  cad:=TGLCadencer(vASAPCadencerList[i]);
                  if (cad.Mode=cmASAP) and cad.Enabled and (cad.progressing=0) then begin
                     try
                        // do stuff
                        cad.Progress;
                     except
                        Application.HandleException(Self);
                     end
                  end;
               end;
               // prepare the return of the infernal loop...
               PostMessage(FWindowHandle, vWMTickCadencer, 0, 0);
            end;
         finally
            vWndProcInLoop:=False;
         end;
      end;
		Result:=0;
	end;
end;

// ------------------
// ------------------ TGLCadencer ------------------
// ------------------

// Create
//
constructor TGLCadencer.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FTimeReference:=cmPerformanceCounter;
	downTime:=GetRawReferenceTime;
	FOriginTime:=downTime;
	FTimeMultiplier:=1;
	FSleepLength:=-1;
	Mode:=cmASAP;
   Enabled:=True;
   FSubscribedCadenceableComponents:=TList.Create;
end;

// Destroy
//
destructor TGLCadencer.Destroy;
begin
   UnRegisterASAPCadencer(Self);
   FSubscribedCadenceableComponents.Free;
   FSubscribedCadenceableComponents:=nil;
	inherited Destroy;
end;

// Subscribe
//
procedure TGLCadencer.Subscribe(aComponent : TGLCadenceAbleComponent);
begin
   if Assigned(FSubscribedCadenceableComponents) then
      if FSubscribedCadenceableComponents.IndexOf(aComponent)<0 then begin
         FSubscribedCadenceableComponents.Add(aComponent);
         aComponent.FreeNotification(Self);
      end;
end;

// UnSubscribe
//
procedure TGLCadencer.UnSubscribe(aComponent : TGLCadenceAbleComponent);
var
   i : Integer;
begin
   if Assigned(FSubscribedCadenceableComponents) then begin
      i:=FSubscribedCadenceableComponents.IndexOf(aComponent);
      if i>=0 then begin
         FSubscribedCadenceableComponents.Remove(aComponent);
         aComponent.RemoveFreeNotification(Self);
      end;
   end;
end;

// Notification
//
procedure TGLCadencer.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
   	if AComponent=FScene then Scene:=nil;
      if Assigned(FSubscribedCadenceableComponents) then
         FSubscribedCadenceableComponents.Remove(AComponent);
   end;
end;

// RestartASAP
//
procedure TGLCadencer.RestartASAP;
begin
   if (Mode=cmASAP) and (not (csDesigning in ComponentState))
      and Assigned(FScene) and Enabled then
         RegisterASAPCadencer(Self)
   else UnRegisterASAPCadencer(Self);
end;

// SetEnabled
//
procedure TGLCadencer.SetEnabled(const val : Boolean);
begin
	if FEnabled<>val then begin
		FEnabled:=val;
      if not (csDesigning in ComponentState) then begin
   		if Enabled then
	   		FOriginTime:=FOriginTime+GetRawReferenceTime-downTime
   		else downTime:=GetRawReferenceTime;
         RestartASAP;
      end;
	end;
end;

// SetScene
//
procedure TGLCadencer.SetScene(const val : TGLScene);
begin
	if FScene<>val then begin
		FScene:=val;
      RestartASAP;
	end;
end;

// StoreTimeMultiplier
//
function TGLCadencer.StoreTimeMultiplier : Boolean;
begin
	Result:=(FTimeMultiplier<>1);
end;

// SetMode
//
procedure TGLCadencer.SetMode(const val : TGLCadencerMode);
begin
	if FMode<>val then begin
		FMode:=val;
      RestartASAP;
	end;
end;

// SetTimeReference
//
procedure TGLCadencer.SetTimeReference(const val : TGLCadencerTimeReference);
begin
	// nothing more, yet
	FTimeReference:=val;
end;

// Progress
//
procedure TGLCadencer.Progress;
var
	deltaTime, newTime : Double;
   i : Integer;
begin
	// basic protection against infinite loops,
   // shall never happen, unless there is a bug in user code
   if progressing<0 then Exit;
	Inc(progressing);
	try
		if Enabled then begin
			// avoid stalling everything else...
			if SleepLength>=0 then
				Sleep(SleepLength);
			// in manual mode, the user is supposed to make sure messages are handled
         if (progressing=1) and (Mode=cmASAP) then
   			Application.ProcessMessages;
			// ...and progress !
			newTime:=GetCurrentTime;
			deltaTime:=newTime-lastTime;
			if Assigned(FScene) and (deltaTime<>0) then begin
            progressing:=-progressing;
            try
   				FScene.Progress(deltaTime, newTime);
            finally
               progressing:=-progressing;
		   		lastTime:=newTime;
            end;
			end;
         for i:=0 to FSubscribedCadenceableComponents.Count-1 do
            with TGLCadenceAbleComponent(FSubscribedCadenceableComponents[i]) do
               DoProgress(deltaTime, newTime);
			if Assigned(FOnProgress) then
				FOnProgress(Self, deltaTime, newTime);
		end;
	finally
      Dec(progressing);
	end;
end;

// GetRawReferenceTime
//
function TGLCadencer.GetRawReferenceTime : Double;
var
	counter : TLargeInteger;
begin
	case FTimeReference of
		cmRTC : // Real Time Clock
			Result:=Now*(3600*24);
		cmPerformanceCounter : begin // Windows HiRes Performance Counter
			QueryPerformanceCounter(counter);
			Result:=counter/vCounterFrequency;
		end;
		cmExternal : // User defined value
			Result:=FCurrentTime;
	else
		Result:=0;
		Assert(False);
	end;
end;

// GetCurrentTime
//
function TGLCadencer.GetCurrentTime : Double;
begin
	Result:=(GetRawReferenceTime-FOriginTime)*FTimeMultiplier;
	FCurrentTime:=Result;
end;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
initialization
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

	// Get our Windows message ID
	vWMTickCadencer:=RegisterWindowMessage(cTickGLCadencer);

	// Preparation for high resolution timer
	if not QueryPerformanceFrequency(vCounterFrequency) then
		vCounterFrequency := 0;

end.
