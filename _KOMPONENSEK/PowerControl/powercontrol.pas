//======================================================
//            Delphi Component PowerControl
//               This component is FREE!
//
//              Autor: Konstantin Kotzev
//             e-mail: k2_cult@yahoo.com
//======================================================
unit PowerControl;

interface

uses WinTypes, WinProcs, Messages, SysUtils, Classes, Controls,
     Forms, Graphics, MMSystem;

type
   TAction = (actLogOFF,actShutDown,actReBoot,actForce,actPowerOFF,actForceIfHung,actMonitorOFF,actMonitorON,actCDEject,actCDUnEject);

type
  TPowerControl = class(TComponent)
    private
        FAction : TAction;
        procedure SetAction(Value : TAction);
    protected
    public
        function Execute : Boolean;
    published
        property Action : TAction read FAction write SetAction;
  end;

procedure Register;

implementation

procedure Register;
begin
     RegisterComponents('K2', [TPowerControl]);
end;

procedure TPowerControl.SetAction(Value : TAction);
begin
     FAction := Value;
end;

function TPowerControl.Execute : Boolean;
begin
    with (Owner as TForm) do
       case FAction of
         actLogOff: ExitWindowsEx(EWX_LOGOFF,1);
         actShutDown: ExitWindowsEx(EWX_SHUTDOWN,1);
         actReBoot: ExitWindowsEx(EWX_REBOOT,1);
         actForce: ExitWindowsEx(EWX_FORCE,1);
         actPowerOff: ExitWindowsEx(EWX_POWEROFF,1);
         actForceIfHung: ExitWindowsEx(EWX_FORCEIFHUNG,1);
         actMonitorOFF: SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MONITORPOWER, 0);
         actMonitorON: SendMessage(Application.Handle, WM_SYSCOMMAND, SC_MONITORPOWER, -1);
         actCDEject: mciSendstring('SET CDAUDIO DOOR OPEN WAIT',nil,0, Handle);
         actCDUnEject: mciSendstring('SET CDAUDIO DOOR CLOSED WAIT',nil,0, Handle);
       end;
    Result := True;
end;

end.
