unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  CoolTrayIcon;

type
  TNetStatus = class(TService)
    CoolTrayIcon1: TCoolTrayIcon;
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  NetStatus: TNetStatus;

implementation

{$R *.DFM}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  NetStatus.Controller(CtrlCode);
end;

function TNetStatus.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TNetStatus.ServiceStart(Sender: TService; var Started: Boolean);
begin
 CoolTrayIcon1.ShowTaskbarIcon;
end;

procedure TNetStatus.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
 CoolTrayIcon1.HideTaskbarIcon;
end;

end.
