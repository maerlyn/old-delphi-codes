program netstatusservice;

uses
  SvcMgr,
  Unit1 in 'Unit1.pas' {NetStatus: TService};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TNetStatus, NetStatus);
  Application.Run;
end.
