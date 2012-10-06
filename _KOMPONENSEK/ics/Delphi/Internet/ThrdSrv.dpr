program ThrdSrv;

uses
  Forms,
  ThrdSrv1 in 'ThrdSrv1.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
