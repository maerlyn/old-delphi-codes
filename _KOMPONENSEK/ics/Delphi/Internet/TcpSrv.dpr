program TcpSrv;

uses
  Forms,
  TcpSrv1 in 'TcpSrv1.pas' {TcpSrvForm},
  WSocket in '..\VC32\WSocket.pas',
  WSocketS in '..\VC32\WSocketS.pas';

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
