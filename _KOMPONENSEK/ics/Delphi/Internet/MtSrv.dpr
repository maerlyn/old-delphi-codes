program mtsrv;

uses
  Forms,
  MtSrv1 in 'MtSrv1.pas' {ServerForm},
  MtSrv2 in 'MtSrv2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TServerForm, ServerForm);
  Application.Run;
end.
