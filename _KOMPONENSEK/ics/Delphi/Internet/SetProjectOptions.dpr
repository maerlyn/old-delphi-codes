program SetProjectOptions;

uses
  Forms,
  SetProjectOptions1 in 'SetProjectOptions1.pas' {AppBaseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppBaseForm, AppBaseForm);
  Application.Run;
end.
