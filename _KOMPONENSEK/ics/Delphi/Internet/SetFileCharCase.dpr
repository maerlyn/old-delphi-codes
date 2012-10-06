program SetFileCharCase;

uses
  Forms,
  SetFileCharCase1 in 'SetFileCharCase1.pas' {AppBaseForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAppBaseForm, AppBaseForm);
  Application.Run;
end.
