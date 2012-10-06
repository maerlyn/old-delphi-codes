program Work2;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frmModositas};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmModositas, frmModositas);
  Application.Run;
end.

