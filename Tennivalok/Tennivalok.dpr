program Tennivalok;

uses
  Forms,
  Unit1 in 'Unit1.pas' {TennivalokForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTennivalokForm, TennivalokForm);
  Application.Run;
end.

