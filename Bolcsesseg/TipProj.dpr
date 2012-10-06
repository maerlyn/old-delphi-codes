program TipProj;

uses
  Forms,
  Unit3 in 'Unit3.pas'; {TipForm}

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TTipForm, TipForm);
  Application.Run;
end.