program Hanoi;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Hanoi tornyok';
  Application.CreateForm(TFoForm, FoForm);
  Application.Run;
end.
