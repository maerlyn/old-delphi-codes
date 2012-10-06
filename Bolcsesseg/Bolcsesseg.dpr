program Bolcsesseg;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FoForm},
  Unit2 in 'Unit2.pas' {TalcaraUloForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Napi bölcsességek';
  Application.ShowMainForm := false;
  Application.CreateForm(TTalcaraUloForm, TalcaraUloForm);
  Application.CreateForm(TFoForm, FoForm);
  FoForm.Indulas;
  Application.Run;
end.
