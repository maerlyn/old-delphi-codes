program Labirintus;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FoForm},
  Unit2 in 'Unit2.pas' {frmPalyaszerkeszto};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Labirintus';
  Application.HelpFile := 'D:\Program Files\Apache Group\Apache\htdocs\Gabor\DELPHI\Labirintus\Labirintus.hlp';
  Application.CreateForm(TFoForm, FoForm);
  Application.CreateForm(TfrmPalyaszerkeszto, frmPalyaszerkeszto);
  Application.Run;
end.
