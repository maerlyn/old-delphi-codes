program Xmas;

uses
  Forms,
  XMain in 'XMain.pas' {XForm},
  XParticles in 'XParticles.pas',
  XParticleClasses in 'XParticleClasses.pas',
  XTexObject in 'XTexObject.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TXForm, XForm);
  Application.Run;
end.
