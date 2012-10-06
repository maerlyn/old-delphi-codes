program zipdemo1;

uses
  Forms,
  msgunit in 'msgunit.pas' {Msgform},
  extrunit in 'extrunit.pas' {Extract},
  Addunit in 'Addunit.pas' {AddForm},
  sfxunit in 'sfxunit.pas' {MakeSFX},
  mainunit in 'mainunit.pas' {Mainform},
  renunit in 'renunit.pas' {RenForm};

{$R *.RES}
{$R ZipMsgUS.RES}

begin
  Application.Initialize;
  Application.Title := 'Zip Demo 1';
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(TMsgform, Msgform);
  Application.CreateForm(TExtract, Extract);
  Application.CreateForm(TAddForm, AddForm);
  Application.CreateForm(TMakeSFX, MakeSFX);
  Application.CreateForm(TRenForm, RenForm);
  Application.Run;
end.
