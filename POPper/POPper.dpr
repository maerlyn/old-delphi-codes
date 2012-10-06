program POPper;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frmBeallitasok},
  Unit3 in 'Unit3.pas' {frmEmailkuldes},
  Unit4 in 'Unit4.pas' {frmKuldendoleveleklistaja},
  Unit5 in 'Unit5.pas' {frmUjleveleklistaja},
  Unit6 in 'Unit6.pas' {frmUjlevelolvasasa},
  About in 'About.pas' {frmAbout};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Putra POPper';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.Run;
end.
