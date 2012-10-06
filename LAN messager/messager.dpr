program messager;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frmOlvasas},
  Unit3 in 'Unit3.pas' {frmCimek},
  Unit4 in 'Unit4.pas',
  Unit5 in 'Unit5.pas' {frmEditAddress};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Putra LAN messager';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmCimek, frmCimek);
  Application.CreateForm(TfrmEditAddress, frmEditAddress);
  Application.Run;
end.
