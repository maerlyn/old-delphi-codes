program Tele4;

uses
  Forms,
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frmList},
  Unit3 in 'Unit3.pas' {frmEmailForm},
  Unit4 in 'Unit4.pas' {frmEmailBForm},
  Unit5 in 'Unit5.pas' {frmWebBrowser},
  Unit6 in 'Unit6.pas' {frmSplash};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSplash, frmSplash);
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmList, frmList);
  Application.CreateForm(TfrmEmailForm, frmEmailForm);
  Application.CreateForm(TfrmEmailBForm, frmEmailBForm);
  Application.CreateForm(TfrmWebBrowser, frmWebBrowser);
  Application.Run;
end.
