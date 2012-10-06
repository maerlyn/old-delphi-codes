program Torigyakorlo;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMainForm},
  Gyakorlas in 'Gyakorlas.pas' {frmGyakorlas},
  Teszt in 'Teszt.pas' {frmTeszt},
  Kerdes in 'Kerdes.pas' {frmKerdes},
  Eredmeny in 'Eredmeny.pas' {frmEredmeny};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Törigyakorló';
  Application.CreateForm(TfrmMainForm, frmMainForm);
  Application.CreateForm(TfrmKerdes, frmKerdes);
  Application.CreateForm(TfrmGyakorlas, frmGyakorlas);
  Application.CreateForm(TfrmTeszt, frmTeszt);
  Application.CreateForm(TfrmEredmeny, frmEredmeny);
  Application.Run;
end.
