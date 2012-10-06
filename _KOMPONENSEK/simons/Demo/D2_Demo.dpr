program D2_Demo;

uses
  Forms,
  D2_Main in 'D2_Main.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simons Komponenten - Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
