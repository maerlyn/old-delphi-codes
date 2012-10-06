program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'PC-Lock';
  if ParamStr(1) <> '-setup' then
  begin
   Application.ShowMainForm := false;
   Application.CreateForm(TForm1, Form1);
  Form1.ShowModal;
  end
  else if ParamStr(1) = '-setup' then
  begin
   Application.CreateForm(TForm2, Form2);
   Form2.ShowModal;
  end;

  Application.Run;
end.
