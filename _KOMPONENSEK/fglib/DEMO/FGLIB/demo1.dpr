program demo1;

uses
  Forms,
  main1 in 'main1.pas' {Form1};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
