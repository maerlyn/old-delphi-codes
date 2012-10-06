program MailSndAsync;

uses
  Forms,
  MailSndAsync1 in 'MailSndAsync1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
