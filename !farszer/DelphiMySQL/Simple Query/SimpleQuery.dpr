program SimpleQuery;

uses
  Forms,
  query in 'query.pas' {Form1},
  _libmysq in '_libmysq.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
