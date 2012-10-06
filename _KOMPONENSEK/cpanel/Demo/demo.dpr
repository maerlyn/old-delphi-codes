program demo;

uses
  Forms,
  _main in '_main.pas' {Form1},
  CirclePanel in '..\..\..\Delphi5\Components\CirclePanel.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
