program netstatus;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MessengerAPI_TLB in 'C:\Program Files\Borland\Delphi7\Imports\MessengerAPI_TLB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
