program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {frmNevjegy};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Szövegszerkesztõ';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TfrmNevjegy, frmNevjegy);
  Application.Run;
end.
