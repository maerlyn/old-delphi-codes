program Vagolapkezelo;

uses
  Forms,
  Windows,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

var Hwnd: THandle;

{$R *.RES}

begin
 Hwnd := FindWindow('TForm1',nil);
 if Hwnd=0 then
  begin
  Application.Initialize;
  Application.Title := 'Vágólapkezelõ';
  Application.ShowMainForm := False;
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm1, Form1);
  Form1.Icon := Application.Icon;
  //Form2.Icon := Application.Icon;
  Application.Run;
  end
 else
  begin
  Application.MessageBox('Ebbõl a programból egyszerre csak egy futhat!','Vágólapkezelõ',MB_OK+MB_ICONHAND);
  Application.Terminate;
  end;
end.
