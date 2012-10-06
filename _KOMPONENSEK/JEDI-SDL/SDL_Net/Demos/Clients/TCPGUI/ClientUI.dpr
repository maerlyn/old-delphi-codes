program ClientUI;

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
{$IFDEF VCL}
  Forms,
{$ENDIF}
{$IFDEF CLX}
  QForms,
{$ENDIF}
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
