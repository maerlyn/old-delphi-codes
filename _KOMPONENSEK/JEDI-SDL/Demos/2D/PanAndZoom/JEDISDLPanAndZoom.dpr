program JEDISDLPanAndZoom;

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
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
