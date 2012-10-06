program HttpTst;

uses
  Forms,
  httptst1 in 'httptst1.pas' {HttpTestForm};

{$R *.RES}

begin
{$IFNDEF VER80}
  Application.CreateForm(THttpTestForm, HttpTestForm);
{$ENDIF}
  Application.Run;
end.
