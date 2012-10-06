program ShaTest;

uses
  Forms,
  ShaTest1 in 'ShaTest1.pas' {SHA1TestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSHA1TestForm, SHA1TestForm);
  Application.Run;
end.
