program BasFtp;

uses
  Forms,
  BasFtp1 in 'BasFtp1.pas' {BasicFtpClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBasicFtpClientForm, BasicFtpClientForm);
  Application.Run;
end.
