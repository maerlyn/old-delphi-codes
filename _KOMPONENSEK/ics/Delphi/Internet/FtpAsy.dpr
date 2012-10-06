program FtpAsy;

uses
  Forms,
  FtpAsy1 in 'FtpAsy1.pas' {FtpAsyncForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFtpAsyncForm, FtpAsyncForm);
  Application.Run;
end.
