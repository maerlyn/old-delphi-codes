library TennivalokShell;

uses
  ComServ,
  TennivalokMenu in 'TennivalokMenu.pas',
  TennivalokShell_TLB in 'TennivalokShell_TLB.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.

