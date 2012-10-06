program Rltest;

uses
  Forms,
  Rltestu in 'RLTESTU.PAS' {RLDemo};

{$R *.RES}

begin
  Application.CreateForm(TRLDemo, RLDemo);
  Application.Run;
end.
