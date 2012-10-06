program demo2;

uses
  Forms,
  MAIN2 in 'MAIN2.PAS' {Form1},
  Fhd in 'FHD.PAS' {HintSetting};

{$R *.RES}

begin
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(THintSetting, HintSetting);
  Application.Run;
end.
