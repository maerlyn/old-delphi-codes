unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  adCpuUsage, RackCtls, ExtCtrls;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    LEDMeter1: TLEDMeter;
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 collectcpudata;
 ledmeter1.Position := trunc(GetCPUUsage(0) * 100);
end;

end.
 
