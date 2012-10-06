unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, Progress, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Progress1: TProgress;
    Gauge1: TGauge;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var i : integer;
begin
     Gauge1.progress := 0;
     Progress1.Progress := 0;

     for i := 0 to 100 do
     begin
          Progress1.progress := i;
          Progress1.text := inttostr(i) + '%';
          Gauge1.progress := i;
     end;

     MessageBox (handle, 'And look ! the TProgress finished the race first (a nasty trcik, isn''t it ?...)', '', mb_ok);
end;

end.
