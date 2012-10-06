unit Toltes;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Gauges;

type
  TfrmToltes = class(TForm)
    Gauge1: TGauge;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmToltes: TfrmToltes;

implementation

uses MainForm;

{$R *.DFM}

procedure TfrmToltes.FormShow(Sender: TObject);
begin
 frmToltes.Color := frmMainForm.HatterSzin;
 Label1.Font.Color := frmMainForm.Betuk;
end;

end.
