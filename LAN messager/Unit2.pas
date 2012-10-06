unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls;

type
  TfrmOlvasas = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbxUrgent: TCheckBox;
    txtDate: TEdit;
    txtSender: TEdit;
    txtMessage: TMemo;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure LoadData(Urgent: boolean;Date,Sender,Msg:string);
    procedure cbxUrgentMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOlvasas: TfrmOlvasas;
  Surgos: boolean;

implementation

{$R *.DFM}

procedure TfrmOlvasas.LoadData(Urgent: boolean; Date, Sender, Msg: string);
begin
 txtDate.Text := Date;
 cbxUrgent.Checked := Urgent;
 txtSender.Text := Sender;
 txtMessage.Lines.Text := Msg;

 Surgos := cbxUrgent.Checked;
end;

procedure TfrmOlvasas.SpeedButton1Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmOlvasas.cbxUrgentMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 cbxUrgent.Checked := Surgos;
end;

end.
