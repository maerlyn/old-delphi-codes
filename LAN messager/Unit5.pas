unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, Unit4;

type
  TfrmEditAddress = class(TForm)
    txtNev: TEdit;
    txtIP: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure txtNevExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEditAddress: TfrmEditAddress;

implementation

{$R *.DFM}

procedure TfrmEditAddress.txtNevExit(Sender: TObject);
begin
 if (CountDots(txtNev.Text) = 3)or(pos('<',txtNev.Text)>0)or(pos('>',txtNev.Text)>0) then
 begin
  Application.MessageBox('Hibás név!','LAN messager',mb_OK + mb_IconError);
  txtNev.SelectAll;
  txtNev.SetFocus;
 end;
end;

procedure TfrmEditAddress.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := true;
 if ((CountDots(txtNev.Text) = 3)or(pos('<',txtNev.Text)>0)or(pos('>',txtNev.Text)>0))and(ModalResult = mrOK) then
 begin
  Application.MessageBox('Hibás név!','LAN messager',mb_OK + mb_IconError);
  txtNev.SelectAll;
  txtNev.SetFocus;
  CanClose := false;
 end;
 if (ModalResult = mrOK)and(CountDots(txtIP.Text)<>3) then
 begin
  Application.MessageBox('Hibás IP-cím!','LAN messager',mb_OK + mb_IconError);
  txtIP.SelectAll;
  txtIP.SetFocus;
  CanClose := false;
 end;
end;

end.
