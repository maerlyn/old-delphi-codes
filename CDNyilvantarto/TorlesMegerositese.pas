unit TorlesMegerositese;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MainForm, ButtonWithColor;

type
  TfrmTorlesMegerositese = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    Button1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTorlesMegerositese: TfrmTorlesMegerositese;

implementation

{$R *.DFM}

procedure TfrmTorlesMegerositese.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;

 Button1.Color := frmMainForm.Gombok;
 Button2.Color := frmMainForm.Gombok;

 Button1.Font.Color := frmMainForm.Betuk;
 Button2.Font.Color := frmMainForm.Betuk;

 Label1.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
end;

procedure TfrmTorlesMegerositese.FormShow(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;

 Button1.Color := frmMainForm.Gombok;
 Button2.Color := frmMainForm.Gombok;

 Button1.Font.Color := frmMainForm.Betuk;
 Button2.Font.Color := frmMainForm.Betuk;

 Label1.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
end;

end.
