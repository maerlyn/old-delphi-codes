unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, Unit4, Menus, Unit5;

type
  TfrmCimek = class(TForm)
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    mnuEdit: TMenuItem;
    mnuDelete: TMenuItem;
    mnuNew: TMenuItem;
    mnuDelall: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure UpdateStringGrid;
    procedure mnuNewClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuDelallClick(Sender: TObject);
  private
    xRow: integer;
    xCol: integer;
  public
    { Public declarations }
  end;

var
  frmCimek: TfrmCimek;

implementation

uses Unit1;

{$R *.DFM}

procedure TfrmCimek.FormCreate(Sender: TObject);
begin
 StringGrid1.Cells[0,0] := 'Név';
 StringGrid1.Cells[1,0] := 'IP';

 LoadAddresses;
 UpdateStringGrid;
end;

procedure TfrmCimek.UpdateStringGrid;
var i: integer;
begin
 if MennyiCim > 1 then
  StringGrid1.RowCount := MennyiCim + 1;
 if MennyiCim = 0 then
  StringGrid1.RowCount := 2;

 if MennyiCim = 0 then
 begin
  StringGrid1.Cells[0,1] := '';
  StringGrid1.Cells[1,1] := '';
 end
 else
  for i := 1 to MennyiCim do
  begin
   StringGrid1.Cells[0,i] := Addresses[i].Nev;
   StringGrid1.Cells[1,i] := Addresses[i].IP;
  end;
end;

procedure TfrmCimek.mnuNewClick(Sender: TObject);
var frm: TfrmEditAddress;
begin
 frm := TfrmEditAddress.Create(frmMainForm);
 if frm.ShowModal = mrOK then
 begin
  inc(MennyiCim);
  Addresses[MennyiCim].Nev := frm.txtNev.Text;
  Addresses[MennyiCim].IP := frm.txtIP.Text;
 end;
 frm.Free;
 SaveAddresses;
 UpdateStringGrid;
end;

procedure TfrmCimek.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 SaveAddresses;
end;

procedure TfrmCimek.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 StringGrid1.MouseToCell(X,Y,xCol,xRow);
end;

procedure TfrmCimek.StringGrid1DblClick(Sender: TObject);
begin
 frmMainForm.txtCim.Text := Addresses[xRow].Nev + ' <' + Addresses[xRow].IP + '>';
end;

procedure TfrmCimek.mnuEditClick(Sender: TObject);
var frm: TfrmEditAddress;
begin
 if (xRow < 1)or(xRow > MennyiCim) then
  Exit;

 frm := TfrmEditAddress.Create(frmMainForm);
 frm.txtNev.Text := Addresses[xRow].Nev;
 frm.txtIP.Text := Addresses[xRow].IP;
 if frm.ShowModal = mrOK then
 begin
  Addresses[xRow].Nev := frm.txtNev.Text;
  Addresses[xRow].IP := frm.txtIP.Text;
 end;
 frm.Free;
 SaveAddresses;
 UpdateStringGrid;
end;

procedure TfrmCimek.mnuDeleteClick(Sender: TObject);
var i: integer;
begin
 if MennyiCim = 0 then
  Abort;

 for i := xRow to 1023 do
 begin
  Addresses[i].Nev := Addresses[i+1].Nev;
  Addresses[i].IP  := Addresses[i+1].IP;
 end;
 
 Addresses[1024].Nev := '';
 Addresses[1024].IP := '';
 dec(MennyiCim);

 SaveAddresses;
 UpdateStringGrid;
end;

procedure TfrmCimek.mnuDelallClick(Sender: TObject);
var i: integer;
begin
 if Application.MessageBox('Biztos törlöd az összes címet?','LAN messager',mb_YesNo + mb_IconQuestion) = idNo then
  Abort;

 for i := 1 to high(Addresses) do
 begin
  Addresses[i].Nev := '';
  Addresses[i].IP := '';
 end;

 MennyiCim := 0;

 UpdateStringGrid;
end;

end.
