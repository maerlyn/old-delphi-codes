unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Unit1, StdCtrls, Buttons, ExtCtrls;

type
  TfrmList = class(TForm)
    cmdBezaras: TBitBtn;
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure cmdBezarasClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmList: TfrmList;
  HanyszorVoltMarMegAzIdozito: integer;

implementation

{$R *.DFM}

procedure TfrmList.FormCreate(Sender: TObject);
var hSysMenu, nCnt: Longint;
    i: integer;
begin
 for i := 0 to frmMainForm.cmbNev.Items.Count - 1 do
  ListBox1.Items.Add(frmMainForm.cmbNev.Items[i]);

//az 'X' gomb kikapcsolása
 hSysMenu := GetSystemMenu(frmList.Handle,false); //a rendszermenü lekérdezése
 if hSysMenu <> 0 then //ha van, akkor
 begin
  nCnt := GetMenuItemCount(hSysMenu); //hány eleme van,
  if nCnt <> 0 then //ha van eleme, akkor
  begin
   RemoveMenu(hSysMenu,nCnt - 1,mf_ByPosition + mf_Remove); //leszedjük a Bezárást, ezzel a gombor is kikapcsoljuk,
   RemoveMenu(hSysMenu,nCnt - 2,mf_ByPosition + mf_Remove); //de elõtte van egy elválasztó is
   DrawMenuBar(frmList.Handle); //erõltetjük az újrarajzolást, ezzel látszik is a mûvünk.
  end;
 end;
//kész
Timer1.Enabled := false;
end;

procedure TfrmList.cmdBezarasClick(Sender: TObject);
begin
 frmList.Close;
end;

procedure TfrmList.ListBox1Click(Sender: TObject);
var i: integer;
begin
 for i := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[i] then frmMainForm.cmbNev.ItemIndex := i;
 frmMainForm.GetData;
end;

procedure TfrmList.FormShow(Sender: TObject);
var i, bookmark: integer;
begin
 for i := 0 to ListBox1.Items.Count - 1 do
  if ListBox1.Selected[i] then bookmark := i;
 ListBox1.Items.Clear;
 for i := 0 to frmMainForm.cmbNev.Items.Count - 1 do
  ListBox1.Items.Add(frmMainForm.cmbNev.Items[i]);
 try
  ListBox1.Selected[bookmark] := true;
 except
  ListBox1.Selected[0] := true;
 end;
end;

procedure TfrmList.Timer1Timer(Sender: TObject);
var i: integer;
begin
 Inc(HanyszorVoltMarMegAzIdozito);
 if HanyszorVoltMarMegAzIdozito = 12 then
 begin
  HanyszorVoltMarMegAzIdozito := 0;
  ListBox1.Items.Clear;
  for i := 0 to frmMainForm.cmbNev.Items.Count - 1 do
   ListBox1.Items.Add(frmMainForm.cmbNev.Items[i]);
 end;
end;

end.
