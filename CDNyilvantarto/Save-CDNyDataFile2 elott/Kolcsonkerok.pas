unit Kolcsonkerok;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MainForm, OleCtrls, ButtonWithColor, CDNyDataFile;

type
  TfrmKolcsonkerok = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    Button4: TBitBtnWithColor;
    txtCim: TEdit;
    txtTelszam: TEdit;
    cmbNev: TComboBox;
    CDNyDataFile_Kolcsonkerok1: TCDNyDataFile_Kolcsonkerok;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure GetData;
    procedure SendData;
    procedure cmbNevClick(Sender: TObject);
    procedure cmbNevKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure LoadList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKolcsonkerok: TfrmKolcsonkerok;

implementation

{$R *.DFM}

procedure TfrmKolcsonkerok.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;
 Button1.Color := frmMainForm.Gombok;
 Button2.Color := frmMainForm.Gombok;
 Button4.Color := frmMainForm.Gombok;

 Label1.Font.Color := frmMainForm.Betuk;
 Label1.Canvas.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.FOnt.Color := frmMainForm.Betuk;
 Button1.Font.Color := frmMainForm.Betuk;
 Button2.Font.Color := frmMainForm.Betuk;
 Button4.Font.Color := frmMainForm.Betuk;
 txtCim.Font.Color := frmMainForm.Betuk;
 txtTelszam.Font.Color := frmMainForm.Betuk;
 cmbNev.Font.Color := frmMainForm.Betuk;

 CDNyDataFile_Kolcsonkerok1.OpenFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
 LoadList;

 cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmKolcsonkerok.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 CDNyDataFile_Kolcsonkerok1.SaveFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
 Action := caFree;
 frmKolcsonkerok := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmKolcsonkerok.Button4Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmKolcsonkerok.GetData;
var i, j:integer;
begin
 j := -1;
 for i := 1 to CDNyDataFile_Kolcsonkerok1.Mennyiseg do
  if CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev = cmbNev.Text then
   j := i;
// cmbNev.Text := ADOTable1.FieldByName('Nev').AsString;
 txtCim.Text := CDNyDataFile_Kolcsonkerok1.GetIndex(j).Cim;
 txtTelszam.Text := CDNyDataFile_Kolcsonkerok1.GetIndex(j).Telszam;
end;

procedure TfrmKolcsonkerok.cmbNevClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmKolcsonkerok.cmbNevKeyPress(Sender: TObject; var Key: Char);
var i, j: integer;
begin
 if Key = #13 then
 begin
  j := -1;
  for i := 1 to CDNyDataFile_Kolcsonkerok1.Mennyiseg do
   if CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev = cmbNev.Text then
    j := i;
  if j <> -1 then
   GetData
  else
  begin
   txtCim.Text := '';
   txtTelszam.Text := '';
   txtCim.SetFocus;
  end;
 end;
end;

procedure TfrmKolcsonkerok.SendData;
var i, j: integer;
begin
 if cmbNev.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 j := -1;
 for i := 1 to CDNyDataFile_Kolcsonkerok1.Mennyiseg do
  if CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev = cmbNev.Text then
   j := i;

 if j <> -1 then
 begin
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,kCim,txtCim.Text);
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,kTelszam,txtTelszam.Text);
 end
 else
 begin
  j := CDNyDataFile_Kolcsonkerok1.Mennyiseg + 1;
  CDNyDataFile_Kolcsonkerok1.Mennyiseg := j;
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,kNev,cmbNev.Text);
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,kCim,txtCim.Text);
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,kTelszam,txtTelszam.Text);
  cmbNev.Items.Add(cmbNev.Text);
 end;
end;

procedure TfrmKolcsonkerok.Button1Click(Sender: TObject);
begin
 SendData;
end;

procedure TfrmKolcsonkerok.Button2Click(Sender: TObject);
var Reply: integer;
    i: integer;
begin
 Reply := Application.MessageBox('Tényleg tölöd az aktuális rekordot?','CD-nyilvántartó',mb_YesNo + mb_IconQuestion);
 if Reply = id_No then Exit;

 for i := 1 to CDNyDataFile_Kolcsonkerok1.Mennyiseg do
  if CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev = cmbNev.Text then
   CDNyDataFile_Kolcsonkerok1.DeleteIndex(i);

 CDNyDataFile_Kolcsonkerok1.Mennyiseg := CDNyDataFile_Kolcsonkerok1.Mennyiseg - 1;

 cmbNev.Items.Delete(cmbNev.ItemIndex);

// cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmKolcsonkerok.LoadList;
var i: integer;
begin
 cmbNev.Items.Clear;
 for i := 1 to CDNyDataFile_Kolcsonkerok1.Mennyiseg do
  cmbNev.Items.Add(CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev);
end;

end.
