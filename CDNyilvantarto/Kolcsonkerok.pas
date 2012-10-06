unit Kolcsonkerok;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MainForm, OleCtrls, ButtonWithColor, CDNyDataFile2,
  Toltes;

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
    CDNyDataFile_Kolcsonkerok1: TCDNyKolcsonkerok;
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
    procedure LoadData;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKolcsonkerok: TfrmKolcsonkerok;

implementation

uses TorlesMegerositese;

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

// CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
// LoadList;

// cmbNev.ItemIndex := 0;
// GetData;

 LoadData;
end;

procedure TfrmKolcsonkerok.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// CDNyDataFile_Kolcsonkerok1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
 Action := caFree;
 frmKolcsonkerok := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmKolcsonkerok.Button4Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmKolcsonkerok.GetData;
var j: integer;
begin
 j := CDNyDataFile_Kolcsonkerok1.IndexOfItem(cmbNev.Text);
// cmbNev.Text := ADOTable1.FieldByName('Nev').AsString;
 txtCim.Text := CDNyDataFile_Kolcsonkerok1.GetIndex(j).Cim;
 txtTelszam.Text := CDNyDataFile_Kolcsonkerok1.GetIndex(j).Telszam;
end;

procedure TfrmKolcsonkerok.cmbNevClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmKolcsonkerok.cmbNevKeyPress(Sender: TObject; var Key: Char);
var j: integer;
begin
 if Key = #13 then
 begin
  j := CDNyDataFile_Kolcsonkerok1.IndexOfItem(cmbNev.Text);
  if j <> 0 then
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
var j: integer;
begin
 if cmbNev.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 j := CDNyDataFile_Kolcsonkerok1.IndexOfItem(cmbNev.Text);

 if j <> 0 then
 begin
  CDNyDataFile_Kolcsonkerok1.SetIndex(j,cmbNev.Text,txtCim.Text,txtTelszam.Text);
 end
 else
 begin
  CDNyDataFile_Kolcsonkerok1.AddNew(cmbNev.Text,txtCim.Text,txtTelszam.Text);
  cmbNev.Items.Add(cmbNev.Text);
 end;

 frmToltes.Gauge1.Progress := 0;
 frmToltes.Show;
 CDNyDataFile_Kolcsonkerok1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 frmToltes.Hide;
end;

procedure TfrmKolcsonkerok.Button1Click(Sender: TObject);
begin
 SendData;
end;

procedure TfrmKolcsonkerok.Button2Click(Sender: TObject);
var Reply: integer;
    i: integer;
begin
// Reply := Application.MessageBox('Tényleg tölöd az aktuális rekordot?','CD-nyilvántartó',mb_YesNo + mb_IconQuestion);
// if Reply = id_No then Exit;
 Reply := frmTorlesMegerositese.ShowModal;
 if (Reply = mrOK)and(frmTorlesMegerositese.CheckBox1.Checked) then
 begin
  i := CDNyDataFile_Kolcsonkerok1.IndexOfItem(cmbNev.Text);
   CDNyDataFile_Kolcsonkerok1.DeleteIndex(i);

  cmbNev.Items.Delete(cmbNev.ItemIndex);

  cmbNev.ItemIndex := 0;
  GetData;
 end; 
end;

procedure TfrmKolcsonkerok.LoadList;
begin
 cmbNev.Items.Clear;
 cmbNev.Items.Text := CDNyDataFile_Kolcsonkerok1.GetItems;
end;

procedure TfrmKolcsonkerok.LoadData;
begin
 frmToltes.Label1.Caption := 'Kölcsönkérõk betöltése';
 frmToltes.Show;
 Sleep(5);
 CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 Sleep(5);
 frmToltes.Hide;

 LoadList;

 cmbNev.ItemIndex := 0;
 GetData;
end;

end.
