unit CDk;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Mask, ComCtrls, ExtCtrls, MainForm, IniFiles,
  ButtonWithColor, CDNyDataFile2, Toltes;

type
  TfrmCDk = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    Button4: TBitBtnWithColor;
    cmbCDNeve: TComboBox;
    cmbTartalom: TComboBox;
    txtPontosTartalom: TMemo;
    txtMegjegyzes: TMemo;
    txtHely: TEdit;
    cmbKolcsonkero: TComboBox;
    cbxKolcsonVanEKerve: TCheckBox;
    dtpKolcsonadasDatuma: TDateTimePicker;
    CDNyDataFile_CDk1: TCDNyCDk;
    CDNyDataFile_Kolcsonkerok1: TCDNyKolcsonkerok;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmbCDNeveClick(Sender: TObject);
    procedure cmbCDNeveKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure GetData;
    procedure SendData;
    procedure LoadList;
    procedure LoadData;
  public
    { Public declarations }
  end;

var
  frmCDk: TfrmCDk;

implementation

uses TorlesMegerositese;

{$R *.DFM}

procedure TfrmCDk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
// CDNyDataFile_CDk1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny');
 Action := caFree;
 frmCDk := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmCDk.FormShow(Sender: TObject);
var Cfg: TIniFile;
    i: integer;
begin
 LoadList;

 Cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 cmbTartalom.Items.Clear;
 for i := 1 to Cfg.ReadInteger('Categories','CategoryCount',1) do
  cmbTartalom.Items.Add(Cfg.ReadString('Categories','Cat' + IntToStr(i),'[hiba]'));
 Cfg.Free;

 cmbCDNeve.ItemIndex := 0;
 GetData;
end;

procedure TfrmCDk.Button4Click(Sender: TObject);
begin
 frmToltes.Label1.Caption := 'CDk mentése';
 frmToltes.Show;
 CDnyDataFile_CDk1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny',frmTOltes.Gauge1);
 frmToltes.Hide;
 Self.Close;
end;

procedure TfrmCDk.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;
 Button1.Color := frmMainForm.Gombok;
 Button2.Color := frmMainForm.Gombok;
 Button4.Color := frmMainForm.Gombok;

 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.Font.Color := frmMainForm.Betuk;
 Label4.Font.Color := frmMainForm.Betuk;
 Label4.Canvas.Font.Color := frmMainForm.Betuk;
 Label5.Font.Color := frmMainForm.Betuk;
 Label5.Canvas.Font.Color := frmMainForm.Betuk;
 Label6.Font.Color := frmMainForm.Betuk;
 Label6.Canvas.Font.Color := frmMainForm.Betuk;
 Label7.Font.Color := frmMainForm.Betuk;
 Label7.Canvas.Font.Color := frmMainForm.Betuk;
 Label8.Font.Color := frmMainForm.Betuk;
 Label8.Canvas.Font.Color := frmMainForm.Betuk;
 Button1.Font.Color := frmMainForm.Betuk;
 Button2.Font.Color := frmMainForm.Betuk;
 Button4.Font.Color := frmMainForm.Betuk;
 cmbCDNeve.Font.Color := frmMainForm.Betuk;
 cmbCDNeve.Canvas.Font.Color := frmMainForm.Betuk;
 cmbTartalom.Font.Color := frmMainForm.Betuk;
 cmbTartalom.Canvas.Font.Color := frmMainForm.Betuk;
 txtPontosTartalom.Font.Color := frmMainForm.Betuk;
 txtMegjegyzes.Font.Color := frmMainForm.Betuk;
 txtHely.Font.Color := frmMainForm.Betuk;
 cmbKolcsonkero.Font.Color := frmMainForm.Betuk;
 cmbKolcsonkero.Canvas.Font.Color := frmMainForm.Betuk;
 cbxKolcsonVanEKerve.Font.Color := frmMainForm.Betuk;
 dtpKolcsonadasDatuma.Font.Color := frmMainForm.Betuk;

// CDNyDataFile_CDk1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny');
// CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
// LoadList;
// cmbCDNeve.ItemIndex := 0;
// GetData;

 LoadData;
end;

procedure TfrmCDk.GetData;
var j: integer;
begin
 j := CDNyDataFile_CDk1.IndexOfItem(cmbCDNeve.Text);
 if j = 0 then
 begin
  Application.MessageBox('Hiba: nem találom a CDt!','CD-Nyilvántartó',mb_Ok + mb_IconError);
  cmbTartalom.Text := '[hiba]';
  txtPontosTartalom.Text := '[hiba]';
  txtHely.Text := '[hiba]';
  txtMegjegyzes.Text := '[hiba]';
  Abort;
 end;

 cmbTartalom.Text := CDNyDataFile_CDk1.GetIndex(j).Kategoria;
 txtPontosTartalom.Lines.Text := CDNyDataFile_CDk1.GetIndex(j).Tartalom;
 txtHely.Text := CDNyDataFile_CDk1.GetIndex(j).Hely;
 txtMegjegyzes.Text := CDNyDataFile_CDk1.GetIndex(j).Megjegyzes;
 cbxKolcsonVanEKerve.Checked := CDNyDataFile_CDk1.GetIndex(j).KolcsonVanEKerve;
 dtpKolcsonadasDatuma.Date := CDNyDataFile_CDk1.GetIndex(j).KolcsonkeresDatuma;
 cmbKolcsonkero.ItemIndex := cmbKolcsonkero.Items.IndexOf(CDNyDataFile_CDk1.GetIndex(j).Kolcsonkero);
end;

procedure TfrmCDk.cmbCDNeveClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmCDk.SendData;
var j: integer;
begin
 if cmbCDNeve.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 j := CDNyDataFile_CDk1.IndexOfItem(cmbCDNeve.Text);

 if j <> 0 then
 begin
  CDNyDataFile_CDk1.SetIndex(j,cmbCDNeve.Text,cmbTartalom.Text,txtPontosTartalom.Lines.Text,txtHely.Text,txtMegjegyzes.Text,cbxKolcsonVanEKerve.Checked,dtpKolcsonadasDatuma.Date,cmbKolcsonkero.Text);
 end
 else
 begin
  CDNyDataFile_CDk1.AddNew(cmbCDNeve.Text,cmbTartalom.Text,txtPontosTartalom.Lines.Text,txtHely.Text,txtMegjegyzes.Text,cbxKolcsonVanEKerve.Checked,dtpKolcsonadasDatuma.Date,cmbKolcsonkero.Text);
  cmbCDNeve.Items.Add(cmbCDNeve.Text);
 end;
end;

procedure TfrmCDk.cmbCDNeveKeyPress(Sender: TObject; var Key: Char);
var j: integer;
begin
 if Key = #13 then
 begin
  j := CDNyDataFile_CDk1.IndexOfItem(cmbCDNeve.Text);

  if j <> 0 then
   GetData
  else
  begin
   cmbTartalom.ItemIndex := 0;
   txtPontosTartalom.Lines.Text := '';
   txtHely.Text := '';
   txtMegjegyzes.Text := '';
   cbxKolcsonVanEKerve.Checked := false;
   dtpKolcsonadasDatuma.Date := Now;
   cmbKolcsonkero.ItemIndex := 0;
   cmbTartalom.SetFocus;
  end;
 end;
end;

procedure TfrmCDk.Button1Click(Sender: TObject);
begin
 SendData;
end;

procedure TfrmCDk.Button2Click(Sender: TObject);
var Reply: integer;
    j: integer;
begin
// Reply := Application.MessageBox('Tényleg törlöd az aktuális rekordot?','CD-nyilvántartó',mb_YesNo + mb_IconQuestion);
// if Reply = id_No then Exit;
 Reply := frmTorlesMegerositese.ShowModal;
 if (Reply = mrOK)and(frmTorlesMegerositese.CheckBox1.Checked) then
 begin
  j := CDNyDataFile_CDk1.IndexOfItem(cmbCDNeve.Text);

  if j = 0 then
  begin
   Application.MessageBox('Hiba: a rekordot nem lehet törölni.','CD-Nyilántartó',mb_Ok + mb_IconError);
   Abort;
  end;

  CDNyDataFile_CDk1.DeleteIndex(j);

  cmbCDNeve.Items.Delete(cmbCDNeve.Items.IndexOf(cmbCDNeve.Text));
  cmbCDNeve.ItemIndex := 0;
  GetData;
 end; 
end;

procedure TfrmCDk.LoadList;
begin
 cmbCDNeve.Items.Clear;
 cmbKolcsonkero.Items.Clear;

 cmbCDNeve.Items.Text := CDNyDataFile_CDk1.GetItems;
 cmbKolcsonkero.Items.Text := CDNyDataFile_Kolcsonkerok1.GetItems;
end;

procedure TfrmCDk.LoadData;
begin
 frmToltes.Label1.Caption := 'CDk betöltése';
 frmToltes.Show;
 Sleep(5);
 CDNyDataFile_CDk1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny',frmToltes.Gauge1);
 Sleep(5);
 frmToltes.Label1.Caption := 'Kölcsönkérõk betöltése';
 CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 Sleep(5);
 frmToltes.Hide;

 LoadList;

 cmbCDNeve.ItemIndex := 0;
 GetData;
end;

end.
