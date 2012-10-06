unit KolcsonkapottCDk;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ButtonWithColor, ComCtrls, CDNyDataFile2, IniFiles, Toltes;

type
  TfrmKolcsonkapottCDk = class(TForm)
    Label1: TLabel;
    cmbCDNeve: TComboBox;
    Label2: TLabel;
    dtpKolcsonkapasDatuma: TDateTimePicker;
    Label3: TLabel;
    cmbKolcsonado: TComboBox;
    Label4: TLabel;
    dtpHatarido: TDateTimePicker;
    cmdMentes: TBitBtnWithColor;
    cmdDelete: TBitBtnWithColor;
    cmdKesz: TBitBtnWithColor;
    CDNyKolcsonkapott1: TCDNyKolcsonkapott;
    CDNyKolcsonkerok1: TCDNyKolcsonkerok;
    procedure FormCreate(Sender: TObject);
    procedure GetData;
    procedure SetData;
    procedure cmdMentesClick(Sender: TObject);
    procedure cmdDeleteClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmbCDNeveClick(Sender: TObject);
    procedure cmbCDNeveKeyPress(Sender: TObject; var Key: Char);
    procedure cmdKeszClick(Sender: TObject);
    procedure LoadData;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKolcsonkapottCDk: TfrmKolcsonkapottCDk;

implementation

uses MainForm, TorlesMegerositese;

{$R *.DFM}

procedure TfrmKolcsonkapottCDk.FormCreate(Sender: TObject);
begin
// CDNyKolcsonkapott1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkapott.cdny');
// cmbCDNeve.Items.Clear;
// cmbCDNeve.Items.Text := CDNyKolcsonkapott1.GetItems;

// CDNyKolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');
// cmbKolcsonado.Items.Clear;
// cmbKolcsonado.Items.Text := CDNyKolcsonkerok1.GetItems;

 LoadData;

 cmbCDNeve.ItemIndex := 0;
 GetData;

 Self.Color := frmMainForm.HatterSzin;

 cmdMentes.Color := frmMainForm.Gombok;
 cmdDelete.Color := frmMainForm.Gombok;
 cmdKesz.Color := frmMainForm.Gombok;

 Label1.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label4.Font.Color := frmMainForm.Betuk;

 cmdMentes.Font.Color := frmMainForm.Betuk;
 cmdDelete.Font.Color := frmMainForm.Betuk;
 cmdKesz.Font.Color := frmMainForm.Betuk;
end;

procedure TfrmKolcsonkapottCDk.GetData;
var j: integer;
begin
 j := CDNyKolcsonkapott1.IndexOfItem(cmbCDNeve.Text);

 if j = 0 then
 begin
  Application.MessageBox('Hiba: nem találom a CDt!','CD-Nyilvántartó',mb_Ok + mb_IconError);
  dtpKolcsonkapasDatuma.Date := 0;
  cmbKolcsonado.ItemIndex := 0;
  dtpHatarido.Date := 0;
  Abort;
 end;

 dtpKolcsonkapasDatuma.Date := CDNyKolcsonkapott1.GetIndex(j).KolcsonkapasDatuma;
 cmbKolcsonado.ItemIndex := cmbKolcsonado.Items.IndexOf(CDNyKolcsonkapott1.GetIndex(j).Kolcsonado);
 dtpHatarido.Date := CDNyKolcsonkapott1.GetIndex(j).Hatarido;
end;

procedure TfrmKolcsonkapottCDk.SetData;
var j: integer;
begin
 if cmbCDNeve.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 j := CDNyKolcsonkapott1.IndexOfItem(cmbCDNeve.Text);

 if j <> 0 then
  CDNyKolcsonkapott1.SetIndex(j,cmbCDNeve.Text,dtpKolcsonkapasDatuma.Date,cmbKolcsonado.Text,dtpHatarido.Date)
 else
 begin
  CDNyKolcsonkapott1.AddNew(cmbCDNeve.Text,dtpKolcsonkapasDatuma.Date,cmbKolcsonado.Text,dtpHatarido.Date);
  cmbCDNeve.Items.Add(cmbCDNeve.Text);
 end;
end;

procedure TfrmKolcsonkapottCDk.cmdMentesClick(Sender: TObject);
begin
 SetData;
 frmToltes.Label1.Caption := 'Kölcsönkapott CDk mentése';
 frmToltes.Show;
 CDNyKolcsonkapott1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkapott.cdny',frmToltes.Gauge1);
 frmToltes.Hide;
end;

procedure TfrmKolcsonkapottCDk.cmdDeleteClick(Sender: TObject);
var a: integer;
begin
 a := frmTorlesMegerositese.ShowModal;

 if (a = mrOK)and(frmTorlesMegerositese.CheckBox1.Checked = true) then
 begin
  a := CDNyKolcsonkapott1.IndexOfItem(cmbCDNeve.Text);

  if a = 0 then
  begin
   Application.MessageBox('Hiba: a rekordot nem lehet törölni.','CD-Nyiilvántartó',mb_Ok + mb_IconError);
   Abort;
  end;

  CDNyKolcsonkapott1.DeleteIndex(a);
  cmbCDNeve.Items.Delete(cmbCDNeve.Items.IndexOf(cmbCDNeve.Text));
  cmbCDNeve.ItemIndex := 0;
  GetData;
 end;
end;

procedure TfrmKolcsonkapottCDk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 frmKolcsonkapottCDk := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmKolcsonkapottCDk.cmbCDNeveClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmKolcsonkapottCDk.cmbCDNeveKeyPress(Sender: TObject;var Key: Char);
var j: integer;
    TurelmiIdo: integer;
    Cfg: TIniFile;
begin
 if Key = #13 then
 begin
  j := CDNyKolcsonkerok1.IndexOfItem(cmbCDNeve.Text);

  if j <> 0 then
   GetData
  else
  begin
   Cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
   TurelmiIdo := Cfg.ReadInteger('TurelmiIdo','Napok',30);
   Cfg.Free;

   dtpKolcsonkapasDatuma.Date := Now;
   cmbKolcsonado.ItemIndex := 0;
   dtpHatarido.Date := Now + TurelmiIdo;
  end;
 end;
end;

procedure TfrmKolcsonkapottCDk.cmdKeszClick(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmKolcsonkapottCDk.LoadData;
begin
 frmToltes.Label1.Caption := 'Kölcsönkapott CDk betöltése';
 frmToltes.Show;
 Sleep(5);
 CDNyKolcsonkapott1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkapott.cdny',frmToltes.Gauge1);
 cmbCDNeve.Items.Clear;
 cmbCDNeve.Items.Text := CDNyKolcsonkapott1.GetItems;
 Sleep(5);
 frmToltes.Label1.Caption := 'Kölcsönkérõk betöltöse';
 CDNyKolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 cmbKolcsonado.Items.Clear;
 cmbKolcsonado.Items.Text := CDNyKolcsonkerok1.GetItems;
 Sleep(5);
 frmToltes.Hide;
end;

end.
