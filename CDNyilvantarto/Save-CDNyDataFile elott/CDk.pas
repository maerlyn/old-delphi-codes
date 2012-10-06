unit CDk;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, DBCtrls, Mask, Db, ComCtrls, ExtCtrls, MainForm, IniFiles,
  ButtonWithColor, dbf;

type
  TfrmCDk = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ADOTable1: TDbf;
    ADOTable2: TDbf;
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
    ADOTable3: TDbf;
    dtpKolcsonadasDatuma: TDateTimePicker;
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
  public
    { Public declarations }
  end;

var
  frmCDk: TfrmCDk;

implementation

{$R *.DFM}

procedure TfrmCDk.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 cmbCDNeve.Items.SaveToFile(ADOTable1.FilePath + '\CDk.cdny');

 Action := caFree;
 frmCDk := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmCDk.FormShow(Sender: TObject);
var Cfg: TIniFile;
    i: integer;
begin
 ADOTable1.Refresh;
 cmbCDNeve.Items.Clear;
 ADOTable1.First;
{ while not ADOTable1.Eof do
 begin
  cmbCDNeve.Items.Add(ADOTable1.FieldByName('CDNeve').AsString);
  ADOTable1.Next;
  Application.ProcessMessages;
 end;}

 ADOTable2.Refresh;
 cmbKolcsonkero.Items.Clear;
 ADOTable2.First;
{ while not ADOTable2.Eof do
 begin
  cmbKolcsonkero.Items.Add(ADOTable2.FieldByName('Nev').AsString);
  ADOTable2.Next;
  Application.ProcessMessages;
 end;}
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
 ADOTable1.Close;
 ADOTable2.Close;
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

 ADOTable1.FilePath := ExtractFilePath(ParamStr(0)) + 'Data';
 ADOTable2.FilePath := ADOTable1.FilePath;
 ADOTable3.FilePath := ADOTable1.FilePath;

 ADOTable1.Open;
 ADOTable2.Open;
 cmbCDNeve.ItemIndex := 0;
 GetData;
end;

procedure TfrmCDk.GetData;
begin
 ADOTable1.Locate('CDNeve',cmbCDNeve.Text,[]);
 cmbTartalom.Text := ADOTable1.FieldByName('Kategoria').AsString;
 txtPontosTartalom.Text := ADOTable1.FieldByName('Tartalom').AsString;
 txtHely.Text := ADOTable1.FieldByName('Hely').AsString;
 txtMegjegyzes.Text := ADOTable1.FieldByName('Megjegyzes').AsString;
 cbxKolcsonVanEKerve.Checked := ADOTable1.FieldByName('KolcsonVan').AsBoolean;
 dtpKolcsonadasDatuma.Date := ADOTable1.FieldByName('Datum').AsDateTime;
 cmbKolcsonkero.ItemIndex := cmbKolcsonkero.Items.IndexOf(ADOTable1.FieldByName('Kolcsonker').AsString);
end;

procedure TfrmCDk.cmbCDNeveClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmCDk.SendData;
begin
 if cmbCDNeve.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');

 if ADOTable1.Locate('CDNeve',cmbCDNeve.Text,[]) then
 begin
  ADOTable1.Edit;
  ADOTable1.FieldByName('Kategoria').AsString := cmbTartalom.Text;
  ADOTable1.FieldByName('Tartalom').AsString := txtPontosTartalom.Text;
  ADOTable1.FieldByName('Hely').AsString := txtHely.Text;
  ADOTable1.FieldByName('Megjegyzes').AsString := txtMegjegyzes.Text;
  ADOTable1.FieldByName('KolcsonVan').AsBoolean := cbxKolcsonVanEKerve.Checked;
  ADOTable1.FieldByName('Datum').AsDateTime := dtpKolcsonadasDatuma.Date;
  ADOTable1.FieldByName('Kolcsonker').AsString := cmbKolcsonkero.Text;
  ADOTable1.Post;
 end
 else
 begin
  ADOTable1.InsertRecord([cmbCDNeve.Text,
                          cmbTartalom.Text,
                          txtPontosTartalom.Text,
                          txtHely.Text,
                          txtMegjegyzes.Text,
                          cbxKolcsonVanEKerve.Checked,
                          dtpKolcsonadasDatuma.Date,
                          cmbKolcsonkero.Text]);
  cmbCDNeve.Items.Add(cmbCDNeve.Text);
 end;
end;

procedure TfrmCDk.cmbCDNeveKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
 begin
  if ADOTable1.Locate('CDNeve',cmbCDNeve.Text,[]) then
   GetData
  else
  begin
   ADOTable3.Open;
   ADOTable3.First;
   cmbTartalom.ItemIndex := 0;
   txtPontosTartalom.Text := '';
   txtHely.Text := '';
   txtMegjegyzes.Text := '';
   cbxKolcsonVanEKerve.Checked := false;
   dtpKolcsonadasDatuma.Date := Now;
   cmbKolcsonkero.Text := '';
   cmbTartalom.SetFocus;
   ADOTable3.Close;
  end;
 end;
end;

procedure TfrmCDk.Button1Click(Sender: TObject);
begin
 SendData;
end;

procedure TfrmCDk.Button2Click(Sender: TObject);
var Reply: integer;
begin
 Reply := Application.MessageBox('Tényleg törlöd az aktuális rekordot?','CD-nyilvántartó',mb_YesNo + mb_IconQuestion);
 if Reply = id_No then Exit;
 ADOTable1.Delete;
 ADOTable1.Next;
 ADOTable2.Refresh;
 LoadList;
end;

procedure TfrmCDk.LoadList;
begin
 cmbCDNeve.Items.Clear;
 cmbCDNeve.Items.LoadFromFile(ADOTable1.FilePath + '\CDk.cdny');

 cmbKolcsonkero.Items.Clear;
 cmbKolcsonkero.Items.LoadFromFile(ADOTable1.FilePath + '\Kolcsonkerok.cdny');
end;

end.
