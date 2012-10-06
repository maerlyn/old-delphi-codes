unit Kereses;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ComCtrls, IniFiles, ButtonWithColor,
  CDNyDataFile2, Toltes;

type
  TfrmKereses = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    cbxKolcsonkeroNev: TCheckBox;
    cbxKolcsonkeroCim: TCheckBox;
    cbxKolcsonkeroTelszam: TCheckBox;
    txtKolcsonkeroNev: TEdit;
    txtKolcsonkeroCim: TEdit;
    txtKolcsonkeroTelszam: TEdit;
    Timer1: TTimer;
    cmdKolcsonkerokKereses: TBitBtnWithColor;
    cmdKolcsonkerokMegse: TBitBtnWithColor;
    cbxCDNev: TCheckBox;
    cbxCDTartalom: TCheckBox;
    cbxCDPontosTartalom: TCheckBox;
    cbxCDHely: TCheckBox;
    cbxCDMegjegyzes: TCheckBox;
    cbxCDKolcsonVanEKerve: TCheckBox;
    cbxCDKolcsonkeresDatuma: TCheckBox;
    cbxCDKolcsonkero: TCheckBox;
    txtCDNev: TEdit;
    txtCDTartalom: TEdit;
    txtCDPontosTartalom: TEdit;
    txtCDHely: TEdit;
    txtCDMegjegyzes: TEdit;
    txtCDKolcsonkero: TEdit;
    cmdCDKereses: TBitBtnWithColor;
    cmdCDMegse: TBitBtnWithColor;
    cbxCDKolcsonVanEKerveErtek: TCheckBox;
    dtpCDKolcsonkeresDatuma: TDateTimePicker;
    ListBox1: TListBox;
    CDNyDataFile_CDk1: TCDNyCDk;
    CDNyDataFile_Kolcsonkerok1: TCDNyKolcsonkerok;
    procedure cbxKolcsonkeroNevClick(Sender: TObject);
    procedure cbxKolcsonkeroCimClick(Sender: TObject);
    procedure cbxKolcsonkeroTelszamClick(Sender: TObject);
    procedure cbxCDNevClick(Sender: TObject);
    procedure cbxCDTartalomClick(Sender: TObject);
    procedure cbxCDPontosTartalomClick(Sender: TObject);
    procedure cbxCDHelyClick(Sender: TObject);
    procedure cbxCDMegjegyzesClick(Sender: TObject);
    procedure cbxCDKolcsonVanEKerveClick(Sender: TObject);
    procedure cbxCDKolcsonkeresDatumaClick(Sender: TObject);
    procedure cbxCDKolcsonkeroClick(Sender: TObject);
    procedure cmdKolcsonkerokKeresesClick(Sender: TObject);
    procedure cmdCDKeresesClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdCDMegseClick(Sender: TObject);
    procedure cmdKolcsonkerokMegseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbxCDKolcsonVanEKerveErtekClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1DblClick(Sender: TObject);
    procedure PageControl1DrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKereses: TfrmKereses;
  HanyKolcsonkerosVanBeixelve: integer = 1;
  HanyCDsVanBeixelve: integer = 1;
  Kepsorszam: integer = 1;
  MelyikbenKeresel: string;
  MelyikbenKerestelUtoljara: string;
  Feliratok: array[boolean] of string;

const
  FulFeliratok: array[0..1] of string = ('Kölcsönkérõ','CD');

implementation

uses Kolcsonkerok, CDk, MainForm;

{$R *.DFM}

procedure TfrmKereses.cbxKolcsonkeroNevClick(Sender: TObject);
begin
 txtKolcsonkeroNev.Enabled := cbxKolcsonkeroNev.Checked;
 if cbxKolcsonkeroNev.Checked then
  Inc(HanyKolcsonkerosVanBeixelve)
 else
  Dec(HanyKolcsonkerosVanBeixelve);
end;

procedure TfrmKereses.cbxKolcsonkeroCimClick(Sender: TObject);
begin
 txtKolcsonkeroCim.Enabled := cbxKolcsonkeroCim.Checked;
 if cbxKolcsonkeroNev.Checked then
  Inc(HanyKolcsonkerosVanBeixelve)
 else
  Dec(HanyKolcsonkerosVanBeixelve);
end;

procedure TfrmKereses.cbxKolcsonkeroTelszamClick(Sender: TObject);
begin
 txtKolcsonkeroTelszam.Enabled := cbxKolcsonkeroTelszam.Checked;
 if cbxKolcsonkeroTelszam.Checked then
  Inc(HanyKolcsonkerosVanBeixelve)
 else
  Dec(HanyKolcsonkerosVanBeixelve);
end;

procedure TfrmKereses.cbxCDNevClick(Sender: TObject);
begin
 txtCDNev.Enabled := cbxCDNev.Checked;
 if cbxCDNev.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDTartalomClick(Sender: TObject);
begin
 txtCDTartalom.Enabled := cbxCDTartalom.Checked;
 if cbxCDTartalom.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDPontosTartalomClick(Sender: TObject);
begin
 txtCDPontosTartalom.Enabled := cbxCDPontosTartalom.Checked;
 if cbxCDPontosTartalom.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDHelyClick(Sender: TObject);
begin
 txtCDHely.Enabled := cbxCDHely.Checked;
 if cbxCDHely.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDMegjegyzesClick(Sender: TObject);
begin
 txtCDMegjegyzes.Enabled := cbxCDMegjegyzes.Checked;
 if cbxCDMegjegyzes.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDKolcsonVanEKerveClick(Sender: TObject);
begin
 cbxCDKolcsonVanEKerveErtek.Enabled := cbxCDKolcsonVanEKerve.Checked;
 if cbxCDKolcsonVanEKerve.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDKolcsonkeresDatumaClick(Sender: TObject);
begin
 dtpCDKolcsonkeresDatuma.Enabled := cbxCDKolcsonkeresDatuma.Checked;
 if cbxCDKolcsonkeresDatuma.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cbxCDKolcsonkeroClick(Sender: TObject);
begin
 txtCDKolcsonkero.Enabled := cbxCDKolcsonkero.Checked;
 if cbxCDKolcsonkero.Checked then
  Inc(HanyCDsVanBeixelve)
 else
  Dec(HanyCDsVanBeixelve);
end;

procedure TfrmKereses.cmdKolcsonkerokKeresesClick(Sender: TObject);
var i: integer;
    HanyFeltetelVan, HanyFeltetelOK: integer;
begin
 if HanyKolcsonkerosVanBeixelve = 0 then
 begin
  Application.MessageBox('A semmire nem keresek rá!','CD-Nyilvántartó',mb_Ok + mb_IconHand);
  Exit;
 end;

 if Self.Width = 545 then
  for i := 545 downto 297 do
   Self.Width := i;

 MelyikbenKeresel := 'Kolcsonkerok';
 cmdKolcsonkerokKereses.Enabled := false;
 Timer1.Enabled := true;

 Screen.Cursor := crHourGlass;

 HanyFeltetelVan := HanyKolcsonkerosVanBeixelve;
 HanyFeltetelOK := 0;
 frmToltes.Label1.Caption := 'Kölcsönkérõk betöltése';
 frmToltes.Show;
 CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 frmToltes.Hide;
 ListBox1.Items.Clear;
 frmToltes.Label1.Caption := 'Keresés...';
 frmToltes.Gauge1.Progress := 0;
 frmToltes.Gauge1.MaxValue := CDNyDataFile_Kolcsonkerok1.Count;
 frmToltes.Show;
 for i := 1 to CDNyDataFile_Kolcsonkerok1.Count do
 begin
  if cbxKolcsonkeroNev.Checked then
   if Pos(LowerCase(txtKolcsonkeroNev.Text),LowerCase(CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxKolcsonkeroCim.Checked then
   if Pos(LowerCase(txtKolcsonkeroCim.Text),LowerCase(CDNyDataFile_Kolcsonkerok1.GetIndex(i).Cim)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxKolcsonkeroTelszam.Checked then
   if Pos(LowerCase(txtKolcsonkeroTelszam.Text),LowerCase(CDNyDataFile_Kolcsonkerok1.GetIndex(i).Telszam)) > 0 then
    Inc(HanyFeltetelOK);
  if HanyFeltetelVan = HanyFeltetelOK then
  begin
   ListBox1.Items.Add(CDNyDataFile_Kolcsonkerok1.GetIndex(i).Nev);
  end;
  HanyFeltetelOK := 0;
//  Timer1Timer(Self);
  Application.ProcessMessages;
  frmToltes.Gauge1.Progress := i;
 end;
 frmToltes.Hide;
 for i := 297 to 545 do
  Self.Width := i;
 Screen.Cursor := crDefault;

 cmdKolcsonkerokKereses.Enabled := true;
 MelyikbenKeresel := '';
 MelyikbenKerestelUtoljara := 'Kolcsonkerok';
 Timer1.Enabled := false;
end;

procedure TfrmKereses.cmdCDKeresesClick(Sender: TObject);
var i: integer;
    HanyFeltetelVan, HanyFeltetelOK: integer;
begin
 if HanyCDsVanBeixelve = 0 then
 begin
  Application.MessageBox('Meg kell adni legalább 1 keresési feltételt!','CD-Nyilvántartó',mb_Ok + mb_IconHand);
  Exit;
 end;

 MelyikbenKeresel := 'CDk';
 cmdCDKereses.Enabled := false;
 Timer1.Enabled := true;

 if Self.Width = 545 then
  for i := 545 downto 297 do
   Self.Width := i;

 HanyFeltetelVan := HanyCDsVanBeixelve;
 HanyFeltetelOK := 0;
 frmToltes.Label1.Caption := 'CDk betöltése';
 frmToltes.Show;
 CDNyDataFile_CDk1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny',frmToltes.Gauge1);
 frmTOltes.Hide;
 ListBox1.Items.Clear;
 Screen.Cursor := crHourGlass;
 frmToltes.Label1.Caption := 'Keresés...';
 frmToltes.Gauge1.Progress := 0;
 frmToltes.Gauge1.MaxValue := CDNyDataFile_CDk1.Count;
 frmToltes.Show;
 for i := 1 to CDNyDataFile_CDk1.Count do
 begin
  if cbxCDNev.Checked then
   if Pos(LowerCase(txtCDNev.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).CDNeve)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxCDTartalom.Checked then
   if Pos(LowerCase(txtCDTartalom.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).Kategoria)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxCDPontosTartalom.Checked then
   if Pos(LowerCase(txtCDPontosTartalom.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).Tartalom)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxCDHely.Checked then
   if Pos(LowerCase(txtCDHely.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).Hely)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxCDMegjegyzes.Checked then
   if Pos(LowerCase(txtCDMegjegyzes.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).Megjegyzes)) > 0 then
    Inc(HanyFeltetelOK);
  if cbxCDKolcsonVanEKerve.Checked then
   if cbxCDKolcsonVanEKerveErtek.Checked = CDNyDataFile_CDk1.GetIndex(i).KolcsonVanEKerve then
    Inc(HanyFeltetelOK);
  if cbxCDKolcsonkeresDatuma.Checked then
   if trunc(dtpCDKolcsonkeresDatuma.Date) = trunc(CDNyDataFile_CDk1.GetIndex(i).KolcsonkeresDatuma) then
    Inc(HanyFeltetelOK);
  if cbxCDKolcsonkero.Checked then
   if Pos(LowerCase(txtCDKolcsonkero.Text),LowerCase(CDNyDataFile_CDk1.GetIndex(i).Kolcsonkero)) > 0 then
    Inc(HanyFeltetelOK);

  if HanyFeltetelVan = HanyFeltetelOK then
   ListBox1.Items.Add(CDNyDataFile_CDk1.GetIndex(i).CDNeve);
  HanyFeltetelOK := 0;
  Application.ProcessMessages;
//  Timer1Timer(Self);
  frmToltes.Gauge1.Progress := i;
 end;
 frmToltes.Hide;
 for i := 297 to 545 do
  Self.Width := i;
 Screen.Cursor := crDefault;

 MelyikbenKeresel := '';
 MelyikbenKerestelUtoljara := 'CDk';
 cmdCDKereses.Enabled := true;
 Timer1.Enabled := false;
end;

procedure TfrmKereses.Timer1Timer(Sender: TObject);
begin
 inc(Kepsorszam);
 if Kepsorszam = 9 then Kepsorszam := 1;
 Self.Icon.Handle := LoadIcon(HInstance,PChar('SEARCH' + IntToStr(Kepsorszam)));
 if MelyikbenKeresel = 'Kolcsonkerok' then
  cmdKolcsonkerokKereses.Glyph.LoadFromResourceName(HInstance,'SEARCH' + IntToStr(Kepsorszam))
 else
  cmdCDKereses.Glyph.LoadFromResourceName(HInstance,'SEARCH' + IntToStr(Kepsorszam));
end;

procedure TfrmKereses.PageControl1Change(Sender: TObject);
begin
 if not Timer1.Enabled then Exit;
 if MelyikbenKeresel = 'Kolcsonkerok' then
  PageControl1.ActivePage := TabSheet1
 else
  PageControl1.ActivePage := TabSheet2;
end;

procedure TfrmKereses.FormCreate(Sender: TObject);
var temp: string;
    tCfg: TIniFile;
    Bmp: TBitmap;
begin
 tCfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 temp := tCfg.ReadString('Skin','Actual','default');
 temp := ExtractFilePath(ParamStr(0)) + 'skins\' + temp + '\';
 tCfg.Free;
 Bmp := TBitmap.Create;
 if FileExists(temp + 'kereses.bmp') then
 begin
  Bmp.LoadFromFile(temp + 'kereses.bmp');
  if (Bmp.Width = 16)and(Bmp.Height = 16) then
  begin
   cmdKolcsonkerokKereses.Glyph.LoadFromFile(temp + 'kereses.bmp');
   cmdKolcsonkerokKereses.NumGlyphs := 1;
   cmdCDKereses.Glyph.LoadFromFile(temp + 'kereses.bmp');
   cmdCDKereses.NumGlyphs := 1;
  end;
 end;
 if FileExists(temp + 'megse.bmp') then
 begin
  Bmp.LoadFromFile(temp + 'megse.bmp');
  if (Bmp.Width = 18)and(Bmp.Height = 18) then
  begin
   cmdKolcsonkerokMegse.Glyph.LoadFromFile(temp + 'megse.bmp');
   cmdKolcsonkerokMegse.NumGlyphs := 1;
   cmdCDMegse.Glyph.LoadFromFile(temp + 'megse.bmp');
   cmdCDMegse.NumGlyphs := 1;
  end;
 end;
 Bmp.Free;

 Self.Color := frmMainForm.HatterSzin;
 cmdKolcsonkerokKereses.Color := frmMainForm.Gombok;
 cmdKolcsonkerokMegse.Color := frmMainForm.Gombok;
 cmdCDKereses.Color := frmMainForm.Gombok;
 cmdCDMegse.Color := frmMainForm.Gombok;

 PageControl1.Font.Color := frmMainForm.Betuk;
 PageControl1.Canvas.Font.Color := frmMainForm.Betuk;
 TabSheet1.Font.Color := frmMainForm.Betuk;
 TabSheet2.Font.Color := frmMainForm.Betuk;
 cbxKolcsonkeroNev.Font.Color := frmMainForm.Betuk;
 cbxKolcsonkeroCim.Font.Color := frmMainForm.Betuk;
 cbxKolcsonkeroTelszam.Font.Color := frmMainForm.Betuk;
 txtKolcsonkeroNev.Font.Color := frmMainForm.Betuk;
 txtKolcsonkeroCim.Font.Color := frmMainForm.Betuk;
 txtKolcsonkeroTelszam.Font.Color := frmMainForm.Betuk;
 cmdKolcsonkerokKereses.Font.Color := frmMainForm.Betuk;
 cmdKolcsonkerokMegse.Font.Color := frmMainForm.Betuk;
 cbxCDNev.Font.Color := frmMainForm.Betuk;
 cbxCDTartalom.Font.Color := frmMainForm.Betuk;
 cbxCDPontosTartalom.Font.Color := frmMainForm.Betuk;
 cbxCDHely.Font.Color := frmMainForm.Betuk;
 cbxCDMegjegyzes.Font.Color := frmMainForm.Betuk;
 cbxCDKolcsonVanEKerve.Font.Color := frmMainForm.Betuk;
 cbxCDKolcsonkeresDatuma.Font.Color := frmMainForm.Betuk;
 cbxCDKolcsonkero.Font.Color := frmMainForm.Betuk;
 txtCDNev.Font.Color := frmMainForm.Betuk;
 txtCDTartalom.Font.Color := frmMainForm.Betuk;
 txtCDPontosTartalom.Font.Color := frmMainForm.Betuk;
 txtCDHely.Font.Color := frmMainForm.Betuk;
 txtCDMegjegyzes.Font.Color := frmMainForm.Betuk;
 txtCDKolcsonkero.Font.Color := frmMainForm.Betuk;
 cmdCDKereses.Font.Color := frmMainForm.Betuk;
 cmdCDMegse.Font.Color := frmMainForm.Betuk;
 cbxCDKolcsonVanEKerveErtek.Font.Color := frmMainForm.Betuk;
 dtpCDKolcsonkeresDatuma.Font.Color := frmMainForm.Betuk;
 ListBox1.Font.Color := frmMainForm.Betuk;
 ListBox1.Canvas.Font.Color := frmMainForm.Betuk;

 PageControl1.ActivePage := TabSheet1;
 Feliratok[true] := 'igen';
 Feliratok[false] := 'nem';
end;

procedure TfrmKereses.cmdCDMegseClick(Sender: TObject);
var i:integer;
begin
 if Self.Width = 545 then
  for i := 545 downto 297 do
   Self.Width := i;

 Timer1.Enabled := false;
 cmdCDKereses.Enabled := true;
 MelyikbenKeresel := '';
 Self.Close;
end;

procedure TfrmKereses.cmdKolcsonkerokMegseClick(Sender: TObject);
var i: integer;
begin
 if Self.Width = 545 then
  for i := 545 downto 297 do
   Self.Width := i;

 Timer1.Enabled := false;
 cmdKolcsonkerokKereses.Enabled := true;
 MelyikbenKeresel := '';
 Self.Close;
end;

procedure TfrmKereses.FormShow(Sender: TObject);
begin
 Self.Width := 297;
end;

procedure TfrmKereses.cbxCDKolcsonVanEKerveErtekClick(Sender: TObject);
begin
 cbxCDKolcsonVanEKerveErtek.Caption := Feliratok[cbxCDKolcsonVanEKerveErtek.Checked];
end;

procedure TfrmKereses.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 frmKereses := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmKereses.ListBox1DblClick(Sender: TObject);
var MireKattintottSzam: integer;
    MireKattintottSzoveg: string;
    Kolcs: TfrmKolcsonkerok;
    CDkf: TfrmCDk;
begin
 MireKattintottSzam := ListBox1.ItemIndex;

 MireKattintottSzoveg := ListBox1.Items.Strings[MireKattintottSzam];

 if MelyikbenKerestelUtoljara = 'Kolcsonkerok' then
 begin
  Kolcs := TfrmKolcsonkerok.Create(Application);
  Kolcs.cmbNev.ItemIndex := Kolcs.cmbNev.Items.IndexOf(MireKattintottSzoveg);
  Kolcs.GetData;
 end
 else
 begin
  CDkf := TfrmCDk.Create(Application);
  CDkf.cmbCDNeve.ItemIndex := CDkf.cmbCDNeve.Items.IndexOf(MireKattintottSzoveg);
  CDkf.GetData;
 end;
end;

procedure TfrmKereses.PageControl1DrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
 Control.Canvas.Brush.Style := bsSolid;
 Control.Canvas.Brush.Color := frmMainForm.Fulek;
 Control.Canvas.FillRect(Rect);
 Control.Canvas.TextOut(Rect.Left+4,Rect.Top+2,FulFeliratok[TabIndex]);
end;

end.

