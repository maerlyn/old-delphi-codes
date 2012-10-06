unit Beallitasok;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
  StdCtrls, Buttons, Spin, ComCtrls, FileCtrl, ButtonWithColor;

type
  TfrmBeallitasok = class(TForm)
    PageControl1: TPageControl;
    TabSheet5: TTabSheet;
    Label1: TLabel;
    sedTurelmiIdo: TSpinEdit;
    sedForintPerNap: TSpinEdit;
    Label2: TLabel;
    BitBtn1: TBitBtnWithColor;
    BitBtn2: TBitBtnWithColor;
    cbxProgramInditasakor: TCheckBox;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    txtNeved: TEdit;
    txtCimed: TEdit;
    TabSheet3: TTabSheet;
    Label5: TLabel;
    lstSkinek: TListBox;
    dlbSkinek: TDirectoryListBox;
    lblSkin: TLabel;
    ListBox1: TListBox;
    TabSheet1: TTabSheet;
    chkKilepesiEffekt: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    SpinEdit1: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1DrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure ListBox1Click(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBeallitasok: TfrmBeallitasok;
  Cfg: TIniFile;

const
  Feliratok: array[0..3] of string = ('Türelmi idõ','Kategóriák','Számla','Skin');

implementation

uses MainForm;

{$R *.DFM}

procedure TfrmBeallitasok.BitBtn1Click(Sender: TObject);
var i: integer;
    kijelolt: integer;
    Konyvtar: string;
    tKep: TBitmap;
    Old: TRect;
    tCfg: TIniFIle;
begin
 Cfg.WriteInteger('TurelmiIdo','Napok',sedTurelmiIdo.Value);
 Cfg.WriteInteger('TurelmiIdo','ForintPerNap',sedForintPerNap.Value);
 Cfg.WriteBool('TurelmiIdo','IndulaskorEllenorzes',cbxProgramInditasakor.Checked);

 Cfg.WriteBool('Altalanos','KilepesiEffekt',chkKilepesiEffekt.Checked);
 Cfg.WriteInteger('Altalanos','Lepes',SpinEdit1.Value);

 for i := 1 to Memo1.Lines.Count do
  Cfg.WriteString('Categories','Cat' + IntToStr(i),Memo1.Lines[i-1]);

 Cfg.WriteString('Szamla','Neved',txtNeved.Text);
 Cfg.WriteString('Szamla','Cimed',txtCimed.Text);

 kijelolt := -1;
 for i := 0 to lstSkinek.Items.Count - 1 do
  if lstSkinek.Selected[i] then
   kijelolt := i;
 if kijelolt <> -1 then
  Cfg.WriteString('Skin','Actual',lstSkinek.Items[kijelolt]);

 Konyvtar := Cfg.ReadString('Skin','Actual','[Hiba]');
 if Konyvtar <> '[Hiba]' then
 begin
  Konyvtar := ExtractFilePath(ParamStr(0)) + 'skins\' + Konyvtar + '\';
  tKep := TBitmap.Create;
  if FileExists(Konyvtar + '1.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '1.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdKolcsonkerok.Glyph.LoadFromFile(Konyvtar + '1.bmp');
  end;
  if FileExists(Konyvtar + '2.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '2.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdCDk.Glyph.LoadFromFile(Konyvtar + '2.bmp');
  end;
  if FileExists(Konyvtar + '3.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '3.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdKolcsonadas.Glyph.LoadFromFile(Konyvtar + '3.bmp');
  end;
  if FileExists(Konyvtar + '4.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '4.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdVisszakapas.Glyph.LoadFromFile(Konyvtar + '4.bmp');
  end;
  if FileExists(Konyvtar + '5.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '5.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdKolcsonkapas.Glyph.LoadFromFile(Konyvtar + '5.bmp');
  end;
  if FileExists(Konyvtar + '6.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '6.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdKereses.Glyph.LoadFromFile(Konyvtar + '6.bmp');
  end;
  if FileExists(Konyvtar + '7.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '7.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdBeallitasok.Glyph.LoadFromFile(Konyvtar + '7.bmp');
  end;
  if FileExists(Konyvtar + '8.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '8.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdAbout.Glyph.LoadFromFile(Konyvtar + '8.bmp');
  end;
  if FileExists(Konyvtar + '9.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '9.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    frmMainForm.cmdKilepes.Glyph.LoadFromFile(Konyvtar + '9.bmp');
  end;
  if FileExists(Konyvtar + 'bg.bmp') then
   frmMainForm.proHatterkep.LoadFromFile(Konyvtar + 'bg.bmp')
  else
   frmMainForm.proHatterkep.LoadFromResourceName(HInstance,'background');
  tKep.Free;
 end;

 frmMainForm.Megvaltozott := true;
 tCfg := TIniFile.Create(Konyvtar + 'skin.ini');
 frmMainForm.HatterSzin := tCfg.ReadInteger('Szinek','Hatter',14209224);
 frmMainForm.Gombok := tCfg.ReadInteger('Szinek','Gombok',14209224);
 frmMainForm.Fulek := tCfg.ReadInteger('Szinek','Fulek',14209224);
 frmMainForm.Betuk := tCfg.ReadInteger('Szinek','Betuk',0);
 tCfg.Free;

 Old.Top := Self.Top;
 Old.Left := Self.Left;
 Old.Right := Self.Width;
 Old.Bottom := Self.Height;

 Self.Width := 2000;
 Self.Height := 2000;
 Self.Top := -20;
 Self.Left := -20;

 Self.Top := Old.Top;
 Self.Left := Old.Left;
 Self.Width := Old.Right;
 Self.Height := Old.Bottom;

 Self.Close;
end;

procedure TfrmBeallitasok.FormShow(Sender: TObject);
var i, kijelolt: integer;
begin
 sedTurelmiIdo.Value := Cfg.ReadInteger('TurelmiIdo','Napok',30);
 sedForintPerNap.Value := Cfg.ReadInteger('TurelmiIdo','ForintPerNap',5);
 cbxProgramInditasakor.Checked := Cfg.ReadBool('TurelmiIdo','IndulaskorEllenorzes',false);

 chkKilepesiEffekt.Checked := Cfg.ReadBool('Altalanos','KilepesiEffekt',true);
 SpinEdit1.Value := Cfg.ReadInteger('Altalanos','Lepes',100);

 Memo1.Lines.Clear;
 for i := 1 to Cfg.ReadInteger('Categories','CategoryCount',1) do
  Memo1.Lines.Add(Cfg.ReadString('Categories','Cat' + IntToStr(i),'[hiba]'));

 txtNeved.Text := Cfg.ReadString('Szamla','Neved','');
 txtCimed.Text := Cfg.ReadString('Szamla','Cimed','');

 dlbSkinek.Drive := ExtractFileDrive(ParamStr(0))[1];
 try
  dlbSkinek.Directory := ExtractFilePath(ParamStr(0)) + 'skins\';
 except
  on EInOutError do
  begin
   Application.MessageBox('Hiba: nem találom a skin könyvtárat','Hiba',mb_Ok + mb_IconAsterisk);
   lstSkinek.Items.Add('[''Fatál'' Hiba]');
   Abort;
  end;
 end;
 kijelolt := -1;
 lstSkinek.Items.Clear;
 for i := 0 to dlbSkinek.Items.Count - 1 do
  if dlbSkinek.Selected[i] then
   kijelolt := i;
 if kijelolt = -1 then
 begin
  lstSkinek.Items.Add('[Belsõ hiba]');
  Abort;
 end;

 for i := kijelolt + 1 to dlbSkinek.Items.Count-1 do
 begin
  try
   lstSkinek.Items.Add(dlbSkinek.Items[i]);
  except
  end;
 end;

 lstSkinek.ItemIndex := lstSkinek.Items.IndexOf(Cfg.ReadString('Skin','Actual','default'));
 lblSkin.Caption := 'Aktuális: ' + Cfg.ReadString('Skin','Actual','[Hiba]');

 PageControl1.ActivePage := TabSheet1;
 ListBox1.ItemIndex := 0;

 PageControl1.Font.Color := frmMainForm.Betuk;
 PageControl1.Canvas.Font.Color := frmMainForm.Betuk;
 TabSheet1.Font.Color := frmMainForm.Betuk;
 Label1.Font.Color := frmMainForm.Betuk;
 Label1.Canvas.Font.Color := frmMainForm.Betuk;
 sedTurelmiIdo.Font.Color := frmMainForm.Betuk;
 sedForintPerNap.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 BitBtn1.Font.Color := frmMainForm.Betuk;
 BitBtn2.Font.Color := frmMainForm.Betuk;
 cbxProgramInditasakor.Font.Color := frmMainForm.Betuk;
 TabSheet2.Font.Color := frmMainForm.Betuk;
 Memo1.Font.Color := frmMainForm.Betuk;
 TabSheet3.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.Font.Color := frmMainForm.Betuk;
 Label4.Font.Color := frmMainForm.Betuk;
 Label4.Canvas.Font.Color := frmMainForm.Betuk;
 txtNeved.Font.Color := frmMainForm.Betuk;
 txtCimed.Font.Color := frmMainForm.Betuk;
 TabSheet4.Font.Color := frmMainForm.Betuk;
 Label5.Font.Color := frmMainForm.Betuk;
 Label5.Canvas.Font.Color := frmMainForm.Betuk;
 lstSkinek.Font.Color := frmMainForm.Betuk;
 lstSkinek.Canvas.Font.Color := frmMainForm.Betuk;
 dlbSkinek.Font.Color := frmMainForm.Betuk;
 dlbSkinek.Canvas.Font.Color := frmMainForm.Betuk;
 lblSkin.Font.Color := frmMainForm.Betuk;
 lblSkin.Canvas.Font.Color := frmMainForm.Betuk;
 ListBox1.Color := frmMainForm.HatterSzin;
 chkKilepesiEffekt.Color := frmMainForm.Hatterszin;
 chkKilepesiEffekt.Font.Color := frmMainForm.Betuk;
end;

procedure TfrmBeallitasok.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caFree;
 frmBeallitasok := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmBeallitasok.BitBtn2Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmBeallitasok.FormCreate(Sender: TObject);
var i: integer;
    temp: string;
    Bmp: TBitmap;
begin
 Cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 temp := Cfg.ReadString('Skin','Actual','default');
 temp := ExtractFilePath(ParamStr(0)) + 'skins\' + temp + '\';
 Bmp := TBitmap.Create;
 if FileExists(temp + 'ok.bmp') then
 begin
  Bmp.LoadFromFile(temp + 'ok.bmp');
  if (Bmp.Width = 18)and(Bmp.Height = 18) then
  begin
   BitBtn1.Glyph.LoadFromFile(temp + 'ok.bmp');
   BitBtn1.NumGlyphs := 1;
  end;
 end;
 if FileExists(temp + 'megse.bmp') then
 begin
  Bmp.LoadFromFile(temp + 'megse.bmp');
  if (Bmp.Width = 18)and(Bmp.Height = 18) then
  begin
   BitBtn2.Glyph.LoadFromFile(temp + 'megse.bmp');
   BitBtn2.NumGlyphs := 1;
  end;
 end;
 Bmp.Free;

 Memo1.Lines.Clear;
 for i := 1 to Cfg.ReadInteger('Categories','CategoryCount',1) do
  Memo1.Lines.Add(Cfg.ReadString('Categories','Cat' + IntToStr(i),'[hiba]'));

 Self.Color := frmMainForm.HatterSzin;
 BitBtn1.Color := frmMainForm.Gombok;
 BitBtn2.Color := frmMainForm.Gombok;
 ListBox1.Color := frmMainForm.HatterSzin;
end;

procedure TfrmBeallitasok.FormDestroy(Sender: TObject);
begin
 Cfg.Free;
end;

procedure TfrmBeallitasok.PageControl1DrawTab(Control: TCustomTabControl;
  TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
 Control.Canvas.Brush.Style := bsSolid;
 Control.Canvas.Brush.Color := frmMainForm.Fulek;
 Control.Canvas.FillRect(Rect);
 Control.Canvas.Font.Color := frmMainForm.Betuk;
 Control.Canvas.TextOut(Rect.Left+4,Rect.Top+2,Feliratok[TabIndex]);
end;

procedure TfrmBeallitasok.ListBox1Click(Sender: TObject);
begin
 PageControl1.ActivePage := (FindComponent('TabSheet' + IntToStr(ListBox1.ItemIndex + 1)) as TTabSheet);
end;

procedure TfrmBeallitasok.SpinEdit1Change(Sender: TObject);
begin
 if SpinEdit1.Value < 1 then
  SpinEdit1.Value := 1;
end;

end.

