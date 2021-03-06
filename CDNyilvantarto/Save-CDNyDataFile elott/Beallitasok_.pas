unit Beallitasok;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, IniFiles,
  StdCtrls, Buttons, Spin, ComCtrls, FileCtrl, SRColBtn;

type
  TfrmBeallitasok = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    sedTurelmiIdo: TSpinEdit;
    sedForintPerNap: TSpinEdit;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    BitBtn1: TSRColorButton;
    BitBtn2: TSRColorButton;
    cbxProgramInditasakor: TCheckBox;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    TabSheet3: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    txtNeved: TEdit;
    txtCimed: TEdit;
    TabSheet4: TTabSheet;
    Label5: TLabel;
    lstSkinek: TListBox;
    dlbSkinek: TDirectoryListBox;
    lblSkin: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1DrawTab(Control: TCustomTabControl; TabIndex: Integer; const Rect: TRect; Active: Boolean);
    procedure RemoveWindow_Shrink(Handle: HWND; Obj:tform);    
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBeallitasok: TfrmBeallitasok;
  Cfg: TIniFile;

const
  Feliratok: array[0..3] of string = ('T�relmi id�','Kateg�ri�k','Sz�mla','Skin');

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
    frmMainForm.cmdExport.Glyph.LoadFromFile(Konyvtar + '5.bmp');
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
   Application.MessageBox('Hiba: nem tal�lom a skin k�nyvt�rat','Hiba',mb_Ok + mb_IconAsterisk);
   lstSkinek.Items.Add('[''Fat�l ''Hiba]');
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
  lstSkinek.Items.Add('[Hiba]');
  Abort;
 end;

 for i := kijelolt + 1 to dlbSkinek.Items.Count do
 begin
  try
   lstSkinek.Items.Add(dlbSkinek.Items[i]);
  except
  end;
 end;

 lblSkin.Caption := 'Aktu�lis: ' + Cfg.ReadString('Skin','Actual','[Hiba]');

 PageControl1.ActivePage := TabSheet1;

 PageControl1.Font.Color := frmMainForm.Betuk;
 PageControl1.Canvas.Font.Color := frmMainForm.Betuk;
 TabSheet1.Font.Color := frmMainForm.Betuk;
 Label1.Font.Color := frmMainForm.Betuk;
 Label1.Canvas.Font.Color := frmMainForm.Betuk;
 sedTurelmiIdo.Font.Color := frmMainForm.Betuk;
 sedForintPerNap.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 SpeedButton1.Font.Color := frmMainForm.Betuk;
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

end;

procedure TfrmBeallitasok.SpeedButton1Click(Sender: TObject);
begin
 if Application.MessageBox('T�nyleg vissza akarod �ll�tani az alap�rt�keket?','CD-Nyilv�ntart�',mb_YesNo + mb_IconQuestion) = id_No then
  Exit;
 sedTurelmiIdo.Value := 30;
 sedForintPerNap.Value := 5;
 cbxProgramInditasakor.Checked := false;
end;

procedure TfrmBeallitasok.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caFree;
 frmBeallitasok := nil;
 RemoveWindow_Shrink((Self as TForm).Handle,(Self as TForm));
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

procedure TfrmBeallitasok.RemoveWindow_Shrink(Handle: HWND; Obj: tform);
var
  lepeskoz    : integer;
  varakozas   : integer;
  oldalarany  : real;
  y_up        : integer;
  y_down      : integer;
  x_left      : integer;
  x_right     : integer;
  rgn         : HRGN;
  rgnpoints   : array[1..22] of tpoint;
  rgnpcount   : integer;
begin
  rgnpcount:=4;

  y_up       :=0;
  y_down     :=obj.height;
  oldalarany :=obj.width/obj.height;
  lepeskoz   :=10;
  varakozas  :=10;
  randomize;
  //ELTUNTETES
  while (y_up<y_down) do
  begin
    inc(y_up,   lepeskoz);
    dec(y_down, lepeskoz);
    x_left  :=  round(y_up  *  oldalarany);
    x_right :=  round(y_down * oldalarany);
    while y_up>y_down+1 do
    begin
      dec(y_up);
      inc(y_down);
    end;
    rgnpoints[1].x:=x_right;  rgnpoints[1].y:=y_down;
    rgnpoints[2].x:=x_left;   rgnpoints[2].y:=y_down;
    rgnpoints[3].x:=x_left;   rgnpoints[3].y:=y_up;
    rgnpoints[4].x:=x_right;  rgnpoints[4].y:=y_up;
    Rgn := CreatePolygonRgn(RgnPoints, rgnpcount, ALTERNATE);
    SetWindowRgn(Handle, Rgn, True);
    sleep(varakozas);
  end;
end;

end.

