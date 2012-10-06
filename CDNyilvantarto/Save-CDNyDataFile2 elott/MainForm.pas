unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ToolWin,
  ComCtrls, StdCtrls, Buttons, ExtCtrls, CDKolcsonadasa, CDVisszakapasa,
  ShellAPI, ActnList, Menus, IniFiles, Beallitasok, Kereses, About, ImgList,
  CDNyDataFile;

type
  TfrmMainForm = class(TForm)
    ToolBar1: TToolBar;
    cmdKolcsonkerok: TSpeedButton;
    cmdCDk: TSpeedButton;
    cmdKolcsonadas: TSpeedButton;
    cmdKilepes: TSpeedButton;
    Bevel1: TBevel;
    cmdVisszakapas: TSpeedButton;
    ActionList1: TActionList;
    Bevel3: TBevel;
    cmdKereses: TSpeedButton;
    cmdBeallitasok: TSpeedButton;
    KolcsonkerokreKattintas: TAction;
    CDkreKattintas: TAction;
    CDKolcsonadasaraKattintas: TAction;
    CDVisszakapaasraKattintas: TAction;
    KeresesreKattintas: TAction;
    BeallitasokraKattintas: TAction;
    KilepesreKattintas: TAction;
    SegitsegKell: TAction;
    cmdAbout: TSpeedButton;
    PopupMenu1: TPopupMenu;
    mnuNevjegy: TMenuItem;
    N1: TMenuItem;
    mnuTartalom: TMenuItem;
    mnuTargymutato: TMenuItem;
    NevjegyreKattintas: TAction;
    cmdAblakElrendezes: TSpeedButton;
    PopupMenu2: TPopupMenu;
    mnuCascade: TMenuItem;
    mnuHTile: TMenuItem;
    mnuArrangeicons: TMenuItem;
    mnuVTile: TMenuItem;
    ImageList1: TImageList;
    ADOTable1: TCDNyDataFile_CDk;
    procedure cmdKolcsonkerokClick(Sender: TObject);
    procedure cmdCDkClick(Sender: TObject);
    procedure cmdKilepesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure cmdKolcsonadasClick(Sender: TObject);
    procedure cmdVisszakapasClick(Sender: TObject);
    procedure KolcsonkerokreKattintasExecute(Sender: TObject);
    procedure CDkreKattintasExecute(Sender: TObject);
    procedure CDKolcsonadasaraKattintasExecute(Sender: TObject);
    procedure CDVisszakapasaraKattintasExecute(Sender: TObject);
    procedure KilepesreKattintasExecute(Sender: TObject);
    procedure SegitsegKellExecute(Sender: TObject);
    procedure cmdAboutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function NalNel(Kinel: string): string;
    procedure cmdBeallitasokClick(Sender: TObject);
    procedure cmdKeresesClick(Sender: TObject);
    procedure KeresesreKattintasExecute(Sender: TObject);
    procedure BeallitasokraKattintasExecute(Sender: TObject);
    procedure mnuNevjegyClick(Sender: TObject);
    procedure mnuTartalomClick(Sender: TObject);
    procedure mnuTargymutatoClick(Sender: TObject);
    procedure NevjegyreKattintasExecute(Sender: TObject);
    procedure cmdAblakElrendezesClick(Sender: TObject);
    procedure mnuCascadeClick(Sender: TObject);
    procedure mnuArrangeiconsClick(Sender: TObject);
    procedure mnuVTileClick(Sender: TObject);
    procedure mnuHTileClick(Sender: TObject);
    procedure RemoveWindow(Handle: HWND; Obj: TForm);
  private
    OldWinProc, NewWinProc: Pointer;
    OutCanvas: TCanvas;
    Hatterkep: TBitmap;
    fHatterSzin: TColor;
    fMegvaltozott: boolean;
    fGombok: TColor;
    fFulek: TColor;
    fBetuk: TColor;
    procedure NewWinProcedure(var Msg: TMessage);
    procedure RemoveWindow_Shrink(Handle: HWND; Obj: TForm);
    procedure RemoveWindow_Clock(Handle: HWND; Obj: TForm; Lepes: integer);
    procedure RemoveWindow_UpLeft(Handle: HWND; Obj: TForm);
  public
    property proHatterkep: TBitmap read Hatterkep write Hatterkep;
    property HatterSzin: TColor read fHatterSzin write fHatterSzin default 14209224;
    property Megvaltozott: boolean read fMegvaltozott write fMegvaltozott default false;
    property Gombok: TColor read fGombok write fGombok default 14209224;
    property Fulek: TColor read fFulek write fFulek default 14209224;
    property Betuk: TColor read fBetuk write fBetuk default 0;
  end;

var
  frmMainForm: TfrmMainForm;
  ConfigFile: TIniFile;

implementation

uses Kolcsonkerok, CDk;

{$R *.DFM}
{$R kepek.res}

procedure TfrmMainForm.RemoveWindow_Shrink(Handle: HWND; Obj:tform);
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

procedure TfrmMainForm.cmdKolcsonkerokClick(Sender: TObject);
begin
 if not Assigned(frmKolcsonkerok) then
  frmKolcsonkerok := TfrmKolcsonkerok.Create(Application);
 frmKolcsonkerok.Show;
end;

procedure TfrmMainForm.cmdCDkClick(Sender: TObject);
begin
 if not Assigned(frmCDk) then
  frmCDk := TfrmCDk.Create(Application);
 frmCDk.Show;
end;

procedure TfrmMainForm.cmdKilepesClick(Sender: TObject);
begin
 frmMainForm.Close;
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
var temp: string;
    tempdate: TDateTime;
    Status: integer;
    TurelmiNapokSzama, ForintPerNap: integer;
    Konyvtar: string;
    tKep: TBitmap;
    tCfg: TIniFile;
    i: integer;
begin
 NewWinProc := MakeObjectInstance(NewWinProcedure);
 OldWinProc := Pointer(SetWindowLong(ClientHandle,gwl_WndProc,Cardinal(NewWinProc)));
 OutCanvas := TCanvas.Create;

{------------------------------------------------------------------------------}
 ConfigFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 Konyvtar := ConfigFile.ReadString('Skin','Actual','[Hiba]');
 if Konyvtar <> '[Hiba]' then
 begin
  Konyvtar := ExtractFilePath(ParamStr(0)) + 'skins\' + Konyvtar + '\';
  tCfg := TIniFile.Create(Konyvtar + 'skin.ini');
  if tCfg.ValueExists('Szinek','Hatter') then
   fHatterSzin := tCfg.ReadInteger('Szinek','Hatter',14209224)
  else
   fHatterSzin := 14209224;
  if tCfg.ValueExists('Szinek','Gombok') then
   fGombok := tCfg.ReadInteger('Szinek','Gombok',14209224)
  else
   fGombok := 14209224;
  if tCfg.ValueExists('Szinek','Fulek') then
   fFulek := tCfg.ReadInteger('Szinek','Fulek',14209224)
  else
   fFulek := 14209224;
  if tCfg.ValueExists('Szinek','Betuk') then
   fBetuk := tCfg.ReadInteger('Szinek','Betuk',0)
  else
   fBetuk := 0;
  frmMainForm.Color := fHatterSzin;
  tCfg.Free;
  tKep := TBitmap.Create;
  if FileExists(Konyvtar + '1.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '1.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdKolcsonkerok.Glyph.LoadFromFile(Konyvtar + '1.bmp');
  end;
  if FileExists(Konyvtar + '2.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '2.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdCDk.Glyph.LoadFromFile(Konyvtar + '2.bmp');
  end;
  if FileExists(Konyvtar + '3.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '3.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdKolcsonadas.Glyph.LoadFromFile(Konyvtar + '3.bmp');
  end;
  if FileExists(Konyvtar + '4.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '4.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdVisszakapas.Glyph.LoadFromFile(Konyvtar + '4.bmp');
  end;
  if FileExists(Konyvtar + '6.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '6.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdKereses.Glyph.LoadFromFile(Konyvtar + '6.bmp');
  end;
  if FileExists(Konyvtar + '7.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '7.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdBeallitasok.Glyph.LoadFromFile(Konyvtar + '7.bmp');
  end;
  if FileExists(Konyvtar + '8.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '8.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdAbout.Glyph.LoadFromFile(Konyvtar + '8.bmp');
  end;
  if FileExists(Konyvtar + '9.bmp') then
  begin
   tKep.LoadFromFile(Konyvtar + '9.bmp');
   if (tKep.Width = 32)and(tKep.Height = 32) then
    cmdKilepes.Glyph.LoadFromFile(Konyvtar + '9.bmp');
  end;
  Hatterkep := TBitmap.Create;
  if FileExists(Konyvtar + 'bg.bmp') then
   Hatterkep.LoadFromFile(Konyvtar + 'bg.bmp')
  else
   Hatterkep.LoadFromResourceName(HInstance,'background');
  tKep.Free;
 end
 else
 begin
  Hatterkep := TBitmap.Create;
  Hatterkep.LoadFromResourceName(HInstance,'background');
 end;

 ConfigFile.Free;
{------------------------------------------------------------------------------}

 ConfigFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 TurelmiNapokSzama := ConfigFile.ReadInteger('TurelmiIdo','Napok',30);
 ForintPerNap := ConfigFile.ReadInteger('TurelmiIdo','ForintPerNap',5);

 if ConfigFile.ReadBool('TurelmiIdo','IndulaskorEllenorzes',false) then
 begin
  ADOTable1.OpenFile(ExtractFilePath(ParamStr(0)) + 'CDk.cdny');
  for i := 1 to ADOTable1.Mennyiseg do
  begin
   if ADOTable1.GetIndex(i).KolcsonVanEKerve then
    begin
    TempDate := ADOTable1.GetIndex(i).KolcsonkeresDatuma;
    TempDate := TempDate + TurelmiNapokSzama;
    if TempDate <= Now then
    begin
     temp := 'A ' + ADOTable1.GetIndex(i).CDNeve + ' nevû CD ' + IntToStr(TurelmiNapokSzama) + ' napnál tovább volt ';
     temp := temp + NalNel(ADOTable1.GetIndex(i).Kolcsonkero) + '(';
     temp := temp + IntToStr(trunc(Now - TempDate)) + ' nappal = ' + IntToStr((trunc(Now-TempDate))*ForintPerNap) + ' Ft).';
     Application.MessageBox(PChar(temp),'CD-Nyilvántartó',mb_Ok + mb_IconInformation);
    end;
   end;
  end;
 end;

 Status := ConfigFile.ReadInteger('frmMainForm','Status',0);
 if Status <> 0 then
 begin
  Top := ConfigFile.ReadInteger('frmMainForm','Top',Top);
  Left := ConfigFile.ReadInteger('frmMainForm','Left',Left);
  Width := ConfigFile.ReadInteger('frmMainForm','Width',Width);
  Height := ConfigFile.ReadInteger('frmMainForm','Height',Height);
  case status of
   1: WindowState := wsNormal;
   2: WindowState := wsMinimized;
   3: WindowState := wsMaximized;
  end;
 end;
 ConfigFile.Free;
end;

procedure TfrmMainForm.NewWinProcedure(var Msg: TMessage);
var BmpWidth, BmpHeight: integer;
    I, J: integer;
begin
 Msg.Result := CallWindowProc(OldWinProc,ClientHandle,Msg.Msg,Msg.wParam,Msg.lParam);
 if Msg.Msg = wm_EraseBkgnd then
 begin
  BmpWidth := Hatterkep.Width;
  BmpHeight := Hatterkep.Height;
  if (BmpWidth <> 0)and(BmpHeight <> 0) then
  begin
   OutCanvas.Handle := Msg.wParam;
   for I := 0 to frmMainForm.ClientWidth div BmpWidth do
    for J := 0 to frmMainForm.ClientHeight div BmpHeight do
     OutCanvas.Draw(I * BmpWidth,J * BmpHeight,Hatterkep);
  end;
 end;

 if fMegvaltozott then
 begin
  frmMainForm.Color := fHatterSzin;
  frmAbout.Color := fHatterSzin;

  ToolBar1.Font.Color := fBetuk;
  ToolBar1.Canvas.Font.Color := fBetuk;
  cmdKolcsonkerok.Font.Color := fBetuk;
  cmdCDk.Font.Color := fBetuk;
  cmdKolcsonadas.Font.Color := fBetuk;
  cmdKilepes.Font.Color := fBetuk;
  cmdVisszakapas.Font.Color := fBetuk;
  cmdKereses.Font.Color := fBetuk;
  cmdBeallitasok.Font.Color := fBetuk;
  cmdAbout.Font.Color := fBetuk;
 end;
end;

procedure TfrmMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Button = mbRight) and (ssShift in Shift) then
 begin
  frmAbout.Show;
  frmAbout.ShowAboutText;
 end;
end;

procedure TfrmMainForm.cmdKolcsonadasClick(Sender: TObject);
begin
 if not Assigned(frmCDKolcsonadasa) then
  frmCDKolcsonadasa := TfrmCDKolcsonadasa.Create(Application);
 frmCDKolcsonadasa.Show;
end;

procedure TfrmMainForm.cmdVisszakapasClick(Sender: TObject);
begin
 if not Assigned(frmCDVisszakapasa) then
  frmCDVisszakapasa := TfrmCDVisszakapasa.Create(Application);
 frmCDVisszakapasa.Show;
end;

procedure TfrmMainForm.KolcsonkerokreKattintasExecute(Sender: TObject);
begin
 cmdKolcsonkerokClick(Sender);
end;

procedure TfrmMainForm.CDkreKattintasExecute(Sender: TObject);
begin
 cmdCDkClick(Sender);
end;

procedure TfrmMainForm.CDKolcsonadasaraKattintasExecute(Sender: TObject);
begin
 cmdKolcsonadasClick(Sender);
end;

procedure TfrmMainForm.CDVisszakapasaraKattintasExecute(Sender: TObject);
begin
 cmdVisszakapasClick(Sender);
end;

procedure TfrmMainForm.KilepesreKattintasExecute(Sender: TObject);
begin
 cmdKilepesClick(Sender);
end;

procedure TfrmMainForm.SegitsegKellExecute(Sender: TObject);
begin
 if not FileExists(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.hlp') then
 begin
  Application.MessageBox('Nem találom a súgófilet!','Hiba',mb_Ok + mb_IconHand);
  Exit;
 end;
 ShellExecute(Handle,'open',PChar(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.hlp'),'',PChar(ExtractFilePath(ParamStr(0))),sw_ShowNormal);
end;

procedure TfrmMainForm.cmdAboutClick(Sender: TObject);
var Pt: TPoint;
begin
 GetCursorPos(Pt);
 PopupMenu1.Popup(Pt.x,Pt.y);
end;

procedure TfrmMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var Status: integer;
begin
 ConfigFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 Status := 0;
 case WindowState of
  wsNormal: begin
             ConfigFile.WriteInteger('frmMainForm','Top',Top);
             ConfigFile.WriteInteger('frmMainForm','Left',Left);
             ConfigFile.WriteInteger('frmMainForm','Width',Width);
             ConfigFile.WriteInteger('frmMainForm','Height',Height);
             Status := 1;
            end;
  wsMinimized: Status := 2;
  wsMaximized: Status := 3;
 end;
// if not Active then Status := 2;
 if not Application.Active then Status := 2;
 ConfigFile.WriteInteger('frmMainForm','Status',Status);
 ConfigFile.Free;

 RemoveWindow((Self as TForm).Handle,Self as TForm);
end;

function TfrmMainForm.NalNel(Kinel: string): string;
var temp: string;
    i: integer;
begin
 temp := 'nél';
 for i := 1 to Length(Kinel) do
  if Kinel[i] in ['a','A','á','Á','o','O','ó','Ó','u','U'] then
   temp := 'nál';
 Result := Kinel + temp;
end;

procedure TfrmMainForm.cmdBeallitasokClick(Sender: TObject);
begin
 if not Assigned(frmBeallitasok) then
  frmBeallitasok := TfrmBeallitasok.Create(Application);
 frmBeallitasok.Show;
end;

procedure TfrmMainForm.cmdKeresesClick(Sender: TObject);
begin
 if not Assigned(frmKereses) then
  frmKereses := TfrmKereses.Create(Application);
 frmKereses.Show; 
end;

procedure TfrmMainForm.KeresesreKattintasExecute(Sender: TObject);
begin
 cmdKeresesClick(Sender);
end;

procedure TfrmMainForm.BeallitasokraKattintasExecute(Sender: TObject);
begin
 cmdBeallitasokClick(Sender);
end;

procedure TfrmMainForm.mnuNevjegyClick(Sender: TObject);
begin
 FormMouseDown(Sender,mbRight,[ssShift],0,0);
end;

procedure TfrmMainForm.mnuTartalomClick(Sender: TObject);
begin
 Application.HelpCommand(Help_Finder,0);
end;

procedure TfrmMainForm.mnuTargymutatoClick(Sender: TObject);
begin
 Application.HelpCommand(Help_PartialKey,Longint(StrNew('')));
end;

procedure TfrmMainForm.NevjegyreKattintasExecute(Sender: TObject);
begin
 FormMouseDown(Sender,mbRight,[ssShift],0,0);
end;

procedure TfrmMainForm.cmdAblakElrendezesClick(Sender: TObject);
var P: TPoint;
begin
 GetCursorPos(P);
 PopupMenu2.Popup(P.x,P.y);
end;

procedure TfrmMainForm.mnuCascadeClick(Sender: TObject);
begin
 frmMainForm.Cascade;
end;

procedure TfrmMainForm.mnuArrangeiconsClick(Sender: TObject);
begin
 frmMainForm.ArrangeIcons;
end;

procedure TfrmMainForm.mnuVTileClick(Sender: TObject);
begin
 frmMainForm.TileMode := tbVertical;
 frmMainForm.Tile;
end;

procedure TfrmMainForm.mnuHTileClick(Sender: TObject);
begin
 frmMainForm.TileMode := tbHorizontal;
 frmMainForm.Tile;
end;

procedure TfrmMainForm.RemoveWindow(Handle: HWND; Obj: TForm);
var Cfg: TIniFile;
    i: integer;
begin
 Cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'CDNyilvantarto.ini');
 if Cfg.ReadBool('Altalanos','KilepesiEffekt',true) then
 begin
  Randomize;
  i := trunc(Random(3) + 1);
  case i of
   1 : RemoveWindow_Shrink(Handle,Obj);
   2 : RemoveWindow_Clock(Handle,Obj,Cfg.ReadInteger('Altalanos','Lepes',100));
   3 : RemoveWindow_UpLeft(Handle,Obj);
   else
    Application.MessageBox('Belsõ hiba!','CD-Nyilvántartó',mb_Ok + mb_IconAsterisk);
  end;
 end;
 Cfg.Free;
end;

procedure TfrmMainForm.RemoveWindow_Clock(Handle: HWND; Obj: TForm; Lepes: integer);
var xm, ym: integer;
    ps: array[0..6] of TPoint;
    fh: THandle;
    x, y: single;
    LepesX, LepesY: single;
    Fill: integer;
begin
 xm := Obj.Width div 2;
 ym := Obj.Height div 2;

 LepesX := Obj.Width / Lepes;
 LepesY := Obj.Height / Lepes;

 Fill := alternate;

 x := xm;
 while x <= Obj.Width do
 begin                           //1
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(trunc(x),0);
  ps[4] := point(Obj.width,0);
  ps[5] := point(Obj.width,Obj.height);
  ps[6] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,7,Fill);
  SetWindowRgn(Handle,fh,true);
//  Sleep(1);
  x := x + LepesX;
 end;
 y := 0;
 while y <= height do
 begin                           //2
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(Obj.width,trunc(y));
  ps[4] := point(Obj.width,Obj.height);
  ps[5] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,6,Fill);
  SetWindowRgn(Handle,fh,true);
//  Sleep(1);
  y := y + LepesY;
 end;
 x := width;
 while x >= 0 do
 begin                           //3
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(trunc(x),Obj.height);
  ps[4] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,5,Fill);
  SetWindowRgn(Handle,fh,true);
//  Sleep(1);
  x := x - LepesX;
 end;
 y := height;
 while y >= 0 do
 begin                           //4
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(0,trunc(y));
  fh := CreatePolygonRgn(ps,4,Fill);
  SetWindowRgn(Handle,fh,true);
//  Sleep(1);
  y := y - LepesY;
 end;
 x := 0;
 while x <= xm do
 begin                           //5
  ps[0] := point(trunc(x),0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  fh := CreatePolygonRgn(ps,3,Fill);
  SetWindowRgn(Handle,fh,true);
//  Sleep(1);
  x := x + LepesX;
 end;
end;

procedure TfrmMainForm.RemoveWindow_UpLeft(Handle: HWND; Obj: TForm);
var i: integer;
begin
 for i := Obj.ClientHeight downto 0 do
  Obj.ClientHeight := i;
 for i := Obj.ClientWidth downto 0 do
  Obj.ClientWidth := i;
end;

end.
