unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, ComCtrls, IniFiles, AppEvnts, Unit2;

type
  TFoForm = class(TForm)
    Image1: TImage;
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    mnuJatek: TMenuItem;
    mnuJatekUj: TMenuItem;
    N1: TMenuItem;
    mnuJatekKilepes: TMenuItem;
    mnuSegitseg: TMenuItem;
    mnuSegitsegIndex: TMenuItem;
    N2: TMenuItem;
    mnuSegitsegNevjegy: TMenuItem;
    Timer1: TTimer;
    mnuSegitsegTargymutato: TMenuItem;
    mnuJatekUjNormal: TMenuItem;
    mnuJtekUjTournament: TMenuItem;
    RaketaIdozito: TTimer;
    mnuPalyaszerkeszto: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure UjraRajzolas;
    procedure mnuJatekUjNormalClick(Sender: TObject);
    procedure mnuJatekKilepesClick(Sender: TObject);
    procedure mnuSegitsegNevjegyClick(Sender: TObject);
    procedure BillenyuNyomas(var Msg: TWMKeyDown);message wm_KeyDown;
    procedure Timer1Timer(Sender: TObject);
    procedure CelEllenorzes;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuSegitsegIndexClick(Sender: TObject);
    procedure mnuSegitsegTargymutatoClick(Sender: TObject);
    procedure mnuJtekUjTournamentClick(Sender: TObject);
    procedure NextTournamentLevel;
    procedure RaketaMozgas(Melyik: integer);
    procedure FormDestroy(Sender: TObject);
    procedure RaketaIdozitoTimer(Sender: TObject);
    procedure WMSysCommand(var Msg: TWmSysCommand);message wm_SysCommand;
    procedure mnuPalyaszerkesztoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TRaketaIrany = (riFel, riLe, riBalra, riJobbra);

  TRaketa = class
   public
    RaketaSzama: integer;
    Pozicio: TPoint;
    Irany: TRaketaIrany;
    Letezek: boolean;
    constructor Create(AOwner: TComponent);
    destructor Destroy;override;
  end;

var
  FoForm: TFoForm;
  Map: array[1..20,1..20] of string;
  TeHolVagy: TPoint;
  HolVanACel: TPoint;
  Vege: boolean;
  FalonAtmenes: boolean;
  PasiEredetiHelyzete: TPoint;
  CheattalNyeres: boolean;
  LabirintusFile: TIniFile;
  TournamentMode: boolean;
  TournamentPalyaSzama: integer;
  Raketak: array[1..100] of TRaketa;

  kep_nothing, kep_wall, kep_manus, kep_finish, kep_manusacelban: TBitmap;
  kep_raketa_d, kep_raketa_l, kep_raketa_r, kep_raketa_u: TBitmap;

const
  MF_BYPOSITION = $400;
  MF_REMOVE = $1000;

implementation

{$R *.DFM}
{$R kepek.RES}

//---------------------------------------------------------
procedure TFoForm.FormCreate(Sender: TObject);
var x: integer;
    hSysMenu, nCnt: Longint;
begin
 Vege := true;

 if not FileExists(ExtractFilePath(ParamStr(0)) + 'Labirintus.dat') then
 begin
  Application.MessageBox('Nem találom a labitintus file-t!','Hiba',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;

  kep_nothing := TBitmap.Create;
  kep_wall := TBitmap.Create;
  kep_manus := TBitmap.Create;
  kep_finish := TBitmap.Create;
  kep_manusacelban := TBitmap.Create;

  kep_raketa_d := TBitmap.Create;
  kep_raketa_l := TBitmap.Create;
  kep_raketa_r := TBitmap.Create;
  kep_raketa_u := TBitmap.Create;

  kep_nothing.LoadFromResourceName(hInstance, 'kep_nothing');
  kep_wall.LoadFromResourceName(hInstance, 'kep_wall');
  kep_manus.LoadFromResourceName(hInstance, 'kep_manus');
  kep_finish.LoadFromResourceName(hInstance, 'kep_finish');
  kep_manusacelban.LoadFromResourceName(hInstance, 'kep_manusacelban');

  kep_raketa_d.LoadFromResourceName(hInstance, 'kep_raketa_d');
  kep_raketa_l.LoadFromResourceName(hInstance, 'kep_raketa_l');
  kep_raketa_r.LoadFromResourceName(hInstance, 'kep_raketa_r');
  kep_raketa_u.LoadFromResourceName(hInstance, 'kep_raketa_u');

 LabirintusFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Labirintus.dat');

 Image1.Canvas.StretchDraw(Rect(0,0,200,200),kep_nothing);

 FalonAtmenes := false;
 CheattalNyeres := false;
 with PasiEredetiHelyzete do
 begin
  x := 0;
  y := 0;
 end;

 for x := 1 to 100 do Raketak[x] := TRaketa.Create(FoForm);

//az 'X' gomb kikapcsolása
 hSysMenu := GetSystemMenu(FoForm.Handle,false); //a rendszermenü lekérdezése
 if hSysMenu <> 0 then //ha van, akkor
 begin
  nCnt := GetMenuItemCount(hSysMenu); //hány eleme van,
  if nCnt <> 0 then //ha van eleme, akkor
  begin
   RemoveMenu(hSysMenu,nCnt - 1,mf_ByPosition + mf_Remove); //leszedjük a Bezárást, ezzel a gombor is kikapcsoljuk,
   RemoveMenu(hSysMenu,nCnt - 2,mf_ByPosition + mf_Remove); //de elõtte van egy elválasztó is
   DrawMenuBar(FoForm.Handle); //erõltetjük az újrarajzolást, ezzel látszik is a mûvünk.
  end;
 end;
//kész, de ezt a tálcai rendszermenübõl is kiszedjük
 hSysMenu := GetSystemMenu(Application.Handle,false);
 if hSysMenu <> 0 then
 begin
  nCnt := GetMenuItemCount(hSysMenu);
  if nCnt <> 0 then
  begin
   RemoveMenu(hSysMenu,nCnt - 1,mf_ByPosition + mf_Remove);
   RemoveMenu(hSysMenu,nCnt - 2,mf_ByPosition + mf_Remove);
  end;
 end;
//kész
end;

//---------------------------------------------------------

procedure TFoForm.UjraRajzolas;
var x, y: integer;
begin
 for x := 1 to 20 do
  for y := 1 to 20 do
  begin
   if Map[x,y] = '-' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_nothing);
   if Map[x,y] = '+' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_wall);
   Image1.Canvas.Draw(TeHolVagy.y * 10 - 10, TeHolVagy.x * 10 - 10,    kep_manus);
   Image1.Canvas.Draw(HolVanACel.y * 10 - 10, HolVanACel.x * 10 - 10,  kep_finish);
  end;
 for x := 1 to 100 do
  if Raketak[x].Letezek = true then
  begin
   if Raketak[x].Irany = riLe then Image1.Canvas.Draw(Raketak[x].Pozicio.y * 10 - 10,Raketak[x].Pozicio.x * 10 - 10,kep_raketa_d);
   if Raketak[x].Irany = riFel then Image1.Canvas.Draw(Raketak[x].Pozicio.y * 10 - 10,Raketak[x].Pozicio.x * 10 - 10,kep_raketa_u);
   if Raketak[x].Irany = riBalra then Image1.Canvas.Draw(Raketak[x].Pozicio.y * 10 - 10,Raketak[x].Pozicio.x * 10 - 10,kep_raketa_l);
   if Raketak[x].Irany = riJobbra then Image1.Canvas.Draw(Raketak[x].Pozicio.y * 10 - 10,Raketak[x].Pozicio.x * 10 - 10,kep_raketa_r);
  end;
end;

//---------------------------------------------------------

procedure TFoForm.mnuJatekUjNormalClick(Sender: TObject);
var x, y, PalyaSzama: integer;
    temp, PalyakSzama: string;
label Eleje;
begin
 if not FileExists(ExtractFilePath(ParamStr(0)) + 'Labirintus.dat') then
 begin
  Application.MessageBox('Nem találom a labitintus file-t!','Hiba',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;
 try
  try
   PalyakSzama := LabirintusFile.ReadString('General Info','NumberOfLevels','1');
   if StrToInt(PalyakSzama) > 1 then
   begin
Eleje: //-----------------------------label Eleje-------------------------------
    //temp := InputBox('Labirintus','A labirintus.dat-ban ' + temp + ' pálya van.'#13#10'Melyiken akarsz játszani?','1');
    temp := InputBox('Labirintus','Pálya száma (1-' + PalyakSzama + '):','1');
    if LabirintusFile.SectionExists('Level' + temp) = false then
    begin
     Application.MessageBox('Ilyen Pálya nincs.','Hiba',mb_Ok + mb_IconAsterisk);
     GoTo Eleje;
    end
   else
    PalyaSzama := StrToInt(temp);
   for x := 1 to 20 do
   begin
    temp := LabirintusFile.ReadString('Level' + IntToStr(PalyaSzama),'sor' + IntToStr(x),'--------------------');
    for y := 1 to 20 do Map[x,y] := temp[y];
   end;
  end; 
  except
   on EFOpenError do
   begin
    Application.MessageBox('File-megnyitási hiba: Labirintus.dat','Hiba',mb_Ok + mb_IconHand);
    Application.Terminate;
   end;
  end;
 finally
 end;

 for x := 1 to 20 do
  for y := 1 to 20 do
  begin
   if Map[x,y] = '-' then image1.Canvas.Draw(y * 10 - 10,x * 10 - 10,kep_nothing);
   if Map[x,y] = '+' then image1.Canvas.Draw(y * 10 - 10,x * 10 - 10,kep_wall);
   if Map[x,y] = 'm' then begin
                          image1.Canvas.Draw(y * 10 - 10,x * 10 - 10,kep_manus);
                          Map[x,y] := '-';
                          TeHolVagy.x := x;
                          TeHolVagy.y := y;
                          end;
   if Map[x,y] = 'f' then begin
                          image1.Canvas.Draw(y * 10 - 10,x * 10 - 10,kep_finish);
                          HolVanACel.x := x;
                          HolVanACel.y := y;
                          end;
  end;
  StatusBar1.Panels[0].Text := '';
  FalonAtmenes := false;
  CheattalNyeres := false;
  with PasiEredetiHelyzete do
  begin
   x := 0;
   y := 0;
  end;
  Vege := false;
end;

//---------------------------------------------------------

procedure TFoForm.mnuJatekKilepesClick(Sender: TObject);
begin
 Application.Terminate;
end;

//---------------------------------------------------------

procedure TFoForm.mnuSegitsegNevjegyClick(Sender: TObject);
begin
 if not FileExists(ExtractFilePath(ParamStr(0)) + 'Nevjegy.exe') then
 begin
  Application.HelpContext(5);
  Exit;
 end;
 WinExec(PChar(ExtractFilePath(ParamStr(0)) + 'Nevjegy.exe'),SW_SHOWNORMAL);
end;

//---------------------------------------------------------

procedure TFoForm.BillenyuNyomas(var Msg: TWMKeyDown);
var x: integer;
begin
if Vege then Exit;
 case Msg.CharCode of
  37: begin             //balra
       if TeHolVagy.y = 1 then
       begin
        StatusBar1.Panels[0].Text := 'A pályáról nem mehetsz ki!';
        Timer1.Enabled := true;
        Exit;
       end;
       if (not FalonAtmenes)and(Map[TeHolVagy.x,TeHolVagy.y-1]='+') then
       begin
        StatusBar1.Panels[0].Text := 'Ott fal van!!';
        Timer1.Enabled := true;
        Exit;
       end;
       TeHolVagy.y := TeHolVagy.y - 1;
       UjraRajzolas;
       CelEllenorzes;
      end;
  39: begin             //jobbra
       if TeHolVagy.y = 20 then
       begin
        StatusBar1.Panels[0].Text := 'A pályáról nem mehetsz ki!';
        Timer1.Enabled := true;
        Exit;
       end;
       if (not FalonAtmenes)and(Map[TeHolVagy.x,TeHolVagy.y+1]='+') then
       begin
        StatusBar1.Panels[0].Text := 'Ott fal van!!';
        Timer1.Enabled := true;
        Exit;
       end;
       TeHolVagy.y := TeHolVagy.y + 1;
       UjraRajzolas;
       CelEllenorzes;
      end;
  38: begin             //fel
       if TeHolVagy.x = 1 then
       begin
        StatusBar1.Panels[0].Text := 'A pályáról nem mehetsz ki!';
        Timer1.Enabled := true;
        Exit;
       end;
       if (not FalonAtmenes)and(Map[TeHolVagy.x-1,TeHolVagy.y]='+') then
       begin
        StatusBar1.Panels[0].Text := 'Ott fal van!!';
        Timer1.Enabled := true;
        Exit;
       end;
       TeHolVagy.x := TeHolVagy.x - 1;
       UjraRajzolas;
       CelEllenorzes;
      end;
  40: begin             //le
       if TeHolVagy.x = 20 then
       begin
        StatusBar1.Panels[0].Text := 'A pályáról nem mehetsz ki!';
        Timer1.Enabled := true;
        Exit;
       end;
       if (not FalonAtmenes)and(Map[TeHolVagy.x+1,TeHolVagy.y]='+') then
       begin
        StatusBar1.Panels[0].Text := 'Ott fal van!!';
        Timer1.Enabled := true;
        Exit;
       end;
       TeHolVagy.x := TeHolVagy.x + 1;
       UjraRajzolas;
       CelEllenorzes;
      end;
  87: begin             //rakéta fel-W
       x := 0;
       repeat
        Inc(x);
       until (Raketak[x].Letezek = false)or(x = 100);
       if x = 100 then Exit;
       Raketak[x].Letezek := true;
       Raketak[x].Irany := riFel;
       Raketak[x].Pozicio.x := TeHolVagy.x;
       Raketak[x].Pozicio.y := TeHolVagy.y;
      end;
  65: begin             //rakéta balra-A
       x := 0;
       repeat
        Inc(x);
       until (Raketak[x].Letezek = false)or(x = 100);
       if x = 100 then Exit;
       Raketak[x].Letezek := true;
       Raketak[x].Irany := riBalra;
       Raketak[x].Pozicio.x := TeHolVagy.x;
       Raketak[x].Pozicio.y := TeHolVagy.y;
      end;
  83: begin             //rakéta le-S
       x := 0;
       repeat
        Inc(x);
       until (Raketak[x].Letezek = false)or(x = 100);
       if x = 100 then Exit;
       Raketak[x].Letezek := true;
       Raketak[x].Irany := riLe;
       Raketak[x].Pozicio.x := TeHolVagy.x;
       Raketak[x].Pozicio.y := TeHolVagy.y;
      end;
  68: begin             //rakéta jobbra-D
       x := 0;
       repeat
        Inc(x);
       until (Raketak[x].Letezek = false)or(x = 100);
       if x = 100 then Exit;
       Raketak[x].Letezek := true;
       Raketak[x].Irany := riJobbra;
       Raketak[x].Pozicio.x := TeHolVagy.x;
       Raketak[x].Pozicio.y := TeHolVagy.y;
      end;
 end;
end;

//---------------------------------------------------------

procedure TFoForm.Timer1Timer(Sender: TObject);
begin
 Timer1.Enabled := false;
 StatusBar1.Panels[0].Text := '';
end;

//---------------------------------------------------------

procedure TFoForm.CelEllenorzes;
label Eleje;
begin
 if (TeHolVagy.x = HolVanACel.x) and (TeHolVagy.y = HolVanACel.y) then
 begin
  StatusBar1.Panels[0].Text := 'Nyertél!';
  Vege := true;
  Image1.Canvas.Draw(HolVanACel.y * 10 - 10,HolVanACel.x * 10 - 10,kep_manusacelban);
  if TournamentMode then
  begin
Eleje: //-----------------------label Eleje-------------------------------------
   Inc(TournamentPalyaSzama);
   if TournamentPalyaSzama > StrToInt(LabirintusFile.ReadString('General Info','NumberOfLevels','1')) then
   begin
    Application.MessageBox('Vége a tournamentnek','Labirintus',mb_Ok + mb_IconInformation);
    TournamentMode := false;
    Exit;
   end;
   if not LabirintusFile.SectionExists('Level' + IntToStr(TournamentPalyaSzama)) then
    GoTo Eleje;
   NextTournamentLevel;
  end;
 end;

end;

procedure TFoForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
 Shift: TShiftState; X, Y: Integer);
var CheatCode: string;
begin
 if (ssShift in Shift)and(ssAlt in Shift)and(ssCtrl in Shift)and(Button = mbRight) then
 begin
  CheatCode := InputBox('','','');
  if CheatCode = 'iwouldliketobeabletogothroughthewalls' then
  begin
   FalonAtmenes := not FalonAtmenes;
   if FalonAtmenes = true then
    Application.MessageBox('Cheat mód aktiválva: átmehetsz a falon.','Labirintus',mb_IconInformation + mb_Ok)
   else
    Application.MessageBox('Falon átmenés deaktiválva.','Labirintus',mb_IconInformation + mb_Ok);
  end;

  if CheatCode = 'iwouldliketobesucceededinwinningthismarvellouslevel' then
  begin
   if (Vege)and(not CheattalNyeres) then
   begin
    Application.MessageBox('Már nyertél!','Labirintus',mb_Ok + mb_IconHand);
    Exit;
   end;
   CheattalNyeres := not CheattalNyeres;
   if CheattalNyeres then
   begin
    PasiEredetiHelyzete := TeHolVagy;
    TeHolVagy := HolVanACel;
   end
    else
   begin
    TeHolVagy := PasiEredetiHelyzete;
    Vege := false;
    StatusBar1.Panels[0].Text := '';
   end;
   UjraRajzolas;
   CelEllenorzes;
  end;

  if CheatCode = 'deleteallcheats' then
  begin
   if FalonAtmenes then
   begin
    Application.MessageBox('Falon átmenés deaktiválva.','Labirintus',mb_IconInformation + mb_Ok);
    FalonAtmenes := false;
   end;
   if CheattalNyeres then
   begin
    TeHolVagy := PasiEredetiHelyzete;
    Vege := false;
    StatusBar1.Panels[0].Text := '';
   end;
  end;
 end;
end;

procedure TFoForm.mnuSegitsegIndexClick(Sender: TObject);
begin
 Application.HelpCommand(Help_Contents,0);
end;

procedure TFoForm.mnuSegitsegTargymutatoClick(Sender: TObject);
begin
 Application.HelpCommand(Help_PartialKey,LongInt(StrNew('')));
end;

procedure TFoForm.mnuJtekUjTournamentClick(Sender: TObject);
var temp: string;
begin
 temp := LabirintusFile.ReadString('General Info','NumberOfLevels','1');
 if StrToInt(temp) < 2 then
 begin
  Application.MessageBox('Csak 1 pálya van!','Hiba',mb_Ok + mb_IconAsterisk);
  Exit;
 end;
 TournamentPalyaSzama := 1;
 TournamentMode := true;
 NextTournamentLevel;
 Application.MessageBox('Tournament mód elkezdve.','Labirintus',mb_Ok + mb_IconInformation);
end;

procedure TFoForm.NextTournamentLevel;
var i,k: integer;
    temp: string;
begin
 if not LabirintusFile.SectionExists('Level' + IntToStr(TournamentPalyaSzama)) then Exit;
 for i := 1 to 20 do
 begin
  temp := LabirintusFile.ReadString('Level' + IntToStr(TournamentPalyaSzama),'sor' + IntToStr(i),'--------------------');
  for k := 1 to 20 do Map[i,k] := temp[k];
 end;
 StatusBar1.Panels[0].Text := ''; 
 for i := 1 to 20 do
  for k := 1 to 20 do
  begin
   if Map[i,k] = 'm' then
   begin
    Map[i,k] := '-';
    TeHolVagy.x := i;
    TeHolVagy.y := k;
   end;
   if Map[i,k] = 'f' then
   begin
    HolVanACel.x := i;
    HolVanACel.y := k;
   end;
  end;
  Vege := false;
 UjraRajzolas;
end;

{ TRaketa }

constructor TRaketa.Create(AOwner: TComponent);
begin
 inherited Create;
 Letezek := false;
 with Pozicio do
 begin
  x := 0;
  y := 0;
 end;
end;

destructor TRaketa.Destroy;
begin
 Letezek := false;
 inherited Destroy;
end;

procedure TFoForm.RaketaMozgas(Melyik: integer);
var PosX, PosY: integer;
begin
 if Vege then Exit;
 PosX := Raketak[Melyik].Pozicio.x;
 PosY := Raketak[Melyik].Pozicio.y;
 case Raketak[Melyik].Irany of
  riLe: begin
         if PosX = 20 then
         begin
          Raketak[Melyik].Letezek := false;
          UjraRajzolas;
          Exit;
         end;
         if Map[PosX+1,PosY] = '+' then
         begin
          Map[PosX+1,PosY] := '-';
          Raketak[Melyik].Letezek := false;
          UjraRajzolas;
          Exit;
         end;
         Inc(Raketak[Melyik].Pozicio.x);
         UjraRajzolas;
        end;
  riFel: begin
          if PosX = 1 then
          begin
           Raketak[Melyik].Letezek := false;
           UjraRajzolas;
           Exit;
          end;
          if Map[PosX-1,PosY] = '+' then
          begin
           Map[PosX-1,PosY] := '-';
           Raketak[Melyik].Letezek := false;
           UjraRajzolas;
           Exit;
          end;
          Dec(Raketak[Melyik].Pozicio.x);
          UjraRajzolas;
         end;
  riBalra: begin
            if PosY = 1 then
            begin
             Raketak[Melyik].Letezek := false;
             UjraRajzolas;
             Exit;
            end;
            if Map[PosX,PosY-1] = '+' then
            begin
             Map[PosX,PosY-1] := '-';
             Raketak[Melyik].Letezek := false;
             UjraRajzolas;
             Exit;
            end;
           Dec(Raketak[Melyik].Pozicio.y);
           UjraRajzolas;
           end;
  riJobbra: begin
             if PosY = 20 then
             begin
              Raketak[Melyik].Letezek := false;
              UjraRajzolas;
              Exit;
             end;
             if Map[PosX,PosY+1] = '+' then
             begin
              Map[PosX,PosY+1] := '-';
              Raketak[Melyik].Letezek := false;
              UjraRajzolas;
              Exit;
             end;
             Inc(Raketak[Melyik].Pozicio.y);
             UjraRajzolas;
            end;
 end;
end;

procedure TFoForm.FormDestroy(Sender: TObject);
var x: integer;
begin
 for x := 1 to 100 do Raketak[x].Free;
end;

procedure TFoForm.RaketaIdozitoTimer(Sender: TObject);
var x: integer;
begin
 for x := 1 to 100 do
 begin
  if Raketak[x].Letezek then
   RaketaMozgas(x);
 end;
end;

procedure TFoForm.WMSysCommand(var Msg: TWmSysCommand);
var Item: TMenuItem;
begin
 Item := MainMenu1.FindItem(Msg.CmdType,fkCommand);
 if Item <> nil then
  if Assigned(Item.OnClick) then
   Item.Click;
 inherited;
end;

procedure TFoForm.mnuPalyaszerkesztoClick(Sender: TObject);
begin
 frmPalyaszerkeszto.Show;
end;

end.
