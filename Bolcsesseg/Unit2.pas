unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, Menus, ShellAPI, Unit1, Registry, StdCtrls;

const wm_IconMessage = wm_User;

type
  TTalcaraUloForm = class(TForm)
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    mnuMindenIndulaskor: TMenuItem;
    mnuBolcsessegMutatasa: TMenuItem;
    N1: TMenuItem;
    mnuNevjegy: TMenuItem;
    mnuKilepes: TMenuItem;
    mnuWinInditasakor: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure TalcaraUles;
    procedure IconTray(var Msg: TMessage);message wm_IconMessage;
    procedure TunesATalcarol;
    procedure mnuBolcsessegMutatasaClick(Sender: TObject);
    procedure mnuKilepesClick(Sender: TObject);
    procedure mnuNevjegyClick(Sender: TObject);
    procedure mnuMindenIndulaskorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuWinInditasakorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TalcaraUloForm: TTalcaraUloForm;
  nid: TNotifyIconData;
  LentATalcan: boolean;
  Reg: TRegistry;

implementation

{$R *.DFM}

procedure TTalcaraUloForm.FormShow(Sender: TObject);
begin
 TalcaraUloForm.Hide;
end;

procedure TTalcaraUloForm.IconTray(var Msg: TMessage);
var Pt: TPoint;
begin
 if (Msg.lParam = wm_lButtonUp) or (Msg.lParam = wm_rButtonUp) then
 begin
  GetCursorPos(Pt);
  PopupMenu1.Popup(Pt.X,Pt.Y);
 end;
 if Msg.lParam = wm_lButtonDblClk then FoForm.BolcsessegMutatasa;
end;

procedure TTalcaraUloForm.TalcaraUles;
begin
 nid.cbSize := SizeOf(nid);
 nid.Wnd := Handle;
 nid.uID := 1;
 nid.uCallbackMessage := wm_IconMessage;
 nid.hIcon := Application.Icon.Handle;
 nid.szTip := 'Napi bölcsesség';
 nid.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
 Shell_NotifyIcon(NIM_ADD,@nid);
 LentATalcan := false;
end;

procedure TTalcaraUloForm.TunesATalcarol;
begin
 nid.uFlags := 0;
 Shell_NotifyIcon(NIM_DELETE,@nid);
 LentATalcan := false;
end;

procedure TTalcaraUloForm.mnuBolcsessegMutatasaClick(Sender: TObject);
begin
 FoForm.BolcsessegMutatasa;
end;

procedure TTalcaraUloForm.mnuKilepesClick(Sender: TObject);
begin
 TunesATalcarol;
 Application.Terminate;
end;

procedure TTalcaraUloForm.mnuNevjegyClick(Sender: TObject);
var Szoveg: string;
begin
 Szoveg := 'Napi Bölcsesség v1.0'#13#10;
 Szoveg := Szoveg + 'by Putra Ware, Hungary, 2001.'#13#10#13#10;
 Szoveg := Szoveg + 'e-Mail: televonzsinor@yahoo.com'#13#10;
 Szoveg := Szoveg + 'homepage: http://putraware.ini.hu';
 Application.MessageBox(PChar(Szoveg),'Névjegy',MB_OK + MB_ICONINFORMATION)
end;

procedure TTalcaraUloForm.mnuMindenIndulaskorClick(Sender: TObject);
begin
 if mnuMindenIndulaskor.Tag = 0 then mnuMindenIndulaskor.Tag := 1
  else mnuMindenIndulaskor.Tag := 0;
 mnuMindenIndulaskor.ImageIndex := mnuMindenIndulaskor.Tag;

 Reg := TRegistry.Create;
 Reg.RootKey := HKEY_CURRENT_USER;
 Reg.OpenKey('SOFTWARE\Zsinor Ware\Bolcsesseg',true);
 if mnuMindenIndulaskor.Tag = 1 then Reg.WriteInteger('MindenIndulaskor',0);
 if mnuMindenIndulaskor.Tag = 0 then Reg.WriteInteger('MindenIndulaskor',1);
 Reg.CloseKey;
// Reg.Free;
end;

procedure TTalcaraUloForm.FormCreate(Sender: TObject);
begin
 Reg := TRegistry.Create;
 Reg.RootKey := HKEY_CURRENT_USER;
 Reg.OpenKey('SOFTWARE\Zsinor Ware\Bolcsesseg',true);
 if Reg.ValueExists('MindenIndulaskor') then
  mnuMindenIndulaskor.Tag := Reg.ReadInteger('MindenIndulaskor')
 else
  mnuMindenIndulaskor.Tag := 1;

 mnuMindenIndulaskorClick(Sender);
 mnuWinInditasakorClick(Sender);
 Reg.CloseKey;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',false);
 if Reg.KeyExists('NB') then
  mnuWinInditasakor.Tag := 0
 else
  mnuWinInditasakor.Tag := 1;
 Reg.CloseKey;
// Reg.Free;
end;

procedure TTalcaraUloForm.FormDestroy(Sender: TObject);
begin
 mnuKilepesClick(Sender);
end;

procedure TTalcaraUloForm.mnuWinInditasakorClick(Sender: TObject);
begin
 if mnuWinInditasakor.Tag = 0 then mnuWinInditasakor.Tag := 1
  else mnuWinInditasakor.Tag := 0;
 mnuWinInditasakor.ImageIndex := mnuWinInditasakor.Tag;

 Reg := TRegistry.Create;
 Reg.RootKey := HKEY_LOCAL_MACHINE;
 Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run',false);
 if mnuWinInditasakor.Tag = 0 then
  Reg.WriteString('NB',Application.ExeName)
 else
  Reg.DeleteValue('NB'); 
 Reg.CloseKey;
// Reg.Free;
end;

end.
