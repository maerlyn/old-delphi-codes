unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ShellAPI, ClipBrd, StdCtrls, RichEdit, Unit2, Registry, ExtCtrls,
  MPlayer;

const wm_IconMessage = wm_User;

type
  TForm1 = class(TForm)
    PopupMenu1: TPopupMenu;
    mnuEmpty: TMenuItem;
    mnuEdit: TMenuItem;
    mnuWhatIsOn: TMenuItem;
    N1: TMenuItem;
    mnuAbout: TMenuItem;
    mnuExit: TMenuItem;
    txtSzovegmezo: TMemo;
    btnOK: TButton;
    btnCancel: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuEmptyClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuWhatIsOnClick(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure IconTray(var Msg: TMessage);message wm_IconMessage;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  nid: TNotifyIconData;
  IdozitoKepAktiv: Boolean;

implementation

{$R *.DFM}

{ TForm1 }

procedure TForm1.IconTray(var Msg: TMessage);
var Pt: TPoint;
begin
 if (Msg.lParam = wm_rButtonDown) or (Msg.LParam = wm_lButtonDown) then begin
  GetCursorPos(Pt);
  Popupmenu1.Popup(Pt.X,Pt.Y);
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 nid.cbSize := SizeOf(nid);
 nid.Wnd := Handle;
 nid.uID := 1;
 nid.uCallbackMessage := wm_IconMessage;
 nid.hIcon := Application.Icon.Handle;
 nid.szTip := 'Vágólapkezelõ';
 nid.uFlags := NIF_MESSAGE + NIF_ICON + NIF_TIP;
 Shell_NotifyIcon(NIM_ADD,@nid);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var Kereso: TSearchRec;
    Szam: SHORT;
label Vege;
begin
 nid.uFlags := 0;
 Shell_NotifyIcon(NIM_DELETE,@nid);
 FindFirst('C:\WINDOWS\$vk\*.*',faAnyFile,Kereso);
 if Kereso.Name<>'' then DeleteFile(Kereso.Name);
 While Kereso.Name<>'' do
 begin
  FindNext(Kereso);
  DeleteFile(Kereso.Name);
  Szam := Szam + 1;
  if Szam >= 10 then goto Vege;
 end;
Vege:
 FindClose(Kereso);
{$I-}
 RmDir('C:\WINDOWS\Temp\$vk');
{$I+}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caNone;
 ShowWindow(Handle,SW_HIDE);
end;

procedure TForm1.mnuExitClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm1.mnuEmptyClick(Sender: TObject);
begin
 if Application.MessageBox('Tényleg törölni akarod a vágólap tartalmát?','Vágólapkezelõ',MB_YESNO) = idYes then
 begin
  Clipboard.Clear;
 end;
end;

procedure TForm1.mnuAboutClick(Sender: TObject);
begin
 Form2.Show;
end;

procedure TForm1.mnuWhatIsOnClick(Sender: TObject);
var Szovegmezo: TMemo;
begin
 Szovegmezo := TMemo.Create(Form1);
 if Clipboard.HasFormat(CF_BITMAP) then Application.MessageBox('A vágólapon bittérkép típusú kép van.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else if Clipboard.HasFormat(CF_METAFILEPICT) then Application.MessageBox('A vágólapon metafájl típusú kép van.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else if Clipboard.HasFormat(CF_TEXT) then Application.MessageBox('A vágólapon szöveg van.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else if Clipboard.HasFormat(CF_TIFF) then Application.MessageBox('A vágólapon TIFF kép van','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else if Clipboard.HasFormat(CF_WAVE) then Application.MessageBox('A vágólapon hanghullám hang (WAV) van.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else if SendMessage(txtSzovegmezo.Handle,em_CanPaste,0,0) = 0 then Application.MessageBox('A vágólap üres.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION)
 else Application.MessageBox('A vágólapon fel nem ismert formátumú adat van.','Vágólapkezelõ',MB_OK+MB_ICONINFORMATION);
 Szovegmezo.Free;
end;

procedure TForm1.mnuEditClick(Sender: TObject);
var Kep: TImage;
    Reg: TRegistry;
    temp: string;
    SzovegFile: TextFile;
begin
 Reg := TRegistry.Create;
 Reg.RootKey := HKEY_CLASSES_ROOT;
 if Clipboard.HasFormat(CF_BITMAP) then
 begin
  Reg.OpenKey('.bmp',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Reg.OpenKey(temp + '\shell\open\command',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Kep := TImage.Create(Form1);
  Kep.Picture.Assign(Clipboard);
{$I-}
  MkDir('C:\WINDOWS\Temp\$vk');
{$I+}
  AssignFile(SzovegFile,'C:\WINDOWS\Temp\$vk\image.bmp');
  ReWrite(SzovegFile);
  CloseFile(SzovegFile);
  Kep.Picture.SaveToFile('C:\WINDOWS\Temp\$vk\image.bmp');
  WinExec(PChar(temp + 'C:\WINDOWS\Temp\$vk\image.bmp'),SW_SHOWNORMAL);
  Kep.Free;
 end
 else if Clipboard.HasFormat(CF_METAFILEPICT) then
 begin
  Reg.OpenKey('.wmf',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Reg.OpenKey(temp + '\shell\open\command',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Kep := TImage.Create(Form1);
  Kep.Picture.Assign(Clipboard);
{$I-}
  MkDir('C:\WINDOWS\Temp\$vk');
{$I+}
  AssignFile(SzovegFile,'C:\WINDOWS\Temp\$vk\image.wmf');
  ReWrite(SzovegFile);
  CloseFile(SzovegFile);
  Kep.Picture.SaveToFile('C:\WINDOWS\Temp\$vk\image.wmf');
  WinExec(PChar(temp + 'C:\WINDOWS\Temp\$vk\image.wmf'),SW_SHOWNORMAL);
  Kep.Free;
 end
 else if Clipboard.HasFormat(CF_TEXT) then
 begin
  txtSzovegmezo.Lines.Clear;
  txtSzovegmezo.Lines.Text := Clipboard.AsText;
  Form1.Visible := True;
 end
 else if Clipboard.HasFormat(CF_TIFF) then
 begin
  Reg.OpenKey('.tif',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Reg.OpenKey(temp + '\shell\open\command',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Kep := TImage.Create(Form1);
  Kep.Picture.Assign(Clipboard);
{$I-}
  MkDir('C:\WINDOWS\Temp\$vk');
{$I+}
  AssignFile(SzovegFile,'C:\WINDOWS\Temp\$vk\image.tif');
  ReWrite(SzovegFile);
  CloseFile(SzovegFile);
  Kep.Picture.SaveToFile('C:\WINDOWS\Temp\$vk\image.tif');
  WinExec(PChar(temp + 'C:\WINDOWS\Temp\$vk\image.tif'), SW_SHOWNORMAL);
  Kep.Free;
 end
 else if Clipboard.HasFormat(CF_WAVE) then
 begin
  Reg.OpenKey('.wav',false);
  temp := Reg.ReadString('');
  Reg.CloseKey;
  Reg.OpenKey(temp + '\shell\open\command',false);
  temp := reg.ReadString('');
  Reg.CloseKey;
  WinExec(PChar(temp),SW_SHOWNORMAL);
 end
 else if SendMessage(txtSzovegmezo.Handle,em_CanPaste,0,0) = 0 then
  Application.MessageBox('A vágólap üres.','Vágólapkezelõ',MB_OK+MB_ICONHAND)
 else Application.MessageBox('A vágólapon fel nem ismert formátumú adat van!','Vágólapkezelõ',MB_OK+MB_ICONHAND);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 IdozitoKepAktiv := not IdozitoKepAktiv;
 Case IdozitoKepAktiv of
  True:
   begin
    nid.hIcon := Form2.Icon.Handle;
    nid.uFlags := NIF_ICON + NIF_TIP;
    Shell_NotifyIcon(NIM_MODIFY,@nid);
   end;
  False:
   begin
    nid.hIcon := Application.Icon.Handle;
    nid.uFlags := NIF_ICON + NIF_TIP;
    Shell_NotifyIcon(NIM_MODIFY,@nid);
   end;
 end;
end;

procedure TForm1.btnOKClick(Sender: TObject);
begin
 txtSzovegmezo.SelectAll;
 txtSzovegmezo.CutToClipboard;
 Form1.Visible := False;
end;

procedure TForm1.btnCancelClick(Sender: TObject);
begin
 txtSzovegmezo.Lines.Clear;
 Form1.Visible := False;
end;

end.
