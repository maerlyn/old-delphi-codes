unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ScktComp, StdCtrls, Grids, ShellAPI, Menus, Unit2;

const
  wm_IconMessage = wm_User + 1;

type
  TfrmMainForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtCim: TEdit;
    txtUzenet: TMemo;
    cmdKuldes: TButton;
    ClientSocket1: TClientSocket;
    ServerSocket1: TServerSocket;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    StringGrid1: TStringGrid;
    PopupMenu1: TPopupMenu;
    mnuRestore: TMenuItem;
    mnuExit: TMenuItem;
    lblCharleft: TLabel;
    PopupMenu2: TPopupMenu;
    mnuDelselected: TMenuItem;
    cmdAddress: TButton;
    mnuDelall: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure cmdKuldesClick(Sender: TObject);
    procedure ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure UpdateStringGrid;
    procedure IconTray(var Msg: TMessage);message wm_IconMessage;
    procedure FormDestroy(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuRestoreClick(Sender: TObject);
    procedure UpdateTaskbarIcon;
    procedure StringGrid1DblClick(Sender: TObject);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Delay(Seconds, MilliSec: Word);
    procedure txtUzenetChange(Sender: TObject);
    procedure mnuDelselectedClick(Sender: TObject);
    procedure cmdAddressClick(Sender: TObject);
    procedure ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
    procedure mnuDelallClick(Sender: TObject);
    procedure ServerSocket1ClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;


var
  frmMainForm: TfrmMainForm;
  nid: TNotifyIconData;
  MainIcon: THandle;
  NewIcon: THandle;
  UrgentIcon: THandle;
  xCol,xRow: integer;

implementation

uses Unit3, Unit4;

{$R *.DFM}
{$R 'kepek.res'}

procedure TfrmMainForm.FormCreate(Sender: TObject);
begin
 try
  ServerSocket1.Open;
 except
  if Application.MessageBox('Nem sikerült megnyitni a portot a beérkezõ üzenetek számára. Kilépsz?','LAN messager',mb_YesNo + mb_IconQuestion) = id_Yes then
   Abort;
 end;

 StringGrid1.Cells[0,0] := '';
 StringGrid1.Cells[1,0] := '';
 StringGrid1.Cells[2,0] := 'Dátum';
 StringGrid1.Cells[3,0] := 'Küldõ';

 MennyiUzenet := 0;
 LoadMessageData;
 UpdateStringGrid;

 MainIcon := LoadIcon(hInstance,'MAIN');
 NewIcon := LoadIcon(hInstance,'NEW');
 UrgentIcon := LoadIcon(hInstance,'URGENT');

 nid.cbSize := sizeof(nid);
 nid.Wnd := Handle;
 nid.uID := 1;
 nid.uCallbackMessage := wm_IconMessage;
 nid.hIcon := MainIcon;
 nid.szTip := 'LAN messager';
 nid.uFlags := nif_Message + nif_Icon + nif_Tip;
 Shell_NotifyIcon(NIM_ADD,@nid);

 xCol := 0;
 xRow := 0;
end;

procedure TfrmMainForm.cmdKuldesClick(Sender: TObject);
var s: string;
begin
 if (CountDots(txtCim.Text)=3)and(pos('<',txtCim.Text)<1) then
  ClientSocket1.Address := txtCim.Text
 else
 begin
  s := GetIPOfName(txtCim.Text);
  if s <> 'ERROR' then
  begin
   if pos('<',s) > 0 then
   begin
    delete(s,1,pos('<',s));
    delete(s,length(s),1);
   end; 
   ClientSocket1.Address := s
  end
  else
  begin
   Application.MessageBox('Nincs ilyen név a címek között!','LAN messager',mb_OK + mb_IconError);
   Abort;
  end;
 end;

 s := 'MSG';
 if CheckBox1.Checked then
  s := s + '1' + #182
 else
  s := s + '0' + #182;
 s := s + txtUzenet.Lines.Text;

 try
  ClientSocket1.Open;
  Delay(0,100);
  ClientSocket1.Socket.SendText(s);
  Delay(0,100);
  ClientSocket1.Close;
 except
  Application.MessageBox('Hiba az üzenet küldése során!','LAN messager',mb_OK + mb_IconError);
 end;
end;

procedure TfrmMainForm.ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var s: string;
    i: integer;
begin
 s := Socket.ReceiveText;

 if pos('MSG',s) = 1 then
 begin
  inc(MennyiUzenet);
  Uzenetek[MennyiUzenet].Uj := true;
  Uzenetek[MennyiUzenet].Datum := Now;
  Uzenetek[MennyiUzenet].Kuldo := Socket.RemoteAddress;

  if s[4] = '0' then
   Uzenetek[MennyiUzenet].Surgos := false
  else if s[4] = '1' then
  begin
   Uzenetek[MennyiUzenet].Surgos := true;
   frmMainForm.BringToFront;
   Application.BringToFront;
  end;

  delete(s,1,pos(#182,s));
  ZeroMemory(@(Uzenetek[MennyiUzenet].Uzenet),2048);
  for i := 1 to length(s) do
   Uzenetek[MennyiUzenet].Uzenet[i] := s[i];
//  Uzenetek[MennyiUzenet].Uzenet := copy(s,pos(#182,s)+1,length(s));

  UpdateStringGrid;
  UpdateTaskbarIcon;
 end;
end;

procedure TfrmMainForm.UpdateStringGrid;
var i: integer;
begin
 if MennyiUzenet <> 0 then
  StringGrid1.RowCount := MennyiUzenet + 1
 else
  StringGrid1.RowCount := 2;

 if MennyiUzenet = 0 then
 begin
  StringGrid1.Cells[0,1] := '';
  StringGrid1.Cells[1,1] := '';
  StringGrid1.Cells[2,1] := '';
  StringGrid1.Cells[3,1] := '';
 end;

 for i := 1 to MennyiUzenet do
 begin
  if Uzenetek[i].Uj then
   StringGrid1.Cells[0,i] := chr(149)
  else
   StringGrid1.Cells[0,i] := '';

  if Uzenetek[i].Surgos then
   StringGrid1.Cells[1,i] := chr(149)
  else
   StringGrid1.Cells[1,i] := '';

  StringGrid1.Cells[2,i] := DateTimeToStr(Uzenetek[i].Datum);
  StringGrid1.Cells[3,i] := GetNameOfIP(Uzenetek[i].Kuldo);
 end;
end;

procedure TfrmMainForm.IconTray(var Msg: TMessage);
var Pt: TPoint;
begin
 if Msg.lParam = wm_rButtonDown then
 begin
  GetCursorPos(Pt);
  PopupMenu1.Popup(Pt.x,Pt.y);
 end
 else
 if Msg.lParam = wm_lButtonDblClk then
 begin
  mnuRestoreClick(frmMainForm); 
 end;
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
 SaveMessageData;
 nid.uFlags := 0;
 Shell_NotifyIcon(NIM_DELETE,@nid);
end;

procedure TfrmMainForm.mnuExitClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TfrmMainForm.mnuRestoreClick(Sender: TObject);
begin
 ShowWindow(Handle,sw_Show);
end;

procedure TfrmMainForm.UpdateTaskbarIcon;
var i: integer;
    new: boolean;
    urgent: boolean;
begin
 new := false;
 urgent := false;

 for i := 1 to MennyiUzenet do
  if Uzenetek[i].Uj then
  begin
   new := true;
   if Uzenetek[i].Surgos then
    urgent := true;
  end;

 if urgent then
 begin
  nid.hIcon := UrgentIcon;
  Shell_NotifyIcon(NIM_MODIFY,@nid);
 end
 else
 if new then
 begin
  nid.hIcon := NewIcon;
  Shell_NotifyIcon(NIM_MODIFY,@nid);
 end
 else
 if (not urgent) and (not new) then
 begin
  nid.hIcon := MainIcon;
  Shell_NotifyIcon(NIM_MODIFY,@nid);
 end;
end;

procedure TfrmMainForm.StringGrid1DblClick(Sender: TObject);
var frm: TfrmOlvasas;
begin
 if xRow = 0 then
  Abort;

 frm := TfrmOlvasas.Create(frmMainForm);
 frm.LoadData(Uzenetek[xRow].Surgos,DateTimeToStr(Uzenetek[xRow].Datum),GetNameOfIP(Uzenetek[xRow].Kuldo),Uzenetek[xRow].Uzenet);
 frm.Show;

 if Uzenetek[xRow].Uj then
 begin
  Uzenetek[xRow].Uj := false;
  UpdateTaskbarIcon;
  UpdateStringGrid;
 end;
end;

procedure TfrmMainForm.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 StringGrid1.MouseToCell(X,Y,xCol,xRow);
end;

procedure TfrmMainForm.Delay(Seconds, MilliSec: Word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

procedure TfrmMainForm.txtUzenetChange(Sender: TObject);
begin
 lblCharleft.Caption := 'Maradék karakter: ' + IntToStr(2048 - length(txtUzenet.Lines.Text));
end;

procedure TfrmMainForm.mnuDelselectedClick(Sender: TObject);
var i: integer;
begin
 if (xRow < 1)or(xRow > MennyiUzenet) then
  Abort;

 for i := xRow to 65534 do
 begin
  Uzenetek[i].Uj :=     Uzenetek[i+1].Uj;
  Uzenetek[i].Surgos := Uzenetek[i+1].Surgos;
  Uzenetek[i].Datum :=  Uzenetek[i+1].Datum;
  Uzenetek[i].Kuldo :=  Uzenetek[i+1].Kuldo;
  Uzenetek[i].Uzenet := Uzenetek[i+1].Uzenet;
 end;
 Uzenetek[65535].Uj := false;
 Uzenetek[65535].Surgos := false;
 Uzenetek[65535].Datum := 0;
 Uzenetek[65535].Kuldo := '';
 ZeroMemory(@(Uzenetek[65536].Uzenet),2048);
// Uzenetek[65535].Uzenet := '';

 dec(MennyiUzenet);

 UpdateStringGrid;
 UpdateTaskbarIcon;
end;

procedure TfrmMainForm.cmdAddressClick(Sender: TObject);
var frm: TfrmCimek;
begin
 frm := TfrmCimek.Create(frmMainForm);
 frm.Show;
end;

procedure TfrmMainForm.ClientSocket1Error(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
 Application.MessageBox('Hiba a csatlakozás során!','LAN messager',mb_OK + mb_IconHand);
 ErrorCode := 0;
end;

procedure TfrmMainForm.mnuDelallClick(Sender: TObject);
var i: integer;
begin
 if Application.MessageBox('Biztosan törtöd az összes eddig beérkezett üzenetet?','LAN messager',mb_YesNo + mb_IconQuestion) = idNo then
  Abort;

 for i := 1 to high(Uzenetek) do
 begin
  Uzenetek[i].Uj := false;
  Uzenetek[i].Surgos := false;
  Uzenetek[i].Datum := 0;
  Uzenetek[i].Kuldo := '';
  ZeroMemory(@(Uzenetek[i].Uzenet),2048);
 end;

 MennyiUzenet := 0;

 UpdateTaskbarIcon;
 UpdateStringGrid;
end;

procedure TfrmMainForm.ServerSocket1ClientError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
 Application.MessageBox('Hiba a szerver üzeme során!','LAN messager',mb_OK + mb_IconHand);
 ErrorCode := 0;
end;

end.
