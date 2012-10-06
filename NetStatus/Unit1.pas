unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, MessengerAPI_TLB, ComObj, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdMultipartFormData,
  ComCtrls, ShellAPI, Menus;

const
  wm_TrayIcon = wm_User + 1;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Image1: TImage;
    Image2: TImage;
    Label2: TLabel;
    Button2: TButton;
    Button3: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Button4: TButton;
    Button5: TButton;
    Label7: TLabel;
    Image3: TImage;
    Button6: TButton;
    Label8: TLabel;
    IdHTTP1: TIdHTTP;
    Image4: TImage;
    Button8: TButton;
    Label9: TLabel;
    TrackBar1: TTrackBar;
    Label10: TLabel;
    Timer1: TTimer;
    Label11: TLabel;
    Label12: TLabel;
    ComboBox1: TComboBox;
    PopupMenu1: TPopupMenu;
    Kilps1: TMenuItem;
    Frisstsengedlyezve1: TMenuItem;
    Gpnl1: TMenuItem;
    Elfoglalt1: TMenuItem;
    Mindjrtjvk1: TMenuItem;
    Nemvagyokagpnl1: TMenuItem;
    Hzonkvl1: TMenuItem;
    Kajolok1: TMenuItem;
    N1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateData;
    procedure Button8Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure IconTray(var Msg: TMessage);message wm_TrayIcon;
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Kilps1Click(Sender: TObject);
    procedure Frisstsengedlyezve1Click(Sender: TObject);
    procedure Gpnl1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  oMess : IMessenger;
  oMessServices : IMessengerServices;
  oMessService : IMessengerService;
  dataWinamp,
  dataBSPlayer,
  dataFree,
  dataRAM,
  dataMIRC,
  dataMSN,
  dataUptime: string;
  TimePassed: integer;
  nid: TNotifyIconData;

const
  Base64_Table : shortstring = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';


implementation

{$R *.dfm}

function Base64Encode(const Source: AnsiString): AnsiString;
var
  NewLength: Integer;
begin
  NewLength := ((2 + Length(Source)) div 3) * 4;
  SetLength( Result, NewLength);

  asm
    Push  ESI
    Push  EDI
    Push  EBX
    Lea   EBX, Base64_Table
    Inc   EBX                // Move past String Size (ShortString)
    Mov   EDI, Result
    Mov   EDI, [EDI]
    Mov   ESI, Source
    Mov   EDX, [ESI-4]        //Length of Input String
@WriteFirst2:
    CMP EDX, 0
    JLE @Done
    MOV AL, [ESI]
    SHR AL, 2
{$IFDEF VER140} // Changes to BASM in D6
    XLATB
{$ELSE}
    XLAT
{$ENDIF}
    MOV [EDI], AL
    INC EDI
    MOV AL, [ESI + 1]
    MOV AH, [ESI]
    SHR AX, 4
    AND AL, 63
{$IFDEF VER140} // Changes to BASM in D6
    XLATB
{$ELSE}
    XLAT
{$ENDIF}
    MOV [EDI], AL
    INC EDI
    CMP EDX, 1
    JNE @Write3
    MOV AL, 61                        // Add ==
    MOV [EDI], AL
    INC EDI
    MOV [EDI], AL
    INC EDI
    JMP @Done
@Write3:
    MOV AL, [ESI + 2]
    MOV AH, [ESI + 1]
    SHR AX, 6
    AND AL, 63
{$IFDEF VER140} // Changes to BASM in D6
    XLATB
{$ELSE}
    XLAT
{$ENDIF}
    MOV [EDI], AL
    INC EDI
    CMP EDX, 2
    JNE @Write4
    MOV AL, 61                        // Add =
    MOV [EDI], AL
    INC EDI
    JMP @Done
@Write4:
    MOV AL, [ESI + 2]
    AND AL, 63
{$IFDEF VER140} // Changes to BASM in D6
    XLATB
{$ELSE}
    XLAT
{$ENDIF}
    MOV [EDI], AL
    INC EDI
    ADD ESI, 3
    SUB EDX, 3
    JMP @WriteFirst2
@done:
    Pop EBX
    Pop EDI
    Pop ESI
  end;
end;


procedure TForm1.Button1Click(Sender: TObject);
var winampTitle: array[0..255] of char;
    s: string;
    h: HWND;
begin
 h := FindWindow('Winamp v1.x',nil);
 if h > 0 then
 begin
  GetWindowText(h,winampTitle,255);
  s := winampTitle;
  delete(s,1,pos(' ',s));
  delete(s,length(s)-length(' - winamp')+1,length(s));
  Label1.Caption := s;
  dataWinamp := trim(s);
 end
 else
 begin
  Label1.Caption := 'Winamp beza nem fut';
  dataWinamp := '0';
 end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var h: HWND;
    s: string;
    bsTitle: array[0..255] of char;
    cds: TCopyDataStruct;
    adr: pointer;
begin
 h := FindWindow('BSPlayer',nil);
 if h > 0 then
 begin
  adr:=@bsTitle;
  cds.dwData:=$1010B;
  cds.lpData:=@adr;
  cds.cbData:=4;
  SendMessage(h,WM_COPYDATA,Application.Handle,lParam(@cds));
  s := bsTitle;
  Label2.Caption := ExtractFileName(s);
  dataBSPlayer := Label2.Caption;
 end
 else
 begin
  Label2.Caption := 'BSPlayer biza nem fut';
  dataBSPlayer := '0';
 end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var freeC,totalC: int64;
    freeD,totalD: int64;
    freeE,totalE: int64;
    temp: int64;
begin
 GetDiskFreeSpaceEx('C:\',freeC,totalC,nil);
 GetDiskFreeSpaceEx('D:\',freeD,totalD,nil);
 GetDiskFreeSpaceEx('E:\',freeE,totalE,nil);

 temp := 1024*1024*1024;
 Label3.Caption := Format('%.2f / %.2f',[freeC/temp,totalC/temp]);
 Label4.Caption := Format('%.2f / %.2f',[freeD/temp,totalD/temp]);
 Label5.Caption := Format('%.2f / %.2f',[freeE/temp,totalE/temp]);

 dataFree := Format('%.2f|%.2f#%.2f|%.2f#%.2f|%.2f',[freeC/temp,totalC/temp,freeD/temp,totalD/temp,freeE/temp,totalE/temp]);
// showmessage(datafree);
end;

procedure TForm1.Button4Click(Sender: TObject);
var buf: MEMORYSTATUS;
    temp: double;
begin
 GlobalMemoryStatus(buf);
 temp := 1024 * 1024;
 Label6.Caption := Format('%.2f / %.2f',[buf.dwAvailPhys / temp,buf.dwTotalPhys / temp]);
 dataRAM := Format('%.2f|%.2f',[buf.dwAvailPhys/temp,buf.dwTotalPhys/temp]);
end;

procedure TForm1.Button5Click(Sender: TObject);
var h: HWND;
    hMap: THandle;
    mData: LPSTR;
begin
 h := FindWindow('mIRC',nil);
 if (h > 0) then
 begin
  hMap := CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,4096,PChar('mIRC'));
  if hMap > 0 then
  begin
   mData := MapViewOfFile(hMap,FILE_MAP_ALL_ACCESS,0,0,0);
   StrCopy(mData,PChar('$me'));
   SendMEssage(h,wm_User + 201,1,0);
   Label7.Caption := Format('%s',[mData]);
   datamIRC := Label7.Caption;
   UnmapViewOfFile(mData);
   CloseHandle(hMap);
  end
  else
   Label7.Caption := 'Hiba a mIRC lekérés során';
 end
 else
 begin
  Label7.Caption := 'mIRC biza nem fut';
  datamIRC := Label7.Caption;
 end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
 try
  case oMessService.MyStatus of
   MISTATUS_UNKNOWN : Label8.Caption := 'ismeretlen';
   MISTATUS_OFFLINE : Label8.Caption := 'nem kapcsolódik';
   MISTATUS_ONLINE : Label8.Caption := 'elérhetõ';
   MISTATUS_INVISIBLE : Label8.Caption := 'láthatatlan';
   MISTATUS_BUSY : Label8.Caption := 'elfoglalt';
   MISTATUS_BE_RIGHT_BACK : Label8.Caption := 'mindjárt jön';
   MISTATUS_IDLE : Label8.Caption := 'tétlen';
   MISTATUS_AWAY : Label8.Caption := 'nincs a gépnél';
   MISTATUS_ON_THE_PHONE : Label8.Caption := 'telefonál';
   MISTATUS_OUT_TO_LUNCH : Label8.Caption := 'kajál';
   MISTATUS_LOCAL_FINDING_SERVER : Label8.Caption := 'keresi a szervert';
   MISTATUS_LOCAL_CONNECTING_TO_SERVER : Label8.Caption := 'kapcsolódik a szerverhez';
   MISTATUS_LOCAL_SYNCHRONIZING_WITH_SERVER : Label8.Caption := 'szinkronizál a szerverrel';
   MISTATUS_LOCAL_DISCONNECTING_FROM_SERVER : Label8.Caption := 'lekapcsolódik a szerverrõl';
   else
    Label8.Caption := 'egyéb';
  end;
  dataMSN := IntToStr(oMessService.MyStatus);
 except
  on E: Exception do
  begin
   Label8.Caption := 'Az MSN biza nem fut';
   dataMSN := '0';
  end;
 end;
end;

procedure TForm1.FormCreate;
begin
 oMess := CreateCOMObject( CLASS_Messenger ) as IMessenger;
 oMessServices := ( oMess.Services ) as IMessengerServices;
 oMessService := ( oMessServices.PrimaryService ) as IMessengerService;
 Trackbar1Change(Sender);
 Label11.Caption := 'Elõzõ frissítés óta eltelt: 0 másodperc';
 Label12.Caption := 'Utolsó frissítés állapota: nem volt még';
 TimePassed := 0;

 nid.cbSize := sizeof(nid);
 nid.Wnd := Handle;
 nid.uID := 1;
 nid.uCallbackMessage := wm_TrayIcon;
 nid.hIcon := LoadIcon(hInstance,'MAINICON');
 nid.szTip := 'NetStatus';
 nid.uFlags := nif_Message + nif_Icon + nif_Tip;
 Shell_NotifyIcon(nim_Add,@nid);
end;

procedure TForm1.UpdateData;
var s: string;
    Params: TIdMultiPartFormDataStream;
begin
 Button1Click(Self);
 Button2Click(Self);
 Button3Click(Self);
 Button4Click(Self);
 Button5Click(Self);
 Button6Click(Self);
 Button8Click(Self);

 s := 'a:8:{';
 s := s + 's:6:"winamp";s:' + IntToStr(length(dataWinamp)) + ':"' + dataWinamp + '";';
 s := s + 's:8:"bsplayer";s:' + IntToStr(length(dataBSPlayer)) + ':"' + dataBSPlayer + '";';
 s := s + 's:4:"free";s:' + IntToStr(length(dataFree)) + ':"' + dataFree + '";';
 s := s + 's:3:"ram";s:' + IntToStr(length(dataRAM)) + ':"' + dataRAM + '";';
 s := s + 's:4:"mirc";s:' + IntToStr(length(datamIRC)) + ':"' + datamIRC + '";';
 s := s + 's:3:"msn";s:' + IntToStr(length(dataMSN)) + ':"' + dataMSN + '";';
 s := s + 's:6:"uptime";s:' + IntToStr(length(dataUptime)) + ':"' + dataUptime + '";';
 s := s + 's:6:"status";s:' + IntToStr(length(ComboBox1.Items[ComboBox1.ItemIndex])) + ':"' + ComboBox1.Items[ComboBox1.ItemIndex] + '";';
 s := s + '}';

// Edit1.Text := s;
//a:6:{i:0;s:1:"0";i:1;s:31:"Weeds.S01E03.HDTV.XviD-LOKi.avi";i:2;s:35:"4,47|23,12#20,65|125,93#14,37|76,33";i:3;s:14:"512,60|1023,23";i:4;s:7:"Maerlyn";i:5;s:2:"10";}

 Params := TIdMultiPartFormDataStream.Create;

 try
  try
   Params.AddFormField('data',Base64Encode(s));
//   if trim(IdHTTP1.Post('http://localhost/uj/netstatus.php?source=mazsola',Params)) = 'OK' then
   s := trim(IdHTTP1.Post('http://ikon.inf.elte.hu/~fasiga/netstatus.php?source=mazsola',Params));
   if pos('OK',s)<1 then
    Label12.Caption := 'Utolsó frissítés állapota: sikeres'
   else
    Label12.Caption := 'Utolsó frissítés állapota: sikertelen, a szerver van a gond'+#13#10+s;
  finally
   Params.Free;
  end;
 except
  Label12.Caption := 'Utolsó frissítés állapota: sikertelen, itt a gond';
 end;
end;

procedure TForm1.Button8Click(Sender: TObject);
begin
 Label9.Caption := IntToStr(GetTickCount div 1000);
 dataUptime := Label9.Caption;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
 if Trackbar1.Position = 0 then
  Timer1.Enabled := false
  else
   if not Timer1.Enabled then
    Timer1.Enabled := true;

 Label10.Caption := 'Frissítés ennyi másodpercenként: ' + IntToStr(TrackBar1.Position);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 inc(TimePassed);
 Label11.Caption := 'Utolsó frissítés óta eltelt: ' + IntToStr(TimePassed) + ' másodperc';
 if TimePassed >= Trackbar1.Position then
 begin
  UpdateData;
  TimePassed := 0;
 end;
end;

procedure TForm1.IconTray(var Msg: TMessage);
var Pt: TPoint;
begin
 if Msg.lParam = wm_rButtonDown then
 begin
  GetCursorPos(Pt);
  PopupMenu1.Popup(Pt.X,Pt.Y);
 end;

 if Msg.lParam = wm_lButtonDblClk then
 begin
  ShowWindow(Application.Handle,sw_Show);
  ShowWindow(Form1.Handle,sw_Show);
 end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 nid.uFlags := 0;
 Shell_NotifyIcon(nim_Delete,@nid);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caNone;
 ShowWindow(Form1.Handle,sw_Hide);
 ShowWindow(Application.Handle,sw_Hide);
end;

procedure TForm1.Kilps1Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm1.Frisstsengedlyezve1Click(Sender: TObject);
begin
 Frisstsengedlyezve1.Checked := not Frisstsengedlyezve1.Checked; 
 Timer1.Enabled := Frisstsengedlyezve1.Checked;
end;

procedure TForm1.Gpnl1Click(Sender: TObject);
begin
 ComboBox1.ItemIndex := (Sender as TMenuItem).Tag;
 (Sender as TMenuItem).Checked := true;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
 case ComboBox1.ItemIndex of
  0: Gpnl1.Checked := true;
  1: Elfoglalt1.Checked := true;
  2: Mindjrtjvk1.Checked := true;
  3: Nemvagyokagpnl1.Checked := true;
  4: Hzonkvl1.Checked := true;
  5: Kajolok1.Checked := true;
 end;
end;

end.
