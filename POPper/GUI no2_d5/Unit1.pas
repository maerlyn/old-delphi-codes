unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, ExtCtrls, ComCtrls, ImgList, StdCtrls, Jpeg, IniFiles, OleCtrls,
  SHDocVw, POPperData, ButtonWithColor, Menus, Buttons, NMsmtp, Psock,
  NMpop3, Egyenlo, TitleButton, ShellAPI;

const wm_IconTray = wm_User + 2;

type
  TfrmMainForm = class(TForm)
    TreeView1: TTreeView;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    PageControl1: TPageControl;
    tabPostalada: TTabSheet;
    tabBeallitasok: TTabSheet;
    Label2: TLabel;
    Label1: TLabel;
    Image1: TImage;
    Image2: TImage;
    tabLevelkuldesibeallitasok: TTabSheet;
    tabLevelletoltesibeallitasok: TTabSheet;
    GroupBox3: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    txtPop3Host: TEdit;
    txtPop3Userid: TEdit;
    txtPop3Password: TEdit;
    CheckBox2: TCheckBox;
    GroupBox1: TGroupBox;
    Label11: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Edit4: TEdit;
    Edit5: TEdit;
    Label15: TLabel;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    RadioGroup2: TRadioGroup;
    tabBejovolevelek: TTabSheet;
    ListView1: TListView;
    Splitter2: TSplitter;
    Panel1: TPanel;
    popBejovo: TPOPperData;
    popElkuldott: TPOPperData2;
    popKuldendo: TPOPperData2;
    WebBrowser1: TWebBrowser;
    tabElkuldendo: TTabSheet;
    ListView2: TListView;
    Splitter3: TSplitter;
    Panel2: TPanel;
    WebBrowser2: TWebBrowser;
    tabKuldott: TTabSheet;
    ListView3: TListView;
    Splitter4: TSplitter;
    Panel3: TPanel;
    WebBrowser3: TWebBrowser;
    tabAltalanosbeallitasok: TTabSheet;
    ColorDialog1: TColorDialog;
    PopupMenu1: TPopupMenu;
    mnuDelete: TMenuItem;
    tabUjlevel: TTabSheet;
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    tabInternet: TTabSheet;
    Panel4: TPanel;
    GroupBox4: TGroupBox;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    ListBox2: TListBox;
    ProgressBar1: TProgressBar;
    NMPOP31: TNMPOP3;
    NMSMTP1: TNMSMTP;
    popCimek: TPOPperData3;
    PopupMenu2: TPopupMenu;
    tabCimek: TTabSheet;
    Panel5: TPanel;
    ListView4: TListView;
    PopupMenu3: TPopupMenu;
    nmuCimekUj: TMenuItem;
    mnuCimekSzerkesztes: TMenuItem;
    mnuCimekTorles: TMenuItem;
    Panel6: TPanel;
    Label3: TLabel;
    Edit6: TEdit;
    ListBox1: TListBox;
    Label7: TLabel;
    Edit7: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit8: TEdit;
    Edit9: TEdit;
    Label6: TLabel;
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SaveDialog1: TSaveDialog;
    FontDialog1: TFontDialog;
    tabSugo: TTabSheet;
    WebBrowser4: TWebBrowser;
    TitleButton1: TTitleButton;
    Panel7: TPanel;
    GroupBox2: TGroupBox;
    BitBtnWithColor1: TBitBtnWithColor;
    BitBtnWithColor2: TBitBtnWithColor;
    SpeedButton6: TSpeedButton;
    cbxLevelekellenorzese: TCheckBox;
    Label16: TLabel;
    PopupMenu4: TPopupMenu;
    mnuPopNow: TMenuItem;
    mnuQuit: TMenuItem;
    Timer2: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure tabLevelkuldesibeallitasokShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tabLevelletoltesibeallitasokShow(Sender: TObject);
    procedure tabLevelletoltesibeallitasokHide(Sender: TObject);
    procedure tabLevelkuldesibeallitasokHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tabBejovolevelekShow(Sender: TObject);
    procedure ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure tabElkuldendoShow(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tabAltalanosbeallitasokShow(Sender: TObject);
    procedure BitBtnWithColor1Click(Sender: TObject);
    procedure BitBtnWithColor2Click(Sender: TObject);
    procedure tabAltalanosbeallitasokHide(Sender: TObject);
    procedure ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure mnuDeleteClick(Sender: TObject);
    procedure tabKuldottShow(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WebBrowserBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
    procedure ListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    procedure TreeView1Collapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
    procedure SpeedButton3Click(Sender: TObject);
    procedure RestoreWIndowBecauseOfAMessage(var Msg: TMessage);message wm_User;
    procedure EnableInternetButtons(enable: boolean);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure tabInternetShow(Sender: TObject);
    procedure AddToList(s: string);
    procedure NMPOP31List(Msg, Size: Integer);
    procedure StartMailDownload(var Msg: TMessage);message wm_User+1;
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure LoadVersionInfo;
    procedure PopupMenu2Popup(Sender: TObject);
    procedure PopupMenu2ItemClick(Sender: TObject);
    procedure tabCimekShow(Sender: TObject);
    procedure ListView4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure nmuCimekUjClick(Sender: TObject);
    procedure mnuCimekSzerkesztesClick(Sender: TObject);
    procedure mnuCimekTorlesClick(Sender: TObject);
    procedure NMPOP31AuthenticationFailed(var Handled: Boolean);
    procedure NMPOP31AuthenticationNeeded(var Handled: Boolean);
    procedure NMPOP31ConnectionFailed(Sender: TObject);
    procedure TitleButton1MouseUp(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure NMSMTP1Connect(Sender: TObject);
    procedure NMSMTP1Disconnect(Sender: TObject);
    procedure NMPOP31Connect(Sender: TObject);
    procedure NMPOP31Disconnect(Sender: TObject);
    procedure txtLevelekellenorzeseKeyPress(Sender: TObject; var Key: Char);
    procedure cbxLevelekellenorzeseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IconMessage(var Msg: TMessage);message wm_IconTray;
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuPopNowClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure AddSysTrayIcon;
    procedure ShowBalloonTips(num: integer);
  private
    { Private declarations }
  public
    cfg: TIniFile;
  end;

type TThreadSMTP = class(TThread)
     protected
       procedure Execute;override;
     end;

     TThreadPOP3 = class(TThread)
     protected
       procedure Execute;override;
     end;

type TLangInfoBuffer = array[1..4] of smallint;

const
  NIF_INFO = $10;
  NIF_MESSAGE = 1;
  NIF_ICON = 2;
  NOTIFYICON_VERSION = 3;
  NIF_TIP = 4;
  NIM_SETVERSION = $00000004;
  NIM_SETFOCUS = $00000003;
  NIIF_INFO = $00000001;
  NIIF_WARNING = $00000002;
  NIIF_ERROR = $00000003;

  NIN_BALLOONSHOW = WM_USER + 2;
  NIN_BALLOONHIDE = WM_USER + 3;
  NIN_BALLOONTIMEOUT = WM_USER + 4;
  NIN_BALLOONUSERCLICK = WM_USER + 5;
  NIN_SELECT = WM_USER + 0;
  NINF_KEY = $1;
  NIN_KEYSELECT = NIN_SELECT or NINF_KEY;
  
  TRAY_CALLBACK = WM_USER + $7258;

type
  PNewNotifyIconData = ^TNewNotifyIconData;
  TDUMMYUNIONNAME    = record
    case Integer of
      0: (uTimeout: UINT);
      1: (uVersion: UINT);
  end;

  TNewNotifyIconData = record
    cbSize: DWORD;
    Wnd: HWND;
    uID: UINT;
    uFlags: UINT;
    uCallbackMessage: UINT;
    hIcon: HICON;
   //Version 5.0 is 128 chars, old ver is 64 chars
    szTip: array [0..127] of Char;
    dwState: DWORD; //Version 5.0
    dwStateMask: DWORD; //Version 5.0
    szInfo: array [0..255] of Char; //Version 5.0
    DUMMYUNIONNAME: TDUMMYUNIONNAME;
    szInfoTitle: array [0..63] of Char; //Version 5.0
    dwInfoFlags: DWORD;   //Version 5.0
  end;

var
  frmMainForm: TfrmMainForm;
  newlistviewitem: TListItem;
  FirstShow: boolean;
  RightClickPos: record
                  ListViewNum: byte;
                  ItemIndex: integer;
                 end;
  LastItem: TListItem;
  ColumnToSort: integer;
  tSMTP: TThreadSMTP;
  tPOP3: TThreadPOP3;
  MailSizes: array[1..32767] of integer;
  StartPOP: boolean;
  LastAddressItem: TListItem;
  EditingAlreadyWrittenMail: boolean;
  EditingAlreadyWrittenMailIndex: integer;
  TimerCount: integer = 0;
  IconData: TNewNotifyIconData;

const
  INDEX_KUKA_TELE         : integer = 0;
  INDEX_KUKA_URES         : integer = 1;

  INDEX_NYIL_FEL_KEK      : integer = 2;
  INDEX_NYIL_FEL_PIROS    : integer = 3;
  INDEX_NYIL_FEL_ZOLD     : integer = 4;

  INDEX_NYIL_LE_KEK       : integer = 5;
  INDEX_NYIL_LE_PIROS     : integer = 6;
  INDEX_NYIL_LE_ZOLD      : integer = 7;

implementation

uses Unit2, About;

{$R *.DFM}
{$R 'pwlogo.res'}
{$R 'help\help.res'}

procedure TfrmMainForm.FormCreate(Sender: TObject);
var trs: TResourceStream;
    tjpeg: TJPEGImage;
    a: TFontStyles;
    i: integer;
begin
 popBejovo.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');
 popElkuldott.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'sent.pop');
 popKuldendo.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
 popCimek.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'addresses.pop');

 TreeView1.FullExpand;
 TreeView1.Items.Item[1].Selected := true;

 trs := TResourceStream.Create(hInstance,'pwlogo','data');
 tjpeg := TJPEGImage.Create;
 tjpeg.LoadFromStream(trs);
 Image1.Picture.Bitmap.Assign(tjpeg);
 Image2.Picture.Bitmap.Assign(tjpeg);

 tjpeg.Free;
 trs.Free;

 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'popper.ini');

 tabAltalanosbeallitasokShow(Sender);
 tabLevelkuldesibeallitasokShow(Sender);
 tabLevelletoltesibeallitasokShow(Sender);
 tabCimekShow(Sender);

 FirstShow := true;
 StartPOP := false;
 EditingAlreadyWrittenMail := false;

 Panel4.BevelInner := bvNone;
 Panel4.BevelOuter := bvNone;
 Panel5.BevelInner := bvNone;
 Panel5.BevelOuter := bvNone;
 Panel7.BevelInner := bvNone;
 Panel7.BevelOuter := bvNone;

 tSMTP := TThreadSMTP.Create(true);
 tPOP3 := TThreadPOP3.Create(true);

 Memo1.Font.Name := cfg.ReadString('font','name','Fixedsys');
 if cfg.ReadBool('font','bold',false) then
  include(a,fsBold);
 if cfg.ReadBool('font','italic',false) then
  include(a,fsItalic);
 if cfg.ReadBool('font','underline',false) then
  include(a,fsUnderline);
 if cfg.ReadBool('font','strikeout',false) then
  include(a,fsStrikeOut);
 Memo1.Font.Style := a;
 Memo1.Font.Size := cfg.ReadInteger('font','size',8);
 Memo1.Font.Color := cfg.ReadInteger('font','color',0);
 SpeedButton6.Font := Memo1.Font;

 LoadVersionInfo;
 WebBrowser4.Navigate('res://popper.exe/help_htm');

 if ParamCount = 2 then
 begin
  if ParamStr(1) = '/mailto' then
  begin
   for i := 0 to TreeView1.Items.Count-1 do
    if TreeView1.Items.Item[i].Text = 'Új levél' then
     TreeView1.Items.Item[i].Selected := true;
   Edit6.Text := ParamStr(2);
  end;
 end;

 AddSysTrayIcon;
end;

procedure TfrmMainForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
 if Node.Text = 'Postaláda' then
  PageControl1.ActivePage := tabPostalada
 else if Node.Text = 'Beállítások' then
  PageControl1.ActivePage := tabBeallitasok
 else if Node.Text = 'Levélküldés' then
  PageControl1.ActivePage := tabLevelkuldesibeallitasok
 else if Node.Text = 'Levélletöltés' then
  PageControl1.ActivePage := tabLevelletoltesibeallitasok
 else if Node.Text = 'Bejövõ levelek' then
  PageControl1.ActivePage := tabBejovolevelek
 else if Node.Text = 'Elküldendõ levelek' then
  PageControl1.ActivePage := tabElkuldendo
 else if Node.Text = 'Küldött levelek' then
  PageControl1.ActivePage := tabKuldott
 else if Node.Text = 'Általános' then
  PageControl1.ActivePage := tabAltalanosbeallitasok
 else if Node.Text = 'Új levél' then
  PageControl1.ActivePage := tabUjlevel
 else if Node.Text = 'Internet' then
  PageControl1.ActivePage := tabInternet
 else if Node.Text = 'Címtár' then
  PageControl1.ActivePage := tabCimek
 else if Node.Text = 'Súgó' then
  PageControl1.ActivePage := tabSugo;
end;

procedure TfrmMainForm.tabLevelkuldesibeallitasokShow(Sender: TObject);
begin
 Groupbox1.Left := (tabLevelkuldesibeallitasok.ClientWidth - Groupbox1.Width) div 2;
 Groupbox1.Top := (tabLevelkuldesibeallitasok.ClientHeight - GroupBox1.Height) div 2;

 Edit1.Text := cfg.ReadString('smtp','name','');
 Edit2.Text := cfg.ReadString('smtp','email','');
 Edit3.Text := cfg.ReadString('smtp','replyto','');
 Edit4.Text := cfg.ReadString('smtp','host','');
 Edit5.Text := cfg.ReadString('smtp','userid','');
 Checkbox4.Checked := cfg.ReadBool('smtp','delete',true);
 Checkbox5.Checked := cfg.ReadBool('smtp','save',true);
 RadioGroup2.ItemIndex := cfg.ReadInteger('smtp','format',0);
end;

procedure TfrmMainForm.FormDestroy(Sender: TObject);
begin
 ListView1.OnChange := nil;
 ListView2.OnChange := nil;
 ListView3.OnChange := nil;

 tabAltalanosbeallitasokHide(Sender);
 tabLevelletoltesibeallitasokHide(Sender);
 tabLevelkuldesibeallitasokHide(Sender);

 cfg.WriteString('font','name',Memo1.Font.Name);
 if fsBold in Memo1.Font.Style then
  cfg.WriteBool('font','bold',true)
 else
  cfg.WriteBool('font','bold',false);
 if fsItalic in Memo1.Font.Style then
  cfg.WriteBool('font','italic',true)
 else
  cfg.WriteBool('font','italic',false);
 if fsUnderline in Memo1.Font.Style then
  cfg.WriteBool('font','underline',true)
 else
  cfg.WriteBool('font','underline',false);
 if fsStrikeOut in Memo1.Font.Style then
  cfg.WriteBool('font','strikeout',true)
 else
  cfg.WriteBool('font','strikeout',false);
 cfg.WriteInteger('font','size',Memo1.Font.Size);
 cfg.WriteInteger('font','color',Memo1.Font.Color);

 cfg.UpdateFile;
 cfg.Free;

 popBejovo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');
 popElkuldott.SaveToFile(ExtractFilePath(ParamStr(0)) + 'sent.pop');
 popKuldendo.SaveToFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');
 popCimek.SaveToFile(ExtractFilePath(ParamStr(0)) + 'addresses.pop');

 if FileExists(ExtractFilePath(ParamStr(0)) + 'temp.htm') then
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'temp.htm');

 Shell_NotifyIcon(NIM_DELETE, @IconData);
 DeallocateHWnd(IconData.Wnd);
end;

procedure TfrmMainForm.tabLevelletoltesibeallitasokShow(Sender: TObject);
begin
 Groupbox3.Left := (tabLevelletoltesibeallitasok.ClientWidth - Groupbox3.Width) div 2;
 Groupbox3.Top := (tabLevelletoltesibeallitasok.ClientHeight - Groupbox3.Height) div 2;

 txtPOP3Host.Text := cfg.ReadString('pop3','host','');
 txtPOP3Userid.Text := cfg.ReadString('pop3','userid','');
 txtPOP3Password.Text := cfg.ReadString('pop3','password','');
 Checkbox2.Checked := cfg.ReadBool('pop3','delete',false);
end;

procedure TfrmMainForm.tabLevelletoltesibeallitasokHide(Sender: TObject);
var valtozas: boolean;
    i: integer;
begin
 valtozas := false;

 if txtPOP3Host.Text <> cfg.ReadString('pop3','host','') then
  valtozas := true;
 if txtPOP3Userid.Text <> cfg.ReadString('pop3','userid','') then
  valtozas := true;
 if txtPOP3Password.Text <> cfg.ReadString('pop3','password','') then
  valtozas := true;
 if Checkbox2.Checked <> cfg.ReadBool('pop3','delete',false) then
  valtozas := true;

 if not valtozas then
  Exit;

 i := Application.MessageBox('Mentsem a megváltozott beálításokat?','Putra POPper',mb_YesNo + mb_IconQuestion);
 if i = idNo then
  Exit;

 cfg.WriteString('pop3','host',txtPOP3Host.Text);
 cfg.WriteString('pop3','userid',txtPOP3Userid.Text);
 cfg.WriteString('pop3','password',txtPOP3Password.Text);
 cfg.WriteBool('pop3','delete',CheckBox2.Checked); 
end;

procedure TfrmMainForm.tabLevelkuldesibeallitasokHide(Sender: TObject);
var valtozas: boolean;
    i: integer;
begin
 valtozas := false;

 if Edit1.Text <> cfg.ReadString('smtp','name','') then
  valtozas := true;
 if Edit2.Text <> cfg.ReadString('smtp','email','') then
  valtozas := true;
 if Edit3.Text <> cfg.ReadString('smtp','replyto','') then
  valtozas := true;
 if Edit4.Text <> cfg.ReadString('smtp','host','') then
  valtozas := true;
 if Edit5.Text <> cfg.ReadString('smtp','userid','') then
  valtozas := true;
 if CheckBox4.Checked <> cfg.ReadBool('smtp','delete',true) then
  valtozas := true;
 if Checkbox5.Checked <> cfg.ReadBool('smtp','save',true) then
  valtozas := true;
 if RadioGroup2.ItemIndex <> cfg.ReadInteger('smtp','format',0) then
  valtozas := true;

 if not valtozas then
  Exit;

 i := Application.MessageBox('Mentsem a megváltozott beállításokat?','POPper',mb_YesNo + mb_IconQuestion);
 if i = idNo then
  Exit;

 cfg.WriteString('smtp','name',Edit1.Text);
 cfg.WriteString('smtp','email',Edit2.Text);
 cfg.WriteString('smtp','replyto',Edit3.Text);
 cfg.WriteString('smtp','host',Edit4.Text);
 cfg.WriteString('smtp','userid',Edit5.Text);
 cfg.WriteBool('smtp','delete',CheckBox4.Checked);
 cfg.WriteBool('smtp','save',CheckBox5.Checked);
 cfg.WriteInteger('smtp','format',RadioGroup2.ItemIndex);
end;

procedure TfrmMainForm.FormResize(Sender: TObject);
begin
 Groupbox1.Left := (tabLevelkuldesibeallitasok.ClientWidth - Groupbox1.Width) div 2;
 Groupbox1.Top := (tabLevelkuldesibeallitasok.ClientHeight - GroupBox1.Height) div 2;

 Groupbox3.Left := (tabLevelletoltesibeallitasok.ClientWidth - Groupbox3.Width) div 2;
 Groupbox3.Top := (tabLevelletoltesibeallitasok.ClientHeight - Groupbox3.Height) div 2;

 Panel4.Left := (tabInternet.ClientWidth - Panel4.Width) div 2;
 Panel4.Top := (tabInternet.ClientHeight - Panel4.Height) div 2;

 Panel5.Left := (tabCimek.ClientWidth - Panel5.Width) div 2;
 Panel5.Top := (tabCimek.ClientHeight - Panel5.Height) div 2;

 Panel6.Left := (tabUjlevel.ClientWidth - Panel6.Width) div 2;
 Panel6.Top := (tabUjlevel.ClientHeight - Panel6.Height) div 2;

 Panel7.Top := (tabAltalanosbeallitasok.ClientHeight - Panel7.Height) div 2;
 Panel7.Left := (tabAltalanosbeallitasok.ClientWidth - Panel7.Width) div 2;
end;

procedure TfrmMainForm.tabBejovolevelekShow(Sender: TObject);
var i: integer;
    a: TListItem;
begin
 ListView1.OnChange := nil;
 ListView1.Items.Clear;

 for i := 1 to popBejovo.Count do
 begin
  a := ListView1.Items.Add;

  a.ImageIndex := ImageList1.Count;

  if boolean(popBejovo.GetIndex(i,sitRead)) = false then
   a.SubItems.Add(chr(149))
  else
   a.SubItems.Add(' ');

  if trim(string(popBejovo.GetIndex(i,sitAttach))) <> '' then
   a.SubItems.Add(chr(149))
  else
   a.SubItems.Add(' ');

  a.SubItems.Add(string(popBejovo.GetIndex(i,sitSender)));
  a.SubItems.Add(string(popBejovo.GetIndex(i,sitSubject)));
  a.SubItems.Add(IntToStr(integer(popBejovo.GetIndex(i,sitSize))));
  a.SubItems.Add(DateTimeToStr(TDateTime(popBejovo.GetIndex(i,sitDate))));
  a.SubItems.Add(IntToStr(i));
 end;

 if (ListView1.Items.Count = 0)and(WebBrowser1.LocationURL <> 'about:blank') then
  WebBrowser1.Navigate('about:blank');

 ListView1.OnChange := ListViewChange;
end;

procedure TfrmMainForm.ListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
 Timer1.Enabled := false;
 Timer1.Tag := (Sender as TListView).Tag;
 LastItem := Item;
 Timer1.Enabled := true;
end;

procedure TfrmMainForm.tabElkuldendoShow(Sender: TObject);
var i: integer;
    a: TListItem;
begin
 ListView2.OnChange := nil;
 ListView2.Items.Clear;

 for i := 1 to popKuldendo.Count do
 begin
  a := ListView2.Items.Add;
  if trim(string(popKuldendo.GetIndex(i,sit2Attach))) <> '' then
   a.SubItems.Add(chr(149))
  else
   a.SubItems.Add(' ');

  a.SubItems.Add(string(popKuldendo.GetIndex(i,sit2To)));
  a.SubItems.Add(string(popKuldendo.GetIndex(i,sit2CC)));
  a.SubItems.Add(string(popKuldendo.GetIndex(i,sit2BCC)));
  a.SubItems.Add(string(popKuldendo.GetIndex(i,sit2Subject)));
  a.SubItems.Add(IntToStr(i));
 end;

 if (ListView2.Items.Count = 0)and(WebBrowser2.LocationURL <> 'about:blank') then
  WebBrowser2.Navigate('about:blank');
 ListView2.OnChange := ListViewChange; 
end;

procedure TfrmMainForm.FormShow(Sender: TObject);
begin
 if FirstShow then
 begin
  FirstShow := false;
   WebBrowser1.Navigate('about:blank');
   WebBrowser2.Navigate('about:blank');
   WebBrowser3.Navigate('about:blank');
 end;
end;

procedure TfrmMainForm.tabAltalanosbeallitasokShow(Sender: TObject);
begin
 BitBtnWithColor1.Color := cfg.ReadInteger('colors','text',0);
 BitBtnWithColor2.Color := cfg.ReadInteger('colors','bgcolor',16777215);
 SpeedButton6.Font := Memo1.Font;
 cbxLevelekellenorzese.Checked := cfg.ReadBool('levelellenorzes','ellenorzes',false);
// txtLevelekellenorzese.Text := cfg.ReadString('levelellenorzes','intervallum','10');
end;

procedure TfrmMainForm.BitBtnWithColor1Click(Sender: TObject);
begin
 ColorDialog1.Color := BitBtnWithColor1.Color;
 if ColorDialog1.Execute then
  BitBtnWithColor1.Color := ColorDialog1.Color;
end;

procedure TfrmMainForm.BitBtnWithColor2Click(Sender: TObject);
begin
 ColorDialog1.Color := BitBtnWithColor2.Color;
 if ColorDialog1.Execute then
  BitBtnWithColor2.Color := COlorDialog1.Color;
end;

procedure TfrmMainForm.tabAltalanosbeallitasokHide(Sender: TObject);
var valtozas: boolean;
    i: integer;
begin
 valtozas := false;

 if BitBtnWithColor1.Color <> cfg.ReadInteger('colors','text',0) then
  valtozas := true;
 if BitBtnWithColor2.Color <> cfg.ReadInteger('colors','bgcolor',16777216) then
  valtozas := true;
 if cbxLevelekellenorzese.Checked <> cfg.ReadBool('levelellenorzes','ellenorzes',false) then
  valtozas := true;
// if txtLevelekellenorzese.Text <> cfg.ReadString('levelellenorzes','intervallum','10') then
//  valtozas := true;

 if not valtozas then
  Exit;

 i := Application.MessageBox('Mentsem a megváltozott beállításokat?','POPper',mb_YesNo + mb_IconQuestion);

 if i = idNo then
  Exit;

 cfg.WriteInteger('colors','text',BitBtnWithColor1.Color);
 cfg.WriteInteger('colors','bgcolor',BitBtnWithColor2.Color);
 cfg.WriteBool('levelellenorzes','ellenorzes',cbxLevelekellenorzese.Checked);
// cfg.WriteString('levelellenorzes','intervallum',txtLevelekellenorzese.Text);
end;

procedure TfrmMainForm.ListViewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
 if Button <> mbRight then
  Exit;

 RightClickPos.ListViewNum := (Sender as TListView).Tag;

 for i := 0 to (Sender as TListView).Items.Count-1 do
  if (Sender as TListView).Items.Item[i].Selected then
   RightClickPos.ItemIndex := i;
end;

procedure TfrmMainForm.mnuDeleteClick(Sender: TObject);
var i: integer;
    s: string;
begin
 s := 'Tényleg törlöd a következõ ';

 case RightClickPos.ListViewNum of
   1: s := s + 'feladójú levelet: "' + string(popBejovo.GetIndex(RightClickPos.ItemIndex+1,sitSender));
   2: s := s + 'címzettû levelet: ' + string(popKuldendo.GetIndex(RightClickPos.ItemIndex+1,sit2To));
   3: s := s + 'címzettû levelet: ' + string(popElkuldott.GetIndex(RightClickPos.ItemIndex+1,sit2To));
 end;

 s := s + '"?';

 i := Application.MessageBox(PChar(s),'POPper',mb_YesNo + mb_IconQuestion);
 if i = idNo then
  Exit;

 if RightClickPos.ListViewNum = 1 then
  popBejovo.Delete(RightClickPos.ItemIndex+1)
 else if RightClickPos.ListViewNum = 2 then
  popKuldendo.Delete(RightClickPos.ItemIndex+1)
 else if RightClickPos.ListViewNum = 3 then
  popElkuldott.Delete(RightClickPos.ItemIndex+1);

 tabBejovolevelekShow(Sender);
 tabElkuldendoShow(Sender);
 tabKuldottShow(Sender);
end;

procedure TfrmMainForm.tabKuldottShow(Sender: TObject);
var i: integer;
    a: TListItem;
begin
 ListView3.OnChange := nil;
 ListView3.Items.Clear;

 for i := 1 to popElkuldott.Count do
 begin
  a := ListView3.Items.Add;
  if trim(string(popElkuldott.GetIndex(i,sit2Attach))) <> '' then
   a.SubItems.Add(chr(149))
  else
   a.SubItems.Add(' ');

  a.SubItems.Add(string(popElkuldott.GetIndex(i,sit2To)));
  a.SubItems.Add(string(popElkuldott.GetIndex(i,sit2CC)));
  a.SubItems.Add(string(popElkuldott.GetIndex(i,sit2BCC)));
  a.SubItems.Add(string(popElkuldott.GetIndex(i,sit2Subject)));
  a.SubItems.Add(IntToStr(i));
 end;

 if (ListView3.Items.Count = 0)and(WebBrowser3.LocationURL <> 'about:blank') then
  WebBrowser3.Navigate('about:blank');
 ListView3.OnChange := ListViewChange;  
end;

procedure TfrmMainForm.SpeedButton2Click(Sender: TObject);
var i: integer;
begin
 if not EditingAlreadyWrittenMail then
 begin
  i :=  Application.MessageBox('Tényleg törlöd az épp írt levelet?','POPper',mb_YesNo + mb_IconQuestion);
  if i = idNo then
   Exit;
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  ListBox1.Items.Clear;
  Memo1.Lines.Clear;
  Edit6.SetFocus;
 end else
 if EditingAlreadyWrittenMail then
 begin
  EditingAlreadyWrittenMail := false;
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  ListBox1.Items.Clear;
  Memo1.Lines.Clear;
  TreeView1.Items[2].Selected := true;
 end;
end;

procedure TfrmMainForm.SpeedButton1Click(Sender: TObject);
begin
 if not EditingAlreadyWrittenMail then
 begin
  popKuldendo.AddNew(Edit6.Text,
                     Edit7.Text,
                     Edit8.Text,
                     Edit9.Text,
                     ListBox1.Items.Text,
                     Memo1.Lines.Text);
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  ListBox1.Items.Clear;
  Memo1.Lines.Clear;
  TreeView1.Items.Item[2].Selected := true;
  ListView2.Items.Item[ListView2.Items.Count-1].Selected := true;
 end else
 if EditingAlreadyWrittenMail then
 begin
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2To,Edit6.Text);
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2CC,Edit7.Text);
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2BCC,Edit8.Text);
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2Subject,Edit9.Text);
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2Attach,ListBox1.Items.Text);
  popKuldendo.SetIndex(EditingAlreadyWrittenMailIndex,sit2Body,Memo1.Lines.Text);
  Edit6.Text := '';
  Edit7.Text := '';
  Edit8.Text := '';
  Edit9.Text := '';
  ListBox1.Items.Clear;
  Memo1.Lines.Clear;
  TreeView1.Items.Item[2].Selected := true;
  ListView2.Items.Item[EditingAlreadyWrittenMailIndex-1].Selected := true;
  EditingAlreadyWrittenMail := false;
 end;
end;

procedure TfrmMainForm.Timer1Timer(Sender: TObject);
var i,k: integer;
    tsl, tsl2: TStringList;
begin
 Timer1.Enabled := false;
 tsl := TStringList.Create;
 i := StrToInt(LastItem.SubItems[LastItem.SubItems.Count-1]);

 if Timer1.Tag = 1 then
 begin
  tsl.Text := '';
  tsl.Add('<html>');
  tsl.Add('<body bgcolor="#' + IntToHex(cfg.ReadInteger('colors','bgcolor',16777215),6) + '" text="' + IntToHex(cfg.ReadInteger('colors','text',0),6) + '">');
  tsl.Add('<h2>Beérkezett e-mail</h2>');
  tsl.Add('<table cellspacing="0" cellpadding="0">');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Levél feladója:</td>');
  tsl.Add('  <td width="25"></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popBejovo.GetIndex(i,sitSender))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Levél mérete:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + IntToStr(integer(popBejovo.GetIndex(i,sitSize))) + '</td>');
  tsl.Add(' </tr>');

  if trim(string(popBejovo.GetIndex(i,sitAttach))) <> '' then
  begin
   tsl.Add(' <tr>');
   tsl.Add('  <td align="right">Csatolt fileok:</td>');
   tsl.Add('  <td></td>');
   tsl.Add('  <td>');
   tsl2 := TStringList.Create;
   tsl2.Text := string(popBejovo.GetIndex(i,sitAttach));
   for k := 0 to tsl2.Count-1 do
    tsl.Add(tsl2[k] + '<a href="pop|save|' + IntToStr(i) + '|' + IntToStr(k) + '"><img src="res://popper.exe/save_jpg" border="0" alt="file mentése"></a>');
   tsl2.Free;
   tsl.Add('  </td>');
   tsl.Add(' </tr>');
  end;

  tsl.Add(' <tr height="10">');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Dátuma:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + DateTimeToStr(TDateTime(popBejovo.GetIndex(i,sitDate))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Levél tárgya:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + popBejovo.GetIndex(i,sitSubject) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add('</table>');
  tsl.Add('<br><hr><br>');
  tsl2 := TStringList.Create;
  tsl2.Text := string(popBejovo.GetIndex(i,sitBody));

  for k := 0 to tsl2.Count-1 do
   tsl.Add(tsl2[k] + '<br>');

  tsl2.Free;

  tsl.Add('<br><hr><br>');
  tsl.Add('<a href="pop|reply|' + IntToStr(i) + '">Válasz erre</a>&nbsp;');
  tsl.Add('<a href="pop|fwd|' + IntToStr(i) + '">Továbbküldés</a>');

  tsl.Add('</body>');
  tsl.Add('</html>');

  tsl.SaveToFile(ExtractFilePath(ParamStr(0)) + 'temp.htm');
  tsl.Free;

  WebBrowser1.Navigate(ExtractFilePath(ParamStr(0)) + 'temp.htm');
 end
 else
 if Timer1.Tag = 2 then
 begin
  tsl.Add('<html>');
  tsl.Add('<body bgcolor="#' + IntToHex(cfg.ReadInteger('colors','bgcolor',16777215),6) + '" text="' + IntToHex(cfg.ReadInteger('colors','text',0),6) + '">');
  tsl.Add('<h2>Elküldendõ e-mail</h2>');
  tsl.Add('<table cellspacing="0" cellpadding="0">');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Levél címzettje:</td>');
  tsl.Add('  <td width="25"></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popKuldendo.GetIndex(i,sit2To))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Másolatot kap:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popKuldendo.GetIndex(i,sit2CC))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Rejtett másolatot kap:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popKuldendo.GetIndex(i,sit2BCC))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr height="10">');
  tsl.Add(' </tr>');

  if trim(string(popKuldendo.GetIndex(i,sit2Attach))) <> '' then
  begin
   tsl.Add(' <tr>');
   tsl.Add('  <td align="right">Csatolt fileok:</td>');
   tsl.Add('  <td></td>');
   tsl.Add('  <td>' + string(popKuldendo.GetIndex(i,sit2Attach)) + '</td>');
   tsl.Add(' </tr>');
  end;

  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Tárgy</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + string(popKuldendo.GetIndex(i,sit2Subject)) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add('</table>');
  tsl.Add('<br><hr><br>');

  tsl2 := TStringList.Create;
  tsl2.Text := string(popKuldendo.GetIndex(i,sit2Body));
  for k := 0 to tsl2.Count-1 do
   tsl.Add(tsl2[k] + '<br>');

  tsl2.Free;

  tsl.Add('<br><hr><br>');
  tsl.Add('<a href="pop|fwd|' + IntToStr(i) + '">Továbbküldés</a>&nbsp;');
  tsl.Add('<a href="pop|edit|' + IntToStr(i) + '">Levél szerkesztése</a>');
  tsl.Add('</body>');
  tsl.Add('</html>');

  tsl.SaveToFile(ExtractFilePath(ParamStr(0)) + 'temp.htm');
  tsl.Free;

  WebBrowser2.Navigate(ExtractFilePath(ParamStr(0)) + 'temp.htm');
 end
 else
 if Timer1.Tag = 3 then
 begin
  tsl.Add('<html>');
  tsl.Add('<body bgcolor="#' + IntToHex(cfg.ReadInteger('colors','bgcolor',16777215),6) + '" text="' + IntToHex(cfg.ReadInteger('colors','text',0),6) + '">');
  tsl.Add('<h2>Elküldött e-mail</h2>');
  tsl.Add('<table cellspacing="0" cellpadding="0">');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Levél címzettje:</td>');
  tsl.Add('  <td width="25"></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popElkuldott.GetIndex(i,sit2To))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Másolatot kapott:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popElkuldott.GetIndex(i,sit2CC))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Rejtett másolatot kapott:</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + ConvertEmailAddress(string(popElkuldott.GetIndex(i,sit2BCC))) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add(' <tr height="10">');
  tsl.Add(' </tr>');

  if trim(string(popElkuldott.GetIndex(i,sit2Attach))) <> '' then
  begin
   tsl.Add(' <tr>');
   tsl.Add('  <td align="right">Csatolt fileok:</td>');
   tsl.Add('  <td></td>');
   tsl.Add('  <td>' + string(popElkuldott.GetIndex(i,sit2Attach)) + '</td>');
   tsl.Add(' </tr>');
  end;

  tsl.Add(' <tr>');
  tsl.Add('  <td align="right">Tárgy</td>');
  tsl.Add('  <td></td>');
  tsl.Add('  <td>' + string(popElkuldott.GetIndex(i,sit2Subject)) + '</td>');
  tsl.Add(' </tr>');
  tsl.Add('</table>');
  tsl.Add('<br><hr><br>');

  tsl2 := TStringList.Create;
  tsl2.Text := string(popElkuldott.GetIndex(i,sit2Body));
  for k := 0 to tsl2.Count-1 do
   tsl.Add(tsl2[k] + '<br>');

  tsl2.Free;

  tsl.Add('<br><hr><br>');
  tsl.Add('<a href="pop|fwd|' + IntToStr(i) + '">Továbbküldés</a>');
  tsl.Add('</body>');
  tsl.Add('</html>');

  tsl.SaveToFile(ExtractFilePath(ParamStr(0)) + 'temp.htm');
  tsl.Free;

  WebBrowser3.Navigate(ExtractFilePath(ParamStr(0)) + 'temp.htm');
 end;
end;

procedure TfrmMainForm.WebBrowserBeforeNavigate2(Sender: TObject; const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData, Headers: OleVariant; var Cancel: WordBool);
var s: string;
    i,k: integer;
    filein, fileout: TFileStream;
    tsl: TStringList;
begin
 s := URL;
 Cancel := false;

 delete(s,1,length(ExtractFilePath(ParamStr(0))));
 if pos('pop',s) <> 1 then
  Exit;

 delete(s,1,4);

 if pos('reply',s) = 1 then
 begin
  i := StrToInt(copy(s,pos('|',s)+1,length(s)));
  if (Sender as TWebBrowser).Tag = 1 then
  begin
   Cancel := true;
   Edit6.Text := popBejovo.GetIndex(i,sitSender);
   Edit9.Text := 'Re: ' + popBejovo.GetIndex(i,sitSubject);
   ListBox1.Items.Text := popBejovo.GetIndex(i,sitAttach);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + Edit6.Text + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popBejovo.GetIndex(i,sitBody) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end else
  if (Sender as TWebBrowser).Tag = 2 then
  begin
   Cancel := true;
   Edit6.Text := popKuldendo.GetIndex(i,sit2To);
   Edit9.Text := 'Re: ' + popKuldendo.GetIndex(i,sit2Subject);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + Edit6.Text + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popKuldendo.GetIndex(i,sit2Body) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end else
  if (Sender as TWebBrowser).Tag = 3 then
  begin
   Cancel := true;
   Edit6.Text := popElkuldott.GetIndex(i,sit2To);
   Edit9.Text := 'Re: ' + popElkuldott.GetIndex(i,sit2Subject);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + Edit6.Text + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popElkuldott.GetIndex(i,sit2Body) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end;
 end else
 if pos('fwd',s) = 1 then
 begin
  i := StrToInt(copy(s,pos('|',s)+1,length(s)));
  if (Sender as TWebBrowser).Tag = 1 then
  begin
   Cancel := true;
   Edit9.Text := 'Fwd: ' + popBejovo.GetIndex(i,sitSubject);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + popBejovo.GetIndex(i,sitSender) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popBejovo.GetIndex(i,sitBody) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end else
  if (Sender as TWebBrowser).Tag = 2 then
  begin
   Cancel := true;
   Edit9.Text := 'Fwd: ' + popKuldendo.GetIndex(i,sit2Subject);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + cfg.ReadString('smtp','email','') + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popKuldendo.GetIndex(i,sit2Body) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end else
  if (Sender as TWebBrowser).Tag = 3 then
  begin
   Cancel := true;
   Edit9.Text := 'Fwd: ' + popElkuldott.GetIndex(i,sit2Subject);
   Memo1.Lines.Text := '--------------------------------' + #13#10 + 'Idézet: ' + cfg.ReadString('smtp','email','') + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + popElkuldott.GetIndex(i,sit2Body) + #13#10#13#10;
   Memo1.Lines.Text := Memo1.Lines.Text + 'Idézet vége' + #13#10 + '--------------------------------';
   TreeView1.Items.Item[9].Selected := true;
  end;
 end else
 if pos('save',s) = 1 then
 begin
  delete(s,1,5);
  i := StrToInt(copy(s,1,pos('|',s)-1));
  delete(s,1,pos('|',s));
  k := StrToInt(s);
  tsl := TStringList.Create;
  tsl.Text := popBejovo.GetIndex(i,sitAttach);
  SaveDialog1.FileName := 'c:\' + tsl[k];
  SaveDialog1.Filter := '*' + ExtractFileExt(tsl[k]) + '|*' + ExtractFileExt(tsl[k]);
  SaveDialog1.DefaultExt := copy(ExtractFileExt(tsl[k]),2,length(tsl[k]));
  if not SaveDialog1.Execute then
  begin
   Cancel := true;
   tsl.Free;
   Exit;
  end;
  filein := TFileStream.Create(ExtractFilePath(ParamStr(0)) + 'AttachFiles\' + tsl[k],fmOpenRead + fmShareDenyWrite);
  fileout := TFileStream.Create(SaveDialog1.FileName,fmCreate);
  fileout.CopyFrom(filein,filein.Size);
  fileout.Free;
  filein.Free;
  tsl.Free;
  Cancel := true;
 end else
 if pos('edit',s) = 1 then
 begin
  i := StrToInt(copy(s,pos('|',s)+1,length(s)));
  EditingAlreadyWrittenMail := true;
  EditingAlreadyWrittenMailIndex := i;
  Edit6.Text := popKuldendo.GetIndex(i,sit2To);
  Edit7.Text := popKuldendo.GetIndex(i,sit2CC);
  Edit8.Text := popKuldendo.GetIndex(i,sit2BCC);
  Edit9.Text := popKuldendo.GetIndex(i,sit2Subject);
  ListBox1.Items.Text := popKuldendo.GetIndex(i,sit2Attach);
  Memo1.Lines.Text := popKuldendo.GetIndex(i,sit2Body);
  TreeView1.Items[9].Selected := true;
 end;
end;

procedure TfrmMainForm.ListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
 ColumnToSort := Column.Index;
 (Sender as TListView).AlphaSort;
end;

procedure TfrmMainForm.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
begin
 if ColumnToSort = 0 then
  Compare := CompareText(Item1.Caption,Item2.Caption)
 else begin
  ix := ColumnToSort - 1;
  Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);
 end;
end;

procedure TfrmMainForm.TreeView1Collapsing(Sender: TObject; Node: TTreeNode; var AllowCollapse: Boolean);
begin
 AllowCollapse := false;
end;

procedure TfrmMainForm.SpeedButton3Click(Sender: TObject);
var i: integer;
begin
 EnableInternetButtons(false);

// tSMTP.Execute;
 if frmMainForm.popKuldendo.Count = 0 then
  Abort;

 frmMainForm.AddToList('SMTP beállítások elvégzése');
 frmMainForm.nmSMTP1.Host := frmMainForm.cfg.ReadString('smtp','host','');
 frmMainForm.nmSMTP1.UserID := frmMainForm.cfg.ReadString('smtp','userid','');

 if frmMainForm.cfg.ReadInteger('smtp','format',0) = 0 then
  frmMainForm.nmSMTP1.SubType := mtPlain
 else
  frmMainForm.nmSMTP1.SubType := mtHtml;

 frmMainForm.nmSMTP1.EncodeType := uuMime;
 frmMainForm.ProgressBar1.Max := frmMainForm.popKuldendo.Count;

 frmMainForm.AddToList('Csatlakozás...');
 for i := 1 to frmMainForm.popKuldendo.Count do
 begin
  try
   frmMainForm.nmSMTP1.Connect;Application.ProcessMessages;

   frmMainForm.nmSMTP1.PostMessage.FromAddress := frmMainForm.cfg.ReadString('smtp','email','');Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.FromName := frmMainForm.cfg.ReadString('smtp','name','');Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.ReplyTo := frmMainForm.cfg.ReadString('smtp','replyto','');Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.LocalProgram := 'Putra POPper';Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.Date := DateTimeToStr(Now);Application.ProcessMessages;

   frmMainForm.nmSMTP1.PostMessage.ToAddress.Text := frmMainForm.popKuldendo.GetIndex(i,sit2To);Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.ToCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2CC);Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.ToBlindCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2BCC);Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.Subject := frmMainForm.popKuldendo.GetIndex(i,sit2Subject);Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.Attachments.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Attach);Application.ProcessMessages;
   frmMainForm.nmSMTP1.PostMessage.Body.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Body);Application.ProcessMessages;

   frmMainForm.nmSMTP1.SendMail;Application.ProcessMessages;
   frmMainForm.AddToList('Levél #' + IntToStr(i) + '/' + IntToStr(frmMainForm.popKuldendo.Count) + ' elküldve');Application.ProcessMessages;

   if frmMainForm.cfg.ReadBool('smtp','save',true) = true then
    frmMainForm.popElkuldott.AddNew(frmMainForm.popKuldendo.GetIndex(i,sit2To),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2CC),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2BCC),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Subject),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Attach),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Body));Application.ProcessMessages;

  finally
   frmMainForm.nmSMTP1.Disconnect;Application.ProcessMessages;
   EnableInternetButtons(true);
  end;
 end;
// frmMainForm.AddToList('Lecsatlakozva az SMTP szerverrõl');

 if frmMainForm.cfg.ReadBool('smtp','delete',true) = true then
  while frmMainForm.popKuldendo.Count > 0 do
   frmMainForm.popKuldendo.Delete(1);

// SendMessage(frmMainForm.Handle,wm_User+1,0,0);

 // -----ENDOFPASTEDCODE

 EnableInternetButtons(true);
end;

procedure TfrmMainForm.RestoreWIndowBecauseOfAMessage(var Msg: TMessage);
begin
 Application.Restore;
end;

procedure TfrmMainForm.EnableInternetButtons(enable: boolean);
begin
 SpeedButton3.Enabled := enable;
 SpeedButton4.Enabled := enable;
 SpeedButton5.Enabled := enable;
end;

procedure TfrmMainForm.SpeedButton4Click(Sender: TObject);
begin
 EnableInternetButtons(false);

 tPOP3.Execute;

// EnableInternetButtons(true);
end;

procedure TfrmMainForm.SpeedButton5Click(Sender: TObject);
begin
 EnableInternetButtons(false);

 tSMTP.Execute;
 StartPOP := true;

 EnableInternetButtons(true);
end;

procedure TfrmMainForm.tabInternetShow(Sender: TObject);
begin
 FormResize(Sender);
end;

procedure TfrmMainForm.AddToList(s: string);
begin
 ListBox2.Items.Add(s);
 ListBox2.ItemIndex := ListBox2.Items.Count - 1;
 Application.ProcessMessages;
end;

{ TThreadSMTP }

procedure TThreadSMTP.Execute;
var i: integer;
begin
 if frmMainForm.popKuldendo.Count = 0 then
  Abort;

 frmMainForm.AddToList('SMTP beállítások elvégzése');
 frmMainForm.nmSMTP1.Host := frmMainForm.cfg.ReadString('smtp','host','');
 frmMainForm.nmSMTP1.UserID := frmMainForm.cfg.ReadString('smt','userid','');

 if frmMainForm.cfg.ReadInteger('smtp','format',0) = 0 then
  frmMainForm.nmSMTP1.SubType := mtPlain
 else
  frmMainForm.nmSMTP1.SubType := mtHtml;

 frmMainForm.nmSMTP1.EncodeType := uuMime;
 frmMainForm.ProgressBar1.Max := frmMainForm.popKuldendo.Count;

 frmMainForm.AddToList('Csatlakozás...');
 for i := 1 to frmMainForm.popKuldendo.Count do
 begin
  try
   frmMainForm.nmSMTP1.Connect;

   frmMainForm.nmSMTP1.PostMessage.FromAddress := frmMainForm.cfg.ReadString('smtp','email','');
   frmMainForm.nmSMTP1.PostMessage.FromName := frmMainForm.cfg.ReadString('smtp','name','');
   frmMainForm.nmSMTP1.PostMessage.ReplyTo := frmMainForm.cfg.ReadString('smtp','replyto','');
   frmMainForm.nmSMTP1.PostMessage.LocalProgram := 'Putra POPper';
   frmMainForm.nmSMTP1.PostMessage.Date := DateTimeToStr(Now);

   frmMainForm.nmSMTP1.PostMessage.ToAddress.Text := frmMainForm.popKuldendo.GetIndex(i,sit2To);
   frmMainForm.nmSMTP1.PostMessage.ToCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2CC);
   frmMainForm.nmSMTP1.PostMessage.ToBlindCarbonCopy.Text := frmMainForm.popKuldendo.GetIndex(i,sit2BCC);
   frmMainForm.nmSMTP1.PostMessage.Subject := frmMainForm.popKuldendo.GetIndex(i,sit2Subject);
   frmMainForm.nmSMTP1.PostMessage.Attachments.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Attach);
   frmMainForm.nmSMTP1.PostMessage.Body.Text := frmMainForm.popKuldendo.GetIndex(i,sit2Body);

   frmMainForm.nmSMTP1.SendMail;
   frmMainForm.AddToList('Levél #' + IntToStr(i) + '/' + IntToStr(frmMainForm.popKuldendo.Count) + ' elküldve');

   if frmMainForm.cfg.ReadBool('smtp','save',true) = true then
    frmMainForm.popElkuldott.AddNew(frmMainForm.popKuldendo.GetIndex(i,sit2To),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2CC),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2BCC),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Subject),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Attach),
                                    frmMainForm.popKuldendo.GetIndex(i,sit2Body));

  finally
   frmMainForm.nmSMTP1.Disconnect;
  end;
 end;
// frmMainForm.AddToList('Lecsatlakozva az SMTP szerverrõl');

 if frmMainForm.cfg.ReadBool('smtp','delete',true) = true then
  while frmMainForm.popKuldendo.Count > 0 do
   frmMainForm.popKuldendo.Delete(1);

 SendMessage(frmMainForm.Handle,wm_User+1,0,0);
end;

{ TThreadPOP3 }

procedure TThreadPOP3.Execute;
var i: integer;
    s1: string;
    s2: string;
begin
 frmMainForm.nmPOP31.Host := frmMainForm.cfg.ReadString('pop3','host','');
 frmMainForm.nmPOP31.UserID := frmMainForm.cfg.ReadString('pop3','userid','');
 frmMainForm.nmPOP31.Password := frmMainForm.cfg.ReadString('pop3','password','');
 frmMainForm.AddToList('POP3 beállítások elvégezve');
 frmMainForm.AddToList('Csatlakozás a POP3 szerverre');

 try
  frmMainForm.nmPOP31.Connect;

  if frmMainForm.nmPOP31.MailCount = 0 then
  begin
   frmMainForm.AddToList('Nincs új levél!');
   frmMainForm.nmPOP31.Disconnect;
   frmMainForm.AddToList('Lecsatlakozva a POP3 szerverrõl');
   Exit;
  end;

  frmMainForm.ShowBalloonTips(frmMainForm.nmPOP31.MailCount);

  frmMainForm.nmPOP31.AttachFilePath := ExtractFilePath(ParamStr(0)) + 'AttachFiles';
  frmMainForm.ProgressBar1.Max := frmMainForm.nmPOP31.MailCount;

  for i := 1 to frmMainForm.nmPOP31.MailCount do
  begin
   frmMainForm.nmPOP31.GetSummary(i);
   frmMainForm.nmPOP31.GetMailMessage(i);
   s1 := TargyAtalakitas(frmMainForm.nmPOP31.MailMessage.From,false);
   s2 := TargyAtalakitas(frmMainForm.nmPOP31.MailMessage.Subject,false);

   while pos(#13#10,s1) > 0 do
    delete(s1,pos(#13#10,s1),2);
   while pos(#13#10,s2) > 0 do
    delete(s2,pos(#13#10,s2),2);

 try
   frmMainForm.popBejovo.AddNew(s1,
                    false,
                    MailSizes[i],
                    GetDateFromSummary(frmMainForm.nmPOP31.Summary.Header.Text),
                    s2,
                    frmMainForm.nmPOP31.MailMessage.Attachments.Text,
                    EgyenlosegTorles(trim(frmMainForm.nmPOP31.MailMessage.Body.Text)));
   frmMainForm.AddToList('Levél #' + IntToStr(i) + '/' + IntToStr(frmMainForm.nmPOP31.MailCount) + ' letöltve');
 except
   frmMainForm.AddToList('Hiba a ' + IntToStr(i) + ' levél letöltése során');
 end;
   frmMainForm.ProgressBar1.Position := i;

   if frmMainForm.cfg.ReadBool('pop3','delete',false) = true then
   begin
    frmMainForm.nmPOP31.DeleteMailMessage(i);
    frmMainForm.AddToList('Levél #' + IntToStr(i) + ' törölve a szerverrõl');
   end;
  end;
 finally
  frmMainForm.nmPOP31.Disconnect;
  frmMainForm.AddToList('Lecsatlakozva a POP3 szerverrõl');
  frmMainForm.EnableInternetButtons(true);
 end;
end;

procedure TfrmMainForm.NMPOP31List(Msg, Size: Integer);
begin
 MailSizes[Msg] := Size;
end;

procedure TfrmMainForm.StartMailDownload(var Msg: TMessage);
begin
 if StartPOP then
  tPOP3.Execute;
 StartPOP := false; 
end;

procedure TfrmMainForm.ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key = vk_Insert then
  if OpenDialog1.Execute then
   ListBox1.Items.Add(OpenDialog1.FileName);

 if Key = vk_Delete then
  if ListBox1.ItemIndex > -1 then
   if Application.MessageBox(PChar('Törlöd a következõ csatolt filet: ' + ListBox1.Items[ListBox1.ItemIndex] + '?'),'Putra POPper',mb_YesNo + mb_IconQuestion) = idYes then
    ListBox1.Items.Delete(ListBox1.ItemIndex);
end;

procedure TfrmMainForm.ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var lXPoint, lYPoint, lIndex: integer;
begin
 lXPoint := X;
 lYPoint := Y;

 lIndex := SendMessageA(ListBox1.Handle,lb_ItemFromPoint,0,(lYPoint * 65536)+lXPoint);
 if (lIndex >= 0) and (lIndex < ListBox1.Items.Count) then
  ListBox1.Hint := ListBox1.Items[lIndex]
 else
 begin
  ListBox1.Hint := '';
  Application.HideHint;
 end;
end;

procedure TfrmMainForm.LoadVersionInfo;
var VInfoSize, DetSize: dword;
    pVInfo, pDetail: pointer;
    MajorVer, MinorVer: integer;
    FileDescription: string;
    pLangInfo: ^TLangInfoBuffer;
    strLangID: string;
begin
 MajorVer := 0;
 MinorVer := 0;

 VInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),DetSize);
 if VInfoSize > 0 then
 begin
  GetMem(pVInfo,VInfoSize);
  try
   GetFileVersionInfo(PChar(ParamStr(0)),0,VInfoSize,pVInfo);
   VerQueryValue(PVInfo,'\',pDetail,DetSize);
   with TVSFixedFileInfo(pDetail^) do
   begin
    MajorVer := HiWord(dwFileVersionMS);
    MinorVer := LoWord(dwFileVersionMS);
    VerQueryValue(pVInfo,'\VarFileInfo\Translation',pointer(pLangInfo),DetSize);
    strLangID := IntToHex(smallint(pLangInfo^[1]),4) + IntToHex(smallint(pLangInfo^[2]),4);
    strLangID := 'StringFileInfo\' + strLangID;
    VerQueryValue(pVInfo,PChar(strLangID + '\FileDescription'),pDetail,DetSize);
    FileDescription := PChar(pDetail);
   end;
  finally
   FreeMem(PVInfo);
  end;
 end;

 frmMainForm.Caption := 'Putra POPper v' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + #32 + FileDescription;
end;

procedure TfrmMainForm.PopupMenu2Popup(Sender: TObject);
var i: integer;
    tsl: TStringList;
    a: TMenuItem;
begin
 while PopupMenu2.Items.Count > 0 do
  PopupMenu2.Items.Delete(0);

 tsl := TStringList.Create;
 tsl.Text := popCimek.GetItems;

 for i := 0 to tsl.Count-1 do
 begin
  a := TMenuItem.Create(Self);
  a.Caption := tsl[i];
  a.Tag := i+1;
  a.OnClick := PopupMenu2ItemClick;
  PopupMenu2.Items.Add(a);
 end;
end;

procedure TfrmMainForm.PopupMenu2ItemClick(Sender: TObject);
begin
 Edit6.Text := popCimek.GetIndex((Sender as TMenuItem).Tag,sit3Name) + ' <' +
               popCimek.GetIndex((Sender as TMenuItem).Tag,sit3Address) + '>';
end;

procedure TfrmMainForm.tabCimekShow(Sender: TObject);
var i: integer;
    a: TListItem;
begin
 while ListView4.Items.Count > 0 do
  ListView4.Items.Delete(0);

 for i := 1 to popCimek.Count do
 begin
  a := ListView4.Items.Add;
  a.SubItems.Add(popCimek.GetIndex(i,sit3Name));
  a.SubItems.Add(popCimek.GetIndex(i,sit3Address));
 end;
end;

procedure TfrmMainForm.ListView4MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
 if Button <> mbRight then
  Exit;

 LastAddressItem := nil;

 for i := 0 to ListView4.Items.Count-1 do
  if ListView4.Items.Item[i].Selected then
   LastAddressItem := ListView4.Items.Item[i];
end;

procedure TfrmMainForm.nmuCimekUjClick(Sender: TObject);
var frm: TfrmCimlista;
    i: integer;
begin
 frm := TfrmCimlista.Create(frmMainForm);
 frm.Caption := 'Új címzett létrehozása';
 i := frm.ShowModal;

 if i = mrOK then
  popCimek.AddNew(frm.Edit1.Text,frm.Edit2.text);

 tabCimekShow(Sender);
 for i := 0 to ListView4.Items.Count-1 do
  if ListView4.Items.Item[i].SubItems[0] = frm.Edit1.Text then
   ListView4.Items.Item[i].Selected := true;

 frm.Free; 
end;

procedure TfrmMainForm.mnuCimekSzerkesztesClick(Sender: TObject);
var i: integer;
    frm: TfrmCimlista;
    k: integer;
    s: string;
begin
 if LastAddressItem = nil then
  Exit;

 frm := TfrmCimlista.Create(frmMainForm);
 frm.Caption := 'Címzett szerkesztése';
 s := LastAddressItem.SubItems[0];
 k := popCimek.IndexOfItem(s);
 frm.Edit1.Text := popCimek.GetIndex(k,sit3Name);
 frm.Edit2.Text := popCimek.GetIndex(k,sit3Address);
 i := frm.ShowModal;

 if i = mrOK then
 begin
  popCimek.SetIndex(k,sit3Name,frm.Edit1.Text);
  popCimek.SetIndex(k,sit3Address,frm.Edit2.Text);
 end;

 tabCimekShow(Sender);

 for i := 0 to ListView4.Items.Count-1 do
  if ListView4.Items.Item[i].SubItems[0] = frm.Edit1.Text then
   ListView4.Items.Item[i].Selected := true;

 frm.Free;  
end;

procedure TfrmMainForm.mnuCimekTorlesClick(Sender: TObject);
var i: integer;
    k: integer;
    s: string;
begin
 if LastAddressItem = nil then
  Exit;

 i := Application.MessageBox(PChar('Tényleg törlöd a következõt: ' + LastAddressItem.SubItems[0] + '?'),'Putra POPper',mb_YesNo + mb_IconQuestion);

 if i = idNo then
  Exit;

 s := LastAddressItem.SubItems[0];
 k := popCimek.IndexOfItem(s);
 popCimek.DeleteIndex(k);
 tabCimekShow(Sender);
end;

procedure TfrmMainForm.NMPOP31AuthenticationFailed(var Handled: Boolean);
begin
 Application.MessageBox('Nem sikerült bejelentkezni a POP3 szerverre, ellenõrizd a megadott jelszót és felhasználónevet!','Putra POPper',mb_OK + mb_Iconexclamation);
 nmPOP31.Disconnect;
 EnableInternetButtons(true);
 Handled := true;
end;

procedure TfrmMainForm.NMPOP31AuthenticationNeeded(var Handled: Boolean);
begin
 Application.MessageBox('Nem sikerült bejelentkezni a POP3 szerverre, ellenõrizd a megadott jelszót és felhasználónevet!','Putra POPper',mb_OK + mb_IconExclamation);
 nmPOP31.Disconnect;
 EnableInternetButtons(true);
 Handled := true;
end;

procedure TfrmMainForm.NMPOP31ConnectionFailed(Sender: TObject);
begin
 Application.MessageBox('Nem sikerült csatlakozni a POP3 szerverre, az valószínûleg túlterhelt. Próbáld meg késõbb!','Putra POPper',mb_OK + mb_IconExclamation);
 nmPOP31.Disconnect;
 EnableInternetButtons(true);
end;

procedure TfrmMainForm.TitleButton1MouseUp(Sender: TObject);
begin
 frmAbout.Show;
end;

procedure TfrmMainForm.SpeedButton6Click(Sender: TObject);
begin
 FontDialog1.Font := SpeedButton6.Font;
 if FontDialog1.Execute then
 begin
  SpeedButton6.Font := FontDialog1.Font;
  Memo1.Font := FontDialog1.Font;
 end;
end;

procedure TfrmMainForm.NMSMTP1Connect(Sender: TObject);
begin
 AddToList('Csatlakozva az SMTP szerverre');
end;

procedure TfrmMainForm.NMSMTP1Disconnect(Sender: TObject);
begin
 AddToList('Lecsatlakozva az SMTP szerverrõl');
end;

procedure TfrmMainForm.NMPOP31Connect(Sender: TObject);
begin
 AddToList('Csatlakozva a POP3 szerverhez');
end;

procedure TfrmMainForm.NMPOP31Disconnect(Sender: TObject);
begin
 AddToList('Lecsatlakozva a POP3 szerverrõl');
end;

procedure TfrmMainForm.txtLevelekellenorzeseKeyPress(Sender: TObject; var Key: Char);
begin
 if not (Key in ['0'..'9',#8]) then
  Key := #0;
end;

procedure TfrmMainForm.cbxLevelekellenorzeseClick(Sender: TObject);
begin
 Timer2.Enabled := cbxLevelekellenorzese.Checked;
end;

procedure TfrmMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caHide;
end;

procedure TfrmMainForm.IconMessage(var Msg: TMessage);
var Pt: TPoint;
begin
 if Msg.lParam = wm_rButtonDown then
 begin
  GetCursorPos(Pt);
  PopupMenu4.Popup(Pt.x,Pt.y);
 end;
 if Msg.lParam = wm_lButtonDblClk then
  frmMainForm.Show;
end;

procedure TfrmMainForm.mnuQuitClick(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TfrmMainForm.mnuPopNowClick(Sender: TObject);
begin
 tPOP3.Execute;
end;

procedure TfrmMainForm.Timer2Timer(Sender: TObject);
begin
 TimerCount := TimerCount + 1;
 if TimerCount > 600 then
 begin
  if not nmPOP31.Connected then
   tPOP3.Execute;
  TimerCount := 0;
 end;
end;

procedure TfrmMainForm.AddSysTrayIcon;
begin
 IconData.cbSize := SizeOf(IconData);
 IconData.Wnd := AllocateHWnd(IconMessage);
 IconData.uID := 0;
 IconData.uFlags := NIF_ICON or NIF_MESSAGE or NIF_TIP;
 IconData.uCallbackMessage := wm_IconTray;
 IconData.hIcon := Application.Icon.Handle;
 IconData.szTip := 'Putra POPper';
 Shell_NotifyIcon(NIM_ADD, @IconData);
end;

procedure TfrmMainForm.ShowBalloonTips(num: integer);
var
  TipInfo, TipTitle: string;
begin
 IconData.cbSize := SizeOf(IconData);
 IconData.uFlags := NIF_INFO;
 TipInfo := IntToStr(num) + ' új leveled van';
 strPLCopy(IconData.szInfo, TipInfo, SizeOf(IconData.szInfo) - 1);
 IconData.DUMMYUNIONNAME.uTimeout := 3000;
 TipTitle := 'Putra POPper';
 strPLCopy(IconData.szInfoTitle, TipTitle, SizeOf(IconData.szInfoTitle) - 1);
 IconData.dwInfoFlags := NIIF_INFO;
 Shell_NotifyIcon(NIM_MODIFY, @IconData);
 IconData.DUMMYUNIONNAME.uVersion := NOTIFYICON_VERSION;
 Shell_NotifyIcon(NIM_SETVERSION, @IconData);
end;

end.
