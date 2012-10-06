unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, Buttons, ScktComp, AmobaClientList;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lstGamers: TListBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    mnuServerCreate: TMenuItem;
    mnuServerClose: TMenuItem;
    Label2: TLabel;
    txtName: TEdit;
    mnuClientConnect: TMenuItem;
    mnuClientDisconnect: TMenuItem;
    mnuServerKick: TMenuItem;
    ClientSocket1: TClientSocket;
    ServerSocket1: TServerSocket;
    ACL: TAmobaClientList;
    procedure BitBtn1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuServerCreateClick(Sender: TObject);
    procedure mnuServerCloseClick(Sender: TObject);
    procedure mnuServerKickClick(Sender: TObject);
    procedure ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
    procedure mnuClientConnectClick(Sender: TObject);
    procedure mnuClientDisconnectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  IAmTheServer: boolean = false;

const
 ProgramName: string = 'Putra NetÖdölõ';

implementation

{$R *.DFM}

procedure TForm2.BitBtn1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Pt: TPoint;
begin
 GetCursorPos(Pt);
 PopupMenu1.Popup(Pt.X,Pt.Y);
end;

procedure TForm2.BitBtn2Click(Sender: TObject);
var Pt: TPoint;
begin
 GetCursorPos(Pt);
 PopupMenu2.Popup(Pt.x,Pt.y);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
 ServerSocket1.Port := 1467;
 ClientSocket1.Port := 1467;
end;

procedure TForm2.mnuServerCreateClick(Sender: TObject);
begin
 if trim(txtName.Text) = '' then
 begin
  Application.MessageBox('Elõbb válassz magadnak egy nevet!',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 if ServerSocket1.Active then
 begin
  Application.MessageBox('Már fut a szerver!',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 ServerSocket1.Active := true;
 IAmTheServer := true;

 ACL.AddNew(txtName.Text,'0.0.0.0');

 lstGamers.Items.Clear;
 lstGamers.Items.Add(txtName.Text + '(0, 0)');
 lstGamers.Items.Add('[Összesen ' + inttostr(ACL.Count) + ' játékos]');

 BitBtn2.Enabled := false;

 mnuServerCreate.Enabled := false;
 mnuServerClose.Enabled := true;
 mnuServerKick.Enabled := true;
end;

procedure TForm2.mnuServerCloseClick(Sender: TObject);
var i: integer;
begin
 if not ServerSocket1.Active then
 begin
  Application.MessageBox('Nincs aktív szerver!',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 case Application.MessageBox('Tényleg meg akarod szüntetni a szervert?',PChar(ProgramName),mb_YesNo + mb_IconQuestion) of
  id_No:  Abort;
  id_Yes: begin
           for  i := 0 to ServerSocket1.Socket.ActiveConnections -1 do
            ServerSocket1.Socket.Connections[i].SendText('T_SERVERCLOSE');
           ServerSocket1.Active := false;
           mnuServerCreate.Enabled := true;
           mnuServerClose.Enabled := false;
           mnuServerKick.Enabled := false;
           lstGamers.Items.Text := '[csatlakozz egy szerverhez!]';
           BitBtn2.Enabled := true;
          end;
 end;
end;

procedure TForm2.mnuServerKickClick(Sender: TObject);
var Kirugando: string;
    Index, i: integer;
begin
 if (lstGamers.ItemIndex=lstGamers.Items.Count-1)or(lstGamers.ItemIndex=-1) then
 begin
  Application.MessageBox('Válaszd ki, hogy kit akarsz kirúgni!',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 Kirugando := Copy(lstGamers.Items[lstGamers.ItemIndex],1,Pos('(',lstGamers.Items[lstGamers.ItemIndex])-1);
 if Kirugando = txtName.Text then
 begin
  Application.MessageBox('Önmagadat nem rúghatod ki!',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 Index := ACL.Find(Kirugando);
 if Index = -1 then
 begin
  Application.MessageBox('Hiba: nincs ilyen játékos',PChar(ProgramName),mb_Ok + mb_IconHand);
  Abort;
 end;

 for i := 1 to ServerSocket1.Socket.ActiveConnections - 1 do
  if ServerSocket1.Socket.Connections[i].RemoteAddress = ACL.Get(Index).IPAddress then
   ServerSocket1.Socket.Connections[i].SendText('T_YOUAREKICKED');
end;

procedure TForm2.ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var kuldott: string;
    temp: string;
begin
 kuldott := Socket.ReceiveText;
 if pos('T_APPLY_',kuldott) = 1 then
 begin
  temp := copy(kuldott,9,length(kuldott));
  if ACL.Find(temp) = -1 then
   if ACL.Find2(Socket.RemoteAddress) = -1 then
    Socket.SendText('T_ACCEPT')
   else
    Socket.SendText('T_REFUSE_SAMEIP')
  else
   Socket.SendText('T_REFUSE_SAMENICK');
 end;
end;

procedure TForm2.ClientSocket1Read(Sender: TObject; Socket: TCustomWinSocket);
var kuldott: string;
begin
 kuldott := Socket.ReceiveText;

 if kuldott = 'T_ACCEPT' then
 begin
  Application.MessageBox('A szerver elfogadta a kapcsolatot',PChar(ProgramName),mb_Ok + mb_IconInformation);
 end;
 if kuldott = 'T_REFUSE_SAMENICK' then
 begin
  Application.MessageBox('A szerverre már jelentkezett egy ugyanilyen nevû játékos, más nevet kell választanod.',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  ClientSocket1.Active := false;
  BitBtn1.Enabled := true;
  mnuClientConnect.Enabled := true;
  mnuClientDisconnect.Enabled := false;
 end;
 if kuldott = 'T_REFUSE_SAMEIP' then
 begin
  Application.MessageBox('A szerverre már jelentkezett egy játékos ugyanerrõl az IP címõl',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  ClientSocket1.Active := false;
  BitBtn1.Enabled := true;
  mnuClientConnect.Enabled := true;
  mnuClientDisconnect.Enabled := false;
 end;
end;

procedure TForm2.mnuClientConnectClick(Sender: TObject);
var IP: string;
begin
 if ClientSocket1.Active then
 begin
  Application.MessageBox('Már van egy aktív kliens-csatlakozás',PChar(ProgramName),mb_Ok + mb_IconExclamation);
  Abort;
 end;

 IP := InputBox(PChar(ProgramName),'Kérem a szerver IP címét:','');
 if IP = '' then
 begin
  Application.MessageBox('Csatlakozási folyamat megszakítva',PChar(ProgramName),mb_Ok + mb_IconHand);
  Abort;
 end;

 ClientSocket1.Address := IP;
 ClientSocket1.Active := true;
end;

procedure TForm2.mnuClientDisconnectClick(Sender: TObject);
begin
 if Application.MessageBox('Tényleg meg akarod szakítani a kapcsolatot a szerverrel?',PChar(ProgramName),mb_YesNo + mb_IconQuestion) = idNo then
  Abort;

 ClientSocket1.Socket.SendText('T_QUIT');
 ClientSocket1.Active := false; 
end;

end.
