unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ScktComp;

type
  TForm1 = class(TForm)
    ServerSocket1: TServerSocket;
    LabeledEdit1: TLabeledEdit;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    procedure CheckBox1Click(Sender: TObject);
    procedure ServerSocket1ClientConnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1ClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure ServerSocket1ClientRead(Sender: TObject;
      Socket: TCustomWinSocket);
  private
    procedure AddItem(s: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.AddItem(s: string);
begin
 ListBox1.Items.Add(s);
 ListBox1.ItemIndex := ListBox1.Items.Count-1;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
 if CheckBox1.Checked = false then
 begin
  ServerSocket1.Close;
  AddItem('szerver leállítva');
  Abort;
 end;

 try
  ServerSocket1.Port := StrToInt(LabeledEdit1.Text);
 except
  on e:exception do
  begin
   Application.MessageBox('Hiba: számot adj meg portnak!','',mb_ok);
   Abort;
  end;
 end;

 ServerSocket1.Open;
 AddItem('szerver elindítva: ' + ServerSocket1.Socket.LocalHost + ' (' + ServerSocket1.Socket.LocalAddress + ')');
end;

procedure TForm1.ServerSocket1ClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
 AddItem('kliens csatlakozott: ' + Socket.RemoteHost + ' (' + Socket.RemoteAddress + ')');
end;

procedure TForm1.ServerSocket1ClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
 AddItem('kliens lecsatlakozott: ' + Socket.RemoteHost + ' (' + Socket.RemoteAddress + ')');
end;

procedure TForm1.ServerSocket1ClientRead(Sender: TObject; Socket: TCustomWinSocket);
var msg: string;
    i: integer;
begin
 msg := Socket.ReceiveText;
 for i := 0 to ServerSocket1.Socket.ActiveConnections-1 do
  ServerSocket1.Socket.Connections[i].SendText(msg);
end;

end.
