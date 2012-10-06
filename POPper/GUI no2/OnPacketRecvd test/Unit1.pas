unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Psock, NMpop3, ComCtrls;

type
  TForm1 = class(TForm)
    NMPOP31: TNMPOP3;
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    ListView1: TListView;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    Label2: TLabel;
    ProgressBar2: TProgressBar;
    ListView2: TListView;
    procedure Button1Click(Sender: TObject);
    procedure NMPOP31List(Msg, Size: Integer);
    procedure NMPOP31PacketRecvd(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mailsizes: array[1..1024] of integer;
  currmail: integer;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var i: integer;
begin
 button1.enabled := false;
 try
  nmpop31.Host := edit1.text;
  nmpop31.UserID := edit2.text;
  nmpop31.Password := edit3.text;

  nmpop31.Connect;
  nmpop31.List;

  progressbar2.Max := nmpop31.MailCount;
  for i := 1 to nmpop31.MailCount do
  begin
   nmpop31.GetMailMessage(i);
   progressbar2.Position := progressbar2.Position + 1;
  end;

 finally
  nmpop31.Disconnect;
  button1.Enabled := true;
 end;
end;

procedure TForm1.NMPOP31List(Msg, Size: Integer);
var a: tlistitem;
begin
 mailsizes[msg] := size;
 a := listview1.Items.Add;
 a.SubItems.add(inttostr(msg));
 a.subitems.add(inttostr(msg));
end;

procedure TForm1.NMPOP31PacketRecvd(Sender: TObject);
var a: tlistitem;
begin
 a := listview2.items.add;
 a.subitems.add(inttostr(currmail));
 a.SubItems.add(inttostr(nmpop31.BytesRecvd));
 a.subitems.add(inttostr(nmpop31.bytestotal));
 progressbar1.Max := nmpop31.BytesTotal;
 progressbar1.Position := nmpop31.BytesRecvd;
end;

end.
