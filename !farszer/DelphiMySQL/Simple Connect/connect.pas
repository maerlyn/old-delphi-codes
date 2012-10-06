unit connect;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, _libmysq;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    edtHostname: TEdit;
    edtUsername: TEdit;
    edtPasswd: TEdit;
    edtDatabase: TEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mysqlrec: mysql; //Global mysql struct
  connected: Integer; //Global var to keep track of whether we are connected
implementation

{$R *.DFM}

procedure TForm1.Button3Click(Sender: TObject);
begin
 Close;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     if connected = 1 then
     {Close the Database Connection}
     mysql_close(@mysqlrec);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
   host, user, passwd: String;

begin
     host:= edtHostname.Text;
     user:= edtUsername.Text;
     passwd:= edtPasswd.Text;
     {Connect to server}
     mysql_connect(@mysqlrec, PChar(host), PChar(user), PChar(passwd));
     if mysqlrec.status = mysql_status_ready then
     begin
          ShowMessage ('Successfully connected to the Database.');
          connected:= 1; //keep track of connection
          Button2.Enabled:= true;
          Button4.Enabled:= true;
          Button1.Enabled:= false;
     end
     else
         ShowMessage ('Unable to connect to database.');
end;

procedure TForm1.Button4Click(Sender: TObject);
var
   db: String;
   retval: Integer;
begin
     db:= edtDatabase.Text;
     retval:= mysql_select_db(@mysqlrec, PChar(db));
     if retval <> 0 then
        ShowMessage('Error attaching to: ' + db)
     else
         ShowMessage('Successfully attached to ' + db);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
     mysql_close(@mysqlrec);
     ShowMessage('Disconnected from the database.');
     Button1.Enabled:= true;
     Button2.Enabled:= false;
     Button4.Enabled:= false;
     connected:= 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     connected:= 0; //We are not connected to the server
     {Disable the disconnect button till connected}
     Button2.Enabled:= false;
     {Disable the attach to db button till connected}
     Button4.Enabled:= false;
end;

end.
