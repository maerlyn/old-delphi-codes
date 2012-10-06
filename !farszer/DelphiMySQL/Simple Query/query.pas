unit query;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, _libmysq, Grids, DBGrids;

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
    Button5: TButton;
    edtQuery: TEdit;
    Label5: TLabel;
    strGrid: TStringGrid;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
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
     if mysqlrec._net.last_errno = 0 then
     begin
          ShowMessage ('Successfully connected to server');
          connected:= 1; //keep track of connection
          Button2.Enabled:= true;
          Button4.Enabled:= true;
          Button1.Enabled:= false;
     end
     else
         ShowMessage (Trim(mysqlrec._net.last_error));
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
         begin
         ShowMessage('Successfully attached to ' + db);
         Button5.Enabled:= true;
         end;
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
     {Disable query till we attach to a database}
     Button5.Enabled:= false;
end;

procedure TForm1.Button5Click(Sender: TObject); //Execute Query on DB mysql
var
   presults: pmysql_res; //results structure *pointer
   prow: pmysql_row;     //row structure *pointer
   row: mysql_row;       //Couldnt figure out pointer arithmetic so....you'll see
   i, j: Integer;        //Counter vars
   query: String;
begin
     query:= edtQuery.Text; //Query text
     presults:= nil;
     try
     mysql_query(@mysqlrec, PChar(query)); //Send Query to server
     presults:= mysql_store_result(@mysqlrec); //Store results locally
     strGrid.Cells[0,0]:= 'Host';
     strGrid.Cells[1,0]:= 'User';
     strGrid.Cells[2,0]:= 'DB';
     for i:= 1 to presults^.row_count do begin
     prow:= mysql_fetch_row(presults);
     row:= prow^;  //Only way I could figure out to use an index into a MYSQL_ROW struct
     for j:= 0 to presults^.field_count -1 do begin
         StrGrid.Cells[j, i]:= StrPas(row[j]); // Index into row for fields
     end; // j
     end; // i
     finally
         mysql_free_result(presults); //release the stored results from memory
     end;
end;

end.
