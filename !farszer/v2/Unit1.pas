unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, ToolWin, Grids, IniFiles, _libmysq;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    Bevel1: TBevel;
    Splitter2: TSplitter;
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    mysqlrec: mysql;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var cfg: TIniFile;
    host,user,passwd: string;
    retval: integer;
    presults: pmysql_res;
    prow: pmysql_row;
    row: mysql_row;
    query: string;
    a: TListColumn;
begin
 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'farszer.ini');
 host:= cfg.ReadString('mysql','host','');
 user:= cfg.ReadString('mysql','user','');
 passwd:= cfg.ReadString('mysql','passwd','');

 mysql_connect(@mysqlrec, PChar(host), PChar(user), PChar(passwd));
 if mysqlrec._net.last_errno <> 0 then
 begin
  Application.MessageBox('Nem sikerült csatlakozni az adatbázisszerverhez!','Fárszer',mb_IconError + mb_Ok);
  Application.Terminate;
 end;

 retval := mysql_select_db(@mysqlrec,'farszer');
 if retval <> 0 then
 begin
  Application.MessageBox('nem sikerült kiválasztani az adatbázist!','Fárszer',mb_IconError+mb_Ok);
  Halt;
 end;

 query := 'SELECT prod_name FROM products ORDER BY prod_name ASC;';
 presults := nil;
 try
  mysql_query(@mysqlrec, PChar(query));
  presults := mysql_store_result(@mysqlrec);
  prow := mysql_fetch_row(presults);
  row := prow^;
  a := ListView1.Columns.Add;
  a.Caption := row[0];
 finally
  mysql_free_result(presults);
 end;

 mysql_close(@mysqlrec);
 cfg.Free;
end;

end.
