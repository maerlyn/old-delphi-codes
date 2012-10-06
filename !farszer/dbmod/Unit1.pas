unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, _libmysq, IniFiles;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var host,user,passwd: string;
    retval: Integer;
    presults: pmysql_res;
    query: string;
    cfg: TIniFile;
    mysqlrec: mysql;
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
  Application.Terminate;
 end;

 query:= 'ALTER TABLE main ADD numbers VARCHAR(8) NOT NULL AFTER status;';
 presults:= nil;
 try
  mysql_query(@mysqlrec, PChar(query));
  presults:= mysql_store_result(@mysqlrec);
 finally
  mysql_free_result(presults);
 end;

 Memo1.Lines.Add('Kész, lehet használni a programot.');

end;
end.
