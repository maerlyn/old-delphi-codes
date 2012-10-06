unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, ADODB, StdCtrls, DBCtrls, ExtCtrls, ShellAPI;

type
  TTennivalokForm = class(TForm)
    ADOConnection1: TADOConnection;
    DataSource1: TDataSource;
    ADOTable1: TADOTable;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBMemo1: TDBMemo;
    procedure FormCreate(Sender: TObject);
    procedure DropFiles(var Msg: TWmDropFiles);message wm_DropFiles;
    procedure CopyData(var Msg: TWmCopyData);message wm_CopyData;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TennivalokForm: TTennivalokForm;

implementation

{$R *.DFM}

procedure TTennivalokForm.CopyData(var Msg: TWmCopyData);
var Filename: string;
begin
 if IsIconic(Application.Handle) then
  Application.Restore;
 Filename := PChar(Msg.CopyDataStruct.lpData);
 ADOTable1.Insert;
 ADOTable1.FieldByName('Filename').AsString := Filename;
 DBMemo1.SetFocus; 
end;

procedure TTennivalokForm.DropFiles(var Msg: TWmDropFiles);
var nFiles, i: integer;
    Filename: string;
begin
 nFiles := DragQueryFile(Msg.Drop,$FFFFFFFF,nil,0);
 try
  for i := 0 to nFiles - 1 do
  begin
   SetLength(Filename,80);
   DragQueryFile(Msg.Drop,i,PChar(Filename),80);
   Filename := PChar(Filename);
   ADOTable1.InsertRecord([Filename,'']);
  end;
 finally
  DragFinish(Msg.Drop);
 end;
 ADOTable1.Edit;
 DBMemo1.SetFocus;  
end;

procedure TTennivalokForm.FormCreate(Sender: TObject);
var temp: string;
begin
 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Password="";';
 temp := temp + 'Data Source=' + ExtractFilePath(ParamStr(0)) + 'Data.mdb' + ';';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Persist Security Info=True';
 ADOConnection1.ConnectionString := temp;

 DragAcceptFiles(Handle,true);
end;

end.
 
