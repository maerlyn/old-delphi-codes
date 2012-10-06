unit Compatibility;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, dbf, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TCompatibilityForm = class(TForm)
    Dbf1: TDbf;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    DBGrid1: TDBGrid;
    RadioGroup1: TRadioGroup;
    Label1: TLabel;
    DBMemo1: TDBMemo;
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CompatibilityForm: TCompatibilityForm;

implementation

{$R *.dfm}

procedure TCompatibilityForm.RadioGroup1Click(Sender: TObject);
var
  filename:string;
begin
  case RadioGroup1.ItemIndex of
  0: filename:='DBASE3+.dbf';
  1: filename:='DBASE4.dbf';
  2: filename:='DBASEWIN.dbf';
  3: filename:='VisualDBase.dbf';
  end;
  dbf1.Active:=false;
  dbf1.TableName:=filename;
  dbf1.Active:=true;
end;

end.
