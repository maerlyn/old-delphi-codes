unit multipleuse;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, dbf;

type
  TMultipleUseForm = class(TForm)
    Dbf1: TDbf;
    Dbf2: TDbf;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  MultipleUseForm: TMultipleUseForm;

implementation

{$R *.DFM}

procedure TMultipleUseForm.FormCreate(Sender: TObject);
begin
  dbf1.Active:=true;
  dbf2.Active:=true;
end;

end.
