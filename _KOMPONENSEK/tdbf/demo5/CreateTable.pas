unit CreateTable;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, DBGrids, Db, dbf, StdCtrls, UDbfFieldDef, UDbfCommon;

type
  TCreateTableForm = class(TForm)
    CreateMethod1: TButton;
    Dbf1: TDbf;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Populate: TButton;
    CreateMethod2: TButton;
    procedure CreateMethod1Click(Sender: TObject);
    procedure PopulateClick(Sender: TObject);
    procedure CreateMethod2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  CreateTableForm: TCreateTableForm;

implementation

{$R *.DFM}

procedure TCreateTableForm.CreateMethod1Click(Sender: TObject);
begin
  With Dbf1 do begin
    Close;
    TableName := 'table1.dbf';
// Method 1
    with FieldDefs do begin
      Clear;
      Add('Field1',ftString,10,False);
      Add('Field2',ftInteger,0,False);
      Add('Address',ftMemo,0,False);
      Add('Date',ftDate,0,False);
    end;
    CreateTable;
    Open;
  end;
end;

procedure TCreateTableForm.PopulateClick(Sender: TObject);
var
  i:integer;
  f1,f2:TField;
begin
  f1:=Dbf1.FieldByName('Field1');
  f2:=Dbf1.FieldByName('Field2');
  for i:=0 to 100 do begin
    Dbf1.Append;
    f1.AsString:=
      chr((i * 1 + 4)  mod 26 + 65)+
      chr((i * 2 + 5)  mod 26 + 65)+
      chr((i * 3 + 6)  mod 26 + 65);
    f2.AsInteger:=i;
    Dbf1.Post;
  end;
end;


procedure TCreateTableForm.CreateMethod2Click(Sender: TObject);
var
  DbfFieldDefs:TDbfFieldDefs;
begin
  With Dbf1 do begin
    Close;
    TableName := 'table1.dbf';
// Method 2
    DbfFieldDefs:=TDbfFieldDefs.Create(self);
    try
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'Field1';
        NativeFieldType:='C';
        Size := 10;
      end;
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'Field2';
        NativeFieldType:='I';
        Size := 10;
        Prec := 3;
      end;
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'fAutoInc';
        NativeFieldType:='+';
      end;
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'f_I';
        NativeFieldType:='I';
      end;
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'f_O';
        NativeFieldType:='O';
      end;
      with DbfFieldDefs.AddFieldDef do begin
        FieldName := 'f_@';
        NativeFieldType:='I'; //'@';
      end;
      Dbf1.CreateTableEx(DbfFieldDefs);
    finally
      FreeAndNil(DbfFieldDefs);
    end;
    Open;
  end;
end;

end.

