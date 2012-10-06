unit CopyTable;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Grids, DBGrids, Db, dbf;

type
  TCopyTableForm = class(TForm)
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    DataSource2: TDataSource;
    Panel1: TPanel;
    Panel2: TPanel;
    Copy1In2: TButton;
    Clear1: TButton;
    Add100_1: TButton;
    Dbf1: TDbf;
    Dbf2: TDbf;
    Dbf1Field1: TIntegerField;
    Dbf1Field2: TStringField;
    FastCopy: TCheckBox;
    procedure Clear1Click(Sender: TObject);
    procedure Add100_1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Copy1In2Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  CopyTableForm: TCopyTableForm;

implementation

{$R *.DFM}

procedure TCopyTableForm.Clear1Click(Sender: TObject);
begin
  try
    Dbf1.Active:=false;
    Dbf1.CreateTable;
  finally
    Dbf1.Active:=true;
    Add100_1Click(Sender);
  end;
end;

procedure TCopyTableForm.Add100_1Click(Sender: TObject);
var
  x:integer;
  RandomString:string;
begin
  Dbf1.DisableControls;
  try
    for x:=1 to 100 do begin
      Dbf1.Append;
      RandomString:=
        chr(random(26)+65)+
        chr(random(10)+48)+
        chr(random(10)+48)+
        chr(random(10)+48)+
        chr(random(10)+48);

      Dbf1Field1.AsInteger:=Random(10000);
      Dbf1Field2.AsString:=RandomString;
      Dbf1.Post;
    end;
  finally
    Dbf1.EnableControls;
  end;
end;

procedure TCopyTableForm.FormShow(Sender: TObject);
begin
  Dbf1.Active:=false;
  Dbf2.Active:=false;
  Clear1Click(Sender);
  Dbf1.Active:=true;
end;

procedure TCopyTableForm.Copy1In2Click(Sender: TObject);
var
  i:integer;
begin
  Dbf2.Active:=false;
  Dbf2.CreateTableEx(Dbf1.DbfFieldList);
  Dbf2.Active:=true;
  Dbf1.First;
  if FastCopy.Checked then begin
    Dbf1.DisableControls;
    Dbf2.DisableControls;
  end;
  while not Dbf1.Eof do begin
    Dbf2.Insert;
    For i:=0 to Dbf1.FieldCount-1 do begin
      Dbf2.Fields[i].Value:=Dbf1.Fields[i].Value;
    end;
    Dbf2.Post;
    Dbf1.Next;
  end;
  Dbf1.EnableControls;
  Dbf2.EnableControls;
end;

end.

