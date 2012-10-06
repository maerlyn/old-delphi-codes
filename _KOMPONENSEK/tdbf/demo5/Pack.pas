unit Pack;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ExtCtrls, DBCtrls, Grids, DBGrids, dbf, StdCtrls;

type
  TPackTableForm = class(TForm)
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DataSource1: TDataSource;
    Dbf1: TDbf;
    Panel1: TPanel;
    Button2: TButton;
    Button3: TButton;
    Dbf1Field1: TStringField;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    labnormal: TLabel;
    labdeleted: TLabel;
    Button5: TButton;
    ShowDeleted: TCheckBox;
    Dbf1Deleted: TBooleanField;
    DBMemo1: TDBMemo;
    Splitter1: TSplitter;
    Dbf1Field2: TMemoField;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Dbf1Field3: TFloatField;
    procedure FormShow(Sender: TObject);
    procedure ClearTableClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure RefreshInfo(Sender: TObject);
    procedure Dbf1AfterDelete(DataSet: TDataSet);
    procedure ShowDeletedClick(Sender: TObject);
    procedure Dbf1AfterPost(DataSet: TDataSet);
    procedure Dbf1CalcFields(DataSet: TDataSet);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    batchmode:boolean;
  end;

var
  PackTableForm: TPackTableForm;

implementation

{$R *.DFM}

procedure TPackTableForm.FormShow(Sender: TObject);
begin
  Dbf1.Active:=false;
  ClearTableClick(Sender);
  Dbf1.Active:=true;
end;

procedure TPackTableForm.ClearTableClick(Sender: TObject);
begin
  try
    Dbf1.Active:=false;
    Dbf1.CreateTable;
  finally
    Dbf1.Active:=true;
  end;
  RefreshInfo(Sender);
end;

procedure TPackTableForm.Button2Click(Sender: TObject);
var
  x,i,r:integer;
  RandomString:string;
  MemoString:string;
  recno:integer;
begin
  Dbf1.DisableControls;
  batchMode:=true;
  try
    for x:=1 to 10 do begin
      Dbf1.Append;
      recno:=Dbf1.RecordCount;
      RandomString:=
        chr(random(26)+65)+
        chr(random(26)+65)+
        IntToStr(recno);

      r:=Random(100);
      Dbf1Field1.AsString:=RandomString;
      MemoString:='';
      for i:=1 to r do begin
        MemoString:=MemoString+'<<<' + RandomString
        + ' : '+IntToStr(i)+' of '+IntToStr(r)+' >>>' + #13 + #10;
      end;
      Dbf1Field2.AsString:=MemoString;
      Dbf1.Post;
    end;
  finally
    batchMode:=false;
    Dbf1.EnableControls;
  end;
  RefreshInfo(Sender);
end;

procedure TPackTableForm.Button3Click(Sender: TObject);
var
  x:integer;
  OldShowDeleted:boolean;
begin
  Dbf1.DisableControls;
  batchMode:=true;
  OldShowDeleted:=Dbf1.ShowDeleted;
  Dbf1.ShowDeleted:=false;
  try
    for x:=1 to 5 do begin
      if Dbf1.Eof then begin
        dbf1.first;
        if dbf1.Eof then exit;
      end;
      dbf1.delete;
    end;
    Dbf1.ShowDeleted:=OldShowDeleted;
  finally
    batchMode:=false;
    Dbf1.EnableControls;
  end;
  RefreshInfo(Sender);
end;

procedure TPackTableForm.Button5Click(Sender: TObject);
begin
  Dbf1.PackTable;
  RefreshInfo(Sender);
end;

procedure TPackTableForm.RefreshInfo(Sender: TObject);
var
  nbdeleted,nbnormal:integer;
  b:string;
  OldShowDeleted:boolean;
begin
  if batchMode then exit;
  Dbf1.DisableControls;
  b:=Dbf1.Bookmark;
  OldShowDeleted:=Dbf1.ShowDeleted;
  Dbf1.ShowDeleted:=true;
  try
    nbnormal:=0;
    nbdeleted:=0;
    Dbf1.First;
    While not Dbf1.Eof do begin
      if Dbf1.IsDeleted then inc(nbdeleted)
      else inc(nbnormal);
      Dbf1.Next;
    end;
    labnormal.caption:=intToStr(nbnormal);
    labdeleted.caption:=intToStr(nbdeleted);
  finally
    Dbf1.ShowDeleted:=OldShowDeleted;
    Dbf1.EnableControls;
    Dbf1.Bookmark:=b;
  end;
end;

procedure TPackTableForm.Dbf1AfterDelete(DataSet: TDataSet);
begin
  RefreshInfo(nil);
end;

procedure TPackTableForm.ShowDeletedClick(Sender: TObject);
begin
  Dbf1.ShowDeleted:=ShowDeleted.Checked;
  Dbf1.Resync([]);
end;

procedure TPackTableForm.Dbf1AfterPost(DataSet: TDataSet);
begin
  RefreshInfo(nil);
end;

procedure TPackTableForm.Dbf1CalcFields(DataSet: TDataSet);
begin
  Dbf1Deleted.AsBoolean:=Dbf1.IsDeleted;
end;

end.
