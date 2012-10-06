unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ExtCtrls, DBCtrls, Grids, DBGrids, Menus, Buttons,
  ComCtrls, dbf, UDbfCommon, shellapi;

type
  TMainForm = class(TForm)
    DemoButton: TButton;
    DbfDemo: TDbf;
    DataSourceDemo: TDataSource;
    DbfDisco: TDbf;
    DatasourceDisco: TDataSource;
    Button1: TButton;
    DBNavigator1: TDBNavigator;
    DBText1: TDBText;
    DBGrid1: TDBGrid;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label1: TLabel;
    Bevel3: TBevel;
    Image1: TImage;
    DBRichEdit1: TDBRichEdit;
    DbfDiscoAUTHOR: TStringField;
    DbfDiscoTITLE: TStringField;
    DbfDiscoCOMPANY: TStringField;
    DbfDiscoCOUNTRY: TStringField;
    DbfDiscoYEAR: TSmallintField;
    DbfDiscoPRICE: TFloatField;
    DbfDiscoNOTE: TStringField;
    DbfDiscoQTY: TSmallintField;
    DbfDemoID: TStringField;
    DbfDemoTITLE: TStringField;
    DbfDemoDESCR: TMemoField;
    DbfDemoDEMO: TStringField;
    DbfDiscoCALCPRICE: TCurrencyField;
    LabelEmail: TLabel;
    label_website: TLabel;
    DbfDiscoHIGHPRICE: TBooleanField;
    DbfDiscoLAST_SELL: TDateField;
    DbfDiscoIN_STOCK: TBooleanField;
    LabelVersion: TLabel;
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    procedure DbfDemoAfterScroll(DataSet: TDataSet);
    procedure DemoButtonClick(Sender: TObject);
    procedure DbfDiscoFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DBGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DbfDiscoCalcFields(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure DataSourceDemoStateChange(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelEmailMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure label_websiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Edit1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    lastForm:TForm;
  end;
var
  MainForm: TMainForm;

implementation

uses EditTopics, Simple, Index, Search, Filter, Calc, Schema, Schema2,
  CreateTable, Pack, CopyTable, multipleuse, compatibility;

{$R *.DFM}

  

procedure TMainForm.DbfDemoAfterScroll(DataSet: TDataSet);
begin
  DemoButton.Enabled:=length(trim(DbfDemo.FieldByName('DEMO').AsString))>0;
end;

procedure TMainForm.DemoButtonClick(Sender: TObject);
var
  demo:string;
  newForm:TForm;
begin
  newForm:=nil;
  demo:=trim(DbfDemo.FieldByName('DEMO').AsString);
  if demo='simple' then newForm:=simpleForm
  else if demo='index' then newForm:=indexForm
  else if demo='search' then newForm:=SearchForm
  else if demo='filter' then newForm:=FilterForm
  else if demo='memo' then newForm:=EditTopicsForm
  else if demo='calc' then newForm:=CalcForm
  else if demo='schema' then newForm:=Schema1Form
  else if demo='schema2' then newForm:=Schema2Form
  else if demo='create' then newForm:=CreateTableForm
  else if demo='pack' then newForm:=PackTableForm
  else if demo='copy' then newForm:=CopyTableForm
  else if demo='multiple' then newForm:=MultipleUseForm
  else if demo='compatib' then newForm:=CompatibilityForm
  else ;
  if (lastForm<>newForm) and (lastform<>nil) then lastForm.Hide;
  if (newform<>nil) then newForm.Show;
  lastForm:=newForm;
end;

procedure TMainForm.DbfDiscoFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
var
  year:integer;
  country:string;
  correct_year:boolean;
  correct_country:boolean;
begin
  year:=StrToIntDef(DbfDiscoYear.AsString,0);
  Country:=DbfDiscoCountry.AsString;
  correct_year:=
    ((year=0) and FilterForm.cbBlank.checked)
    or
    ((year >= FilterForm.Year_From.Tag)
    and (year <= FilterForm.Year_To.Tag));

  if Country='USA' then correct_country:=FilterForm.cbUSA.checked
  else if Country='USA' then correct_country:=FilterForm.cbUSA.checked
  else if Country='SWE' then correct_country:=FilterForm.cbSWE.checked
  else if Country='UK' then correct_country:=FilterForm.cbUK.checked
  else if Country='GER' then correct_country:=FilterForm.cbGER.checked
  else if Country='HOL' then correct_country:=FilterForm.cbHOL.checked
  else if Country='ITA' then correct_country:=FilterForm.cbITA.checked
  else correct_country:=FilterForm.cbOTH.checked;

  Accept:=correct_year and correct_country;

end;

procedure TMainForm.ButtonCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  DbfDemo.IndexName:='ID.NDX';
end;

procedure TMainForm.DBGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssShift in Shift) and (ssCtrl in Shift) then EditTopicsForm.showModal;
end;

procedure TMainForm.DbfDiscoCalcFields(DataSet: TDataSet);
var
  Price:double;
  Qty:double;
  CalcPrice:double;
begin
  try
    Price:=DbfDiscoPRICE.AsFloat;
    Qty:=DbfDiscoQTY.AsFloat;
    if Qty=0 then calcPrice:=0
    else calcPrice:=Price/Qty;
    DbfDiscoCALCPRICE.AsFloat:=calcPrice;
    DbfDiscoHighPrice.AsBoolean:=calcPrice>=10;
  except
    DbfDiscoCALCPRICE.AsFloat:=0;
    DbfDiscoHighPrice.AsBoolean:=false;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  path:string;
begin
  path:=ExtractFilePath(Application.ExeName)+'data';
  createdir(path);
(*
  I do that sometime for debugging purposes sometime

  DeleteFile(path+'\author.ndx');
  DeleteFile(path+'\id.ndx');
  DeleteFile(path+'\title.ndx');
  DeleteFile(path+'\price.ndx');

  FileCopy(path+'savebase\disco.dbf'     ,path+'disco.dbf');
  FileCopy(path+'savebase\tdbf_demo.dbf',path+'tdbf_demo.dbf');
  FileCopy(path+'savebase\tdbf_demo.dbt' ,path+'tdbf_demo.dbt');
*)
  LabelVersion.Caption:=DbfDemo.Version;
  DbfDemo.Active:=true;
  DbfDisco.Active:=true;
end;


procedure TMainForm.DataSourceDemoStateChange(Sender: TObject);
var
  ed:boolean;
begin
  ed:=DbfDemo.State in [dsEdit,dsInsert];
  if editTopicsForm=nil then exit;
  with editTopicsForm do begin
    speedButton1.enabled:=ed;
    speedButton2.enabled:=ed;
    speedButton3.enabled:=ed;
    speedButton4.enabled:=ed;
    speedButton5.enabled:=ed;
    speedButton6.enabled:=ed;
    speedButton7.enabled:=ed;
    speedButton8.enabled:=ed;
    speedButton9.enabled:=ed;
  end;
end;

procedure TMainForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DbfDemo.About;
end;


procedure TMainForm.LabelEmailMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Nice and easy isn't it ?
  ShellExecute(handle,'open','mailto:Pascal GANAYE<p.ganaye@bigfoot.com>',nil,nil,SW_SHOWNORMAL);
end;

procedure TMainForm.label_websiteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Nice and easy isn't it ?
  ShellExecute(handle,'open','http://www.tdbf.net',nil,nil,SW_SHOWNORMAL);
end;

procedure TMainForm.Edit1Click(Sender: TObject);
begin
  if (lastForm<>EditTopicsForm) and (lastform<>nil) then lastForm.Hide;
  EditTopicsForm.Show;
  lastForm:=EditTopicsForm;
end;

end.


