unit Unit6;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, _libmysq;

type
  TForm6 = class(TForm)
    ListView1: TListView;
    BitBtn1: TBitBtn;
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;
  ColumnToSort: integer;
  compname,prodname: string;

implementation

uses Unit1,Unit2,Unit4;

{$R *.dfm}

procedure TForm6.ListView1Compare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
var
  ix: Integer;
begin
 if ColumnToSort = 0 then
  Compare := CompareText(Item1.Caption,Item2.Caption)
 else begin
  ix := ColumnToSort - 1;
  Compare := CompareText(Item1.SubItems[ix],Item2.SubItems[ix]);
 end;
end; 

procedure TForm6.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
 ColumnToSort := Column.Index;
 ListView1.AlphaSort;
end;

procedure TForm6.ListView1DblClick(Sender: TObject);
var frm: TForm2;
    i: integer;
    presults: pmysql_res;
    row: mysql_row;
    query: string;
    compid, prodid: integer;
    memo: string;
    tempstr: string;
    modal_result: integer;
begin
//  compname
//  prodname

 frm := TForm2.Create(Self);

 query := 'SELECT comp_id FROM companies WHERE comp_name="' + compname + '";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@(Form1.mysqlrec),PChar(query));
  presults := mysql_store_result(@(Form1.mysqlrec));
  row := mysql_fetch_row(presults)^;
  compid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;
 query := 'SELECT prod_id FROM products WHERE prod_name="' + prodname + '";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@(Form1.mysqlrec),PChar(query));
  presults := mysql_store_result(@(Form1.mysqlrec));
  row := mysql_fetch_row(presults)^;
  prodid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 query := 'SELECT memo,status FROM main WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid) + ';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@(Form1.mysqlrec),PChar(query));
  presults := mysql_store_result(@(Form1.mysqlrec));
  row := mysql_fetch_row(presults)^;
  memo := row[0];
  frm.Memo1.Text := row[0];

  for i := 0 to frm.ControlCount-1 do
   if pos('CheckBox',frm.Controls[i].Name)=1 then
    if pos(chr(frm.Controls[i].Tag),row[1]) > 0 then
     (frm.Controls[i] as TCheckBox).Checked := true;
 finally
  mysql_free_result(presults);
 end;



 query := 'SELECT memodate,todo FROM main WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@(Form1.mysqlrec),PChar(query));
  presults := mysql_store_result(@(Form1.mysqlrec));
  row := mysql_fetch_row(presults)^;
  frm.Label5.Caption := 'Jegyzet utolsó módosításának dátuma: ' + row[0];
  if row[1]='1' then
   frm.cbx7.Checked := true
  else
   frm.cbx7.Checked := false;
 finally
  mysql_free_result(presults);
 end;

 frm.lblCompany.Caption := compname;
 frm.lblProduct.Caption := prodname;
 tempstr := '123456';
 modal_result := mrOK;
 while (length(tempstr)>5)and(modal_result = mrOK) do
 begin
  frm.Width := 433;
  modal_result := frm.ShowModal;

  tempstr := '';
  for i := 0 to frm.ComponentCount-1 do
   if pos('CheckBox',frm.Components[i].Name)=1 then
    if (frm.Components[i] as TCheckBox).Checked then
     tempstr := tempstr + chr(frm.Components[i].Tag);
   if (length(tempstr) > 5)and(modal_result = mrOK) then
    Application.MessageBox('Egyszerre legfeljebb 5 lehetõséget lehet kiválasztani!','Fárszer',mb_OK+mb_IconError);
 end;

 if modal_result = mrCancel then
  Abort;

 query := 'UPDATE main SET status="'+tempstr+'" WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@(Form1.mysqlrec),PChar(query));

 if memo <> frm.Memo1.Text then
 begin
  tempstr := Form1.EncodeToHex(frm.Memo1.Text);

  query := 'UPDATE main SET memo='+tempstr+', memodate=now() WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  mysql_query(@(Form1.mysqlrec),PChar(query));
 end;

 query := 'UPDATE main SET todo=';
 if frm.cbx7.Checked then
  query := query + '1'
 else
  query := query + '0';
 query := query + ' WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@(Form1.mysqlrec),PChar(query));

 frm.Free;
end;

procedure TForm6.ListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
 compname := Item.SubItems[0];
 prodname := Item.SubItems[1];
end;

end.
