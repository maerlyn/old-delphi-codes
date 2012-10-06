//{$DEFINE DEBUG}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, _libmysq, StdCtrls, ComCtrls, IniFiles, Grids, Menus, Buttons,
  ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    PopupMenu1: TPopupMenu;
    jcg1: TMenuItem;
    jtermk1: TMenuItem;
    N1: TMenuItem;
    Cgtrlse1: TMenuItem;
    ermktrlse1: TMenuItem;
    N2: TMenuItem;
    Keress1: TMenuItem;
    StringGrid1: TStringGrid;
    Cgtneve1: TMenuItem;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
    function  EncodeToHex(source: string): string;
    procedure jcg1Click(Sender: TObject);
    procedure jtermk1Click(Sender: TObject);
    procedure Cgtrlse1Click(Sender: TObject);
    procedure ermktrlse1Click(Sender: TObject);
    procedure Keress1Click(Sender: TObject);
    procedure Cgtneve1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    mysqlrec: mysql;
  end;

var
  Form1: TForm1;
  connected: boolean;
  companies: TStringList;
  products: TStringList;
  TimeTillRefresh: integer = 60;
  cfg: TIniFile;

implementation

uses Unit2, Unit3, Unit4, Unit5, Unit6;

{$R *.dfm}

function TForm1.EncodeToHex(source: string): string;
var i: integer;
    s: string;
begin
 Result := '0x';
 for i := 1 to length(source) do
 begin
  s := IntToHex(ord(source[i]),2);
  Result := Result + s;
 end;
 if pos('E1',Result)>0 then
  Result := copy(Result,1,pos('E1',Result)-1)+'A0'+copy(Result,pos('E1',Result)+2,length(Result));
 if pos('E9',Result)>0 then
  Result := copy(Result,1,pos('E9',Result)-1)+'82'+copy(Result,pos('E9',Result)+2,length(Result));
end;

procedure TForm1.FormCreate(Sender: TObject);
var host,user,passwd: string;
    retval: Integer;
    presults: pmysql_res;
    prow: pmysql_row;
    row: mysql_row;
    i,j: integer;
    query: string;
    s: string;
begin
 cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'farszer.ini');
 host:= cfg.ReadString('mysql','host','');
 user:= cfg.ReadString('mysql','user','');
 passwd:= cfg.ReadString('mysql','passwd','');

 connected := false;
 mysql_connect(@mysqlrec, PChar(host), PChar(user), PChar(passwd));
 if mysqlrec._net.last_errno <> 0 then
 begin
  Application.MessageBox('Nem sikerült csatlakozni az adatbázisszerverhez!','Fárszer',mb_IconError + mb_Ok);
  Application.Terminate;
 end;
 connected := true;

 retval := mysql_select_db(@mysqlrec,'farszer');
 if retval <> 0 then
 begin
  Application.MessageBox('nem sikerült kiválasztani az adatbázist!','Fárszer',mb_IconError+mb_Ok);
  Application.Terminate;
 end;

 query:= 'SELECT count(comp_id) FROM companies';
 presults:= nil;
 try
  mysql_query(@mysqlrec, PChar(query));
  presults:= mysql_store_result(@mysqlrec);
  prow:= mysql_fetch_row(presults);
  row:= prow^;
  StringGrid1.RowCount := StrToInt(row[0])+1;
 finally
  mysql_free_result(presults);
 end;
 query := 'SELECT count(prod_id) FROM products';
 presults := nil;
 try
  mysql_query(@mysqlrec, PChar(query));
  presults := mysql_store_result(@mysqlrec);
  prow := mysql_fetch_row(presults);
  row := prow^;
  StringGrid1.ColCount := StrToInt(row[0])+2;
 finally
  mysql_free_result(presults);
 end;

 query := 'SELECT comp_name FROM companies ORDER BY comp_name';
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults:= mysql_store_result(@mysqlrec);
  ListBox1.Items.Clear;
  for i := 1 to presults^.row_count do
  begin
   prow:= mysql_fetch_row(presults);
   row:= prow^;
   ListBox1.Items.Add(row[0]);
  end;
 finally
  mysql_free_result(presults);
 end;

 StringGrid1.ColWidths[0] := 1;
 for i := 0 to ListBox1.Items.Count-1 do
 begin
  s := ListBox1.Items[i];
  StringGrid1.Cells[1,i+1] := s;
  if StringGrid1.ColWidths[1] < StringGrid1.Canvas.TextWidth(ListBox1.Items[i])+10 then
   StringGrid1.ColWidths[1] := StringGrid1.Canvas.TextWidth(ListBox1.Items[i])+10;
  StringGrid1.Cells[0,i+1] := IntToStr(i+1);
  if StringGrid1.ColWidths[0] < StringGrid1.Canvas.TextWidth(IntToStr(i))+10 then
   StringGrid1.ColWidths[0] := StringGrid1.Canvas.TextWidth(IntToStr(i))+10;
 end;

 query := 'SELECT prod_name FROM products';
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  ListBox1.Items.Clear;
  for j := 1 to presults^.row_count do
  begin
   prow := mysql_fetch_row(presults);
   row := prow^;
   ListBox1.Items.Add(row[0]);
  end;
 finally
  mysql_free_result(presults);
 end;

 for j := 0 to ListBox1.Items.Count-1 do
 begin
  StringGrid1.Cells[j+2,0] := ListBox1.Items[j];
  StringGrid1.ColWidths[j+2] := StringGrid1.Canvas.TextWidth(ListBox1.Items[j])+10;
 end;

 companies := TStringList.Create;
 products := TStringList.Create;

 for i := 1 to StringGrid1.RowCount-1 do
  companies.Add(StringGrid1.Cells[1,i]);
 for i := 1 to StringGrid1.ColCount-1 do
  products.Add(StringGrid1.Cells[i,0]);

 query := 'SELECT companies.comp_name, products.prod_name, main.status FROM main main inner join companies companies ';
 query := query + 'on (companies.comp_id=main.comp_id) inner join products products on ';
 query := query + '(products.prod_id=main.prod_id)';
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  for i := 1 to presults^.row_count do
  begin
   prow := mysql_fetch_row(presults);
   row := prow^;
   StringGrid1.Cells[products.IndexOf(row[1])+1,companies.IndexOf(row[0])+1] := row[2];
  end;
 finally
  mysql_free_result(presults);
 end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if connected then
  mysql_close(@mysqlrec);
 companies.Free;
 products.Free;
 cfg.Free;
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
var Pt: TPoint;
    Cell: TPoint;
    frm: TForm2;
    frm2: TForm4;
    i: integer;
    presults: pmysql_res;
    row: mysql_row;
    query: string;
    compid, prodid: integer;
    memo: string;
    tempstr: string;
    modal_result: integer;
begin
 GetCursorPos(Pt);
 Pt := StringGrid1.ScreenToClient(Pt);
 StringGrid1.MouseToCell(Pt.X,Pt.Y,Cell.X,Cell.Y);
 if not((Cell.X <= 1)or(Cell.Y = 0)) then
 begin
  frm := TForm2.Create(Self);
  frm.lblCompany.Caption := StringGrid1.Cells[1,Cell.Y];
  frm.lblProduct.Caption := StringGrid1.Cells[Cell.X,0];
  for i := 0 to frm.ControlCount-1 do
   if pos('CheckBox',frm.Controls[i].Name)=1 then
    if pos(chr(frm.Controls[i].Tag),StringGrid1.Cells[Cell.X,Cell.Y]) > 0 then
     (frm.Controls[i] as TCheckBox).Checked := true;

  query := 'SELECT comp_id FROM companies WHERE comp_name="'+StringGrid1.Cells[1,Cell.Y]+'";';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   compid := StrToInt(row[0]);
  finally
   mysql_free_result(presults);
  end;
  query := 'SELECT prod_id FROM products WHERE prod_name="'+StringGrid1.Cells[Cell.X,0]+'";';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   prodid := StrToInt(row[0]);
  finally
   mysql_free_result(presults);
  end;

  query := 'SELECT memo FROM main WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid) + ';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   memo := row[0];
   frm.Memo1.Text := row[0];
  finally
   mysql_free_result(presults);
  end;

  query := 'SELECT memodate,todo,numbers FROM main WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   frm.Label5.Caption := 'Jegyzet utolsó módosításának dátuma: ' + row[0];
   if row[1]='1' then
    frm.cbx7.Checked := true
   else
    frm.cbx7.Checked := false;

   frm.txtViszony.Text := row[2]; 
  finally
   mysql_free_result(presults);
  end;

  tempstr := '123456';
  modal_result := mrOK;
  while (length(tempstr)>5)and(modal_result = mrOK) do
  begin
   frm.Width := 433;
   modal_result := frm.ShowModal; //it mutatjuk meg a formot

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
  mysql_query(@mysqlrec,PChar(query));
  StringGrid1.Cells[Cell.X,Cell.Y] := tempstr;

  if memo <> frm.Memo1.Text then
  begin
   tempstr := EncodeToHex(frm.Memo1.Text);

   query := 'UPDATE main SET memo='+tempstr+', memodate=now() WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
   {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
   mysql_query(@mysqlrec,PChar(query));
  end;

  query := 'UPDATE main SET todo=';
  if frm.cbx7.Checked then
   query := query + '1'
  else
   query := query + '0';
  query := query + ' WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  mysql_query(@mysqlrec,PChar(query));

  query := 'UPDATE main SET numbers="' + frm.txtViszony.Text;
  query := query + '" WHERE comp_id='+IntToStr(compid)+' AND prod_id='+IntToStr(prodid)+';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  mysql_query(@mysqlrec,PChar(query));

  frm.Free;
 end
 else
 if (Cell.X = 1)and(Cell.Y>0) then  //ha cégnévre kattintott
 begin
  frm2 := TForm4.Create(Self);
  query := 'SELECT comp_id FROM companies WHERE comp_name="'+StringGrid1.Cells[1,Cell.Y]+'";';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   compid := StrToInt(row[0]);
  finally
   mysql_free_result(presults);
  end;

  query := 'SELECT comp_id,comp_name,comp_addr,comp_telnum,comp_faxnum,comp_contact,comp_vatnum,';
  query := query + 'comp_owner,comp_email,comp_homepage FROM companies WHERE comp_id=' + IntToStr(compid) + ';';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  presults := nil;
  try
   mysql_query(@mysqlrec,PChar(query));
   presults := mysql_store_result(@mysqlrec);
   row := mysql_fetch_row(presults)^;
   frm2.Label2.Caption := row[0];
   frm2.Label4.Caption := row[1];
   frm2.LabeledEdit1.Text := row[2];
   frm2.LabeledEdit2.Text := row[3];
   frm2.LabeledEdit3.Text := row[4];
   frm2.LabeledEdit4.Text := row[5];
   frm2.LabeledEdit5.Text := row[6];
   frm2.LabeledEdit6.Text := row[7];
   frm2.LabeledEdit7.Text := row[8];
   frm2.LabeledEdit8.Text := row[9];
  finally
   mysql_free_result(presults);
  end;

  modalresult := frm2.ShowModal;
  if modalresult = mrOK then
  begin
   if trim(frm2.LabeledEdit5.Text) = '' then
    frm2.LabeledEdit5.Text := '0';
   query := 'UPDATE companies SET comp_addr="' + frm2.LabeledEdit1.Text + '", ' +
                                 'comp_telnum="' + frm2.LabeledEdit2.Text + '", ' +
                                 'comp_faxnum="' + frm2.LabeledEdit3.Text + '", ' +
                                 'comp_contact="' + frm2.LabeledEdit4.Text + '", ' +
                                 'comp_vatnum=' + frm2.LabeledEdit5.Text + ', ' +
                                 'comp_owner="' + frm2.LabeledEdit6.Text + '", ' +
                                 'comp_email="' + frm2.LabeledEdit7.Text + '", ' +
                                 'comp_homepage="' + frm2.LabeledEdit8.Text + '" ' +
                                 'WHERE comp_id='+IntToStr(compid)+';';
   {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
   mysql_query(@mysqlrec,PChar(query));
  end;
 end;
end;

procedure TForm1.jcg1Click(Sender: TObject);
var compname: string;
    compid: integer;
    query: string;
    presults: pmysql_res;
    row: mysql_row;
    prod_ids: TStringList;
    i,k: integer;
begin
//új cég
 compname := InputBox('Fárszer','Kérem az új cég nevét:','');
 if trim(compname)='' then
  Exit;

 query := 'INSERT INTO companies (comp_name) VALUES ("'+compname+'");';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 query := 'SELECT comp_id FROM companies WHERE comp_name="'+compname+'";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  row := mysql_fetch_row(presults)^;
  compid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 prod_ids := TStringList.Create;
 query := 'SELECT prod_id FROM products';
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  for i := 1 to presults^.row_count do
  begin
   row := mysql_fetch_row(presults)^;
   prod_ids.Add(row[0]);
  end;
 finally
  mysql_free_result(presults);
 end;

 for i:=0 to prod_ids.Count-1 do
 begin
  query := 'INSERT INTO main (comp_id,prod_id,status,memo,todo,numbers) VALUES ('+IntToStr(compid)+','+prod_ids[i]+',"","",0,"");';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  mysql_query(@mysqlrec,PChar(query));
 end;

 query := 'UPDATE main SET memodate=NOW() WHERE comp_id='+IntToStr(compid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));
 query := 'UPDATE main SET todo=0 WHERE comp_id='+IntToStr(compid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 companies.Free;
 products.Free;

 for i := 0 to StringGrid1.ColCount-1 do
  for k := 0 to StringGrid1.RowCount-1 do
   StringGrid1.Cells[i,k] := '';

 FormCreate(Sender);
end;

procedure TForm1.jtermk1Click(Sender: TObject);
var prodname: string;
    prodid: integer;
    query: string;
    presults: pmysql_res;
    row: mysql_row;
    comp_ids: TStringList;
    i,k: integer;
begin
//új termék
 prodname := InputBox('Fárszer','Kérem az új termék nevét:','');
 if trim(prodname)='' then
  Exit;

 query := 'INSERT INTO products (prod_name) VALUES ("'+prodname+'");';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 query := 'SELECT prod_id FROM products WHERE prod_name="'+prodname+'";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  row := mysql_fetch_row(presults)^;
  prodid:= StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 comp_ids := TStringList.Create;
 query := 'SELECT comp_id FROM companies';
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  for i := 1 to presults^.row_count do
  begin
   row := mysql_fetch_row(presults)^;
   comp_ids.Add(row[0]);
  end;
 finally
  mysql_free_result(presults);
 end;

 for i:=0 to comp_ids.Count-1 do
 begin
  query := 'INSERT INTO main (comp_id,prod_id,status,memo,numbers) VALUES ('+comp_ids[i]+','+IntToStr(prodid)+',"","","");';
  {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
  mysql_query(@mysqlrec,PChar(query));
 end;


 query := 'UPDATE main SET todo=0 WHERE prod_id=' + IntToStr(prodid) + ';';
 mysql_query(@mysqlrec,PChar(query));
 query := 'UPDATE main SET memodate=NOW() WHERE prod_id=' + IntToStr(prodid) + ';';
 mysql_query(@mysqlrec,PChar(query));

 companies.Free;
 products.Free;

 for i := 0 to StringGrid1.ColCount-1 do
  for k := 0 to StringGrid1.RowCount-1 do
   StringGrid1.Cells[i,k] := '';

 FormCreate(Sender);
end;

procedure TForm1.Cgtrlse1Click(Sender: TObject);
var frm: TForm3;
    i,k: integer;
    modal_result: integer;
    query: string;
    presults: pmysql_res;
    row: mysql_row;
    compid: integer;
begin
//cég törlése
 frm := TForm3.Create(Self);
 for i := 1 to StringGrid1.RowCount-1 do
  frm.ListBox1.Items.Add(StringGrid1.Cells[1,i]);

 frm.ListBox1.ItemIndex := -1;

 modal_result := frm.ShowModal;

 if modal_result = mrCancel then
  Exit;

 if frm.ListBox1.ItemIndex = -1 then
 begin
  Application.MessageBox('Ki kell választani egy céget a törléshez!','Fárszer',mb_IconError+mb_Ok);
  Exit;
 end;

 query := 'SELECT comp_id FROM companies WHERE comp_name="'+frm.ListBox1.Items[frm.ListBox1.ItemIndex]+'";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  row := mysql_fetch_row(presults)^;
  compid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 query := 'DELETE FROM main WHERE comp_id='+IntToStr(compid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 query := 'DELETE FROM companies WHERE comp_id='+IntToStr(compid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 for i := 0 to StringGrid1.ColCount-1 do
  for k := 0 to StringGrid1.RowCount-1 do
   StringGrid1.Cells[i,k] := '';

 companies.Free;
 products.Free;

 FormCreate(Sender);
end;

procedure TForm1.ermktrlse1Click(Sender: TObject);
var frm: TForm3;
    i,k: integer;
    modal_result: integer;
    query: string;
    presults: pmysql_res;
    row: mysql_row;
    prodid: integer;
begin
//termék törlése
 frm := TForm3.Create(Self);
 for i := 2 to StringGrid1.ColCount-1 do
  frm.ListBox1.Items.Add(StringGrid1.Cells[i,0]);

 frm.ListBox1.ItemIndex := -1;

 modal_result := frm.ShowModal;

 if modal_result = mrCancel then
  Exit;

 if frm.ListBox1.ItemIndex = -1 then
 begin
  Application.MessageBox('Ki kell választani egy terméket a törléshez!','Fárszer',mb_IconError+mb_Ok);
  Exit;
 end;

 query := 'SELECT prod_id FROM products WHERE prod_name="'+frm.ListBox1.Items[frm.ListBox1.ItemIndex]+'";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  row := mysql_fetch_row(presults)^;
  prodid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 query := 'DELETE FROM main WHERE prod_id='+IntToStr(prodid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 query := 'DELETE FROM products WHERE prod_id='+IntToStr(prodid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 for i := 0 to StringGrid1.ColCount-1 do
  for k := 0 to StringGrid1.RowCount-1 do
   StringGrid1.Cells[i,k] := '';

 companies.Free;
 products.Free;

 FormCreate(Sender);
end;

procedure TForm1.Keress1Click(Sender: TObject);
var frm: TForm5;
    query: string;
    frm3: TForm3;
    presults: pmysql_res;
    row: mysql_row;
    prow: pmysql_row;
    i: integer;
    tempdate: TDateTime;
    ev,honap,nap: word;
    ora,perc,masodperc,millisec: word;
    frm6: TForm6;
    tli: TListItem;

begin
 frm := TForm5.Create(Self);

 frm.PageControl1.ActivePage := frm.TabSheet1;

 if frm.ShowModal = mrCancel then
 begin
  frm.Free;
  Abort;
 end;

 if frm.PageControl1.ActivePage = frm.TabSheet1 then //companies
 begin
  query := 'SELECT comp_name FROM companies WHERE ';

  if frm.CheckBox1.Checked then
   query := query + 'comp_name LIKE ''%' + frm.Edit1.Text + '%'' AND ';
  if frm.CheckBox2.Checked then
   query := query + 'comp_addr LIKE ''%' + frm.Edit2.Text + '%'' AND ';
  if frm.CheckBox3.Checked then
   query := query + 'comp_telnum LIKE ''%' + frm.Edit3.Text + '%'' AND ';
  if frm.CheckBox4.Checked then
   query := query + 'comp_faxnum LIKE ''%' + frm.Edit4.Text + '%'' AND ';
  if frm.CheckBox5.Checked then
   query := query + 'comp_contact LIKE ''%' + frm.Edit5.Text + '%'' AND ';
  if frm.CheckBox6.Checked then
   query := query + 'comp_vatnum LIKE ''%' + frm.Edit6.Text + '%'' AND ';
  if frm.CheckBox7.Checked then
   query := query + 'comp_owner LIKE ''%' + frm.Edit7.Text + '%'' AND ';
  if frm.CheckBox8.Checked then
   query := query + 'comp_email LIKE ''%' + frm.Edit8.Text + '%'' AND ';
  if frm.CheckBox9.Checked then
   query := query + 'comp_homepage LIKE ''%' + frm.Edit9.Text + '%'' AND ';

  delete(query,length(query) - length(' AND ')+1,length(' AND '));
  query := StringReplace(query,'%%','%',[rfReplaceAll]);

  {$IFDEF DEBUG}
   Application.MessageBox(PChar(query),'',mb_OK);
  {$ENDIF}
// end;
//  frm.Free;

  frm3 := TForm3.Create(Self);
  frm3.Caption := 'Keresés eredménye';
  frm3.BitBtn1.Hide;
  frm3.BitBtn2.Left := frm3.BitBtn1.Left;
  frm3.BitBtn2.Width := frm3.Width - 2*frm3.BitBtn2.Left;
  frm3.BitBtn2.Kind := bkOK;
  frm3.BitBtn2.Caption := 'Bezár';
  frm3.ListBox1.OnDblClick := frm3.ListBoxDblClick;

  presults:= nil;
  try
   mysql_query(@mysqlrec, PChar(query));
   presults:= mysql_store_result(@mysqlrec);
   frm3.ListBox1.Items.Clear;
   if presults^.row_count > 0 then
    for i:= 1 to presults^.row_count do
    begin
     prow:= mysql_fetch_row(presults);
     row:= prow^;
     frm3.ListBox1.Items.Add(row[0]);
    end;

  finally
   mysql_free_result(presults);
  end;
  frm3.ShowModal;

  frm3.Free;
 end
 else
 if frm.PageControl1.ActivePage = frm.TabSheet2 then //products
 begin
  query := 'SELECT prod_name FROM products WHERE ';
  query := query + ' prod_name LIKE ''%' + frm.Edit10.Text + '%'';';

  query := StringReplace(query,'%%','%',[rfReplaceAll]);

  {$IFDEF DEBUG}
   Application.MessageBox(PChar(query),'',mb_OK);
  {$ENDIF}
// end;
  frm.Free;

  frm3 := TForm3.Create(Self);
  frm3.Caption := 'Keresés eredménye';
  frm3.BitBtn1.Hide;
  frm3.BitBtn2.Left := frm3.BitBtn1.Left;
  frm3.BitBtn2.Width := frm3.Width - 2*frm3.BitBtn2.Left;
  frm3.BitBtn2.Kind := bkOK;
  frm3.BitBtn2.Caption := 'Bezár';
  frm3.ListBox1.OnDblClick := frm3.ListBoxDblClick;

  presults:= nil;
  try
   mysql_query(@mysqlrec, PChar(query));
   presults:= mysql_store_result(@mysqlrec);
   frm3.ListBox1.Items.Clear;
   if presults^.row_count > 0 then
    for i:= 1 to presults^.row_count do
    begin
     prow:= mysql_fetch_row(presults);
     row:= prow^;
     frm3.ListBox1.Items.Add(row[0]);
    end;

  finally
   mysql_free_result(presults);
  end;
  frm3.ListBox1.OnDblClick := nil;
  frm3.ShowModal;

  frm3.Free;
 end
 else
 if frm.PageControl1.ActivePage = frm.TabSheet3 then //main
 begin
  query := 'SELECT companies.comp_name, products.prod_name, main.status FROM main main inner join companies companies ';
  query := query + 'on (companies.comp_id=main.comp_id) inner join products products on ';
  query := query + '(products.prod_id=main.prod_id) WHERE';

  if frm.CheckBox10.Checked then
   query := query + ' status LIKE ''%' + frm.Edit11.Text + '%'' AND ';
  if frm.CheckBox11.Checked then
   query := query + ' memo LIKE ''%' + frm.Edit12.Text + '%'' AND ';
  if frm.CheckBox12.Checked then
  begin
   query := query + ' memodate ';
   if frm.Button3.Caption = 'újabb, mint' then
    query := query + '> '
   else
    query := query + '< ';
   tempdate := frm.DateTimePicker1.Date;
   decodedate(tempdate,ev,honap,nap);
   tempdate := frm.DateTimePicker2.Time;
   decodetime(tempdate,ora,perc,masodperc,millisec);

 //   2005-04-07 15:38:35
   query := query + '''' + IntToStr(ev) + '-' + IntToStr(honap) + '-' + IntToStr(nap) + ' ';
   query := query + IntToStr(ora) + ':' + IntToStr(perc) + ':' + IntToStr(masodperc) + ''' AND ';
  end;
  if frm.CheckBox13.Checked then
  begin
   query := query + ' todo=';
   if frm.CheckBox14.Checked then
    query := query + '1 AND '
   else
    query := query + '0 AND ';
  end;
  delete(query,length(query) - length(' AND ')+1,length(' AND '));
  query := StringReplace(query,'%%','%',[rfReplaceAll]);

  {$IFDEF DEBUG}
   Application.MessageBox(PChar(query)),'debug',mb_Ok);
  {$ENDIF}

  frm6 := TForm6.Create(Self);
  frm6.Caption := 'Keresés eredménye';

  presults:= nil;
  try
   mysql_query(@mysqlrec, PChar(query));
   presults:= mysql_store_result(@mysqlrec);
   frm6.ListView1.Items.Clear;
   if presults^.row_count > 0 then
    for i:= 1 to presults^.row_count do
    begin
     prow:= mysql_fetch_row(presults);
     row:= prow^;
     tli := frm6.ListView1.Items.Add;
     tli.SubItems.Add(row[0]);
     tli.SubItems.Add(row[1]);
    end;

  finally
   mysql_free_result(presults);
  end;

  frm6.ShowModal;
  frm6.Free;
 end;
end;

procedure TForm1.Cgtneve1Click(Sender: TObject);
var frm: TForm3;
    modal_result: integer;
    query: string;
    presults: pmysql_res;
    row: mysql_row;
    compid: integer;
    newname: string;
    i,k: integer;
begin
 //cég átnevezése
 frm := TForm3.Create(Self);
 for i := 1 to StringGrid1.RowCount-1 do
  frm.ListBox1.Items.Add(StringGrid1.Cells[1,i]);

 frm.ListBox1.ItemIndex := -1;
 frm.BitBtn1.Caption := 'Átnevezés';

 modal_result := frm.ShowModal;

 if modal_result = mrCancel then
  Exit;

 if frm.ListBox1.ItemIndex = -1 then
 begin
  Application.MessageBox('Ki kell választani egy céget az átnevezéshez!','Fárszer',mb_IconError+mb_Ok);
  Exit;
 end;

 query := 'SELECT comp_id FROM companies WHERE comp_name="'+frm.ListBox1.Items[frm.ListBox1.ItemIndex]+'";';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 presults := nil;
 try
  mysql_query(@mysqlrec,PChar(query));
  presults := mysql_store_result(@mysqlrec);
  row := mysql_fetch_row(presults)^;
  compid := StrToInt(row[0]);
 finally
  mysql_free_result(presults);
 end;

 newname := InputBox('farszer','Új név:',frm.ListBox1.Items[frm.ListBox1.ItemIndex]);

 query := 'UPDATE companies SET comp_name="' + newname + '" WHERE comp_id='+IntToStr(compid)+';';
 {$IFDEF DEBUG}Application.MessageBox(PChar(query),'',mb_OK);{$ENDIF}
 mysql_query(@mysqlrec,PChar(query));

 for i := 0 to StringGrid1.ColCount-1 do
  for k := 0 to StringGrid1.RowCount-1 do
   StringGrid1.Cells[i,k] := '';

 companies.Free;
 products.Free;

 FormCreate(Sender);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 dec(TimeTillRefresh);
 Form1.Caption := 'A táblázat frissítéséig hátralevõ idõ: ' + IntToStr(TimeTillRefresh) + 'mp.';
 if TimeTillRefresh = 0 then
 begin
  TimeTillRefresh := 60;
  Form1.Caption := 'Frissítés...';
  FormCreate(Sender);
  Form1.Caption := 'Frissítés kész.';
 end;
end;

end.
