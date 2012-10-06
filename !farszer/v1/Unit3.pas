unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TForm3 = class(TForm)
    ListBox1: TListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  public
    procedure ListBoxDblClick(Sender: TObject);
  private
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation
 uses Unit1, _libmysq, Unit4;

{$R *.dfm}

{ TForm3 }

procedure TForm3.ListBoxDblClick(Sender: TObject);
var presults: pmysql_res;
//    prow: pmysql_row;
    row: mysql_row;
    query: string;
    frm2: TForm4;
    compid: integer;
begin
 query := 'SELECT * FROM companies WHERE comp_name=''' + ListBox1.Items[ListBox1.ItemIndex] + ''';';
 mysql_query(@(Form1.mysqlrec),PChar(query));
 frm2 := TForm4.Create(Self);

 try
  query := 'SELECT comp_id FROM companies WHERE comp_name="'+ListBox1.Items[ListBox1.ItemIndex]+'";';
  presults := nil;
  try
   mysql_query(@Form1.mysqlrec,PChar(query));
   presults := mysql_store_result(@Form1.mysqlrec);
   row := mysql_fetch_row(presults)^;
   compid := StrToInt(row[0]);
  finally
   mysql_free_result(presults);
  end;

  query := 'SELECT comp_id,comp_name,comp_addr,comp_telnum,comp_faxnum,comp_contact,comp_vatnum,';
  query := query + 'comp_owner,comp_email,comp_homepage FROM companies WHERE comp_id=' + IntToStr(compid) + ';';
  presults := nil;
  try
   mysql_query(@Form1.mysqlrec,PChar(query));
   presults := mysql_store_result(@Form1.mysqlrec);
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

  if frm2.ShowModal = mrOK then
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
   mysql_query(@Form1.mysqlrec,PChar(query));
  end;
 finally
  frm2.Free;
 end;
end;

end.
