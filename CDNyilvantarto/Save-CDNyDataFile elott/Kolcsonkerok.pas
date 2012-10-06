unit Kolcsonkerok;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, Db, StdCtrls, Mask, MainForm, OleCtrls,
  SRColBtn, ButtonWithColor, dbf, ADODB;

type
  TfrmKolcsonkerok = class(TForm)
    ADOConnection1: TADOConnection;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    DataSource1: TDataSource;
    ADOTable1: TDbf;
    Button1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    Button4: TBitBtnWithColor;
    txtCim: TEdit;
    txtTelszam: TEdit;
    cmbNev: TComboBox;
    ADOQuery1: TADOQuery;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button4Click(Sender: TObject);
    procedure GetData;
    procedure SendData;
    procedure cmbNevClick(Sender: TObject);
    procedure cmbNevKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure LoadList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmKolcsonkerok: TfrmKolcsonkerok;

implementation

{$R *.DFM}

procedure TfrmKolcsonkerok.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;
 Button1.Color := frmMainForm.Gombok;
 Button2.Color := frmMainForm.Gombok;
 Button4.Color := frmMainForm.Gombok;

 Label1.Font.Color := frmMainForm.Betuk;
 Label1.Canvas.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.FOnt.Color := frmMainForm.Betuk;
 Button1.Font.Color := frmMainForm.Betuk;
 Button2.Font.Color := frmMainForm.Betuk;
 Button4.Font.Color := frmMainForm.Betuk;
 txtCim.Font.Color := frmMainForm.Betuk;
 txtTelszam.Font.Color := frmMainForm.Betuk;
 cmbNev.Font.Color := frmMainForm.Betuk;

 ADOTable1.FilePath := ExtractFilePath(ParamStr(0)) + 'Data';

{ ADOTable1.Open;
 while not ADOTable1.Eof do
 begin
  cmbNev.Items.Add(ADOTable1.FieldByName('Nev').AsString);
  ADOTable1.Next;
  Application.ProcessMessages;
 end;}
 LoadList;
 ADOTable1.Open;

 cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmKolcsonkerok.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 cmbNev.Items.SaveToFile(ADOTable1.FilePath + '\Kolcsonkerok.cdny');

 Action := caFree;
 frmKolcsonkerok := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmKolcsonkerok.Button4Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmKolcsonkerok.GetData;
begin
 ADOTable1.Locate('Nev',cmbNev.Text,[]);
// cmbNev.Text := ADOTable1.FieldByName('Nev').AsString;
 txtCim.Text := ADOTable1.FieldByName('Cim').AsString;
 txtTelszam.Text := ADOTable1.FieldByName('Telszam').AsString;
end;

procedure TfrmKolcsonkerok.cmbNevClick(Sender: TObject);
begin
 GetData;
end;

procedure TfrmKolcsonkerok.cmbNevKeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
 begin
  if ADOTable1.Locate('Nev',cmbNev.Text,[]) then
   GetData
  else
  begin
   txtCim.Text := '';
   txtTelszam.Text := '';
   txtCim.SetFocus;
  end;
 end;
end;

procedure TfrmKolcsonkerok.SendData;
begin
 if cmbNev.Text = '' then
  raise Exception.Create('Meg kell adni egy nevet!');
 if ADOTable1.Locate('Nev',cmbNev.Text,[]) then
 begin
  ADOTable1.Edit;
  ADOTable1.FieldByName('Cim').AsString := txtCim.Text;
  ADOTable1.FieldByName('Telszam').AsString := txtTelszam.Text;
  ADOTable1.Post;
 end
 else
 begin
  ADOTable1.InsertRecord([cmbNev.Text,txtCim.Text,txtTelszam.Text]);
  cmbNev.Items.Add(cmbNev.Text);
 end;
end;

procedure TfrmKolcsonkerok.Button1Click(Sender: TObject);
begin
 SendData;
end;

procedure TfrmKolcsonkerok.Button2Click(Sender: TObject);
var Reply: integer;
begin
 Reply := Application.MessageBox('Tényleg tölöd az aktuális rekordot?','CD-nyilvántartó',mb_YesNo + mb_IconQuestion);
 if Reply = id_No then Exit;

 ADOTable1.Delete;
 ADOTable1.First;
 cmbNev.Items.Clear;
{ while not ADOTable1.Eof do
 begin
  cmbNev.Items.Add(ADOTable1.FieldByName('Nev').AsString);
  ADOTable1.Next;
 end;}

 cmbNev.Items.Delete(cmbNev.ItemIndex);

 cmbNev.ItemIndex := 0;
 GetData;
end;

procedure TfrmKolcsonkerok.LoadList;
begin
 cmbNev.Items.Clear;
 cmbNev.Items.LoadFromFile(ADOTable1.FilePath + '\Kolcsonkerok.cdny');
end;

end.
