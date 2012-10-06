unit CDKolcsonadasa;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ComCtrls, ButtonWithColor, dbf;

type
  TfrmCDKolcsonadasa = class(TForm)
    lstKolcsonkerok: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    lstCDk: TListBox;
    Label3: TLabel;
    Button1: TBitBtnWithColor;
    Button3: TBitBtnWithColor;
    ADOTable1: TDbf;
    dtpKolcsonadasDatuma: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCDKolcsonadasa: TfrmCDKolcsonadasa;

implementation

uses MainForm;

{$R *.DFM}

procedure TfrmCDKolcsonadasa.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;
 Button1.Color := frmMainForm.Gombok;
 Button3.Color := frmMainForm.Gombok;

 lstKolcsonkerok.Font.Color := frmMainForm.Betuk;
 lstKolcsonkerok.Canvas.Font.Color := frmMainForm.Betuk;
 Label1.Font.Color := frmMainForm.Betuk;
 Label1.Canvas.Font.Color := frmMainForm.Betuk;
 Label2.Font.Color := frmMainForm.Betuk;
 Label2.Canvas.Font.Color := frmMainForm.Betuk;
 lstCDk.FOnt.Color := frmMainForm.Betuk;
 lstCDk.Canvas.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.Font.Color := frmMainForm.Betuk;
 Button1.Font.Color := frmMainForm.Betuk;
 Button3.Font.Color := frmMainForm.Betuk;
 dtpKolcsonadasDatuma.Font.Color := frmMainForm.Betuk;

 ADOTable1.FilePath := ExtractFilePath(ParamStr(0)) + 'Data';

 lstKolcsonkerok.Items.Clear;
 lstCDk.Items.Clear;
 lstCDk.Items.LoadFromFile(ADOTable1.FilePath + '\CDk.cdny');

 ADOTable1.TableName := 'CDk';
 ADOTable1.Open;
 ADOTable1.First;
 while not ADOTable1.Eof do
 begin
  if ADOTable1.FieldByName('KolcsonVan').AsBoolean = false then
   lstCDk.Items.Add(ADOTable1.FieldByName('CDNeve').AsString);
  ADOTable1.Next;
  Application.ProcessMessages;
 end;
 ADOTable1.Close;

 dtpKolcsonadasDatuma.Date := Now;
end;

procedure TfrmCDKolcsonadasa.Button1Click(Sender: TObject);
var CDNeve, Kolcsonkero: string;
begin

 if lstKolcsonkerok.ItemIndex >= 0 then
  Kolcsonkero := lstKolcsonkerok.Items[lstKolcsonkerok.ItemIndex]
 else
  raise Exception.Create('Ki kell választani, hogy kinek adod kölcsön!');

 if lstCDk.ItemIndex >= 0 then
  CDNeve := lstCDk.Items[lstCDk.ItemIndex]
 else
  raise Exception.Create('Ki kell választani, hogy melyik CDt adod kölcsön!');

 lstKolcsonkerok.Enabled := false;
 lstCDk.Enabled := false;
 dtpKolcsonadasDatuma.Enabled := false;
 Button1.Enabled := false;
 Button3.Enabled := false;

 ADOTable1.TableName := 'CDk';
 ADOTable1.Open;
 ADOTable1.Locate('CDNeve',CDNeve,[]);
 if ADOTable1.FieldByName('KolcsonVan').AsBoolean = true then
 begin
  Application.MessageBox('Ez a CD már kölcsön van adva!','CD-nyilvántartó',mb_Ok + mb_IconExclamation);
  Exit;
 end;
 ADOTable1.Edit;
 ADOTable1.FieldByName('KolcsonVan').AsBoolean := true;
 ADOTable1.FieldByName('Datum').AsDateTime := dtpKolcsonadasDatuma.Date;
 ADOTable1.FieldByName('Kolcsonker').AsString := Kolcsonkero;
 ADOTable1.Post;

 ADOTable1.Close;

 lstCDk.Items.Delete(lstCDk.Items.IndexOf(CDNeve));

 lstKolcsonkerok.Enabled := true;
 lstCDk.Enabled := true;
 dtpKolcsonadasDatuma.Enabled := true;
 Button1.Enabled := true;
 Button3.Enabled := true;
// Application.MessageBox('Kész.','CD-nyilvántartó',mb_Ok + mb_IconInformation);
end;

procedure TfrmCDKolcsonadasa.Button3Click(Sender: TObject);
begin
 Self.Close;
end;

procedure TfrmCDKolcsonadasa.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
 frmCDKolcsonadasa := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmCDKolcsonadasa.FormActivate(Sender: TObject);
begin
 ADOTable1.Close;
 ADOTable1.TableName := 'CDk';
 ADOTable1.Open;
 ADOTable1.First;
 lstCDk.Items.Clear;
 while not ADOTable1.Eof do
 begin
  if ADOTable1.FieldByName('KolcsonVan').AsBoolean = false then
   lstCDk.Items.Add(ADOTable1.FieldByName('CDNeve').AsString);
  ADOTable1.Next;
 end;
 ADOTable1.Close;

 lstKolcsonkerok.Items.Clear;
 lstKolcsonkerok.Items.LoadFromFile(ADOTable1.FilePath + '\Kolcsonkerok.cdny');
end;

end.
