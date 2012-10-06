unit CDKolcsonadasa;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ButtonWithColor, CDNyDataFile2, Toltes;

type
  TfrmCDKolcsonadasa = class(TForm)
    lstKolcsonkerok: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    lstCDk: TListBox;
    Label3: TLabel;
    Button1: TBitBtnWithColor;
    Button3: TBitBtnWithColor;
    dtpKolcsonadasDatuma: TDateTimePicker;
    CDNyDataFile_CDk1: TCDNyCDk;
    CDNyDataFile_Kolcsonkerok1: TCDNyKolcsonkerok;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LoadData;
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
 lstCDk.Font.Color := frmMainForm.Betuk;
 lstCDk.Canvas.Font.Color := frmMainForm.Betuk;
 Label3.Font.Color := frmMainForm.Betuk;
 Label3.Canvas.Font.Color := frmMainForm.Betuk;
 Button1.Font.Color := frmMainForm.Betuk;
 Button3.Font.Color := frmMainForm.Betuk;
 dtpKolcsonadasDatuma.Font.Color := frmMainForm.Betuk;

 lstKolcsonkerok.Items.Clear;
 lstCDk.Items.Clear;

// CDNyDataFile_CDk1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny');
// CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny');

// for i := 1 to CDNyDataFile_CDk1.Count do
//  if CDNyDataFile_CDk1.GetIndex(i).KolcsonVanEKerve  = false then
//   lstCDk.Items.Add(CDNyDataFile_CDk1.GetIndex(i).CDNeve);

 LoadData;

 lstKolcsonkerok.Items.Text := CDNyDataFile_Kolcsonkerok1.GetItems;

 dtpKolcsonadasDatuma.Date := Now;
end;

procedure TfrmCDKolcsonadasa.Button1Click(Sender: TObject);
var CDNeve, Kolcsonkero: string;
    j: integer;
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

 j := CDNyDataFile_CDk1.IndexOfItem(CDNeve);

 if CDNyDataFile_CDk1.GetIndex(j).KolcsonVanEKerve then
 begin
  Application.MessageBox('Ez a CD már kölcsön van adva!','CD-nyilvántartó',mb_Ok + mb_IconExclamation);
  Abort;
 end;
 CDNyDataFile_CDk1.SetBorrower(j,true,dtpKolcsonadasDatuma.Date,Kolcsonkero);

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
 frmToltes.Label1.Caption := 'CDk mentése';
 frmToltes.Show;
 CDNyDataFile_CDk1.SaveToFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny',frmToltes.Gauge1);
 frmToltes.Hide;
 Action := caFree;
 frmCDKolcsonadasa := nil;
 frmMainForm.RemoveWindow((Self as TForm).Handle,(Self as TForm));
end;

procedure TfrmCDKolcsonadasa.LoadData;
var i: integer;
begin
 frmToltes.Label1.Caption := 'CDk betöltése';
 frmToltes.Show;
 Sleep(5);
 CDNyDataFile_CDk1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\CDk.cdny',frmToltes.Gauge1);
 Sleep(5);
 frmToltes.Label1.Caption := 'Kölcsönkérõk betöltése';
 CDNyDataFile_Kolcsonkerok1.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\Data\Kolcsonkerok.cdny',frmToltes.Gauge1);
 Sleep(5);
 frmToltes.Label1.Caption := 'Nem kölcsönadott CDk válogatása';
 frmToltes.Gauge1.Progress := 0;
 frmToltes.Gauge1.MaxValue := CDNyDataFile_CDk1.Count;
 for i := 1 to CDNyDataFile_CDk1.Count do
 begin
  if CDNyDataFile_CDk1.GetIndex(i).KolcsonVanEKerve  = false then
   lstCDk.Items.Add(CDNyDataFile_CDk1.GetIndex(i).CDNeve);
  frmToltes.Gauge1.Progress := i;
 end;
 Sleep(5);
 frmToltes.Hide;
end;

end.
