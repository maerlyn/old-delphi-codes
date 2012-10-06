unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Gauges, CDNyDataFile, Db, ADODB, StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ADOTable1: TADOTable;
    CDNyDataFile_CDk1: TCDNyDataFile_CDk;
    CDNyDataFile_Kolcsonkerok1: TCDNyDataFile_Kolcsonkerok;
    OpenDialog1: TOpenDialog;
    Gauge1: TGauge;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var temp: string;
    i: integer;
begin
 if not OpenDialog1.Execute then
  Abort;

 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Data Source=' + OpenDialog1.FileName + ';';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Persist Security Info=False';
 ADOTable1.ConnectionString := temp;

 ADOTable1.TableName := 'CDk';

 ADOTable1.Open;
 Gauge1.MaxValue := ADOTable1.RecordCount;
 CDNyDataFile_CDk1.Mennyiseg := 0;
 i := 0;

 while not ADOTable1.Eof do
 begin
  Inc(i);
  with CDNyDataFile_CDk1 do
  begin
   SetIndex(i,sa1CDNeve,ADOTable1.FieldByName('CDNeve').AsString);
   SetIndex(i,sa1Kategoria,ADOTable1.FieldByName('Tartalom').AsString);
   SetIndex(i,sa1Tartalom,ADOTable1.FieldByName('PontosTartalom').AsString);
   SetIndex(i,sa1Hely,ADOTable1.FieldByName('Hely').AsString);
   SetIndex(i,sa1Megjegyzes,ADOTable1.FieldByName('Megjegyzes').AsString);
   SetIndex(i,sa1KolcsonVan,ADOTable1.FieldByName('KolcsonVanEKerve').AsBoolean);
   SetIndex(i,sa1Datum,ADOTable1.FieldByName('KolcsonkeresDatuma').AsDateTime);
   SetIndex(i,sa1Kolcsonker,ADOTable1.FieldByName('Kolcsonkero').AsString);
   Mennyiseg := i;
  end;
  ADOTable1.Next;
  Gauge1.Progress := i;
 end;

 ForceDirectories(ExtractFilePath(OpenDialog1.FileName) + '\Data');
 CDNyDataFile_CDk1.SaveFile(ExtractFilePath(OpenDialog1.FileName) + '\Data\CDk.cdny');

 ADOTable1.Close;
 ADOTable1.TableName := 'Kolcsonkerok';
 ADOTable1.Open;
 ADOTable1.First;

 CDNyDataFile_Kolcsonkerok1.Mennyiseg := 0;
 i := 0;
 Gauge1.Progress := 0;
 Gauge1.MaxValue := ADOTable1.RecordCount;

 while not ADOTable1.Eof do
 begin
  inc(i);
  with CDNyDataFile_Kolcsonkerok1 do
  begin
   SetIndex(i,sa2Nev,ADOTable1.FieldByName('Nev').AsString);
   SetIndex(i,sa2Cim,ADOTable1.FieldByName('Cim').AsString);
   SetIndex(i,sa2Telszam,ADOTable1.FieldByName('Telszam').AsString);
   Mennyiseg := i;
  end;
  ADOTable1.Next;
  Gauge1.Progress := i;
 end;

 ADOTable1.Close;
 CDNyDataFile_Kolcsonkerok1.SaveFile(ExtractFilePath(OpenDialog1.FileName) + '\Data\Kolcsonkerok.cdny');

 Application.MessageBox('Az átalakítás kész.','Átalakító',mb_IconInformation + mb_Ok);
end;

end.
