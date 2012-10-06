unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, dbf, Db, ADODB, FileCtrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    ADOTable1: TADOTable;
    Dbf1: TDbf;
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
{$R 'res.res'}

procedure TForm1.Button1Click(Sender: TObject);
var temp: string;
    a: TResourceStream;
    s: string;
    lst: TStringList;
begin
 if not OpenDialog1.Execute then
 begin
  Application.MessageBox('Ki kell választani egy filet az átalakításhoz!','Átalakító',mb_Ok + mb_IconExclamation);
  Abort;
 end;


 s := ExtractFilePath(OpenDialog1.FileName) + 'Data';
 ForceDirectories(s);
 a := TResourceStream.Create(HInstance,'C1','CUSTOM');
 a.SaveToFile(s + '\CDk.cdny');
 a.Free;
 a := TResourceStream.Create(HInstance,'C2','CUSTOM');
 a.SaveToFile(s + '\Kolcsonkerok.cdny');
 a.Free;
 a := TResourceStream.Create(HInstance,'C3','CUSTOM');
 a.SaveToFile(s + '\CDk.dbf');
 a.Free;
 a := TResourceStream.Create(HInstance,'C4','CUSTOM');
 a.SaveToFile(s + '\Kolcsonkerok.dbf');
 a.Free;
 a := TResourceStream.Create(HInstance,'C5','CUSTOM');
 a.SaveToFile(s + '\CDk.dbt');
 a.Free;

 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Data Source=' + OpenDialog1.FileName + ';';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Persist Security Info=False';
 ADOTable1.ConnectionString := temp;

 Dbf1.FilePath := s;
 Dbf1.TableName := 'Kolcsonkerok';

 try
  try
   Screen.Cursor := crHourGlass;

   ADOTable1.TableName := 'CDk';
   Dbf1.TableName := 'CDk';
   ADOTable1.Open;
   Dbf1.Open;

   lst := TStringList.Create;

   ADOTable1.First;
   while not ADOTable1.Eof do
   begin
    Dbf1.Append;
    Dbf1.AppendRecord([ADOTable1.FieldByName('CDNeve').AsString,
                       ADOTable1.FieldByName('Tartalom').AsString,
                       ADOTable1.FieldByName('PontosTartalom').AsString,
                       ADOTable1.FieldByName('Hely').AsString,
                       ADOTable1.FieldByName('Megjegyzes').AsString,
                       ADOTable1.FieldByName('KolcsonVanEKerve').AsBoolean,
                       ADOTable1.FieldByName('KolcsonkeresDatuma').AsDateTime,
                       ADOTable1.FieldByName('Kolcsonkero').AsString]);
    Dbf1.Append;
    lst.Add(ADOTable1.FieldByName('CDNeve').AsString);
    ADOTable1.Next;
   end;
   lst.SaveToFile(s + '\CDk.cdny');
   lst.Clear;
   ADOTable1.Close;

{   Dbf1.First;
   while not Dbf1.Eof do
   begin
    if Dbf1.FieldByName('CDNeve').AsString = 'Putra Ware programok gyûjteménye' then
    begin
     Dbf1.Delete;
     Break;
    end;
    Dbf1.Next;
   end;}

   Dbf1.Close;

   ADOTable1.TableName := 'Kolcsonkerok';
   Dbf1.TableName := 'Kolcsonkerok';
   ADOTable1.Open;
   Dbf1.Open;

   ADOTable1.First;
   while not ADOTable1.Eof do
   begin
    Dbf1.Append;
    Dbf1.AppendRecord([ADOTable1.FieldByName('Nev').AsString,
                       ADOTable1.FieldByName('Cim').AsString,
                       ADOTable1.FieldByName('Telszam').AsString]);
    lst.Add(ADOTable1.FieldByName('Nev').AsString);
    ADOTable1.Next;
   end;
   lst.SaveToFile(s + '\Kolcsonkerok.cdny');
   lst.Free;
   ADOTable1.CLose;

{   Dbf1.First;
   while not Dbf1.Eof do
   begin
    if Dbf1.FieldByName('Nev').AsString = 'Tele von Zsinór' then
    begin
     Dbf1.Delete;
     Break;
    end;
    Dbf1.Next;
   end;}

   Dbf1.Close;
  except
   Application.MessageBox('Az átalakítás közben hiba történt!','Átalakító',mb_Ok + mb_IconAsterisk);
  end;
 finally
  Screen.Cursor := crDefault;
 end;

 Application.MessageBox('Kész!','Átalakító',mb_Ok + mb_IconInformation);
end;

end.
