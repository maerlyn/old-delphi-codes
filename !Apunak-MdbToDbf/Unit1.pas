unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ADODB, Db, DBTables, StdCtrls, ComCtrls, Gauges, OleServer, Excel97;

type
  TForm1 = class(TForm)
    Table1: TADOTable;
    ADOConnection1: TADOConnection;
    ADOTable1: TADOTable;
    OpenDialog1: TOpenDialog;
    ADOConnection2: TADOConnection;
    ProgressBar1: TGauge;
    ExcelApplication1: TExcelApplication;
    procedure FormCreate(Sender: TObject);
    function Masolas(Honnan, Hova: string): boolean;
  private
    procedure Feldolg_BV;
    procedure Feldolg_VFon;
    procedure Feldolg_HTML_BV;
    procedure Feldolg_XLS_VFon;
    procedure Feldolg_XLS_BV;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  t: cardinal;
  XLSSorokSzama: integer;

implementation

{$R *.DFM}

procedure TForm1.Feldolg_BV;
var temp_mellek                          : string;
    temp_fovonal                         : string;
    temp_datum                           : TDate;
    temp_ido                             : string;
    temp_datumido                        : TDateTime;
    temp_idotartam                       : string;
    temp_idotartam_ora                   : integer;
    temp_idotartam_perc                  : integer;
    temp_idotartam_masodp                : integer;
    temp_hivottszam                      : string;
    temp_dij                             : single;
    temp_dij_s                           : string;
    i                                    : integer;
    temp                                 : string;
    Ev,Honap,Nap,Ora,Perc,Masodp,Ezredmp : word;
begin
 if not Masolas(ExtractFilePath(Application.ExeName)+'Null-BV.dbf',ExtractFilePath(OpenDialog1.FileName)+ChangeFileExt(ExtractFileName(OpenDialog1.FileName),'.dbf')) then
 begin
  Application.MessageBox('Nem találom a Null-BV.dbf-t!','Hiba',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;

 Form1.Show;
 temp := ExtractFileName(OpenDialog1.FileName);
 temp := ChangeFileExt(temp,'');
 Table1.Close;
 Table1.TableName := temp;

 ADOTable1.Open;
 Table1.Open;
 ADOTable1.First;
 Table1.First;
 i := 0;
 ProgressBar1.MaxValue := ADOTable1.RecordCount;

try try
 while not ADOTable1.Eof do
 begin
  inc(i);
  ProgressBar1.Progress := i;
  temp_mellek := IntToStr(ADOTable1.FieldByName('MELLEK').AsInteger);
  temp_fovonal := IntToStr(ADOTable1.FieldByName('TRUNK').AsInteger);
  temp_datumido := ADOTable1.FieldByName('DATUM').AsDateTime;
  temp_idotartam := IntToStr(ADOTable1.FieldByName('IDOTARTAM_SEC').AsInteger);

  temp_idotartam_masodp := ADOTable1.FieldByName('IDOTARTAM_SEC').AsInteger;
  temp_idotartam_perc := temp_idotartam_masodp div 60;
  temp_idotartam_ora := temp_idotartam_perc div 60;
  temp_idotartam_masodp := temp_idotartam_masodp - temp_idotartam_ora * 60 * 60 - temp_idotartam_perc * 60;
  if Length(IntToStr(temp_idotartam_ora)) = 1 then
   temp_idotartam := '0' + IntToStr(temp_idotartam_ora) + ':'
  else
   temp_idotartam := IntToStr(temp_idotartam_ora) + ':';
  if Length(IntToStr(temp_idotartam_perc)) = 1 then
   temp_idotartam := temp_idotartam + '0' + IntToStr(temp_idotartam_perc) + ':'
  else
   temp_idotartam := temp_idotartam + IntToStr(temp_idotartam_perc) + ':';
  if Length(IntToStr(temp_idotartam_masodp)) = 1 then
   temp_idotartam := temp_idotartam + '0' + IntToStr(temp_idotartam_masodp)
  else
   temp_idotartam := temp_idotartam + IntToStr(temp_idotartam_masodp);

  if Length(temp_mellek) > 5 then temp_mellek := Copy(temp_mellek,1,5);
  if Length(temp_fovonal) > 4 then temp_fovonal := Copy(temp_fovonal,1,4);

{//  temp_datumido_s := DateTimeToStr(temp_datumido);
  temp_datumido_s := FormatDateTime('yyyy. mm. dd. hh:nn',temp_datumido);
  temp_datum := StrToDate(Copy(temp_datumido_s,1,13));
  temp_ido := Copy(temp_datumido_s,15,4);}

//-----------------------------------------------------------------------------
 DecodeDate(temp_datumido,Ev,Honap,Nap);
 DecodeTime(temp_datumido,Ora,Perc,Masodp,Ezredmp);
 temp_datum := EncodeDate(Ev,Honap,Nap);

 if Length(IntToStr(Ora)) = 1 then
  temp_ido := '0' + IntToStr(Ora) + ':'
 else
  temp_ido := IntToStr(Ora) + ':';

 if Length(IntToStr(Perc)) = 1 then
  temp_ido := temp_ido + '0' + IntToStr(Perc) + ':'
 else
  temp_ido := temp_ido + IntToStr(Perc) + ':';

 if Length(IntToStr(Masodp)) = 1 then
  temp_ido := temp_ido + '0' + IntToStr(Masodp)
 else
  temp_ido := temp_ido + IntToStr(Masodp);
//-----------------------------------------------------------------------------

  while Pos(' ',temp_ido) < 0 do
   Delete(temp_ido,Pos(' ',temp_ido),1);

  if Length(Copy(temp_ido,1,Pos(':',temp_ido)-1)) = 1 then
   temp_ido := '0' + temp_ido;
  if Length(Copy(temp_ido,Pos(':',temp_ido)+1,2)) = 1 then
   temp_ido := Copy(temp_ido,1,Pos(':',temp_ido)) + '0' + Copy(temp_ido,Pos(':',temp_ido)+1,1);

  temp := Copy(temp_ido,Pos(':',temp_ido)+1,Length(temp_ido));
  if Pos(':',temp) = 0 then
   temp_ido := temp_ido + ':00'
  else if Length(Copy(temp,Pos(':',temp),Length(temp))) = 1 then
   temp_ido := temp_ido + ':0' + Copy(temp,Pos(':',temp),Length(temp))
  else
   temp_ido := temp_ido + ':' + Copy(temp,Pos(':',temp),Length(temp));

  temp_hivottszam := ADOTable1.FieldByName('HIVOTTSZAM').AsString;
  if Length(temp_hivottszam) > 19 then
   temp_hivottszam := Copy(temp_hivottszam,1,19);

  temp_dij := ADOTable1.FieldByName('DIJ').AsCurrency;
  temp_dij_s := FloatToStr(temp_dij);
  if Length(Copy(temp_dij_s,Pos(',',temp_dij_s),10)) > 1 then
   temp_dij_s := Copy(temp_dij_s,1,Pos(',',temp_dij_s)-1) + ',' + Copy(temp_dij_s,Pos(',',temp_dij_s) + 1,1);

  Table1.InsertRecord([temp_mellek,temp_datum,temp_ido,temp_idotartam,temp_hivottszam,temp_dij,temp_fovonal]);

  ADOTable1.Next;

  Form1.Caption := 'Mdb -> Dbf ' + IntToStr(trunc((ProgressBar1.Progress/ProgressBar1.MaxValue)*100)) + '%';
  Application.Title := Form1.Caption;
 end;
except
 on E:Exception do
  begin
   Application.MessageBox(pchar('Hiba! '+e.message),'',mb_ok);
   Exit;
  end;
end;
finally
// ADOTable1.Close;
 Table1.close;
end;
end;

procedure TForm1.Feldolg_HTML_BV;
var temp, temp2                          : string;
    HTMLSorokSzama                       : integer;
    HTMLFile                             : TStringList;
    temp_mellek                          : string;
    temp_datumido_s                      : string;
    temp_datum                           : TDate;
    temp_ido_s                           : string;
    temp_tartam                          : string;
    temp_dij_s                           : string;
    temp_dij_f                           : single;
    temp_fovonal                         : string;
    temp_hivottsza                       : string;
    Ev,Honap,Nap                         : word;
begin
 temp2 := OpenDialog1.FileName;
 temp2 := ExtractFilePath(temp2);
 Delete(temp2,Length(temp2),1);

{ temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'User ID=Admin;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE Files;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DefaultDir=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection1.ConnectionString := temp;}

 temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE fájlok;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection1.ConnectionString := temp;

 if not Masolas(ExtractFilePath(ParamStr(0)) + 'Null-BV.dbf',ChangeFileExt(OpenDialog1.FileName,'.dbf')) then
 begin
  Application.MessageBox('Nem találom a Null-BV.dbf-t!','Hiba',mb_Ok + mb_IconAsterisk);
  Abort;
 end;

 ADOTable1.TableName := Copy(ExtractFileName(OpenDialog1.FileName),1,Pos('.',ExtractFileName(OpenDialog1.FileName))-1);
 ADOTable1.Open;

 HTMLFile := TStringList.Create;
 HTMLFile.LoadFromFile(OpenDialog1.FileName);

 ProgressBar1.MaxValue := HTMLFile.Count;
 Form1.Show;

 for HTMLSorokSzama := 0 to HTMLFile.Count - 1 do
 begin
  if Pos('<TR class=tb>',HTMLFile[HTMLSorokSzama]) = 1 then
  begin
   temp := Copy(HTMLFile[HTMLSorokSzama],Length('<TR class=tb><TD>')+1,Length(HTMLFile[HTMLSorokSzama]));
   temp_mellek := Copy(temp,1,Pos('<',temp)-1);
   Delete(temp,1,Pos('>',temp));
   temp_datumido_s := Copy(temp,1,Pos('<',temp)-1);
   Delete(temp,1,Pos('>',temp));
   temp_tartam := Copy(temp,1,Pos('<',temp)-1);
   Delete(temp,1,Pos('>',temp));
   temp_hivottsza := Copy(temp,1,Pos('<',temp)-1);
   Delete(temp,1,Pos('>',temp));
   Delete(temp,1,4);
   Delete(temp,1,Pos('>',temp));
   temp_dij_s := Copy(temp,1,Pos('<',temp)-1);

   Ev := StrToInt(Copy(temp_datumido_s,1,4));
   Honap := StrToInt(Copy(temp_datumido_s,6,2));
   Nap := StrToInt(Copy(temp_datumido_s,9,2));

   temp_datum := EncodeDate(Ev,Honap,Nap);
   temp_ido_s := Copy(temp_datumido_s,17,5);

   temp_dij_s := Copy(temp_dij_s,1,Pos('&',temp_dij_s)-1);
   temp_dij_f := StrToFloat(temp_dij_s);
   temp_fovonal := '9999';

   ADOTable1.InsertRecord([temp_mellek,temp_datum,temp_ido_s,temp_tartam,temp_hivottsza,temp_dij_f,temp_fovonal]);
 //példa (valójában 1 sor) 123456789012345678901
// <TR class=tb><TD>114<TD>2002.03.20&nbsp;09:30<TD>0:01:55<TD>0622454097
// <TD>Székesfehérvár<TD class=tj>52,00&nbsp;Ft</TD></TR>
  end;
  ProgressBar1.Progress := HTMLSorokSzama + 1;
  Form1.Caption := 'Htm -> Dbf ' + IntToStr(trunc((ProgressBar1.Progress / ProgressBar1.MaxValue)*100)) + '%';
  Application.Title := Form1.Caption;
  XLSSorokSzama := HTMLSorokSzama + 1;
 end;

 ADOTable1.Close;
end;

procedure TForm1.Feldolg_VFon;
var temp_hivo               : string;
    temp_hivott             : string;
    temp_hivaskezdete       : string;
    temp_tartam             : longint;
    temp_nettoosszeg        : currency;
    k                       : integer;
    i                       : integer;
    temp                    : string;
begin
 if not Masolas(ExtractFilePath(Application.ExeName)+'Null-VFon.dbf',ExtractFilePath(OpenDialog1.FileName)+ChangeFileExt(ExtractFileName(OpenDialog1.FileName),'.dbf')) then
 begin
  Application.MessageBox('Nem találom a Null-VFon.dbf-t!','Hiba',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;

 Form1.Show;
 temp := ExtractFileName(OpenDialog1.FileName);
 temp := ChangeFileExt(temp,'');
 Table1.Close;
 Table1.TableName := temp;

 ProgressBar1.MaxValue := ADOTable1.RecordCount;

 ADOTable1.Open;
 Table1.Open;
 ADOTable1.First;
 Table1.First;
 k := 0;

try try
while not(ADOTable1.Eof) do
begin
 inc(k);
 temp_hivo := ADOTable1.FieldByName('Hívó').AsString;
 temp_hivaskezdete := ADOTable1.FieldByName('Hívás kezdete').AsString;
 temp_hivott := ADOTable1.FieldByName('Hívott').AsString;
 temp := ADOTable1.FieldByName('Tartam').AsString;
 i := Pos(':',temp);
 temp_tartam := 60 * StrToInt(Copy(temp,1,i-1)) + StrToInt(Copy(temp,i+1,Length(temp)));
 temp := ADOTable1.FieldByName('Nettó összeg').AsString;
 while Pos('.',temp) > 0 do
  Delete(temp,Pos('.',temp),1);
 temp_nettoosszeg := StrToCurr(temp);

 Table1.InsertRecord([temp_hivo,temp_hivaskezdete,temp_hivott,temp_tartam,temp_tartam,temp_nettoosszeg]);

 ADOTable1.Next;
 ProgressBar1.Progress := ProgressBar1.Progress + 1;

 Form1.Caption := 'Mdb -> Dbf ' + IntToStr(trunc((ProgressBar1.Progress/ProgressBar1.MaxValue)*100)) + '%';
 Application.Title := Form1.Caption;
end;
except
 on E: Exception do
 begin
  Application.MessageBox(pchar('Hiba a(z) ' + IntToStr(k) + '. rekordban: '+e.message),'',mb_ok);
  Exit;
 end;
end;
finally
 Table1.Close;
end;
end;

procedure TForm1.Feldolg_XLS_BV;
var temp, temp2                          : string;
    i                                    : integer;
    temp_mellek                          : string;
    temp_hivas_datu                      : TDateTime;
    temp_hivas_idop                      : string;
    temp_hivas_tart                      : TDateTime;
    temp_hivas_tart_s                    : string;
    temp_hivas_tart_f                    : double;
    temp_hivott_sza                      : string;
    temp_dij                             : single;
    temp_dij_s                           : string;
    temp_fovonal                         : string;
    Ev,Honap,Nap,Ora,Perc,Masodp,Ezredmp : word;
begin
 temp2 := OpenDialog1.FileName;
 temp2 := ExtractFilePath(temp2);
 Delete(temp2,Length(temp2),1);

{ temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'User ID=Admin;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE Files;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DefaultDir=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection2.ConnectionString := temp;}

 temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE fájlok;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection2.ConnectionString := temp;

 if not Masolas(ExtractFilePath(Application.ExeName)+'Null-BV.dbf',ExtractFilePath(OpenDialog1.FileName)+ChangeFileExt(ExtractFileName(OpenDialog1.FileName),'.dbf')) then
 begin
  Application.MessageBox('Nem találom a Null-BV.dbf-t!','Hiba',mb_Ok + mb_IconAsterisk);
  Abort;
 end;

 Table1.TableName := Copy(ExtractFileName(OpenDialog1.FileName),1,Pos('.',ExtractFileName(OpenDialog1.FileName))-1);
 Table1.Open;

 i := 1;
 repeat
  inc(i);
  temp := ExcelApplication1.Range['A' + IntToStr(i),'A' + IntToStr(i)].Value;
 until temp = '';

 Form1.Show;
 ProgressBar1.MaxValue := i;

 XLSSorokSzama := 2;
 repeat
  temp_mellek := ExcelApplication1.Range['A'+IntToStr(XLSSorokSzama),'A'+IntToStr(XLSSorokSzama)].Value;
  temp_hivas_datu := StrToDateTime(ExcelApplication1.Range['B'+IntToStr(XLSSorokSzama),'B'+IntToStr(XLSSorokSzama)].Value);
  temp_hivas_tart_s := ExcelApplication1.Range['C'+IntToStr(XLSSorokSzama),'C'+IntToStr(XLSSorokSzama)].Value;
  temp_hivott_sza := ExcelApplication1.Range['D'+IntToStr(XLSSorokSzama),'D'+IntToStr(XLSSorokSzama)].Value;
  temp_dij_s := ExcelApplication1.Range['F'+IntToStr(XLSSorokSzama),'F'+IntToStr(XLSSorokSzama)].Value;
  temp_fovonal := '9999';

  DecodeDate(temp_hivas_datu,Ev,Honap,Nap);
  DecodeTime(temp_hivas_datu,Ora,Perc,Masodp,Ezredmp);
  temp_hivas_datu := EncodeDate(Ev,Honap,Nap);
  temp_hivas_idop := '';
  if Length(IntToStr(Ora)) = 1 then
   temp_hivas_idop := '0';
  temp_hivas_idop := temp_hivas_idop + IntToStr(Ora) + ':';
  if Length(IntToStr(Perc)) = 1 then
   temp_hivas_idop := temp_hivas_idop + '0';
  temp_hivas_idop := temp_hivas_idop + IntToStr(Perc) + ':';
  if Length(IntToStr(Masodp)) = 1 then
   temp_hivas_idop := temp_hivas_idop + '0';
  temp_hivas_idop := temp_hivas_idop + IntToStr(Masodp);

  Delete(temp_dij_s,Length(temp_dij_s)-2,3);
  while Pos(' ',temp_dij_s) > 0 do
   Delete(temp_dij_s,Pos(' ',temp_dij_s),1);
  if Pos('.',temp_dij_s) > 0 then
  begin
   if Length(Copy(temp_dij_s,Pos('.',temp_dij_s)+1,Length(temp_Dij_s))) > 1 then
    temp_dij_s := Copy(temp_dij_s,1,Pos('.',temp_dij_s)-1) + '.' + Copy(temp_dij_s,Pos('.',temp_dij_s)+1,1);
  end;
  temp_dij := StrToFloat(temp_dij_s);

  temp_hivas_tart_f := StrToFloat(temp_hivas_tart_s);
  temp_hivas_tart := temp_hivas_tart_f;
  DecodeTime(temp_hivas_tart,Ora,Perc,Masodp,Ezredmp);
  temp_hivas_tart_s := '';
  if Length(IntToStr(Ora)) = 1 then
   temp_hivas_tart_s := '0';
  temp_hivas_tart_s := temp_hivas_tart_s + IntToStr(Ora) + ':';
  if Length(IntToStr(Perc)) = 1 then
   temp_hivas_tart_s := temp_hivas_tart_s + '0';
  temp_hivas_tart_s := temp_hivas_tart_s + IntToStr(Perc) + ':';
  if Length(IntToStr(Masodp)) = 1 then
   temp_hivas_tart_s := temp_hivas_tart_s + '0';
  temp_hivas_tart_s := temp_hivas_tart_s + IntToStr(Masodp);

  Table1.InsertRecord([temp_mellek,temp_hivas_datu,temp_hivas_idop,temp_hivas_tart_s,temp_hivott_sza,temp_dij,temp_fovonal]);

  inc(XLSSorokSzama);
  ProgressBar1.Progress := XLSSorokSzama;
  Form1.Caption := 'Xls -> Dbf ' + IntToStr(trunc((ProgressBar1.Progress / ProgressBar1.MaxValue)*100)) + '%';
  Application.Title := Form1.Caption;
  temp := ExcelApplication1.Range['A'+IntToStr(XLSSorokSzama),'A'+IntToStr(XLSSorokSzama)].Value;
 until temp = '';

 XLSSorokSzama := XLSSorokSzama - 2;
 Table1.Close;

 ExcelApplication1.Quit;
end;

procedure TForm1.Feldolg_XLS_VFon;
var temp, temp2             : string;
    i                       : integer;
    temp_hivo               : string;
    temp_hivaskezdete       : string;
    temp_hivott             : string;
    temp_tartam             : longint;
    temp_nettoosszeg        : currency;
    temp_hivasfelepitesidij : currency;
    temp_dij                : currency;
    temp_tartam_s           : string;
begin
 temp2 := OpenDialog1.FileName;
 temp2 := ExtractFilePath(temp2);
 Delete(temp2,Length(temp2),1);

 {temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'User ID=Admin;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE Files;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DefaultDir=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection2.ConnectionString := temp;}

 temp := 'Provider=MSDASQL.1;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Extended Properties="DSN=dBASE fájlok;';
 temp := temp + 'DBQ=' + temp2 + ';';
 temp := temp + 'DriverId=277;';
 temp := temp + 'MaxBufferSize=2048;';
 temp := temp + 'PageTimeout=5;";';
 temp := temp + 'Initial Catalog=' + temp2;
 ADOConnection2.ConnectionString := temp;

 if not Masolas(ExtractFilePath(Application.ExeName)+'Null-VFon.dbf',ExtractFilePath(OpenDialog1.FileName)+ChangeFileExt(ExtractFileName(OpenDialog1.FileName),'.dbf')) then
 begin
  Application.MessageBox('Nem találom a Null-VFon.dbf-t!','Hiba',mb_Ok + mb_IconAsterisk);
  Abort;
 end;

 Table1.TableName := Copy(ExtractFileName(OpenDialog1.FileName),1,Pos('.',ExtractFileName(OpenDialog1.FileName))-1);
 Table1.Open;

// ExcelApplication1.Visible[0] := false;
// ExcelApplication1.Workbooks.Open(OpenDialog1.FileName,null,null,null,null,null,null,null,null,null,null,null,null,0);

 i := 1;
 repeat
  inc(i);
  temp := ExcelApplication1.Range['A'+IntToStr(i),'A'+IntToStr(i)].Value;
 until temp = '';
 i := i - 2;

 Form1.Show;

 ProgressBar1.MaxValue := i;

 XLSSorokSzama := 2;
 repeat //amíg nem talál üres sort, addíg megy
  temp_hivo := ExcelApplication1.Range['A'+IntToStr(XLSSorokSzama),'A'+IntToStr(XLSSorokSzama)].Value;
  temp_hivott := ExcelApplication1.Range['B'+IntToStr(XLSSorokSzama),'B'+IntToStr(XLSSorokSzama)].Value;
  temp_hivaskezdete := ExcelApplication1.Range['C'+IntToStr(XLSSorokSzama),'C'+IntToStr(XLSSorokSzama)].Value;
  temp_tartam_s := ExcelApplication1.Range['D'+IntToStr(XLSSorokSzama),'D'+IntToStr(XLSSorokSzama)].Value;
  temp := ExcelApplication1.Range['F'+IntToStr(XLSSorokSzama),'F'+IntToStr(XLSSorokSzama)].Value;
  while pos('.',temp) > 0 do
   delete(temp,pos('.',temp),1);
  temp_nettoosszeg := StrToFloat(temp);
  temp := ExcelApplication1.Range['G'+IntToStr(XLSSorokSzama),'G'+IntToStr(XLSSorokSzama)].Value;
  temp_hivasfelepitesidij := StrToFloat(temp);

  if temp_hivo = '25251009' then
   temp_hivo := '994'
  else if temp_hivo = '25285928' then
   temp_hivo := '993'
  else
   temp_hivo := '';

  if Pos(':',temp_tartam_s) > 0 then
   temp_tartam := 60*StrToInt(Copy(temp_tartam_s,1,Pos(':',temp_tartam_s)-1)) + StrToInt(Copy(temp_tartam_s,Pos(':',temp_tartam_s)+1,Length(temp_tartam_s)))
  else
   temp_tartam := StrToInt(temp_tartam_s);

  temp_dij := temp_nettoosszeg;// + temp_hivasfelepitesidij;

  Table1.InsertRecord([temp_hivo,temp_hivaskezdete,temp_hivott,temp_tartam,temp_tartam,temp_dij]);

  inc(XLSSorokSzama);
  ProgressBar1.Progress := XLSSorokSzama;
  Form1.Caption := 'Xls -> Dbf ' + IntToStr(trunc((ProgressBar1.Progress / Progressbar1.MaxValue)*100)) + '%';
  Application.Title := Form1.Caption;
  temp := ExcelApplication1.Range['A'+IntToStr(XLSSorokSzama),'A'+IntToStr(XLSSorokSzama)].Value;
 until temp = '';

 XLSSorokSzama := XLSSorokSzama - 2;
 Table1.Close;

 ExcelApplication1.Quit;
end;

procedure TForm1.FormCreate(Sender: TObject);
var FD            : TFieldDefs;
    i             : integer;
    hSysMenu,nCnt : longint;
    temp,temp2    : string;
begin
 OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
 if not OpenDialog1.Execute then
  Halt;

if OpenDialog1.FileName <> '' then
begin
//az 'X' gomb kikapcsolása
 hSysMenu := GetSystemMenu(Form1.Handle,false); //a rendszermenü lekérdezése
 if hSysMenu <> 0 then //ha van, akkor
 begin
  nCnt := GetMenuItemCount(hSysMenu); //hány eleme van,
  if nCnt <> 0 then //ha van eleme, akkor
  begin
   RemoveMenu(hSysMenu,nCnt - 1,mf_ByPosition + mf_Remove); //leszedjük a Bezárást, ezzel a gombor is kikapcsoljuk,
   RemoveMenu(hSysMenu,nCnt - 2,mf_ByPosition + mf_Remove); //de elõtte van egy elválasztó is
   DrawMenuBar(Form1.Handle); //erõltetjük az újrarajzolást, ezzel látszik is a mûvünk.
  end;
 end;
//kész, de ezt a tálcai rendszermenübõl is kiszedjük
 hSysMenu := GetSystemMenu(Application.Handle,false);
 if hSysMenu <> 0 then
 begin
  nCnt := GetMenuItemCount(hSysMenu);
  if nCnt <> 0 then
  begin
   RemoveMenu(hSysMenu,nCnt - 1,mf_ByPosition + mf_Remove);
   RemoveMenu(hSysMenu,nCnt - 2,mf_ByPosition + mf_Remove);
  end;
 end;
//kész

 if ExtractFileExt(OpenDialog1.FileName) = '.xls' then
 begin
  ExcelApplication1.Visible[0] := false;
  ExcelApplication1.Workbooks.Open(OpenDialog1.FileName,null,null,null,null,null,null,null,null,null,null,null,null,0);
  if ExcelApplication1.Range['A1','A1'].Value = 'Hívó' then
  begin
   t := GetTickCount;
   Feldolg_XLS_VFon;
  end
  else
  begin
   t := GetTickCount;
   Feldolg_XLS_BV;
  end;
 end
 else
 if Copy(ExtractFileExt(OpenDialog1.FileName),1,4) = '.htm' then
 begin
  t := GetTickCount;
  Feldolg_HTML_BV;
 end
 else
 begin
  temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
  temp := temp + 'Password="";';
  temp := temp + 'User ID=Admin;';
  temp := temp + 'Data Source=';
  temp := temp + OpenDialog1.FileName + ';';
  temp := temp + 'Mode=ReadWrite;';
  temp := temp + 'Extended Properties="";';
  temp := temp + 'Jet OLEDB:System database="";';
  temp := temp + 'Jet OLEDB:Registry Path="";';
  temp := temp + 'Jet OLEDB:Database Password="";';
  temp := temp + 'Jet OLEDB:Engine Type=4;';
  temp := temp + 'Jet OLEDB:Database Locking Mode=0;';
  temp := temp + 'Jet OLEDB:Global Partial Bulk Ops=2;';
  temp := temp + 'Jet OLEDB:Global Bulk Transactions=1;';
  temp := temp + 'Jet OLEDB:New Database Password="";';
  temp := temp + 'Jet OLEDB:Create System Database=False;';
  temp := temp + 'Jet OLEDB:Encrypt Database=False;';
  temp := temp + 'Jet OLEDB:Don''t Copy Locale on Compact=False;';
  temp := temp + 'Jet OLEDB:Compact Without Replica Repair=False;';
  temp := temp + 'Jet OLEDB:SFP=False';
  ADOConnection1.ConnectionString := temp;

  temp2 := OpenDialog1.FileName;
  temp2 := ExtractFilePath(temp2);
  Delete(temp2,Length(temp2),1);

{  temp := 'Provider=MSDASQL.1;';
  temp := temp + 'Password="";';
  temp := temp + 'Persist Security Info=True;';
  temp := temp + 'User ID=Admin;';
  temp := temp + 'Mode=ReadWrite;';
  temp := temp + 'Extended Properties="DSN=dBASE Files;';
  temp := temp + 'DBQ=' + temp2 + ';';
  temp := temp + 'DefaultDir=' + temp2 + ';';
  temp := temp + 'DriverId=533;';
  temp := temp + 'MaxBufferSize=2048;';
  temp := temp + 'PageTimeout=5;";';
  temp := temp + 'Initial Catalog=' + temp2;
  ADOConnection2.ConnectionString := temp;}

  temp := 'Provider=MSDASQL.1;';
  temp := temp + 'Password="";';
  temp := temp + 'Persist Security Info=True;';
  temp := temp + 'Mode=ReadWrite;';
  temp := temp + 'Extended Properties="DSN=dBASE fájlok;';
  temp := temp + 'DBQ=' + temp2 + ';';
  temp := temp + 'DriverId=277;';
  temp := temp + 'MaxBufferSize=2048;';
  temp := temp + 'PageTimeout=5;";';
  temp := temp + 'Initial Catalog=' + temp2;
  ADOConnection2.ConnectionString := temp;

  ADOTable1.Open;

  FD := ADOTable1.FieldDefs;
  for i := 0 to FD.Count - 1 do
   if LowerCase(FD[i].Name) = LowerCase('PINCODE') then
    begin
     t := gettickcount;
     Feldolg_BV;
     XLSSorokSzama := ADOTable1.RecordCount;
    end
   else if LowerCase(FD[i].Name) = LowerCase('Hívó') then
    begin
     t := gettickcount;
     Feldolg_VFon;
     XLSSorokSzama := ADOTable1.RecordCount;
    end;
 end;

t := gettickcount-t;
t := t div 1000;
Application.MessageBox(pchar('OK!'#13#10'Rekordok száma: '+inttostr(XLSSorokSzama)+' db'#13#10'Idõtartam: '+floattostr(t)+' sec'#13#10'1 rekord '+floattostr(t/XLSSorokSzama)+' sec alatt,'#13#10'1 sec alatt '+floattostr(XLSSorokSzama/t)+' rekord.'),'',mb_Ok + mb_IconInformation);
ADOTable1.Close;
Application.Terminate;
end;
end;

function TForm1.Masolas(Honnan, Hova: string): boolean;
var Stream1, Stream2: TFileStream;
begin
 if not FileExists(Honnan) then
 begin
  Result := false;
  Abort;
 end;
 Stream1 := TFileStream.Create(Honnan,fmOpenRead);
 try
  Stream2 := TFileStream.Create(Hova,fmOpenWrite or fmCreate);
  try
   Stream2.CopyFrom(Stream1,Stream1.Size);
  finally
   Stream2.Free;
  end;
 finally
  Stream1.Free;
 end;
 Result := true;
end;
end.

