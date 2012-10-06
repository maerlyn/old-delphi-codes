unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ADODB, Gauges, StdCtrls;

type
  TForm1 = class(TForm)
    Gauge1: TGauge;
    OpenDialog1: TOpenDialog;
    ADOConnection1: TADOConnection;
    ADOTable1: TADOTable;
    ADOTable2: TADOTable;
    ADOTable3: TADOTable;
    Gauge2: TGauge;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Atalakitas;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Atalakitas;
var temp: string;
begin
 Memo1.Lines.Clear;
 Memo1.Lines.Add('<html>');
 Memo1.Lines.Add('	<head>');
 Memo1.Lines.Add('		<meta http-equiv="Content-Language" content="hu">');
 Memo1.Lines.Add('		<meta http-equiv="Content-Type" content="text/html; charset=windows-1250">');

 Gauge1.Progress := 0;
 Gauge2.Progress := 0;

 ADOTable1.First;
 while not ADOTable1.Eof do
 begin
  Gauge1.Progress := Gauge1.Progress + 1;
  Memo1.Lines.Add('<title>'+ADOTable1.FieldByName('szerv').AsString+'</title');Gauge2.Progress := 1;
  Memo1.Lines.Add('	</head>');
  Memo1.Lines.Add('<body>');
  Memo1.Lines.Add('	<table border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="83%" id="AutoNumber1">');
  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="87%" colspan="2"><i><b><font size="4">'+ADOTable1.FieldByName('szerv').AsString+'</font></b></i></td></tr>');Gauge2.Progress := 2;
  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="12%">'+ADOTable1.FieldByName('inf-szam').AsString+'</td>');Gauge2.Progress := 3;
  Memo1.Lines.Add('			<td width="88%"><i><b>'+ADOTable1.FieldByName('iranyitoszam').AsString+' '+ADOTable1.FieldByName('helyseg').AsString+', '+ADOTable1.FieldByName('utca').AsString+'</b></i></td></tr>');Gauge2.Progress := 4;
  Memo1.Lines.Add('	</table>');
  Memo1.Lines.Add('	<p><i><b><font size="4">parancsnok:</font></b></i></p>');
  Memo1.Lines.Add('	<table border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="83%" id="AutoNumber2">');
  Memo1.Lines.Add('		<tr>');

  ADOTable2.Locate('vez-szint;szerv-kod;',VarArrayOf(['P',ADOTable1.FieldByName('szerv-kod').AsString]),[]);
  Memo1.Lines.Add('			<td width="12%"></td>');Gauge2.Progress := 7;
  Memo1.Lines.Add('			<td width="3%"><i><b>'+ADOTable2.FieldByName('elo').AsString+'</b></i></td>');Gauge2.Progress := 7;
  Memo1.Lines.Add('			<td width="85%"><i><b>'+ADOTable2.FieldByName('nev').AsString+' '+ADOTable2.FieldByName('rendfokozat').AsString+'</b></i></td></tr>');Gauge2.Progress := 8;
  Memo1.Lines.Add('	</table>');
  Memo1.Lines.Add('	&nbsp;');
  Memo1.Lines.Add('	<table border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="83%" id="AutoNumber3">');
  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="28%">&nbsp;</td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>vezetékes</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>mobil</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>BM</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>BV</b></i></td></tr>');
  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="32%">&nbsp;</td>');

  ADOTable3.Locate('szerv-kod',ADOTable1.FieldByName('szerv-kod').AsString,[]);

        temp := '			<td width="18%" valign="top" align="center">';
        if ADOTable3.FieldByName('pk-kozvetlen-1').AsString <> '' then
         temp := temp + ADOTable3.FieldByName('pk-kozvetlen-1').AsString;
        if ADOTable3.FieldByName('pk-kozvetlen-2').AsString <> '' then
         temp := temp + ',<br>' + ADOTable3.FieldByName('pk-kozvetlen-2').AsString + '</td>';
  Memo1.Lines.Add(temp);Gauge2.Progress := 10;

  Memo1.Lines.Add('			<td width="18%" valign="top" align="center">'+ADOTable3.FieldByName('pk-mobil').AsString+'</td>');Gauge2.Progress := 11;
  Memo1.Lines.Add('			<td width="18%" valign="top" align="center">&nbsp;</td>');
  Memo1.Lines.Add('			<td width="18%" valign="top" align="center">'+ADOTable3.FieldByName('pk-bv').AsString+'</td></tr>');Gauge2.Progress := 12;
  Memo1.Lines.Add('	</table>');
  Memo1.Lines.Add('	&nbsp;');
  Memo1.Lines.Add(#13#10);
  Memo1.Lines.Add('	<table border="0" cellpadding="0" cellspacing="0" style="border-collapse: collapse" bordercolor="#111111" width="83%" id="AutoNumber4">');
  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="28%" align="right"><i><b>központ:</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>vezetékes</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>mobil</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>BM</b></i></td>');
  Memo1.Lines.Add('			<td width="18%" align="center"><i><b>BV</b></i></td></tr>');

  if (ADOTable3.FieldByName('gyorshivo').AsString<>'')or(ADOTable3.FieldByName('gyorshivo-bm').AsString<>'') then
  begin
   Memo1.Lines.Add('		<tr>');
   Memo1.Lines.Add('			<td width="28%" align="right"><i><b>gyorshívó</b></i></td>');
   Memo1.Lines.Add('			<td width="18%" align="center">*7'+ADOTable3.FieldByName('gyorshivo').AsString+'</td>');Gauge2.Progress := 14;
   Memo1.Lines.Add('			<td width="18%" align="center">&nbsp;</td>');
   Memo1.Lines.Add('			<td width="18%" align="center">*7'+ADOTable3.FieldByName('gyorshivo-bm').AsString+'</td>');Gauge2.Progress := 15;
   Memo1.Lines.Add('			<td width="18%" align="center">&nbsp;</td></tr>');
  end;

  Memo1.Lines.Add('		<tr>');
  Memo1.Lines.Add('			<td width="28%">&nbsp;</td>');
        temp := '			<td width="18%" valign="top" align="center">';
        if ADOTable3.FieldByName('kozpont-1').AsString <> '' then
         temp := temp + ADOTable3.FieldByName('kozpont-1').AsString;
	if ADOTable3.FieldByName('kozpont-2').AsString <> '' then
	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-2').AsString;
	if ADOTable3.FieldByName('kozpont-3').AsString <> '' then
	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-3').AsString;
	if ADOTable3.FieldByName('kozpont-4').AsString <> '' then
	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-4').AsString;
	if ADOTable3.FieldByName('kozpont-5').AsString <> '' then
	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-5').AsString;
	if ADOTable3.FieldByName('kozpont-6').AsString <> '' then
 	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-6').AsString;
	if ADOTable3.FieldByName('kozpont-7').AsString <> '' then
	 temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-7').AsString + '</td>';
  Memo1.Lines.Add(temp);Gauge2.Progress := 16;

        temp := '			<td width="18%" valign="top" align="center">';
        if ADOTable3.FieldByName('kozpont-mobil-1').AsString <> '' then
         temp := temp + ADOTable3.FieldByName('kozpont-mobil-1').AsString;
        if ADOTable3.FieldByName('kozpont-mobil-2').AsString <> '' then
         temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-mobil-2').AsString + '</td>';
  Memo1.Lines.Add(temp);Gauge2.Progress := 17;

        temp := '			<td width="18%" valign="top" align="center">';
        if ADOTable3.FieldByName('pk-bm-1').AsString <> '' then
         temp := temp + ADOTable3.FieldByName('pk-bm-1').AsString;
        if ADOTable3.FieldByName('pk-bm-2').AsString <> '' then
         temp := temp + ',<br>' + ADOTable3.FieldByName('pk-bm-2').AsString + '</td>';
  Memo1.Lines.Add(temp);Gauge2.Progress := 18;

        temp := '			<td width="18%" valign="top" align="center">';
        if ADOTable3.FieldByName('kozpont-bv-1').AsString <> '' then
         temp := temp + ADOTable3.FieldByName('kozpont-bv-1').AsString;
        if ADOTable3.FieldByName('kozpont-bv-2').AsString <> '' then
         temp := temp + ',<br>' + ADOTable3.FieldByName('kozpont-bv-2').AsString + '</td></tr>';
  Memo1.Lines.Add(temp);Gauge2.Progress := 19;

  Memo1.Lines.Add('	</table>');
  Memo1.Lines.Add('<p>&nbsp;</p>');
  Memo1.Lines.Add('</body>');
  Memo1.Lines.Add('</html>');

  Memo1.Lines.SaveToFile(ExtractFilePath(OpenDialog1.FileName) + ADOTable1.FieldByName('filenev').AsString + '.htm');
  Gauge2.Progress := 0;
  Gauge1.Progress := Gauge1.Progress + 1;

  Memo1.Lines.Clear;

  ADOTable1.Next;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var temp: string;
    t: DWORD;
begin
 if not OpenDialog1.Execute then
  Application.Terminate;

 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Data Source=' + OpenDialog1.FileName + ';';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Persist Security Info=False';

 ADOConnection1.ConnectionString := temp;

 ADOTable1.TableName := 'Szervek';
 ADOTable2.TableName := 'Vezetok';
 ADOTable3.TableName := 'Telefonszamok';

 ADOConnection1.Open;
 ADOTable1.Open;
 ADOTable2.Open;
 ADOTable3.Open;

 Gauge1.MaxValue := ADOTable1.RecordCount;
 Gauge2.MaxValue := 28;

 Form1.Show;

 t := GetTickCount;
 Atalakitas;
 t := GetTickCount - t;
 Application.MessageBox(PChar('Kész.'#13#10'Összes idő: '+IntToStr(trunc(t/1000))+' máodperc'#13#10+'Rekordszám: '+IntToStr(Gauge1.MaxValue)+#13#10#13#10+'1 rekord '+FloatToStr((t/1000)/Gauge1.MaxValue)+' másodperc alatt.'),'',mb_Ok + mb_IconInformation);

 Application.Terminate;
end;

end.
