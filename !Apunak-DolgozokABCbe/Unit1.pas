unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, ADODB, Gauges, StdCtrls;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Gauge1: TGauge;
    Gauge2: TGauge;
    ADOConnection1: TADOConnection;
    ADOTable1: TADOTable;
    ListBox1: TListBox;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Atalakitas;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  HasznaltBetuk: string = '';

const
 ABC: array[1..35] of string =('A','Á','B','C','D','E','É','F','G','H',
                               'I','Í','J','K','L','M','N','O','Ó','Ö',
                               'Õ','P','Q','R','S','T','U','Ú','Ü','Û',
                               'V','W','X','Y','Z');

implementation

{$R *.DFM}

procedure TForm1.Atalakitas;
var i,j: integer;
    temp: string;
begin
 for i := 1 to 35 do
 begin
  ADOTable1.First;
//  Memo1.Lines.Clear;
  ListBox1.Items.Clear;
  while not ADOTable1.Eof do
  begin
   if ADOTable1.FieldByName('nev').AsString[1] = ABC[i] then
    ListBox1.Items.Add(ADOTable1.FieldByName('nev').AsString);
   ADOTable1.Next;
  end;
  if ListBox1.Items.Count <> 0 then
  begin
   HasznaltBetuk := HasznaltBetuk + ABC[i];
   Memo1.Lines.Add('<a name="'+ABC[i]+'"><h1>'+ABC[i]+'</h1></a>');
   Memo1.Lines.Add('<table>');
   for j := 0 to ListBox1.Items.Count-1 do
   begin
    ADOTable1.Locate('nev',ListBox1.Items[j],[]);
    temp := temp + '<tr>';
    temp := temp + '<td width="15">'+ADOTable1.FieldByName('elo').AsString+'</td>';
    temp := temp + '<td width="250">'+ADOTable1.FieldByName('nev').AsString+'&nbsp;'+ADOTable1.FieldByName('rendfokozat').AsString+'</td>';
    temp := temp + '<td>'+ADOTable1.FieldByName('belso-1').AsString+'</td>';
    temp := temp + '<td>'+ADOTable1.FieldByName('belso-2').AsString+'</td>';
    temp := temp + '<td>'+ADOTable1.FieldByName('belso-3').AsString+'</td>';
    temp := temp + '<td>'+ADOTable1.FieldByName('belso-4').AsString+'</td>';
    temp := temp + '</tr>';
   end;
   Memo1.Lines.Add(temp+#13#10);
   temp := '';
   Memo1.Lines.Add('</table>');
   Gauge2.Progress := Gauge2.Progress + 1;
  end;
  Gauge1.Progress := Gauge1.Progress + 1;
 end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var temp: string;
    t: DWORD;
    i: integer;
begin
 if not OpenDialog1.Execute then
  Application.Terminate;

 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Data Source=' + OpenDialog1.FileName +';';
 temp := temp + 'Mode=ReadWrite;';
 temp := temp + 'Persist Security Info=False';

 ADOConnection1.ConnectionString := temp;

 ADOTable1.TableName := 'Dolgozok';

 ADOConnection1.Open;
 ADOTable1.Open;

 Gauge1.MaxValue := 35;
 Gauge2.MaxValue := ADOTable1.RecordCount;

 Form1.Show;

 Memo1.Lines.Clear;
 Memo1.Lines.Add('<html>');
 Memo1.Lines.Add('<title>'+ExtractFileName(OpenDialog1.FileName+'</title>'));


 t := GetTickCount;
 Atalakitas;
 t := GetTickCount - t;
 Application.MessageBox(PChar('Kész.'#13#10'Összes idõ: '+IntToStr(trunc(t/1000))+' máodperc'#13#10+'Rekordszám: '+IntToStr(Gauge1.MaxValue)+#13#10#13#10+'1 rekord '+FloatToStr((t/1000)/Gauge1.MaxValue)+' másodperc alatt.'),'',mb_Ok + mb_IconInformation);

 Memo1.Lines.Add('</html>');
 Memo1.Lines.SaveToFile(ExtractFilePath(OpenDialog1.FileName) + 'also.htm');
 Memo1.Lines.Clear;

 Memo1.Lines.Add('<html>');
 for i := 1 to Length(HasznaltBetuk) do
 begin
  Memo1.Lines.Add('<a href="also.htm#'+HasznaltBetuk[i]+'" target="also">'+HasznaltBetuk[i]+'</a>');
 end;
 Memo1.Lines.Add('</html>');
 Memo1.Lines.SaveToFile(ExtractFilePath(OpenDialog1.FileName) + 'felso.htm');


 Memo1.Lines.Clear;
 Memo1.Lines.Add('<html>');
 Memo1.Lines.Add('<frameset rows="50,*" border="0">');
 Memo1.Lines.Add('<frame src="felso.htm" scrolling="no">');
 Memo1.Lines.Add('<frame src="also.htm" name="also">');
 Memo1.Lines.Add('</frameset>');
 Memo1.Lines.Add('</html>');
 Memo1.Lines.SaveToFile(ExtractFilePath(OpenDialog1.FileName)+'index.html');

 Application.Terminate;
end;

end.
