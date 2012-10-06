{$DEFINE DEBUG}
library Office97;

uses
  SysUtils,
  Dialogs,
  Excel97,
  Word97,
  ADODB,
  OleServer;

var
  ExcelApplication1: TExcelApplication;
  WordApplication1: TWordApplication;
  WordDocument1: TWordDocument;
  ADOTable1: TADOTable;
  ADOConnection1: TADOConnection;

const
  KolcsonkeroMezokNevei: array[1..3] of string = ('Név','Cím','Telefonszám');
  CDMezokNevei: array[1..8] of string = ('CD Neve','Kategória','Pontos tartalom','Hely','Megjegyzés','Kölcsön van-e kérve?','Kölcsönkérés dátuma','Kölcsönkérõ');

{$R *.RES}

procedure Letrehoz(Utvonal: string);
var temp: string;
begin
{$IFDEF DEBUG}
 showmessage('letrehoz-eleje');
{$ENDIF}
 ADOConnection1 := ADOConnection1.Create(nil);
 if ADOConnection1.Connected then
  ADOConnection1.Close;
 temp := 'Provider=Microsoft.Jet.OLEDB.4.0;';
 temp := temp + 'Password="";';
 temp := temp + 'Persist Security Info=True;';
 temp := temp + 'Data Source=' + Utvonal + ';';
 temp := temp + 'Mode=Read|Write';
 ADOConnection1.ConnectionString := temp;
 ADOConnection1.LoginPrompt := false;
 ADOConnection1.Open;

 ADOTable1 := TADOTable.Create(nil);
 ADOTable1.Connection := ADOConnection1;

 WordApplication1 := TWordApplication.Create(nil);
 WordDocument1 := TWordDocument.Create(nil);
 ExcelApplication1 := TExcelApplication.Create(nil);
{$IFDEF DEBUG}
 showmessage('letrehoz-vege');
{$ENDIF}
end;

procedure Megsemmisit;
begin
 if ADOConnection1.Connected then
  ADOConnection1.Close;

 if ADOTable1.Active then
  ADOTable1.Close;

 FreeAndNil(ADOConnection1);
 FreeAndNil(ADOTable1);
end;

procedure WordExport(Utvonal, Tabla: string);stdcall;
var U, T: string;
    RangeW: Word97.Range;
    v1: variant;
    ov1: OleVariant;
    Row1: Word97.Row;
    tempstr: string;
begin
{$IFDEF DEBUG}
 showmessage('word-eleje');
{$ENDIF}

 U := (Utvonal);
 T := (Tabla);

 Letrehoz(U);
 ADOTable1.TableName := T;
 ADOTable1.Open;

 WordApplication1.Visible := true;
 WordDocument1.PageSetup.Orientation := 1;
 WordDocument1.Activate;

 if U = 'Kolcsonkerok' then
 begin
  WordDocument1.Range.Text := 'Kölcsönkérõk';
  WordDocument1.Range.Font.Size := 14;
  ADOTable1.TableName := 'Kolcsonkerok';
  ADOTable1.Open;
  ADOTable1.First;
  while not ADOTable1.Eof do
  begin
   WordDocument1.Range.InsertParagraphAfter;
   WordDocument1.Paragraphs.Last.Range.Text := ADOTable1.FieldByName('Nev').AsString + #9 + ADOTable1.FieldByName('Cim').AsString + #9 + ADOTable1.FieldByName('Telszam').AsString;
   ADOTable1.Next;
  end;
  ADOTable1.Close;
  RangeW := WordDocument1.Content;
  v1 := RangeW;
  v1.ConvertToTable(#9,19,3);
  Row1 := WordDocument1.Tables.Item(1).Rows.Get_First;
  Row1.Range.Bold := 1;
  Row1.Range.Font.Size := 30;
  Row1.Range.InsertParagraphAfter;
  ov1 := ' ';
  Row1.ConvertToText(ov1);
 end else
 if U = 'CDk' then
 begin
  WordDocument1.Range.Text := 'CDk';
  WordDocument1.Range.Font.Size := 14;
  ADOTable1.TableName := 'CDk';
  ADOTable1.Open;
  ADOTable1.First;
  while not ADOTable1.Eof do
  begin
   WordDocument1.Range.InsertParagraphAfter;
   tempstr := ADOTable1.FieldByName('CDNeve').AsString + #9 + ADOTable1.FieldByName('Tartalom').AsString + #9;
   tempstr := tempstr + ADOTable1.FieldByName('PontosTartalom').AsString + #9;
   tempstr := tempstr + ADOTable1.FieldByName('Hely').AsString + #9;
   tempstr := tempstr + ADOTAble1.FieldByName('Megjegyzes').AsString + #9;
   if ADOTable1.FieldByName('KolcsonVanEKerve').AsBoolean then
    tempstr := tempstr + 'kölcsönadva' + #9
   else
    tempstr := tempstr + 'nincs kölcsönadva' + #9;
   tempstr := tempstr + DateToStr(ADOTable1.FieldByName('KolcsonkeresDatuma').AsDateTime) + #9;
   tempstr := tempstr + ADOTable1.FieldByName('Kolcsonkero').AsString;
   WordDocument1.Paragraphs.Last.Range.Text := tempstr;
   ADOTable1.Next;
  end;
  ADOTable1.Close;
  RangeW := WordDocument1.Content;
  v1 := RangeW;
  v1.ConvertToTable(#9,19,8);
  Row1 := WordDocument1.Tables.Item(1).Rows.Get_First;
  Row1.Range.Bold := 1;
  Row1.Range.Font.Size := 30;
  Row1.Range.InsertParagraphAfter;
  ov1 := ' ';
  Row1.ConvertToText(ov1);
 end;

 ADOTable1.Close;
 Megsemmisit;
end;

procedure ExcelExport(Utvonal, Tabla: string);stdcall;
var U, T: string;
    RangeE: Excel97.Range;
    I, Row: integer;
begin
 U := Utvonal;
 T := Tabla;

 Letrehoz(U);

 ExcelApplication1.Visible[0] := true;
 ExcelApplication1.Workbooks.Add(null,0);

 if T = 'Kolcsonkerok' then
 begin
  RangeE := ExcelApplication1.ActiveCell;
  ADOTable1.TableName := 'Kolcsonkerok';
  ADOTable1.Open;
  RangeE := ExcelApplication1.ActiveCell;
  for i := 1 to 3 do
  begin
   RangeE.Value := KolcsonkeroMezokNevei[i];
   RangeE := RangeE.Next;
  end;
  try
   ADOTable1.First;
   Row := 2;
   while not ADOTable1.Eof do
   begin
    RangeE := ExcelApplication1.Range['A' + IntToStr(Row),'A' + IntToStr(Row)];
    for i := 0 to ADOTable1.Fields.Count - 1 do
    begin
     RangeE.Value := ADOTable1.Fields[i].AsString;
     RangeE := RangeE.Next;
    end;
    ADOTable1.Next;
    Inc(Row);
   end;
  finally
   ADOTable1.Close;
  end;
  RangeE := ExcelApplication1.Range['A1','C' + IntToStr(Row - 1)];
  RangeE.AutoFormat(3,null,null,null,null,null,null);
 end
 else if T = 'CDk' then
 begin
  ADOTable1.TableName := 'CDk';
  ADOTable1.Open;
  RangeE := ExcelApplication1.ActiveCell;
  for i := 1 to 8 do
  begin
   RangeE.Value := CDMezokNevei[i];
   RangeE := RangeE.Next;
  end;
  try
   ADOTable1.First;
   Row := 2;
   while not ADOTable1.Eof do
   begin
    RangeE := ExcelApplication1.Range['A' + IntToStr(Row),'A' + IntToStr(Row)];
    for i := 0 to ADOTable1.Fields.Count - 1 do
    begin
     if i <> 5 then
      RangeE.Value := ADOTable1.Fields[i].AsString
     else
     begin
      if ADOTable1.Fields[i].AsBoolean then
       RangeE.Value := 'igen'
      else
       RangeE.Value := 'nem';
     end;
     RangeE := RangeE.Next;
    end;
    ADOTable1.Next;
    Inc(Row);
   end;
  finally
   ADOTable1.Close;
  end;
  RangeE := ExcelApplication1.Range['A1','H' + IntToStr(Row - 1)];
  RangeE.AutoFormat(3,null,null,null,null,null,null);
 end;

 Megsemmisit;
end;

exports
 ExcelExport, WordExport;

begin
 {
  Ez a DLL a CD-Nyilvántartóhoz (c 2001-2002 Putra Ware, http://putraware.ini.hu) készült, az Office 97-be exportáló
  függvényeket tartalmazza. Ha ezt a DLLt a fõprogram megtalálja, engedélyez még 2 gombot az eszköztáron, amikkel
  O97-be lehet exportálni az adatbázist.
 }
end.

