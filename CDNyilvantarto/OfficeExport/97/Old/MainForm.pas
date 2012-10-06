unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ToolWin, ComCtrls, StdCtrls, Buttons, ExtCtrls, CDKolcsonadasa, CDVisszakapasa,
  ShellAPI, ActnList, Menus, Db, ADODB, IniFiles, Excel97, Word97, OleServer, Kerdes,
  Beallitasok, Kereses, About;

type
  TfrmMainForm = class(TForm)
    WordApplication1: TWordApplication;
    WordDocument1: TWordDocument;
    ExcelApplication1: TExcelApplication;
    ADOTable1: TADOTable;
    procedure cmdWordClick(Sender: TObject);
    procedure cmdExcelClick(Sender: TObject);
  end;

const
 KolcsonkeroMezokNevei: array[1..3] of string = ('Név','Cím','Telefonszám');
 CDMezokNevei: array[1..8] of string = ('CD Neve','Tartalom','Pontos tartalom','Hely','Megjegyzés','Kölcsön van-e kérve?','Kölcsönkérés dátuma','Kölcsönkérõ');

var
  frmMainForm: TfrmMainForm;

implementation

{$R *.DFM}

procedure TfrmMainForm.cmdWordClick(Sender: TObject);
var RangeW: Word97.Range;
    v1: variant;
    ov1: OleVariant;
    Row1: Word97.Row;
    Reply: integer;
    tempstr: string;
    KolcsonkerokSzama: integer;
begin
 Reply := Application.MessageBox('Figyelem! Az exportáláshoz a MS Word 97-es verziója kell!','Figyelmeztetés',mb_OkCancel + mb_IconWarning);
 if Reply = id_Cancel then Exit;
 Reply := frmKerdes.ShowModal;
 if Reply = mrCancel then Exit;

 WordApplication1.Visible := true;
 WordDocument1.PageSetup.Orientation := 1;
 WordDocument1.Activate;

 if frmKerdes.RadioGroup1.ItemIndex = 0 then
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
 end
 else if frmKerdes.RadioGroup1.ItemIndex = 1 then
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
 end
 else if frmKerdes.RadioGroup1.ItemIndex = 2 then
 begin
  WordDocument1.Range.Text := 'Kölcsönkérõk';
  WordDocument1.Range.Font.Size := 14;
  ADOTable1.TableName := 'Kolcsonkerok';
  ADOTable1.Open;
  ADOTable1.First;
  KolcsonkerokSzama := ADOTable1.RecordCount;
  while not ADOTable1.Eof do
  begin
   WordDocument1.Range.InsertParagraphAfter;
   WordDocument1.Paragraphs.Last.Range.Text := ADOTable1.FieldByName('Nev').AsString + #9 + ADOTable1.FieldByName('Cim').AsString + #9 + ADOTable1.FieldByName('Telszam').AsString;
   ADOTable1.Next;
  end;
  ADOTable1.Close;
  WordDocument1.Range.InsertParagraphAfter;
  WordDocument1.Paragraphs.Last.Range.Text := 'CDk';
  ADOTable1.TableName := 'CDk';
  ADOTable1.Open;
  ADOTable1.First;
  while not ADOTable1.Eof do
  begin
   WordDocument1.Range.InsertParagraphAfter;
   tempstr := ADOTable1.FieldByName('CDNeve').AsString + #9 + ADOTable1.FieldByName('Tartalom').AsString + #9;
   tempstr := tempstr + ADOTable1.FieldByName('PontosTartalom').AsString + #9;
   tempstr := tempstr + ADOTable1.FieldByName('Hely').AsString + #9;
   tempstr := tempstr + ADOTable1.FieldByName('Megjegyzes').AsString + #9;
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
//------TÁBLÁZATTÁ ALAKÍTÁS-----------------
  RangeW := WordDocument1.Content;
  v1 := RangeW;
  v1.ConvertToTable(#9,19,8);
  Row1 := WordDocument1.Tables.Item(1).Rows.Get_First;
  Row1.Range.Bold := 1;
  Row1.Range.Font.Size := 30;
  Row1.Range.InsertParagraphAfter;
  ov1 := ' ';
  Row1.ConvertToText(ov1);

  Row1 := WordDocument1.Tables.Item(1).Rows.Item(KolcsonkerokSzama + 1);
  Row1.Range.Bold := 1;
  Row1.Range.Font.Size := 30;
  Row1.Range.InsertParagraphBefore;
  Row1.Range.InsertParagraphBefore;
  Row1.Range.InsertParagraphAfter;
  Row1.ConvertToText(ov1);
 end;
end;

procedure TfrmMainForm.cmdExcelClick(Sender: TObject);
var Reply: integer;
    RangeE: Excel97.Range;
    I, Row: integer;
begin
 Reply := Application.MessageBox('Figyelem! Az exportáláshoz a MS Excel 97-es verziója kell!','Filegyelmeztetés',mb_OkCancel + mb_IconWarning);
 if Reply = id_Cancel then Exit;
 Reply := frmKerdes.ShowModal;
 if Reply = mrCancel then Exit;
 ExcelApplication1.Visible[0] := true;
 ExcelApplication1.Workbooks.Add(null,0);
 if frmKerdes.RadioGroup1.ItemIndex = 0 then
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
 else if frmKerdes.RadioGroup1.ItemIndex = 1 then
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
 end
 else if frmKerdes.RadioGroup1.ItemIndex = 2 then
 begin
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
  ADOTable1.Close;
  ADOTable1.TableName := 'CDk';
  ADOTable1.Open;
  RangeE := ExcelApplication1.Range['E1','E1'];
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
    RangeE := ExcelApplication1.Range['E' + IntToStr(Row),'E' + IntToStr(Row)];
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
  RangeE := ExcelApplication1.Range['E1','L' + IntToStr(Row-1)];
  RangeE.AutoFormat(3,null,null,null,null,null,null);
 end;
end;

end.
