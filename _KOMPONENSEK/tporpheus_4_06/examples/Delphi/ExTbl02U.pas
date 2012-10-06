{$J+} {Writable constants}

unit ExTbl02U;

interface

uses
  {$IFDEF Win32}
  Windows,
  {$ELSE}
  WinTypes, WinProcs,
  {$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls,
  OvcData,
  OvcTCIco, OvcTCBmp, OvcTCGly, OvcTCCBx, OvcTCNum, OvcTCPic, OvcTCBEF,
  OvcTCSim, OvcTCBox, OvcTCEdt, OvcTCHdr, OvcTCmmn, OvcTCell, OvcTCStr,
  OvcBase, OvcTable, OvcEF, OvcPB, OvcNF, OvcSF, OvcNbk;

type
  TForm1 = class(TForm)
    OvcController1: TOvcController;
    OvcTCString1: TOvcTCString;
    OvcTCRowHead1: TOvcTCRowHead;
    OvcTCMemo1: TOvcTCMemo;
    OvcTCCheckBox1: TOvcTCCheckBox;
    OvcTCSimpleField1: TOvcTCSimpleField;
    OvcTCPictureField1: TOvcTCPictureField;
    OvcTCNumericField1: TOvcTCNumericField;
    OvcTCComboBox1: TOvcTCComboBox;
    OvcTCComboBox2: TOvcTCComboBox;
    OvcTCGlyph1: TOvcTCGlyph;
    OvcTCBitMap1: TOvcTCBitMap;
    OvcTable1: TOvcTable;
    OvcTCColHead1: TOvcTCColHead;
    OvcTCComboBox3: TOvcTCComboBox;
    procedure OvcTable1GetCellAttributes(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var CellAttr: TOvcCellAttributes);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
    procedure OvcTable1ColumnsChanged(Sender: TObject; ColNum1,
      ColNum2: Integer; Action: TOvcTblActions);
    procedure OvcTable1RowsChanged(Sender: TObject; RowNum1,
      RowNum2: Longint; Action: TOvcTblActions);
    procedure OvcTable1SizeCellEditor(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var CellRect: TRect;
      var CellStyle: TOvcTblEditorStyle);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

type
  TMyRecord = record
    mrString        : string[39];
    mrMemo          : array [0..79] of char;
    mrCheckBox      : TCheckBoxState;
    mrSimple        : string[9];
    mrPicture       : TOvcDate;
    mrNumeric       : double;
    mrComboBox1     : integer;
    mrComboBox2Int  : integer;
    mrComboBox2Str  : string[19];
    mrComboBox3Int  : integer;
    mrComboBox3Items: TStringList;
    mrGlyph         : integer;
  end;
  PMyDatabase = ^TMyDatabase;
  TMyDatabase = array [1..199] of TMyRecord;

var
  MyDB : PMyDatabase;
  ColToFieldMap : array [0..12] of TColNum;
  Items1 : TStringList;
  Items2 : TStringList;
  MyBitmap : TBitmap;

{$IFNDEF Win32}
type
  ShortString = string[255];

procedure SetLength(var S : ShortString; Len : byte);
  begin
    S[0] := char(Len);
  end;
{$ENDIF}

function RandomString(MaxLen : integer) : ShortString;
  var
    i : integer;
    len : integer;
  begin
    Len := Random(MaxLen) + 1;
    SetLength(Result, Len);
    for i := 1 to Len do
      Result[i] := char(Random(26) + ord('A'));
  end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Row : TRowNum;
  Col : TColNum;
begin
  {set up the items lists for combo box 3: it uses run-time lists}
  Items1 := TStringList.Create;
  Items1.Add('Brie');
  Items1.Add('Camembert');
  Items1.Add('Pont L''Eveque');
  Items1.Add('Port Salut');
  Items1.Add('Boursin');
  Items2 := TStringList.Create;
  Items2.Add('Youngs');
  Items2.Add('Fullers');
  Items2.Add('Theakstons');
  Items2.Add('Courage');
  Items2.Add('Shepherd Neame');
  {set up the database}
  New(MyDB);
  FillChar(MyDB^, sizeof(MyDB^), 0);
  for Row := 1 to 199 do
    with MyDB^[Row] do
      begin
        mrString := RandomString(39);
        StrPCopy(mrMemo, RandomString(79));
        mrCheckBox := TCheckBoxState(Odd(Random(50)));
        mrSimple := RandomString(9);
        mrPicture := (Random(1000) + 144000);
        mrNumeric := (integer(Random(2000)) - 1000) / 100.0;
        mrComboBox1 := Random(10);
        mrComboBox2Int := Random(10);
        mrComboBox3Int := Random(5);
        if Odd(Random(50)) then
          mrComboBox3Items := Items1
        else
          mrComboBox3Items := Items2;
        mrGlyph := Random(4);
      end;
  {set up the column-to-field mapping}
  for Col := 0 to 12 do
    ColToFieldMap[Col] := Col;
  {get the bitmap}
  MyBitmap := TBitmap.Create;
  MyBitmap.LoadFromFile('EXTBL02.BMP');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Dispose(MyDB);
  MyDB := nil;
  MyBitmap.Free;
  MyBitmap := nil;

  Items1.Free;
  Items2.Free;
end;

procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
begin
  Data := nil;
  if (0 < RowNum) and (RowNum <= 199) then                      {!!.01}
    case ColToFieldMap[ColNum] of
      1 : Data := @MyDB^[RowNum].mrString;
      2 : Data := @MyDB^[RowNum].mrMemo;
      3 : Data := @MyDB^[RowNum].mrCheckBox;
      4 : Data := @MyDB^[RowNum].mrSimple;
      5 : Data := @MyDB^[RowNum].mrPicture;
      6 : Data := @MyDB^[RowNum].mrNumeric;
      7 : Data := @MyDB^[RowNum].mrComboBox1;
      8 : Data := @MyDB^[RowNum].mrComboBox2Int;
      9 : Data := @MyDB^[RowNum].mrComboBox3Int;
     10 : Data := @MyDB^[RowNum].mrGlyph;
     11 : Data := MyBitmap;
    end;
end;

procedure TForm1.OvcTable1GetCellAttributes(Sender: TObject;
  RowNum: Longint; ColNum: Integer; var CellAttr: TOvcCellAttributes);
begin
  if (RowNum = 0) then
    begin
      {display the headings for the checkbox and glyph columns centrally}
      if (ColToFieldMap[ColNum] = 3) or (ColToFieldMap[ColNum] = 10) then
        CellAttr.caAdjust := otaCenter
      {display the heading for the numeric field right adjusted}
      else if (ColToFieldMap[ColNum] = 6) then
        CellAttr.caAdjust := otaCenterRight;
      {display the active heading flat and red and bold}
      if (ColNum = OvcTable1.ActiveCol) then
        begin
          CellAttr.caTextStyle := tsFlat;
          CellAttr.caColor := clRed;
          CellAttr.caFont.Style := [fsBold];
        end;
    end
  else {it's a data row}
    begin
      {display negative amounts for the numeric field in red}
      if (ColToFieldMap[ColNum] = 6) then
        if (MyDB^[RowNum].mrNumeric < 0.0) then
          CellAttr.caFontColor := clRed;
    end;
end;

procedure TForm1.OvcTable1ColumnsChanged(Sender: TObject; ColNum1,
  ColNum2: Integer; Action: TOvcTblActions);
var
  Temp : TColNum;
begin
  {for a column exchange, update the column-to-field mapping}
  if (Action = taExchange) then
    begin
      Temp := ColToFieldMap[ColNum1];
      ColToFieldMap[ColNum1] := ColToFieldMap[ColNum2];
      ColToFieldMap[ColNum2] := Temp;
    end;
end;

procedure TForm1.OvcTable1RowsChanged(Sender: TObject; RowNum1,
  RowNum2: Longint; Action: TOvcTblActions);
var
  Temp : TMyRecord;
begin
  {for a row exchange, swap the actual row data}
  if (Action = taExchange) then
    begin
      Temp := MyDB^[RowNum1];
      MyDB^[RowNum1] := MyDB^[RowNum2];
      MyDB^[RowNum2] := Temp;
    end;
end;

procedure TForm1.OvcTable1SizeCellEditor(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var CellRect: TRect; var CellStyle: TOvcTblEditorStyle);
begin
  case ColToFieldMap[ColNum] of
    1 : begin
          with CellRect do
            inc(Right, Right-Left);
          CellStyle := tesBorder;
        end;
    2 : begin
          with CellRect do
            begin
              inc(Right, Right-Left);
              inc(Bottom, (Bottom-Top)*2);
              CellStyle := tesBorder;
            end;
        end;
  else
    CellStyle := tes3D;
  end;{case}
end;


end.
