{$J+} {Writable constants}

unit Extbl10u;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OvcTCEdt, OvcTCmmn, OvcTCell, OvcTCStr, OvcTCHdr, OvcBase, OvcTable,

  Buttons, StdCtrls;


const
  BITMAP_1 = 10001;
  BITMAP_2 = 10002;

type
  MyArrayRecord = record
    S1 : string[10];
    S2 : string[10];
  end;

  TForm1 = class(TForm)
    OvcTable1: TOvcTable;
    OvcController1: TOvcController;
    OvcTCColHead1: TOvcTCColHead;
    OvcTCString1: TOvcTCString;
    OvcTCString2: TOvcTCString;
    procedure FormCreate(Sender: TObject);
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
    procedure OvcTCColHead1OwnerDraw(Sender: TObject; TableCanvas: TCanvas;
      const CellRect: TRect; RowNum: Longint; ColNum: Integer;
      const CellAttr: TOvcCellAttributes; Data: Pointer;
      var DoneIt: Boolean);
    procedure OvcTable1LockedCellClick(Sender: TObject; RowNum: Longint;
      ColNum: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OvcTable1ActiveCellMoving(Sender: TObject; Command: Word;
      var RowNum: Longint; var ColNum: Integer);
    procedure OvcTable1ActiveCellChanged(Sender: TObject; RowNum: Longint;
      ColNum: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    MyArray : array[1..9] of MyArrayRecord;
    Col1Down,
    Col2Down  : Boolean;

    SortCol   : TColNum;

    InactiveBMP,
    ActiveBMP    : TBitmap;

    procedure SortRecords(Col : Integer);
  end;

var
  Form1: TForm1;


implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  I, J : integer;
begin
  Randomize;
  for I := 1 to 9 do
  begin
    MyArray[I].S1[0] := Chr(10);
    MyArray[I].S2[0] := Chr(10);
    for J := 1 to 10 do
    begin
      MyArray[I].S1[J] := Chr(Ord('A') + Random(26));
      MyArray[I].S2[J] := Chr(Ord('A') + Random(26));
    end;
  end;

  Col1Down := False;
  Col2Down := False;

  InactiveBMP := TBitmap.Create;
  ActiveBMP := TBitmap.Create;

{$IFDEF WIN32}
  InactiveBMP.LoadFromResourceID(HInstance, BITMAP_1);
  ActiveBMP.LoadFromResourceID(HInstance, BITMAP_2);
{$ELSE}
  InactiveBMP.Handle := LoadBitmap(HInstance, MAKEINTRESOURCE(BITMAP_1));
  ActiveBMP.Handle := LoadBitmap(HInstance, MAKEINTRESOURCE(BITMAP_2));
{$ENDIF}

end;

procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
begin
  Data := nil;
  if (RowNum > 0) and (RowNum <= High(MyArray)) then
  begin
    case ColNum of
      1 : Data := @MyArray[RowNum].S1;
      2 : Data := @MyArray[RowNum].S2;
    end;
  end;
end;

procedure TForm1.OvcTCColHead1OwnerDraw(Sender: TObject;
  TableCanvas: TCanvas; const CellRect: TRect; RowNum: Longint;
  ColNum: Integer; const CellAttr: TOvcCellAttributes; Data: Pointer;
  var DoneIt: Boolean);
var
  DRect : TRect;
  SRect : TRect;
begin
  if RowNum = 0 then
  begin
    TableCanvas.Font.Color := clBlack;

    DRect := Rect(CellRect.Right-24, CellRect.Top+4,
                  CellRect.Right-8, CellRect.Top+20);
    SRect := Rect(0,0,16,16);
    case ColNum of
      0 : DrawButtonFace(TableCanvas, CellRect, 2, bsAutoDetect, False, False, False);
      1 : begin
            DrawButtonFace(TableCanvas, CellRect, 2, bsAutoDetect, False, Col1Down, False);
            if Col1Down then
            begin
              TableCanvas.TextOut(CellRect.Left+10, CellRect.Top+10, 'Active');
              TableCanvas.BrushCopy(DRect, ActiveBMP, SRect, clSilver);
            end else
            begin
              TableCanvas.TextOut(CellRect.Left+10, CellRect.Top+10, 'Inactive');
              TableCanvas.BrushCopy(DRect, InactiveBMP, SRect, clSilver);
            end;
          end;

      2 : begin
            DrawButtonFace(TableCanvas, CellRect, 2, bsAutoDetect, False, Col2Down, False);
            if Col2Down then
            begin
              TableCanvas.TextOut(CellRect.Left+10, CellRect.Top+10, 'Active');
              TableCanvas.BrushCopy(DRect, ActiveBMP, SRect, clSilver);
            end else
            begin
              TableCanvas.TextOut(CellRect.Left+10, CellRect.Top+10, 'Inactive');
              TableCanvas.BrushCopy(DRect, InactiveBMP, SRect, clSilver);
            end;
          end;
    end;
    DoneIt := True;
  end;
end;

procedure TForm1.OvcTable1LockedCellClick(Sender: TObject; RowNum: Longint;
  ColNum: Integer);
begin
  if (RowNum <> 0) then Exit;

  Col1Down := ColNum = 1;
  Col2Down := ColNum = 2;
  with OvcTable1 do
  begin
    AllowRedraw := False;

    if Col1Down then
      SortRecords(1)
    else
      SortRecords(2);

    if Col1Down then
      TOvcTCString(Columns[1].DefaultCell).Color := clRed
    else
      TOvcTCString(Columns[1].DefaultCell).Color := clSilver;

    if Col2Down then
      TOvcTCString(Columns[2].DefaultCell).Color := clRed
    else
      TOvcTCString(Columns[2].DefaultCell).Color := clSilver;

    InvalidateTable;
    AllowRedraw := True;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(InactiveBMP) then
    InactiveBMP.Free;
  if Assigned(ActiveBMP) then
    ActiveBMP.Free;
end;


procedure TForm1.SortRecords(Col : Integer);
var
  I,
  J   : Integer;
  TR  : MyArrayRecord;
begin
  for I := 1 to High(MyArray)-1 do begin
    for J := I+1 to High(MyArray) do begin
      if (Col = 1) then begin
        if CompareText(MyArray[J].S1, MyArray[I].S1) < 0 then begin
          TR := MyArray[I];
          MyArray[I] := MyArray[J];
          MyArray[J] := TR;
        end;
      end else begin
        if CompareText(MyArray[J].S2, MyArray[I].S2) < 0 then begin
          TR := MyArray[I];
          MyArray[I] := MyArray[J];
          MyArray[J] := TR;
        end;
      end;
    end;
  end;
end;

procedure TForm1.OvcTable1ActiveCellMoving(Sender: TObject; Command: Word;
  var RowNum: Longint; var ColNum: Integer);
begin
  SortCol := OvcTable1.ActiveCol;
end;

procedure TForm1.OvcTable1ActiveCellChanged(Sender: TObject;
  RowNum: Longint; ColNum: Integer);
begin
  with OvcTable1 do begin
    AllowRedraw := False;
    try
      SortRecords(SortCol);
      InvalidateTable;
    finally
      AllowRedraw := True;
    end;
  end;
end;

end.



