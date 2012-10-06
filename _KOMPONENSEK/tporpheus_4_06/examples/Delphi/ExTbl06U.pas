{$J+} {Writable constants}

unit ExTbl06U;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, OvcTCmmn, OvcTCell, OvcTCStr, OvcTCEdt,
  OvcBase, OvcTable;

type
  S10 = string[10];

  TForm1 = class(TForm)
    OvcTable1: TOvcTable;
    OvcController1: TOvcController;
    OvcTCString1: TOvcTCString;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    HiIdx :   TRowNum;
    MyArray : array[1..9] of S10;


    procedure DeleteMyArray(Row : TRowNum);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  for I := 1 to High(MyArray) do
    MyArray[I] := Chr(Ord('A') + Random(26));
  HiIdx := High(MyArray);
end;

procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
begin
  Data := nil;
  if (RowNum > 0) and (RowNum <= HiIdx) then
    if ColNum = 1 then
      Data := @MyArray[RowNum];
end;


procedure TForm1.DeleteMyArray(Row : TRowNum);
var
  I : LongInt;
begin
  if Row < HiIdx then
    for I := Row to HiIdx-1 do
      MyArray[I] := MyArray[I+1];
  MyArray[HiIdx] := '';
  Dec(HiIdx);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  AR : TRowNum;
begin
  with OvcTable1 do
  begin
    if (HiIdx = 1) then
    begin
      ShowMessage('Cannot delete all records');
      Exit;
    end;
    AR := ActiveRow;
    AllowRedraw := False;
    DeleteMyArray(ActiveRow);
    Rows.Delete(ActiveRow);
    AllowRedraw := True;

    if AR <= HiIdx then
      ActiveRow := AR
    else
      ActiveRow := HiIdx;
  end;
end;

end.
