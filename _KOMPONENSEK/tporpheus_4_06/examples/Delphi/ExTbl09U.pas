{$J+} {Writable constants}

unit ExTbl09U;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OvcTCBmp, OvcTCGly, OvcTCBox, OvcTCmmn, OvcTCell, OvcTCStr, OvcTCEdt,
  OvcBase, OvcTable, StdCtrls, OvcUser, OvcData, OvcTCBEF, OvcTCPic, OvcPF;

const
  MyMessage1 = WM_USER + 1;

type
  MyDataRecord = record
    TF1     : String[10];
  end;

  TForm1 = class(TForm)
    OvcTable1: TOvcTable;
    OvcController1: TOvcController;
    PF1: TOvcTCPictureField;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OvcTable1BeginEdit(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var AllowIt: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }

    procedure OnMyMessage1(var Msg : TMessage);
      message MyMessage1;
  end;

var
  Form1: TForm1;
  MyData : array [1..9] of MyDataRecord;
  MyUserData : TOvcUserData;


implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var
  I,
  J : Integer;
begin
  MyUserData := TOvcUserData.Create;
  MyUserData.UserCharSet[pmUser1] := ['A'..'C', '0'..'9'];

  Randomize;
  for I := 1 to 9 do
  with MyData[I] do begin
    TF1[0] := Chr(10);
    for J := 1 to 5 do
      TF1[J] := Chr(Ord('A') + (I - 1) mod 3);
    for J := 6 to 10 do
      TF1[J] := IntToStr(I)[1];
  end;
end;

procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
begin
  Data := nil;
  if (RowNum > 0) and (RowNum < 10) then
  begin
    case ColNum of
       1 : Data := @MyData[RowNum].TF1;
    end;
  end;
end;

procedure TForm1.OnMyMessage1(var Msg : TMessage);
begin
  (PF1.CellEditor as TOvcPictureField).UserData := MyUserData;
end;

procedure TForm1.OvcTable1BeginEdit(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var AllowIt: Boolean);
begin
{
OnBeginEdit is called before the CellEditor exists, therefore you
must post a message so that the cell can be created before trying
to set the UserData property
}
  AllowIt := True;
  if ColNum = 1 then
    PostMessage(Handle, MyMessage1, 0 , 0);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MyUserData.Free;
end;

end.

