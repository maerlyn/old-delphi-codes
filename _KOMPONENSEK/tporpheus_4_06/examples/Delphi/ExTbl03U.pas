{$J+} {Writable constants}

{Example showing how to work with data used by the table outside the table's}
{usual method. In this case, the data accessed by a TOvcTCTMemo is always}
{displayed in lower case but stored in the data structure in upper case. This}
{could be expanded to access a memo field in a database and store the changed}
{data back to the database. The PictureField accesses a numeric value but }
{displays and edits the data as a string. When the data is placed back to the}
{data structure, it is converted back to a numeric value. This principle might}
{be expanded to allow custom picture mask editing, data conversion, etc.}

unit ExTbl03U;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, OvcTCmmn, OvcTCell, OvcTCStr, OvcTCBEF, OvcTCPic,
  OvcBase, OvcTable, OvcTCEdt, StdCtrls;

type
  StrBuf = array[0..99] of char;
  GroupNrString = string[7];

  TForm1 = class(TForm)
    OvcTable1: TOvcTable;
    OvcController1: TOvcController;
    OvcTCMemo1: TOvcTCMemo;
    Button1: TButton;
    OvcTCPictureField1: TOvcTCPictureField;
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
    procedure FormCreate(Sender: TObject);
    procedure OvcTable1DoneEdit(Sender: TObject; RowNum: Longint;
      ColNum: Integer);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    function LoStrBuf(SB : PChar) : PChar;
    procedure UpStrBuf(SB : PChar; Row : TRowNum);

    function String2GroupNr(S : GroupNrString) : Word;
    function GroupNr2String(N : Word) : GroupNrString;
  end;

var
  Form1: TForm1;
  MyMemos : array[1..9] of StrBuf;
  ABuf : PChar;

  MyWords : array[1..9] of Word;
  AString : GroupNrString;

implementation

{$R *.DFM}

function TForm1.LoStrBuf(SB : PChar) : PChar;
begin
  Result := StrLower(SB);
end;

procedure TForm1.UpStrBuf(SB : PChar; Row : TRowNum);
begin
  strCopy(MyMemos[Row], StrUpper(SB));
end;


function TForm1.String2GroupNr(S : GroupNrString) : Word;
var w1, w2 : Word;
begin
  w1 := 0;
  w2 := 0;
  w1 := StrToInt(Copy(S, 1, 2));
  w2 := StrToInt(Copy(S, 4, 4));
  Result := (w1 shl 11) + w2;
end;

function TForm1.GroupNr2String(N : Word) : GroupNrString;
var
  S1,
  S2  : GroupNRString;
begin
  S1 := IntToStr((N and $7800) shr 11);
  while (Byte(S1[0]) < 2) do
    S1 := ' ' + S1;
  S2 := IntToStr(N and $07FF);
  while (Byte(S2[0]) < 4) do
    S2 := '0' + S2;
  Result := S1 + '/' + S2;
end;


procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose: TOvcCellDataPurpose);
begin
  Data := nil;
  if (RowNum > 0) and (RowNum < 10) then
  begin
    case ColNum of
      1 : begin
            if (Purpose <> cdpForSave) then
            begin
              StrCopy(ABuf, MyMemos[RowNum]);
              ABuf := LoStrBuf(ABuf);
            end;
            Data := ABuf;
          end;

      2 : begin
            if (Purpose <> cdpForSave) then
              AString := GroupNr2String(MyWords[RowNum]);
            Data := @AString;
          end;
    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I : integer;
begin
  for I := 1 to 9 do
  begin
    MyWords[I] := I;
    if ((I mod  2) = 0) then
      StrPCopy(MyMemos[I], 'a')
    else
      StrPCopy(MyMemos[I], 'A');
  end;
  GetMem(ABuf, SizeOf(StrBuf)+1);
end;

procedure TForm1.OvcTable1DoneEdit(Sender: TObject; RowNum: Longint;
  ColNum: Integer);
begin
  if ColNum = 1 then
    UpStrBuf(ABuf, RowNum);
  if ColNum = 2 then
    MyWords[RowNum] := String2GroupNr(AString);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  with OvcTable1 do
    ShowMessage(StrPas(MyMemos[ActiveRow]));
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeMem(ABuf, SizeOf(StrBuf)+1);
end;

end.
