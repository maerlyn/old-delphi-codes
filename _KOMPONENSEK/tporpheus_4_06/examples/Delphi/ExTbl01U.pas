{$J+} {Writable constants}

unit ExTbl01U;

{  This example shows two techniques...one where the string portion (if the
   user added something not in the list) is temporarily stored in your
   "database" and the second technique uses a temporary record for the
   string, and only stores the index in the database.  The second method
   takes a little more work, since you have to use the OnDoneEdit event to
   update the database.

   Note that you will have to devise some sort of way to initialize the
   combobox's "Items" property when you initialize the table and/or form...
   and of course to store the "Items" list before you exit the form and/or
   program.  This, of course, depends on what you are doing with your data.

   You'll also likely want to change some properties in the combobox to help
   you enforce not adding two identical items, etc...and also synch the
   combobox's index to the index in the database (so the highlighted item in
   the list matches the item in the edit when you first pull it up).  I
   wanted to keep the example as simple as possible, so I didn't bother with
   those details.  You will likely make use of the OnBeginEdit event to
   handle some of these details...

   There are a couple of key properties to set in the combo, so be sure to
   check what I have done carefully.  A biggie that comes to mind is that
   MaxLength must be properly set.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, OvcTCHdr, OvcTCCBx, OvcTCmmn, OvcTable, OvcTCStr, OvcBase,
  StdCtrls, Buttons, OvcTCell;

type
  TForm1 = class(TForm)
    OvcTable1: TOvcTable;
    OvcController1: TOvcController;
    OvcTCColHead1: TOvcTCColHead;
    OvcTCComboBox1: TOvcTCComboBox;
    OvcTCComboBox2: TOvcTCComboBox;
    OvcTCRowHead1: TOvcTCRowHead;
    BitBtn1: TBitBtn;
    procedure OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
      ColNum: Integer; var Data: Pointer; Purpose : TovcCellDataPurpose);
    procedure OvcTable1DoneEdit(Sender: TObject; RowNum: Longint;
      ColNum: Integer);
    procedure BitBtn1Click(Sender: TObject);
  private
    CellData : String;
    ComboTemp : TCellComboBoxInfo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

type
  TTestRec = packed record
    C1Index  : integer;       { index for combobox1 }
    C1String : string[20];     { string for combobox1 }
    C2Index  : integer;       { index for combobox2 }
  end;

  TTestDataBase = array [1..9] of TTestRec;

const
  TestDatabase : TTestDataBase = (
    (C1Index : 2; C1String : ''; C2Index : 7),
    (C1Index : 2; C1String : ''; C2Index : 4),
    (C1Index : 4; C1String : ''; C2Index : 9),
    (C1Index : 6; C1String : ''; C2Index : 8),
    (C1Index : 1; C1String : ''; C2Index : 2),
    (C1Index : 4; C1String : ''; C2Index : 5),
    (C1Index : 7; C1String : ''; C2Index : 6),
    (C1Index : 3; C1String : ''; C2Index : 7),
    (C1Index : 5; C1String : ''; C2Index : 1));


procedure TForm1.OvcTable1GetCellData(Sender: TObject; RowNum: Longint;
  ColNum: Integer; var Data: Pointer; Purpose : TovcCellDataPurpose);
begin
  CellData := '';
  Data := @CellData;
    if (1 <= RowNum) and (RowNum <= 9) then
      case ColNum of
        1 : begin
              Data := PCellComboBoxInfo(@TestDatabase[RowNum].C1Index);
              if TestDataBase[RowNum].C1String <> '' then begin
                TestDataBase[RowNum].C1Index :=
                  OvcTCComboBox1.Items.Add(TestDataBase[RowNum].C1String);
                TestDataBase[RowNum].C1String := '';
              end;
            end;
        2 : begin
              ComboTemp.Index := TestDataBase[RowNum].C2Index;
              ComboTemp.St := '';
              Data := PCellComboBoxInfo(@ComboTemp);
            end;
      end{case}
    else if (RowNum = 0) then
      Data := nil;
end;

procedure TForm1.OvcTable1DoneEdit(Sender: TObject; RowNum: Longint;
  ColNum: Integer);
begin
  if ColNum = 2 then begin
    if ComboTemp.Index = -1 then
      ComboTemp.Index := OvcTCComboBox2.Items.Add(ComboTemp.St);
    TestDataBase[RowNum].C2Index := ComboTemp.Index;
  end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

end.
