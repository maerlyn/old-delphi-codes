unit Unit5;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, POPperData, Unit3, Unit6;

type
  TfrmUjleveleklistaja = class(TForm)
    StringGrid1: TStringGrid;
    POPperData1: TPOPperData;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    xRow: integer;
    xCol: integer;
  public
    { Public declarations }
  end;

var
  frmUjleveleklistaja: TfrmUjleveleklistaja;

implementation

uses Unit1;

{$R *.DFM}

procedure TfrmUjleveleklistaja.FormCreate(Sender: TObject);
var i: integer;
    s: string;
    Year, Month, Day: word;
    Hour, Min, Sec, MSec: word;
begin
 StringGrid1.Top := 0;
 StringGrid1.Left := 0;
 StringGrid1.Width := Self.ClientWidth;
 StringGrid1.Height := Self.ClientHeight;
 StringGrid1.ColWidths[6] := 0;

 StringGrid1.Cells[0,0] := 'O.'; //'Olvasott';
 StringGrid1.Cells[1,0] := 'Cs.'; //'Csatolt file';
 StringGrid1.Cells[2,0] := 'Küldõ';
 StringGrid1.Cells[3,0] := 'Tárgy';
 StringGrid1.Cells[4,0] := 'Méret';
 StringGrid1.Cells[5,0] := 'Dátum';

 POPperData1.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'newmail.pop');

 if POPperData1.Count > 1 then
  StringGrid1.RowCount := POPperData1.Count + 1
 else
  StringGrid1.RowCount := 2;

  for i := POPperData1.Count downto 1 do
  begin
   if boolean(POPperData1.GetIndex(i,sitRead)) = true then
    StringGrid1.Cells[0,i] := chr(149)
   else
    StringGrid1.Cells[0,i] := '';

   if trim(string(POPperData1.GetIndex(i,sitAttach))) <> '' then
    StringGrid1.Cells[1,i] := chr(149)
   else
    StringGrid1.Cells[1,i] := '';

   StringGrid1.Cells[2,i] := POPperData1.GetIndex(i,sitSender);
   StringGrid1.Cells[3,i] := POPperData1.GetIndex(i,sitSubject);
   StringGrid1.Cells[4,i] := IntToStr(POPperData1.GetIndex(i,sitSize));
//   StringGrid1.Cells[5,i] := DateTimeToStr(TDateTime(POPperData1.GetIndex(i,sitDate)));
   DecodeDate(TDateTime(POPperData1.GetIndex(i,sitDate)),Year,Month,Day);
   DecodeTime(TDateTime(POPperData1.GetIndex(i,sitDate)),Hour,Min,Sec,MSec);
   s := IntToStr(Year) + '.';
   if length(IntToStr(Month)) = 1 then
    s := s + '0';
   s := s + IntToStr(Month) + '.';
   if length(IntToStr(Day)) = 1 then
    s := s + '0';
   s := s + IntToStr(Day);
   StringGrid1.Cells[5,i] := s;
   StringGrid1.Cells[6,i] := IntToStr(i);
  end;
end;

procedure TfrmUjleveleklistaja.FormActivate(Sender: TObject);
begin
 FormCreate(Sender);
end;

procedure TfrmUjleveleklistaja.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TfrmUjleveleklistaja.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 StringGrid1.MouseToCell(X,Y,xCol,xRow);
end;

procedure TfrmUjleveleklistaja.StringGrid1DblClick(Sender: TObject);
var frm: TfrmUjlevelolvasasa;
begin
 frm := TfrmUjlevelolvasasa.Create(frmMainForm);
 frm.Read(StrToInt(StringGrid1.Cells[6,xRow]));
 frm.Show;
end;

end.
