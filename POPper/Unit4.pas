unit Unit4;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, POPperData, Unit3;

type
  TfrmKuldendoleveleklistaja = class(TForm)
    StringGrid1: TStringGrid;
    POPperData21: TPOPperData2;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1DblClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ShowSentEmails;
  private
    xCol: integer;
    xRow: integer;
  public
    { Public declarations }
  end;

var
  frmKuldendoleveleklistaja: TfrmKuldendoleveleklistaja;

implementation

uses Unit1;

{$R *.DFM}

procedure TfrmKuldendoleveleklistaja.FormCreate(Sender: TObject);
var i: integer;
begin
 Self.Height := 166;
 Self.Width := 634;

 StringGrid1.Left := 0;
 StringGrid1.Top := 0;
 StringGrid1.Width := Self.ClientWidth;
 StringGrid1.Height := Self.ClientHeight;

 StringGrid1.Cells[0,0] := 'Címzett';
 StringGrid1.Cells[1,0] := 'Másolat';
 StringGrid1.Cells[2,0] := 'Rejtett másolat';
 StringGrid1.Cells[3,0] := 'Tárgy';


 if FileExists(ExtractFilePath(ParamStr(0)) + 'tobesent.pop') then
  POPperData21.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'tobesent.pop');

 if POPperData21.Count > 1 then
  StringGrid1.RowCount := POPperData21.Count + 1
 else
  StringGrid1.RowCount := 2;

 for i := 1 to POPperData21.Count do
 begin
  StringGrid1.Cells[0,i] := POPperData21.GetIndex(i,sit2To);
  StringGrid1.Cells[1,i] := POPperData21.GetIndex(i,sit2CC);
  StringGrid1.Cells[2,i] := POPperData21.GetIndex(i,sit2BCC);
  StringGrid1.Cells[3,i] := POPperData21.GetIndex(i,sit2Subject);
 end;
end;

procedure TfrmKuldendoleveleklistaja.FormResize(Sender: TObject);
begin
 StringGrid1.Width := Self.Width;
 StringGrid1.Height := Self.Height;
end;

procedure TfrmKuldendoleveleklistaja.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TfrmKuldendoleveleklistaja.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 StringGrid1.MouseToCell(X,Y,xCol,xRow);
end;

procedure TfrmKuldendoleveleklistaja.StringGrid1DblClick(Sender: TObject);
var frm: TfrmEmailkuldes;
begin
 if xRow = 0 then
  Abort;

 if xRow <= POPperData21.Count then
 begin
  frm := TfrmEmailkuldes.Create(frmMainForm);
  frm.ReadLetter(xRow);
  frm.Show;
 end; 
end;

procedure TfrmKuldendoleveleklistaja.FormActivate(Sender: TObject);
var i: integer;
begin
 for i := 1 to StringGrid1.RowCount - 1 do
 begin
  StringGrid1.Cells[0,i] := '';
  StringGrid1.Cells[1,i] := '';
  StringGrid1.Cells[2,i] := '';
  StringGrid1.Cells[3,i] := '';
 end;

 FormCreate(Sender);
end;

procedure TfrmKuldendoleveleklistaja.ShowSentEmails;
var i: integer;
begin
 POPperData21.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'sent.pop');

 if POPperData21.Count > 1 then
  StringGrid1.RowCount := POPperData21.Count + 1
 else
  StringGrid1.RowCount := 2;

 for i := 1 to POPperData21.Count do
 begin
  StringGrid1.Cells[0,i] := POPperData21.GetIndex(i,sit2To);
  StringGrid1.Cells[1,i] := POPperData21.GetIndex(i,sit2CC);
  StringGrid1.Cells[2,i] := POPperData21.GetIndex(i,sit2BCC);
  StringGrid1.Cells[3,i] := POPperData21.GetIndex(i,sit2Subject);
 end;

 Self.Caption := 'Elküldött levelek listája';
end;

end.
