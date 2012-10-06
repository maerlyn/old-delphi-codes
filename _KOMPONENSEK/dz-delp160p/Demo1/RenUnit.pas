unit renunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, ZipMstr, Grids, SortGrid;

type
  TRenForm = class( TForm )
    OkBitBtn:     TBitBtn;
    CancelBitBtn: TBitBtn;
    Label1:       TLabel;
    Label2:       TLabel;
    Label3:       TLabel;
    Label4:       TLabel;
    Panel1:       TPanel;
    OldCombo:     TComboBox;
    NewEdit:      TEdit;
    DTEdit:       TEdit;
    AddBtn:       TButton;
    RemoveBtn:    TButton;
    DTAllBtn:     TButton;
    SelectedGrid: TSortGrid;

    procedure FormShow( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure AddBtnClick( Sender: TObject );
    procedure DTAllBtnClick( Sender: TObject );
    procedure OkBitBtnClick( Sender: TObject );
    procedure OldComboClick( Sender: TObject );
    procedure RemoveBtnClick( Sender: TObject );
    procedure CancelBitBtnClick( Sender: TObject );
    procedure SelectedGridGetCellFormat( Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions );

  private { Private declarations }
    procedure ClearZipRenList;

  public  { Public declarations }
     ZipRenameList: TList;
     GDateTime: Integer;
  end;

var
  RenForm: TRenForm;

Implementation

Uses mainunit, msgunit;

{$R *.DFM}

procedure TRenForm.FormShow( Sender: TObject );
var
   i: Integer;
begin
   GDateTime := 0;
   SelectedGrid.RowCount := 2;
   SelectedGrid.Rows[1].Clear();
   RenForm.Caption := 'Rename items in zip archive: ' + ExtractFileName( MainForm.ZipMaster1.ZipFileName );
   OldCombo.Clear();
   ClearZipRenList();
   for i := 1 to MainForm.ZipMaster1.Count do
   begin
      with ZipDirEntry( MainForm.ZipMaster1.ZipContents[i - 1]^ ) do
      begin
          OldCombo.Items.Add( FileName );
      end;
   end;
end;

procedure TRenForm.AddBtnClick( Sender: TObject );
var
   AddItem:  String;
   RenRec:  pZipRenameRec;
begin
   AddItem := OldCombo.Text;
   if (AddItem <> NewEdit.Text) or (DTEdit.Text <> '') then
   begin
      if (Length( AddItem ) > 0) and (Length( NewEdit.Text ) > 0) then
      begin
         if (SelectedGrid.RowCount > 2) or ((SelectedGrid.RowCount = 2) and (SelectedGrid.Cells[0, 1] <> '')) then
            SelectedGrid.RowCount := SelectedGrid.RowCount + 1;
         New( RenRec );
         RenRec^.Source   := AddItem;
         RenRec^.Dest     := NewEdit.Text;
         try
            if DTEdit.Text <> '' then
               RenRec^.DateTime := DateTimeToFileDate( StrToDateTime( DTEdit.Text ) );
         except
            else
               Dispose( RenRec );
               Raise;
         end;
         ZipRenameList.Add( RenRec );
         SelectedGrid.Cells[0, SelectedGrid.RowCount - 1] := AddItem;
         SelectedGrid.Cells[1, SelectedGrid.RowCount - 1] := NewEdit.Text;
         SelectedGrid.Cells[2, SelectedGrid.RowCount - 1] := DTEdit.Text;
      end;
      OldCombo.Text := '';
      NewEdit.Text  := '';
      DTEdit.Text   := '';
   end;
end;

procedure TRenForm.FormCreate( Sender: TObject );
begin
   ZipRenameList := TList.Create();
   SelectedGrid.Cells[0, 0] := 'Old name';
   SelectedGrid.Cells[1, 0] := 'New name';
   SelectedGrid.Cells[2, 0] := 'Date/Time';
end;

procedure TRenForm.FormDestroy( Sender: TObject );
begin
   ClearZipRenList();
   ZipRenameList.Free();
end;

procedure TRenForm.ClearZipRenList();
var
   i:       Integer;
   RenRec: pZipRenameRec;
begin
   for i := 0 to ZipRenameList.Count - 1 do
   begin
      RenRec := ZipRenameList.Items[i];
      Dispose( RenRec );
   end;
   ZipRenameList.Clear();
end;

procedure TRenForm.RemoveBtnClick( Sender: TObject );
var
   i, j:    Integer;
   RenRec: pZipRenameRec;
begin
   j := SelectedGrid.Selection.Top;
   for i := SelectedGrid.Selection.Bottom downto j do
   begin
      if SelectedGrid.Cells[0, i] <> '' then
      begin
         RenRec := ZipRenameList.Items[i - 1];
         ZipRenameList.Delete( i - 1 );
         Dispose( RenRec );
         SelectedGrid.Rows[i].Clear();
         if i <> 1 then
            SelectedGrid.DeleteRow( i );
      end;
   end;
end;

procedure TRenForm.CancelBitBtnClick( Sender: TObject );
begin
   Hide();
end;

procedure TRenForm.OkBitBtnClick( Sender: TObject );
var
   RenameErr: Integer;
begin
   AddBtnClick( Sender );

   MsgForm.RichEdit1.Clear();
   MsgForm.Show();
   { Put this message into the message form's memo }
   MainForm.ZipMaster1Message( self, 0, 'Begin renaming entries in: ' + MainForm.ZipMaster1.ZipFileName );

   RenameErr := MainForm.ZipMaster1.Rename( ZipRenameList, GDateTime );
   if RenameErr <> 0 then
      ShowMessage( 'Error ' + IntToStr( RenameErr ) + ' occured in rename zip specification(s)');
   MsgForm.Hide();
   Hide();
end;

procedure TRenForm.OldComboClick( Sender: TObject );
begin
   NewEdit.Text := OldCombo.Items.Strings[OldCombo.ItemIndex];
   NewEdit.SetFocus();
   NewEdit.SelStart := Length( NewEdit.Text );
end;

procedure TRenForm.SelectedGridGetCellFormat( Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions );
begin
   if Row = 0 then
   begin
      FormatOptions.AlignmentHorz := taCenter;
      FormatOptions.Font.Style    := FormatOptions.Font.Style + [fsBold];
      FormatOptions.Font.Color    := clBlue;
   end;
end;

procedure TRenForm.DTAllBtnClick( Sender: TObject );
begin
   if DTEdit.Text <> '' then
      GDateTime := DateTimeToFileDate( StrToDateTime( DTEdit.Text ) )
   else
      GDateTime := 0;
end;

end.

