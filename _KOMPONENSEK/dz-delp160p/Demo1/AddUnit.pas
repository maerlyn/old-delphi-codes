unit Addunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl, ExtCtrls, Menus, ShlObj;

type
  TAddForm = class( TForm )
    Panel1:            TPanel;
    Panel2:            TPanel;
    Panel3:            TPanel;
    Panel4:            TPanel;
    Panel5:            TPanel;
    Panel6:            TPanel;
    Panel7:            TPanel;
    Panel8:            TPanel;
    DriveComboBox1:    TDriveComboBox;
    FileListBox1:      TFileListBox;
    DirectoryListBox1: TDirectoryListBox;
    OKBut:             TButton;
    AddBtn:            TButton;
    SortBut:           TButton;
    RemoveBut:         TButton;
    CancelBut:         TButton;
    AddDirBut:         TButton;
    VolSizeBut:        TButton;
    AddFileBut:        TButton;
    FreeDisk1But:      TButton;
    SelectAllBut:      TButton;
    SelectedList:      TListBox;
    Label1:            TLabel;
    Label2:            TLabel;
    DirNameCB:         TCheckBox;
    RecurseCB:         TCheckBox;
    EncryptCB:         TCheckBox;
    DiskSpanCB:        TCheckBox;
    Bevel1:            TBevel;
    PopupMenu1:        TPopupMenu;
    Add1:              TMenuItem;
    Update1:           TMenuItem;
    Freshen1:          TMenuItem;
    Move1:             TMenuItem;

    procedure OKButClick( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure AddFileButClick( Sender: TObject );
    procedure SortButClick( Sender: TObject );
    procedure RemoveButClick( Sender: TObject );
    procedure SelectAllButClick( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure AddDirButClick( Sender: TObject );
    procedure AddBtnClick( Sender: TObject );
    procedure Add1Click( Sender: TObject );
    procedure VolSizeButClick( Sender: TObject );
    procedure FreeDisk1ButClick( Sender: TObject );
    procedure DiskSpanCBClick(Sender: TObject);

  public
    { Public declarations }
    ZipAction: Integer;
  end;

var
  AddForm: TAddForm;
  InMouseClick: Boolean;

implementation

uses mainunit;

{$R *.DFM}

procedure TAddForm.OKButClick( Sender: TObject );
begin
   MainUnit.Canceled:=False;
   Close;
end;

procedure TAddForm.CancelButClick( Sender: TObject );
begin
  MainUnit.Canceled := True;
  Close;
end;

procedure TAddForm.SortButClick( Sender: TObject );
begin
  SelectedList.Sorted := True;
  SortBut.Enabled := False;  { list will remain sorted }
end;

procedure TAddForm.RemoveButClick( Sender: TObject );
var
   i: Integer;
begin
   for i := SelectedList.Items.Count - 1 downto 0 do
   begin
      if SelectedList.Selected[i] then
         SelectedList.Items.Delete(i);
   end;
end;

procedure TAddForm.SelectAllButClick( Sender: TObject );
var
   i: Integer;
begin
   for i := 0 to FileListBox1.Items.Count - 1 do
      FileListBox1.Selected[i] := True;
end;

procedure TAddForm.FormCreate( Sender: TObject );
var
   SpecFolder: String;
begin
   SpecFolder := '';

   MainForm.GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, SpecFolder );
   DriveComboBox1.Drive := ExtractFileDrive( SpecFolder )[1];
   DirectoryListBox1.Directory := ExtractFilePath( SpecFolder );
   InMouseClick := False;
end;

procedure TAddForm.AddDirButClick( Sender: TObject );
var
   i:        Integer;
   FullName: String;
begin
   MainUnit.Canceled := True;  // default
   for i := 0 to DirectoryListBox1.Items.Count - 1 do
   begin
      if DirectoryListBox1.Selected[i] then
      begin
         // Add this file if it isn't already in listbox
         FullName := Mainform.ZipMaster1.AppendSlash( DirectoryListBox1.Directory ) + '*.*';

         if SelectedList.Items.IndexOf( FullName ) < 0 then
            SelectedList.Items.Add( FullName );
      { Never de-select dirnames from the DirectoryList! }
      {  DirectoryListBox1.Selected[i]:=False; }
      end;
   end;
   { Position the "SelectedList" listbox at the bottom }
   with SelectedList do
   begin
      Selected[Items.Count-1] := True;
      Selected[Items.Count-1] := False;
   end;
end;

procedure TAddForm.AddFileButClick( Sender: TObject );
var
   i:        Integer;
   FullName: String;
begin
   MainUnit.Canceled := True;  // default
   for i := 0 to FileListBox1.Items.Count - 1 do
   begin
      if FileListBox1.Selected[i] then
      begin
         // Add this file if it isn't already in listbox
         FullName := Mainform.ZipMaster1.AppendSlash( DirectoryListBox1.Directory ) + FileListBox1.Items[i];
         if SelectedList.Items.IndexOf( FullName ) < 0 then
            SelectedList.Items.Add( FullName );
         FileListBox1.Selected[i] := False;
      end;
   end;
   { Position the "SelectedList" listbox at the bottom }
   with SelectedList do
   begin
      Selected[Items.Count - 1] := True;
      Selected[Items.Count - 1] := False;
   end;
end;

procedure TAddForm.AddBtnClick( Sender: TObject );
var
   pt: TPoint;
begin
   pt.x := 4;
   pt.y := 4;
   pt := AddBtn.ClientToScreen( pt );
   PopupMenu1.Popup( pt.x, pt.y );
end;

procedure TAddForm.Add1Click( Sender: TObject );
begin
   ZipAction := TMenuItem(Sender).Tag;
   TMenuItem(Sender).Checked := True;
   TMenuItem(Sender).Default := True;
   AddBtn.Caption := 'Action: ' + TMenuItem(Sender).Caption;
end;

procedure TAddForm.VolSizeButClick( Sender: TObject );
begin
   Mainform.ZipMaster1.MaxVolumeSize := StrToIntDef( InputBox( 'Max Vol size', 'Maximum size of an archive part',
      IntToStr( Mainform.ZipMaster1.MaxVolumeSize ) ), 0 );
end;

procedure TAddForm.FreeDisk1ButClick( Sender: TObject );
begin
   Mainform.ZipMaster1.KeepFreeOnDisk1 := StrToIntDef( InputBox( 'Keep free on Disk', 'Unused bytes on disk 1',
      IntToStr( Mainform.ZipMaster1.KeepFreeOnDisk1 ) ), 0 );
end;

procedure TAddForm.DiskSpanCBClick( Sender: TObject );
begin
   VolSizeBut.Enabled   := DiskSpanCB.Checked;
   FreeDisk1But.Enabled := DiskSpanCB.Checked;
end;

end.
  