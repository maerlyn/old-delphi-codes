unit extrunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, FileCtrl, ShlObj;

type
  TExtract = class( TForm )
    Panel1:            TPanel;
    Panel2:            TPanel;
    Panel3:            TPanel;
    OKBut:             TButton;
    CancelBut:         TButton;
    RadioGroup1:       TRadioGroup;
    RadioGroup2:       TRadioGroup;
    RadioGroup3:       TRadioGroup;
    DriveComboBox1:    TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;

    procedure OKButClick( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure FormActivate( Sender: TObject );
    procedure FormCreate(Sender: TObject);
  end;

var
  Extract: TExtract;

implementation

uses MainUnit;

{$R *.DFM}

procedure TExtract.OKButClick( Sender: TObject );
begin
   MainUnit.Canceled := False;
   MainUnit.ExtractDir := DirectoryListBox1.Directory;
   if RadioGroup1.ItemIndex = 0 then
      MainUnit.ExpandDirs := False
   else
      MainUnit.ExpandDirs := True;
   if RadioGroup2.ItemIndex = 0 then
      MainUnit.Overwr := False
   else
      MainUnit.Overwr := True;
   if RadioGroup3.ItemIndex = 0 then
      MainUnit.AllFiles := True
   else
      MainUnit.AllFiles := False;
   Close;
end;

procedure TExtract.CancelButClick( Sender: TObject );
begin
   MainUnit.ExtractDir := '';
   Close;
end;

procedure TExtract.FormActivate( Sender: TObject );
begin
   MainUnit.Canceled := True; { default }
end;

procedure TExtract.FormCreate(Sender: TObject);
var
   SpecFolder: String;
begin
   SpecFolder := '';

   MainForm.GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, SpecFolder );
   DriveComboBox1.Drive := ExtractFileDrive( SpecFolder )[1];
   DirectoryListBox1.Directory := ExtractFilePath( SpecFolder );
end;

end.
 