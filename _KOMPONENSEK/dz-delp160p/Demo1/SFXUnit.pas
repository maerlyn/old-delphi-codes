unit sfxunit;

{$INCLUDE ZipVers.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ZipMstr, Buttons, ComCtrls, ShlObj, ShellAPI
  {$IfDef VERD4+} // if Delphi v4.xx
     ,ImgList
  {$EndIf}
  ;

type
  TMakeSFX = class( TForm )
    Panel1:           TPanel;
    Panel2:           TPanel;
    CmdLineCB:        TCheckBox;
    FileListCB:       TCheckBox;
    HideOverWriteCB:  TCheckBox;
    AutoRunCB:        TCheckBox;
    NoMsgShowCB:      TCheckBox;
    DfltOverWriteGrp: TRadioGroup;
    OkCancelRB:       TRadioButton;
    YesNoRB:          TRadioButton;
    OkBttnRB:         TRadioButton;
    DefIconRB:        TRadioButton;
    AutoIconRB:       TRadioButton;
    OrigIconRB:       TRadioButton;
    ExecBut:          TButton;
    CancelBut:        TButton;
    BitBtn1:          TBitBtn;
    BitBtn2:          TBitBtn;
    SFXPage:          TPageControl;
    TabSheet1:        TTabSheet;
    TabSheet2:        TTabSheet;
    TabSheet3:        TTabSheet;
    TabSheet4:        TTabSheet;
    TabSheet5:        TTabSheet;
    Label9:           TLabel;
    Label12:          TLabel;
    Label2:           TLabel;
    Label1:           TLabel;
    IconIndexLabel:   TLabel;
    MsgEdit:          TEdit;
    CaptionEdit:      TEdit;
    CommandEdit:      TEdit;
    IconEdit:         TEdit;
    IconIndexEdit:    TEdit;
    Memo2:            TMemo;
    Memo3:            TMemo;
    Memo4:            TMemo;
    Memo1:            TMemo;
    GroupBox1:        TGroupBox;
    GroupBox2:        TGroupBox;
    Image1:           TImage;
    OpenDialog1:      TOpenDialog;
    ImageList1:       TImageList;
    IconIndexUD:      TUpDown;
    DirectoryEdit:    TComboBox;

    procedure FormShow( Sender: TObject );
    procedure ExecButClick( Sender: TObject );
    procedure BitBtn1Click( Sender: TObject );
    procedure BitBtn2Click( Sender: TObject );
    procedure CancelButClick( Sender: TObject );
    procedure AutoRunCBClick( Sender: TObject );
    procedure DefIconRBClick( Sender: TObject );
    procedure IconEditKeyPress( Sender: TObject; var Key: Char );
    procedure IconIndexUDClick( Sender: TObject; Button: TUDBtnType );

    public
    procedure LoadCustomIcon( IconPath: String; IconIndex: Integer );
  end;

var
  MakeSFX: TMakeSFX;
  IconDir: String;

implementation

uses mainunit;

{$R *.DFM}

procedure TMakeSFX.ExecButClick( Sender: TObject );
begin
   with Mainform.ZipMaster1 do
   begin
      if CmdLineCB.Checked then
         SFXOptions := SFXOptions + [SFXAskCmdLine]
      else
         SFXOptions := SFXOptions - [SFXAskCmdLine];
      if FileListCB.Checked then
         SFXOptions := SFXOptions + [SFXAskFiles]
      else
         SFXOptions := SFXOptions - [SFXAskFiles];
      if HideOverWriteCB.Checked then
         SFXOptions := SFXOptions + [SFXHideOverWriteBox]
      else
         SFXOptions := SFXOptions - [SFXHideOverWriteBox];
      if AutoRunCB.Checked then
         SFXOptions := SFXOptions + [SFXAutoRun]
      else
         SFXOptions := SFXOptions - [SFXAutoRun];
      if NoMsgShowCB.Checked then
         SFXOptions := SFXOptions + [SFXNoSuccessMsg]
      else
         SFXOptions := SFXOptions - [SFXNoSuccessMsg];

      if DfltOverWriteGrp.ItemIndex = 0 then
         SFXOverWriteMode := ovrConfirm;
      if DfltOverWriteGrp.ItemIndex = 1 then
         SFXOverWriteMode := ovrAlways;
      if DfltOverWriteGrp.ItemIndex = 2 then
         SFXOverWriteMode := ovrNever;

      SFXCaption     := CaptionEdit.Text;
      SFXDefaultDir  := DirectoryEdit.Text;
      SFXCommandLine := CommandEdit.Text;
      SFXMessage     := '';
      if OkCancelRB.Checked then
         SFXMessage := #1;
      if YesNoRB.Checked then
         SFXMessage := #2;
      SFXMessage := SFXMessage + MsgEdit.Text;
   end;
   Mainform.DoIt := True;
   Close;
end;

procedure TMakeSFX.CancelButClick( Sender: TObject );
begin
   Mainform.DoIt := False;
   Close;
end;

procedure TMakeSFX.BitBtn1Click( Sender: TObject );
var TempStr: String;
begin
   TempStr := DirectoryEdit.Text;
   MainForm.AskDirDialog( MakeSFX.Handle, TempStr );
   DirectoryEdit.Text := TempStr;
end;

procedure TMakeSFX.FormShow( Sender: TObject );
begin
   with MainForm.ZipMaster1 do
   begin
      if SFXAskCmdLine in SFXOptions then
         CMDLineCB.Checked := True
      else
         CMDLineCB.Checked := False;
      if SFXAskFiles in SFXOptions then
         FileListCB.Checked := True
      else
         FileListCB.Checked := False;
      if SFXHideOverWriteBox in SFXOptions then
         HideOverWriteCB.Checked := True
      else
         HideOverWriteCB.Checked := False;
      if SFXAutoRun in SFXOptions then
         AutoRunCB.Checked := True
      else
         AutoRunCB.Checked := False;
      if SFXNoSuccessMsg in SFXOptions then
         NoMsgShowCB.Checked := True
      else
         NoMsgShowCB.Checked := False;

      case SFXOverWriteMode of
         ovrConfirm: DfltOverWriteGrp.ItemIndex := 0;
         ovrAlways:  DfltOverWriteGrp.ItemIndex := 1;
         ovrNever:   DfltOverWriteGrp.ItemIndex := 2;
      end;

      CaptionEdit.Text   := SFXCaption;
      DirectoryEdit.Text := SFXDefaultDir;
      CommandEdit.Text   := SFXCommandLine;
      if (Length( SFXMessage ) > 0) and ((SFXMessage[1] = #1) or (SFXMessage[1] = #2)) then
      begin
         if SFXMessage[1] = #1 then
            OkCancelRB.Checked := True
         else
            YesNoRB.Checked := True;
         MsgEdit.Text     := Copy( SFXMessage, 2, Length( SFXMessage )- 1);
      end else
      begin
         MsgEdit.Text     := SFXMessage;
         OkBttnRB.Checked := True;
      end;
   end;

   if ImageList1.Count = 3 then
      ImageList1.Delete( 2 );
   if IconEdit.Text = '' then
   begin
      if MainForm.ZipMaster1.SFXIcon.Empty then
      begin
         ImageList1.GetIcon( Integer( AutoRunCB.Checked ), Image1.Picture.Icon );
         MainForm.ZipMaster1.SFXIcon := Image1.Picture.Icon;
      end else
      begin
         ImageList1.AddIcon( MainForm.ZipMaster1.SFXIcon );
         OrigIconRB.Checked := True;
      end;
   end;
   if ImageList1.Count = 3 then
      OrigIconRB.Enabled := True
   else
      OrigIconRB.Enabled := False
end;

procedure TMakeSFX.BitBtn2Click( Sender: TObject );
begin
   if IconDir = '' then
      MainForm.GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, IconDir );
   with OpenDialog1 do
   begin
      InitialDir := IconDir;
      if Execute then
      begin
         IconDir       := ExtractFilePath( FileName );
         IconEdit.Text := FileName;
         DefIconRB.Checked := false;
         AutoIconRB.Checked := false;
         if UpperCase( ExtractFileExt( FileName ) ) <> 'ICO' then
         begin
            LoadCustomIcon( FileName, StrToIntDef( IconIndexEdit.Text, 0) );
            IconIndexLabel.Enabled := True;
            IconIndexEdit.Enabled  := True;
            IconIndexUD.Enabled    := True;
            IconIndexEdit.Color    := clWindow;
         end else
         begin
            Image1.Picture.Icon.LoadFromFile( FileName );
            IconIndexEdit.Text     := '0';
            IconIndexLabel.Enabled := False;
            IconIndexEdit.Enabled  := False;
            IconIndexUD.Enabled    := False;
            IconIndexEdit.Color    := clBtnFace;
            MainForm.ZipMaster1.SFXIcon := Image1.Picture.Icon;
         end;
      end;
   end;
end;

procedure TMakeSFX.AutoRunCBClick( Sender: TObject );
begin
   if IconEdit.Text = '' then
   begin
      ImageList1.GetIcon( Integer( AutoRunCB.Checked ), Image1.Picture.Icon );
      MainForm.ZipMaster1.SFXIcon := Image1.Picture.Icon;
   end;
end;

procedure TMakeSFX.DefIconRBClick( Sender: TObject );
begin
   ImageList1.GetIcon( TRadioButton(Sender).Tag, Image1.Picture.Icon );
   MainForm.ZipMaster1.SFXIcon := Image1.Picture.Icon;
   IconEdit.Text               := '';
   IconIndexEdit.Text          := '0';
   IconIndexLabel.Enabled      := False;
   IconIndexEdit.Enabled       := False;
   IconIndexUD.Enabled         := False;
   IconIndexEdit.Color         := clBtnFace;
end;

procedure TMakeSFX.LoadCustomIcon( IconPath: String; IconIndex: Integer );
var
   IconHandle: THandle;
begin
   Image1.Picture.Icon.ReleaseHandle();
   MainForm.ZipMaster1.SFXIcon.ReleaseHandle();
   IconHandle := ExtractIcon( hInstance, PChar( IconPath ), IconIndex );
   if (IconHandle <> 0) and (IconHandle <> 1) then
   begin
      Image1.Picture.Icon.Handle := IconHandle;
      MainForm.ZipMaster1.SFXIcon.Handle := IconHandle;
   end;
end;

procedure TMakeSFX.IconIndexUDClick( Sender: TObject; Button: TUDBtnType );
begin
   LoadCustomIcon( IconEdit.Text, StrToIntDef( IconIndexEdit.Text, 0) );
end;

procedure TMakeSFX.IconEditKeyPress( Sender: TObject; var Key: Char );
begin
   if FileExists ( IconEdit.Text ) then
      LoadCustomIcon( IconEdit.Text, StrToIntDef( IconIndexEdit.Text, 0) );
end;

end.
 
