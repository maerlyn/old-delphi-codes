unit mainunit;

{$INCLUDE ZipVers.inc}

Interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, ExtCtrls, SortGrid, ZipMstr, Menus, ShlObj, FileCtrl
  {$IfDef VERD4+} // if Delphi v4.xx or higher
     ,ImgList
  {$EndIf}
  ;

{$IfDef VERD2D3}  // if not Delphi v4.xx
type
   Int64 = Comp;  // 64 bit integers are supported differently by Delphi 2 and 3
{$EndIf}

type
  TMainform = class( TForm )
    Panel1:          TPanel;
    Panel2:          TPanel;
    Panel3:          TPanel;
    Panel4:          TPanel;
    ZipMaster1:      TZipMaster;
    StringGrid1:     TSortGrid;
    OpenDialog1:     TOpenDialog;
    Label1:          TLabel;
    Label2:          TLabel;
    Label4:          TLabel;
    Bevel1:          TBevel;
    Bevel2:          TBevel;
    ZipFName:        TLabel;
    TimeLabel:       TLabel;
    FilesLabel:      TLabel;
    MsgBut:          TButton;
    AddBut:          TButton;
    TestBut:         TButton;
    CloseBut:        TButton;
    DeleteBut:       TButton;
    NewZipBut:       TButton;
    ZipOpenBut:      TButton;
    ConvertBut:      TButton;
    ExtractBut:      TButton;
    DeleteZipBut:    TButton;
    RenameBut:       TButton;
    MainMenu1:       TMainMenu;
    File1:           TMenuItem;
    Exit1:           TMenuItem;
    Project1:        TMenuItem;
    Zipcomment1:     TMenuItem;
    Showlasterror1:  TMenuItem;
    DLLversioninfo1: TMenuItem;
    TraceCB:         TCheckBox;
    VerboseCB:       TCheckBox;
    UnattendedCB:    TCheckBox;
    ImageList1:      TImageList;

    procedure ZipOpenButClick( Sender: TObject );
    procedure CloseButClick( Sender: TObject );
    procedure NewZipButClick( Sender: TObject );
    procedure DeleteZipButClick( Sender: TObject );
    procedure ExtractButClick( Sender: TObject );
    procedure ZipMaster1DirUpdate( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure FillGrid;
    procedure AddButClick( Sender: TObject );
    procedure ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: string );
    procedure ZipMaster1Progress(Sender: TObject; ProgrType: ProgressType; FileName: string; FileSize: Integer );
    procedure DeleteButClick( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure TestButClick( Sender: TObject );
    procedure MsgButClick( Sender: TObject );
    procedure ConvertButClick( Sender: TObject );
    procedure FormResize( Sender: TObject );
    procedure VerboseCBClick( Sender: TObject );
    procedure TraceCBClick( Sender: TObject );
    procedure DLLversioninfo1Click( Sender: TObject );
    procedure Zipcomment1Click( Sender: TObject );
    procedure Showlasterror1Click( Sender: TObject );
    procedure Exit1Click( Sender: TObject );
    procedure UnattendedCBClick( Sender: TObject );
    procedure StringGrid1GetCellFormat( Sender: TObject; Col, Row: LongInt; State: TGridDrawState; var FormatOptions: TFormatOptions );
    procedure StringGrid1EndSort( Sender: TObject; Col: LongInt );
    procedure RenameButClick( Sender: TObject );

  public
    { Public declarations }
    DoIt:         Boolean;
    TotUncomp, TotComp: Cardinal;
    TotalSize1, TotalProgress1, TotalSize2, TotalProgress2: Int64;

    function  ShowLTime( s, f: Longint ): String;
    procedure SetZipFName( aCaption: String; AssignName: Boolean );
    function  GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
    procedure SetZipTotals;
    function  AskDirDialog( const FormHandle: HWND; var DirPath: String ): Boolean;
  end;

var
  Mainform:   TMainform;
  ExtractDir: String;
  ExpandDirs: Boolean;
  OverWr:     Boolean;
  AllFiles:   Boolean;
  Canceled:   Boolean;

Implementation

uses extrunit, msgunit, addunit, sfxunit, renunit;

{$R *.DFM}

procedure TMainform.FormCreate( Sender: TObject );
begin
   with StringGrid1 do
   begin
     { Make sure "goColMoving" is false in object inspector. This lets the
       TSortGrid use Mouse Clicks on the col headers. }
     RowCount := 2;  { first row is fixed, and used for column headers }
     Cells[0,0] := 'File Name';
     Cells[1,0] := 'Compr. Size';
     Cells[2,0] := 'Uncmpr. Size';
     Cells[3,0] := 'Date/Time';
     Cells[4,0] := 'Ratio';
     Cells[5,0] := 'Path';
   end;
   ZipMaster1.Load_Zip_Dll;
   ZipMaster1.Load_Unz_Dll;
   { If we had args on the cmd line, then try to open the first one
     as a zip/exe file.  This is most useful in case user has an association
     to ".zip" that causes this program to run when user dble clicks on a zip
     file in Explorer. }
   if ParamCount > 0 then
      ZipMaster1.ZipFilename := ParamStr(1);
end;

procedure TMainform.FormResize( Sender: TObject );
begin
   if Width - 291 > 0 then
      ZipFName.Width := Width - 291
   else
      ZipFName.Width := 0;
   SetZipFName( ZipMaster1.ZipFilename, False );
end;

procedure TMainform.CloseButClick( Sender: TObject );
begin
   Close;
end;

procedure TMainform.FormDestroy( Sender: TObject );
begin
   ZipMaster1.Unload_Zip_Dll;
   ZipMaster1.Unload_Unz_Dll;
end;

procedure TMainform.ZipOpenButClick( Sender: TObject );
var
   FirstDir: String;
begin
   if FirstDir = '' then
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, FirstDir );
   with OpenDialog1 do
   begin
      InitialDir := FirstDir;
      Title      := 'Open Existing ZIP File';
      Filter     := 'ZIP Files (*.ZIP, *.EXE)|*.zip;*.exe';
      FileName   := '';
      Options := Options + [ofHideReadOnly, ofShareAware, ofPathMustExist, ofFileMustExist];
      if Execute then
      begin
         FirstDir := ExtractFilePath( FileName );
         { Set the caption after assigning the filename. This
           way, the filename will be null if the open failed. }
         SetZipFName( FileName, True );
      end;
   end;
end;

procedure TMainform.NewZipButClick(Sender: TObject);
var
   ans:      Boolean;
   FirstDir: String;
begin
   if FirstDir = '' then
      GetSpecialFolder( CSIDL_DESKTOPDIRECTORY, FirstDir );
   with OpenDialog1 do
   begin
      InitialDir := FirstDir;
      FileName   := '';
      Filter     := 'ZIP Files (*.ZIP)|*.zip';
      DefaultExt := 'Zip';
      Title      := 'Create New ZIP File';
      Options := Options + [ofHideReadOnly, ofShareAware];
      Options := Options - [ofPathMustExist, ofFileMustExist];
      if Execute then
      begin
         FirstDir := ExtractFilePath( FileName );
         if FileExists( FileName ) then
         begin
           Ans := MessageDlg( 'Overwrite Existing File: ' + FileName + '?',
                          mtConfirmation, [mbYes, mbNo], 0 ) = mrYes;
           if Ans then
              DeleteFile( FileName )
           else
              Exit;  { Don't use the new name }
         end;
         SetZipFName( FileName, True );
      end;
   end;
end;

procedure TMainform.DeleteZipButClick( Sender: TObject );
var
   ans: Boolean;
begin
   if FileExists( ZipMaster1.ZipFilename ) then
   begin
      Ans:=MessageDlg( 'Are you sure you want to delete: ' + ZipMaster1.ZipFilename
            + '?', mtConfirmation, [mbYes, mbNo], 0 ) = mrYes;
      if Ans then
      begin
         DeleteFile( ZipMaster1.ZipFilename );
         SetZipFName( '', True );
      end else
         Exit;  { Don't use the new name }
   end
   else
      ShowMessage( 'Zip file not found: ' + ZipMaster1.ZipFilename );
end;

procedure TMainform.ExtractButClick( Sender: TObject );
var
   i:             Integer;
   s, f, SelRow:  Longint;
   IsOne:         String;
begin
   if not FileExists( ZipMaster1.ZipFilename ) then
   begin
      ShowMessage( 'Error: file not found: ' + ZipMaster1.ZipFilename );
      Exit;
   end;
   Extract.ShowModal;
   if (ExtractDir = '') or Canceled then
      Exit;

   with StringGrid1 do
   begin
      if ZipMaster1.Count < 1 then
      begin
         ShowMessage( 'Error - no files to extract' );
         Exit;
      end;
      ZipMaster1.FSpecArgs.Clear;
      { Get fspecs of selected files, unless user wants all files extracted }
      if not AllFiles then
      begin
         for i := 1 to SelectedCount do
         begin
            SelRow := SelectedItems[ i ];
            if (SelRow > 0) and (SelRow <> RowCount - 1) then
               ZipMaster1.FSpecArgs.Add( Cells[5, SelRow] + Cells[0, SelRow] );
         end;
         if ZipMaster1.FSpecArgs.Count < 1 then
         begin
            ShowMessage( 'Error - no files selected' );
            Exit;
         end;
      end;
   end; { end with }

   MsgForm.RichEdit1.Clear;
   MsgForm.Show;
   { Put this message into the message form's memo }
   ZipMaster1Message( self, 0, 'Beginning Extract from ' + ZipMaster1.ZipFilename );

   with ZipMaster1 do
   begin
      ExtrBaseDir := ExtractDir;
      ExtrOptions := [];
      if ExpandDirs then
         ExtrOptions := ExtrOptions + [ExtrDirNames];
      if Overwr then
         ExtrOptions := ExtrOptions + [ExtrOverwrite];
      s := GetTickCount;
      try
         Extract;
      except
         ShowMessage( 'Error in Extract; Fatal DLL Exception in mainunit' );
      end;
      f := GetTickCount;
      TimeLabel.Caption := ShowLTime( s, f );
      if SuccessCnt = 1 then
         IsOne := ' was'
      else
         IsOne := 's were';
      ShowMessage( IntToStr( SuccessCnt ) + ' file' + IsOne + ' extracted' );
   end; { end with }
end;

procedure TMainform.AddButClick( Sender: TObject );
var
   s, f:  LongInt;
   IsOne: String;
begin
   if ZipMaster1.ZipFileName = '' then
   begin
      ShowMessage( 'Error - open a zip file first' );
      Exit;
   end;
   AddForm.Left   := Left;
   AddForm.Top    := Top;
   AddForm.Width  := Width;
   AddForm.Height := Height;
   Canceled := False;
   AddForm.ShowModal;  { let user pick filenames to add }
   if Canceled then
      Exit;
   if AddForm.SelectedList.Items.Count = 0 then
   begin
      ShowMessage( 'No files selected' );
      Exit;
   end;
   MsgForm.RichEdit1.Clear;
   MsgForm.Show;
   { Put this message into the message form's memo }
   ZipMaster1Message( self, 0, 'Beginning Add to ' + ZipMaster1.ZipFileName );

   with ZipMaster1 do
   begin
      { We want any DLL error messages to show over the top
        of the message form. }
      AddOptions := [];
      case AddForm.ZipAction of   // Default is plain ADD.
         2: AddOptions := AddOptions + [AddUpdate];   // Update
         3: AddOptions := AddOptions + [AddFreshen];  // Freshen
         4: AddOptions := AddOptions + [AddMove];     // Move
      end;
      if AddForm.RecurseCB.Checked then
         AddOptions := AddOptions + [AddRecurseDirs];  { we want recursion }
      if AddForm.DirnameCB.Checked then
         AddOptions := AddOptions + [AddDirNames];  { we want dirnames }
      if AddForm.DiskSpanCB.Checked then
         AddOptions := AddOptions + [AddDiskSpan];  { we want diskspanning }
      if AddForm.EncryptCB.Checked then
      begin
         AddOptions := AddOptions + [AddEncrypt];  { we want a password }
      // GetAddPassword;
      // if Password = '' then
            { The 2 password's entered by user didn't match. }
            { We'll give him one more try; if he still messes it
              up, the DLL itself will prompt him one final time. }
       //   GetAddPassword;
      end;
      FSpecArgs.Clear;
      FSpecArgs.Assign( AddForm.SelectedList.Items ); { specify filenames }
      AddForm.SelectedList.Clear;
      s := GetTickCount;
      try
         Add;
      except
         ShowMessage( 'Error in Add; Fatal DLL Exception in mainunit' );
      end;
      f := GetTickCount;
      TimeLabel.Caption := ShowLTime( s, f );
      if SuccessCnt = 1 then
         IsOne := ' was'
      else
         IsOne := 's were';
      ShowMessage( IntToStr( SuccessCnt ) + ' file' + IsOne + ' added' );
   end; { end with }
end;

procedure TMainform.DeleteButClick( Sender: TObject );
var
   i:            Integer;
   Ans:          Boolean;
   s, f,SelRow:  LongInt;
   IsOne:        String;
begin
   with StringGrid1 do
   begin
      if ZipMaster1.Count < 1 then
      begin
         ShowMessage( 'Error - no files to delete' );
         Exit;
      end;
      Ans := MessageDlg( 'Delete selected files from: ' + ZipMaster1.ZipFileName + '?',
                   mtConfirmation, [mbYes, mbNo], 0 ) = mrYes;
      if not Ans then
         Exit;

      ZipMaster1.FSpecArgs.Clear;
      for i := 1 to SelectedCount do
      begin
         SelRow := SelectedItems[ i ];
         if (SelRow > 0) and (SelRow <> RowCount - 1) then
            ZipMaster1.FSpecArgs.Add( Cells[5, SelRow] + Cells[0, SelRow] );
      end;

      if ZipMaster1.FSpecArgs.Count < 1 then
      begin
         ShowMessage( 'Error - no files selected' );
         Exit;
      end;
   end; { end with }

   MsgForm.RichEdit1.Clear;
   MsgForm.Show;
   { Put this message into the message form's memo }
   ZipMaster1Message( self, 0, 'Beginning delete from ' + ZipMaster1.ZipFileName );

   s := GetTickCount;
   try
      ZipMaster1.Delete;
   except
      ShowMessage( 'Fatal error trying to delete' );
   end;
   f := GetTickCount;
   TimeLabel.Caption := ShowLTime( s, f );
   if ZipMaster1.SuccessCnt = 1 then
      IsOne := ' was'
   else
      IsOne := 's were';
   ShowMessage( IntToStr( ZipMaster1.SuccessCnt ) + ' file' + IsOne + ' deleted' );
end;

procedure TMainform.TestButClick( Sender: TObject );
var
   s, f: LongInt;
begin
   if ZipMaster1.Count < 1 then
   begin
      ShowMessage( 'Error - nothing to Test' );
      Exit;
   end;
   if Zipmaster1.Zipfilename = '' then
      Exit;
   MsgForm.RichEdit1.Clear;
   MsgForm.Show;
   ZipMaster1Message( self, 0, 'Beginning test of ' + ZipMaster1.ZipFileName );
   with ZipMaster1 do
   begin
      FSpecArgs.Clear;
      ExtrOptions := ExtrOptions + [ExtrTest];
      FSpecArgs.Add( '*.*' ); // Test all the files in the .zip
      // IMPORTANT: In this release, you must test all files.
      s := GetTickCount;
      Extract;  // This will really do a test
   end;
   f := GetTickCount;
   TimeLabel.Caption := ShowLTime( s, f );

   with ZipMaster1 do
   begin
      if SuccessCnt = DirOnlyCount + Count then
         ShowMessage( 'All ' + IntToStr( DirOnlyCount + Count ) + ' files tested OK' )
      else
         ShowMessage( 'ERROR: ' + IntToStr( DirOnlyCount + Count - SuccessCnt ) +
                        ' files tested as bad, or skipped!' );
   end;
end;

procedure TMainform.MsgButClick( Sender: TObject );
begin
   MsgForm.Show;
end;

procedure TMainform.ConvertButClick( Sender: TObject );
var
   ConvertErr: Integer;
begin
   if ZipMaster1.Count = 0 then
   begin
      ShowMessage( 'Error: no files in archive' );
      Exit;
   end;
   { determine which conversion is to be done }
   if UpperCase( ExtractFileExt( ZipMaster1.ZipFilename ) ) = '.EXE' then
   begin
      { Convert .EXE to .ZIP }
      ConvertErr := ZipMaster1.ConvertZIP;
      if  ConvertErr = 0 then
         ShowMessage( 'Filename is now: ' + ZipMaster1.ZipFilename )
      else
         ShowMessage( 'Error ' + IntToStr( ConvertErr ) + ' occured in making .ZIP file' );
   end
   else
   begin
      { Convert .ZIP to .EXE }
      { NOTE: If you put the ZIPSFX.BIN file into the WINDOWS
        or WINDOWS SYSTEM dir, then you don't need to set the
        SFXPath property below: }
      { ZipMaster1.SFXPath := 'c:\windows\system\zipsfx.bin'; }
      MakeSFX.ShowModal;
      if DoIt = False then
         Exit;
      ConvertErr := ZipMaster1.ConvertSFX;
      if ConvertErr = 0 then
         ShowMessage( 'Filename is now: ' + ZipMaster1.ZipFilename )
      else
         ShowMessage( 'Error ' + IntToStr( ConvertErr ) + ' occured in making .EXE file');
   end;
end;

procedure TMainform.VerboseCBClick( Sender: TObject );
begin
   ZipMaster1.Verbose := VerboseCB.Checked;
end;

procedure TMainform.TraceCBClick( Sender: TObject );
begin
   ZipMaster1.Trace := TraceCB.Checked;
end;

procedure TMainform.UnattendedCBClick( Sender: TObject );
begin
   ZipMaster1.Unattended := UnattendedCB.Checked;
end;

procedure TMainform.Showlasterror1Click( Sender: TObject );
begin
   if ZipMaster1.ErrCode <> 0 then
      ShowMessage( IntToStr( ZipMaster1.ErrCode ) + ' ' + ZipMaster1.Message )
   else
      ShowMessage( 'No last error present' );
end;

procedure TMainform.Exit1Click( Sender: TObject );
begin
   Close;
end;

procedure TMainform.Zipcomment1Click( Sender: TObject );
begin
   if ZipMaster1.ZipComment <> '' then
   begin
      MsgForm.RichEdit1.Clear;
      MsgForm.RichEdit1.Lines.Add( ZipMAster1.ZipComment );
      MsgForm.Show;
   end else
      ShowMessage( 'No Zip comment in this zip file' );
end;

procedure TMainform.DLLversioninfo1Click( Sender: TObject );
begin
   ShowMessage( 'UnZip Dll version: ' + IntToStr( ZipMaster1.UnzVers ) + #10 +
      '  Zip Dll version: ' + IntToStr( ZipMaster1.ZipVers ) );
end;

//***********************ZipMaster Event handling***************************
//---------------------------------------------------------------------------

// This is the "OnMessage" event handler
procedure TMainform.ZipMaster1Message( Sender: TObject; ErrCode: Integer; Message: string );
begin
   MsgForm.RichEdit1.Lines.Append( Message );
   PostMessage( MsgForm.RichEdit1.Handle, EM_SCROLLCARET, 0, 0 );
   if (ErrCode > 0) and not ZipMaster1.Unattended then
      ShowMessage( 'Error Msg: ' + Message );
end;

procedure TMainform.ZipMaster1DirUpdate( Sender: TObject );
begin
   FillGrid;
   FilesLabel.Caption := IntToStr( ZipMaster1.Count );
   if UpperCase( ExtractFileExt( ZipMaster1.ZipFilename ) ) = '.EXE' then
      ConvertBut.Caption := 'Convert to ZIP'
   else
      ConvertBut.Caption := 'Convert to EXE';
end;

procedure TMainform.ZipMaster1Progress( Sender: TObject; ProgrType: ProgressType; Filename: string; FileSize: Integer );
var
   Step : Integer;
begin
   case ProgrType of
      TotalSize2Process:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type TotalBytes, size= ' + IntToStr( FileSize ) );
            MsgForm.StatusBar1.Panels.Items[0].Text := 'Total size: ' + IntToStr( FileSize div 1024 ) + ' Kb';
            MsgForm.ProgressBar2.Position := 1;
            TotalSize2                    := FileSize;
            TotalProgress2                := 0;
         end;
      TotalFiles2Process:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type TotalFiles, files= ' + IntToStr( FileSize ) );
            MsgForm.StatusBar1.Panels.Items[1].Text := IntToStr( FileSize ) + ' files';
         end;
      NewFile:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type NewFile, size= ' + IntToStr( FileSize ) );
            MsgForm.FileBeingZipped.Caption := Filename;
            MsgForm.ProgressBar1.Position   := 1;         // Current position of bar.
            TotalSize1                      := FileSize;
            TotalProgress1                  := 0;
         end;
      ProgressUpdate:
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type Update, size= ' + IntToStr( FileSize ) );
            // FileSize gives now the bytes processed since the last call.
            TotalProgress1 := TotalProgress1 + FileSize;
            TotalProgress2 := TotalProgress2 + FileSize;
            if TotalSize1 <> 0 then
            begin
               {$IfDef VERD4+}  // D4+   (D5 gives a compiler error when using Int64 conversion!?)
               Step := MulDiv( TotalProgress1, 10000, TotalSize1 );
               {$Else}          // D2 and D3
               try
                  Step := Round( TotalProgress1 * 10000 / TotalSize1 );
               except
                  Step := 2147483647;
               end;
               {$EndIf}
               // ZipMaster1Message( self, 0, 'Step = ' + IntToStr( Step ) );
               MsgForm.ProgressBar1.Position := 1 + Step;
            end else
               MsgForm.ProgressBar1.Position := 10001;
            if TotalSize2 <> 0 then
            begin
               {$IfDef VERD4+}
               Step := MulDiv( TotalProgress2, 10000, TotalSize2 );
               {$Else}
               try
                  Step := Round( TotalProgress2 * 10000 / TotalSize2 );
               except
                  Step := 2147483647;
               end;
               {$EndIf}
               MsgForm.ProgressBar2.Position := 1 + Step;
            end;
         end;
      EndOfBatch:    // Reset the progress bar and filename.
         begin
            // ZipMaster1Message( self, 0, 'in OnProgress type EndOfBatch' );
            MsgForm.FileBeingZipped.Caption   := '';
            MsgForm.ProgressBar1.Position     := 1;
            MsgForm.StatusBar1.Panels[0].Text := '';
            MsgForm.StatusBar1.Panels[1].Text := '';
            MsgForm.ProgressBar2.Position     := 1;
         end;
   end;   // EOF Case
end;

//***********************User defined functions *****************************
//---------------------------------------------------------------------------


Function TMainform.ShowLTime( s, f: Longint ): String;
var
   min, sec,{mil,} st: Integer;
   smin, ssec{,smil}: String;
begin
   st := f - s;
   //mil := st mod 1000;
   //mil := mil mod 60;
   sec := st div 1000;
   min := sec div 60;
   sec := sec mod 60;
   //if mil > 9 then smil := IntToStr( mil ) else smil := '0' + IntToStr( mil );
   if sec > 9 then ssec := IntToStr( sec ) else ssec := '0' + IntToStr( sec );
   if min > 9 then smin := IntToStr( min ) else smin := '0' + IntToStr( min );
   Result := smin + ':' + ssec;
end;

procedure TMainform.SetZipFName( aCaption: String; AssignName: Boolean );
begin
   // Assigning the filename will cause the table of contents to be read.
   // and possibly reset it to an empty string (If error found).
   if AssignName then
      ZipMaster1.ZipFilename := aCaption;

   if ZipMaster1.ZipFilename = '' then
      ZipFName.Caption := AnsiString( '<none>' )
   else
      ZipFName.Caption := MinimizeName( ZipMaster1.ZipFilename, ZipFName.Canvas, ZipFName.Width );

   if ZipFName.Canvas.TextWidth( ZipMaster1.ZipFilename ) > ZipFName.Width then
   begin
      ZipFName.Hint     := ZipMaster1.ZipFilename;
      ZipFName.ShowHint := True;
   end else
      ZipFName.ShowHint := False;
end;

//---------------------------------------------------------------------------
procedure TMainform.SetZipTotals;
begin
   with StringGrid1 do
   begin
      Cells[0, RowCount - 1] := 'Total';
      Cells[1, RowCount - 1] := IntToStr( TotComp );
      Cells[2, RowCount - 1] := IntToStr( TotUncomp );
      if TotUnComp <> 0 then
         Cells[4, RowCount - 1] := IntToStr( Round( (1- (TotComp / TotUnComp) )* 100) ) + '% '
      else
         Cells[4, RowCount - 1] := '0 % ';
      Cells[5, RowCount - 1]    := '';
   end;
end;

//---------------------------------------------------------------------------
function TMainform.AskDirDialog( const FormHandle: HWND; var DirPath: String ): Boolean;
var
   pidl:        PItemIDList;
   FBrowseInfo: TBrowseInfo;
   Success:     Boolean;
   TitleName:   String;
   Buffer:      Array[0..MAX_PATH] of Char;
begin
   Result := False;
   ZeroMemory( @FBrowseInfo, SizeOf( FBrowseInfo ) );
   try
      GetMem( FBrowseInfo.pszDisplayName, MAX_PATH );
      FBrowseInfo.hwndOwner := FormHandle;
      TitleName := 'Please specify a directory';
      FBrowseInfo.lpszTitle := PChar( TitleName );
      pidl := ShBrowseForFolder( FBrowseInfo );
      if pidl <> nil then
      begin
         Success := SHGetPathFromIDList( pidl, Buffer );
         // if False then pidl not part of namespace
         if Success then
         begin
            DirPath := Buffer;
            if DirPath[Length( DirPath )] <> '\' then
               DirPath := DirPath + '\';
            Result := True;
         end;
         GlobalFreePtr( pidl );
      end;
   finally
      if Assigned( FBrowseInfo.pszDisplayName ) then
         FreeMem( FBrowseInfo.pszDisplayName, Max_Path );
   end;
end;

//---------------------------------------------------------------------------
{* Folder types are a.o.
 *	CSIDL_DESKTOPDIRECTORY, CSIDL_STARTMENU, CSIDL_SENDTO,
 * CSIDL_PROGRAMS, CSIDL_STARTUP etc.
 *}
function TMainform.GetSpecialFolder( aFolder: Integer; var Location: String ): LongWord;
var
   pidl:      PItemIDList;
   hRes:      HRESULT;
   RealPath:  Array[0..MAX_PATH] of Char;
   Success:   Boolean;
begin
   Result := 0;
   hRes := SHGetSpecialFolderLocation( Handle, aFolder, pidl );
   if hRes = NO_ERROR then
   begin
      Success := SHGetPathFromIDList( pidl, RealPath );
      if Success then
         Location := String( RealPath ) + '\'
      else
         Result := LongWord( E_UNEXPECTED );
   end else
      Result := hRes;
end;

//**************************Grid functions **********************************
//---------------------------------------------------------------------------
procedure TMainForm.FillGrid;
var
  i:  Integer;
  so: TSortOptions;
begin
  with StringGrid1 do
  begin
    { remove everything from grid except col titles }
    RowCount := 2;
    Rows[1].Clear;
    if ZipMaster1.Count = 0 then
       Exit;

    StringGrid1.RowCount := ZipMaster1.Count + 2;
    TotUnComp := 0;
    TotComp   := 0;
    for i := 1 to ZipMaster1.Count do
    begin
       with ZipDirEntry( ZipMaster1.ZipContents[i - 1]^ ) do
       begin
          Cells[0, i] := ExtractFileName( FileName );
          Cells[1, i] := IntToStr( CompressedSize );
          Cells[2, i] := IntToStr( UncompressedSize );
          Cells[3, i] := FormatDateTime( 'ddddd  t', FileDateToDateTime( DateTime ) );
          if UncompressedSize <> 0 then
             Cells[4, i] := IntToStr( Round( (1- (CompressedSize / UnCompressedSize) )* 100) ) + '% '
          else
             Cells[4, i] := '0% ';
          Cells[5, i] := ExtractFilePath( FileName );
          TotUncomp   := TotUnComp + Cardinal(UncompressedSize);
          Inc( TotComp, CompressedSize );
       end; // end with
    end; // end for
    so.SortDirection := sdAscending;
    so.SortStyle     := ssAutomatic;
    so.SortCaseSensitive := False;
    SortByColumn( SortColumn, so );
    Row := 1;
  end; // end with
end;

procedure TMainform.StringGrid1EndSort( Sender: TObject; Col: LongInt );
begin
   SetZipTotals;
end;

procedure TMainform.StringGrid1GetCellFormat( Sender: TObject; Col, Row: LongInt; State: TGridDrawState; var FormatOptions: TFormatOptions );
begin
   if (Row <> 0) and (Col <> 0) and (Col <> 5) then
      FormatOptions.AlignmentHorz := taRightJustify;
end;

procedure TMainform.RenameButClick( Sender: TObject );
begin
   RenForm.Show();
end;

End.

