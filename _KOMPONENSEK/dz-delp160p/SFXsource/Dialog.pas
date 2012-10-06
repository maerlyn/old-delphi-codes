(******************************************************************)
(* Copyright 1997, Microchip Systems / Carl Bunton                *)
(* Email: Twojags@cris.com                                        *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(*                                                                *)
(* Modified by Markus Stephany  mirbir.st@t-online.de             *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* SFX for DelZip v1.6                                            *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

// Changes:
// Sep 13, 2000 -When FileName is equal to Setup.exe a "!" is not needed.
//              -Before executing an inf the CurrentDirectory is set to the extract directory.

{the first modifications came from Eric W. Engler EEngler@zcsterling.com
 he is the creator of the powerful freeware zip-vcl DelZip for Delphi.

credits to : Deepu Chandy Thomas (deepu@md3.vsnl.net.in) for adding the SHBROWSEFORFOLDER routines (083198)
credits to : didier havelange (Didier.Havelange@ping.be) for the autorun feature (100298)
Frank Reichert (##FR) <F.Rei@gmx.de> added variable header length support, "path from registry"
and ExecInf (10/10/98) }

Unit Dialog;

INTERFACE

Uses Messages, Windows, SFXmisc, ShellApi;

Type // Lucjan Lukasik
  { IMalloc interface }

  IMalloc = interface( IUnknown )
    ['{00000002-0000-0000-C000-000000000046}']
    Function Alloc( cb: LongInt ): Pointer; StdCall;
    Function Realloc( pv: Pointer; cb: LongInt ): Pointer; StdCall;
    Procedure Free( pv: Pointer ); StdCall;
    Function GetSize( pv: Pointer ): LongInt; StdCall;
    Function DidAlloc( pv: Pointer ): Integer; StdCall;
    Procedure HeapMinimize; StdCall;
  end;

// reads the special header (s.b.), if any and sets some variables
procedure GetDefParams;

// execute the command-line read from the special header, if any
procedure ExecuteCMD( Int: Integer );

// returns True, if the new-dir button has been clicked
function SelectDir( Parent: HWND; Path: Pointer ): Boolean;

// enable/disable all children of the given parent window
// this is used to disable all main dialog's controls during archive extraction
// thanks to David - Kazuya david-kazuya@usa.net for report
procedure EnableChildren( Const wnd: HWND; Const Enable: Boolean );

// format the run checkbox text
function GetRunCheckBoxTitle: String;

Function NewDirProc (    DlgNew     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; StdCall;


Function MainDialogProc( DlgWin     : hWnd ;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM ;
                         DlgLParam  : LPARAM) : BOOL; StdCall;

Function FileExistsProc( DlgWin     : hWnd;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM;
                         DlgLParam  : LPARAM) : BOOL; StdCall;

Function PwdProc(        DlgWin     : hWnd;
                         DlgMessage : UINT;
                         DlgWParam  : WPARAM;
                         DlgLParam  : LPARAM) : BOOL; StdCall;

Var  MPVStoredPath   : pChar;                // the default-directory stored in the special header (out)
     MPVCommandLine  : pChar;                // the command line read from the special header
     MPVCaption      : pChar;                // the definable caption for the main dialog
     MPVFirstMessage : pChar;                // message to show before opening the main dialog
     FGUseSP         : Boolean = False;      // no stored path
     FGUseCL         : Boolean = False;      // no cammand line
     FGShowMsg       : Boolean = False;      // show a stored message before opening the main dialog
     FGAllowSel      : Boolean = True;       // user can choose files to extract
     FGAllowDCL      : Boolean = True;       // user can disable execution of the command line
     FGHideOWM       : Boolean = False;      // user can change the overwrite-mode
     FGDefOWM        : Integer = CM_CONFIRM; // by default : confirm
     FGAutoRun       : Boolean = False;      // no automatic extraction by default
     FGCheckSize     : Boolean = True;       // should we check the file size ?
     FGShowSuccess   : Boolean = True;       // should we show a message if all files have been extracted ?


IMPLEMENTATION

Uses SFXstrings, SFXgbls;

// SHBrowseFor Folder Code Definitions start //
Type
  TFNBFFCallBack = Function( Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM ): Integer stdcall;
   {TSHItemID -- Item ID }
  TSHItemID = Packed Record           { mkid }
    cb:   Word;                       { Size of the ID (including cb itself) }
    abID: Array[0..0] Of Byte;        { The item ID (variable length) }
  End;

  { TItemIDList -- List if item IDs (combined with 0-terminator) }
  pItemIDList = ^TItemIDList;
  TItemIDList = Packed Record         { idl }
     mkid: TSHItemID;
  End;

  TBrowseInfo = Packed Record
    hwndOwner:      HWND;
    pidlRoot:       pItemIDList;
    pszDisplayName: pAnsiChar;          { Return display name of item selected. }
    lpszTitle:      pAnsiChar;          { text to go in the banner over the tree. }
    ulFlags:        UINT;               { Flags that control the return stuff }
    lpfn:           TFNBFFCallBack;
    lParam:         LPARAM;             { extra info that's passed back in callbacks }
    iImage:         Integer;            { output var: where to return the Image index. }
  End;

Var
  BrowseInfo:  TBrowseInfo;
  DisplayName: Array[0..MAX_PATH] Of Char ;
  idBrowse:    pItemIDList;
  WND_NEWBT:   hwnd;
  FG_NEWBT:    Boolean;

  Function SHBrowseForFolder( Var lpbi: TBrowseInfo ): pItemIDList; StdCall; External 'shell32.dll' name 'SHBrowseForFolderA';
  Function SHGetPathFromIDList( pidl: pItemIDList; pszPath: pChar ): BOOL; StdCall; External 'shell32.dll' name 'SHGetPathFromIDListA';
  Function SHGetMalloc( Var ppMalloc: IMalloc ): HResult; StdCall; External 'shell32.dll' name 'SHGetMalloc'; // Lucjan Lukasik
  // SHBrowseFor Folder Code Definitions end //

Procedure FormatStr( Var Format: String; Const Insert: String );
Var
   pip: Integer;
Begin
   Repeat
      pip := Pos( '><', Format );
      If pip = 0 Then
         Break;
      Format := Copy( Format, 1, pip - 1 ) + Insert + Copy( Format, pip + 2, MAXINT );
   Until False;
End;

Function GetArgument( Index: Integer ): String; // gets an argument from the stored command line
//                1 : the part before the pipe (if there's no pipe, returns the whole command line)
//                2 : the part after the pipe (if no pipe, returns "")
//                all "><" will be replaced by the extraction path
Var
   pip: Integer;
Begin
   AppendDirTail( ExtPath );
   Result := pCharToStr( MPVCommandLine, StrLen( MPVCommandLine ) );
   pip := Pos( '|', Result );
   If pip = 0 Then
   Begin
      If Index = 2 Then
         Result := ''
   End Else
   Begin
      If Index = 1 Then
         Result := Copy( Result, 1, pip - 1 )
      Else
         Result := Copy( Result, pip + 1, MAXINT );
   End;

   FormatStr( Result, ExtPath );

   // get the short (8+3)-filename (it seems that shellexecute has some problems with lfn)
   GetShortPathName( pChar( Result ), pChar( Result ), Length( Result ) );
End;

Function TestForInf( sr1: String ): Boolean;
Var
   i: Integer;
Begin
   Result := False;
   For i := Length( sr1 )- 2 to Length( sr1 ) Do
      If ( sr1[i]  >= 'a' ) And ( sr1[i] <= 'z' ) Then
         sr1[i] := UpCase( sr1[i] );

   If Copy( sr1 , Length( sr1 )- 3, 4 ) = '.INF' Then
      Result := True;
End;


Function GetRunCheckBoxTitle: String;
Var
   sr1: String;
Begin
   sr1 := ExtractFileName( GetArgument( 1 ) );
   Result := STR_RUN_PRE + sr1 + ' '+ ExtractFileName( GetArgument( 2 ) );
   If TestForInf( sr1 ) Then
      FormatStr( Result, STR_RUN_INST )
   Else
      FormatStr( Result, STR_RUN_RUN );
End;

Function ForceDirs( Path1, Path2: pChar ): Boolean; // check whether all directories can be created
Var
   sr: String;
Begin
   Result := False;
   sr     := PCharToStr( Path2, StrLen( Path2 ) );

   If Pos( ':', sr ) > 0 Then Exit;

   While ( sr <> '' ) And ( sr[1] = BSL ) Do Delete( sr , 1 , 1 );

   if sr = '' Then Exit;

   ForceDirectories( pCharToStr( Path1, StrLen( Path1 ) ) + sr );

   If FileExists( pChar( sr + BSL ) ) Then
   Begin
      SetCurrentDirectory( pChar( sr ) );
      Result := True;
   End;
End;

//##FR execute inf-scripts using rundll, not nice but works!
Function ExecInf( Var Path, Param: String ): Cardinal;
Var
   osvi: TOSVersionInfo;
Begin
   Result:=0;

   if Param = '.ntx86'
   then
       Param := Param + ' '
   else
       Param := '';

   osvi.dwOSVersionInfoSize := SizeOf( OSvi );
   If GetVersionEx( OSVI ) Then
   Begin
      Case osvi.dwPlatformID Of
        VER_PLATFORM_WIN32_WINDOWS: Path := 'rundll.exe setupx.dll,InstallHinfSection DefaultInstall 132 ' + Path;
        VER_PLATFORM_WIN32_NT: Path := 'rundll32.exe setupapi.dll,InstallHinfSection DefaultInstall' +
                                      Param + '132 ' + Path;
      End;
      Result := WinExec( pChar( Path ), SW_SHOW );
   End;
End;

//##FR modified to enable inf-scripts
Procedure ExecuteCMD( Int: Integer ); // parses and executes the stored command line after extraction
Var
   sr1, sr2: String;
   OldDir:   Array[ 0..MAX_PATH + 1 ] of Char;
Begin
   If ( Int = 1 ) and FGUseCL Then
   Begin
      sr1 := GetArgument( 1 );
      sr2 := GetArgument( 2 );
      GetCurrentDirectory( MAX_PATH, OldDir );
      if Length( MPVStoredPath ) <> 0 then
         SetCurrentDirectory( pChar( MPVStoredPath ) );
      If Length( sr1 ) > 4 Then
      Begin
         If TestForInf( sr1 ) Then
            ExecInf( sr1, sr2 ) //error if < 32
         Else
            ShellExecute( 0, 'open', pChar( sr1 ), pChar( sr2 ), ExtPath, SW_SHOW );
      End;
      SetCurrentDirectory( OldDir );
   End;
End;

// hook-wnd-proc for shbrowseforfolder-dialog
Function OwnWndProc( wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM ): LRESULT; StdCall;
Begin
   Result := 1;
   If (Msg = WM_COMMAND) and (wParam = CM_NEW) Then
   Begin
      FG_NEWBT := True; // if the new button is pressed, send an ok-click and remember the new-button
      PostMessage( wnd , WM_COMMAND, CM_YES, 0 )
   End Else
      Result := DefDlgProc( wnd, Msg, wParam, lParam );
End;

// add a "new" button to the shbrowseforfolder
Procedure AddNewButton( wnd: HWND );
Var
  tempwnd  : hwnd;                  //hwnd for the child-windows of the dialog
  x1,y1,h1 : Integer;               //left,top,height of the button

  rct      : TRect;                 //storage for the ok-button's rect
  tp       : TPoint;                //point for screentoclient-api
Begin
        //get the top, left and height of the ok-button

        SetWindowText( wnd , STR_BROWSECAP );
        tempwnd := GetDlgItem ( wnd , 1 ) ; //ok-button

        GetWindowRect ( tempwnd , rct );
        tp.x := rct.left;
        tp.y := rct.top;
        ScreenToClient ( wnd , tp );
        x1 := tp.x-90; // our button's width is 75, so this should a bit bigger
        y1 := tp.y;
        h1 := rct.bottom-rct.top;

        //end of position-detection

        WND_NEWBT := CreateWindow( 'BUTTON' , STR_BTNFOLD , WS_CHILD OR WS_CLIPSIBLINGS OR WS_VISIBLE OR
                                    WS_TABSTOP OR BS_PUSHBUTTON, x1, y1, 75, h1, wnd, CM_NEW, HInstance, nil );

        // get the ok-button's font and set it for our button
        PostMessage( WND_NEWBT, WM_SETFONT, SendMessage( tempwnd, WM_GETFONT, 0, 0 ), MAKELPARAM( 1, 0 ) );

        // default -> disable, since we are on "my computer"
        EnableWindow( WND_NEWBT, False );

        //hook a new window procedure to catch this button
	SetWindowLong( wnd, GWL_WNDPROC, LongInt( @OwnWNDProc ) );
End;

// callback proc for shbrowseforfolder
Function BrowseCBProc( HWindow: HWND; uMsg: Integer; lParameter: LPARAM; lpBrowseFolder: LPARAM ): Integer; StdCall ;
Begin
   Result := 0;
   Case uMsg Of
      1{BFFM_INITIALIZED}: AddNewButton( HWindow );
      2{BFFM_SELCHANGED}:
      Begin
         // test if the currently selected path is a valid file-system path
         If NOT SHGetPathFromIDList( PItemIDList( lParameter ), DisplayName ) Then
            EnableWindow( GetDlgItem( HWindow, ID_OK ), False );

         // enable new-button only if ok-button is enabled
         EnableWindow( WND_NEWBT, IsWindowEnabled( GetDlgItem( HWindow, ID_OK ) ) );
      End;
   End;
End;


Function SelectDir( Parent: HWND; Path: Pointer ): Boolean;
var
     ppMalloc: IMalloc;  // Lucjan Lukasik
Begin
     Result := False;

     With BrowseInfo
     Do Begin
          hWndOwner      := Parent;
          pidlRoot       := nil;
          pszDisplayName := DisplayName;
          lpszTitle      := STR_EXPT;
          ulFlags        := $0001;
          lpfn           := @BrowseCBProc;
          lParam         := 0;
     End;

     FG_NEWBT := False;
     idBrowse := SHBrowseForFolder( BrowseInfo );

     If Assigned( idBrowse )  Then
     Begin
        // convert pidl to folder name //
        SHGetPathFromIDList( idBrowse, Path );
        if (SHGetMalloc( ppMalloc ) and $80000000) <> 0 then // Lucjan Lukasik
        // If error, then Exit;
           Exit;  // Result = False
        ppMalloc.Free( idBrowse );
        SetCurrentDirectory( Path );
        Result := FG_NEWBT;
        { result is only true if new-button has been clicked.
          if cancel has been pressed, the original path won't be changed. not very nice, but works in our case}
     End;
End;

(* to check correct file size of the input file *) // +++ 08/13/98
Procedure CheckFileSize;
Var
   pSZ, pCT, pFP :  DWORD;
   pBU           : pChar;
   cfp           :  DWORD;
   i             :  Integer;  //loop counter only var for compiler optimization (register value) ##FR
Const
   pFL           :  Boolean = False;
Begin
   // get the needed size of the buffer ( max 65536 + SizeOf( eocd ), min SizeOf( file ) )
   pSZ := GetFileSize( InFile, nil ) - DWORD( StartOfFile );
   If pSZ > 65558 Then
      pSZ := 65558;

   If pSZ > 22 Then  //if smaller, then no correct zip file
   Begin
      GetMem( pBU, pSZ );
      Try
         cfp := FSeek( 0, FILE_CURRENT );  //##FR mark the current file pos (cfp)
         pFP := FSeek( -pSZ, FILE_END );   //FSeek is now a function, not a proc, see sfxmisc.pas
         ReadFile( InFile, pBU[0], pSZ, DWORD( pCT ), nil );
         FSeek ( cfp , FILE_BEGIN );       //##FR jump back to marked filepos
         If pCT = pSZ Then For i := 0 to pSZ - 22  Do
            If (pBU[i] = #$50) And (pBU[i + 1] = #$4b) And (pBU[i + 2] = #$05) And (pBU[i + 3] = #$06) Then
               // eocd is found, now check if size is correct ( = pos+22+eocd.commentsize)
               If (DWORD( (Ord( pBU[i + 21] )* 256 ) + Ord( pBU[i + 20] ) + pFP + DWORD( i ) + 22 ) ) = GetFileSize( Infile, nil ) Then
               Begin
                  pFL := True; // set ok flag
                  Break;
               End;
      Finally
         FreeMem( pBU, pSZ );
      End;
   End;

   If NOT pFL Then
   Begin
      MessageBox( 0, STR_EINC_SIZE, STR_E, MB_ICONERROR );
      Halt;
   End;
End;

function CompareText( s1, s2: String ): Integer;
var
   i, L1: Integer;
begin
   Result := 1;
   L1 := Length( s1 );
   if L1 <> Length( s2 ) then Exit;
   for i := 1 to L1 do
     if UpCase( s1[ i ] ) <> UpCase( s2[ i ] ) then Exit;
   Result := 0;
end;

procedure GetDefParams; // reads the values from the special header, if any
var
   sig                           : Array[0..3] of Char;
   br, DataSize{of regkey value} : DWORD;
   srl, flb                      : Byte;
   keyname, keypath, data,
   pipfolder                     : pChar;
   key                           : HKEY; //##FR added for registry-routine
   AddOffset                     : Word;
   fn                            : String;
begin
   // the SFX executable's file size check this on recompile!!!
   {$IfDef VER130}       // D5 & BCB5
      StartOfFile := 43008;  // CT = 41472;
   {$else}
      {$IfDef VER125}       // BCB4
         StartOfFile := 40448;
      {$Else}
         {$IfDef VER120}       // D4
            StartOfFile := 40448; //40448=US, 40448=NL,DE
         {$Else}
            {$IfDef VER100}    // D3
               StartOfFile := 39936;
            {$Else}
               {$IfDef VER90}  // D2
                  StartOfFile := { 37888 check this!!! };
               {$EndIf}
            {$EndIf}
         {$EndIf}
      {$EndIf}
   {$EndIf}

   // ##FR: to enable headers with variable length I added the new MPV header.
   // because of its double use I changed cll to the new var flb (flag byte).

   FSeek( StartOfFile, FILE_BEGIN ); // let's look for a special-header signature directly after the sfx-code
   sig := #0#0#0#0;
   ReadFile( InFile, sig[0], 3, br, nil );

   If ( sig <> 'MPV' ) Then
   Begin
      MessageBox( 0, STR_EINC_SIZE, STR_E, MB_ICONERROR );
      Halt;
   End;

   ReadFile( InFile, flb, 1, br, nil );   // read and evaluate the flags-byte
   FGAllowDCL := (flb AND 1) = 1;
   FGAllowSel := (flb AND 2) = 2;
   FGHideOWM  := (flb AND 4) = 4;
   Case (flb AND 24) Of
       8 : FGDefOWM := CM_OVERWRITE;
      16 : FGDefOWM := CM_SKIP;
   End;

   // +++ aug 13, 1998
   FGCheckSize := ((flb AND 32) = 0);

   // +++ oct 02, 1998 (credits to didier havelange for this feature), filename must start with a "!" char
   // to enable autorun capability, sep 13, 2000 when FileName is equal to Setup.exe a "!" is not needed.
   fn := ExtractFileName( ParamStr( 0 ) );
   FGAutoRun := ((flb AND 64) = 64) And ((Pos( '!', fn ) = 1) or (CompareText( fn, 'Setup.exe' ) = 0));

   // +++ nov 01, 1998
   FGShowSuccess := ((flb AND 128) = 0 );

   // read over the next 2 bytes ( flag extension byte + mpv version byte )
   ReadFile( InFile, AddOffset, 2, br, nil );

   // now read the size of the MPV header
   ReadFile( InFile, AddOffset, 2, br, nil );
   StartOfFile := StartOfFile + AddOffset;

   ReadFile( InFile, srl, 1, br, nil );      // read length of caption
   // read the caption;
   if srl > 0 Then
   Begin
      ReadFile( InFile, MPVCaption^, srl, br, nil );
      MPVCaption[srl] := #0;
   End;

   ReadFile( InFile, srl, 1, br, nil );      // read length of stored path
   FGUseSP := srl > 0;
   // read the stored path;
   If FGUseSP Then
   Begin
      ReadFile( InFile, MPVStoredPath^, srl, br, nil );
      MPVStoredPath[ srl ] := #0;

      //get the path from registry, added 10/10/98 ##FR
      If ( MPVStoredPath[ 0 ] = 'H') And (MPVStoredPath[ 1 ] = 'K' ) Then
      Begin
         Key := 0;
         DataSize := 0;
         pipfolder := StrRScan( MPVStoredPath, '|' ) + 1;
         If pipfolder <> nil Then MPVStoredPath[StrLen( MPVStoredPath )- StrLen( pipfolder ) - 1] := #0;
         KeyName := StrRScan( MPVStoredPath, BSL );
         If keyname <> nil Then MPVStoredPath[StrLen( MPVStoredPath )- StrLen( keyname )] := #0;
         KeyPath := StrScan( MPVStoredPath, BSL );
         If keypath <> nil Then MPVStoredPath[StrLen( MPVStoredPath )- StrLen( keypath )] := #0;
         If MPVStoredPath = 'HKEY_CURRENT_USER' Then key := HKEY_CURRENT_USER Else
         If MPVStoredPath = 'HKCU' Then key := HKEY_CURRENT_USER Else //either full name or abbr.
         If MPVStoredPath = 'HKEY_LOCAL_MACHINE' Then key := HKEY_LOCAL_MACHINE Else
         If MPVStoredPath = 'HKLM' Then key := HKEY_LOCAL_MACHINE Else
         If MPVStoredPath = 'HKEY_USERS' Then key := HKEY_USERS Else
         If MPVStoredPath = 'HKU' Then key := HKEY_USERS; //other keys don't contain paths, I believe
         If key <> 0 Then If RegOpenKeyEx( Key, KeyPath+1, 0, KEY_EXECUTE, key ) = ERROR_SUCCESS Then
         Begin
            If RegQueryValueEx( key, KeyName + 1, nil, nil, nil, @DataSize ) = ERROR_SUCCESS Then
            Begin
               GetMem( Data, DataSize );
               RegQueryValueEx( key, KeyName + 1, nil, nil, pByte( data ), @DataSize );
               If StrScan( Data, ';' ) <> nil Then Data[StrLen( Data ) - StrLen( StrScan( Data, ';' ) )] := #0; //first path entry
               If StrLen( Data ) > max_path Then Data[max_path + 1] := #0;
               Move( Data[0] , MPVStoredPath[0], StrLen( Data ) + 1 );
               FreeMem( Data );
               If pipfolder <> nil Then If (StrLen( pipfolder ) + StrLen( MPVStoredPath )+ 2) < max_path Then
               Begin
                  AppendDirTail( MPVStoredPath );//see sfxMisc for modification of AppendDir Tail
                  Move( pipFolder[0], MPVStoredPath[StrLen( MPVStoredPath )], StrLen( pipFolder ) + 1 );
               End;
               RegCloseKey( key );
            End;
         End;        //at least "C:\"+#0
         If DataSize < 5 Then GetTempPath( max_path, MPVStoredPath ); //temp, if registry key doesn't exist
      End;//end of registry-routine

      //+++++ added march 01,98 : if the def path = "><", then use temporary directory
      If MPVStoredPath = '><' Then
         GetTempPath( MAX_PATH, MPVStoredPath );
   End;

   ReadFile( InFile, srl, 1, br, nil );      // read length of command line
   FGUseCL := srl > 0;
   // read the command line;
   If FGUseCL Then ReadFile( InFile, MPVCommandLine^, srl, br, nil );
   MPVCommandLine[srl] := #0;

   ReadFile( InFile, srl, 1, br, nil );     // read length of the message to show
   FGShowMsg := srl > 0;
   // read the message
   If FGShowMsg Then ReadFile( InFile, MPVFirstMessage^, srl, br, nil );
   MPVFirstMessage[srl] := #0;

   // now do the file size check
   If FGCheckSize Then CheckFileSize();

   // shall we show the message ?
   If FGShowMsg Then
   Begin
      AddOffset := MB_OK;
      Case MPVFirstMessage[0] Of
          #1 : AddOffset := MB_ICONINFORMATION or MB_OKCANCEL;
          #2 : AddOffset := MB_ICONQUESTION or MB_YESNO;
      End;

      If MPVFirstMessage[0] < #7 Then
         Move( MPVFirstMessage[1], MPVFirstMessage[0], StrLen( MPVFirstMessage ) );

      br := MessageBox( 0, MPVFirstMessage, MPVCaption, AddOffset );
      If AddOffset <> MB_OK Then
         Case br Of
            ID_NO,ID_CANCEL:  Halt;
      End;
   End;
End;

// added october 10, 1998
Procedure EnableChildren( Const wnd: HWND; Const Enable: Boolean ); // enable/disable all children of the given parent window
   Function FindChE( wnd: HWND; lParam: LPARAM ): Bool; StdCall;
   Var
      pCH : Array [0..20] Of Char;
   Begin
      Result := True;
      GetClassName( wnd, @pCH, 20 );
      if IsWindowVisible( wnd ) And (pCH <> 'Static') Then
         EnableWindow( wnd, Boolean( lParam ) );
   End;
Begin
   EnumChildWindows( wnd, @FindChE, Integer( Enable ) );
end;


(*--------------------------------------------------------------------------*)
(*     FileExistsProc --- Handle messages for password dialog.              *)
(*--------------------------------------------------------------------------*)
Function FileExistsProc( DlgWin: hWnd; DlgMessage: UINT; DlgWParam: WPARAM; DlgLParam: LPARAM ): BOOL; StdCall;
Var
   Msg: String;
Begin
   Result := True;
   Case DlgMessage Of
      WM_INITDIALOG:
         Begin
            CenterDialog( DlgWin );
            Msg := CurrentFile + STR_EXISTS;
            SendMessage( GetDlgItem( DlgWin, CM_EDITS ), WM_SETTEXT, 0, LongInt( pChar( Msg ) ) );
         End;

      WM_COMMAND:
         Case LOWORD( DlgWParam ) Of
            CM_YES, CM_NO:
               Begin
                  (* No ask Overwrite checked, show results in
                     affected option buttons *)
                  If SendMessage( GetDlgItem( DlgWin, CM_NOASK ), BM_GETCHECK, 0, 0 ) = 1 Then
                  Begin
                     (* Set CM_CONFIRM to Unchecked *)
                     SendMessage( GetDlgItem( MainWin, CM_CONFIRM ), BM_SETCHECK, 0, 0 );
                     If LOWORD( DlgWParam ) = CM_YES Then
                     Begin
                        (* Set CM_OVERWRITE to checked *)
                        SendMessage( GetDlgItem( MainWin, CM_OVERWRITE ), BM_SETCHECK, 1, 0 );
                        (* Don't ask for OverWrites *)
                        OverWriteMode := 0;
                        OverWriteFile := True;
                     End Else
                     Begin   (* CM_SKIP *)
                        (* Set CM_SKIP to checked *)
                        SendMessage( GetDlgItem( MainWin, CM_SKIP ), BM_SETCHECK, 1, 0 );
                        (* Don't ask... skip *)
                        OverWriteMode := 1;
                        OverWriteFile := False;
                     End;
                  End Else // if sendmessage
                     (* Set Mode *)
                     If OverWriteMode > 0 Then
                        OverWriteFile := LOWORD( DlgWParam ) = CM_Yes;
                  EndDialog( DlgWin, LOWORD( DlgWParam ) );
                  Exit;
               End;
         End;  //CASE DlgWParam
   End;  // CASE WM_COMMAND
   Result := False;
End;

(*--------------------------------------------------------------------------*)
(*     PasswordProc --- Handle messages for password dialog.                *)
(*--------------------------------------------------------------------------*)
Function PwdProc( DlgWin: hWnd; DlgMessage: UINT; DlgWParam: WPARAM; DlgLParam: LPARAM ): BOOL; StdCall;
Begin
   Case DlgMessage Of
      WM_INITDIALOG:
         Begin
            (* Center dialog on screen.       *)
            CenterDialog( DlgWin );
            (* Set input focus to the first   *)
            (* edit field.                    *)
            SetFocus( GetDlgItem( DlgWin, CM_EDITS ) );
         End;

      WM_COMMAND:
         Case LOWORD( DlgWParam ) Of
            CM_YES:
               Begin
                  PWLen := GetDlgItemText( DlgWin, CM_EDITS, Password, fsMaxPassword );
                  Password[ PwLen ] := #0;
                  EndDialog( DlgWin, LOWORD( DlgWParam ) );
               End;
         End;  //CASE WM_COMMAND
   End;  //CASE DlgMessage
   Result := False;
END;

(*--------------------------------------------------------------------------*)
(*     MainDialogProc --- Handle messages for main window dialog.           *)
(*--------------------------------------------------------------------------*)
Function MainDialogProc( DlgWin: hWnd; DlgMessage: UINT; DlgWParam: WPARAM; DlgLParam: LPARAM ): BOOL; StdCall;
Var
   EditLen: LONGINT;
   cm1:     Integer;
Begin (* MainDialogProc *)
   RESULT := True;
   Case DlgMessage Of
      WM_INITDIALOG:
         Begin
            (* Set the icon for the program.  *)
            SetClassLONG( DlgWin, GCL_HICON, LoadIcon( hInstance, 'MnIcon' ) );

            If NOT FGUseSP Then   // Stored path
               GetCurrentDirectory( MAX_PATH, ExtPath )
            Else
               Move( MPVstoredpath[0], ExtPath[0], StrLen( MPVstoredpath )+ 1 );

            cm1 := GetDlgItem( DlgWin, CM_RUNAPP );  // the run... checkbox
            If NOT FGUseCL Then   // Command Line
               ShowWindow( cm1, SW_HIDE )   //## no cmd-line, so hide the run... checkbox
            Else Begin
               //## give the run... checkbox a title
               SendMessage( cm1, WM_SETTEXT, 0, Integer( GetRunCheckBoxTitle ) );

               //## check it by default
               SendMessage( cm1, BM_SETCHECK, 1, 0 );
               If NOT FGAllowDCL Then   //## if not allowed to disable the cmd-line, hide the run... cb
                  ShowWindow( cm1, SW_HIDE );
            End;
            SendMessage( GetDlgItem( DlgWin, CM_EDITS ), WM_SETTEXT, 0, LongInt( @ExtPath ) );
            (* Hilite string in Edit1 control *)
            SendMessage( GetDlgItem( DlgWin, CM_EDITS ), EM_SETSEL, 0, $7fff );

            (* Set overwrite mode *)
            SendMessage( GetDlgItem( DlgWin, FGDefOwm ), BM_SETCHECK, 1, 0 );
            OverWriteMode := FGDefOwm - 501; //## and calculate the command for the overwrite-mode

            If FGHideOWM Then  //## if we do not want to select another overwrite-mode, destroy the controls
            Begin
               DestroyWindow( GetDlgItem( DlgWin, CM_OVERWRITE ) );
               DestroyWindow( GetDlgItem( DlgWin, CM_SKIP ) );
               DestroyWindow( GetDlgItem( DlgWin, CM_CONFIRM ) );
               DestroyWindow( GetDlgItem( DlgWin, CM_GROUP ) );
            End;

            SetWindowText( DlgWin, MPVCaption );

            CenterDialog( DlgWin );   //## now center the dialog

            (* Fill the list box *)

            //## added a parameter to the processarchive cause we have two listboxes (only 1 is visible)
            ProcessArchive( DlgWin, CM_LIST, True );   //## first fill the multisel-listbox

            If FGAllowSel Then   //## if the user can (de)select files
               ShowWindow( GetDlgItem( DlgWin, CM_LBSHOW ), SW_HIDE )   //## hide the singlesel-listbox
            Else Begin //## else
               ShowWindow( GetDlgItem( DlgWin, CM_LIST ), SW_HIDE );   //## hide the multisel-lb
               ProcessArchive( DlgWin, CM_LBSHOW, True );    //## and read the archive-contents to the singlesel-lb
            End;

            (* Select all items in the listbox *)
            SendMessage( GetDlgItem( DlgWin, CM_LIST ), LB_SETSEL, 1, -1 );

            If FGAutoRun Then   // do the extraction automatically, if autorun = true
               SendMessage( DlgWin,  WM_COMMAND, CM_YES, 0 );

            (* Assign to a global *)
            MainWin := DlgWin;
         End;

      (* Handle button presses, etc. *)

      WM_COMMAND:
         Case LOWORD( DlgWParam ) Of
            //## added the ability to select a extract directory
            // Modified by Deepu Chandy Thomas //
            CM_BROWSE:
               Begin
                  If SelectDir( DlgWin , @ExtPath[0] ) Then
                     If DialogBox( hInstance, 'DLGNEW', DlgWin, @NewDirProc ) <> ID_OK Then
                        GetDlgItemText( DlgWin , CM_EDITS , ExtPath , MAX_PATH );
                  SetDlgItemText( DlgWin , CM_EDITS , ExtPath );
               End;

            // show the copyright information
            CM_ABOUT:
               MessageBox( DlgWin, STR_ABOUT_MSG, STR_ABOUT_CAP, MB_OK );

            CM_OVERWRITE:
               OverWriteMode := 0;

            CM_SKIP:
               OverWriteMode := 1;

            CM_CONFIRM:
               OverWriteMode := 2;

            CM_YES:
               Begin
                  //## if the user is not allowed to (de)select files from the archive, then
                  //   select them all; hide the single-sel-lb and show the multisel-lb
                  //   to have the ability to show the user what files have not been extracted
                  If NOT FGAllowSel Then
                  Begin
                     SendMessage( GetDlgItem( DlgWin, CM_LIST ), LB_SETSEL, 1, -1 );
                     ShowWindow( GetDlgItem( DlgWin, CM_LBSHOW ), SW_HIDE );
                     ShowWindow( GetDlgItem( DlgWin, CM_LIST ), SW_SHOW );
                  End;
                  EditLen := GetDlgItemText( DlgWin, CM_EDITS, ExtPath, MAX_PATH );
                  ExtPath[EditLen] := #0;
                  RemoveDirTail( ExtPath ); //## cut a final \
                  If ExtPath = '' Then
                  Begin
                     GetCurrentDirectory( MAX_PATH, ExtPath );
                     SendMessage( GetDlgItem( DlgWin, CM_EDITS ), WM_SETTEXT, 0, LongInt( @ExtPath ) );
                  End;

                  EnableChildren( DlgWin, False );

                  //   execute the command line (if any) and close the dialog
                  If ProcessArchive( DlgWin, CM_LIST, False ) Then
                  Begin
                     EnableChildren( DlgWin, True );
                     ExecuteCMD( SendMessage( GetDlgItem( DlgWin, CM_RUNAPP ), BM_GETCHECK, 0, 0 ) );
                     EndDialog( DlgWin, IDOK );
                  End;
                  EnableChildren( DlgWin, True );

                  If FGAutoRun Then
                     SendMessage( DlgWin, WM_COMMAND, CM_NO, 0 );
               End;

            CM_NO:
               Begin
                  EditLen := GetDlgItemText( DlgWin, CM_EDITS, ExtPath, MAX_PATH );
                  Extpath[ EditLen ] := #0;
                  If FileExists( ExtPath ) Then
                     SetCurrentDirectory( ExtPath );
                  EndDialog( DlgWin, LOWORD( DlgWParam ) );
                  Exit;
               End;
         End;   // CASE WM_COMMAND
   End;   // CASE DlgMessage
   Result := False;
End   (* MainDialogProc *);
(*--------------------------------------------------------------------------*)

// window proc for the new-directory dialog
Function NewDirProc( DlgNew: hWnd; DlgMessage: UINT; DlgWParam: WPARAM; DlgLParam: LPARAM ): BOOL; StdCall;
Var
   Buffer: Array [0..MAX_PATH + 1 ] Of Char;
Begin (* newdirProc *)
   Result := False;
   Case DlgMessage Of
      WM_INITDIALOG:
         Begin
            AppendDirTail( ExtPath );
            (* Center dialog on screen.       *)
            CenterDialog( DlgNew );
            SetDlgItemText( DlgNew, CM_ND_PATH, ExtPath ); // and set it in this dialog
            SetFocus( GetDlgItem( DlgNew, CM_ND_EDIT ) );
         End;

      (* Handle button presses, etc. *)
      WM_COMMAND:
         Case LOWORD( DlgWParam ) Of
            CM_YES:
               Begin
                  // make subdirs;
                  GetDlgItemText( DlgNew, CM_ND_EDIT, Buffer, MAX_PATH );
                  If StrLen( Buffer ) > 0 Then
                     If NOT ForceDirs( ExtPath, Buffer ) Then
                     Begin
                        MessageBox( DlgNew, STR_EDIRECTORY, STR_E, MB_ICONERROR );
                        Exit;
                     End Else
                     Begin
                        If StrLen( ExtPath ) + StrLen( Buffer )+ 1 < MAX_PATH Then
                           Move( Buffer[0] , ExtPath[ StrLen( ExtPath ) ] , StrLen( Buffer )+ 1 );
                     End;
                  EndDialog( DlgNew, LOWORD( DlgWParam ) );
               End;

            CM_NO:
               EndDialog( DlgNew, LOWORD( DlgWParam ) );
         End (* CASE *);
   End (* CASE *);
End   (* newdirProc *);


Initialization
   New( CRC32Table );
   GetMem( MPVStoredPath, MAX_PATH + 1 );
   GetMem( MPVCommandLine, MAX_PATH + 1 );
   GetMem( MPVCaption, MAX_PATH + 1 );
   GetMem( MPVFirstMessage, MAX_PATH + 1 );
   FillChar( MPVCaption[0], MAX_PATH + 1, 0 );
   Move( STR_APP[1], MPVCaption[0], Length( STR_APP ) );

Finalization
   FreeMem( MPVStoredPath );
   FreeMem( MPVCommandLine );
   FreeMem( MPVCaption );
   FreeMem( MPVFirstMessage );

   If (InFile <> 0) And NOT CloseHandle( InFile ) Then
      MessageBox( 0, STR_CANNOTCLOSE, STR_E , MB_ICONERROR );
   Dispose( CRC32Table );

END.
