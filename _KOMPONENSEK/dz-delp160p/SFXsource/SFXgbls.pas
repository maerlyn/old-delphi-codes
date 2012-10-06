(******************************************************************)
(* Copyright 1997, Microchip Systems / Carl Bunton                *)
(* e-mail: Twojags@cris.com                                       *)
(* Web-page: http://www.concentric.net/~twojags                   *)
(*                                                                *)
(* Last modified by Markus Stephany mirbir.st@t-online.de         *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* SFX for DelZip v1.6                                            *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

// bug with data_descriptor fixes in function readheader by mst 01/23/98. thanks to eric !
// correct filetime will now be set, thanks to angus johnson, ajohnson@rpi.net.au 15/03/98

// credits to thomas hoelzer (thoelzer@cityweb.de) for correcting an error while extraction of directory names 090198
Unit SFXgbls;

{$A-}

INTERFACE

Uses  Messages, Windows;

Type
  TLocal = Record
     SignAtr        : LONGINT;
     VerNum         : WORD;  (* VersionMadeBy *)
     BitFlag        : WORD;
     CompressType   : WORD;
     FileDate       : LONGINT;
     crc32          : DWORD;
     PackedSize     : LONGINT;
     UnpackedSize   : LONGINT;
     FilenameLen    : WORD;
     ExtraFieldLen  : WORD;
  End;


Type
  pCRC32Table = ^TCRC32;
  TCRC32      = Array[0..256] Of DWORD;

Var
  CRC32Table : pCRC32Table;
  CRC32Val   : DWORD;   { Running CRC (32 bit) value }

Var
  key : Array[0..2] Of LongInt;
  HEADER_SIGNATURE: LongInt;

Const
  WSIZE         = 32768;      { window size--must be a power OF two, and at least 32k}
  RAND_HEAD_LEN = 12;
  CRC_MASK      = $FFFFFFFF;

Var
  Bytes_To_Go : LongInt;
  InFile      : THandle;
  OutFile     : THandle;
  Header      : TLocal;

  Function  ReadHeader: BOOLEAN;

  Function  ProcessArchive( DlgWin: hWnd; lb: Integer; FillListBox: BOOLEAN ): Boolean;
  Function  decrypt_pw( Encrypt_Head: pChar; EncHead_len: BYTE; BitFlag: WORD; CRC,
                          FileDate: LONGINT; password: String ): BOOLEAN;
  Function  decrypt_byte: Integer;
  Procedure seedk( passwd: String );
  Procedure UDK( c: Byte );
  Procedure Make_CRC32Table;
  Function  UpdC32( Octet: Byte; Crc: DWORD ): DWORD;

IMPLEMENTATION

Uses  SFXinflt, Dialog, SFXmisc, SFXStrings;


(*--------------------------------------------------------------------------*)
Function ReadHeader: BOOLEAN;
Var
   BytesRead: DWORD;
Begin
   Try
      Header.SignAtr := 0;
      ReadFile( InFile, Header, SizeOf( Header ), BytesRead, nil );
      Result := (Header.SignAtr = HEADER_SIGNATURE) Or (Header.SignAtr = $50000000);
      If Result Then
         Bytes_To_Go := Header.PackedSize
      Else Begin
         Bytes_To_Go := 0;
         If header.signatr = $08074B50 Then
         Begin //## if there is a data-descriptor
            FSeek( FilePos + 16, FILE_BEGIN );    //## read over it
            Result := ReadHeader;                 //## and try again
         End;
      End;
   Except
      Result := False;
   End;
End (* ReadHeader *);




(*--------------------------------------------------------------------------*)
Function ProcessArchive( DlgWin: hWnd; lb: Integer; FillListBox: Boolean ): Boolean;
Var
   FN         : pChar;
   IsPassword : Boolean;
   EncryptHDR : pChar;
   Directory  : String;
   Filename   : String;
   ExtractFile: String;
   hWndList   : hWnd;
   i          : BYTE;
   SavePos    : LongInt;
   BytesRead  : DWORD;
   Msg        : TMsg;

Label ByPass;

Begin
   Result := False;

   // We store the local header signature in the file one greater than
   // the real value, so we don't falsely detect a zip file entry inside
   // the SFX code of the .EXE file
   HEADER_SIGNATURE := $04034B51; // intentionally 1 higher than the real sig
   Dec( HEADER_SIGNATURE );       // fix the sig in RAM only

   IsPassword := False; // default
   If FSeek( StartOfFile, FILE_BEGIN ) = $FFFFFFFF Then
   Begin
      MessageBox( 0, STR_EARCHIVE, STR_E, MB_ICONERROR );
      Exit;
   End;
   (* Get handle of ArchiveFileListbox *)
   hWndList := GetDlgItem( DlgWin, lb ); //## changed
   GetMem( FN, 256 + 2 );
   Try
      (* Start search at beginning of listbox *)
      Index := -1;
      HasStoredPaths := False;
      (* Clear the listbox *)
      If FillListBox Then
         SendMessage( hWndList, LB_RESETCONTENT, 0, 0 );
      While ReadHeader Do
      Begin
         SavePos := FilePos;
         ReadFile( InFile, FN^, Header.FilenameLen, BytesRead, nil );
         Filename := pCharToStr( FN, Header.FilenameLen );
         If Filename = '' Then
         Begin
            MessageBox( 0, STR_INVALIDNAME, STR_E, MB_ICONERROR );
            Exit;
         End;
         If NOT HasStoredPaths Then
            If (Pos( BSL, Filename ) > 0) Or (Pos( '/', Filename ) > 0) Then
               HasStoredPaths := True;

         If FillListBox Then
         Begin
            // OemToChar( pChar( Filename ), pChar( Filename ) );
            (* Add each string to the listbox *)
            SendMessage( hWndList, LB_ADDSTRING, 0, LONGINT( pChar( Filename ) ) );
         End Else
         Begin
            // OemToChar( pChar( Filename ), pChar( Filename ) );   // find filname in listbox -> it's not the same as in archive
            If IsSelected( hWndList, pChar( Filename ) ) Then
            Begin
               // CharToOem( pChar( Filename ), pChar( Filename ) );   // restore original -> filename in archive
               (* Default *)
               IsPassword := False;
               If (Header.BitFlag AND 1) = 1 Then
               Begin
                  // password protected file
                  Dec( Bytes_To_Go, RAND_HEAD_LEN );
                  Try
                     GetMem( EncryptHDR, RAND_HEAD_LEN * 2 );
                     ReadFile( InFile, EncryptHDR^, RAND_HEAD_LEN, BytesRead, nil );
                     (* make a working copy of encrypted header in upper half of buffer *)
                     Move( EncryptHDR[0], EncryptHDR[RAND_HEAD_LEN], RAND_HEAD_LEN );
                     Try
                        If PW <> '' Then
                           IsPassword := decrypt_pw( EncryptHDR, RAND_HEAD_LEN, Header.BitFlag,
                                                    Header.CRC32, Header.FileDate, PW );

                         If NOT IsPassword Then
                            For i := 0 To 2 Do // 3 shots at getting pwd correct
                            Begin
                               DialogBox( hInstance, STR_PDLG, 0, @PwdProc );
                               PW := PCharToStr( Password, PWLen );
                               IsPassword := decrypt_pw( EncryptHDR, RAND_HEAD_LEN, Header.BitFlag, Header.CRC32,
                                                         Header.FileDate, PW );

                               If IsPassword Then Break;
                            End;
                     Finally
                        FreeMem( EncryptHDR );
                     End;
                  Except
                  End;
               End Else
                  (* Not pw protected.. set value to extract *)
                  IsPassword := True;

               If IsPassword Then
               Begin
                  // OemToChar( pChar( Filename ), pChar( Filename ) );   // now work on disk with correct filename
                  ExtractFile := pCharToStr( RemoveDirTail( ExtPath ), StrLen( AppendDirTail( ExtPath ) ) )
                         +BSL + Filename;  // EWE: add '\' //## i don't know why, but we really need it sometimes

                  If FileExists( pChar( ExtractFile ) ) Then
                  Begin
                     Case OverWriteMode Of
                        //0:              (* Overwrite *)
                        1: Begin          (* Skip *)
                              SendMessage( hWndList, LB_SETSEL, 0, Index );
                              Goto ByPass;
                           End;

                        2: Begin          (* Confirm *)
                              CurrentFile := ExtractFilename( ExtractFile );
                              DialogBox( hInstance, STR_FILEEXISTS, 0, @FileExistsProc );
                              If NOT OverWriteFile Then
                              Begin
                                 SendMessage( hWndList, LB_SETSEL, 0, Index );
                                 Goto ByPass;
                              End;
                           End;
                     End;
                  End Else
                  Begin
                     Directory := ExtractFilePath( ExtractFile );
                     //MessageBox(0, PCHAR('trying to force dir: ' + Directory), 'EWE', mb_OK);
                     ForceDirectories( Directory );  (* removes trailing dir char *)
                     If NOT FileExists( pChar( Directory ) ) Then
                     Begin
                        MessageBox( 0, pChar( Directory ), STR_EDIRECTORY , MB_ICONERROR );
                        Break;
                     End;
                  End;
                  //MessageBox(0, PCHAR('trying to create file: ' + ExtractFile), 'EWE', mb_OK);
                  OutFile := CreateFile( pChar( ExtractFile ), GENERIC_WRITE, FILE_SHARE_WRITE, nil,
                                         CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );

                  If OutFile <> INVALID_HANDLE_VALUE Then
                  Begin
                     Try
                        Try
                           PeekMessage( Msg, 0, 0, 0, PM_REMOVE );
                           TranslateMessage( Msg );
                           DispatchMessage( Msg );

                           CRC32Val := CRC_MASK;
                           Bytes_To_Go := Header.PackedSize;      (* assign to global *)

                           If (Header.BitFlag AND 1) = 1 Then
                              Dec( Bytes_To_Go, RAND_HEAD_LEN );
                           Case Header.CompressType Of
                              0: UnStore;    (* Stored *)
                              8: Inflate;    (* Inflate *)
                              Else
                              Begin
                                 MessageBox( 0, STR_ETYPE, STR_E, MB_ICONERROR );
                                 Exit;
                           End;
                        End;
                        (* Un-HiLight file in listbox *)
                        If DWORD( Header.CRC32 ) = (CRC32Val XOR CRC_MASK) Then
                           SendMessage( hWndList, LB_SETSEL, 0, Index );
                     Except
                        //MessageBox(0, 'Error...', 'Error', mb_OK)
                     End;
                  Finally
                     FileSetDate( Outfile , Header.FileDate ); //## added to set the correct file time
                     If NOT CloseHandle( OutFile ) Then
                        MessageBox( 0, pChar( ExtractFile ) {FN}, STR_CANNOTCLOSE, MB_ICONERROR );
                  End;
               End Else
               Begin
                  //   Erweiterung von Thomas Hölzer
                  // Wenn der gerade bearbeitete Eintrag ein Verzeichnis ist,
                  // wird keine Messagebox "Kann Datei nicht öffnen" angezeigt und die Selektierung des
                  // Eintrags entfernt, so daß auch am Ende des Extraktionsvorgangs keine
                  // Fehler gemeldet werden.

                  If GetFileAttributes( pChar( ExtractFile ) ) = GetFileAttributes( pChar( ExtractFile ) )
                        and NOT FILE_ATTRIBUTE_DIRECTORY Then
                     MessageBox( 0, pChar( ExtractFile ) {FN}, STR_CANNOTOPEN, MB_ICONERROR )
                  Else
                     SendMessage( hWndList, LB_SetSel, 0, index );
                    //   Ende Erweiterung Thomas
               End;
            End Else
               (* Terminate file processing if not password *)
               Break;
         End;
      End;

   ByPass: FSeek( SavePos + SizeOf( Header ) + Header.FilenameLen + {Header.CommentLen +} Header.PackedSize, FILE_BEGIN );
   End;
   If IsPassword And NOT FillListBox Then
   Begin
      If SendMessage( hWndList, LB_GETSELCOUNT, 0, 0 ) = 0 Then
      Begin
         If (NOT FGAutoRun) And FGShowSuccess Then
            MessageBox( 0, STR_ALLEXT, STR_OK, mb_OK );
         Result := True;
      End Else
      Begin
         MessageBox( 0, STR_NOTSELEXT, STR_E, MB_ICONERROR );
         Result := False;
      End
   End Else
      (* if listbox empty, exit program *)
      If FillListBox Then
         If SendMessage( hWndList, LB_GETCOUNT, 0, 0 ) = 0 Then
            MessageBox( 0, STR_EARCHIVE, STR_E, MB_ICONERROR );
   Finally
      Freemem( FN );
   End;
End (* ProcessArchive *);

(*--------------------------------------------------------------------------*)
Procedure Make_CRC32Table;
Var
    i, j: WORD;
    r:    DWORD;
Const
    CRCPOLY    = $EDB88320;
    UCHAR_MAX  = 255;
    CHAR_BIT   = 8;
Begin
    For i := 0 To UCHAR_MAX Do
    Begin
       r := i;
       For j := CHAR_BIT DownTo 1 Do
          If (r And 1) > 0 Then
             r := (r SHR 1) XOR CRCPOLY
          Else
             r := r SHR 1;
       CRC32Table[i] := r;
    End;
End;

(*--------------------------------------------------------------------------*)
Function UpdC32( Octet: Byte; Crc: DWORD ): DWORD;
Begin
   Result := CRC32TABLE[ Byte( Crc XOR DWORD( Octet ) ) ] XOR ( (Crc SHR 8) AND $00FFFFFF );
End;

(*--------------------------------------------------------------------------*)
(* Update the encryption keys with the next byte of plain text *)
Procedure UDK( c: Byte );
Begin
   key[0] := UpdC32( c, key[0] );
   key[1] := key[1] + key[0] AND $000000ff;
   key[1] := key[1] * 134775813 + 1;
   key[2] := UpdC32( HIBYTE( HIWORD( key[1] ) ), key[2] );
End;

(*--------------------------------------------------------------------------*)
(* Initialize the encryption keys and the random header according to
     the given password. *)
Procedure seedk( passwd: String );
Var
    i: BYTE;
Begin
    key[0] := 305419896;
    key[1] := 591751049;
    key[2] := 878082192;
    For i := 1 To LENGTH( passwd ) Do
        udk( BYTE( passwd[i] ) );
End;

(*--------------------------------------------------------------------------*)
(* Return the next byte in the pseudo-random sequence *)
Function decrypt_byte: Integer;
Var
    temp: WORD;
Begin
    temp   := Word( key[2] or 2 );
    Result := Integer( Word( (temp * (temp XOR 1)) SHR 8) AND $ff );
End;

(*--------------------------------------------------------------------------*)
Function decrypt_pw( Encrypt_Head: pChar; EncHead_len: Byte;
                BitFlag: WORD; CRC, FileDate: LongInt; password: String ): BOOLEAN;
Var
    i,c,b: BYTE;
Begin
    Result := False;
    If password = '' Then
       Exit;
    seedk( Password );
    For i := 0 to EncHead_len - 1 Do
    Begin
       c := Byte( Encrypt_Head[i + EncHead_len] ) XOR decrypt_byte;
       udk( c );
       Encrypt_Head[i] := Char( c );
    End;

    (* version 2.0+ *)
    b := Byte( Encrypt_Head[EncHead_len - 1] );

    If NOT ((BitFlag AND 8) = 8) Then
    Begin
       IF b = HIBYTE(HIWORD( crc ) ) Then
            Result := True;
    End Else
    Begin
       If b = LOWORD( FileDate ) SHR 8 Then
          Result := True;
    End;
End;
(*--------------------------------------------------------------------------*)

END.
