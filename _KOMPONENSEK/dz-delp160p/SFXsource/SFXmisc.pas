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
(*                                                                *)
{ modifications marked with ##FR are enhancements and bug fixes by
  Frank Reichert F.Rei@gmx.de, thanks ! }

Unit SFXmisc;

INTERFACE

Uses Messages, Windows;

Const
  (* Dialog Control-IDs *)
  //general
  CM_YES        = 1;
  CM_NO         = 2;
  CM_EDITS      = 101;

  // file already exist : don't ask again
  CM_NOASK      = 401;

  CM_LIST       = 301;
  CM_OVERWRITE  = 501;
  CM_SKIP       = 502;
  CM_CONFIRM    = 503;
  CM_ABOUT      = 103;
  CM_BROWSE     = 775;
  CM_RUNAPP    = 1007;
  CM_LBSHOW     = 302;
  CM_GROUP      = 509;

  // shbrowse.. dialog (new dir button )
  CM_NEW       = 1277;

  // new dir dialog
  CM_ND_PATH   = 3001;
  CM_ND_EDIT   = 3002;

Const
  fsMaxPassword = 80;

Var
  Index         : LONGINT;
  HasStoredPaths: Boolean;
  ExtPath       : Array[0..MAX_PATH] Of Char;
  Password      : Array[0..fsMaxPassword - 1] Of Char;
  PW            : String;
  PWLen         : LONGINT {BYTE};
  FilePos       : DWORD;
  StartOfFile   : LONGINT;
  OverWriteMode : BYTE;
  OverWriteFile : BOOLEAN;
  CurrentFile   : String;
  MainWin       : hWnd;


Procedure FileSetDate ( Handle : Integer ; Age : Integer);
//## this has been added 03/15/98 to set the correct file time of  the extracted files (in sfxgbls.pas, line ~300
//## thanks to angus johnson, ajohnson@rpi.net.au

Procedure Unstore;  {(FileLen: LONGINT);}
Procedure CenterDialog( Wnd: hWnd );
Procedure ForceDirectories( Dir: String ); // RCV04
Function  FSeek( Offset: DWORD; MoveMethod: WORD ): DWORD;  //##FR was procedure
Procedure Crc32_Buf( str: pChar; len: Integer; Var crc: DWORD );
Function  IsSelected( hWndList: hWnd; Filename: pChar ): Boolean;
Function  ExtractFileName( FileName: String ): String;
Function  PCharToStr( p: pChar; Len: WORD ): String;
Function  Min( Const I1, I2: LongInt ): LongInt;
function  StrLen( Str: pChar ): Cardinal; //##FR: was FUNCTION StrLen(Str: PCHAR): WORD;
Function  AppendDirTail( sDir: pChar ) : pChar;
Function  RemoveDirTail( sDir: pChar ) : pChar;
Function  FileExists( Filename: pChar ): Boolean;
Function  DirExists( Const Name: pChar ): THandle;
Function  ExtractFilePath( Filename: String ): String;
Function  StrRScan( Str: pChar; Chr: Char ): pChar;
Function  StrScan( Str: pChar; Chr: Char ): pChar;

IMPLEMENTATION

Uses SFXgbls;

(* to set the correct file date an time on extraction *)
Procedure FileSetDate( Handle: Integer; Age: Integer );
Var
  LocalFileTime, FileTime: TFileTime;
Begin
    DosDateTimeToFileTime( Age shr 16, Age and $FFFF, LocalFileTime );
    LocalFileTimeToFileTime( LocalFileTime, FileTime );
    SetFileTime( Handle, nil, nil, @FileTime );
End;

(*--------------------------------------------------------------------------*)
(*     CenterDialog --- Center dialog on screen.                            *)
(*--------------------------------------------------------------------------*)
Procedure CenterDialog( Wnd: hWnd );
Var
   R : TRect;
Begin (* CenterDialog *)
   GetWindowRect(Wnd , R);
   R.Left := (GetSystemMetrics( sm_CXScreen ) - R.right  + R.left) DIV 2;
   R.Top  := (GetSystemMetrics( sm_CYScreen ) - R.bottom + R.top) DIV 2;
   SetWindowPos( Wnd, 0, R.left, R.top, 0, 0, Swp_NoSize OR Swp_NoZOrder );
End   (* CenterDialog *);

(*--------------------------------------------------------------------------*)
Procedure Crc32_Buf( str: PCHAR; len: INTEGER; Var crc: DWORD );
Begin
   While len > 0 Do
   Begin
      crc := UpdC32( Byte( str^ ), crc );
      Inc( str );
      Dec( len );
   End;
End;

(*--------------------------------------------------------------------------*)
Function Min( Const I1, I2: LONGINT ): LONGINT;
Begin
   If I2 < I1 Then
      Min := I2
   Else
      Min := I1;
End;

(*--------------------------------------------------------------------------*)
Function IsSelected( hWndList: hWnd; Filename: pChar ): Boolean;
Begin
   Index  := SendMessage( hWndList, LB_FINDSTRINGEXACT, Index, LONGINT( Filename ) );
   RESULT := SendMessage( hWndList, LB_GETSEL, Index, 0 ) > 0;
End (* IsSelected *);

(*--------------------------------------------------------------------------*)
Procedure Unstore;  {(FileLen: LONGINT);}
Var
  i, NumBytes : DWORD;
  OutBuf   : PCHAR;
Begin
  GetMem( OutBuf, Min( Bytes_To_Go, WSIZE ) + 2 );
  Try
     While Bytes_To_Go > 0 Do
     Begin
        ReadFile( InFile, OutBuf^, Min( Bytes_To_Go, WSIZE ), NumBytes, nil );
        Dec( Bytes_To_Go, NumBytes );
        If (Header.BitFlag AND 1) = 1 Then
           For i := 0 To NumBytes - 1 Do
           Begin
              OutBuf[i] := Char( Byte( OutBuf[i] ) XOR decrypt_byte );
              {update_keys}UDK( Byte( OutBuf[i] ) );
           End;
        WriteFile( OutFile, OutBuf^, NumBytes, NumBytes, nil );
        Crc32_Buf( outbuf, NumBytes, Crc32Val );
     End;
  Finally
     Dispose( OutBuf );
  End;
End (* Unstore *);

(*--------------------------------------------------------------------------*)
Function StrLen( Str: pChar ): Cardinal; Assembler; //##FR
Asm
        MOV     EDX,EDI
        MOV     EDI,EAX
        MOV     ECX,0FFFFFFFFH
        XOR     AL,AL
        REPNE   SCASB
        MOV     EAX,0FFFFFFFEH
        SUB     EAX,ECX
        MOV     EDI,EDX
End;

(*--------------------------------------------------------------------------*)
Function FSeek( Offset: DWORD; MoveMethod: WORD ): DWORD; //##FR
Begin
   // We do not check for an errr here, but if there is one FSeek and FilePos are $FFFFFFFF
   FilePos := SetFilePointer( InFile, Offset, nil, MoveMethod );
   FSeek := FilePos;
End;

(*--------------------------------------------------------------------------*)
Function ExtractFilePath( Filename: String ): String;
Var
  i: Integer;
Begin
  (* Handle archive relative paths *)
  i := Length(Filename);
  If (i = 3) AND (Pos(':', Filename) > 0) Then
     RESULT := Filename
  Else
  Begin
     While (i > 0) And NOT (Filename[ i ] In ['\', '/', ':']) Do
        Dec( i );

     If (Filename[ i ] = '\') Or (Filename[ i ] = '/') Then
        If i <> 3 Then
           dec(i)
        Else
           If Filename[ 2 ] <> ':' Then
              dec( i );
     RESULT := Copy( Filename, 1, i );
  End;
End;

(*--------------------------------------------------------------------------*)
Function ExtractFileName( FileName: String ): String;
Var
  I: Integer;
Begin
  (* Handle archive relative paths *)
  I := Length( FileName );
  While (I > 0) And NOT (FileName[I] In ['\', '/', ':'])
     Do Dec( I );
  Result := Copy( FileName, I + 1, 255 );
End;

(*--------------------------------------------------------------------------*)
Function FixDirChar( s: pChar ): pChar;
Var
   i: BYTE;
Begin
   For i := 0 To StrLen(s) Do
      If s[i] = '/' Then
         s[i] := '\';
   RESULT := s;
End;

(*--------------------------------------------------------------------------*)
Function FileExists( Filename: pChar ): BOOLEAN; //## this func has changed a bit
Var
   SearchRec:  TWin32FindData;
   i, sl:      Integer;
   Handle:     THandle;
   FN:        pCHAR;
Begin
   If Filename[ StrLen( Filename )- 1 ] = '\' Then
   begin
      FN := nil;
      try
         GETMEM( FN, 255 );
         sl := StrLen( FileName );
         For i := 0 to sl - 1 do FN[ i ] := Filename[ i ];
         if (sl = 3) and (FN[ 0 ] >= 'A') and (FN[ 0 ] <= 'Z') and (FN[ 1 ] = ':') then
         begin
            HANDLE := DirExists( FN );
         end else
         begin
            FN[ sl  ]    := '*';
            FN[ sl + 1 ] := '.';
            FN[ sl + 2 ] := '*';
            FN[ sl + 3 ] := #0;
            HANDLE := FindFirstFile( FixDirChar( FN ), SearchRec );
         end;
      Finally
         DISPOSE( FN );
      End;
   End else
      HANDLE := FindFirstFile( FixDirChar( Filename ), SearchRec );

   Result := HANDLE <> INVALID_HANDLE_VALUE;
   FindClose( HANDLE );
End;

function DirExists( Const Name: pChar ): THandle;
var
   Code: DWORD;
begin
   Result := INVALID_HANDLE_VALUE;
   if Name[ 0 ] <> #0 then
   begin
      Code := GetFileAttributes( Name );
      if (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0) then
         Result := FILE_ATTRIBUTE_DIRECTORY;
   end;
end;

(*--------------------------------------------------------------------------*)
(* Set the contents of a STRING *)
Function PCharToStr( p: pChar; Len: WORD ): String;
Var
   s: String;
Begin
  SetLength( s, Len );
  Move( p^, s[1], Len );
  If POS( #0, s ) > 0 Then
     SetLength( s, POS( #0, s ) - 1 );
  RESULT := s;
End (* SetString *);

(*--------------------------------------------------------------------------*)
Procedure ForceDirectories( Dir: String );
Begin
  If Dir[ Length(Dir) ] = '\' Then
     SetLength( Dir, Length( Dir ) - 1 );
  If (Length( Dir ) < 3) Or FileExists( pChar( Dir ) ) Then
     EXIT;
  ForceDirectories( ExtractFilePath( Dir ) );
  MkDir( Dir );
End;

(*--------------------------------------------------------------------------*)
Function AppendDirTail( sDir: pChar ): pChar;  //##FR modified
Var i: WORD;
Begin
   i := StrLen( sDir );
   If (sDir[ i - 1 ] <> '\') And (StrLen( sDir ) < max_path) Then
   Begin
      sDir[ i ] := '\';
      sDir[ i + 1 ] := #0;
   End;
   RESULT := sDir;
End;

(*--------------------------------------------------------------------------*)
Function RemoveDirTail( sDir: pChar ): pChar;
Var
   i: WORD;
Begin
   i := StrLen( sDir );

   If sDir[ i - 1 ] = '\' Then
      sDir[ i - 1 ] := #0;
   RESULT := sDir;
End;

Function StrRScan( Str: pChar; Chr: Char ): pChar;
Begin
   Result := nil;
   If StrLen( Str ) > 0 Then
   Begin
      Result := Str + StrLen( Str )- 1;
      Repeat
         If Result[0] = Chr Then
            Break;
         Dec( Result );
      Until Result = (Str - 1);
      If Result = (Str - 1) Then
         Result := nil;
   End;
End;

Function StrScan( Str: pChar; Chr: Char ): pChar;
Begin
   Result := nil;
   If StrLen( Str ) > 0 Then
   Begin
      Result := Str;
      Repeat
         If Result[0] = Chr Then
            Break;
         Inc( Result );
      Until Result[0] = #0;
      If Result[0] = #0 Then
         Result := nil;
   End;
End;

END.
