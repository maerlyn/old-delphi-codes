{$A-,B-,C-,D+,E-,F-,G+,H+,I-,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y-,Z1}
{$MINSTACKSIZE $00004000}
{$MAXSTACKSIZE $00100000}
{$IMAGEBASE $00400000}
{$APPTYPE GUI}
(******************************************************************)
(* ZipSFX                                                         *)
(* Copyright 1997, Carl Bunton  Twojags@cris.com                  *)
(*                                                                *)
(* DelZipSFX Copyright 1997 - 2000                                *)
(*                                                                *)
(* Modified by Markus Stephany  mirbir.st@t-online.de             *)
(*                                                                *)
(* Now maintained by Chris Vleghert                               *)
(* SFX for DelZip v1.6                                            *)
(* e-mail: cvleghrt@WorldOnline.nl                                *)
(* www:    http://www.geocities.com/SiliconValley/Orchard/8607/   *)
(* www:    http://members.tripod.lycos.nl/Vleghert/               *)
(******************************************************************)

// Changes RCV:
// Jan. 10, 1999  Adapted for D4 beta v0.99f=now v1.60
// Feb. 10, 1999  Changed the Initialization and Finalization sections
//                to include file close and CRC table.
//                ( The Crc table was not freed after an Halt. )
// Jun. 15, 2000  Added code to Dialog.pas to free a pidl, bug found by
//                Lucjan Lukasik
// Sep. 01, 2000  Added version Checked for Delphi 5 and BCB 4 and 5
// Oct. 09, 2000  Added DirExists to the function FileExists because
//                FindFirstFile does not work when there is no file on
//                a drive (e.g. an empty 'A' drive) extract to that drive
//                would not work, found by Clyde England clyde@conres.com.au


{ Notes:

the initial release of zipsfx comes from Carl Bunton (see above).

the first modifications came from Eric W. Engler, the author of the great freeware
delphi-vcl delzip that can handle zip-archives and -sfx's. (EEngler@zcsterling.com)

original zip-code comes from the infozip-group, they developped a free implementation
of the zip/unzip-code for unix and later for other platforms.
  Info-Zip home page:
  http://freesoftware.com/pub/infozip/Info-ZIP.html

regards, Markus Stephany
losheim am see, saarland, germany, jan 03 1998 - oct 31 1998

NOTE : !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                                                                       !
TO GET ZIPSFX WORKING CORRECTLY, YOU SHOULD FIRST COMPILE IT ONCE AND THEN SET THE CORRECT VALUE FOR   !
StartOfFile IN LINE 427 IN DIALOG.PAS. THIS MUST BE SET TO THE SIZE OF THE COMPILED EXE.               !
THIS VALUE CAN DIFFER DEPENDING ON THE DELPHI-VERSION (AND OTHER THINGS I DO NOT UNDERSTAND).          !
PLEASE USE THE INCLUDED SFX.DOF AS YOUR PROJECT'S OPTIONS FILE.                                        !
                                                                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

}

(* the structure of a zipsfx-file :
- zipsfx-executable code (0-xxxxx)
- signature "MPV"                        or                           zip-archive
-                |_ rest of the special header
-                                             |_ zip-archive

the structure of the special-header :
Byte 0..2   : signature "MPV"

Byte    3   : Bit 0 (val  1) : if set, user can disable running the command line after extraction (if any)
              Bit 1 (val  2) : if set, user can choose what files to extract
              Bit 2 (val  4) : if set, user cannot change the overwrite-mode (confirm, overwrite, skip)
              Bit3-4(val 8,16) : default-overwrite mode
                     0 : confirm overwriting existing files
                     8 : overwrite existing files
                    16 : skip existing files
              Bit5 (val  32) : internally used, if set, then do not check file size
              Bit6 (val  64) : if set, then automatically extract all files
              Bit7 (val 128) : if set, don't show success message ("all files have been extracted")
Byte	4   : flag array for future enhancements
Byte    5   : Version of MPV-Header ( currently 01 and not used in sfx )
Byte    6+7 : SizeOf ( MPV-Header )
Byte    8   : length of user-defined caption / 0=default caption
Byte 9..m   : the dialog's caption, if byte 8 <> 0 ( without terminating zero)
Byte  m+1   : length of default extraction path / 0=current dir
     m+2..n : the default-extraction-path, if byte m+1 <> 0 (dito)

              ++++added  march 01,98 if set to "><", then use  temp-dir

              ##FR: added 10/10/98 If the first two characters are "HK" the extraction-path
              will be read from the registry. If the registry-key doesn't exist, the
              default path will be set to the temp path. Either full names
              (HKEY_CURRENT_USER\...) or abbreviations (as known from INF-files)
              for the root keys HKCU, HKLM and HKU are supported.
              examples:
              "HKEY_CURRENT_USER\Software\Borland\Delphi\2.0\Library\SearchPath"
              "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\ProgramFilesDir"
              "HKCU\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\Personal"
              "HKCU\Software\Microsoft\Office\8.0\Excel\Microsoft Excel\AddIn Path"
              For subdirectories to be created use the pipe symbol "|", e.g.
              "HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\SystemRoot|NewDir"
              will parse to : "C:\Windows\NewDir" (or whereever your system root is).

Byte  n+1   : length of command line / 0=no command line
     n+2..o : the command line to execute after successfull extraction, if byte n+1 <> 0 (dito)
              format : the command line has a special format
              if the string "><" (greater than+less than) is somewhere in the command line,
              it will be replaced with the path where the archive has been extracted to.
              (e.g. "><readme\test.txt" after an extraction to the path "C:\Program files\unpacked" means :
              "c:\progra~1\unpacked\readme\test.txt") <- the short path will be created by zipsfx.
              if the pipe "|" is in the command-line, the part to the left will get the application to run
              and the part to the right will be it's argument;
              if the archive is extracted to e.g. "d:\unpack", then we will get the following :
              "><setup\setup.exe|><install.inf" will parse to :
              run "d:\unpack\setup\setup.exe" with parameters "d:\unpack\install.inf".
              "c:\windows\notepad.exe|><readme.txt" will parse to :
              run "c:\windows\notepad.exe" with parameters "d:\unpack\readme.txt".
              "><readme.txt" will parse to :
              open "d:\unpack\readme.txt" with its associated program, if there is any.
              "><setup.exe" will run "d:\unpack\setup.exe" without special parameters.
***10/10/98   INF-scripts are accepted as well //##FR
              "><setup.inf" will run the [DefaultInstall] section of "d:\unpack\setup.inf".
              "><setup.inf|.ntx86" will run the [DefaultInstall] section if Win95 (98?),
              but [DefaultInstall.ntx86] section if WinNT.

Byte  o+1   : length of message to display / 0=no message
     o+2..p : the message to display before opening the main dialog, if byte o+1 <> 0 (dito)
	      if byte 0 of this message is
		1 : message begins at byte o+3, messagebox style = MB_ICONINFORMATION ,
		    buttons : (ok,cancel ; if cancel is pressed, stop sfx )
		2 : message begins at byte o+3, messagebox style = MB_ICONCONFIRMATION ,
		    buttons : (yes,no ; if no is pressed, stop sfx )

the size of the special header (MPV) must be dword aligned !!! (old MPU is always 256 byte)
*)

Program SFX;


Uses  Dialog,
  SFXgbls,
  SFXmisc,
  Windows,
  SFXstrings;


//{$R *.RES} //## renamed to sfx1.res (because of the delphi-created res-file)
{$R SFX1.res}

Begin
   { Open the archive }
   InFile := CreateFile( pChar( ParamStr( 0 ) ), GENERIC_READ, FILE_SHARE_READ, nil,
                       OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );

   { If error, notify and abort }
   If InFile = INVALID_HANDLE_VALUE Then
   Begin
      MessageBox( 0, pChar( ParamStr( 0 ) ), STR_EARCHIVE, MB_ICONERROR );
      Exit;
   End;

   Make_CRC32Table;  // Created in the initialisation section of Dialog.pas

   { Display the dialog }
   GetDefParams;   //## read the special setup-header from the file, if any
   DialogBox( hInstance, STR_MAINDLG, 0, @MainDialogProc );
END.

