{ UNZDLL.PAS   - Delphi v2 translation of file "wizunzip.h" by Eric W. Engler }
{ Import Unit for UNZDLL - put this into the "uses" clause of any
  other unit that wants to access the UNZDLL. }

{ I changed this to use dynamic loading of the DLL in order to allow
  the user program to control when to load and unload the DLLs.
  Thanks to these people for sending me dynamic loading code:
     Ewart Nijburg, Nijsoft@Compuserve.com
     P.A. Gillioz,  pag.aria@rhone.ch
}

Unit UNZDLL;

Interface

Uses Windows, Dialogs, ZCallBck;

{ These records are very critical.  Any changes in the order of items, the
  size of items, or modifying the number of items, may have disasterous
  results.  You have been warned! }
Type UnZipParms1 = packed record
   Handle:             HWND;
   Caller:             Pointer;    { "self" referance of the Delphi form }
                                   { This is passed back to us in the callback function
                                     so we can direct the info to the proper form instance
                                     - thanks to Dennis Passmore for this idea. }
   Version:            LongInt;    { version of DLL we expect to see }
   ZCallbackFunc:      ZFunctionPtrType; { type def in ZCallBck.PAS }
   fTraceEnabled:      LongBool;

   {============== Begin UnZip Flag section ============== }
   fPromptToOverwrite: LongBool;   // not used yet
   pZipPassword:       pChar;      // password pointer
   fTest:              LongBool;   // if true, test zipfile, don't save extracted files
   fComments:          LongBool;   // show zip comment (not supported yet)
   fConvert:           LongBool;   // if true, do ASCII/EBCDIC or EOL translation

   fQuiet:             LongBool;   // DLL be quiet!
   fVerboseEnabled:    LongBool;   // verbose flag
   fUpdate:            LongBool;   // "update" (extract only newer files & brand new files)
   fFreshen:           LongBool;   // "freshen" (extract only newer files that already exist)
   fDirectories:       LongBool;   // if true, recreate dir structure
   fOverwrite:         LongBool;   // if true, overwrite existing (no asking)

   { Count of filespecs to extract - don't forget to set this! }
   fArgc:              LongInt;
   { ptr to zipfile name }
   pZipFN:             pChar;
   Seven:              LongInt;    { pass a 7 here to validate record size }
   { Array of filenames contained in the ZIP archive.
     The last specification MUST be nil! The UnzDll requires this!
     FilesMax can be set to a different value, the dll uses whatever is set. }
   pFileNames:         Array[0..FilesMax] of pChar;
 end;

Type UnzFileData = packed record
         fFileSpec:       pChar;
         fFileAltName:    pChar;
         fPassword:       pChar;
         fNotUsed:        Array[0..14] of Cardinal;
     end;
     pUnzFileData = ^UnzFileData;

Type UnzExFileData = packed record
         fFileSpec:       pChar;
         fNotUsed:        Array[0..2] of Cardinal;
     end;
     pUnzExFileData = ^UnzExFileData;

Type UnZipParms2 = packed record
   Handle:             HWND;
   Caller:             Pointer;
   Version:            LongInt;
   ZCallbackFunc:      ZFunctionPtrType;
   fTraceEnabled:      LongBool;
   fPromptToOverwrite: LongBool;
   pZipPassword:       pChar;
   fTest:              LongBool;
   fComments:          LongBool;
   fConvert:           LongBool;
   fQuiet:             LongBool;
   fVerboseEnabled:    LongBool;
   fUpdate:            LongBool;
   fFreshen:           LongBool;
   fDirectories:       LongBool;
   fOverwrite:         LongBool;
   fArgc:              LongInt;
   pZipFN:             pChar;
   { After this point the record is different from UnZipParms1 }
   { Pointer to an Array of UnzFileData records,
     the last pointer MUST be nil! The UnzDll requires this! }
   fUFDS:              pUnzFileData;
   { Pointer to an Array of ExUnzFileData records }
   fXUFDS:             pUnzExFileData;
   fUseOutStream:      LongBool;                   // NEW Use Memory stream as output.
   fOutStream:         Pointer;                    // NEW Pointer to the start of streaam data.
   fOutStreamSize:     LongInt;                    // NEW Size of the output data.
   fUseInStream:       LongBool;                   // NEW Use memory stream as input.
   fInStream:          Pointer;                    // NEW Pointer to the start of the input stream data.
   fInStreamSize:      LongInt;                    // NEW Size of the input data.
   fPwdReqCount:       Cardinal;                   // NEW PasswordRequestCount, How many times a password will be asked per file
   fExtractDir:        pChar; 
   fNotUsed:           Array[0..7] of Cardinal;
   fSeven:             LongInt;
 end;

{ A (pointer) union of the two Unzip parameter records used in TZipMaster }
Type UnzParmCol = (UnzParms1, UnzParms2);
     UnzParms0 = record
     case UnzParmCol of
        UnzParms1: (up1: ^UnZipParms1);
        UnzParms2: (up2: ^UnZipParms2);
 end;

Type
   pUnZipParms = ^UnZipParms1;


Implementation

end.
