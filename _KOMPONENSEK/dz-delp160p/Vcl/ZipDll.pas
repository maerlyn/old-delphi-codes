{ ZIPDLL.PAS   - Delphi translation of file "wizzip.h" by Eric W. Engler }
{ Import Unit for ZIPDLL - put this into the "uses" clause of any
  other unit that wants to access the DLL. }

{ I changed this to use dynamic loading of the DLL in order to allow
  the user program to control when to load and unload the DLLs.
  Thanks to these people for sending me dynamic loading code:
     Ewart Nijburg, Nijsoft@Compuserve.com
     P.A. Gillioz,  pag.aria@rhone.ch
}

Unit ZIPDLL;

{$INCLUDE ZipVers.inc}

Interface

Uses Windows, Dialogs, ZCallBck;

{$IfDef VERD2D3}
Type LongWord = Cardinal;
{$EndIf}

{ These records are very critical.  Any changes in the order of items, the
  size of items, or modifying the number of items, may have disasterous
  results.  You have been warned! }
Type ZipParms1 = packed record
   Handle:          HWND;
   Caller:          Pointer;    { "self" referance of the Delphi form }
                                { This is passed back to us in the callback function
                                  so we can direct the info to the proper form instance
                                - thanks to Dennis Passmore for this idea. }
   Version:         LongInt;    { version of DLL we expect to see }
   ZCallbackFunc:   ZFunctionPtrType; { type def in ZCallBck.PAS }
   fTraceEnabled:   LongBool;

   {============== Begin Zip Flag section ============== }
   pZipPassword:    pChar;      { password pointer }
   pSuffix:         pChar;      { Enum of file extensions; files with this extensions will be stored. v1.6 }
   fEncrypt:        LongBool;   { Encrypt files to be added? }

   { include system and hidden files }
   fSystem:         LongBool;

   { Include volume label }
   fVolume:         LongBool;

   { Include extra file attributes (read-only, unix timestamps, etc) }
   fExtra:          LongBool;

   { Do not add directory names to .ZIP archive }
   { see also: fJunkDir }
   fNoDirEntries:   LongBool;

   { Only add files newer a specified date }
   { See the "Date" array below if you set this to TRUE }
   fDate:           LongBool;

   { Give a little more information to the user via message boxes }
   fVerboseEnabled: LongBool;

   { Quiet operation - the DLL won't issue any messages at all. }
   { Delphi program MUST handle ALL errors via it's callback function. }
   fQuiet:          LongBool;

   { Compression level (0 - 9; 9=max, 0=none) }
   { All of these levels are variations of deflate. }
   { I strongly recommend you use one of 3 values here:
        0 = no compression, just store file
        3 = "fast" compression
        9 = "best" compression }
   fLevel:          LongInt;
   fComprSpecial:   LongBool;   { Not use any more (v1.6) }

   { translate text file end-of-lines }
   fCRLF_LF:        LongBool;

   { junk the directory names }
   { If true, this says not to save dirnames as separate entries,
     in addition to being save with filenames. }
   { see also: fNoDirEntries }
   fJunkDir:        LongBool;

   { Recurse into subdirectories }
   fRecurse:        WordBool;
   fNoRecurseFiles: Word;

         { Allow appending to a zip file }
   fGrow:           LongBool;

   { Convert filenames to DOS 8x3 names - for compatibility
     with PKUNZIP v2.04g, which doesn't understand long filenames }
   fForce:          LongBool;

   { Delete orig files that were added or updated in zip file }
   { This is a variation of Add }
   fMove:           LongBool;

   { Delete specified files from zip file }
   fDeleteEntries:  LongBool;

   { Update zip -- if true, rezip changed, and add new files in fspec }
   { This is a variation of Add }
   fUpdate:         LongBool;

   { Freshen zip -- if true, rezip all changed files in fspec }
   { This is a variation of Add }
   fFreshen:        LongBool;

   { junk the SFX prefix on the self-extracing .EXE archives }
   fJunkSFX:        LongBool;

   { Set zip file time to time of newest file in it }
   fLatestTime:     LongBool;
   {============== End Zip Flag section ============== }

   { Cutoff Date for Add-by-date; add files newer than this day }
   { This is only used if the "fDate" option is TRUE }
   { format = MMDDYY plus 2 trailing nulls }
   Date:            Array[0..7] of Char;

   { Count of files to add or delete - don't forget to set this! }
   Argc:            LongInt;
   { ptr to name of zip file }
   pZipFN:          pChar;
   Seven:           LongInt;    { pass a 7 here to validate struct size }

   { Array of filenames contained in the ZIP archive }
   pFileNames:      Array[0..FilesMax] of pChar;
 end;

Type FileData = packed record
   fFileSpec:        pChar;
   fFileComment:     pChar;                       // NEW z->comment and z->com
   fFileAltName:     pChar;                       // NEW
   fPassword:        pChar;                       // NEW, Override in v1.60L
   fEncrypt:          LongWord;                   // NEW, Override in v1.60L
   fRecurse:          Word;                       // NEW, Override in v1.60L
   fNoRecurseFiles:   Word;                       // NEW, Override
   fDateUsed:         LongBool;                   // NEW, Override
   fDate:             Array[0..7] of Char;        // NEW, Override
	fRootDir:         pChar;                       // NEW RootDir support for relative paths in v1.60L.
   fNotUsed:          Array[0..15] of Cardinal;   // NEW
 end;
Type pFileData = ^FileData;

Type ExcludedFileSpec = packed record
   fFileSpec:        pChar;
 end;
Type pExcludedFileSpec = ^ExcludedFileSpec;

Type ZipParms2 = packed record
   Handle:            HWND;
   Caller:            Pointer;
   Version:           LongInt;
   ZCallbackFunc:     ZFunctionPtrType;
   fTraceEnabled:     LongBool;
   pZipPassword:     pChar;
   pSuffix:          pChar;
   fEncrypt:          LongBool;
   fSystem:           LongBool;
   fVolume:           LongBool;
   fExtra:            LongBool;
   fNoDirEntries:     LongBool;
   fDate:             LongBool;
   fVerboseEnabled:   LongBool;
   fQuiet:            LongBool;
   fLevel:            LongInt;
   fComprSpecial:     LongBool;
   fCRLF_LF:          LongBool;
   fJunkDir:          LongBool;
   fRecurse:          WordBool;
   fNoRecurseFiles:   Word;
   fGrow:             LongBool;
   fForce:            LongBool;
   fMove:             LongBool;
   fDeleteEntries:    LongBool;
   fUpdate:           LongBool;
   fFreshen:          LongBool;
   fJunkSFX:          LongBool;
   fLatestTime:       LongBool;
   Date:              Array[0..7] of Char;
   Argc:              LongInt;
   pZipFN:           pChar;
   // After this point the record is different from ZipParms1 structure.
   fTempPath:        pChar;       // NEW TempDir v1.5
   fArchComment:     pChar;       // NEW ZipComment v1.6
   fArchiveFilesOnly: SmallInt;   // NEW when != 0 only zip when archive bit set
   fResetArchiveBit:  SmallInt;   // NEW when != 0 reset the archive bit after a successfull zip
   fFDS:             pFileData;   // pointer to Array of FileData
   fForceWin:         LongBool;   // NEW
   fTotExFileSpecs:   LongInt;    // NEW Number of ExcludedFileSpec structures.
   fExFiles:         pExcludedFileSpec;   // NEW Array of file specs to exclude from zipping.
   fUseOutStream:     LongBool;   // NEW component v160M, dll v1.6015 Use memory stream as output.
   fOutStream:        Pointer;    // NEW component v160M, dll v1.6015 Pointer to the start of the output stream data.
   fOutStreamSize:    LongWord;   // NEW component v160M, dll v1.6015 Size of the Output data.
   fUseInStream:      LongBool;   // NEW component v160M, dll v1.6015 Use memory stream as input.
   fInStream:         Pointer;    // NEW component v160M, dll v1.6015 Pointer to the start of the input stream data.
   fInStreamSize:     LongWord;   // NEW component v160M, dll v1.6015 Size of the input data.
   fStrFileAttr:      DWORD;      // NEW component v160M, dll v1.6015 File attributes of the file stream.
   fStrFileDate:      DWORD;      // NEW component v160M, dll v1.6015 File date/time to set for the streamed file.
   fHowToMove:        LongBool;
   fWantedCodePage:   SmallInt;
   fNotUsed0:         SmallInt;
   fNotUsed:          Array[0..3] of Cardinal;
   fSeven:            Integer;    // End of record (eg. 7)
 end;

{ A (pointer) union of the two zip parameter records used in TZipMaster }

Type ZipParms0 = record
   case Integer of
      0: (zp1: ^ZipParms1);
      1: (zp2: ^ZipParms2);
   end;

Type
   pZipParms = ^ZipParms1;

   ZipOpt = (ZipAdd, ZipDelete);
   { NOTE: Freshen, Update, and Move are only variations of Add }


implementation

end.

