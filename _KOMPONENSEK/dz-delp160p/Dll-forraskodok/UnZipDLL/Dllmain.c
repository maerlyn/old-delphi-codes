/* Main UnZip DLL module
 * This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 * Based on Mike White's DLL, but is dissimilar in many ways.
 * This module has the entry point for the two calls from BCB and Delphi.
 */

// EWE note: we get globals.h from this path:
// unzip.h -> unzpriv.h -> globals.h
#include <windows.h>
#include "wizunzip.h"
#include "unzip.h"
#include "version.h"
#include "consts.h"
#include <excpt.h>

bool FSetUpToProcessZipFile( struct Globals *pG, DCL1 *C );
void TakeDownFromProcessZipFile( struct Globals *pG );
//static int x_filter( EXCEPTION_POINTERS *xp );

#define PWLEN 80

#ifdef USE_MEMCHECK
static struct Tag_MemData {
	void	  *memptr;
	long		memcnt;
	long		memtype;
	size_t	memlen;
	int		line;
	char		file[MAX_PATH];
} MemData[MEMCHECKSIZE];
#endif

/* u used else not in use */
const char *errors[] = {
	/*  0 */  "", //"File not found",
	/*  1 */  "", //"password failed",
	/*  2 */  "", //"Unexpected end of zip file",
	/*  3u */  "Zip file structure invalid",
	/*  4u */  "Out of memory",
	/*  5 */  "", //"Internal logic error",
	/*  6u */  "Central dir not in ZIP file",			//RCV added
	/*  7u */  "No multi-disk archives supported",	//RCV added
	/*  8u */  "Can't find start of central dir",	//RCV added
	/*  9u */  "Aborted by user",
	/* 10 */  "", //"Temporary file failure",
	/* 11u */  "Error reading a file",
	/* 12 */  "", //"Nothing to do!",
	/* 13u */  "Error opening ZIP file: %s",	//RCV changed
	/* 14u */  "Error writing to a file",
	/* 15 */  "", //"Error creating file",
	/* 16u */  "Bad unzip options specified",
	/* 17 */  "", //"Password canceled",
	/* 18 */  "", //"File not found or no read permission",
	/* 19u */  "Fatal Error: central dir signature not found",
	/* 20u */  "Premature eof in archive"
};

/* ===========================================================================
 * Secondairy entry point.
 */
long __stdcall GetUnzDllVersion( void ) {  /* see version.h */
	return UNZVERS;
}

/* ===========================================================================
 * Third entry point.
 */
long __stdcall GetUnzDllPrivVersion( void ) {  /* see version.h */
	return UNZPRIVVERS;
}


/* ===========================================================================
 * Main entry point to unzip files.
 */
long __stdcall UnzDllExec( DCL1 *C ) {
	struct Globals *pG = GetGlobalPointer();
	int             FilesActedOn;
	bool				 StructOK = false;
	//	DCL2			*DebugDCL2 = (DCL2 *)C;

	GlobalsInit( pG );			// We always want a 'fresh' structure when we start here.

	pG->global_handle		= C->fHandle;		// Application handle; can be NULL.
	pG->global_caller		= C->fCaller;		// We just give this back in the callback call.
	pG->callb				= C->fCallback;	// Point to the BCB/Delphi callback function.
	pG->global_trace_opt	= C->fTraceEnabled;
	pG->WantedCodePage	= C->fWantedCodePage;
	pG->CallerVersion		= C->fVersion;

	diag( pG, "trace is in UNZDLL.DLL" );

	/*----------------------------------------------------------------*/
	if ( C->fVersion > 151 ) {
		if ( ((DCL2 *)C)->fSeven == 7 ) StructOK = true;
   } else if ( C->fSeven == 7 ) StructOK = true;
	if ( !StructOK ) {
		user_callback( pG, 4, UEN_PARMS02, 0, "Structure size mismatch" );
		return 0;
	}
	if ( !C->fZipFN ) {		/* Something screwed up, we don't have a filename */
		user_callback( pG, 4, UEN_PARMS01, 0, "No zip filename received" );	// RCV changed was diag()
		return 0;
	}
	if ( C->fVersion != UNZVERS ) {	/* see version.h */
		lstrcpy( pG->ewemsg, "Warning: UNZDLL.DLL version %d is " );
		/* This message won't appear if user did pass us a zero
		 * Window handle.
		 */
		lstrcat( pG->ewemsg, (C->fVersion < UNZVERS) ? "newer" : "older" );
		if ( C->fVersion > UNZVERS ) lstrcat( pG->ewemsg, "\nPlease get an update" );
		if ( !C->fQuiet ) MsgBox( pG );	/* Only a warning */
		/* Also report this fact via the C++Builder/Delphi callback. */
		printf( pG->ewemsg, UNZPRIVVERS );
	}

	/*----------------------------------------------------------------*/
	diag( pG, "ready to setup" );
	if ( FSetUpToProcessZipFile( pG, C ) ) {			// Parse our cmd line.
		if ( pG->global_trace_opt ) pG->vflag = 1;	// If tracing, make sure verbose is on.

//		__try {
#			ifdef USE_STRM_INPUT
			if ( C->fVersion >= 160 && pG->UseInStream )
				MemExtract( pG );
			else
#			endif
				process_zipfiles( pG );						// Pass ptr to global bufs.
//      }
//		__except( x_filter( GetExceptionInformation() ) ) {
//			diag( pG, "Fatal error, terminating DLL call" );
//		}
		diag( pG, "*** BACK FROM CALL TO process_zipfiles ***" );

		if ( pG->vflag ) {									// Verbose flag.
			// sprintf( pG->ewemsg, "Files acted on = %d", pG->files_acted_on );
			// user_callback( pG, 4, 0, 0, pG->ewemsg );
			printf( "Files acted on = %d", pG->files_acted_on );
		}
	}
	FilesActedOn = pG->files_acted_on;
	inflate_free( pG );
	TakeDownFromProcessZipFile( pG );
#  ifdef USE_MEMCHECK
	DispMemDiag();
#  endif
	ReleaseGlobalPointer();
	return FilesActedOn;
}

/* ===========================================================================
 */
bool FSetUpToProcessZipFile( struct Globals *pG, DCL1 *C ) {
	/* These flags all have a fixed value in this version. */
	pG->extract_flag = 1;
	pG->C_flag  = 1;        /* if true, match filenames case-insensitively			*/
	pG->tflag   = C->fTest; /* if true, test zipfile										*/
	pG->T_flag  = 1;        /* -T: timestamps (unzip) or dec. time fmt (zipinfo)	*/

	// set options from caller
	pG->create_dirs = C->fDirectories;	// used by main(), mapname(), checkdir()
	pG->dflag = C->fDirectories;			// "recreate dir structure"
	pG->jflag = !(pG->dflag);				// "junk pathnames"

	pG->aflag = C->fConvert;				// do ASCII/EBCDIC or EOL conversions
	pG->vflag = C->fVerbose;				// verbose flag
	pG->qflag = C->fQuiet;					// quiet flag
	if ( C->fHandle == 0 )              // if we don't have a window handle, then
		pG->qflag = true;                // we need to be quiet (no dialog boxes)

	pG->uflag = C->fUpdate;					// "Update" - extract only newer files & brand new files
	pG->fflag = C->fFreshen;				// "freshen" (extract only newer files)

	if ( C->fOverwrite )
		pG->overwrite_all  = true;			// Don't ask, always overwrite else Don't overwrite; skip that file.

	pG->filespecs = C->fArgc;				// number of fspecs

	pG->local_hdr_sig[0] = pG->central_hdr_sig[0] = pG->end_central_sig[0] = '\120';		// 'P'

	if ( (pG->hZipFN = GLOBALALLOC( GMEM_MOVEABLE, FILNAMSIZ )) == NULL ) return false;

	pG->zipfn = (char *)GlobalLock( pG->hZipFN );
	lstrcpy( pG->zipfn, C->fZipFN );

#	ifdef CRYPT
	/* IMPORTANT! For ZIPDLL, the password is malloc'd, and then pointed-to
	 * by the global "key",  However, in UNZDLL, this is done:
	 * - "key" and "pG->key" must remain NULL
	 * - pG->pwdarg must point to the password passed-in, or else must be NULL
	 * - pG->P_flag must be set to true if a password was passed-in
    */

	/* if no password, we will prompt user later (only if encrypted file found)*/
	if ( C->fPassword && lstrlen( C->fPassword ) > 0 ) {
		/* allocate memory for the password, and point the
		 * global password pointer to it */
		if ( (pG->pwdarg = MALLOC( PWLEN + 1 )) == NULL ) return 0;

		lstrcpy( pG->pwdarg, C->fPassword ); // copy password to global buf
		Trace( (pG, "UNZDLL was passed password: %s", pG->pwdarg) );
		pG->P_flag = true;   /* command line password given */
	}
	pG->PwdReqCount = (C->fVersion >= 160) ? ( (DCL2 *)C)->fPwdReqCount & 0x0F : 1;
#	endif

	/*  MW: pG->wildzipfn needs to be initialized so that do_wild does not wind
	 *  up clearing out the zip file name when it returns in process.c
	 */
	if ( (pG->hwildZipFN = GLOBALALLOC( GMEM_MOVEABLE, FILNAMSIZ )) == NULL ) return false;
	pG->wildzipfn = GlobalLock( pG->hwildZipFN );
	lstrcpy( pG->wildzipfn, C->fZipFN );

#	ifdef USE_STRM_INPUT
	if ( C->fVersion < 160 || !((DCL2 *)C)->fUseInStream ) {
#	endif
		if ( stat( pG->zipfn, &pG->statbuf ) || (pG->statbuf.st_mode & S_IFMT) == S_IFDIR )
			lstrcat( pG->zipfn, ZSUFX );
		if ( stat( pG->zipfn, &pG->statbuf ) ) {  /* try again */
			printf( "error:  can't find zipfile [ %s ]\n", pG->zipfn ); //stdout
			return false;
		} else
			pG->ziplen = pG->statbuf.st_size;
#	ifdef USE_STRM_INPUT
	}
#	endif

	if ( !C->fArgc )
		pG->process_all_files = true;   // Skip fspec matching for speed.
	else {
		if ( C->fVersion > 151 )	// Just copy the pointer to the FileData array.
			pG->pfnames = ( (DCL2 *)C)->fFNV;
		else {	// We need to copy the filename pointers to a FileData array.
			if ( (pG->hFileDat = GLOBALALLOC( GMEM_MOVEABLE | GMEM_ZEROINIT , sizeof(fFileData) * (C->fArgc + 1) )) != NULL ) {
				int i;

				pG->pfnames = pG->FileDat = (fFileData (*)[])GlobalLock( pG->hFileDat );
				for (i = 0; i < C->fArgc; i++ )
					( (fFileData *)(pG->pfnames) + i)->fFileSpec = C->fFNV[i];
			}
		}
	}
	pG->ExtractDir = ( (DCL2 *)C)->fExtractDir;	// v1.6024
	Trace( (pG, "argc = %d, process_all_files = %d", C->fArgc, pG->process_all_files) );
	/*---------------------------------------------------------------------------
	 *     Ok, we have everything we need to get started.
	 *---------------------------------------------------------------------------*/
	if ( (pG->hInBuf = GLOBALALLOC( GMEM_MOVEABLE, INBUFSIZ + 4 )) != NULL ) {
		pG->inbuf = (uch *)GlobalLock( pG->hInBuf );
		WinAssert( pG->inbuf );
	}
	if ( (pG->hOutBuf = GLOBALALLOC( GMEM_MOVEABLE, OUTBUFSIZ + 1 )) != NULL ) {
		pG->outbuf = (uch *)GlobalLock( pG->hOutBuf );
		WinAssert( pG->outbuf );
		/*  outout = pG->outbuf; */ /*  point to outbuf */
	}
	if ( (pG->hFileName = GLOBALALLOC( GMEM_MOVEABLE, FILNAMSIZ )) != 0 ) {
		pG->filename = GlobalLock( pG->hFileName );
		WinAssert( pG->filename );
	}
	if ( !pG->inbuf || !pG->outbuf || !pG->zipfn || !pG->filename ) return false;

	pG->hold = &pG->inbuf[INBUFSIZ]; /* to check for boundary-spanning signatures */

#	ifdef USE_STRM_INPUT
	if ( C->fVersion >= 160 && ( (DCL2 *)C)->fUseInStream ) {
		pG->UseInStream  = ( (DCL2 *)C)->fUseInStream;
		pG->InStream     = ( (DCL2 *)C)->fInStream;
		pG->InStreamSize = ( (DCL2 *)C)->fInStreamSize;
	}
#	endif
#	ifdef USE_STRM_OUTPUT
	if ( C->fVersion >= 160 && ( (DCL2 *)C)->fUseOutStream ) {
		pG->redirect_data   = true;
		pG->buffer_size     = ( (DCL2 *)C)->fOutStreamSize;
      pG->redirect_buffer = ( (DCL2 *)C)->fOutStream;
#		ifdef USE_STRM_INPUT
		if ( pG->UseInStream ) {
			pG->redirect_pointer = pG->outbuf;	// Circular buffer inside DLL
			pG->redirect_size    = OUTBUFSIZ;
		} else {
#		endif
			pG->redirect_pointer = pG->redirect_buffer;
			pG->redirect_size    = ( (DCL2 *)C)->fOutStreamSize;
#		ifdef USE_STRM_INPUT
		}
#		endif
		if ( !pG->redirect_buffer ) return false;
	}
#	endif
	return true;    /* set up was OK */
}


/* ===========================================================================
 */
void TakeDownFromProcessZipFile( struct Globals *pG ) {
	if ( pG->hFileDat )	{
		if ( pG->FileDat ) GlobalUnlock( pG->hFileDat );
		GLOBALFREE( pG->hFileDat );
	}
	if ( pG->hInBuf ) {
		if ( pG->inbuf ) GlobalUnlock( pG->hInBuf );
		GLOBALFREE( pG->hInBuf );
	}
	if ( pG->hOutBuf ) {
		if ( pG->outbuf ) GlobalUnlock( pG->hOutBuf );
		GLOBALFREE( pG->hOutBuf );
	}
	if ( pG->hZipFN ) {
		if ( pG->zipfn ) GlobalUnlock( pG->hZipFN );
		GLOBALFREE( pG->hZipFN );
	}
	if ( pG->hwildZipFN ) {
		if ( pG->wildzipfn ) GlobalUnlock( pG->hwildZipFN );
		GLOBALFREE( pG->hwildZipFN );
	}
	if ( pG->hFileName ) {
		if ( pG->filename ) GlobalUnlock( pG->hFileName );
		GLOBALFREE( pG->hFileName );
	}
	if ( pG->key )		// RCV 1.608 Added
		FREE( pG->key );
}


/* ===========================================================================
 * printf clone to support DLL
 */
int __cdecl printf( const char *format, ... ) {
	struct Globals *pG = GetGlobalPointer();
	va_list         argptr;
	HANDLE          hMemory;
	LPSTR           Buffer;

	if ( (hMemory = GLOBALALLOC( GMEM_MOVEABLE, STDIO_BUF_SIZE )) != NULL ) {
		if ( (Buffer = GlobalLock( hMemory )) != NULL ) {
			va_start( argptr, format );
			 wvsprintf( Buffer, format, argptr );
			 // warning message, or info message only
			 user_callback( pG, 4, 0, 0, Buffer );
			 GlobalUnlock( hMemory );
			va_end( argptr );
		}
		GLOBALFREE( hMemory );
	}
	return 0;
}


/* ===========================================================================
 */
void __cdecl MyTrace( struct Globals *pG, const char *format, ... ) {
	if ( pG->global_trace_opt ) {
		va_list argptr;
		HANDLE  hMemory;
		LPSTR   Buffer;

		if ( (hMemory = GLOBALALLOC( GMEM_MOVEABLE, STDIO_BUF_SIZE )) != NULL ) {
			if ( (Buffer = GlobalLock( hMemory )) != NULL ) {
				va_start( argptr, format );
				 wvsprintf( Buffer, format, argptr );
				 diag( pG, Buffer );
				 GlobalUnlock( hMemory );
				va_end( argptr );
			}
			GLOBALFREE( hMemory );
		}
	}
}


/* ===========================================================================
 */
void MsgBox( struct Globals *pG ) {
	char ewetmp[2048];
	HWND wHandle = pG->global_handle;

	/* Did the user pass us a good window handle? if not, we can't pop-up a box. */
	if ( !wHandle ) wHandle = GetDesktopWindow();	// v1.6021
	/* bring up a dialog box */
	lstrcpy( ewetmp, "Msg From UNZIP DLL: " );
	strncat( ewetmp, pG->ewemsg, 2027 );
	MessageBox( wHandle, ewetmp, "Message From UNZIP DLL", MB_OK );
}


/* ===========================================================================
 */
void diag( struct Globals *pG, char *msg ) {
	if ( pG->vflag ) {
		char ewetmp[2048];
		/* log the message through the routine message callback */
		lstrcpy( ewetmp, "Trace Msg: " );
		strncat( ewetmp, msg, 2036 );
		/* specify a 0 error code to prevent a dialog box from coming
		 *	up in the BCB/Delphi application program */
		user_callback( pG, 4, 0, 0, ewetmp );
	}
}

/*
 * This provides the calling program with updated info on what the DLL
 * is doing.  Regardless of the type of call being made, the user's
 * function must have a spin of the Windows message loop.  In fact, even
 * if user isn't using a progress bar, he should still spin the msg
 * loop upon getting these callbacks (but he doesn't need to do anything
 * else).  In Delphi, "Application.ProcessMessages;" spins the loop.
 * Here are the types of calls:

 *   ActionCode = 1, we're starting a zip operation on a new file
 *      error_code = N/A
 *      fsize = filesize of file we're going to operate on
 *      name_or_msg = pathname of file
 *   IMPORTANT: The user's function must do the math for the progress
 *   bar upon getting this call.  See the Delphi sample application.

 *   ActionCode = 2, increment the progress bar
 *      These calls will occur after every 32K of input file has been
 *      processed. One additional call is made at the end of each file,
 *      just to make sure the progress bar is max'ed out - this is also
 *      critical for files less than 32K in size (this last one will be
 *      their only one).
 *      error_code = N/A
 *       fsize = N/A
 *      name_or_msg = N/A

 *   ActionCode = 3, we're done with a batch of files - program flow
 *   will quickly return to the user's program.
 *   NOTE: the end of a every file will always be followed by an
 *         action of 1 or 3, so a separate call for end of one file
 *         isn't needed.
 *      error_code = N/A
 *      fsize = N/A
 *      name_or_msg = N/A

 *   ActionCode = 4, a routine message is being passed
 *      error_code = code corresponding to message (not widely used yet)
 *      fsize = N/A
 *      name_or_msg = text of message
 *
 *   ActionCode = 5, the total number of files is being passed.
 *      error_code = N/A
 *      fsize = The total number of files.
 *      name_or_msg = N/A
 *
 *   ActionCode = 6, the total file size is being passed.
 *      error_code = N/A
 *      fsize = The total file size
 *      name_or_msg = N/A
 *
 *   ActionCode = 7, the internal filename is being passed.
 *      error_code = 1 if name_or_msg is changed (I)
 *      fsize = 0 ZipDLL or 1 if UnzipDLL (O)
 *      name_or_msg = the internal filename as the dll thinks it should be. (O)
 *      name_or_msg = the new internal filename as the user says it must be. (I)
 *
 *   ActionCode = 8, Pasword miss/error
 *      error_code = Y (Y=1 from Zipdll, Y=2 from UnzipDll) (O)
 *      error_code = 1 if user entered a password or 0 when user canceled or empty password (I)
 *      fsize = The repeat count (I) unzip dll only (O) both dll's
 *		  actioncode = CancelAll=0(I) unzip dll only (v1.6024)
 *      name_or_msg = the new password to try or empty when canceled (I)
 *
 *   Actioncode = 9, CRC32 error during Extract ( UnzDll only )
 *      error_code = Found CRC (O)
 *      fsize = Stored/Expected CRC (O)
 *      name_or_msg = File for which the CRC check went wrong.
 *
 *   ActionCode = 10, A request for what to do when OverWriting is needed.
 *      error-code high word = reason why the call was made 1=EXISTS_AND_OLDER 2=EXISTS_AND_NEWER. (O)
 *      error-code low  word = Central File Index, should correspond to (ZipDirEntry *)ZipBuilder1->ZipContents->Items[] (O)
 *      fsize = the default action to take (depending on the property ExtrOptions. (O)
 *      fsize = the result of the query 0=Do Not Overwrite, 1=Overwrite. (I)
 *      name_or_msg = the filename the request is made for. (O)
 *
 *   ActionCode = 11, A message for the user when a file is skipped when extracting.
 *      error_code = Possibly a more specific error code why the file is skipped (O)
 *      fsize = The reason why the file is skipped (O) (Value 0x0101-0x0109) (0x02.. reserved for ZipDll)
 *      name_or_msg = the filename for which this message applies. (O)
 *
 *   ActionCode = 12, Used in ZipDLL for FileComment setting.
 *
 *   ActionCode = 13, Used to increase the output buffer when unzipping from a stream
 *      to a stream.
 *      fsize = requested new size (O)
 *      fsize = pointer to the (new) output buffer (I)
 *      error_code = 0 or !0 when memory could not be set. (I)
 *
 */


/* ===========================================================================
 * This calls the application program and passes status info.
 */
void user_callback( struct Globals *pG, long action, long error_code, long fsize, char *name_or_msg ) {
	CallBackStruct *cbd = &pG->CallBackData;

	if ( pG->callb ) {
		cbd->handle         = pG->global_handle;	// Window handle of caller
		cbd->caller         = pG->global_caller;	// Object instance pointer of caller
		cbd->version        = UNZVERS;
		cbd->isoperationzip = false;              // true=zip, false=unzip
		cbd->actioncode     = action;
		cbd->error_code     = ( pG->CallerVersion <= 145) ? (int)(char)(error_code & 0xFF) : error_code;
		cbd->fsize          = fsize;
		if ( !name_or_msg )
			cbd->filenameormsg[0] = '\0';
		else lstrcpy( cbd->filenameormsg, name_or_msg );

		/* Make the call and save the return code.  If true is returned, user
		 * wants to abort the current batch job asap.
		 */
		pG->global_abort_sw |= pG->callb( cbd );  // Call user's program.
		/* Don't bother looking at the global_abort_sw here!. */
	}
}


/* ===========================================================================
 * Custom version of ziperr as used by InfoZip's UNZIP program.
	c :: Error code from the UEN_ class - UnzErr.h
 */
void UnzErr( struct Globals *pG, int c ) {
	char errmsg[256];
	int  oldC = (c & 0xFF0000) >> 16;

	if ( pG->user_notified_of_abort ) return;
	pG->user_notified_of_abort = 1;

	pG->global_error_code = oldC;   /* last error code */

	if ( oldC == 0x7F ) lstrcpy( errmsg, pG->ewemsg );	// User defined message (NOT present in errors[]! )
	else if ( c == UEN_NAME01 ) sprintf( errmsg, errors[oldC], pG->zipfn );
		  else lstrcpy( errmsg, errors[oldC] );

	if ( pG->dll_handles_errors ) {
		/* I'm passing the error via the callback just to get it logged in
		 * the status box - I'm sending it in with a 0 error code to avoid
		 * a dialog box from the application program.
		 */
		user_callback( pG, 4, 0, 0, errmsg );
		sprintf( pG->ewemsg, "%s   code=%d", errmsg, c );
		MsgBox( pG );
	} else
		user_callback( pG, 4, c, 0, errmsg );  /* Only application program handles errors. */
}

/* Replacement for the EXIT macro, so application program will get an error and
	won't be aborted v1.6021.
 */
void EXIT( int x ) {
	struct Globals *pG = GetGlobalPointer();

	UnzErr( pG, x );
	RaiseException( x, 0, 0, NULL );
}
/*
static int x_filter( EXCEPTION_POINTERS *xp ) {
	int rc;
	EXCEPTION_RECORD *xr = xp->ExceptionRecord;
	//CONTEXT          *xc = xp->ContextRecord;

	switch( xr->ExceptionCode ) {
		case UEN_READ01:
			rc = EXCEPTION_EXECUTE_HANDLER;
			break;

		default:		// give up
			rc = EXCEPTION_CONTINUE_SEARCH;
	};
	return rc;
}
*/
#ifdef USE_MEMCHECK
void *ExtAlloc1( size_t memsize, int line, char *file ) {
	int   i;
	void *Ptr = malloc( memsize );

	if ( Ptr ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == Ptr || !MemData[i].memptr ) {
			MemData[i].memptr  = Ptr;
			MemData[i].memtype = 1;
			MemData[i].memlen  = memsize;
			MemData[i].line    = line;
			MemData[i].memcnt++;
			lstrcpy( MemData[i].file, file );
			break;
		}
	}
	return Ptr;
}

HGLOBAL ExtAlloc2( UINT f, DWORD memsize, int line, char *file ) {
	int     i;
	HGLOBAL Ptr = GlobalAlloc( f, memsize );

	if ( Ptr ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == Ptr || !MemData[i].memptr ) {
			MemData[i].memptr  = Ptr;
			MemData[i].memtype = 2;
			MemData[i].memlen  = memsize;
			MemData[i].line    = line;
			MemData[i].memcnt++;
			lstrcpy( MemData[i].file, file );
			break;
		}
	}
	return Ptr;
}

void ExtFree1( void *ptr, int line, char *file ) {
	int i;

	for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == ptr && MemData[i].memtype == 1 ) {
		MemData[i].memcnt--;
		break;
	}
	free( ptr );
}

HGLOBAL ExtFree2( HGLOBAL ptr, int line, char *file ) {
	int i;

	for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == ptr && MemData[i].memtype == 2 ) {
		MemData[i].memcnt--;
		break;
	}
	return GlobalFree( ptr );
}

void DispMemDiag( void ) {
	struct Globals *pG = GetGlobalPointer();
	char				 ewetmp[2048], drive[MAXDRIVE], dir[MAXDIR], file[MAXFILE], ext[MAXEXT];
	int				 i;

	if ( pG->global_caller && pG->callb ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memcnt ) {
			// The global pointer can't be freed first because we still need it, so skip.
			if ( MemData[i].memptr == pG && MemData[i].memcnt == 1 ) continue;

			fnsplit( MemData[i].file, drive, dir, file, ext );
			sprintf( ewetmp, "MemAlloc: at %p size %ld file %s line %d type %d", MemData[i].memptr, MemData[i].memlen, file, MemData[i].line, MemData[i].memtype );
			user_callback( pG, 4, 0, 0, ewetmp );
			MemData[i].memptr = NULL;
			// Very unlikely but...
			if ( i == 3999 ) user_callback( pG, 4, 0, 0, "To many pointers, increase MEMCHECKSIZE" );
		}
	}
}
#endif
