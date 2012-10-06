/* DLLmain.c
 * Copyright (C) 1997 Mike White and Eric W. Engler
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert BCB/Delphi Zip.
*/

#include <windows.h>
#include "Zip.h"
#include "Globals.h"
#include "crypt.h"
#include <process.h>
#include <signal.h>
#include <stdarg.h>
#include <dir.h>

#include "WizZip.h"
#include "version.h"

#ifdef USE_MEMCHECK	// RCV: 1.603
static struct Tag_MemData {
	void	  *memptr;
	long		memcnt;
	long		memtype;
	size_t	memlen;
	int		line;
	char		file[MAX_PATH];
} MemData[ MEMCHECKSIZE ];
#endif

extern int zipmain( int, char **, struct Globals * );

static bool ZipSetParam( ZCL *C, int *i, char **argVee, struct Globals *pG );
static long ZipDllExec2( ZCL2 *C, struct Globals *pG );

static void MakeCmdArg( char **ArgArr, int *i, int size, char *arg ) {
	ArgArr[*i] = (char *)MALLOC( sizeof( char ) * size );
	lstrcpy( ArgArr[(*i)++], arg );
}

/* ===========================================================================
 * Add, update, freshen, move, or delete zip entries in a zip file.
 * SEE THE FILE "SWITCHES.TXT" FOR MORE INFO. ON PROCESSING
 */
long __stdcall ZipDllExec( ZCL *C )	{
	char           **argVee;
	int              argCee, i = 0, k, Error = 0;
	struct Globals  *pG = GetGlobalPointer( &Error );
	long				   RetVal;

	if ( !pG ) {
		char DllErrMsg[34];

		lstrcpy( DllErrMsg, errors[(Error & 0xFF0000) >> 16] );
		MessageBox( NULL, DllErrMsg, "Msg From ZIP DLL", MB_OK );
		return 0;
	}
	GlobalsInit( pG );	// We always want a 'fresh' structure when we start here.

	pG->GlobalCompVersion = C->fVersion;
	/* Copy the window handle and context of caller to our global vars */
	pG->global_handle = C->fHandle;
	pG->global_caller = C->fCaller;
	/* point to the C++Builder/Delphi callback function */
	pG->callb = C->fCallback;

	/* Static callback data */
	pG->callbackdata.handle  = pG->global_handle;	/* Window handle of caller              */
	pG->callbackdata.caller  = pG->global_caller;	/* object instance pointer of caller    */
	pG->callbackdata.version = ZIPVERS;
	pG->callbackdata.isoperationzip = true;			

	if ( C->fVersion > 150 ) return ( ZipDllExec2( (ZCL2 *)C, pG ) );

	if ( C->fSeven != 7 ) {
		user_callback( 4, ZEN_PARMS02, 0, "Structure size mismatch", pG );
		ReleaseGlobalPointer();
		return 0;
	}
	/*----------------------------------------------------------------*/
	/* malloc 26 vectors more than the no. of filenames - allows for
    * max no. of command line arguments
	 */
	argVee = (char **)MALLOC( sizeof( char * ) * (C->fTotFileSpecs + 26) );
	if ( !ZipSetParam( C, &i, argVee, pG ) ) {
		ReleaseGlobalPointer();
		return 0;
	}
	/* Malloc space for the name of the zip file, and copy the name in */
	MakeCmdArg( argVee, &i, lstrlen( C->fZipFN ) + 1, C->fZipFN );

	/* malloc space for each filename to add, and copy the names in
	 * FNV = filename vector in param struct:   char *FNV[];
	 * Note: "i" points to next free loc in the array we will send to ZipMain.
	 * We should already have some items in this array (cmd line sw's).
	 */
	for ( k = 0; k < C->fTotFileSpecs; k++ ) {
		register char *p;

		MakeCmdArg( argVee, &i, lstrlen( C->FNV[k] ) + 1, C->FNV[k] );
		p = argVee[i - 1];	// Replace forward slashes.
		if ( p ) while( *p ) if (*p++ == '/') *(p - 1) = '\\';
	}
	/* we won't malloc space for last one - it's just a null pointer */
	argVee[i] = NULL;	/* no more filename args */
	argCee = i;			/* no. of args for ZipMain (1 based) */

	for ( k = 0; k < argCee; k++ ) {
		sprintf( pG->ewemsg, "Arg: %d for zipmain:  %s ", k, argVee[k] );
		diag( pG->ewemsg, pG );
	}

	if ( zipmain( argCee, argVee, pG ) != 0 ) pG->files_acted_on = 0;
	diag( "*** BACK FROM CALL TO ZIPMAIN ***", pG );

	/* Free the arguments in the array */
	for ( k = 0; k < argCee; k++ ) FREE( argVee[k] );
	/* Then free the array itself */
	FREE( argVee );

	if ( C->fVerbose ) printf( "Files acted on = %d", pG->files_acted_on );
#  ifdef USE_MEMCHECK	// RCV: 1.603
	DispMemDiag();
#  endif
	RetVal = pG->files_acted_on;	// RCV: 1.6010
	ReleaseGlobalPointer();
	return RetVal;
}

/* New entry function for components version >= 1.5 */
long ZipDllExec2( ZCL2 *C, struct Globals *pG ) {
	int    i = 0, k;
	char **argVee;
	long   RetVal;

	if ( C->fSeven != 7 ) {		// This Seven is at another place as in ZCL!
		user_callback( 4, ZEN_PARMS02, 0, "Structure size mismatch", pG );
		ReleaseGlobalPointer();
		return 0;
	}
	// DLL v1.608, Component v1.60L, RootDir support, change DLL v1.6017
	pG->OCDlength = GetCurrentDirectory( 0, pG->OrigCurrentDir );
	if ( (pG->OrigCurrentDir = MALLOC( pG->OCDlength )) == NULL ) {
		user_callback( 4, ZEN_MEM36, 0, "CurrentDir allocation error", pG );
		ReleaseGlobalPointer();
		return 0;
	}
	GetCurrentDirectory( pG->OCDlength, pG->OrigCurrentDir );	// Save the proces defined current dir.

	pG->zcomment = C->fArchComment;								// New v1.60
	if ( C->fArchComment )
		pG->zcomlen = lstrlen( C->fArchComment );
	if ( C->fArchiveFilesOnly ) pG->ArchiveFiles = 1;	 	// New v1.60
	if ( C->fResetArchiveBit ) pG->ResetArchiveBit = 1;	// New v1.60
	if ( C->fHowToMove ) pG->HowToMove = true;
	/* malloc 26 vectors more than the no. of filenames * 2 - allows for
    * max no. of command line arguments
	 */
	argVee = (char **)MALLOC( sizeof( char * ) * (C->fTotFileSpecs * 5 + 26) );	//Changed v1.607,v1.6013
	if ( !ZipSetParam( (ZCL *)C, &i, argVee, pG ) ) {
		ReleaseGlobalPointer();
		return 0;
	}
	if ( C->fTempPath ) {									/* Temporary path -b						*/
		MakeCmdArg( argVee, &i, 3, "-b" );
		MakeCmdArg( argVee, &i, lstrlen( C->fTempPath )+ 1, C->fTempPath );
	}
	/* Malloc space for the name of the zip file, and copy the name in */
	MakeCmdArg( argVee, &i, lstrlen( C->fZipFN ) + 1, C->fZipFN );

	/* malloc space for each filename to add, and copy the names in
	 * FNV = filename vector in param struct:   char *FNV[];
	 * Note: "i" points to next free loc in the array we will send to ZipMain.
	 * We should already have some items in this array (cmd line sw's).
	 */
	for ( k = 0; k < C->fTotFileSpecs; k++ ) {
		register char *p;

		if ( C->fVersion >= 160 ) {
			/* recurse into subdirectories -r or not -R, NEW DLL v1.607, Component 1.60L */
			MakeCmdArg( argVee, &i, 3, (C->fFDS[ k ].fRecurse) ? "-r" : "-R" );
#			ifdef CRYPT
			/* encrypt -e or NOT -E , NEW DLL v1.607, Component 1.60L */
			if ( C->fFDS[ k ].fEncrypt ) {  // v1.6012
				if ( C->fFDS[ k ].fPassword ) {
					MakeCmdArg( argVee, &i, 3, "-P" );
					MakeCmdArg( argVee, &i, lstrlen( C->fFDS[ k ].fPassword ) + 1, C->fFDS[ k ].fPassword );
				}
				MakeCmdArg( argVee, &i, 3, (C->fFDS[ k ].fEncrypt) ? "-e" : "-E" );
			}
#			endif
			/* Set the new RootDir if needed; DLL v1.608, Component v1.60L */
			if ( C->fFDS[ k ].fRootDir ) {
				// We can't use SetCurrentDirectory() because there is only one cd in each process
				// when a user uses threads it went wrong.
				FREE( pG->OrigCurrentDir ); // DLL v1.6017
				if ( (pG->OrigCurrentDir = MALLOC( pG->OCDlength = lstrlen( C->fFDS[ k ].fRootDir )+ 1 )) == NULL ) {
					user_callback( 4, ZEN_MEM36, 0, "CurrentDir allocation error", pG );
					ReleaseGlobalPointer();
					return 0;
				}
				lstrcpy( pG->OrigCurrentDir, C->fFDS[ k ].fRootDir );
			}
		}
		MakeCmdArg( argVee, &i, lstrlen( C->fFDS[ k ].fFileSpec ) + 1, C->fFDS[ k ].fFileSpec );
		p = argVee[i - 1];	// Replace forward slashes.
		if ( p ) while( *p ) if (*p++ == '/') *(p - 1) = '\\';
		sprintf( pG->ewemsg, "Arg: %d for zipmain:  %s ", i - 1, argVee[ i - 1 ] );
		diag( pG->ewemsg, pG );
	}
	/* we won't malloc space for last one - it's just a null pointer */
	argVee[i] = NULL;											/* no more filename args				*/

#	ifdef USE_STRM_INPUT
	if ( C->fVersion >= 160 && C->fUseInStream ) {
		pG->UseOutStream   = C->fUseOutStream;	// In memory compression.
		pG->OutStream      = C->fOutStream;
		pG->OutStreamSize  = C->fOutStreamSize;
		pG->UseInStream    = C->fUseInStream;
		pG->InStream       = C->fInStream;
		pG->InStreamSize   = C->fInStreamSize;
		pG->StrFileAttr	 = C->fStrFileAttr;
		pG->StrFileDate	 = C->fStrFileDate;
	}
#	endif
	if ( C->fVersion >= 160 ) pG->WantedCodePage = C->fWantedCodePage; // v1.6017

	if ( zipmain( i, argVee, pG ) != 0 ) pG->files_acted_on = 0;
	diag( "*** BACK FROM CALL TO ZIPMAIN ***", pG );

	for ( k = 0; k < i; k++ ) FREE( argVee[k] );		/* Free the arguments in the array	*/
	FREE( argVee );											/* Then free the array itself			*/

	if ( C->fVerbose ) printf( "Files acted on = %d", pG->files_acted_on );

	// DLL v1.608, Component v1.60L, RootDir support.
	if ( pG->OrigCurrentDir )
		FREE( pG->OrigCurrentDir );
#	ifdef USE_STRM_INPUT
	if ( pG->UseInStream && pG->UseOutStream )
		C->fOutStreamSize = pG->OutStreamSize;
#	endif

#  ifdef USE_MEMCHECK	// RCV: 1.603
	DispMemDiag();
#  endif
	RetVal = pG->files_acted_on;	// RCV: 1.6010
	ReleaseGlobalPointer();
	return RetVal;
}


bool ZipSetParam( ZCL *C, int *i, char **argVee, struct Globals *pG ) {
	pG->global_trace_opt = C->fTraceEnabled;
	if ( pG->global_trace_opt ) diag( "trace is on in ZIPDLL.DLL", pG );

   /*----------------------------------------------------------------*/
	if ( C->fZipFN == NULL ) {  /* Something screwed up, we don't have a filename */
		user_callback( 4, ZEN_PARMS01, 0, "No zip filename received", pG );
		return false;
	}
	if ( C->fVersion != ZIPVERS ) {  // see version.h
		char vermsg[70] = "Warning: ZIPDLL.DLL version %d is ";
		/* This message won't appear if user did pass us a zero
		 * Window handle.
		 */
		lstrcat( vermsg, (C->fVersion < ZIPVERS) ? "newer" : "older" );
		if ( C->fVersion > ZIPVERS ) lstrcat( vermsg, "\nPlease get an update" );
		if ( !C->fQuiet ) msgbox( vermsg, pG );	/* Only a warning */
		/* Also report this fact via the C++Builder/Delphi callback. */
		printf( vermsg, ZIPPRIVVERS );
	}

	MakeCmdArg( argVee, i, 11, "zipdll.dll" );

	/* allocate room for compression level switch */
	argVee[*i] = (char *)MALLOC( sizeof( char )* 3 );
   argVee[*i][0] = '-';
	argVee[*i][1] = (char)((C->fLevel >= 0 && C->fLevel <= 9) ? (char)C->fLevel + '0' : '9');
	argVee[(*i)++][2] = 0;

	sprintf( pG->ewemsg, "in %s; at fLevel, which is: %d", argVee[0], C->fLevel );
	diag( pG->ewemsg, pG );

	/* build the conventional cmd line switches */
	if ( C->fDeleteEntries )		/* Delete files from zip file -d */
		MakeCmdArg( argVee, i, 3, "-d" );
	if ( C->fNoDirEntries )			/* Do not add directory entries -D */
		MakeCmdArg( argVee, i, 3, "-D" );
	if ( C->fFreshen )				/* Freshen zip file--overwrite only -f */
		MakeCmdArg( argVee, i, 3, "-f" );
	if ( C->fGrow )					/* Allow appending to a zip file -g   Normally TRUE */
		MakeCmdArg( argVee, i, 3, "-g" );
	if ( C->fJunkDir )				/* Junk directory names -j */
		MakeCmdArg( argVee, i, 3, "-j" );
	if ( C->fJunkSFX )				/* Junk sfx prefix */
		MakeCmdArg( argVee, i, 3, "-J" );
	if ( C->fForce )					/* Make entries using DOS names (k for Katz) -k */
		MakeCmdArg( argVee, i, 3, "-k" );
	if ( C->fCRLF_LF )				/* Translate end-of-line -l */
		MakeCmdArg( argVee, i, 3, "-l" );
	if ( C->fMove )					/* Delete files added or updated in zip file -m */
		MakeCmdArg( argVee, i, 3, "-m" );
	if ( C->fLatestTime )			/* Set zip file time to time of latest file in it -o */
		MakeCmdArg( argVee, i, 3, "-o" );
	if ( C->fQuiet || !C->fHandle ) { /* quiet operation -q */
		MakeCmdArg( argVee, i, 3, "-q" );
		pG->dll_handles_errors = 0;		/* All error msgs passed to caller via callback function */
	}
	if ( C->fRecurse && C->fVersion < 160 ) /* recurse into subdirectories -r */
		MakeCmdArg( argVee, i, 3, "-r" );
	/* Not a command line option but we set the global var directly */
	if ( C->fNoRecurseFiles == 2 ) pG->norecursefiles = 1;
	if (C->fSystem)					/* include system and hidden files -S */
		MakeCmdArg( argVee, i, 3, "-S" );
	if ( C->fGenDateUsed ) {		/* Exclude files earlier than specified date -t */
		MakeCmdArg( argVee, i, 3, "-t" );
      /* Date must follow the -t switch */
		MakeCmdArg( argVee, i, lstrlen( C->fGenDate ) + 1, C->fGenDate );
	}
	if ( C->fUpdate )					/* Update zip file--overwrite only if newer -u */
		MakeCmdArg( argVee, i, 3, "-u" );
	if ( (C->fVerbose ) || (pG->global_trace_opt) )  /* verbose messages -v */
		MakeCmdArg( argVee, i, 3, "-v" );
	if ( C->fVolume )					/* Include volume label -$ */
		MakeCmdArg( argVee, i, 3, "-$" );
	if ( C->fExtra )					/* Include extra attributes -X */
		MakeCmdArg( argVee, i, 3, "-X" );
	if ( C->fVersion < 160 ) {
		if ( C->fComprSpecial )		/* try to compress all files that are already compressed  */
			pG->special = NULL;
		else								/* Files with these extensions will not be compressed */
			pG->special = ".gif:.png:.Z:.zip:.zoo:.arc:.lzh:.arj:.taz:.tgz:.lha";
	} else pG->special = C->fSuffix;	/* Component v1.6 can specify the extensions that will not be compressed */
#ifdef CRYPT
	if ( C->fGenEncrypt && C->fVersion < 160 ) {   /* Include Encryption -e */
		MakeCmdArg( argVee, i, 3, "-e" );

		/* if no password, we will prompt user later */
		if ( (C->fGenPassword != NULL) && (lstrlen( C->fGenPassword ) > 0) ) {
			/* allocate memory for the password, and point the
			 * global password pointer to it.
			 */
			if ( (pG->key = MALLOC( PWLEN + 1 )) == NULL ) {
				ziperr( ZEN_MEM04, pG );
				return false;
			}
			lstrcpy( pG->key, C->fGenPassword ); /* copy password to global buf */
		}
	}
#endif
	if ( C->fVersion >= 160 ) {
		// Pass the excluded file spec. list.
		pG->pcount	 = ((ZCL2 *)C)->fTotExFileSpecs;
		pG->ExternalList = (struct plist *)(((ZCL2 *)C)->fExFiles);
	}
	return true;
}


#define STDIO_BUF_SIZE 16384

/* ===========================================================================
 * printf buffers the current output and counts the number of lines
 * within it.  It makes sure there is enough space in the global
 * buffer, then copies the buffered data to the global buffer.
 * It then triggers a repaint of the status buffer.
 */
int __cdecl printf( const char *format, ... ) {
	va_list         argptr;
	HANDLE          hMemory;
	LPSTR           Buffer;
	int				 Error;
	struct Globals *pG = GetGlobalPointer( &Error );

	if ( pG && (hMemory = GLOBALALLOC( GMEM_MOVEABLE, STDIO_BUF_SIZE )) != NULL ) {
		if ( (Buffer = GlobalLock( hMemory )) != NULL ) {
			va_start( argptr, format );
			 wvsprintf( Buffer, format, argptr );
			 // warning message, or info message only
			 user_callback( 4, 0, 0, Buffer, pG );
			 GlobalUnlock( hMemory );
			va_end( argptr );
		}
		GLOBALFREE( hMemory );
	}
	return 0;
}

/* ===========================================================================
 */
long __stdcall GetZipDllVersion( void ) {
	return ZIPVERS;	/* see version.h */
}

long __stdcall GetZipDllPrivVersion( void ) {
	return ZIPPRIVVERS;
}

/* ===========================================================================
 * ziperr() is the preferred error msg function!!!
 */
void msgbox( char *msg, struct Globals *pG ) {
	/* Did user pass us a window handle? */
	if ( pG->global_handle ) {		// RCV changed: was exit if no handle;
		lstrcpy( pG->ewetmp, "Msg From ZIP DLL: " );
		lstrcat( pG->ewetmp, msg );
		MessageBox( pG->global_handle, pG->ewetmp, "Msg From ZIP DLL", MB_OK );
	}
}


/* ===========================================================================
 */
void diag( char *msg, struct Globals *pG ) {
	if ( pG->verbose ) {
		/* log the message through the routine msg callback */
		lstrcpy( pG->ewetmp, "Trace Msg: " );
      lstrcat( pG->ewetmp, msg );
      /* specify a 0 error code to prevent a dialog box from coming
		 *	up in the BCB/Delphi application program */
		user_callback( 4, 0, 0, pG->ewetmp, pG );
	}
}

/* This provides the calling program with updated info on what the DLL
 * is doing.  Regardless of the type of call being made, the user's
 * function must have a spin of the Windows message loop.  In fact, even
 * if user isn't using a progress bar, he should still spin the msg
 * loop upon getting these callbacks (but he doesn't need to do anything
 * else).  In Delphi, "Application.ProcessMessages;" or
 *         in BCPPB   "Application->ProcessMessages(); spins the loop.
 * Here are the types of calls:
 *
 *   ActionCode = 1, we're starting a zip operation on a new file
 *      error_code = N/A
 *      fsize = filesize of file we're going to operate on
 *      name_or_msg = pathname of file
 *   IMPORTANT: The user's function must do the math for the progress
 *   bar upon getting this call.  See the Delphi sample application.
 *
 *   ActionCode = 2, increment the progress bar
 *      These calls will occur after every 32K of input file has been
 *      processed. One additional call is made at the end of each file,
 *      just to make sure the progress bar is max'ed out - this is also
 *      critical for files less than 32K in size (this last one will be
 *      their only one).
 *      error_code = N/A
 *      fsize = N/A
 *      name_or_msg = N/A
 *
 *   ActionCode = 3, we're done with a batch of files - program flow
 *   will quickly return to the user's program.
 *   NOTE: the end of a every file will always be followed by an
 *         action of 1 or 3, so a separate call for end of one file
 *         isn't needed.
 *      error_code = N/A
 *      fsize = N/A
 *      name_or_msg = N/A
 *
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
 *   ActionCode = 8, Also reserved by Unzip DLL. Password
 *      error_code = Y (Y=1 from Zipdll, Y=2 from UnzipDll) (O)
 *      error_code = 1 if user entered a password or 0 when user canceled or empty password (I)
 *      fsize = The repeat count (I)unzip dll only (O) both dll's
 *      name_or_msg = the new password to try or empty when canceled (I)
 *
 *   Actioncode = 9, CRC32 error during Extract ( UnzDll only )
 *      error_code = Found CRC (O)
 *      fsize = Stored/Expected CRC (O)
 *      name_or_msg = File for which the CRC check went wrong.
 *
 *   Actioncode = 10, Extract(UnZip) Overwrite ask.
 *
 *   Actioncode = 11, Skipped during Extract ( UnzDll only )
 *
 *   Actioncode = 12, FileComment while adding ( ZipDll only )
 *      name_or_msg[0-255]   = File for which the filecomment is asked. (O)
 *      name_or_msg[256-511] = Old filecomment if present. (O)
 *      name_or_msg[0-511]   = New filecomment if present. (I)
 *      error_code = 1 if filecomment has changed (I)
 *      fsize = size of new filecomment (I)
 *
 *   Actioncode = 13, Adjust unzip stream size ( UnzDll only )
*/


/* ===========================================================================
 * This calls the application program and passes status info.
 */
void user_callback( long action, long error_code, long fsize, char *name_or_msg, struct Globals *pG ) {
	if ( pG->callb ) {
		pG->callbackdata.actioncode     = action;
		/* Don't pass extended error info to components <= version 145 */
		pG->callbackdata.error_code     = (pG->GlobalCompVersion <= 145) ? (int)(char)(error_code & 0xFF) : error_code;
		pG->callbackdata.fsize          = fsize;
		if ( name_or_msg == NULL )
			pG->callbackdata.filenameormsg[0] = '\0';
		else
			if ( action == 12 )	// In this case there is an extra 0 byte which have to be copied too.
				memcpy( pG->callbackdata.filenameormsg, name_or_msg, 512 );
			else
				lstrcpy( pG->callbackdata.filenameormsg, name_or_msg );

		/* make the call and check the return code.  If true is returned, user
		 * wants to abort the current batch job.
		 */
		pG->global_abort_sw |= pG->callb( &pG->callbackdata );  // call user's program
		/* don't bother looking at the global_abort_sw here! */
	}
}

#ifdef USE_MEMCHECK	// RCV: 1.603
void *ExtAlloc1( size_t memsize, int line, char *file ) {
	int				 i, error;
	struct Globals *pG;
	void				*Ptr = malloc( memsize );

	if ( Ptr ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == Ptr || !MemData[i].memptr ) {
			MemData[i].memptr  = Ptr;
			MemData[i].memtype = 1;
			MemData[i].memlen  = memsize;
			MemData[i].line    = line;
			MemData[i].memcnt++;
			lstrcpy( MemData[i].file, file );
			return Ptr;
		}
	}
	// Very unlikely but...
	pG = GetGlobalPointer( &error );
	user_callback( 4, 0, 0, "Pointer is NULL or to many pointers", pG );
	return NULL;
}

HGLOBAL ExtAlloc2( UINT f, DWORD memsize, int line, char *file ) {
	int				 i, error;
	struct Globals *pG;
	HGLOBAL         Ptr = GlobalAlloc( f, memsize );

	if ( Ptr ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == Ptr || !MemData[i].memptr ) {
			MemData[i].memptr  = Ptr;
			MemData[i].memtype = 2;
			MemData[i].memlen  = memsize;
			MemData[i].line    = line;
			MemData[i].memcnt++;
			lstrcpy( MemData[i].file, file );
			return Ptr;
		}
	}
	// Very unlikely but...
	pG = GetGlobalPointer( &error );
	user_callback( 4, 0, 0, "Pointer is NULL or to many pointers", pG );
	return NULL;
}

#pragma argsused
void ExtFree1( void *ptr, int line, char *file ) {
	int i;

	for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == ptr && MemData[i].memtype == 1 ) {
		MemData[i].memcnt--;
		break;
	}
	free( ptr );
}

#pragma argsused
HGLOBAL ExtFree2( HGLOBAL ptr, int line, char *file ) {
	int i;

	for( i = 0; i < 4000; i++ ) if ( MemData[i].memptr == ptr && MemData[i].memtype == 2 ) {
		MemData[i].memcnt--;
		break;
	}
	return GlobalFree( ptr );
}

void DispMemDiag( void ) {
	int				 error;
	struct Globals *pG = GetGlobalPointer( &error );
	char				 ewetmp[2048], drive[MAXDRIVE], dir[MAXDIR], file[MAXFILE], ext[MAXEXT];
	int				 i;

	if ( pG->global_caller && pG->callb ) {
		for( i = 0; i < 4000; i++ ) if ( MemData[i].memcnt ) {
			// The global pointer can't be freed first because we still need it, so skip.
			if ( MemData[i].memptr == pG && MemData[i].memcnt == 1 ) continue;

			fnsplit( MemData[i].file, drive, dir, file, ext );
			sprintf( ewetmp, "MemAlloc: at %p size %ld file %s line %d type %d", MemData[i].memptr, MemData[i].memlen, file, MemData[i].line, MemData[i].memtype );
			user_callback( 4, 0, 0, ewetmp, pG );
			sprintf( ewetmp, "Data=: %*.*s", MemData[i].memlen, MemData[i].memlen, MemData[i].memptr );
			user_callback( 4, 0, 0, ewetmp, pG );
			MemData[i].memptr = NULL;
		}
	}
}
#endif




