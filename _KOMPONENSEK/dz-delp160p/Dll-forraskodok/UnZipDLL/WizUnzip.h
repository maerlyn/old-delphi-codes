/* wizunzip.h    This is the main UNZIP DLL interface descriptions.
 * Based on Windows Info-ZIP Unzip, version 3.0.
 * Authors: Robert A. Heath, M. White
 * This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 *
 * I used Borland C++ Builder for this port.
 * No use is made of C++ features in any of the code modules, so
 * if you're only familier with C, you will feel right at home.
 * Of course, this also means that we aren't using any
 * vendor-specific class libraries (MFC, OWL, etc.).
 * - Chris Vleghert & Eric W. Engler
 */

#ifndef __wizunzip_h   /* prevent multiple inclusions */
#define __wizunzip_h

#include <windows.h>
#include <stdio.h>

#define STDIO_BUF_SIZE 16384
#define bool BOOL

/* Allow compilation under Borland C++ also */
#ifndef __based
#  define __based( A )
#endif

/* Porting definitions between Win 3.1x and Win32 */

/* The following is to take care of some of the porting problems between
 * Win 3.1 and Win32 for WM_COMMAND notifications.
*/
#define GET_WM_COMMAND_CMD( wp, lp ) HIWORD( wp )

#ifndef NDEBUG
# define WinAssert( exp ) \
        {\
        if ( !(exp) ) { \
            char szBuffer[40]; \
            sprintf( szBuffer, "File %s, Line %d", __FILE__, __LINE__ ); \
            if ( IDABORT == MessageBox( (HWND)NULL, szBuffer, "Assertion Error", \
                MB_ABORTRETRYIGNORE | MB_ICONSTOP ) ) \
                    FatalExit( -1 ); \
            }\
        }
#else
# define WinAssert( exp )
#endif

/* define the data passed back to the BCB/Delphi callback function */
#pragma option -a1
typedef struct {
	HWND  handle;
	void *caller;
	long  version;
	bool  isoperationzip;			/* true=zip, false=unzip */
	long  actioncode;
	long  error_code;
	long  fsize;
	char  filenameormsg[512];	/* NOTE: NOT a pointer - data is here */
} CallBackStruct;
#pragma option -a.

/* Define a type called DLLCALLBK: */
typedef unsigned long __stdcall (*DLLCALLBK)(CallBackStruct *);


// DLL Command Line (DCL1)
#pragma option -a1
typedef struct {
	HWND				fHandle;				  /* handle of calling pgm's active Window								*/
	void			  *fCaller;				  /* object instance ("this/self") of calling BCB/Delphi form
													* (not used in DLL; returned to BCB/Delphi via callback) 		*/
	long				fVersion;			  /* version no. that BCB/Delphi Applic. expects						*/
	DLLCALLBK		fCallback;
	BOOL				fTraceEnabled;
	unsigned short fWantedCodePage;	  /* Gives the Code page the user has set new v1.5          */
	/*========================================================================================*/
	/*								  regular switches																*/
	unsigned short	fPromptToOverwrite; /* not supported yet (was bool before v1.5)				*/
	char			  *fPassword;			  /* ptr to password passed in from caller						*/
	BOOL				fTest;              /* if true, test zipfile												*/
	BOOL				fComments;          /* show zip comment (not supported yet)							*/
	BOOL				fConvert;           /* if true, do ASCII/EBCDIC or EOL translation				*/

	BOOL				fQuiet;          	  /* DLL be quiet!														*/
	BOOL				fVerbose;           /* verbose flag															*/
	BOOL				fUpdate;            /* "update" (extract only newer files & brand new files)	*/
	BOOL				fFreshen;           /* "freshen" (extract only newer files that already exist)*/
	BOOL				fDirectories;       /* if true, recreate dir structure								*/
	BOOL				fOverwrite;         /* if true, overwrite existing (no asking)						*/

	long				fArgc;				  /* Count of filespecs to extract									*/
	char			  *fZipFN;				  /* ptr to zip filename												*/
	int				fSeven;             /* stick a 7 in here to validate the struct offsets			*/
	char			  *fFNV[];				  /* array of filespec args to extract								*/
} DCL1;

typedef struct {
	char			  *fFileSpec;
	char			  *fFileAltName;
	char			  *fPassword;
	long				fNotUsed[15];
} fFileData;

typedef struct {							// Excluded file specs, not implemented yet
	char			  *fFileSpec;
	long				fNotUsed[3];
} fExFileData;

typedef struct {
	HWND				fHandle;
	void			  *fCaller;
	long				fVersion;
	DLLCALLBK		fCallback;
	BOOL				fTraceEnabled;
	unsigned short	fWantedCodePage;
	unsigned short	fPromptToOverwrite;
	char			  *fPassword;
	BOOL				fTest;
	BOOL				fComments;
	BOOL				fConvert;
	BOOL				fQuiet;
	BOOL				fVerbose;
	BOOL				fUpdate;
	BOOL				fFreshen;
	BOOL				fDirectories;
	BOOL				fOverwrite;
	long				fArgc;
	char			  *fZipFN;
	fFileData	 (*fFNV)[];
	fExFileData	 (*fExFileSpec)[];		// pxnames and xfilespecs.
	BOOL				fUseOutStream;			// NEW Use memory stream as output.
	void			  *fOutStream;				// NEW Pointer to the start of the output stream data.
	unsigned long	fOutStreamSize;		// NEW Size of the output data.
	BOOL				fUseInStream;			// NEW Use memory stream as input.
	void			  *fInStream;				// NEW Pointer to the start of the input stream data.
	unsigned long	fInStreamSize;			// NEW Size of the input data.
	unsigned long	fPwdReqCount;			// NEW PasswordRequestCount, How many times a password will be asked per file
	char			  *fExtractDir;			// NEW Extract Directory needed to supports treads.
	long				fNotUsed[8];
	int				fSeven;
} DCL2;
#pragma option -a.

bool  WizUnzipInit( HANDLE hInst );
int   win_fprintf( FILE *file, unsigned int, char * );
void  GetDirectory( LPSTR lpDir );
bool  MakeDirectory( char *path, bool fileAttached );

/* RCV Changed exports for the DLL */
extern __stdcall __declspec(dllexport) long GetUnzDllVersion( void );
extern __stdcall __declspec(dllexport) long GetUnzDllPrivVersion( void );
extern __stdcall __declspec(dllexport) long UnzDllExec( DCL1 *C );

#endif

