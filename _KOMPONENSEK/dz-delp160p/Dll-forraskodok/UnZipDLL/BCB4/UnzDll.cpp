#include <windows.h>
#pragma hdrstop
#include <condefs.h>

//---------------------------------------------------------------------------
//   Important note about DLL memory management in a VCL DLL:
//
// If your DLL uses VCL and exports any functions that pass VCL String objects
// (or structs/classes containing nested Strings) as parameter or function
// results, you will need to build both your DLL project and any EXE projects
// that use your DLL with the dynamic RTL (the RTL DLL).  This will change your
// DLL and its calling EXE's to use BORLNDMM.DLL as their memory manager. In
// these cases, the file BORLNDMM.DLL should be deployed along with your DLL
// and the RTL DLL (CP3240MT.DLL). To avoid the requiring BORLNDMM.DLL in
// these situations, pass string information using "char *" or ShortString
// parameters and then link with the static RTL.
//
//---------------------------------------------------------------------------

/* ---------------------------------------------------------------------------
 *  Note about used '#defines'.
 *  Global defined in the makefile.
 *		WIN32						Windows '95 and NT.
 *		NDEBUG					Don't use included debug code. (See also DEBUG and Trace!)
 *		NO_ASM
 *		DOS
 *		MSDOS						Used for the file system. (Also in unzip.h?)
 *		REENTRANT				This Dll can be used by more than one thread simultaniously.
 *    CRTL_CP_IS_ISO			C RunTimeLibrary uses ISO Codepage.
 *									(do not use CRTL_CP_IS_OEM at the same time!)
 *		USE_STRM_OUTPUT		Use Stream Output, No file is created, output goes into
 *									a memory buffer created by the component.
 *		USE_STRM_INPUT			Use Stream Input (only C++Builder/Delphi streams.)
 *
 *  Defined in one of the headers:
 *		COPYRIGHT_CLEAN		We lose the unzipping of tokenizing and reducing.
 *		UNZIP                Specifies we WILL compile the UnzDll.dll
 *		NO_MULTIPART			No spanning allowed (in this DLL)
 *		PKZIP_BUG_WORKAROUND fix a PKZIP 1.93a problem.
 *		USE_EF_UX_TIME			Use the Extra Field Unix Time if present.
 *
 *  Not defined at the moment but possible to set:
 *		NOCPYRT					Do not include the static copyright lines.
 *		DEBUG						Use debug code. (see also NDEBUG and Trace!)
 *		Trace						Use special trace mode. (See also DEBUG and NDEBUG!)
 *		NO_DEBUG_IN_MACROS	Do not use Trace(printf) in macros.
 *		CRYPT_DEBUG				Use debug code in crypt module.
 *		DYNAMIC_CRC_TABLE		Used if the CRC table must be calcualted (now is static.)
 *		DYNALLOC_CRCTAB		Used if CRC table must be allocated dynamicly.
 *									only in combination with: DYNAMIC_CRC_TABLE.
 *		FUNZIP               ???
 *		NOMEMCPY					Don't use memcpy function ?Why?
 *    CRTL_CP_IS_OEM			C RunTimeLibrary uses OEM Codepage.
 *									(do not use CRTL_CP_IS_ISO at the same time!)
 *		USE_MEMCHECK			Used to check for memory problems (not freed, double freed)
 *
 *  Not defined and, at the moment not possible to set
 *    TIMESTAMP            ???
 *
 *  Note about the editor.
 *		To show the source orderly set the tabstops as following:
 *		4 7 10 13 16 19 22 25 28 31 34 37 40 43 46 ...
 * ---------------------------------------------------------------------------
 */

 /*
 *		Changes:
 *
 * Version 1.600  Identical to v1.506, just a new number to avoid warnings by the component.
 * Version 1.601  Added Output to a memory stream from a zipfile. Enabled by: USE_STRM_OUTPUT
 * Version 1.602  Process Detach also frees the global structure for the main thread.
 * Version 1.603  Changed the way the inputfile is closed, sometimes it was closed twice.
 * Version 1.604  Changed TakeDownFromProcessZipFile function memory was not always freed properly.
 * Version 1.605  Added USE_MEMCHECK directive, if set we can check memory allocations.
 *					   this is only for testing purposes needed.
 * Version 1.606  Made it possible to use an Input stream as ZipInput. Enabled by: USE_STREAM_INPUT.
 * Version 1.607  11-Feb 99 Added support for the new PasswordError event and PasswordRequestCount.
 * Version 1.608  12-Feb-99 Fixed a memory leak when using Encryption.
 * Version 1.609  15-Feb-99 Added support for the new CRC32Error event.
 * Version 1.6010 24-Feb-99 Added support for the new ExtractOverwrite and ExtactSkipped event.
 * Version 1.6011 06-Mar-99 Removed some unnecessary function parameters found by BCB4.
 * Version 1.6012 08-Mar-99 Fixed the way ExtrFreshen worked in extract.c (Existing newer files
 *                were also overwritten when ExtractOverwrite was true) Thanks to Jim Hoops.
 *                [v1.527 is equal to this version with the exception of USE_STRM_OUTPUT].
 * Version 1.6013 27-May-99 Fixed a bug when a password was set (and needed) in the v1.52 component
 *						and using the DLL v.1.527, extract failed in this case, unless the password was
 *						NOT set at all, then the password was asked through the Password Dialog.
 *						(The combination component v1.60 and the DLL 1.6012 did not have this bug.)
 *                [v1.528 is equal to this version with the exception of USE_STRM_OUTPUT].
 *
 *
 */

USERES("UnzDlg.res");
USEUNIT("List.c");
USEUNIT("Explode.c");
USEUNIT("Match.c");
USEUNIT("Extract.c");
USEUNIT("Fileio.c");
USEUNIT("Unshrink.c");
USEUNIT("Dllmain.c");
USEUNIT("Crctab.c");
USEUNIT("Crc32.c");
USEUNIT("Win32.c");
USEUNIT("Globals.c");
USEUNIT("Inflate.c");
USEUNIT("Process.c");
USEUNIT("Crypt.c");
USEUNIT("Passmsg.c");
USEFILE("Unzip.h");
USEFILE("Version.h");

//---------------------------------------------------------------------------
extern "C" DWORD TgbIndex;	// The only global used by all threads except for some constants.
extern "C" void ReleaseGlobalPointer( int status );

//---------------------------------------------------------------------------
#pragma argsused
int WINAPI DllEntryPoint( HINSTANCE hinst, unsigned long reason, void * ) {
	switch ( reason ) {
		case DLL_PROCESS_ATTACH:	// Allocate a index.
			if ( (TgbIndex = TlsAlloc()) == 0xFFFFFFFF ) return 0;
			break;

		case DLL_PROCESS_DETACH:
			ReleaseGlobalPointer( 0 );	// Make sure, only for the main thread.
			if ( TgbIndex ) {
				TlsFree( TgbIndex );		// Release the index.
				TgbIndex = 0;
			}
	}
	return true;
}

