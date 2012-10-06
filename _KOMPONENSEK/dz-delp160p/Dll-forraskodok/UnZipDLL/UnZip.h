/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 *
 * EWE NOTE:  Everything in this file is critical!
 */
/*---------------------------------------------------------------------------
 * unzip.h
 * This header file contains the public macros and typedefs required by
 * both the UnZip sources and by any application using the UnZip API.  If
 * it includes unzpriv.h, which then includes globals.h.
 *---------------------------------------------------------------------------*/

#ifndef __unzip_h   /* prevent multiple inclusions */
#define __unzip_h

#include <stdio.h>
#include <windows.h>

#include "unzerr.h"   

#ifndef CRYPT
# define CRYPT 1
#endif

extern DWORD TgbIndex;	// RCV added, our only none constant global.

/* added by E. Engler; these are in dllmain.c */
// see Trace macro in UnzPriv.h
extern void __cdecl MyTrace( struct Globals *pG, const char *, ... );
extern void user_callback( struct Globals *pG, long action, long error_code, long fsize, char *name_or_msg );
extern void MsgBox( struct Globals *gP );
extern void diag( struct Globals *pG, char *msg );
extern void UnzErr( struct Globals *gP, int c );


#ifndef USE_MEMCHECK
# define MALLOC( s ) malloc( (s) )
# define GLOBALALLOC( f, s ) GlobalAlloc( (f), (s) )
# define FREE( p ) free( (void *)(p) )
# define GLOBALFREE( p ) GlobalFree( (p) )
#else
# define MEMCHECKSIZE 4000		// 4000 positions to save memory pointers
# define MALLOC( s ) ExtAlloc1( (s), __LINE__, __FILE__ )
# define GLOBALALLOC( f, s ) ExtAlloc2( (f), (s), __LINE__, __FILE__ )
# define FREE( p ) ExtFree1( (p), __LINE__, __FILE__ )
# define GLOBALFREE( p ) ExtFree2( (p), __LINE__, __FILE__ )
extern void *ExtAlloc1( size_t memsize, int line, char *file );
extern HGLOBAL ExtAlloc2( UINT f, DWORD memsize, int line, char *file );
extern void ExtFree1( void *ptr, int line, char *file );
extern HGLOBAL ExtFree2( HGLOBAL ptr, int line, char *file );
extern void DispMemDiag( void );
#endif

/*****************************************/
/*  Predefined, Machine-specific Macros  */
/*****************************************/
#if (!defined(DOSWILD))
#  define DOSWILD
#endif

#if (!defined(NO_ZIPINFO))
#  define NO_ZIPINFO
#endif

#if (!defined(MSDOS))
#  define MSDOS
#endif

// By defining COPYRIGHT_CLEAN, we lose support for these 2 compression
// types: tokenizing (which was never popular)
//   and  reducing   (which was only used in pre-v1 beta releases of PKZIP
#ifndef COPYRIGHT_CLEAN
#  define COPYRIGHT_CLEAN
#endif

#ifndef NO_MULTIPART
#  define NO_MULTIPART
#endif

#define true  1
#define false 0

#if (defined ASM_INFLATECODES)
# undef ASM_INFLATECODES
#endif

#ifndef EXPENTRY
#  define UZ_EXP WINAPI
#else
#  define UZ_EXP EXPENTRY
#endif

#ifndef UZ_EXP
#  define UZ_EXP
#endif

// EWE note: All platforms need this VMS version:
#define VMS_UNZIP_VERSION 42

/***************************/
/*  OS-Dependent Includes  */
/***************************/
#ifndef NO_STDDEF_H
#  include <stddef.h>
#endif
#ifndef NO_STDLIB_H
#  include <stdlib.h>  /* standard library prototypes, malloc(), etc. */
#endif
typedef size_t extent;

typedef unsigned char   uch;    /* code assumes unsigned bytes; these type-  */
typedef unsigned short  ush;    /* defs replace byte/UWORD/ULONG (which are  */
typedef unsigned long   ulg;    /* predefined on some systems) & match zip   */

/* InputFn is not yet used and is likely to change: */
typedef int   (MsgFn)    ( struct Globals *pG, uch *buf, ulg size, int flag );
typedef int   (InputFn)  ( struct Globals *pG, uch *buf, int *size, int flag );
typedef void  (PauseFn)  ( struct Globals *pG, const char *prompt, int flag );
typedef int   (PasswdFn) ( struct Globals *pG, int *rcnt, char *pwbuf, int size, char *zfn, char *efn );

typedef struct _UzpBuffer {   /* rxstr */
	ulg    strlength;          /* length of string  */
	char  *strptr;             /* pointer to string */
} UzpBuffer;

typedef struct _UzpInit {
	ulg structlen;            /* length of the struct being passed */

	/* GRR: can we assume that each of these is a 32-bit pointer?  if not,
	 * does it matter? add "far" keyword to make sure? */
	MsgFn   *msgfn;
	InputFn *inputfn;
	PauseFn *pausefn;

	void (*userfn)();    /* user init function to be called after globals */
                        /*  constructed and initialized */

	/* pointer to program's environment area or something? */
	/* hooks for performance testing? */
	/* hooks for extra unzip -v output? (detect CPU or other hardware?) */
	/* anything else?  let me (Greg) know... */
} UzpInit;

#ifdef NEVER
/* intended to be a private struct: */
typedef struct _ver {
	uch major;              /* e.g., integer 5 */
	uch minor;              /* e.g., 2 */
	uch patchlevel;         /* e.g., 0 */
	uch not_used;
} _version_type;

typedef struct _UzpVer {
	ulg structlen;          /* length of the struct being passed */
	ulg flag;               /* bit 0: is_beta   bit 1: uses_zlib */
	char *betalevel;        /* e.g., "g BETA" or "" */
	char *date;             /* e.g., "4 Sep 95" (beta) or "4 September 1995" */
	char *zlib_version;     /* e.g., "0.95" or NULL */
	_version_type unzip;
	_version_type zipinfo;
	_version_type os2dll;
	_version_type windll;
} UzpVer;
#endif

typedef struct central_directory_file_header { /* CENTRAL */
	uch version_made_by[2];
	uch version_needed_to_extract[2];
	ush general_purpose_bit_flag;
	ush compression_method;
	ush last_mod_file_time;
	ush last_mod_file_date;
	ulg crc32;
	ulg csize;
	ulg ucsize;
	ush filename_length;
	ush extra_field_length;
	ush file_comment_length;
	ush disk_number_start;
	ush internal_file_attributes;
	ulg external_file_attributes;
	ulg relative_offset_local_header;
} cdir_file_hdr;


#define UZPINIT_LEN   sizeof( UzpInit )
// #define UZPVER_LEN    sizeof(UzpVer)
#define cbList( func )  int (*func)(char *filename, cdir_file_hdr *crec)

/*---------------------------------------------------------------------------
    Prototypes for public UnZip API (DLL) functions.
  ---------------------------------------------------------------------------*/
int      UzpMain            ( int argc, char **argv );
int      UzpAltMain         ( int argc, char **argv, UzpInit *init );
// UzpVer  *UzpVersion         ( void );
//int      UzpUnzipToMemory   ( char *zip, char *file, UzpBuffer *retstr );
int      UzpFileTree        ( char *name, cbList( callBack ), char *cpInclude[], char *cpExclude[] );

/* default I/O functions (can be swapped out via UzpAltMain() entry point): */
int      UzpMessagePrnt     ( struct Globals *pG, uch *buf, ulg size, int flag );
int      UzpMessageNull     ( struct Globals *pG, uch *buf, ulg size, int flag );
int      UzpInput           ( struct Globals *pG, uch *buf, int *size, int flag );
// void     UzpMorePause       ( struct Globals *pG, const char *prompt, int flag);

/* added for encryption support */
int		UzpGetPassWrd		 ( struct Globals *pG );

/*---------------------------------------------------------------------------
    Remaining private stuff for UnZip compilation.
  ---------------------------------------------------------------------------*/
#include "unzpriv.h"

#endif /* !__unzip_h */

