/* Win32.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
*/

/*
 * Win32 specific functions for ZIP.
 *
 * This version of ZIP heavily relies on the MSDOS version,
 * since we have to do similar things to switch between NTFS, HPFS and FAT.
 */
#include "zip.h"
#include "Globals.h"

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <ctype.h>
#include <windows.h>
#include "win32zip.h"

#define EAID     0x0009


/* ===========================================================================
 * NOTE: NTFS is handled by the HPFS code.
 * FAT / HPFS detection.
 */
int IsFileSystemOldFAT( char *dir, struct Globals *pG ) {
	char  *fp, *p;
	DWORD vfnsize;
	DWORD vfsflags;

	/* We separate FAT and HPFS+other file systems here.
	 * I consider other systems to be similar to HPFS/NTFS, i.e.
	 * support for long file names and being case sensitive to some extent.
	 */
	fp = GetFullPath( pG, dir ); //v1.6017
	if ( isalpha( fp[0] ) && fp[1] == ':' ) {
		fp[0] = to_up( fp[0] );
		fp[1] = ':';
		fp[2] = '\\';
		fp[3] = 0;
	}
	// specify \\MyServer\MyShare as \\MyServer\MyShare\.
	if ( fp[0] == '\\' && fp[1] == '\\' ) {
		if ( (p = strchr( &fp[2], '\\' )) != NULL ) p = strchr( p + 1, '\\' );
		if ( p ) *(p + 1) = 0;
	}
	if ( !GetVolumeInformation( fp, NULL, 0, NULL, &vfnsize, &vfsflags, NULL, 0 )) {
		printf( "zip diagnostic: GetVolumeInformation failed for %s\n", fp );
		return( false );
	}
	return vfnsize <= 12;
}

/* ===========================================================================
 * access mode bits and time stamp.
 */
int GetFileMode( struct Globals *pG, char *name ) {
	DWORD dwAttr;

	dwAttr = GetFileAttributes( GetFullPath( pG, name ) ); //v1.6017
	if ( dwAttr == 0xFFFFFFFF ) {	/* RCV Changed: was ...= -1 */
		printf( "zip diagnostic: GetFileAttributes failed\n" );
		return( 0x20 ); /* the most likely, though why the error? security? */
	}
	return( ( dwAttr & FILE_ATTRIBUTE_READONLY  ? A_RONLY   : 0 )
         | ( dwAttr & FILE_ATTRIBUTE_HIDDEN    ? A_HIDDEN  : 0 )
         | ( dwAttr & FILE_ATTRIBUTE_SYSTEM    ? A_SYSTEM  : 0 )
         | ( dwAttr & FILE_ATTRIBUTE_DIRECTORY ? A_DIR     : 0 )
         | ( dwAttr & FILE_ATTRIBUTE_ARCHIVE   ? A_ARCHIVE : 0 ) );
}

/* ===========================================================================
 */
/*long GetTheFileTime( char *name ) {
	HANDLE   h;
	FILETIME ft, lft;
	WORD     dh, dl;

	h = CreateFile( name, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL );
	if ( h != INVALID_HANDLE_VALUE ) {
		GetFileTime( h, NULL, NULL, &ft );
		FileTimeToLocalFileTime( &ft, &lft );
		FileTimeToDosDateTime( &lft, &dh, &dl );
		CloseHandle( h );
		return( dh << 16 ) | dl;
	} else return 0L;
}*/

/* ===========================================================================
 */
/*void ChangeNameForFAT( char *name ) {
	static char invalid[] = ":\"<>|\t";		// V1.4 Changed was: ":;,=+\"[]<>| \t"
	char *src, *dst, *next, *ptr, *dot, *start;

	if ( isalpha( name[0] ) && (name[1] == ':') )
		start = name + 2;
	else
		start = name;

	src = dst = start;
	if ( *src == '\\' ) src++, dst++;

	while ( *src ) {
		for ( next = src; *next && (*next != '\\'); next++ );		// SLASH

		for ( ptr = src, dot = NULL; ptr < next; ptr++ ) if ( *ptr == '.' ) {
			 dot = ptr; // remember last dot
			*ptr = '_';
		}

		if ( dot == NULL ) for ( ptr = src; ptr < next; ptr++ )
			if ( *ptr == '_' )
				dot = ptr; // remember last _ as if it were a dot

		if ( dot && (dot > src) && ((next - dot <= 4) ||
          ((next - src > 8) && (dot - src > 3))) ) {
			if ( dot ) *dot = '.';

			for ( ptr = src; (ptr < dot) && ((ptr - src) < 8); ptr++ )  *dst++ = *ptr;

			for ( ptr = dot; (ptr < next) && ((ptr - dot) < 4); ptr++ ) *dst++ = *ptr;
		} else {
			if ( dot && (next - src == 1) )
				*dot = '.';           // special case: "." as a path component

			for ( ptr = src; (ptr < next) && ((ptr - src) < 8); ptr++ ) *dst++ = *ptr;
		}

		*dst++ = *next; // either '/' or 0

		if ( *next ) {
			src = next + 1;

			if ( *src == 0 ) // handle trailing '/' on dirs !
				*dst = 0;
		} else break;
	}

	for ( src = start; *src != 0; ++src )
		if ( strchr( invalid, *src ) != NULL ) *src = '_';	// V1.4 removed was ... || (*src == ' ')
}*/

/* ===========================================================================
 */
char *GetLongPathEA( void ) {
	return( NULL ); /* volunteers ? */
}

/* ===========================================================================
 */
/*int IsFileNameValid( char *x ) {
	WIN32_FIND_DATA fd;
	HANDLE          h = FindFirstFile( x, &fd );

	if ( h == INVALID_HANDLE_VALUE ) return false;
	FindClose( h );
	return true;
} */

/* ===========================================================================
 */
//char *StringLower( char *szArg ) {
//	char *szPtr;		/* RCV Changed: was unsigned char. */

//	for ( szPtr = szArg; *szPtr; szPtr++ ) *szPtr = lower[*szPtr];
//	return szArg;
//}


/* ===========================================================================
 * If a volume label exists for the given drive, return its name and
 *  pretend to set its time and mode. The returned name is global data.
 *
 * Drive =  drive name: 'A' .. 'Z' or '\0' for current drive
 */
char *getVolumeLabel( struct Globals *pG, int Drive, ulg *vtime, ulg *vmode, time_t *vutim ) {	// v1.6017
	char RootPath[ 4 ];
	ulg  fnlen, flags;
   long OldMode = SetErrorMode( SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX );
   BOOL Result;

	*vmode = A_ARCHIVE | A_LABEL;					/* this is what msdos returns  */
	*vtime = dostime( 1980, 1, 1, 0, 0, 0 );	/* no true date info available */
	*vutim = dos2unixtime( *vtime );

	lstrcpy( RootPath, "x:\\" );
	RootPath[ 0 ] = (char)Drive;
	Result = GetVolumeInformation( Drive ? RootPath : NULL, pG->vol, MAX_PATH - 1, NULL, &fnlen, &flags, NULL, 0 );
	SetErrorMode( OldMode );
	return Result ? pG->vol : NULL;
}

