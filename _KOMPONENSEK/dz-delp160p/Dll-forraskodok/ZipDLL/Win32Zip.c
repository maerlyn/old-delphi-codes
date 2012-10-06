/* Win32Zip.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
*/

#include "zip.h"
#include "Globals.h"
#include "win32zip.h" 

#include <direct.h>     /* for rmdir() */
#include <time.h>
#include <utime.h>
#include <windows.h>    /* for findfirst/findnext stuff         */
#include <io.h>
#include <ctype.h>

char *GetLongPathEA( void );

#define PAD           0
#define PATH_END      '\\'		// SLASH
#define HIDD_SYS_BITS ( FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM )

typedef struct zdirent {
	ush    d_date, d_time;
	ulg    d_size;
	char   d_attr;
	char   d_name[MAX_PATH];
	int    d_first;
	HANDLE d_hFindFile;
} zDIR;

/* Local functions */
static zDIR           *Opendir( const char *, struct Globals *pG );
static struct zdirent *Readdir( zDIR * );
static void            Closedir( zDIR * );
static char           *readd( zDIR *, struct Globals *pG );

/* ===========================================================================
 * Start searching for files in the MSDOS directory "n"
 * Return a pointer to a zDIR struct.
	*n :: Directory to open.
*/
static zDIR *Opendir( const char *n, struct Globals *pG ) {
	zDIR *d;              /* malloc'd return value */
	char *p;              /* malloc'd temporary string */
	char *q;
	WIN32_FIND_DATA fd;

	//sprintf( pG->ewemsg, "Opendir win32zip.c, path passed in =%s", n );
	//diag( pG->ewemsg, pG );

	if ( (d = (zDIR *)MALLOC( sizeof( zDIR ) )) == NULL || (p = MALLOC( lstrlen( n ) + pG->OCDlength + 5 )) == NULL ) {
		if ( d != NULL ) FREE( d );
		return NULL;
	}
	/* d points to our return zDIR struct,
	 * p points to a temp buf where we'll play with pathname
	 */
	*p = 0;
	if ( (!isalpha( n[0] ) || n[1] != ':') && (n[0] != '\\' || n[1] != '\\') )
		lstrcpy( p, pG->OrigCurrentDir ); // v1.6017
	lstrcat( p, n );

	q = p + lstrlen( p );
	/* q points to null byte */

	/*    q - p is length of pathname */
	/*  *(q - 1) is last byte of pathname */

	if ( (q - p) > 0 && *(q - 1) != '\\' ) *q++ = '\\';  /* add a trailing '/' if not already there SLASH */

	// the following is effectively a lstrcat
	lstrcpy( q, pG->wild_match_all ); /*  "*.*"  MS-DOS match-all spec */

	//sprintf( pG->ewemsg, "doing findfirst with spec %s", p );
	//diag( pG->ewemsg, pG );

	d->d_hFindFile = FindFirstFile( p, &fd );
	FREE( p );

	if ( d->d_hFindFile == INVALID_HANDLE_VALUE ) {
		FREE( d );
		return NULL;
	}

	lstrcpy( d->d_name, fd.cFileName );
	// sprintf( ewemsg, "first entry = %s", d->d_name );
	// diag( ewemsg );

	d->d_attr = (unsigned char)fd.dwFileAttributes;
	d->d_first = 1;
	return d;
}

/* ===========================================================================
 * Read the next directory entry. Pass in the zDIR ptr obtained from OpenDir.
 * Return a pointer to it (zdirent struct *),
 * or NULL if end of directory.
	*d :: Directory stream to read from.
 */
static struct zdirent *Readdir( zDIR *d ) {
	if ( d->d_first )    // This tells us if we just did opendir, and still
		d->d_first = 0;   // have it's spec ready to return to caller.
	else {
		WIN32_FIND_DATA fd;

		if ( !FindNextFile( d->d_hFindFile, &fd ) ) return NULL;
		lstrcpy( d->d_name, fd.cFileName );
		// sprintf( ewemsg, "NEXT ENTRY = %s", d->d_name );
		// diag( ewemsg );
		d->d_attr = (unsigned char)fd.dwFileAttributes;
	}
	return (struct zdirent *)d;
}

/* ===========================================================================
 * Close a directory.  Pass in the zDIR ptr received from OpenDir
 */
static void Closedir( zDIR *d ) {
	FindClose( d->d_hFindFile );
	FREE( d );
}

/* ===========================================================================
 * Read next "normal" directory entry.
 * Hidden and system files are skipped when needed
 * Files with the archive bit not set are skipped when needed.
	zDIR:	ptr received from OpenDir.
 * Return a pointer to the zdirent struct for the next name in the directory
 * or NULL if no more entries or an error occurs.
 */
static char *readd( zDIR *d, struct Globals *pG ) {
	struct zdirent *e;

	// diag( "readd win32zip.c" );

	/* Loop to read all dir entries, until next normal entry is found */
	do
		e = Readdir( d );
	while ( !pG->global_abort_sw && e && ( (!pG->hidden_files &&  (e->d_attr & HIDD_SYS_BITS)) ||
													   ( pG->ArchiveFiles && !(e->d_attr & FILE_ATTRIBUTE_DIRECTORY) && !(e->d_attr & FILE_ATTRIBUTE_ARCHIVE)) ));

	if ( pG->global_abort_sw || !e ) return NULL;
	else return e->d_name;
}

char *AddSlash( char *p ) {
	int len;

	if ( p && (len = lstrlen( p )) != 0 )
		if ( p[len - 1] != '\\' && p[len - 1] != ':' ) lstrcat( p, "\\" );	// SLASH
	return p;
}

/* ===========================================================================
 * If not in exclude mode, expand the pattern based on the contents
 * of the file system.
 * This function is used while gathering filenames to be added or updated
	*w :: Path/pattern to match.
   Possible return values: ZEN_MISS, ZEN_OK, ZEN_ABORT, ZEN_MEM or ZEN_PARMS.
 */
int Wild( char *w, struct Globals *pG ) {
	zDIR *d;                   /* Stream for reading directory       */
	char *e;                   /* File or directory name found.      */
	char *n;                   /* Constructed name from directory    */
	int   WError;              /* Result of Wild()                   */
	char *p, *a;               /* path originale and fixed.          */
	char *q;                   /* Filename / pattern.                */
	int   r;                   /* Result / temp var.                 */
	char  v[5];                /* space for device current directory.*/
	bool  StopRecurs = false;  /* No recursion if filespec is file.  */

	//	sprintf( pG->ewemsg, "in Wild of win32zip.c, pattern=%s recurse=%d", w, pG->recurse );
	//	diag( pG->ewemsg, pG );

	// "zip -$ foo a:" can be used to force a drive name once. 	// v1.6017
	if ( pG->volume_label == 1 ) {
		pG->volume_label = 2;
		pG->label = getVolumeLabel( pG, (w != NULL && w[ 1 ] == ':') ? to_up( w[ 0 ] ) : '\0', &pG->label_time, &pG->label_mode, &pG->label_utim );
		if ( pG->label != NULL )
			(void)newname( pG->label, 0, pG );
		if ( w == NULL || (w[1] == ':' && w[2] == '\0') ) return ZEN_OK;
	}

	/* Allocate and copy pattern */
	if ( (p = a = MALLOC( lstrlen( w ) + 1) ) == NULL ) return ZEN_MEM19;
	lstrcpy( p, w );

	/* Separate path and name into p and q */
   // We have '\' or '\name' or 'path1\path2\name2' or 'C:\path\name' but NOT 'C:\name'
	if ( (q = strrchr( p, '\\' )) != NULL && (q == p || q[-1] != ':') ) {		// SLASH
		*q++ = '\0';
		if ( *p == '\0' ) p = lstrcpy( v, "\\." );	   /* if path is just '\' SLASH               */
	} else if ( (q = strrchr( p, ':' )) != NULL ) {	/* We have 'C:' or 'C:\' or 'C:\name' */
		*q++ = '\0';
		p = lstrcat( lstrcpy( v, p ), ":" );          /* copy device as path eg. 'C:'       */
		if ( *q == '\\' ) {                          /* -> device:/., name  eg. 'C:\.' SLASH     */
			lstrcat( p, "\\" );		// SLASH
			q++;										         /* name or nothing.                   */
		}
		lstrcat( p, "." );   						         /* eg. 'C:.' or 'C:\.'                */
	} else if ( pG->recurse && (!strcmp( p, "." ) || !strcmp( p, ".." )) ) {
		/* current or parent directory */
		/* Get "zip -r foo ." to work. Allow the dubious "zip -r foo .." but
		 * reject "zip -r -m foo ..".  "dispose" means wipe out source path.
		 */
		if ( pG->dispose && !strcmp( p, ".." ) ) ziperr( ZEN_PARMS15, pG );
		q = (char *)pG->wild_match_all;
	}
	else {  /* no path or device */
		q = p;
		p = lstrcpy( v, "." );
	}
	if ( pG->recurse && *q == '\0' ) q = (char *)pG->wild_match_all;
	/* take out a possibly redundant dir name of "." */
	if ( (r = lstrlen( p )) > 1 && (strcmp( p + r - 2, ":." ) == 0 || strcmp( p + r - 2, "\\." ) == 0) )	// SLASH
		*(p + r - 1) = '\0';

	/* Only filename (not the path) can have special matching characters */
	if ( IsShExp( p ) ) {
		diag( "path has illegal chars", pG );
		FREE( a );
		return ZEN_PARMS16;
	}
	// sprintf( ewemsg, "at break up place in Wild: path=%s  name=%s", p, q );
	// diag( ewemsg );

	if ( !IsShExp( q ) ) {	// Speed up checking if file exits in case there are no wildcards
		struct stat s;       // and no recursion and no archiving v1.6016

		if ( !pG->recurse && !pG->ArchiveFiles ) {
			if ( !LSSTAT( GetFullPath( pG, w ), &s ) )                 /* file exists ? */
				return procname( w, false, pG );
			return ZEN_MISS02;                      /* woops, no wildcards where is the file! */
		}
		if ( pG->norecursefiles ) StopRecurs = true;
	}

	/* Now that we have a dir spec, along with an fspec, we'll step
	 * in the dir specified to see if there's any matches against the fspec.
	 */
	WError = ZEN_MISS02;
	if ( (d = Opendir( p, pG )) != NULL ) {
		while ( (e = readd( d, pG )) != NULL ) {
			if ( pG->global_abort_sw ) {
				WError = ZEN_ABORT;
				break;
			}
			// sprintf( ewemsg, "Found %s: %s", d->d_attr & FILE_ATTRIBUTE_DIRECTORY ? "directory" : "file", e );
			// diag( ewemsg );
			/* if e is NOT '.' or '..', and is a dir or match fspec. */
			if ( strcmp( e, "." ) && strcmp( e, ".." ) && (d->d_attr & FILE_ATTRIBUTE_DIRECTORY || dosmatch( q, e, pG )) ) {
				// diag( "Matched" );
				/* we matched fspec or it's a dir and entry is not '.' or '..' */
				if ( d->d_attr & FILE_ATTRIBUTE_DIRECTORY ) {
					// We do not save dirs or go into dirs if norecursefiles==1 and we a file without * or ? specs.
					if ( !StopRecurs && (pG->dirnames || pG->recurse) ) {
						if ( (n = MALLOC( lstrlen( p ) + lstrlen( e ) + lstrlen( q ) + 3 )) == NULL ) {
							WError = ZEN_MEM20;
							break;
						}
						*n = '\0';
						if ( *p != '.' ) AddSlash( lstrcpy( n, p ) );	// No ./ as first dir.
						lstrcat( n, e );
						if ( pG->dirnames ) {		// Save directory names also.
							r = procname( n, false, pG );
							if ( (int)(char)(r & 0xFF) > ZEN_OK || !pG->recurse ) FREE( n );
							if ( (int)(char)(r & 0xFF) > (int)(char)(WError & 0xFF) ) WError = r;
							if ( (int)(char)(r & 0xFF) > ZEN_OK ) break;
						}
						if ( pG->recurse ) {	// Recursively go into dir and check for other pattern matches.
							r = Wild( lstrcat( AddSlash( n ), q ), pG );	// Add the original pattern.
							FREE( n );
							// We keep a ZEN_OK even when ZEN_MISS occurs.
							if ( (int)(char)(r & 0xFF) > (int)(char)(WError & 0xFF) ) WError = r;
							if ( (int)(char)(r & 0xFF) > ZEN_OK ) break;						// An error, stop processing.
						}
   		      }
				} else {
					if ( (n = MALLOC( lstrlen( p ) + lstrlen( e ) + 2 )) == NULL ) {
						WError = ZEN_MEM21;
						break;
					}
					if ( !strcmp( p, "." ) ) r = procname( e, false, pG );
					else r = procname( lstrcat( AddSlash( lstrcpy( n, p ) ), e ), false, pG );
					FREE( n );
					if ( (int)(char)(r & 0xFF) > (int)(char)(WError & 0xFF) ) WError = r;
					if ( (int)(char)(r & 0xFF) > ZEN_OK ) break;
				}
			} /* end "if (strcmp..." */
		} /* end while */
		Closedir( d );
	} else diag( "can't open dir", pG );
	FREE( a );
	// sprintf( ewemsg, "Wild returned: %d", WError );
	// diag( ewemsg );
	return WError;
}

/* ===========================================================================
 * Process a name or wildcard expression to operate on (or exclude).
 * We will only arrive here if we do a Freshen or Delete.
 * Return an error code in the ZEN_ class.
 * ZEN_OK, ZEN_ABORT, ZEN_MISS03, ZEN_MISS04, ZEN_MISS05, ZEN_MEM22, ZEN_MEM23
	*ArgName :: Name to process.
 */
int procname( char *ArgName, bool RecurseDir, struct Globals *pG ) {
	char         *a;       /* path and name for recursion     */
	zDIR         *d;       /* directory stream from opendir() */
	char         *e;       /* pointer to name from readd()    */
	int           m;       /* matched flag                    */
	char         *p;       /* path for recursion              */
	int           pnError; /* ProcName error                  */
	struct stat   StatRes; /* result of stat()                */
	struct zlist *z;       /* steps through zfiles list       */

	// sprintf( ewemsg, "in procname  name=>%s<=  recurse=%d", ArgName, RecurseDir );
	// diag( ewemsg );

	m = 1;  /* set dflt for success indicator (0=success) */

	if ( pG->global_abort_sw )  return ZEN_ABORT;		/* RCV changed was ZEN_MISS? */
	if ( *ArgName == '\0' ) return ZEN_MISS03;

	/* LSSTAT returns 0 if it's arg is any kind of file (even a dir). */
	/* IsShExp returns true if a wildcard symbol is found, or
	 * NULL if none were found -- IsShExp is in util.c
	 */

	if ( LSSTAT( GetFullPath( pG, ArgName ), &StatRes ) || (IsShExp( ArgName ) != NULL) ) {
		// diag( "not a file or dir - 'ArgName' must be a wildcard fspec" );

		// Upon finding a wildcard fspec, we need to mark entries in
		// the "zfiles" list that are included by the wildcard.

		/* convert the "external" (native) filename to an internal
		 * ZIP-archive-compatible filename.
		 */
		p = ex2in( ArgName, (int *)NULL, pG );   /* shouldn't affect matching chars */

		/* does any file already in the archive match this spec? */
		/* Note that we need the pathname and filename together for this */
		for ( z = pG->zfiles; z; z = z->nxt ) {
			if ( dosmatch( p, z->zname, pG ) ) {
				/* name is in archive - mark it for updating */
				z->mark = pG->pcount ? filter( z->zname, pG ) : 1;
				FREE( z->name );	// RCV added + next 8 lines needed for FRESHEN mode.
				if ( (z->name = MALLOC( lstrlen( z->zname ) + 4 )) == NULL ) return ZEN_MEM34;
				z->name[0] = '\0';	//v1.55
				if ( isalpha( ArgName[0] ) && ArgName[1] == ':' && ArgName[2] == '\\') {
					z->name[0] = ArgName[0];
					z->name[1] = ArgName[1];
					z->name[2] = ArgName[2];
					z->name[3] = '\0';
				}
				lstrcat( z->name, z->zname );
				if ( pG->verbose ) printf( "%scluding %s\n", z->mark ? "in" : "ex", z->name );
				m = 0; /* we had at least one file in the archive that we marked */
			}
		}
		FREE( p );

		/* returns 1 if no "zfiles" entries were marked,
		 * 0 if at least one entry was marked
		 */
		if ( m ) {
			// diag( "returning ZEN_MISS04 from procname" );
			return ZEN_MISS04;
		}
		// diag( "returning ZEN_OK from procname" );
		return ZEN_OK;
	} /* end of "if (LSSTAT..." */

	/* Existing and good filename or directory-- add the name if it's a file, recurse if directory */
	// diag( "good entry to add, or a dir to go into" );

	/* check the status returned by LSSTAT earlier to see if 'ArgName' is a dir */
	pnError = ZEN_OK;
	if ( !(StatRes.st_mode & S_IFDIR) ) {
		/* it's not a directory - add file to found list */
		sprintf( pG->ewemsg, "spot 1: Add file %s to found list", ArgName );
		diag( pG->ewemsg, pG );
		/* newname (fileio.c) adds to found list. If error m!=0 on return */
		if ( (m = newname( ArgName, StatRes.st_size, pG )) != ZEN_OK ) {
			sprintf( pG->ewemsg, "returning %d from procname after newname call", m );
			diag( pG->ewemsg, pG );
			return m;
		}
	} else {
		/* It is a directory - Add trailing / to the directory name */
		// diag( "Spot 2, directory found" );
		if ( (p = MALLOC( lstrlen( ArgName )+ 2) ) == NULL ) return ZEN_MEM22;
		if ( !strcmp( ArgName, "." ) || !strcmp( ArgName, "\\." ) ) {		// SLASH
			*p = '\0';  /* avoid "./" prefix and do not create zip entry */
		} else {
			// diag( "spot 3" );
			lstrcpy( p, ArgName );
			a = p + lstrlen( p );
			if ( a[-1] != '\\' ) lstrcpy( a, "\\" );		// SLASH
			/* newname (fileio.c) adds to found list. If error m != 0 on return */
			if ( pG->dirnames && (m = newname( p, 0, pG )) != ZEN_OK ) {
				FREE( p );
				sprintf( pG->ewemsg, "returning %d from procname after 2nd newname call", m );
				diag( pG->ewemsg, pG );
				return m;
			}
		}

		/* recurse into directory */
		// diag( "spot 4: optional recurse into dir" );
		if ( RecurseDir && ((d = Opendir( ArgName, pG )) != NULL) ) {     /* Open new dir (like chdir) */
			// diag( "good open of dir" );
			while ( (e = readd( d, pG )) != NULL ) {
				if ( pG->global_abort_sw ) {	// RCV changed error handling
					pnError = ZEN_ABORT;
					break;
				}
				/* e is pointing to the new filename we just read from dir */
				/* ignore dir entries of . and .. */
				if ( strcmp( e, "." ) && strcmp( e, ".." ) ) {
					/* get a new tmp buffer for the path and fname */
					if ( (a = MALLOC( lstrlen( p ) + lstrlen( e ) + 1 )) == NULL ) {
						pnError = ZEN_MEM23;		/* RCV error handling changed */
						break;
					}
					/* form the new dir's pathname followed by the fname just read */
					/* (we need to send in the dir and fname, or it won't be
					 * detected as a valid file by LSSTAT)
					 */
					lstrcat( lstrcpy( a, p ), e );

					// diag( "DOING RECURSIVE CALL TO PROCNAME FROM PROCNAME" );
					if ( (m = procname( a, RecurseDir, pG )) != ZEN_OK ) {
						/* recursive call failed; return code not ZEN_OK */
						if ( m != ZEN_OK && m != ZEN_MISS05 ) {	/* unknown error; RCV error handling changed */
							pnError = m;
							FREE( a );
							break;
						} else if ( (int)(char)(m & 0xFF) == ZEN_MISS ) zipwarn( "name not matched: ", a );
					}
					FREE( a );
				}
			} /* end while */
			Closedir( d );
		} /* end if (spot 4) */
		FREE( p );
	} /* (StatRes.st_mode & S_IFDIR) == 0) */
	// diag( "returning ZEN_ class from procname" );
	return pnError;
}

/* ===========================================================================
 * Convert the external file name to an "internal" file name that
 * is valid to store in the ZIP archive, returning the malloc'ed
 * string, or NULL if not enough memory.
 * I.e. Strip the drive if present, strip the path if we don't want one
 * and change the name to 8.3 if needed.
 * Not inplemented, but also 'put in' change the short path to a long path.
	*x        :: External file name.
	*pdosflag :: Output: force MSDOS file attributes?
 */
char *ex2in( char *x, int *pdosflag, struct Globals *pG ) {
	char *n;              /* internal file name (malloc'ed)	*/
	char *t;              /* shortened name						*/
	char *x2;				 /* temporary x							*/
	int   dosflag;

	// sprintf( ewemsg,"in ex2in.  x=%s", x );
	// diag( ewemsg );

	dosflag = pG->dosify || IsFileSystemOldFAT( x, pG );
	if ( !pG->dosify && pG->use_longname_ea && (t = GetLongPathEA() ) != NULL ) {
		sprintf( pG->ewemsg, "replacing short name: %s with long name: %s", x, t );
		diag( pG->ewemsg, pG );
		x = t;
		dosflag = 0;
	}

	if ( (x2 = MALLOC( lstrlen( x ) + 1 )) == NULL ) return NULL;
	lstrcpy( x2, x );
	if ( pG->dosify )
		if ( (x2 = DOSName( x2, pG )) == NULL ) return NULL;	// We need a fully qualified path else this won't work. v1.55

	/* Find starting point in name before doing malloc */
	t = *x2 && isalpha((uch)*x2) && *(x2 + 1) == ':' ? x2 + 2 : x2;

	/* Strip "//host/share/" part of a UNC name v1.6017 */
	if ( !strncmp( x2, "\\\\", 2 ) && x2[2] != '\0' && x2[2] != '\\' ) {
		n = x2 + 2;
		while( *n != '\0' && *n != '\\' ) INCSTR( n );     /* strip host name    */
		if ( *n != '\0' ) {
			INCSTR( n );
			while( *n != '\0' && *n != '\\' ) INCSTR( n );  /* strip `share' name */
		}
		if (*n != '\0') t = n + CLEN( n );
	}

	/* Strip leading "\" to convert an absolute path into a relative path */
	while( *t == '\\' ) t++;

	/* Strip leading "./" as well as drive letter v1.6017*/
	while( *t == '.' && t[1] == '\\' ) t += 2;

	/* This is where the dirname gets stripped if user doesn't want it  */
	if ( !pG->pathput ) t = last( t, PATH_END );

	/* Malloc space for internal name and copy it */
	if ( (n = MALLOC( lstrlen( t ) + 1 )) == NULL ) {
		FREE( x2 );
		return NULL;
	}
	lstrcpy( n, t );

	/* set this return flag as needed */
	if ( pdosflag ) *pdosflag = dosflag;

	// sprintf( ewemsg, "ex2in returning: %s", n );
	// diag( ewemsg );

	FREE( x2 );

	/* Give the user a chance to change the internal name */
	user_callback( 7, 0, 0, n, pG );
	if ( pG->callbackdata.error_code ) {
		FREE( n );
		if ( (n = MALLOC( lstrlen( pG->callbackdata.filenameormsg ) + 1 )) == NULL )
			return NULL;
		lstrcpy( n, pG->callbackdata.filenameormsg );
	}
	/* Return the malloc'ed name */
	return n;
}

/* ===========================================================================
 * Convert the "internal" file name (inside the archive) to an
 * external file name, returning the malloc'ed string,
 * or NULL if not enough memory.
	n :: internal file name.
*/
char *in2ex( char *n ) {
	char *x;       /* external file name */

	// sprintf( ewemsg, "in in2ex. n=%s", n );
	// diag( ewemsg );

	if ( (x = MALLOC( lstrlen( n ) + 5 + PAD) ) == NULL ) return NULL;
	lstrcpy( x, n );

	//if ( !IsFileNameValid( x ) ) ChangeNameForFAT( x );	// RCV this test does not work.

	// sprintf( ewemsg, "in2ex returning: %s", x );
	// diag( ewemsg );

	return x;  /* return malloc'ed name */
}

/* ===========================================================================
 * Set last updated and accessed time of file f to the DOS time d.
 * This is called by dllzip.c to set date/time of the zipfile.
 * RCV Removed __TURBOC__ version.
	*f :: name of file to change.
    d :: dos-style time to change it to.
*/
void stamp( char *f, ulg d ) {
	int h;  /* file handle */

	if ( (h = open( f, O_RDWR | O_BINARY )) != -1 ) {	//v1.55 mode changed, was 0
		setftime( h, (struct ftime *)&d );
		close( h );
	}
}

/* ===========================================================================
 * If file *f does not exist, return 0.  Else, return the file's last
 * modified date and time as an MSDOS date and time.  The date and
 * time is returned in a long with the date most significant to allow
 * unsigned integer comparison of absolute times.  Also, if a is not
 * a NULL pointer, store the file attributes there, with the high two
 * bytes being the Unix attributes, and the low byte being a mapping
 * of that to DOS attributes.  If n is not NULL, store the file size
 * there.  If t is not NULL, the file's access and modification time
 * are stored there as UNIX time_t values.
 * If f is "-", use standard input as the file. If f is a device, return
 * a file size of -1
	*f :: Name of file to get info on.
	*a :: Return value: file attributes.
	*n :: Return value: file size.
	*t :: Return value: access and modification time.
 */
ulg filetime( char *f, ulg *a, long *n, ztimbuf *t, struct Globals *pG ) {
	struct stat s;        /* results of stat() */
	char   name[FNMAX];
	int    len = lstrlen( f ), isstdin = !strcmp( f, "-" );

	if ( f == pG->label ) {
		if ( a != NULL ) *a = pG->label_mode;
		if ( n != NULL ) *n = -2L;        /* convention for a label name */
		if ( t != NULL )  t->actime = t->modtime = pG->label_utim;
		return pG->label_time;
	}
	lstrcpy( name, f );
	/* not all systems allow stat'ing a file with / appended, so remove it */
	if ( name[len - 1] == '\\' ) name[len - 1] = '\0';		// SLASH
	/* Accept about any kind of file including directories */
	if ( LSSTAT( GetFullPath( pG, name ), &s ) != 0 ) return 0; // error in stat!

	if ( a != NULL ) {
		*a = ( (ulg)s.st_mode << 16) | (isstdin ? 0L : (ulg)GetFileMode( pG, name ) );
	}
	if ( n != NULL ) *n = (s.st_mode & S_IFMT) == S_IFREG ? s.st_size : -1L;
	if ( t != NULL ) {
		t->actime  = s.st_atime;
		t->modtime = s.st_mtime;
	}
	return unix2dostime( (time_t *)&s.st_mtime );
}

/* ===========================================================================
 * create extra field and change z->att if desired.
 */
/*int set_extra_field( struct zlist *z, ztimbuf *z_utim ) {
#ifdef USE_EF_UX_TIME
	if ( (z->extra = (char *)MALLOC( EB_HEADSIZE + EB_UX_MINLEN )) == NULL ) return ZEN_MEM24;

	z->extra[0]  = 'U';
	z->extra[1]  = 'X';
	z->extra[2]  = EB_UX_MINLEN;          // length of data part of e.f.
	z->extra[3]  = 0;
	z->extra[4]  = (char)(z_utim->actime);
	z->extra[5]  = (char)(z_utim->actime  >>  8);
	z->extra[6]  = (char)(z_utim->actime  >> 16);
	z->extra[7]  = (char)(z_utim->actime  >> 24);
	z->extra[8]  = (char)(z_utim->modtime);
	z->extra[9]  = (char)(z_utim->modtime >>  8);
	z->extra[10] = (char)(z_utim->modtime >> 16);
	z->extra[11] = (char)(z_utim->modtime >> 24);

	z->cext = z->ext = (EB_HEADSIZE+EB_UX_MINLEN);
	z->cextra = z->extra;

	return ZEN_OK;
#else
	return (int)(z - z);
#endif
}
*/

