/* FileIO.c
 * Copyright (C) 1990-1996 Mark Ad\ler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 */

#include "zip.h"
#include "Globals.h"
#include <time.h>
#include <errno.h>

#define PAD 0

/* Local functions */
static int fqcmp( const void *, const void * );
static int fqcmpz( const void *, const void * );

/* ===========================================================================
 * Read a space, \n, \r, or \t delimited name from stdin into n, and return
 * n.  If EOF, then return NULL.  Also, if the name read is too big, return
 * NULL.
	*n :: Where to put name (must have >=FNMAX+1 bytes).
 */
//char *getnam( char *n ) {
//	int   c;              /* last character read */
//	char *p;              /* pointer into name area */
//	char  quote = 0;      /* set for quoted names (containing spaces) */

//	p = n;
//	while ( (c = getchar()) == ' ' || c == '\n' || c == '\r' || c == '\t' );
//	if ( c == EOF ) return NULL;
//	if ( c == '\'' || c == '"' ) {
//		quote = (char)c;
//		c = getchar();
//	}
//	do {
//		if ( p - n >= FNMAX ) return NULL;
//		*p++ = (char)c;
//		c = getchar();
//	} while ( c != EOF && (quote ? c != quote : (c != ' ' && c != '\n' && c != '\r' && c != '\t')) );
//	*p = 0;
//	return n;
//}

/* ===========================================================================
 * Delete the entry *f in the doubly-linked found list.  Return pointer to
 * next entry to allow stepping through list.
	*f :: Entry to delete.
 */
struct flist *fexpel( struct flist *f, struct Globals *pG ) {
	struct flist *t;  /* temporary variable */

	t = f->nxt;
	*(f->lst) = t;										/* point last to next,    */
	if ( t != NULL ) t->lst = f->lst;			/* and next to last       */
	if ( f->name != NULL ) FREE( f->name );	/* free memory used       */
	if ( f->zname != NULL ) FREE( f->zname );
	FREE( f );
	pG->fcount--;										/* decrement count        */
	return t;											/* return pointer to next */
}

/* ===========================================================================
 * Used by qsort() to compare entries in the found list by name.
	*a, *b :: Pointers to pointers to found entries.
 */
static int fqcmp( const void *a, const void *b ) {
	return strcmp( (*(struct flist **)a)->name, (*(struct flist **)b)->name );
}

/* ===========================================================================
 * Used by qsort() to compare entries in the found list by zname.
	*a, *b :: Pointers to pointers to found entries.
 */
static int fqcmpz( const void *a, const void *b ) {
	return strcmp( (*(struct flist **)a)->zname, (*(struct flist **)b)->zname );
}

/* ===========================================================================
 * Return a pointer to the start of the last path component. For a directory
 * name terminated by the character in c, the return value is an empty string.
	*p :: Sequence of path components.
	 c :: Path components separator character.
 */
char *last( char *p, int c ) {
	char *t;              /* temporary variable */

	if ( (t = strrchr( p, c )) != NULL ) return (t + 1);
	return p;
}

/* ===========================================================================
 * Reduce all path components to MSDOS upper case 8.3 style names.
 */
char *DOSName( char *n, struct Globals *pG ) {
	char *p, c[MAX_PATH];
	int L1, L2;

	//sprintf( pG->ewemsg, "In DOSName1 =%s", n );
	//diag( pG->ewemsg, pG );
   if ( pG->WantedCodePage != 2 ) {
		c[0] = '\0';
		// Separate the path from the wildcard.
		if ( IsShExp( n ) != NULL && ( (p = strrchr( n, '\\' )) != NULL || (p = strrchr( n, ':' )) != NULL ) ) {
			if ( *p == ':' ) p++;
			lstrcpy( c, p );
			*p = '\0';
		}
		L2 = lstrlen( n );
		L1 = GetShortPathName( n, n, L2 );	//v1.55
		if ( L1 > L2 ) {
			char *n1 = MALLOC( L1 + 1 );
			if ( !n1 ) {
				FREE( n );
				return NULL;
			}
			GetShortPathName( n, n1, L1 + 1 );
			FREE( n );
			n = n1;
		}
		lstrcat( n, c );
		//sprintf( pG->ewemsg, "In DOSName2 =%s %d %d", n, L1, L2 );
		//diag( pG->ewemsg, pG );

		// Make it all uppercase
		for ( p = n; *p; ) *p = to_up( *p++ );
	}
	// We now the input is ISO so use the present OEM code page for this name.
	CharToOemBuff( n, n, lstrlen( n ) );
//	int   c;                /* current character               */
//	int   f;                /* characters in current component */
//	char *srcptr, *destptr;

//	srcptr = destptr = n;
//	f = 0;
//	while ( (c = *srcptr++) != 0 )
//		if ( c == ' ' || c == ':' ) /* or anything else illegal */
//			continue;                /* char is discarded        */
//		else
//			if ( c == '/' ) {
//				*destptr++ = (char)c;
//				f = 0;     		                       /* new component                */
//			} else
//				if ( c == '.' )
//					if ( f < 9 ) {
//						*destptr++ = (char)c;
//						f = 9;                          /* now in file type             */
//					} else f = 12;                     /* now just excess characters   */
//				else
//					if ( f < 12 && f != 8 ) {
//						*destptr++ = (char)(to_up( c ));
//						f++;                            /* do until end of name or type */
//					}
//	*destptr = 0;
	return n;
}

/* ===========================================================================
 * Sort the found list and remove duplicates.
 * Return ZEN_OK, ZEN_PARMS(warning) or ZEN_MEM(error).
 */
int check_dup( struct Globals *pG ) {
	struct flist *f;          /* steps through found linked list */
	extent j, k;              /* indices for s */
	struct flist **s;         /* sorted table  */
	struct flist **nodup;     /* sorted table without duplicates in the external name */

	/* sort found list, remove duplicates */
	if ( pG->fcount ) {
		if ( (s = (struct flist **)MALLOC( pG->fcount * sizeof( struct flist * ) )) == NULL )
			return ZEN_MEM13;
		/* Make a copy of the tabel to work with */
		for ( j = 0, f = pG->found; f != NULL; f = f->nxt ) s[j++] = f;
		qsort( (char *)s, pG->fcount, sizeof( struct flist * ), fqcmp );	/* Sort it */
		/* Remove the duplicate external names */
		for ( k = j = pG->fcount - 1; j > 0; j-- )
			if ( !strcmp( s[j - 1]->name, s[j]->name ) )
				/* remove duplicate entry from list */
				fexpel( s[j], pG );   /* fexpel() changes fcount */
			else
				/* copy valid entry into destination position    */
				s[k--] = s[j];
		s[k]  = s[0];               /* First entry is always valid */
		nodup = &s[k];              /* Valid entries are at end of array s */

		/* sort only valid items and check for unique internal names */
		qsort( (char *)nodup, pG->fcount, sizeof( struct flist * ), fqcmpz );
		for ( j = 1; j < pG->fcount; j++ )
			if ( !strcmp( nodup[j - 1]->zname, nodup[j]->zname ) ) {
				zipwarn( "name in zip file repeated: ", nodup[j]->zname );
				zipwarn( "  first full name: ", nodup[j - 1]->name );
				zipwarn( " second full name: ", nodup[j]->name );
				FREE( s );			// RCV Added.
				return ZEN_PARMS13;
			}
		FREE( s );
	}
	return ZEN_OK;
}

/* ===========================================================================
 * Scan the exclude list for a match to the given name.
 * Return true if the name must be included, false otherwise.
 * The exclude list is 'made' in zipmain in DllZip.c
 	*Name	:: File to match to
*/
bool filter( char *Name, struct Globals *pG ) {
	register int n;

	for ( n = 0; n < pG->pcount; n++ )
		if ( dosmatch( pG->patterns[n].zname, Name, pG ) )
			return false;

	return true;
}

/* ===========================================================================
 * Add (or exclude) the name of an existing disk file.  Return
 * an error code in the ZEN_ class. Return ZEN_OK if OK.
	*n     :: Name to add (or exclude).
	 nSize :: Size of the file or dir(0).
 */
int newname( char *n, long nSize, struct Globals *pG ) {
	char         *m;
	char         *undosm = NULL;
	struct flist *f;			/* where in found, or new found entry */
	struct zlist *z;			/* where in zfiles (if found)         */
	int           dosflag;	/* force 8x3?                         */
	int			  ErrMsg = ZEN_OK;
	bool			  mUsed = false;

	do {	// Just one loop, with this w'll get a better error handling. RCV: 1.605
		if ( (m = ex2in( n, &dosflag, pG )) == NULL ) {
			ErrMsg = ZEN_MEM14;
			break;
		}
		/* Discard directory names with zip -rj */
		if ( *m == '\0' ) {
			/* If extensions needs to be swapped, we will have empty directory names
			 * instead of the original directory. For example, zipping 'c.', 'c.main'
			 * should zip only 'main.c' while 'c.' will be converted to '\0' by ex2in.
			 */
			if ( pG->pathput ) ziperr( ZEN_LOGIC05, pG );
			break;
		}
		undosm = m;

		if ( dosflag || !pG->pathput ) {
			int save_dosify = pG->dosify, save_pathput = pG->pathput;

			pG->dosify  = 0;
			pG->pathput = 1;
			/* convert external name to internal name */
			if ( (undosm = ex2in( n, NULL, pG )) == NULL ) undosm = m;
			pG->dosify  = save_dosify;
			pG->pathput = save_pathput;
		}

		/* Search for name in zip file.  If there, mark it, else add to
		 * list of new names to do (or remove from that list). */
		if ( (z = zsearch( m, pG )) != NULL ) {
			if ( pG->pcount && !filter( undosm, pG ) ) {
				/* Do not clear z->mark if "exclude", because, when "dosify || !pathput"
				 * is in effect, two files with different filter options may hit the
				 * same z entry.
				 */
				sprintf( pG->ewemsg, "excluding %s", n );
				diag( pG->ewemsg, pG );
			} else {
				z->mark = 1;
				FREE( z->name );
				if ( (z->name = MALLOC( lstrlen( n ) + 1 + PAD )) == NULL ) {
					ErrMsg = ZEN_MEM15;
					break;
				}
				lstrcpy( z->name, n );
#ifdef FORCE_NEWNAME
				FREE( z->zname );
				z->zname = m;
				mUsed		= true;
#endif
				/* Better keep the old name. Useful when updating on MSDOS
				 * a zip file made on Unix.
				 */
				z->dosflag = dosflag;
				sprintf( pG->ewemsg, "including %s", n );
				diag( pG->ewemsg, pG );
			}
			if ( n == pG->label ) pG->label = z->name;
		} else if ( !pG->pcount || filter( undosm, pG ) ) {
			/* Check that we are not adding the zip file to itself. This
			 * catches cases like "zip -m foo ../dir/foo.zip".  SLASH
			 */
			struct stat statb;

			if ( pG->zipstate == -1 )
				pG->zipstate = strcmp( pG->zipfile, "-" ) != 0 && stat( pG->zipfile, &pG->zipstatb ) == 0;
			if ( pG->zipstate == 1 && (statb = pG->zipstatb, stat( GetFullPath( pG, n ), &statb ) == 0
					&& pG->zipstatb.st_mode  == statb.st_mode
					&& pG->zipstatb.st_ino   == statb.st_ino
					&& pG->zipstatb.st_dev   == statb.st_dev
					&& pG->zipstatb.st_uid   == statb.st_uid
					&& pG->zipstatb.st_gid   == statb.st_gid
					&& pG->zipstatb.st_size  == statb.st_size
					&& pG->zipstatb.st_mtime == statb.st_mtime
					&& pG->zipstatb.st_ctime == statb.st_ctime) ) {
				/* Don't compare a_time since we are reading the file */
				break;
			}
			/* allocate space and add to list */
			if ( (f = (struct flist *)MALLOC( sizeof( struct flist ) )) == NULL ||
						(f->name = MALLOC( lstrlen( n ) + 1 + PAD )) == NULL ) {
				if ( f != NULL ) FREE( f );
				ErrMsg = ZEN_MEM16;
				break;
			}
			lstrcpy( f->name, n );
			f->zname    = m;
			mUsed			= true;
			f->dosflag  = dosflag;
			f->len      = nSize;	// RCV added.
			*(pG->fnxt) = f;
			f->lst      = pG->fnxt;
			f->nxt      = NULL;
			pG->fnxt    = &f->nxt;
			pG->fcount++;
			if ( n == pG->label ) pG->label = f->name;
		}
		break;
	} while( true );
	if ( undosm && undosm != m ) FREE( undosm );
	if ( m && !mUsed ) FREE( m );
	return ErrMsg;
}

/* ===========================================================================
 * Return the Unix time_t value (GMT/UTC time) for the DOS format (local)
 * time dostime, where dostime is a four byte value (date in most significant
 * word, time in least significant word), see dostime() function.
	dostime :: DOS time to convert.
 */
time_t dos2unixtime( ulg dostime ) {
	struct tm *t;          /* argument for mktime() */
	const time_t clock = time( NULL );

	t = localtime( &clock );
	/* Convert DOS time to UNIX time_t format */
	t->tm_sec  = ( ( (int)dostime) <<  1) & 0x3E;
	t->tm_min  = ( ( (int)dostime) >>  5) & 0x3F;
	t->tm_hour = ( ( (int)dostime) >> 11) & 0x1F;
	t->tm_mday =   (int)(dostime >> 16) & 0x1F;
	t->tm_mon  = ( (int)(dostime >> 21) & 0x0F)-  1;
	t->tm_year = ( (int)(dostime >> 25) & 0x7F)+ 80;

	return mktime( t );
}

/* ===========================================================================
 * Convert the date y/n/d and time h:m:s to a four byte DOS date and
 * time (date in high two bytes, time in low two bytes allowing magnitude
 * comparison).
	y :: Year.
	n :: Month.
	d :: Day.
	h :: Hour.
	m :: Minute.
	s :: Second.
 */
ulg dostime( int y, int n, int d, int h, int m, int s ) {
	return y < 1980 ? dostime( 1980, 1, 1, 0, 0, 0 ) :
			(((ulg)y - 1980) << 25) | ((ulg)n << 21) | ((ulg)d << 16) |
			((ulg)h << 11) | ((ulg)m << 5) | ((ulg)s >> 1);
}

/* ===========================================================================
 * Return the Unix time t in DOS format, rounded up to the next two
 * second boundary.
	*t :: Unix time to convert.
 */
ulg unix2dostime( time_t *t ) {
	time_t t_even;
	struct tm *s;                 /* result of localtime()     */

	t_even = (*t + 1) & (~1);     /* Round up to even seconds. */
	s = localtime(&t_even);       /* Use local time since MSDOS does. */
	return dostime( s->tm_year + 1900, s->tm_mon + 1, s->tm_mday,
                    s->tm_hour, s->tm_min, s->tm_sec );
}

/* ===========================================================================
 * Return true if the attributes are those of a symbolic link
	a :: Attributes returned by filetime().
 */
int issymlnk( ulg a ) {
#ifdef S_IFLNK
	return ((a >> 16) & S_IFMT) == S_IFLNK;
#else
	return (int)a & 0;    /* Avoid warning on unused parameter. */
#endif
}

/* ===========================================================================
 * Delete the file *f, returning non-zero on failure.
	*f :: File to delete.
 */
int destroy( char *f ) {
	if ( f ) return unlink( f ); 	// v1.6011
	return( ENOENT );	// v1.6011
}

/* ===========================================================================
 * Replace file *d by file *s, removing the old *s.  Return an error code
 * in the ZEN_ class. This function need not preserve the file attributes,
 * this will be done by setfileattr() later.
	*d, *s :: Destination and source file names.
 */
int replace( char *d, char *s, struct Globals *pG ) {
	struct stat t;        /* results of stat() */
	int copy = 0;
	int d_exists;

	d_exists = (LSTAT( d, &t ) == 0);
	if ( d_exists ) {
		/*
		 * respect existing soft and hard links!
		 */
		if ( t.st_nlink > 1
# ifdef S_IFLNK
			|| (t.st_mode & S_IFMT) == S_IFLNK
# endif
			) copy = 1;
		else if ( unlink( d ) )
			return ZEN_CREAT01;                 /* Can't erase zip file--give up */
	}

	if ( !copy ) {
		if ( rename( s, d ) ) {             /* Just move s on top of d */
			copy = 1;                        /* failed ? */
			if ( errno != ENOTSAM ) return ZEN_CREAT02;
		}
	}

	if ( copy ) {
		FILE *f, *g;      /* source and destination files */
		int   r;          /* temporary variable */

		diag( "in replace - fopen for FOPR", pG );

		if ( (f = fopen( s, FOPR )) == NULL ) {
			diag( "in replace - bad open for FOPR", pG );
			printf( " replace: can't open %s\n", s );
			return ZEN_TEMP03;
		}

		diag( "in replace - fopen for FOPW", pG );
		if ( (g = fopen( d, FOPW )) == NULL ) {
			fclose( f );
			diag( "in replace - bad open for FOPW", pG );
			return ZEN_CREAT03;
		}

		r = fcopy( f, g, (ulg)-1L );
		fclose( f );
		if ( fclose( g ) || r != ZEN_OK ) {
			unlink( d );
			return r ? ( (int)(char)(r & 0xFF) == ZEN_TEMP ? ZEN_WRITE07 : r) : ZEN_WRITE04;
		}
		unlink( s );
	}
	return ZEN_OK;
}

/* ===========================================================================
 * Return the file attributes for file f or 0 if failure.
	*f :: File path.
 */
/*int getfileattr( char *f ) {
	struct stat s;

	return SSTAT( f, &s ) == 0 ? s.st_mode : 0;
}*/

/* ===========================================================================
 * Give the file f the attributes a, return non-zero on failure.
	f :: file path.
	a :: attributes returned by getfileattr().
*/
int setfileattr( char *f, int a ) {
	return chmod( f, a );
}

/* ===========================================================================
 * Return a temporary file name in its own malloc'ed space, using tempath.
*/
char *tempname( struct Globals *pG )	{
	char *t;  /* Pointer for malloc'ed space for name. */

	/* Copy the tempath, if present, to t. */
	if ( pG->tempath != NULL ) {
		if ( (t = MALLOC( lstrlen( pG->tempath )+ 14 )) == NULL ) return NULL;
		lstrcpy( t, pG->tempath );
		if ( t[lstrlen( t )- 1] != '\\') lstrcat( t, "\\" );		// SLASH
	} else {
		if ( (t = MALLOC( 14 )) == NULL ) return NULL;
		*t = 0;
	}
	lstrcat( t, "ZipTmpXXXXXX" ); /* Get a unique name. */
	if ( mktemp( t ) == NULL ) {
		FREE( t );
		return NULL;
	}
	sprintf( pG->ewemsg, "Temp filename: %s", t );
	diag( pG->ewemsg, pG );
	return t;
}

/* ===========================================================================
 * Copy n bytes from file *f to file *g, or until EOF if n == -1.
 * Return an error code in the ZEN_ class.
	*b :: malloc'ed buffer for copying.
	 k :: Result of fread().
	 m :: Bytes copied so far.
 */
int fcopy( FILE *f, FILE *g, ulg n ) {
	char   *b;          /* malloc'ed buffer for copying */
	extent  k;          /* result of fread()   */
	ulg     m;          /* bytes copied so far */

	if ( (b = MALLOC( CBSZ )) == NULL ) return ZEN_MEM17;
	m = 0;
	while ( n == (ulg)(-1L) || m < n ) {
		if ( (k = fread( b, 1, n == (ulg)(-1) ?
				CBSZ : (n - m < CBSZ ? (extent)(n - m) : CBSZ), f) ) == 0 )
			if ( ferror( f ) ) {
				FREE( b );
				return ZEN_READ02;
		}
		else break;
		if ( fwrite( b, 1, k, g ) != k ) {
			FREE( b );
			printf( " fcopy: write error\n" );
			return ZEN_TEMP04;
		}
		m += k;
	}
	FREE( b );
	return ZEN_OK;
}

char *GetFullPath( struct Globals *pG, char *Filename ) {
	*(pG->ewetmp) = 0;
	if ( !( (isalpha( Filename[0] ) && Filename[1] == ':') || (Filename[0] == '\\' && Filename[1] == '\\')) )
		lstrcpy( pG->ewetmp, pG->OrigCurrentDir );
	return lstrcat( pG->ewetmp, Filename );
}

