/* ZipFile.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
*/

#include "zip.h"
#include "Globals.h"
#include "dir.h"

/* Macros for converting integers in little-endian to machine format */
/* RCV Aded (ush)( ... ) to remove conversion warnings */
#define SH( a ) (ush)( ((ush)(uch)(a)[0]) | ( ((ush)(uch)(a)[1]) << 8) )
#define LG( a ) ( (ulg)SH(a) | ( (ulg)SH( (a)+ 2 ) << 16) )

/* Macros for writing machine integers to little-endian format */
#define PUTSH( a, f ) {putc( (char)((a) & 0xFF),(f) ); putc( (char)((a) >> 8),(f) );}
#define PUTLG( a, f ) {PUTSH( (a) & 0xFFFF,(f) ) PUTSH( (a) >> 16,(f) )}

/* -- Structure of a ZIP file -- */

/* Signatures for zip file information headers */
#define LOCSIG     0x04034b50L
#define CENSIG     0x02014b50L
#define ENDSIG     0x06054b50L
#define EXTLOCSIG  0x08074b50L

/* Offsets of values in headers */
#define LOCVER  0               /* version needed to extract				*/
#define LOCFLG  2               /* encrypt, deflate flags					*/
#define LOCHOW  4               /* compression method							*/
#define LOCTIM  6               /* last modified file time, DOS format	*/
#define LOCDAT  8               /* last modified file date, DOS format	*/
#define LOCCRC  10              /* uncompressed crc-32 for file			*/
#define LOCSIZ  14              /* compressed size in zip file				*/
#define LOCLEN  18              /* uncompressed size							*/
#define LOCNAM  22              /* length of filename							*/
#define LOCEXT  24              /* length of extra field						*/

#define EXTCRC  0               /* uncompressed crc-32 for file			*/
#define EXTSIZ  4               /* compressed size in zip file				*/
#define EXTLEN  8               /* uncompressed size							*/

#define CENVEM  0               /* version made by								*/
#define CENVER  2               /* version needed to extract				*/
#define CENFLG  4               /* encrypt, deflate flags					*/
#define CENHOW  6               /* compression method							*/
#define CENTIM  8               /* last modified file time, DOS format	*/
#define CENDAT  10              /* last modified file date, DOS format	*/
#define CENCRC  12              /* uncompressed crc-32 for file			*/
#define CENSIZ  16              /* compressed size in zip file				*/
#define CENLEN  20              /* uncompressed size							*/
#define CENNAM  24              /* length of filename							*/
#define CENEXT  26              /* length of extra field						*/
#define CENCOM  28              /* file comment length						*/
#define CENDSK  30              /* disk number start							*/
#define CENATT  32              /* internal file attributes					*/
#define CENATX  34              /* external file attributes					*/
#define CENOFF  38              /* relative offset of local header		*/

#define ENDDSK  0               /* number of this disk						*/
#define ENDBEG  2               /* number of the starting disk				*/
#define ENDSUB  4               /* entries on this disk						*/
#define ENDTOT  6               /* total number of entries					*/
#define ENDSIZ  8               /* size of entire central directory		*/
#define ENDOFF  12              /* offset of central on starting disk	*/
#define ENDCOM  16              /* length of zip file comment				*/

/* Local functions */

static int zqcmp( const void *, const void * );
static int rqcmp( const void *, const void * );
static int zbcmp( const void *, const void * );

#ifdef USE_EF_UX_TIME
 static int ef_scan_ux_time( char *ef_buf, extent ef_len, ztimbuf *z_utim );
#endif

static void cutpath( char *p );

/* ===========================================================================
 * Used by qsort() to compare entries in the zfile list.
 * Compares the internal names z->zname
	*a, *b :: Pointers to pointers to zip entries.
*/
static int zqcmp( const void *a, const void *b ) {
	return namecmp( (*(struct zlist **)a)->zname, (*(struct zlist **)b)->zname );
}

/* ===========================================================================
 * Used by qsort() to compare entries in the zfile list.
 * Compare the internal names z->zname, but in reverse order.
	*a, *b :: Pointers to pointers to zip entries.
*/
static int rqcmp( const void *a, const void *b ) {
	return namecmp( (*(struct zlist **)b)->zname, (*(struct zlist **)a)->zname );
}

/* ===========================================================================
 * Used by search() to compare a target to an entry in the zfile list.
	*n :: String to search for.
	*z :: Pointer to a pointer to a zip entry.
 */
static int zbcmp( const void *n, const void *z ) {
	return namecmp( (char *)n, ( (struct zlist *)z )->zname );
}

/* ===========================================================================
 * Return a pointer to the entry in zfile with the name n, or NULL if
 * not found.
	*n :: Name to find.
 */
struct zlist *zsearch( char *n, struct Globals *pG ) {
	void **p;        /* result of search() */

	if ( pG->zcount && (p = search( n, (void **)pG->zsort, pG->zcount, zbcmp )) != NULL )
		return *(struct zlist **)p;
	return NULL;
}

#define PATHCUT '\\'	// SLASH

/* ===========================================================================
 * If the file name *s has a dot (other than the first char), or if
 * the -A option is used (adjust self-extracting file) then return
 * the name, otherwise append .zip to the name.  Allocate the space for
 * the name in either case.  Return a pointer to the new name, or NULL
 * if malloc() fails.
	*s :: File name to force to zip.
 */
char *ziptyp( char *s, struct Globals *pG ) {
	char *q;              /* temporary pointer */
	char *t;              /* pointer to malloc'ed string */

	if ( (t = MALLOC( lstrlen( s ) + 5 )) == NULL ) return NULL;
	lstrcpy( t, s );

	if ( pG->adjust ) return t;
	if ( strrchr( (q = strrchr( t, PATHCUT ) ) == NULL ? t : q + 1, '.') == NULL )
		lstrcat( t, ".zip" );
	return t;
}

/* RCV added; readzipfile didn't close the Zip archive in case of an error *
 * this would prevent deletion of such a file until the application ended. */
int AfterError2( FILE *f, int Res ) {
	fclose( f );
	return Res;
}
int AfterError1( FILE *f ) {
	int Res = ferror( f ) ? ZEN_READ04 : ZEN_EOF02;
	return AfterError2( f, Res );
}

/* ===========================================================================
 * Make first pass through zip file, reading information from local file
 * headers and then verifying that information with the central file
 * headers.  Any deviation from the expected zip file format returns an
 * error.  At the end, a sorted list of file names in the zip file is made
 * to facilitate searching by name.
 *
 * The name of the zip file is pointed to by the global "zipfile".  The
 * globals zfiles, zcount, zcomlen, zcomment, and zsort are filled in.
 * Return an error code in the ZEN_xxx class.
*/
int readzipfile( struct Globals *pG ) {
	ulg    a = 0L;        /* attributes returned by filetime()			*/
	char   b[CENHEAD];    /* buffer for central headers					*/
	FILE  *f;             /* zip file											*/
	ush    flg;           /* general purpose bit flag						*/
	int    m;             /* mismatch flag										*/
	extent n;             /* length of name									*/
	ulg    p;             /* current file offset								*/
	char   r;             /* holds reserved bits during memcmp()		*/
	ulg    s;             /* size of data, start of central				*/
	char  *t;             /* temporary variable								*/
	char  *u;             /* temporary variable								*/
	char  *ptr;				 /* temporary variable								*/
	struct zlist **x;     /* pointer last entry's link						*/
	struct zlist  *z;     /* current zip entry structure					*/
	ulg    temp;          /* required to avoid Coherent compiler bug	*/
	int    zcomlentmp;    /* tmp var v1.60 */
	char  *zcommenttmp = NULL;   /* tmp var v1.60 */

	/* Initialize zip file info */
	pG->zipbeg  = 0;
	pG->zfiles  = NULL;       /* Points to first header  */
//	pG->zcomlen = 0;          /* zip file comment length RCV removed v1.60 */

	/* If zip file exists, read headers and check structure */
	if ( pG->zipfile != NULL ) {
		sprintf( pG->ewemsg, "ready to open: %s for read only", pG->zipfile );
		diag( pG->ewemsg, pG );
	}

	if ( pG->zipfile != NULL && *pG->zipfile && strcmp( pG->zipfile, "-" ) &&
        (f = fopen( pG->zipfile, FOPR )) != NULL ) {
		diag( "zip archive is now open", pG );

		/* Get any file attribute valid for this OS, to set in the central
		 * directory when fixing the archive:
		 */
		if ( pG->fix ) filetime( pG->zipfile, &a, (long*)&s, NULL, pG );
		x = &pG->zfiles;                    /* first link */
		p = 0;                              /* starting file offset */
		pG->zcount = 0;                     /* number of files */

		/* Find start of zip structures */
		for (;;) {
			while ( (m = getc( f )) != EOF && m != 0x50) p++;  /* 0x50 == 'P' */
			b[0] = (char)m;
			if ( fread( b + 1, 3, 1, f ) != 1 || (s = LG( b )) == LOCSIG || s == ENDSIG )
				break;
			if ( fseek( f, -3L, SEEK_CUR ) ) return AfterError1( f );
			p++;
		}
		pG->zipbeg = p;

		/* Read local headers */
		while ( LG( b ) == LOCSIG ) {
			/* Read local header raw to compare later with central header
			 * (this requires that the offest of ext in the zlist structure
			 * be greater than or equal to LOCHEAD)
			 */
			if ( (z = (struct zlist *)MALLOC( sizeof( struct zlist ) )) == NULL ) return AfterError2( f, ZEN_MEM25 );
			if ( fread( b, LOCHEAD, 1, f ) != 1 ) {
				if ( pG->fix ) break;
				else return AfterError1( f );
			}
			if ( pG->fix ) {
				z->ver = SH( LOCVER + b );
				/* RCV Added below (ush)( ... ) */
				z->vem = (ush)(pG->dosify ? 20 : OS_CODE + 22); // 22 = REVISION
				z->dosflag = pG->dosify;
				flg = z->flg = z->lflg = SH( LOCFLG + b );
				z->how = SH( LOCHOW + b );
				z->tim = LG( LOCTIM + b );          /* time and date into one long */
				z->crc = LG( LOCCRC + b );
				z->siz = LG( LOCSIZ + b );
				z->len = LG( LOCLEN + b );
				n = z->nam = SH( LOCNAM + b );
				z->cext = z->ext = SH( LOCEXT + b );
				z->com = 0;
				z->dsk = 0;
				z->att = 0;
				z->atx = pG->dosify ? a & 0xFF : a;     /* Attributes from filetime() */
				z->mark = 0;
				z->trash = 0;
				s = pG->fix > 1 ? 0L : z->siz; /* discard compressed size with -FF */
			} else {
				t = b;  u = (char *)z;  n = LOCHEAD;
				do {
					*u++ = *t++;
				} while ( --n );
				z->ext = SH( LOCEXT + (uch *)z );
				n      = SH( LOCNAM + (uch *)z );
				flg    = SH( b + LOCFLG );
				s      = LG( LOCSIZ + (uch *)z );
			}

			/* Link into list */
			*x = z;
			z->nxt = NULL;
			x = &z->nxt;

			/* Read file name and extra field and skip data */
			if ( n == 0 ) {
				sprintf( pG->errbuf, "%ld", (ulg)pG->zcount + 1 );
				zipwarn( "zero-length name for entry #", pG->errbuf );
#				ifndef DEBUG
				return AfterError2( f, ZEN_FORM01 );
#				endif
			}
			if ( (z->zname = MALLOC( n + 1 )) ==  NULL ||
				  (z->ext && (z->extra = MALLOC( z->ext )) == NULL) ) return AfterError2( f, ZEN_MEM26 );
			if ( fread( z->zname, n, 1, f ) != 1 ||
				  (z->ext && fread( z->extra, z->ext, 1, f ) != 1) ||
   	       (s && fseek( f, (long)s, SEEK_CUR )) ) return AfterError1( f );

			/* If there is an extended local header, s is either 0 or
			 * the correct compressed size.
			 */
			z->zname[n] = 0;                  /* terminate name */
			for ( ptr = z->zname; *ptr; ptr++ ) if ( *ptr == '/' ) *ptr = '\\';	// SLASH ADDED
			if ( pG->fix ) {
				z->cextra = z->extra;
				if ( pG->noisy ) printf( "zip: reading %s\n", z->zname );
			}
			z->name = in2ex( z->zname );      /* convert to external name */
			if ( z->name == NULL ) return AfterError2( f, ZEN_MEM27 );

			/* Save offset, update for next header */
			z->off = p;
			p += 4 + LOCHEAD + n + z->ext + s;
			pG->zcount++;

			/* Skip extended local header if there is one */
			if ( (flg & 8) != 0 ) {
				/* Skip the compressed data if compressed size is unknown.
				 * For safety, we should use the central directory.
				 */
				if ( s == 0 ) {
					for (;;) {
						while ( (m = getc( f )) != EOF && m != 0x50 );  /* 0x50 == 'P' */
						b[0] = (char)m;
						if ( fread( b + 1, 15, 1, f ) != 1 || LG( b ) == EXTLOCSIG ) break;
						if ( fseek( f, -15L, SEEK_CUR ) ) return AfterError1( f );
					}
					s = LG( 4 + EXTSIZ + b );
					p += s;
					if ( (ulg)ftell( f ) != p + 16L ) {
						zipwarn( "bad extended local header for ", z->zname );
						return AfterError2( f, ZEN_FORM02 );
					}
				} else {
					/* compressed size non zero, assume that it is valid: */
					Assert( p == (ulg)ftell( f ), "bad compressed size with extended header" );

					if ( fseek( f, p, SEEK_SET ) || fread( b, 16, 1, f ) != 1 )
						return AfterError1( f );
					if ( LG( b ) != EXTLOCSIG ) {
						zipwarn( "extended local header not found for ", z->zname );
						return AfterError2( f, ZEN_FORM03 );
					}
				}
				/* overwrite the unknown values of the local header: */
				if ( pG->fix ) {
					/* already in host format */
					z->crc = LG( 4 + EXTCRC + b );
					z->siz = s;
					z->len = LG( 4 + EXTLEN + b );
				} else {
					/* Keep in Intel format for comparison with central header */
					t = b + 4;  u = (char *)z + LOCCRC;  n = 12;
					do {
						*u++ = *t++;
					} while ( --n );
				}
				p += 16L;
			} else if ( pG->fix > 1 ) {
				/* Don't trust the compressed size */
				for (;;) {
					while ((m = getc( f )) != EOF && m != 0x50) p++; /* 0x50 == 'P' */
					b[0] = (char)m;
					if ( fread( b + 1, 3, 1, f ) != 1 || (s = LG( b )) == LOCSIG || s == CENSIG )
						break;
					if ( fseek( f, -3L, SEEK_CUR ) ) return AfterError1( f );
					p++;
				}
				s = p - (z->off + 4 + LOCHEAD + n + z->ext);
				if ( s != z->siz ) {
					printf( " compressed size %ld, actual size %ld for %s\n", z->siz, s, z->zname );
					z->siz = s;
				}
				/* next LOCSIG already read at this point, don't read it again: */
				continue;
			}

			/* Read next signature */
			if ( fread( b, 4, 1, f ) != 1 ) {
				if ( pG->fix )
					break;
				else
					return AfterError1( f );
			}
		}

		/* Point to start of header list and read central headers */
		z = pG->zfiles;
		s = p;                              /* save start of central */
		if ( pG->fix ) {
			if ( LG( b ) != CENSIG && pG->noisy ) {
				printf( "zip warning: %s %s truncated.\n", pG->zipfile, pG->fix > 1 ? "has been" : "would be" );
				if ( pG->fix == 1 ) {
					ziperr( ZEN_FORM04, pG );
				}
			}
		} else while ( LG( b ) == CENSIG ) {
			if ( z == NULL ) {
				zipwarn( "extraneous central header signature", "" );
				return AfterError2( f, ZEN_FORM05 );
			}

			/* Read central header */
			if ( fread( b, CENHEAD, 1, f ) != 1 ) return AfterError1( f );

			/* Compare local header with that part of central header (except
			 * for the reserved bits in the general purpose flags and except
			 * for length of extra fields--authentication can make these
			 * different in central and local headers)
			 */
			z->lflg = SH( LOCFLG + (uch *)z );      /* Save reserved bits */
			r = b[CENFLG + 1];
			( (uch *)z)[LOCFLG + 1] &= 0x1f; /* Zero out reserved bits */
			b[CENFLG + 1] &= 0x1f;
			for ( m = 0, u = (char *)z, n = 0; n < LOCHEAD - 2; n++ ) if ( u[n] != b[n + 2] ) {
				if ( !m && pG->noisy ) zipwarn( "local and central headers differ for ", z->zname );
				m = 1;
				sprintf( pG->errbuf, " offset %d--local = %02x, central = %02x", (int)n, (uch)u[n], (uch)b[n + 2] );
				if ( pG->noisy ) zipwarn( pG->errbuf, "" );
				b[n + 2] = u[n]; /* fix the zipfile */
			}
			if ( m ) return AfterError2( f, ZEN_FORM06 );
   	   b[CENFLG + 1] = r;                /* Restore reserved bits */

			/* Overwrite local header with translated central header */
			z->vem  = SH( CENVEM + b );
			z->ver  = SH( CENVER + b );
			z->flg  = SH( CENFLG + b );       /* may be different from z->lflg */
			z->how  = SH( CENHOW + b );
			z->tim  = LG( CENTIM + b );       /* time and date into one long */
			z->crc  = LG( CENCRC + b );
			z->siz  = LG( CENSIZ + b );
			z->len  = LG( CENLEN + b );
			z->nam  = SH( CENNAM + b );
			z->cext = SH( CENEXT + b );       /* may be different from z->ext */
			z->com  = SH( CENCOM + b );
			z->dsk  = SH( CENDSK + b );
			z->att  = SH( CENATT + b );
			z->atx  = LG( CENATX + b );
			z->dosflag = (z->vem & 0xFF00) == 0;
			temp = LG(CENOFF + b);            /* to avoid Coherent compiler bug */
			if ( z->off != temp && !pG->adjust ) {
				zipwarn( "local offset in central header incorrect for ", z->zname );
				return AfterError2( f, ZEN_FORM07 );
			}

			/* Compare name and extra fields and read comment field */
			if ( (t = MALLOC( z->nam )) == NULL ) return AfterError2( f, ZEN_MEM28 );
			if ( fread( t, z->nam, 1, f ) != 1 ) {
				FREE( t );
				return AfterError1( f );
			}
			// BUG fix 1.54
			for ( ptr = t; (ulg)(ptr - t) < z->nam; ptr++ ) if ( *ptr == '/' ) *ptr = '\\';	// SLASH Added
			if ( memcmp( t, z->zname, z->nam ) ) {
				FREE( t );
				zipwarn( "names in local and central differ for ", z->zname );
				return AfterError2( f, ZEN_FORM08 );
			}
			FREE( t );
			if ( z->cext ) {
				if ( (z->cextra = MALLOC( z->cext )) == NULL ) return ZEN_MEM29;
				if ( fread( z->cextra, z->cext, 1, f ) != 1 ) {
					FREE( z->cextra );
					return AfterError1( f );
				}
				if ( z->ext == z->cext && memcmp( z->extra, z->cextra, z->ext ) == 0 ) {
					FREE( z->cextra );
					z->cextra = z->extra;
				}
			}
			if ( z->com ) {
				if ( (z->comment = MALLOC( z->com )) == NULL ) return AfterError2( f, ZEN_MEM30 );
				if ( fread( z->comment, z->com, 1, f ) != 1 ) {
					FREE( z->comment );
					return AfterError1( f );
				}
			}

			/* Note oddities */
			if ( pG->verbose ) {
				if ( (n = z->vem >> 8) >= NUM_HOSTS ) {
					sprintf( pG->errbuf, "made by version %d.%d on system type %d: ",
								(ush)(z->vem & 0xFF) / (ush)10,
								(ush)(z->vem & 0xFF) % (ush)10, z->vem >> 8 );
					zipwarn( pG->errbuf, z->zname );
				}
				if ( z->ver != 10 && z->ver != 11 && z->ver != 20 ) {
					sprintf( pG->errbuf, "needs unzip %d.%d on system type %d: ",
								(ush)(z->ver & 0xFF) / (ush)10,
								(ush)(z->ver & 0xFF) % (ush)10, z->ver >> 8 );
					zipwarn( pG->errbuf, z->zname );
				}
				if ( z->how > DEFLATE ) {
					sprintf( pG->errbuf, "unknown compression method %u: ", z->how );
					zipwarn( pG->errbuf, z->zname );
				}
				if ( z->dsk ) {
					sprintf( pG->errbuf, "starts on disk %u: ", z->dsk );
					zipwarn( pG->errbuf, z->zname );
				}
				if ( z->att & ~1 ) {
					sprintf( pG->errbuf, "unknown internal attributes = 0x%04x: ", z->att );
					zipwarn( pG->errbuf, z->zname );
				}
			}

			/* Clear actions */
			z->mark  = 0;
			z->trash = 0;

			/* Update file offset */
			p += 4 + CENHEAD + z->nam + z->cext + z->com;

			/* Advance to next header structure */
			z = z->nxt;

			/* Read next signature */
			if ( fread( b, 4, 1, f ) != 1 ) return AfterError1( f );
	   }

		/* Read end header */
		if ( !pG->fix ) {
			if ( z != NULL || LG( b ) != ENDSIG ) {
				zipwarn( "missing end signature--probably not a zip file (did you", "" );
				zipwarn( "remember to use binary mode when you transferred it?)", "" );
				return AfterError2( f, ZEN_FORM09 );
			}
			if ( fread( b, ENDHEAD, 1, f ) != 1 ) return ferror( f ) ? ZEN_READ05 : ZEN_EOF03;
			if ( SH( ENDDSK + b ) || SH( ENDBEG + b ) || SH( ENDSUB + b ) != SH( ENDTOT + b ) )
				zipwarn( "multiple disk information ignored", "" );
			if ( pG->zcount != (extent)(SH( ENDSUB + b )) ) {
				zipwarn( "count in end of central directory incorrect", "" );
				return AfterError2( f, ZEN_FORM10 );
			}
			temp = LG( ENDSIZ + b );
			if ( temp != p - s ) {
				zipwarn( "central directory size is incorrect (made by stzip?)", "" );
				/* stzip 0.9 gets this wrong, so be tolerant */
				/* return ZEN_FORM??; */
			}
			temp = LG( ENDOFF + b );
			if ( temp != s && !pG->adjust ) {
				zipwarn( "central directory start is incorrect", "" );
				return AfterError2( f, ZEN_FORM11 );
			}
		}
		pG->cenbeg = s;
      /* Not needed anymore since the archive comment is read in the component
		 * but we need to skip the filecomment if present v1.60 */
		zcomlentmp = pG->fix ? 0 : SH( ENDCOM + b );
		if ( zcomlentmp ) {
			if ( (zcommenttmp = MALLOC( zcomlentmp )) == NULL ) return AfterError2( f, ZEN_MEM31 );
			if ( fread( zcommenttmp, zcomlentmp, 1, f ) != 1 ) {
				FREE( zcommenttmp );
				return AfterError1( f );
			}
		}
		if ( zcommenttmp ) FREE( zcommenttmp );

		/*		pG->zcomlen = pG->fix ? 0 : SH( ENDCOM + b );
		if ( pG->zcomlen ) {
			if ( (pG->zcomment = MALLOC( pG->zcomlen )) == NULL ) return AfterError2( f, ZEN_MEM31 );
			if ( fread( pG->zcomment, pG->zcomlen, 1, f ) != 1 ) {
				FREE( pG->zcomment );
				return AfterError1( f );
			}
		}
      */
		if ( pG->zipbeg && pG->verbose ) {
			/* This is most likely the size of the self-extracting code */
			sprintf( pG->errbuf, " has a preamble of %lu bytes", pG->zipbeg );
			zipwarn( pG->zipfile, pG->errbuf );
		}
		if ( !pG->fix && getc( f ) != EOF )
			zipwarn( "garbage at end of zip file ignored", "" );

		/* Done with zip file for now */
		fclose( f );

		/* If one or more files, sort by name */
		if ( pG->zcount ) {
			if ( (x = pG->zsort = (struct zlist **)MALLOC( pG->zcount * sizeof( struct zlist * ) )) == NULL )
				return ZEN_MEM32;
			for ( z = pG->zfiles; z != NULL; z = z->nxt ) *x++ = z;
			qsort( (char *)pG->zsort, pG->zcount, sizeof( struct zlist * ), zqcmp );
		}
	} /* End of if ( zipfile != NULL... */
	return ZEN_OK;
}

/* ===========================================================================
 * This will put the filename fn with '\' changed into '/' in fnamebuf
 * used for output to the zipfile.
	*fn :: Filename with possible backward slashes in it.
 */
static char *ChangeBWSlash( char *fn, struct Globals *pG ) {
	unsigned int i;

	strncpy( pG->fnamebuf, fn, MAX_PATH );
	pG->fnamebuf[ MAX_PATH - 1 ] = '\0';		// truncate the name if longer than MAX_PATH.
	for ( i = 0; pG->fnamebuf[i]; i++ ) if ( pG->fnamebuf[i] == '\\' ) pG->fnamebuf[i] = '/';
	return pG->fnamebuf;
}

/* ===========================================================================
 * Write a local header described by *z to file *f.
 * Return ZEN_OK or ZEN_TEMP07.
	*z :: Zip entry to write local header for.
	*f :: File to write to.
 */
int putlocal( struct zlist *z, FILE *f, struct Globals *pG ) {
	PUTLG( LOCSIG,  f );
	PUTSH( z->ver,  f );
	PUTSH( z->lflg, f );
	PUTSH( z->how,  f );
	PUTLG( z->tim,  f );
	PUTLG( z->crc,  f );
	PUTLG( z->siz,  f );
	PUTLG( z->len,  f );
	PUTSH( z->nam,  f );
	PUTSH( z->ext,  f );
	if ( fwrite( ChangeBWSlash( z->zname, pG ), 1, z->nam, f ) != z->nam ||
		(z->ext && fwrite( z->extra, 1, z->ext, f ) != z->ext) ) return ZEN_TEMP07;
	return ZEN_OK;
}

/* ===========================================================================
 * Write an extended local header described by *z to file *f.
 * Return an error code in the ZEN_ class.
	*z :: Zip entry to write local header for.
	*f :: File to write to.
 */
int putextended( struct zlist *z, FILE *f ) {
	PUTLG( EXTLOCSIG, f );
	PUTLG( z->crc, f );
	PUTLG( z->siz, f );
	PUTLG( z->len, f );
	return ZEN_OK;
}

/* ===========================================================================
 * Write a central header described by *z to file *f.  Return an error code
 * in the ZEN_ class.
	*z :: Zip entry to write central header for.
	*f :: File to write to.
 */
int putcentral( struct zlist *z, FILE *f, struct Globals *pG ) {
	PUTLG( CENSIG,  f );
	PUTSH( z->vem,  f );
	PUTSH( z->ver,  f );
	PUTSH( z->flg,  f );
	PUTSH( z->how,  f );
	PUTLG( z->tim,  f );
	PUTLG( z->crc,  f );
	PUTLG( z->siz,  f );
	PUTLG( z->len,  f );
	PUTSH( z->nam,  f );
	PUTSH( z->cext, f );
	PUTSH( z->com,  f );
	PUTSH( z->dsk,  f );
	PUTSH( z->att,  f );
	PUTLG( z->atx,  f );
	PUTLG( z->off,  f );
	if ( fwrite( ChangeBWSlash( z->zname, pG ), 1, z->nam, f ) != z->nam ||
			(z->cext && fwrite( z->cextra,  1, z->cext, f ) != z->cext) ||
			(z->com  && fwrite( z->comment, 1, z->com,  f ) != z->com) ) return ZEN_TEMP08;
	return ZEN_OK;
}

/* ===========================================================================
 * Write the end of central directory data to file *f.
 * Return ZEN_OK or ZEN_TEMP09.
	 n :: Number of entries in central directory.
	 s :: Size of central directory.
	 c :: Offset of central directory.
	 m :: Length of zip file comment (0 if none).
	*z :: Zip file comment if m != 0.
	*f :: File to write to.
 */
int putend( int n, ulg s, ulg c, extent m, char *z, FILE *f ) {
	PUTLG( ENDSIG, f );
	PUTSH( 0, f );
	PUTSH( 0, f );
	PUTSH( n, f );
	PUTSH( n, f );
	PUTLG( s, f );
	PUTLG( c, f );
	PUTSH( m, f );
	if ( m && fwrite( z, 1, m, f ) != m ) return ZEN_TEMP09;
	return ZEN_OK;
}

/* Note: a zip "entry" includes a local header (which includes the file
 * name), an encryption header if encrypting, the compressed data
 * and possibly an extended local header.
 */

/* ===========================================================================
 * Copy the zip entry described by *z from file *x to file *y.  Return an
 * error code in the ZEN_ class.  Also update tempzn by the number of bytes
 * copied.
	*z     :: Zip entry to copy.
	*x, *y :: Source and destination files.
 */
int zipcopy( struct zlist *z, FILE *x, FILE *y, struct Globals *pG ) {
	ulg n;                /* holds local header offset */

	Trace( ("zipcopy %s\n", z->zname ) );
	n = (ulg)(4 + LOCHEAD) + (ulg)z->nam + (ulg)z->ext;

	if ( pG->fix > 1 ) {
		if ( fseek( x, z->off + n, SEEK_SET ) ) /* seek to compressed data */
			return ferror( x ) ? ZEN_READ06 : ZEN_EOF04;

		if ( pG->fix > 2 ) {
			/* Update length of entry's name, it may have been changed.  This is
			 * needed to support the ZipNote ability to rename archive entries.
			 */
			z->nam = lstrlen( z->zname );
			n = (ulg)(4 + LOCHEAD) + (ulg)z->nam + (ulg)z->ext;
		}

		/* do not trust the old compressed size */
		if ( putlocal( z, y, pG ) != ZEN_OK ) return ZEN_TEMP10;

		z->off = pG->tempzn;
		pG->tempzn += n;
		n = z->siz;
	} else {
		if ( fseek( x, z->off, SEEK_SET ) )     /* seek to local header */
			return ferror( x ) ? ZEN_READ07 : ZEN_EOF05;

		z->off = pG->tempzn;
		n += z->siz;
	}
	/* copy the compressed data and the extended local header if there is one */
	if ( z->lflg & 8 ) n += 16;
	pG->tempzn += n;
	return fcopy( x, y, n );
}

#ifdef USE_EF_UX_TIME
/* ===========================================================================
 * This function scans the extra field for a IZUNIX block containing
 * Unix style time_t (GMT) values for the entry's access and modification
 * time.  If a valid block is found, both time stamps are copied to the
 * ztimebuf structure.
 * The return value is 1 (TRUE) in case of success, 0 otherwise.
	*ef_buf :: Buffer containing extra field.
	 ef_len :: Total length of extra field.
	*z_utim :: Return storage: atime and mtime.
 */
static int ef_scan_ux_time( char *ef_buf, extent ef_len, ztimbuf *z_utim ) {
	int      r = 0;
	unsigned eb_id;
	extent   eb_len;

	if ( ef_len == 0 || ef_buf == NULL ) return 0;

	Trace( ("ef_scan_ux_time: scanning extra field of length %u\n", ef_len) );
	while ( ef_len >= EB_HEADSIZE ) {
		eb_id  = SH( EB_ID  + ef_buf );
		eb_len = SH( EB_LEN + ef_buf );

		if ( eb_len > (ef_len - EB_HEADSIZE) ) {
			/* Discovered some extra field inconsistency! */
			Trace( ("ef_scan_ux_time: block length %u > rest ef_size %u\n", eb_len, ef_len - EB_HEADSIZE) );
			break;
		}

		if ( eb_id == EF_IZUNIX && eb_len >= EB_UX_MINLEN ) {
			Trace( ("ef_scan_ux_time: Found IZUNIX extra field\n") );
			z_utim->actime  = LG( (EB_HEADSIZE + EB_UX_ATIME) + ef_buf );
			z_utim->modtime = LG( (EB_HEADSIZE + EB_UX_MTIME) + ef_buf );
			Trace( ("  Unix EF access time = %ld\n", z_utim->actime) );
			Trace( ("  Unix EF modif. time = %ld\n", z_utim->modtime) );
			r = 1;                   /* signal success */
			break;
		}
		/* Skip this extra field block */
		ef_buf += (eb_len + EB_HEADSIZE);
		ef_len -= (eb_len + EB_HEADSIZE);
	}
	return r;
}

/* ===========================================================================
 */
int get_ef_ux_ztime( struct zlist *z, ztimbuf *z_utim ) {
	int r;

	/* First, scan local extra field. */
	r = ef_scan_ux_time( z->extra, z->ext, z_utim );

	/* If this was not successful, try central extra field, but only if
	 * is really different.
	 */
	if ( !r && z->cext > 0 && z->cextra != z->extra )
		r = ef_scan_ux_time( z->cextra, z->cext, z_utim );

	return r;
}
#endif /* USE_EF_UX_TIME */

/*---------------------------------------------------------------------------
// If we do not have a full path then FOF_ALLOWUNDO does not work!
*/
int EraseFile( struct Globals *pG, char *Fname ) {
	SHFILEOPSTRUCT SHF;

	SHF.hwnd   = pG->global_handle;
	SHF.wFunc  = FO_DELETE;
	SHF.pFrom  = Fname;
	SHF.pTo    = NULL;
	SHF.fFlags = FOF_NOCONFIRMATION | FOF_SILENT;
	if ( pG->HowToMove ) SHF.fFlags |= FOF_ALLOWUNDO;
	Fname[ lstrlen(Fname)+ 1 ] = '\0';
	return SHFileOperation( &SHF ); //Returns zero if successful or nonzero if an error occurs.
}

/* ===========================================================================
 * Cut the last path component off the name *p in place.
 * This should work on both internal and external names.
	*p :: Path string.
*/
static void cutpath( char *p ) {
	char *r;              /* pointer to last path delimiter */

	if ( (r = strrchr( p, '\\' )) != NULL )		// SLASH
		*r = 0;
	else
		*p = 0;
}

/* ===========================================================================
 * Delete the compressed files and the directories that contained the deleted
 * files, if empty.  Return an error code in the ZEN_ class.  Failure of
 * destroy() or rmdir() is ignored.
 */
int trash( struct Globals *pG ) {
	extent i;             /* counter on deleted names */
	extent n;             /* number of directories to delete */
	struct zlist **s;     /* table of zip entries to handle, sorted */
	struct zlist  *z;     /* current zip entry */

	/* Delete marked names and count directories */
	n = 0;
	for ( z = pG->zfiles; z != NULL; z = z->nxt ) if ( z->mark == 1 || z->trash ) {
		z->mark = 1;
		if ( z->zname[z->nam - 1] != '\\' ) { /* don't unlink directory SLASH */
			if ( pG->verbose ) printf( "deleting file %s\n", z->name );
			if ( EraseFile( pG, GetFullPath( pG, z->name ) ) ) { //v1.6017
         //	if ( destroy( z->name ) ) {
				zipwarn( "error deleting", z->name );
			}
			/* Try to delete all paths that lead up to marked names. This is
			 * necessary only with the -D option.
			 */
			if ( !pG->dirnames ) {
				diag( "no dirnames will be kept", pG );
				cutpath( z->name );
				cutpath( z->zname );
				if ( z->zname[0] != '\0' ) {
					lstrcat( z->zname, "\\" );	// SLASH
				}
				z->nam = lstrlen( z->zname );
				if ( z->nam > 0 ) n++;
			}
		} else {
			n++;
		}
	}

	/* Construct the list of all marked directories. Some may be duplicated
	 * if -D was used.
	 */
	if ( n ) {
		if ( (s = (struct zlist **)MALLOC( n * sizeof( struct zlist * ) )) == NULL ) return ZEN_MEM33;
		n = 0;
		for ( z = pG->zfiles; z != NULL; z = z->nxt ) {
			if ( z->mark && z->nam > 0 && z->zname[z->nam - 1] == '\\'			// SLASH
				  && (n == 0 || strcmp( z->zname, s[n-1]->zname ) != 0) ) {
				s[n++] = z;
			}
		}
		/* Sort the files in reverse order to get subdirectories first.
		 * To avoid problems with strange naming conventions.(as in VMS),
		 * we sort on the internal names, so x/y/z will always be removed
		 * before x/y. On VMS, x/y/z > x/y but [x.y.z] < [x.y]
		 */
		qsort( (char *)s, n, sizeof( struct zlist *), rqcmp );

		for ( i = 0; i < n; i++ ) {
			char *p = s[i]->name;
			if ( *p == '\0' ) continue;
			if ( p[lstrlen( p ) - 1] == '\\' ) { /* keep VMS [x.y]z.dir;1 intact SLASH */
				p[lstrlen( p ) - 1] = '\0';
			}
			if ( i == 0 || strcmp( s[i]->zname, s[i-1]->zname ) != 0 ) {
				if ( pG->verbose ) printf( "trashing directory %s (if empty)\n", s[i]->name );
				rmdir( s[i]->name );
			}
		}
		FREE( s );
	}
	return ZEN_OK;
}

