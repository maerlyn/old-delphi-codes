/* ZipUp.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
*/

#include <ctype.h>
#include "zip.h"
#include "Globals.h"

#include "crypt.h"
#include <windows.h>

/* Use the raw functions for MSDOS to save on buffer space.
   (This sort of stuff belongs in fileio.c, but oh well.) */
#include "zipup.h"


/* Local functions */
static int suffixes( char *, char *, struct Globals *pG );

/* ===========================================================================
 * Return the percentage compression from n to m using only integer
 * operations.
	n :: Is the original size.
	m :: Is the new size.
*/
int percent( ulg n, ulg m ) {
	if ( n > 0xFFFFFFL ) {       /* If n >= 16M */
		n += 0x80;  n >>= 8;      /*  then divide n and m by 256 */
		m += 0x80;  m >>= 8;
	}
	return n > m ? (int)(1 + (200 * (n - m)/ n)) / 2 : 0;
}

/* ===========================================================================
 * Return true if a ends in any of the suffixes in the list s.
	*a :: Name to check suffix of.
	*s :: List of suffixes separated by : or ;
*/
static int suffixes( char *a, char *s, struct Globals *pG ) {
	int   m;              /* true if suffix matches so far */
	char *p;              /* pointer into special */
	char *q;              /* pointer into name a */

	m = 1;
	q = a + lstrlen( a ) - 1;
	for ( p = s + lstrlen( s ) - 1; p >= s; p-- )
		if ( *p == ':' || *p == ';' )
			if ( m ) return 1;
			else {
				m = 1;
				q = a + lstrlen( a ) - 1;
			}
		else {
			m = m && q >= a && case_map( *p ) == case_map( *q );
			q--;
		}
	return m;
}

/* Note: a zip "entry" includes a local header (which includes the file
 * name), an encryption header if encrypting, the compressed data
 * and possibly an extended local header.
 */

/* ===========================================================================
 * Compress the file z->name into the zip entry described by *z and write
 * it to the file *y. Encrypt if requested.  Return an error code in the
 * ZEN_ class.  Also, update tempzn by the number of bytes written.
	*z :: Zip entry to compress.
	*y :: Output file.
 */
int zipup( struct zlist *z, FILE *y, struct Globals *pG ) {
	ztimbuf	f_utim;			/* UNIX GMT timestamps, filled by filetime()					*/
	ulg		tim;				/* Time returned by filetime()									*/
	ulg		a = 0L;			/* Attributes returned by filetime()							*/
	char	  *b;					/* Malloc'ed file buffer											*/
	extent	k = 0;			/* Result of zread													*/
	int		l = 0;			/* True if this file is a symbolic link						*/
	int		m;					/* Method for this entry											*/
	ulg		o, p;				/* Offsets in zip file												*/
	long		q = -3L;			/* Size returned by filetime										*/
	int		r;					/* Temporary variable												*/
	ulg		s = 0L;			/* Size of compressed data											*/
	int		isdir;			/* Set for a directory name										*/
	int		set_type = 0;	/* Set if file type (ascii/binary) unknown					*/

	z->nam = lstrlen( z->zname );
	isdir = z->zname[ z->nam - 1 ] == '\\';	// SLASH

#	ifdef USE_STRM_INPUT
	if ( pG->UseInStream ) {
		/* This replaces the filetime function */
		a = ((ulg)(S_IFREG | S_IREAD) << 16) | (ulg)pG->StrFileAttr;
		q = pG->InStreamSize;
		tim = f_utim.actime = f_utim.modtime = pG->StrFileDate;
	} else
#	endif
		if ( (tim = filetime( z->name, &a, &q, &f_utim, pG )) == 0 || q < -2L ) return ZEN_OPEN02;

	/* q is set to -1 if the input file is a device, -2 for a volume label */
	if ( q == -2L ) {
		isdir = 1;
		q     = 0;
	} else if ( isdir != ((a & MSDOS_DIR_ATTR) != 0) ) {
		/* don't overwrite a directory with a file and vice-versa */
		return ZEN_MISS01;
	}
	z->att = (ush)UNKNOWN;	/* will be changed later					*/
	z->atx = 0;					/* may be changed by set_extra_field()	*/

	/* Free the old extra fields which are probably obsolete */
	if ( z->ext ) FREE( z->extra );
	if ( z->cext && z->extra != z->cextra ) FREE( z->cextra );
	z->ext = z->cext = 0;

	pG->window_size = 0L;

	/* Select method based on the suffix and the global method */
	m = pG->special != NULL && suffixes( z->name, pG->special, pG ) ? STORE : pG->method;

	if ( isdir ) {     /* directory */
		pG->ifile	= fbad;
		m		= STORE;
		q		= 0;
	} else {
		/* Callback:  action, error code, filesize, filename */
		user_callback( 1, 0, z->len /*fsize*/, z->name, pG );

		/* ======= Here's the open of the input file if applicable ======== */
		/* This time it will stay open for a little while. */
#		ifdef USE_STRM_INPUT
		if ( !pG->UseInStream && (pG->ifile = zopen( GetFullPath( pG, z->name ), fhow )) == fbad ) return ZEN_OPEN03;
#		else
		if ( (pG->ifile = zopen( z->name, fhow )) == fbad ) return ZEN_OPEN03;
#		endif
	}
	z->tim = tim;

	if ( l || q == 0 ) m = STORE;
	if ( m == BEST )   m = DEFLATE;

	/* Do not create STORED files with extended local headers if the
	 * input size is not known, because such files could not be extracted.
	 * So if the zip file is not seekable and the input file is not
	 * on disk, obey the -0 option by forcing deflation with stored block.
	 * Note however that using "zip -0" as filter is not very useful...
	 * ??? to be done.
	 */

	/* Fill in header information and write local header to zip file.
	 * This header will later be re-written since compressed length and
	 * crc are not yet known.
	 */

	/* (Assume ext, cext, com, and zname already filled in.) */
	/* (RCV Added (ush)( ... ) */
	z->vem = (ush)(z->dosflag ? 20 : OS_CODE + 22);	/* Made under MSDOS by PKZIP 2.0 */
	/* For a FAT file system, we cheat and pretend that the file
	 * was not made on OS2, but under DOS. unzip is confused otherwise.
	 */
	/* (RCV Added (ush)( ... ) */
	z->ver = (ush)( (m == STORE) ? 10 : 20 );     /* Need PKUNZIP 2.0 except for store */
	z->crc = 0;  /* to be updated later */
	/* Assume first that we will need an extended local header: */
	z->flg = 8;  /* to be updated later */

#ifdef CRYPT
	if ( pG->key ) {
		z->flg |= 1;
		/* Since we do not yet know the crc here, we pretend that the crc
		 * is the modification time:
		 */
		z->crc = z->tim << 16;
	}
#endif /* CRYPT */

	z->lflg = z->flg;
	/* (RCV Added below (ush) */
	z->how = (ush)m;                        /* may be changed later   */
	z->siz = (ulg)(m == STORE && q >= 0 ? q : 0);  /* will be changed later  */
	z->len = (ulg)(q >= 0 ? q : 0);                /* may be changed later   */
	z->dsk = 0;
	if ( z->att == (ush)UNKNOWN ) {
		z->att = BINARY;                     /* set sensible value in header */
		set_type = 1;
	}

	/* Attributes from filetime(), flag bits from set_extra_field():  */
	z->atx = z->dosflag ? a & 0xFF : a | (z->atx & 0x0000FF00);
	z->off = pG->tempzn;
	if ( (r = putlocal( z, y, pG )) != ZEN_OK ) {
		zclose( pG->ifile );
		return r;
	}
	pG->tempzn += 4 + LOCHEAD + z->nam + z->ext;

#ifdef CRYPT
	if ( pG->key ) {
		crypthead( pG->key, z->crc, y, pG );
		z->siz     += RAND_HEAD_LEN;  /* to be updated later */
		pG->tempzn += RAND_HEAD_LEN;
	}
#endif /* CRYPT */

	if ( ferror( y ) ) {
		zclose( pG->ifile );
		ziperr( ZEN_WRITE05, pG );
	}
	o = ftell( y ); /* for error checking, ftell can fail on pipes */
	if ( ferror( y ) ) clearerr( y );

	/* Write stored or deflated file to zip file */
	pG->isize = 0L;
	pG->crc = crc32( 0L, (uch *)NULL, 0 );

	if ( m == DEFLATE ) {
		bi_init( y, pG );
		if ( set_type ) z->att = (ush)UNKNOWN; /* will be changed in deflate() */
		ct_init( &z->att, &m, pG );
		lm_init( pG->level, &z->flg, pG );

		/* ========== PERFORM THE DEFLATE ==============*/
		s = deflate( pG );
		if ( pG->global_abort_sw ) {
			zclose( pG->ifile );
			return ZEN_ABORT;
		}
	} else if ( !isdir ) {
		if ( (b = MALLOC( CBSZ )) == NULL ) {
			zclose( pG->ifile );
			return ZEN_MEM18;
		}

		if ( l ) {
			/* symbolic link processing */
			k = rdsymlnk( z->name, b, CBSZ );

			/* compute crc first because zfwrite
			 * will alter the buffer b points to !!
			 */
			pG->crc = crc32( pG->crc, (uch *)b, k );
			if ( zfwrite( b, 1, k, y, pG ) != k ) {
				FREE( b );
				zclose( pG->ifile );
				return ZEN_TEMP05;
			}
			pG->isize = k;
		} else while ( (k = file_read( b, CBSZ, pG )) > 0 && k != (extent)EOF ) {
			if ( zfwrite( b, 1, k, y, pG ) != k ) {
				FREE( b );
				zclose( pG->ifile );
				return ZEN_TEMP06;
			}
		}
		FREE( b );
		s = pG->isize;
	}
	if ( pG->ifile != fbad && zerr( pG->ifile ) ) {
		zipwarn( "could not read input file: ", z->zname );
	}
	if ( pG->ifile != fbad ) zclose( pG->ifile );  /* Close the input file */

	pG->tempzn += s;
	p = pG->tempzn; /* save for future fseek() */

	/* Check input size */
	if ( q >= 0 && pG->isize != (ulg)q && !pG->translate_eol ) {
		Trace( (" i=%ld, q=%ld ", pG->isize, q) );
		zipwarn( " file size changed while zipping ", z->name );
	}

	/* Try to rewrite the local header with correct information */
	z->crc = pG->crc;
	z->siz = s;

#ifdef CRYPT
	if ( pG->key ) z->siz += RAND_HEAD_LEN;
#endif

	z->len = pG->isize;
	if ( fseek( y, z->off, SEEK_SET ) ) {
		if ( z->how != (ush)m ) ziperr( ZEN_WRITE06, pG );
		if ( m == STORE && q < 0 ) ziperr( ZEN_PARMS14, pG );
		if ( (r = putextended( z, y )) != ZEN_OK ) return r;
		pG->tempzn += 16L;
		z->flg  = z->lflg; /* if flg modified by inflate */
	} else {
		/* seek ok, ftell() should work, check compressed size */
		if ( p - o != s ) {
			printf( " s=%ld, actual=%ld ", s, p-o );
			ziperr( ZEN_LOGIC06, pG );
		}
		/* (RCV Added in two lines below (ush)( ... ) */
		z->how = (ush)m;
		z->ver = (ush)( (m == STORE) ? 10 : 20 );     /* Need PKUNZIP 2.0 except for store    */
		if ( (z->flg & 1) == 0 ) z->flg &= ~8;        /* clear the extended local header flag */
		z->lflg = z->flg;
		/* rewrite the local header: */
		if ( (r = putlocal( z, y, pG )) != ZEN_OK ) return r;
		if ( fseek( y, p, SEEK_SET ) ) return ZEN_READ03;
		if ( (z->flg & 1) != 0 ) {
			/* encrypted file, extended header still required */
			if ( (r = putextended( z, y )) != ZEN_OK ) return r;
			pG->tempzn += 16L;
		}
	}
	/* Free the local extra field which is no longer needed */
	if ( z->ext ) {
		if ( z->extra != z->cextra ) FREE( z->extra );
		z->ext = 0;
	}

	/* Display statistics */
	if ( pG->noisy && pG->verbose ) {
		char tmpbuf[10];

      lstrcpy( tmpbuf, (m == DEFLATE) ? "deflated" : "stored" );
		printf( "%s  in=%lu,  out=%lu,  %d%%", tmpbuf, pG->isize, s, percent( pG->isize, s ) );
	}
	return ZEN_OK;
}

/* ===========================================================================
 * Read a new buffer from the current input file, perform end-of-line
 * translation, and update the crc and input file size.
 * IN assertion: size >= 2 (for end-of-line translation)
 */
int file_read( char *buf, unsigned size, struct Globals *pG ) {
	unsigned  len, olen;
	char     *b;

	if ( pG->translate_eol == 0 ) {
#		ifdef USE_STRM_INPUT
		if ( pG->UseInStream ) {
			olen = len = size = min( pG->InStreamSize - pG->StreamPos, size );
			memcpy( buf, (char *)pG->InStream + pG->StreamPos, size );
			pG->StreamPos += size;
		} else olen = len = zread( pG->ifile, buf, size );
#		else
		olen = len = zread( pG->ifile, buf, size );
#		endif
		if ( len == (unsigned)EOF || !len ) return (int)len;
	} else if ( pG->translate_eol == 1 ) {
		/* Transform LF to CR LF */
		size >>= 1;
		b = buf + size;
		olen = size = len = zread( pG->ifile, b, size );
		if ( len == (unsigned)EOF || !len ) return (int)len;
		do {
			if ( (*buf++ = *b++) == '\n' ) *(buf - 1) = '\r', *buf++ = '\n', len++;
		} while ( --size != 0 );
		buf -= len;
	} else {
		/* Transform CR LF to LF and suppress final ^Z */
		b = buf;
		olen = size = len = zread( pG->ifile, buf, size - 1 );
		if ( len == (unsigned)EOF || !len ) return (int)len;
		buf[len] = '\n'; /* I should check if next char is really a \n */

		do {
			if ( ( *buf++ = *b++) == '\r' && *b == '\n' ) buf--, len--;
		} while ( --size != 0 );

		if ( !len ) {
			olen = zread( pG->ifile, buf, 1 );
			len  = 1;  /* keep single \r if EOF */
		} else {
			buf -= len;
			if ( buf[len-1] == CTRLZ ) len--; /* suppress final ^Z */
		}
	}
	/* Callback:  actioncode 2 = increment progress bar */
	user_callback( 2, 0, olen, NULL, pG ); // Added for progress bar support.

	pG->crc = crc32( pG->crc, (uch *)buf, len );
	pG->isize += (ulg)len;
	return (int)len;
}

