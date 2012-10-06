/* crypt.c (full version) by Info-ZIP.      Last revised:  [see crypt.h]
 *
 * This code is not copyrighted and is put in the public domain.  The
 * encryption/decryption parts (as opposed to the non-echoing password
 * parts) were originally written in Europe; the whole file can there-
 * fore be freely distributed from any country except the USA.  If this
 * code is imported into the USA, it cannot be re-exported from from
 * there to another country.  (This restriction might seem curious, but
 * this is what US law requires.)
 */

/* This encryption code is a direct transcription of the algorithm from
 * Roger Schlafly, described by Phil Katz in the file appnote.txt.  This
 * file (appnote.txt) is distributed with the PKZIP program (even in the
 * version without encryption capabilities).
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 */

#include "Zip.h"
#include "Globals.h"
#include "Crypt.h"
#include "ttyio.h"

/* For the encoding task used in Zip (and ZipCloak), we want to initialize
 * the crypt algorithm with some reasonably unpredictable bytes, see
 * the crypthead() function. The standard rand() library function is
 * used to supply these 'random' bytes, which in turn is initialized by
 * a srand() call. The srand() function takes an "unsigned" (at least 16bit)
 * seed value as argument to determine the starting point of the rand()
 * pseudo-random number generator.
 * This seed number is constructed as "Seed = Seed1 .XOR. Seed2" with
 * Seed1 supplied by the current time (= "(unsigned)time()") and Seed2
 * as some (hopefully) nondeterministic bitmask. On many (most) systems,
 * we use some "process specific" number, as the PID or something similar,
 * but when nothing unpredictable is available, a fixed number may be
 * sufficient.
 * NOTE:
 * 1.) This implementation requires the availability of the following
 *     standard UNIX C runtime library functions: time(), rand(), srand().
 *     On systems where some of them are missing, the environment that
 *     incorporates the crypt routines must supply suitable replacement
 *     functions.
 * 2.) It is a very bad idea to use a second call to time() to set the
 *     "Seed2" number! In this case, both "Seed1" and "Seed2" would be
 *     (almost) identical, resulting in a (mostly) "zero" constant seed
 *     number passed to srand().
 *
 * The implementation environment defined in the "zip.h" header should
 * supply a reasonable definition for ZCR_SEED2 (an unsigned number; for
 * most implementations of rand() and srand(), only the lower 16 bits are
 * significant!). An example that works on many systems would be
 *      "#define ZCR_SEED2  (unsigned)getpid()".
 * The default definition for ZCR_SEED2 supplied below should be regarded
 * as a fallback to allow successful compilation in "beta state"
 * environments.
 */
#include <time.h>     /* time() function supplies first part of crypt seed */
	/* "last resort" source for second part of crypt seed pattern */

#ifndef ZCR_SEED2		 /* RCV: Is now defined in Zip.h */
#  define ZCR_SEED2 (unsigned)3141592654L     /* use PI as default pattern */
#endif

//#ifndef Trace
//#  ifdef CRYPT_DEBUG
//#    define Trace( x ) printf x
//#  else
//#    define Trace( x )
//#  endif
//#endif

#ifndef CRC_32_TAB
#  define CRC_32_TAB pG->crc_32_tab
#endif

#define CRC32( c, b ) ( CRC_32_TAB[( (int)(c) ^ (b)) & 0xFF] ^ ((c) >> 8) )

/***********************************************************************
 * Return the next byte in the pseudo-random sequence
 */
int decrypt_byte( struct Globals *pG ) {
	unsigned temp;  /* POTENTIAL BUG:  temp*(temp^1) may overflow in an
                    * unpredictable manner on 16-bit systems; not a problem
                    * with any known compiler so far, though
						  */
	temp = ( (unsigned)pG->keys[2] & 0xFFFF ) | 2;
	return (int)(((temp * (temp ^ 1)) >> 8) & 0xFF);
}

/***********************************************************************
 * Update the encryption keys with the next byte of plain text
	c :: Byte of plain text.
 */
int update_keys( int c, struct Globals *pG ) {
	pG->keys[0]  = CRC32( pG->keys[0], c );
	pG->keys[1] += pG->keys[0] & 0xFF;
	pG->keys[1]  = pG->keys[1] * 134775813L + 1;
	{
		register int keyshift = (int)(pG->keys[1] >> 24 );
		pG->keys[2] = CRC32( pG->keys[2], keyshift );
	}
	return c;
}


/***********************************************************************
 * Initialize the encryption keys and the random header according to
 * the given password.
	*passwd :: Password string with which to modify keys.
 */
void init_keys( char *passwd, struct Globals *pG ) {
	pG->keys[0] = 305419896L;
	pG->keys[1] = 591751049L;
	pG->keys[2] = 878082192L;
	while ( *passwd != '\0' ) {
		update_keys( (int)*passwd, pG );
		passwd++;
	}
}

/***********************************************************************
 * Write encryption header to file zfile using the password passwd
 * and the cyclic redundancy check crc.
	*passwd :: Password string.
	 crc    :: Crc of file being encrypted.
	*zfile  :: Where to write header.
 */
void crypthead( char *passwd, ulg crc, FILE *zfile, struct Globals *pG ) {
	int n;                         /* Index in random header.       */
	int t;                         /* Temporary.                    */
	int c;                         /* Random byte.                  */
	int ztemp;                     /* Temporary for zencoded value. */
	uch header[RAND_HEAD_LEN - 2]; /* Random header.                */

	/* First generate RAND_HEAD_LEN - 2 random bytes. We encrypt the
	 * output of rand() to get less predictability, since rand() is
	 * often poorly implemented.
	 */
	if ( ++pG->calls == 1 ) {
		srand( (unsigned)time( NULL ) ^ ZCR_SEED2 );
	}
	init_keys( passwd, pG );
	for ( n = 0; n < RAND_HEAD_LEN - 2; n++ ) {
		c = (rand() >> 7) & 0xFF;
		header[n] = (uch)zencode( c, t );
	}
	/* Encrypt random header (last two bytes is high word of crc) */
	init_keys( passwd, pG );
	for ( n = 0; n < RAND_HEAD_LEN - 2; n++ ) {
		ztemp = zencode( header[n], t );
		putc( (uch)ztemp, zfile );	/* V1.5 Added (uch) */
	}
	ztemp = zencode( (int)(crc >> 16) & 0xFF, t );
	putc( (uch)ztemp, zfile );		/* V1.5 Added (uch) */
	ztemp = zencode( (int)(crc >> 24) & 0xFF, t );
	putc( (uch)ztemp, zfile );		/* V1.5 Added (uch) */
}

/***********************************************************************
 * If requested, encrypt the data in buf, and in any case call fwrite()
 * with the arguments to zfwrite().  Return what fwrite() returns.
	*buf       :: Data buffer.
	 item_size :: Size of each item in bytes.
	 nb        :: Number of items.
	*f         :: File to write to.
 */
unsigned zfwrite( void *buf, extent item_size, extent nb, FILE *f, struct Globals *pG ) {
	int t;  /* temporary */

	if ( pG->key ) {  /* key is the global password pointer */
		ulg   size;               /* buffer size          */
		char *p = (char*)buf;     /* steps through buffer */

		/* Encrypt data in buffer */
		for ( size = item_size * (ulg)nb; size != 0; p++, size-- ) {
			*p = (char)zencode( *p, t );
		}
	}
	/* Write the buffer out */
	return fwrite( buf, item_size, nb, f );
}

