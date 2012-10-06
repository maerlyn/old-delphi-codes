/*
 * crypt.c (full version) by Info-ZIP.      Last revised:  [see crypt.h]
 *
 * This code is not copyrighted and is put in the public domain.  The
 * encryption/decryption parts (as opposed to the non-echoing password
 * parts) were originally written in Europe; the whole file can there-
 * fore be freely distributed from any country except the USA.  If this
 * code is imported into the USA, it cannot be re-exported from from
 * there to another country.  (This restriction might seem curious, but
 * this is what US law requires.)
 */
/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Sep 22, 2000.
 */
/* This encryption code is a direct transcription of the algorithm from
 * Roger Schlafly, described by Phil Katz in the file appnote.txt.  This
 * file (appnote.txt) is distributed with the PKZIP program (even in the
 * version without encryption capabilities).
 */

#define ZCRYPT_INTERNAL

#include "unzip.h"
#define ziperr( c, h ) return

#include "crypt.h"
#include "ttyio.h"

/* char *key = (char *)NULL; moved to globals.h */
#ifndef FUNZIP
	static int testp( struct Globals *pG, uch *h );
	static int testkey( struct Globals *pG, uch *h, char *key );
#endif

#ifndef CRC_32_TAB
#  define CRC_32_TAB     crc_32_tab
#endif

#define CRC32(c, b) (CRC_32_TAB[((int)(c) ^ (b)) & 0xff] ^ ((c) >> 8))


/* ===========================================================================
 * Return the next byte in the pseudo-random sequence
 */
int decrypt_byte( struct Globals *pG ) {
	unsigned temp;  /* POTENTIAL BUG:  temp*(temp^1) may overflow in an
						  * unpredictable manner on 16-bit systems; not a problem
						  * with any known compiler so far, though
                    */
	temp = ( (unsigned)pG->keys[2] & 0xffff ) | 2;
	return (int)( ((temp *(temp ^ 1)) >> 8) & 0xff);
}


/* ===========================================================================
 * Update the encryption keys with the next byte of plain text
	c :: Byte of plain text.
 */
int update_keys( struct Globals *pG, int c ) {
	pG->keys[0]  = CRC32( pG->keys[0], c );
	pG->keys[1] += pG->keys[0] & 0xff;
	pG->keys[1]  = pG->keys[1] * 134775813L + 1;
	{
		register int keyshift = (int)(pG->keys[1] >> 24);
		pG->keys[2] = CRC32( pG->keys[2], keyshift );
	}
	return c;
}


/* ===========================================================================
 * Initialize the encryption keys and the random header according to
 * the given password.
	*passwd :: Password string with which to modify keys.
 */
void init_keys( struct Globals *pG, char *passwd ) {
	pG->keys[0] = 305419896L;
	pG->keys[1] = 591751049L;
	pG->keys[2] = 878082192L;
	while ( *passwd != '\0' ) {
		update_keys( pG, (int)*passwd );
		passwd++;
	}
}


#if (defined(UNZIP) && !defined(FUNZIP))
/* ===========================================================================
 * Get the password and set up keys for current zipfile member.  Return
 * PK_ class error.
 */
int decrypt( struct Globals *pG ) {
	ush b;
	int n, n1, r;
	uch h[RAND_HEAD_LEN];

	Trace( (pG, "in crypt.c, [incnt = %d]: ", pG->incnt) ); //stout

	/*
    if ((pG->pwdarg != NULL) && (lstrlen(pG->pwdarg) > 0))
       Trace( (pG, "pwd passed in via pG->pwdarg: %s", pG->pwdarg) );
    if (pG->P_flag == TRUE)
       Trace( (pG, "pG->P_flag was TRUE at beg of decrypt()") );
    else
       Trace( (pG, "pG->P_flag was FALSE at beg of decrypt()") );
	*/

	/* get header once (turn off "encrypted" flag temporarily so we don't
	 * try to decrypt the same data twice) */
	pG->pInfo->encrypted = false;
	defer_leftover_input( pG );
	for ( n = 0; n < RAND_HEAD_LEN; n++ ) {
		b = (ush)NEXTBYTE;		/* RCV: ush added */
		h[n] = (uch)b;
		//Trace( (pG, " (%02x)", h[n]) );  //stdout
	}
	undefer_input( pG );
	pG->pInfo->encrypted = true;

	Trace( (pG, "EWE - in crypt.c, near pG->newzip test") );

	if ( !pG->PwdReqCount ) return PK_WARN;
	if ( pG->newzip || pG->CallerVersion >= 160 )  {
		pG->newzip = false;
		Trace( (pG, "newzip was set to false") );

		if ( pG->P_flag == true ) {     /* user gave password on command line */
			Trace( (pG, "user set password=%s", pG->pwdarg) );
			if ( !pG->key ) {
				Trace( (pG, "global key was NULL") );
				if ( (pG->key = (char *)MALLOC( PWLEN + 1 )) == NULL ) return PK_MEM2;
				strncpy( pG->key, pG->pwdarg, PWLEN );	// 1.6013 put back again
				/* inhibit password prompting for version < 160! */
				if ( pG->CallerVersion < 160 ) pG->nopwd = true;	// RCV: 1.607 changed
			}
			// For version >=160 w'll always take the original password as startpoint ( RCV: 1.607 )
			if ( pG->CallerVersion >= 160 )
				strncpy( pG->key, pG->pwdarg, PWLEN + 1 );	// 1.6013 ( removed !pG->Key || ... )
		} else if ( pG->key ) {  /* get rid of previous zipfile's key */
			if ( pG->CallerVersion < 160 ) {	//  1.6014
				// no password came in on cmd line
				Trace( (pG, "getting rid of previous zipfiles password") );
				FREE( pG->key );
				pG->key = NULL;
			}
		}
	}

	Trace( (pG, "EWE- near 'have key already' test") );
	/* if have key already, test it; else allocate memory for it */
	if ( pG->key ) {
		if ( !testp( pG, h ) ) {
			Trace( (pG, "existing pwd OK") );
			return PK_COOL;								/* existing password OK (else prompt for new) */
		} else if ( pG->nopwd ) return PK_WARN;	/* user indicated no more prompting				 */
	} else
		if ( (pG->key = (char *)MALLOC( PWLEN + 1 )) == (char *)NULL ) return PK_MEM2;
	Trace( (pG, "EWE- near 'try a few keys' test") );

	/* try a few keys */
	n  = pG->PwdReqCount;	// Version <160 then 1 else a variable with default 1. (RCV: 1.607)
	n1 = 15;
	do {
		if ( pG->CallerVersion >= 160 ) {	// RCV: 1.607
			// Call the component with a request for a password
			user_callback( pG, 8, 2, n, pG->filename );
			if ( pG->CallBackData.error_code ) {
				lstrcpy( pG->key, pG->CallBackData.filenameormsg );
				if ( !testp( pG, h ) ) return PK_COOL;
			}
			if ( pG->CallBackData.actioncode != 8 ) {	// v1.6024
				pG->nopwd = true;
				n1 = 1;
			}
			n = min( pG->CallBackData.fsize & 0x0F, n1 );
		} else {
			r = UzpGetPassWrd( pG );

			if ( r == IZ_PW_ERROR ) {			/* internal error in fetch of PW		 */
				FREE( pG->key );					/* No DialogBox, Password too long, memcopy failed */
				pG->key = NULL;
				return PK_MEM2;
			}
			if ( r != IZ_PW_ENTERED ) {		/* user replied "Cancel" or entered with NULL string */
				*pG->key = '\0';					/*   We try the NIL password, ...	 */
				n = 0;								/*   and cancel fetch for this item. */
			}
			if ( !testp( pG, h ) ) return PK_COOL;

			if ( r == IZ_PW_CANCELALL )		/* User replied "Cancel"				 */
				pG->nopwd = true;					/* inhibit any further PW prompt!	 */
		}
	} while( --n > 0 && --n1 > 0 && !pG->global_abort_sw );	// v1.6024

	return PK_WARN;
} /* end function decrypt() */


/* ===========================================================================
 * Test the password.  Return -1 if bad, 0 if OK.
 */
static int testp( struct Globals *pG, uch *h ) {
	int r;
#if ( defined( STR_TO_CP1 ) || defined( STR_TO_CP2 ) || defined( STR_TO_CP3 ) )
	char *key_translated;
#endif
	/* On systems with "obscure" native character coding (e.g., EBCDIC),
	 * the first test translates the password to the "main standard"
	 * character coding.
    */
#ifdef STR_TO_CP1
	/* allocate buffer for translated password */
	if ( (key_translated = MALLOC( lstrlen( pG->key )+ 1 )) == (char *)NULL ) return -1;
	/* first try, test password translated "standard" charset */
	r = testkey( pG, h, STR_TO_CP1( key_translated, pG->key ) );
#else
	/* first try, test password as supplied on the extractor's host */
	r = testkey( pG, h, pG->key );
#endif

#ifdef STR_TO_CP2
	if ( r != 0 ) {
# ifndef STR_TO_CP1
		/* now prepare for second (and maybe third) test with translated pwd */
		if ( (key_translated = MALLOC( lstrlen( pG->key )+ 1) ) == (char *)NULL ) return -1;
# endif
		/* second try, password translated to alternate ("standard") charset */
		r = testkey( pG, h, STR_TO_CP2( key_translated, pG->key ) );
# ifdef STR_TO_CP3
		if ( r != 0 )
			/* third try, password translated to another "standard" charset */
			r = testkey( pG, h, STR_TO_CP3( key_translated, pG->key ) );
# endif
# ifndef STR_TO_CP1
		FREE( key_translated );
# endif
	}
#endif /* STR_TO_CP2 */

#ifdef STR_TO_CP1
	FREE( key_translated );
	if ( r != 0 ) {
		/* last resort, test password as supplied on the extractor's host */
		r = testkey( pG, h, pG->key );
	}
#endif

	return r;
} /* end function testp() */


/* ===========================================================================
	*h   :: Decrypted header.
	*key :: Decryption password to test.
 */
static int testkey( struct Globals *pG, uch *h, char *key ) {
	ush b;
#ifdef ZIP10
	ush c;
#endif
	int n;
	uch *p;
	uch hh[RAND_HEAD_LEN]; /* decrypted header */

	/* set keys and save the encrypted header */
	init_keys( pG, key );
	memcpy( hh, h, RAND_HEAD_LEN );

	/* check password */
	for ( n = 0; n < RAND_HEAD_LEN; n++ ) {
		zdecode( hh[n] );
		Trace( (pG, " %02x", hh[n]) );  // stdout
	}

	Trace( (pG, "\n  lrec.crc= %08lx  crec.crc= %08lx  pInfo->ExtLocHdr= %s\n",
      pG->lrec.crc32, pG->pInfo->crc, pG->pInfo->ExtLocHdr ? "true" : "false") );  //stdout
	Trace( (pG, "  incnt = %d  unzip offset into zipfile = %ld\n",
      pG->incnt, pG->cur_zipfile_bufstart + pG->inptr - pG->inbuf) );  //stdout

	/* same test as in zipbare(): */

#ifdef ZIP10 /* check two bytes */
	c = hh[RAND_HEAD_LEN - 2], b = hh[RAND_HEAD_LEN - 1];
	Trace( (pG, "  (c | (b<<8)) = %04x  (crc >> 16) = %04x  lrec.time = %04x\n",
			(ush)(c | (b << 8)), (ush)(pG->lrec.crc32 >> 16), pG->lrec.last_mod_file_time) );  //stdout
	if ( (ush)(c | (b << 8)) != (pG->pInfo->ExtLocHdr ?
			pG->lrec.last_mod_file_time : (ush)(pG->lrec.crc32 >> 16)) ) return -1;  /* bad */
#else
	b = hh[RAND_HEAD_LEN - 1];
	Trace( (pG, "  b = %02x  (crc >> 24) = %02x  (lrec.time >> 8) = %02x\n",
		b, (ush)(pG->lrec.crc32 >> 24), (pG->lrec.last_mod_file_time >> 8)) );  //stdout
	if ( b != (pG->pInfo->ExtLocHdr ?
			pG->lrec.last_mod_file_time >> 8 : (ush)(pG->lrec.crc32 >> 24)) ) return -1;  /* bad */
#endif
	/* password OK:  decrypt current buffer contents before leaving */
	for ( n = (long)pG->incnt > pG->csize ? (int)pG->csize : pG->incnt, p = pG->inptr; n--; p++ )
		zdecode( *p );
	return 0;       /* OK */
} /* end function testkey() */

#endif /* UNZIP && !FUNZIP */

