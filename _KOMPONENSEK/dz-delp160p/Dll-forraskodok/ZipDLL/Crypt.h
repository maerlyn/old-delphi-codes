/* Crypt.h (full version) by Info-ZIP.   Last revised:  [see CR_VERSION_DATE]
 *
 * This header file is not copyrighted, and non-beta versions may be
 * distributed without restriction.
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
 */

#ifndef __crypt_h   /* don't include more than once */
#define __crypt_h

#define CR_MAJORVER        2
#define CR_MINORVER        7
#define CR_BETA_VER        ""
#define CR_VERSION_DATE    "22 April 1997"     /* last public release date */
#define CR_RELEASE

#ifdef REALLY_SHORT_SYMS
#  define decrypt_byte   dcrbyt
#endif

#define PWLEN  80            /* Input buffer size for reading encryption key. */
#define RAND_HEAD_LEN  12    /* Length of encryption random header.           */

/* encode byte c, using temp t.  Warning: c must not have side effects.       */
#define zencode( c, t )  (t = decrypt_byte( pG ), update_keys( c, pG ), t^(c))

/* decode byte c in place */
#define zdecode( c )   update_keys( c ^= decrypt_byte( pG ), pG )

int  decrypt_byte( struct Globals *pG );
int  update_keys( int c, struct Globals *pG );
void init_keys( char *passwd, struct Globals *pG );
void crypthead( char *, ulg, FILE *, struct Globals *pG );

unsigned zfwrite( void *, extent, extent, FILE *, struct Globals *pG );

#endif /* !__crypt_h */

