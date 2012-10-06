/* crypt.h (full version) by Info-ZIP.   Last revised:  [see CR_VERSION_DATE]
 * This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, May 27, 1999.
 *
 * This header file is not copyrighted, and non-beta versions may be
 * distributed without restriction.
 */

#ifndef __crypt_h   /* don't include more than once */
#define __crypt_h

#ifdef CRYPT
#  undef CRYPT
#endif
#define CRYPT  1    /* full version */

#define CR_MAJORVER        2
#define CR_MINORVER        7
#ifdef CR_BETA
#  define CR_BETA_VER      "m BETA"
#  define CR_VERSION_DATE  "13 April 1997"     /* last real code change */
#else
#  define CR_BETA_VER      ""
#  define CR_VERSION_DATE  "22 April 1997"     /* last public release date */
#  define CR_RELEASE
#endif

#if defined(VM_CMS) || defined(MVS)
#  ifndef CMS_MVS
#    define CMS_MVS
#  endif
#endif

#ifdef REALLY_SHORT_SYMS
#  define decrypt_byte   dcrbyt
#endif

#define PWLEN  80					/* input buffer size for reading encryption key */
#define RAND_HEAD_LEN  12		/* length of encryption random header */

/* the crc_32_tab array has to be provided externally for the crypt calculus  */
#ifndef UNZIP                 /* UnZip provides this in globals.h   */
   extern ulg *crc_32_tab;
#endif /* !UNZIP */

/* encode byte c, using temp t.  Warning: c must not have side effects. */
#define zencode( c, t )  (t = decrypt_byte( pG ), update_keys( c ), t^(c) )

/* decode byte c in place */
#define zdecode( c )   update_keys( pG, c ^= (uch)decrypt_byte( pG ) )    // RCV Added (uch).

int  decrypt_byte ( struct Globals *pG );
int  update_keys  ( struct Globals *pG, int c );
void init_keys    ( struct Globals *pG, char *passwd );

#if (defined(UNZIP) && !defined(FUNZIP))
 int  decrypt     ( struct Globals *pG );
#endif

#ifdef FUNZIP
   extern int encrypted;
#  ifdef NEXTBYTE
#    undef NEXTBYTE
#  endif
#  define NEXTBYTE (encrypted? update_keys( pG, getc(pG->in)^decrypt_byte( pG )) : getc(pG->in))
#endif /* FUNZIP */

#endif /* !__crypt_h */

