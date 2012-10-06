/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 * Port of util.c in ZIPDLL  I didn't like the MATCH.C for unzip.
 * match() is the function that we want, the rest is support.
 * iswild() is also used from outside this module.
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "unzip.h"

/* case mapping functions. case_map is used to ignore case in comparisons */
#define case_map( c ) upper[( c ) & 0xff]

/* Country-dependent case map table */
unsigned char upper[256]/*, lower[256]*/;

/* Local functions */
int   recmatch( unsigned char *, unsigned char * );
int   shmatch( unsigned char *, unsigned char * );
int   count_args( char * );
//char *isshexp( char * );
void  init_upper( void );


/* ===========================================================================
 * EWE NOTE: a shell expression is just a wildcard used in fspec.
 * If p is a sh expression, a pointer to the first special
 * character is returned.  Otherwise, NULL is returned.
	*p :: Candidate sh expression.
 */
//char *isshexp( char *p ) {
//	for ( ; *p; p++ ) {
//		if ( *p == '\\' && *(p + 1) ) p++;
//		else {	/* unix/dos wildcards */
//			if ( *p == '?' || *p == '*' || *p == '[' ) return p;
//		}
//	}
//	return NULL;  /* no wildcards found */
//}


/* ===========================================================================
	*p :: Candidate sh expression.
 */
int iswild( char *p ) {
	for ( ; *p; ++p ) {
		if ( *p == '\\' && *(p + 1) ) ++p;
		else if ( *p == '?' || *p == '*' || *p == '[' ) return true;
	}
	return false;
}


/* ===========================================================================
 * Recursively compare the sh pattern p, with the string s,
 * and return 1 if they match, and 0 or 2 if they don't (or if
 * there is a syntax error in the pattern).  This routine
 * recurses on itself no deeper than the number of characters
 * in the pattern.
	*p :: sh pattern to match (ex: *.*).
	*s :: String to match it to (ex: FNAME.DAT).
 */
static int recmatch( uch *p, uch *s ) {
	unsigned int c;       /* pattern char or start of range in [-] loop */

	/* Get first character, the pattern for new recmatch calls follows */
	c = *p++;

	/* If that was the end of the pattern, match if string empty too */
	if ( c == 0 ) return *s == 0;

	/* '?' matches any character (but not an empty string) */
	if ( c == '?' ) return *s ? recmatch( p, s + 1 ) : 0;

	/* '*' matches any number of characters, including no char's! */
	/* EWE: todo: for MS-DOS/Win make sure it won't match period! */
	if ( c == '*' ) {
		if ( *p == 0 ) return 1;
		for ( ; *s; s++ ) if ( (c = recmatch( p, s )) != 0 ) return (int)c;
		return 2;           /* 2 means give up -- shmatch will return false */
	}

	/* if escape ('\'), just compare next character */
	if ( c == '\\' ) if ( (c = *p++) == 0 )        /* if \ at end, then syntax error */
		return 0;

	/* Just a character--compare it */
	return case_map( c ) == case_map( *s ) ? recmatch( p, ++s ) : 0;
}


/* ===========================================================================
 * Compare the sh pattern p with the string s and return true(1)
 * if they match, false(0) if they don't or if there is a syntax
 * error in the pattern.
	*p :: sh pattern to match.
	*s :: String to match it to.
 * sample args:  p=*.*(fspec user wants)  s = TEST.DAT( next file in dir )
*/
int shmatch( unsigned char *p, unsigned char *s ) {
	int ret;

	ret = recmatch( p, s ) == 1;
	return( ret );
}


/* ===========================================================================
 * This is based on dosmatch() of ZIPDLL
 * Break the pattern and string into name and extension parts and match
 * each separately using shmatch().
	*s :: Filename to match against wildcard pattern.
	*p :: Dos wildcard pattern to match.
 */
int match( char *s, char *p ) {
	char *p1, *p2;             /* pattern sections  */
	char *s1, *s2;             /* string sections   */
	int   plen = lstrlen( p ); /* length of pattern */
	int   r, r1, r2;           /* result            */

	init_upper();

	/* EWE: convert MS-DOS slashes to Unix convention: */
	for ( p1 = s; p1 < s + lstrlen( s ); p1++ ) if ( *p1 == '\\' ) *p1 = '/';
	for ( p1 = p; p1 < p + plen; p1++ )        if ( *p1 == '\\' ) *p1 = '/';

	if ( (p1 = MALLOC( plen + 1 )) == NULL || (s1 = MALLOC( lstrlen( s ) + 1 )) == NULL ) {
		if ( p1 != NULL ) FREE( p1 );
		return 0;
	}

	lstrcpy( p1, p );  /* pattern to match */
	lstrcpy( s1, s );  /* string to match  */

	/* is there a period in pattern? */
	if ( (p2 = strrchr( p1, '.' )) != NULL ) *p2++ = '\0';  /* yes - wipe out extension */
	else if ( plen && p1[plen - 1] == '*' )  // if fname part ended in *
		p2 = "*";      /* put a zero in place of extension */
	else
		p2 = "";       /* no wild extension */

	/* is there a period in string? */
	if ( (s2 = strrchr( s1, '.' )) != NULL )
		*s2++ = '\0';
	else
		s2 = "";

	r1 = shmatch( (uch *)p2, (uch *)s2 );
	r2 = shmatch( (uch *)p1, (uch *)s1 );
	r = r1 && r2;

	FREE( p1 );
	FREE( s1 );
	return r;
}


/* ===========================================================================
 */
void init_upper() {
	int c;

	for ( c = 0; c < sizeof( upper ); c++ ) upper[c] = /*lower[c] = */ (uch)c;
	for ( c = 'a'; c <= 'z';          c++ ) upper[c] = (uch)(c - 'a' + 'A');
//	for ( c = 'A'; c <= 'Z';          c++ ) lower[c] = (uch)(c - 'A' + 'a');
}

