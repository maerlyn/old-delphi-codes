/* Util.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko,
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
*/

#include "zip.h"
#include "Globals.h"
#include <ctype.h>

/* Local functions */
static int recmatch( uch *, uch *, struct Globals *pG );


/* ===========================================================================
 * EWE NOTE: a shell expression is just a wildcard used in fspec.
 * If p is a sh expression, a pointer to the first special
 * character is returned.  Otherwise, NULL is returned.
	*p :: Candidate sh expression.
 */
char *IsShExp( char *p ) {
	for ( ; *p; p++ ) {
		/* dos wildcards { V1.4 removed unix '[' } */
		if ( *p == '?' || *p == '*' ) return p;
	}
	return NULL;  /* no wildcards found */
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
static int recmatch( uch *p, uch *s, struct Globals *pG ) {
	unsigned int c;       /* pattern char or start of range in [-] loop */

	/* Get first character, the pattern for new recmatch calls follows */
	c = *p++;

	/* If that was the end of the pattern, match if string empty too */
	if ( c == 0 ) return *s == 0;

	/* '?' matches any character (but not an empty string) */
	if ( c == '?' ) return *s ? recmatch( p, s + 1, pG ) : 0;

	/* '*' matches any number of characters, including no char's! */
	/* EWE: todo: for MS-DOS/Win make sure it won't match period! */
	if ( c == '*' ) {
		if ( *p == 0 ) return 1;
		for ( ; *s; s++ ) if ( (c = recmatch( p, s, pG )) != 0 ) return (int)c;
		return 2;           /* 2 means give up -- shmatch will return false */
	}

	/* if escape ('\'), just compare next character */
//	if ( c == '\\' ) if ( (c = *p++) == 0 )        /* if \ at end, then syntax error */
//		return 0;

	/* Just a character--compare it */
	return case_map( c ) == case_map( *s ) ? recmatch( p, ++s, pG ) : 0;
}

/* ===========================================================================
 * Compare the sh pattern p with the string s and return true(1)
 * if they match, false(0) if they don't or if there is a syntax
 * error in the pattern.
	*p :: sh pattern to match.
	*s :: String to match it to.
 * sample args:  p=*.*(fspec user wants)  s = TEST.DAT( next file in dir )
*/
int shmatch( char *p, char *s, struct Globals *pG ) {
	int ret;

	ret = recmatch( (uch *)p, (uch *)s, pG ) == 1;
//	sprintf( ewemsg, "in shmatch of util.c, pat=%s, str=%s, return=%d", p, s, ret );
//	diag( ewemsg );
	return( ret );
}

/* ===========================================================================
 * Break the pattern and string into name and extension parts and match
 * each separately using shmatch().
	*p :: Dos pattern to match.
	*s :: String to match it to.
 */
int dosmatch( char *p, char *s, struct Globals *pG ) {
	char *p1, *p2;             /* Pattern sections.  */
	char *s1, *s2;             /* String sections.   */
	int   plen = lstrlen( p ); /* Length of pattern. */
	int   r;                   /* Result.            */

	//	sprintf( ewemsg, "in dosmatch of util.c, dos pattern=%s, string=%s", p, s );
	//	diag( ewemsg );

	if ( (p1 = MALLOC( plen + 1 )) == NULL || (s1 = MALLOC( lstrlen( s ) + 1 ) ) == NULL ) {
		if ( p1 != NULL ) FREE( p1 );
		return 0;
	}
	lstrcpy( p1, p );  /* pattern to match */
	lstrcpy( s1, s );  /* string to match  */

	/* is there a period in pattern? */
	if ( (p2 = strrchr( p1, '.' )) != NULL )
		*p2++ = '\0';			/* yes - wipe out extension                  */
	else {
		if ( plen && p1[plen - 1] == '*' )  /* if fname part ended in '*' */
			p2 = "*";			/* put a asterix in place of extension       */
		else p2 = "";			/* no Wild extension                         */
	}
	/* is there a period in string? */
	if ( (s2 = strrchr( s1, '.' )) != NULL )
		*s2++ = '\0';
	else
		s2 = "";

	r = shmatch( p1, s1, pG ) && shmatch( p2, s2, pG );
	//	sprintf( ewemsg, "in dosmatch of util.c, results=%d %d", shmatch( p1, s1 ), shmatch( p2, s2 ) );
	//	diag( ewemsg );

	FREE( p1 );
	FREE( s1 );
	return r;
}

/* ===========================================================================
 * Search for b in the pointer list a[0..n-1] using the compare function
 * cmp(b, c) where c is an element of a[i] and cmp() returns negative if
 * *b < *c, zero if *b == *c, or positive if *b > *c.  If *b is found,
 * search returns a pointer to the entry in a[], else search() returns
 * NULL.  The nature and size of *b and *c (they can be different) are
 * left up to the cmp() function.  A binary search is used, and it is
 * assumed that the list is sorted in ascending order.
	 *b   :: Pointer to value to search for.
	**a   :: Table of pointers to values, sorted.
	  n   :: Number of pointers in a[].
	 *cmp :: Comparison function.
 */
typedef int(*CmpFunc)( const void *, const void * );
void **search( void *b, void **a, extent n, CmpFunc cmp ) {
	void **i;        /* pointer to midpoint of current range  */
	void **l;        /* pointer to lower end of current range */
	void **u;        /* pointer to upper end of current range */
	int    r;        /* result of (*cmp)() call */

	l = (void **)a;
	u = l +(n - 1);
	while ( u >= l ) {
		i = l +( (unsigned)(u - l) >> 1 );
		if ( (r = (*cmp)(b, (char *)*(struct zlist **)i )) < 0 )
			u = i - 1;
		else if ( r > 0 )
			l = i + 1;
		else
		return (void **)i;
	}
	return NULL;          /* If b were in list, it would belong at l */
}

/* ===========================================================================
 */
void init_upper( struct Globals *pG ) {
	int c;

	for ( c = 0; c < sizeof( pG->upper ); c++ ) pG->upper[c] = (uch)c;
	for ( c = 'a'; c <= 'z';          c++ ) pG->upper[c] = (uch)(c - 'a' + 'A');
}

/* ===========================================================================
 * Compare the two strings ignoring case, and correctly taking into
 * account national language characters.
 * If equal return 0 else != 0.
 */
int namecmp( char *string1, char *string2 ) {
	int             Error;
	struct Globals *pG = GetGlobalPointer( &Error );

	if ( pG ) for (;;) {
		int d = (int)(uch)case_map( *string1 ) - (int)(uch)case_map( *string2 );

		if ( d || *string1 == 0 || *string2 == 0 ) return d;

		string1++;
		string2++;
	}
	return 1;
}

