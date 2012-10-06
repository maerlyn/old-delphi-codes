/* ttyIO.h
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
 */

#ifndef __ttyio_h      /* don't include more than once */
#define __ttyio_h

#ifndef __crypt_h
#  include "crypt.h"  /* ensure that encryption header file has been seen */
#endif

#ifdef CRYPT

	/* The following systems supply a `non-echo' character input function "getch()"
	 * (or an alias) and do not need the echoff() / echon() function pair.
	 */
# define echoff( f )
# define echon()

# include <conio.h>
# define HAVE_WORKING_GETCH
  int getp( char *m, char *p, struct Globals *pG );

#endif /* ?CRYPT */

#endif /* !__ttyio_h */

