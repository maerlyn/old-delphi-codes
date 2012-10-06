/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Mar 11, 1998.
 * I removed TTYIO.C completely!  We still need this file.
 * ttyio.h
 */

#ifndef __ttyio_h   /* don't include more than once */
#define __ttyio_h

#ifndef __crypt_h
#  include "crypt.h"  /* ensure that encryption header file has been seen */
#endif

#if (defined(CRYPT) || (defined(UNZIP) && !defined(FUNZIP)))
/*
 * Non-echo keyboard/console input support is needed and enabled.
 */

#if (defined(VM_CMS) || defined(MVS))
#  ifndef CMS_MVS
#    define CMS_MVS
#  endif
#endif


/* Function prototypes */

/* The following systems supply a `non-echo' character input function "getch()"
 * (or an alias) and do not need the echoff() / echon() function pair.
 */
#  define echoff(f)
#  define echon()
#  ifdef __EMX__
#    define getch() _read_kbd(0, 1, 0)
#  else /* !__EMX__ */
#    ifdef __GO32__
#      include <pc.h>
#      define getch() getkey()
#    else /* !__GO32__ */
#      include <conio.h>
#    endif /* ?__GO32__ */
#  endif /* ?__EMX__ */
#  define HAVE_WORKING_GETCH

/* For all other systems, ttyio.c supplies the two functions Echoff() and
 * Echon() for suppressing and (re)enabling console input echo.
 */
#ifndef echoff
#  define echoff(f)  Echoff( pG, f )
#  define echon()    Echon( pG )
   void Echoff OF(( struct Globals *pG, int f ));
   void Echon OF(( struct Globals *pG  ));
#endif

/* this stuff is used by MORE and also now by the ctrl-S code; fileio.c only */
#if (defined(UNZIP) && !defined(FUNZIP))
#  ifdef HAVE_WORKING_GETCH
#    define FGETCH(f)  getch()
#  endif
#  ifndef FGETCH
     /* default for all systems where no getch()-like function is available */
     int zgetch OF(( struct Globals *pG, int f ));
#    define FGETCH(f)  zgetch( gP, f )
#  endif
#endif /* UNZIP && !FUNZIP */

#ifdef CRYPT
   char *getp( struct Globals *pG, char *m, char *p, int n );
#endif

#else /* !(CRYPT || (UNZIP && !FUNZIP)) */
/*
 * No need for non-echo keyboard/console input; provide dummy definitions.
 */

#define echoff(f)
#define echon()

#endif /* ?(CRYPT || (UNZIP && !FUNZIP)) */

#endif /* !__ttyio_h */

