/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 */
/*---------------------------------------------------------------------------
 *  MS-DOS specific configuration section:
 *---------------------------------------------------------------------------*/

#ifndef __doscfg_h
#define __doscfg_h

#include <dos.h>           /* for REGS macro (TC) or _dos_setftime (MSC) */
#include <sys/timeb.h>		/* for structure ftime */
#include <utime.h>

#ifdef __EMX__
#  ifndef __32BIT__
#    define __32BIT__
#  endif
#  define far
#endif

#define EXE_EXTENSION ".exe"  /* OS/2 has GetLoadPath() function instead */

#define NOVELL_BUG_WORKAROUND   /* another stat()/fopen() bug with some 16-bit
                                 *  compilers on Novell drives; very dangerous
                                 *  (silently overwrites executables in other
                                 *  directories)  */
#define NOVELL_BUG_FAILSAFE     /* enables additional test & message code
                                 *  that directs UnZip to fail safely in case
                                 *  the "workaround" enabled above does not
                                 *  work as intended.  */

#endif /* !__doscfg_h */

