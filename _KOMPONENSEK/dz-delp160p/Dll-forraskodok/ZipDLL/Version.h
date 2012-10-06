/* Version.h
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 */
#ifndef _VERSION_H
#define _VERSION_H

#define ZIPVERS 160
#define ZIPPRIVVERS 16018

#endif /* _VERSION_H */



#ifndef __revision_h
#define __revision_h

#define REVISION   22
#define PATCHLEVEL 0
#define VERSION    "ZIPDLL.DLL 1.60
#define REVDATE    "Mar 12, 2001"

#define COPYRIGHT_LINES 1

#ifdef NOCPYRT
extern const char *copyright[COPYRIGHT_LINES];
#else /* !NOCPYRT */

const char *copyright = {
"\r\nCopyright (C) 1990-2000 Mark Adler, Richard B. Wales, Jean-loup Gailly\r\n"
"Onno van der Linden, Kai Uwe Rommel, Mike White, and Eric W. Engler\r\n\r\n"
"Modified Win32 ZIP DLL by Chris Vleghert and Eric W. Engler\r\n"
"Send bug reports to cvleghrt@WorldOnline.nl\r\n"
"\r\n"
"Permission is granted to any individual or institution to use, copy, or\r\n"
"redistribute this DLL so long as it is not sold for profit. If this DLL\r\n"
"is modified in any way, credit must be given to the original authors\r\n"
"identified above in the copyright statement, and the new version must be\r\n"
"given a unique name and your own e-mail address for bug reports.\r\n"
"\r\n"
"Large portions of this DLL are based on code from the Info-Zip project.\r\n"
"Consult their home page for free source code of their official releases:\r\n"
"http://www.cdrom.com/pub/infozip/\r\n"
"\r\n"
"LIKE ANYTHING ELSE THAT'S FREE, THIS DLL AND ANY ASSOCIATED FILES AND\r\n"
"UTILITIES ARE PROVIDED AS-IS AND COME WITH NO WARRANTY OF ANY KIND, EITHER\r\n"
"EXPRESSED OR IMPLIED. IN NO EVENT WILL THE COPYRIGHT HOLDERS BE LIABLE FOR\r\n"
"ANY DAMAGES RESULTING FROM THE USE OF THIS SOFTWARE.\r\n"
""};
#endif /* !NOCPYRT */

#endif /* !__revision_h */
