/* Tailor.h -- Not copyrighted 1993 Mark Adler
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
 */
#include "osdep.h"

/*
 * case mapping functions. case_map is used to ignore case in comparisons,
 * to_up is used to force upper case.
 */
#define case_map( c ) pG->upper[(c) & 0xFF]
#define to_up( c )    pG->upper[(c) & 0xFF]

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>

typedef size_t extent;

/* System independent replacement for "struct utimbuf",
 * which is missing in many older OS environments.
 */
typedef struct ztimbuf {
    time_t actime;              /* new access time */
    time_t modtime;             /* new modification time */
} ztimbuf;

#ifdef S_IFLNK
#endif
#ifdef NO_SYMLINK
#endif



/* Some systems define S_IFLNK but do not support symbolic links */
#if defined(S_IFLNK) && defined(NO_SYMLINK)
#  undef S_IFLNK
#endif

/* Open the old zip file in exclusive mode if possible
 * (to avoid adding zip file to itself).
 */
#define FOPR_EX FOPR

/* MSDOS file attribute for directories */
#define MSDOS_DIR_ATTR 0x10

/* Define this symbol if your target allows access to unaligned data.
 * This is not mandatory, just a speed optimization. The compressed
 * output is strictly identical.
 */

#ifndef CBSZ
#  define CBSZ 16384 /* buffer size for copying files      */
#  define ZBSZ 16384 /* buffer size for temporary zip file */
#endif

#ifndef SSTAT
#  define SSTAT      stat
#endif

#ifdef S_IFLNK
#  define LSTAT      lstat
#  define LSSTAT( n, s )  (linkput ? lstat( (n), (s) ) : SSTAT( (n), (s) ))
#else
#  define LSTAT      SSTAT
#  define LSSTAT     SSTAT
#endif

/* The following OS code is defined in pkzip appnote.txt */
#define OS_CODE  0xB00
#define NUM_HOSTS 16
/* Number of operating systems. Should be updated when new ports are made */

#  define zcalloc( items, size ) \
          (void *)calloc( (unsigned)(items), (unsigned)(size) )
#  define zcfree   FREE

/* DBCS support for Info-ZIP's zip  (mainly for japanese (-: )
 * by Yoshioka Tsuneo (QWF00133@nifty.ne.jp,tsuneo-y@is.aist-nara.ac.jp)
 * This code is public domain!   Date: 1998/12/20
 */
#ifdef _MBCS
#   include <locale.h>

    /* Multi Byte Character Set */
    extern char *___tmp_ptr;
    unsigned char *zmbschr OF((ZCONST unsigned char *, unsigned int));
    unsigned char *zmbsrchr OF((ZCONST unsigned char *, unsigned int));
#   define CLEN(ptr) mblen(ptr, MB_CUR_MAX)
#   define PREINCSTR(ptr) (ptr += CLEN(ptr))
#   define POSTINCSTR(ptr) (___tmp_ptr=(char *)ptr,ptr += CLEN(ptr),___tmp_ptr)
    int lastchar OF((ZCONST char *ptr));
#   define MBSCHR(str,c) (char *)zmbschr((ZCONST unsigned char *)(str), c)
#   define MBSRCHR(str,c) (char *)zmbsrchr((ZCONST unsigned char *)(str), (c))
#   define SETLOCALE(category, locale) setlocale(category, locale)
#else /* !_MBCS */
#   define CLEN( ptr ) 1
#   define PREINCSTR( ptr ) (++(ptr))
#   define POSTINCSTR( ptr ) ((ptr)++)
#   define lastchar( ptr ) ((*(ptr) == '\0') ? '\0' : ptr[ lstrlen( ptr )- 1 ])
#   define MBSCHR( str, c ) strchr( str, c )
#   define MBSRCHR( str, c ) strrchr( str, c )
#   define SETLOCALE( category, locale )
#endif /* ?_MBCS */
#define INCSTR( ptr ) PREINCSTR( ptr )

