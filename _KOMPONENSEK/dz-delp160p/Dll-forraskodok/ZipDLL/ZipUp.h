/* ZipUp.h
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 */

#include <share.h>

#define fhow            (O_RDONLY | O_BINARY)
#define fbad            (-1)

typedef int ftype;

#define zopen( n, p )    sopen( n, p, SH_DENYNO )
#define zread( f, b, n ) read( f, b, n )
#ifdef USE_STRM_INPUT
#  define zclose( f )    if ( f ) close( f )
#else
#  define zclose( f )    close( f )
#endif
#define zerr( f )        (k == (extent)(-1L))
#define zstdin           0


