/* OSdep.h
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
*/

#ifndef MSDOS
	/* Windows 95 (and Windows NT) file systems are (to some extent)
	 *  extensions of MSDOS. Common features include for example:
	 *      FAT or (FAT like) file systems,
	 *      '\\' as directory separator in paths,
	 *      "\r\n" as record (line) terminator in text files, ...
	 */
#define MSDOS   /* Inherit MS-DOS file system etc. stuff. */
#endif

/* File operations--use "b" for binary if allowed */
#define FOPR "rb"
#define FOPM "r+b"
#define FOPW "wb"

#include <sys/types.h>
#include <sys/stat.h>
#include <io.h>
#include <malloc.h>

