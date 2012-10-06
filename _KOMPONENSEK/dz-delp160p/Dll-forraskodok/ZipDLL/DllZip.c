/* DLLzip.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 */

#include "zip.h"
#include "Globals.h"
#include "crypt.h"
#include "ttyio.h"
#include "ziperr.h"

#include <process.h>
#include <signal.h>

/* Local option flags */
#define PURGE		 0		/* RCV Changed: was DELETE. (delete is also a function) */
#define ADD        1
#define UPDATE     2
#define FRESHEN    3


/* Local functions */
static void freeup( struct Globals *pG );
static void finish( struct Globals *pG );
// RCV: 1.605 static void GetFilters( int argc, char **argv, struct Globals *pG );
static void SetExclFilters( struct Globals *pG );	// RCV: 1.605
static char *Oem2Iso( char *InFileName, struct Globals *pG );
int         zipmain( int, char **, struct Globals *pG );


/* ===========================================================================
 * Free all allocations in the found list, the zfiles list and the excluded
 * file list.
 */
static void freeup( struct Globals *pG ) {
	struct flist *f;	/* steps through found list				 */
	struct zlist *z;	/* pointer to next entry in zfiles list */
	int			  i;

	for ( f = pG->found; f != NULL; f = fexpel( f, pG ) );

	while ( pG->zfiles != NULL ) {
		z = pG->zfiles->nxt;
		FREE( pG->zfiles->name );
		if ( pG->zfiles->zname != pG->zfiles->name ) FREE( pG->zfiles->zname );
		if ( pG->zfiles->ext ) FREE( pG->zfiles->extra );
		if ( pG->zfiles->cext && pG->zfiles->cextra != pG->zfiles->extra )
			FREE( pG->zfiles->cextra );
		if ( pG->zfiles->com ) FREE( pG->zfiles->comment );
		FREE( pG->zfiles );
		pG->zfiles = z;
		pG->zcount--;
	}

	if ( pG->patterns ) {
		for ( i = 0; i < pG->pcount; i++ )
			if ( pG->patterns[i].zname )
				FREE( pG->patterns[i].zname );
		FREE( pG->patterns );
	}
}


/* ===========================================================================
 * Process -o and -m options (if specified), free up malloc'ed stuff, and
 * exit with the code e.
	e :: Exit code.
*/
static void finish( struct Globals *pG ) {
	int r;            /* return value from trash() */
	ulg t;            /* latest time in zip file   */
	struct zlist *z;  /* pointer into zfile list   */

	/* If latest, set time to zip file to latest file in zip file */
	if ( pG->latest && pG->zipfile && strcmp( pG->zipfile, "-" ) ) {
		diag( "changing time of zip file to time of latest file in it", pG );
		/* find latest time in zip file */
		if ( pG->zfiles == NULL )
			zipwarn( "zip file is empty, can't make it as old as latest entry", "" );
		else {
			t = 0;
			for( z = pG->zfiles; z != NULL; z = z->nxt )
			/* Ignore directories in time comparisons */
#ifdef USE_EF_UX_TIME
			if ( z->zname[z->nam - 1] != '\\' ) {	// SLASH
				ztimbuf z_utim;
				ulg z_tim;

				z_tim = (get_ef_ux_ztime( z, &z_utim ) ? unix2dostime( &z_utim.modtime ) : z->tim);
				if ( t < z_tim ) t = z_tim;
			}
#else
			if ( z->zname[z->nam - 1] != '\\' && t < z->tim ) t = z->tim;	// SLASH
#endif
			/* set modified time of zip file to that time */
			if ( t != 0 ) stamp( pG->zipfile, t );
			else zipwarn( "zip file has only directories, can't make it as old as latest entry", "" );
		}
	}
	if ( pG->tempath != NULL ) {
		FREE( pG->tempath );
		pG->tempath = NULL;
	}
	if ( pG->zipfile != NULL ) {
		FREE( pG->zipfile );
		pG->zipfile = NULL;
	}

	/* If dispose, delete all files in the zfiles list that are marked */
	if ( pG->dispose && !pG->global_abort_sw && !pG->global_abort_sw ) { /* v1.6017 */
		diag( "deleting files that were added to zip file", pG );
		if ( (r = trash( pG )) != ZEN_OK ) ziperr( r, pG );
		return;
	}
#	ifdef CRYPT		// RCV: 1.604 added
	if ( pG->key ) {
		FREE( pG->key );
		pG->key = NULL;
	}
#	endif
	/* Done! */
	freeup( pG );
}


/* ===========================================================================
 * Issue a message for an error, clean up files and memory, and exit.
 * return value is the the same as 'c'.
	c :: Error code from the ZEN_ class.
 */
int ziperr( int c, struct Globals *pG ) {
	char errmsg[ ERRORLENGTH ];
//	int k = 0;            /* keep count for end header */
//	ulg cb = pG->cenbeg;      /* get start of central */
//	struct zlist *z;  /* steps through zfiles linked list */

	if ( pG->user_notified_of_abort ) return c;
	pG->user_notified_of_abort = 1;

	pG->global_error_code = c;		/* Last error code */
	pG->global_abort_sw = true;	/* RCV Added; if we are here it's serious we can't continue. */

	if ( c != ZEN_OK ) lstrcpy( errmsg, errors[(c & 0xFF0000) >> 16] );

	if ( pG->dll_handles_errors )  {
		/* I'm passing the error via the callback just to get it logged in
		 * the status box - I'm sending it in with a 0 error code to avoid
		 * a dialog box from the application program.
		 */
		user_callback( 4, (pG->global_handle) ? 0 : c, 0, errmsg, pG );
		sprintf( pG->ewemsg, "%s   code=%8.8X", errmsg, c );
		msgbox( pG->ewemsg, pG );
	} else user_callback( 4, c, 0, errmsg, pG );  /* applic pgm handles errors (showing) */

	freeup( pG );  // free memory for file linked lists, etc.

	if ( pG->tempzip != pG->zipfile ) {
		if ( pG->tempzf != NULL ) fclose( pG->tempzf );
		pG->tempzf = NULL;
		destroy( pG->tempzip );
		FREE( pG->tempzip );
	}
	if ( pG->zipfile != NULL ) {
		FREE( pG->zipfile );
		pG->zipfile = NULL;
	}
	if ( pG->tempath != NULL ) {
		FREE( pG->tempath );
		pG->tempath = NULL;
	}
 	if ( pG->tempzf != NULL ) {
 		fclose( pG->tempzf );
 		pG->tempzf = NULL;
 	}
	if ( pG->x != NULL ) {
		fclose( pG->x );
		pG->x = NULL;
	}
	if ( pG->y != NULL ) {
		fclose( pG->y );
		pG->y = NULL;
	}
#ifdef CRYPT
	if ( pG->key ) {
		FREE( pG->key );	// RCV: 1.604 added
		pG->key = NULL;
	}
#endif

#ifdef NEVER
// EWE NOTE: This code is problematic!!!
	/* -g option, attempt to restore the old file */
//	int k = 0;            /* keep count for end header */
//	ulg cb = cenbeg;      /* get start of central */
//	struct zlist *z;  /* steps through zfiles linked list */

	printf( "attempting to restore %s to its previous state\n", pG->zipfile );
	fseek( pG->tempzf, pG->cenbeg, SEEK_SET );
	pG->tempzn = pG->cenbeg;
	for ( z = pG->zfiles; z != NULL; z = z->nxt ) {
		putcentral( z, pG->tempzf, pG );
		pG->tempzn += 4 + CENHEAD + z->nam + z->cext + z->com;
		k++;
	}
	putend( k, pG->tempzn - cb, cb, pG->zcomlen, pG->zcomment, pG->tempzf );
	fclose( pG->tempzf );
	pG->tempzf = NULL;

	freeup( pG );  // free memory for file linked lists, etc.
	if ( pG->zipfile != NULL ) {
		FREE( pG->zipfile );
		pG->zipfile = NULL;
	}
#endif
	return c;
}

#if defined( DEBUG ) || defined( DYN_ALLOC )
/* ===========================================================================
 * Internal error, should never happen.
 */
void error( char *h, struct Globals *pG ) {
	diag( h, pG );
	ziperr( ZEN_LOGIC04, pG );
}
#endif

/* ===========================================================================
 * Print a warning message and return.
 */
void zipwarn( char *a, char *b ) {
	printf( "%s: %s\n", a, b ); /* Combine both messages. */
}


/* ===========================================================================
 * Counts number of -i or -x patterns, sets patterns, pcount and icount. The
 * first time counts the number of patterns, the second time the patterns are
 * stored.
	  argc :: Number of tokens in command line.
	**argv :: Command line tokens.
*/
/*RCV: 1.605 static void GetFilters( int argc, char **argv, struct Globals *pG ) {
	int i;
	int flag = 0;

	pG->pcount = 0;
	for ( i = 1; i < argc; i++ ) {
		if ( argv[i][0] == '-' ) {
			if ( strrchr( argv[i], 'i' ) ) {
				flag = 'i';
			} else if ( strrchr( argv[i], 'x' ) ) {
				flag = 'x';
			} else
				flag = 0;
		} else if ( flag ) {
			if ( pG->patterns ) {
				pG->patterns[pG->pcount].zname  = ex2in( argv[i], (int *)NULL, pG );
				pG->patterns[pG->pcount].select = flag;
				if ( flag == 'i' ) pG->icount++;
			}
			pG->pcount++;
		}
	}
	if ( !pG->pcount || pG->patterns ) return;
	if ( (pG->patterns = (struct plist *)MALLOC( pG->pcount * sizeof( struct plist ) )) == NULL ) {
		ziperr( ZEN_MEM05, pG );
		return;
	}
	GetFilters( argc, argv, pG );
}*/

/* ===========================================================================
 * RCV 1.605 Added
*/
static void SetExclFilters( struct Globals *pG ) {
	register int i;

	if ( pG->pcount ) {
		if ( (pG->patterns = (struct plist *)MALLOC( pG->pcount * sizeof( struct plist ) )) == NULL ) {
			ziperr( ZEN_MEM05, pG );
			return;
		}
		for ( i = 0; i < pG->pcount; i++ ) {
			register char *p = pG->ExternalList[i].zname;	// Replace forward slashes.
			if ( p ) while( *p ) if (*p++ == '/') *(p - 1) = '\\';
			pG->patterns[i].zname = ex2in( pG->ExternalList[i].zname, NULL, pG );
		}
	}
}

/*static void SendDebugInfo( char *what ) {
   SYSTEMTIME SystemT;
   char       msg[ 256 ];

   GetSystemTime( &SystemT );
   sprintf( msg, "%s Uur %d, min %d, sec %d msec %d\n", what, SystemT.wHour, SystemT.wMinute, SystemT.wSecond, SystemT.wMilliseconds );
  	OutputDebugString( msg );
}*/


/* ===========================================================================
 * Add, update, freshen, or delete zip entries in a zip file.
	  argc;       /* Number of tokens in command line.
	**argv;       /* Command line tokens.
*/
int zipmain( int argc, char **argv, struct Globals *pG ) {
	int             a;          /* attributes of zip file						 */
	ulg             c;          /* start of central directory				 */
	int             d;          /* true if just adding to a zip file		 */
	struct flist   *f;          /* steps through "found" linked list		 */
	int             i;          /* arg counter, root directory flag		 */
	int             k;          /* next argument type, marked counter,    *
                                * comment size, entry count					 */
	ulg             n;          /* total of entry len's						 */
	// int          o;          /* true if there were any ZEN_OPEN errors */
	char           *p;          /* steps through option arguments			 */
	int             r;          /* temporary variable							 */
	ulg             t;          /* file time, length of central directory */
	int             first_listarg = 0;  /* index of first arg of "process *
                                        * these files" list					 */
	struct zlist   *v;          /* temporary variable							 */
	struct zlist  **w;          /* pointer to last link in "zfiles" list	 */
	struct zlist   *z;          /* steps through "zfiles" linked list		 */
	unsigned long	 TotFiles = 0;
	unsigned long	 TotSize  = 0;

#ifdef CRYPT
	int             pwrcode;
	char           *ekey = NULL; /* Double check password						 */
#endif

	if ( (p = getenv( "TZ" )) == NULL || *p == '\0' )
		pG->extra_fields = 0;       /* disable storing "Unix" time stamps */

	SetExclFilters( pG );

	/* Process arguments */
	diag( "processing arguments", pG );

	d = 0;   /* disallow appending to a zip file */
	k = 0;   /* Next non-option argument type (zip filename by dflt) */

	// skip over program name in argv[0]
	for ( i = 1; i < argc; i++ ) {
		if ( (argv[i] == NULL) || (argv[i][0] == 0) ) {
			diag( "breaking from outer loop", pG );
			break;
		}
		if ( argv[i][0] == '-' ) {
			// found dash that likely preceeds a switch

			if ( argv[i][1] ) {   /* if there is a char following the dash */
				for ( p = argv[i] + 1; *p; p++ ) {
					sprintf( pG->ewemsg, "%c switch found", *p );
					diag( pG->ewemsg, pG );
					switch ( *p ) {
						case '0':
							pG->method = STORE;
							pG->level = 0;
							break;
						case '1':
						case '2':
						case '3':
						case '4':
						case '5':
						case '6':
						case '7':
						case '8':
						case '9':
							/* Set the compression level */
							pG->level = *p - '0';
							sprintf( pG->ewemsg, "setting compression level to %d", pG->level );
							diag( pG->ewemsg, pG );
							break;
						case 'A':  /* Adjust unzipsfx'd zipfile: adjust offsets only */
							pG->adjust = 1;
							break;
						case 'b':   /* Specify path for temporary file */
							pG->tempdir = 1;
							/* if k isn't 0, then we have already parsed the zip filename */
							if ( k != 0 ) return( ziperr( ZEN_PARMS03, pG ) );
							else k = 1;      /* Next non-option is path */
							break;
						case 'd':   /* Delete files from zip file */
							if ( pG->action != ADD ) return( ziperr( ZEN_PARMS04, pG ) );
							pG->action = PURGE;
							break;
						case 'D':   /* Do not add directory entries */
							pG->dirnames = 0;
							break;
						case 'E':   /* Do NOT Encrypt, NEW v1.607, this allows us to specify multiple encryption options on one command line */
#                    ifndef CRYPT
							return( ziperr( ZEN_PARMS05 ) );
#                    else     /* CRYPT */
							if ( pG->key ) {
								FREE( pG->key );
								pG->key = NULL;
								pG->key_len = 0;
							}
#                    endif    /* ?CRYPT */
							break;
						case 'e':   /* Encrypt */
#                    ifndef CRYPT
							return( ziperr( ZEN_PARMS05 ) );
#                    else   /* CRYPT */
							/* did user passed a password to DLL? then DllMain.c did the malloc for it */
							if ( pG->key ) break;
							/* allocate memory for the password, and point the global password pointer to it */
							diag( "ZIPDLL was not passed a password", pG );
							if ( (pG->key = MALLOC( PWLEN + 1 )) == NULL ) return( ziperr( ZEN_MEM06, pG ) );
							if ( pG->GlobalCompVersion < 160 ) {
								pwrcode = getp( "Enter password: ", pG->key, pG );
								switch( pwrcode ) {
									case ZEN_PW_CANCELALL:
										return( ziperr( ZEN_PW_CANCELALL, pG ) );
									case ZEN_PW_ERROR:
										return( ziperr( ZEN_PW_ERROR, pG ) );
									case ZEN_PW_ENTERED:
										pG->key[ pG->key_len ] = '\0';
										diag( "password entered", pG );
								}

								if ( (ekey = MALLOC( PWLEN + 1 )) == NULL ) return( ziperr( ZEN_MEM07, pG ) );
								pwrcode = getp( "Verify password:", ekey, pG );
								switch( pwrcode ) {
									case ZEN_PW_CANCELALL:
										FREE( ekey );
										return( ziperr( ZEN_PW_CANCELALL, pG ) );
									case ZEN_PW_ERROR:
										FREE( ekey );
										return( ziperr( ZEN_PW_ERROR, pG ) );
									case ZEN_PW_ENTERED:
										ekey[ pG->key_len ] = '\0';
										diag( "2nd copy of password entered", pG );
								}
								r = strcmp( pG->key, ekey );
								FREE( ekey );
								if ( r ) {
									diag( "passwords do not match", pG );
									return( ziperr( ZEN_PW_ERROR, pG ) ); // pwds did NOT match
								}
								if ( *pG->key == '\0' ) {
									diag( "no passwords entered: only CR", pG );
									return( ziperr( ZEN_PW_ERROR, pG ) ); // no pwds entered
								}
							} else {
								user_callback( 8, 1, 1, "", pG );
								if ( pG->callbackdata.error_code ) {
									lstrcpy( pG->key, pG->callbackdata.filenameormsg );
									pG->key_len = lstrlen( pG->key );
								} else return( ziperr( ZEN_PW_CANCELALL, pG ) );
							}
							sprintf( pG->ewemsg, "passwords match: %s", pG->key );
							diag( pG->ewemsg, pG );
#                    endif    /* ?CRYPT */
							break;

						case 'f':   /* Freshen zip file--overwrite only */
							if ( pG->action != ADD ) return( ziperr( ZEN_PARMS06, pG ) );
							pG->action = FRESHEN;
							break;
						case 'g':   /* Allow appending to a zip file */
							d = 1;
							break;
						case 'j':   /* Junk (don't save) directory names */
							pG->pathput = 0;
							break;
						case 'J':   /* Junk (ignore) sfx prefix */
							pG->junk_sfx = 1;
							break;
						case 'k':   /* Make entries using DOS names (8x3) */
							pG->dosify = 1;
							break;
						case 'l':   /* Translate end-of-line */
							pG->translate_eol++;
							break;
						case 'm':   /* Delete orig files added or updated in zip file */
							pG->dispose = 1;
							break;
//	v1.6 removed	case '!':   /* DO try to compress files with a special suffix */
//							pG->special = NULL;           /* this is oppos of orig -n */
//							break;
						case 'o':   /* Set zip file time to time of latest file in it */
							pG->latest = 1;
							break;
						case 'P':	/* Want new password form command line, NEW DLL v1.607, Component v1.60L */
							k = 2;	/* This permits multiple password given by the component on a filespec basis */
							break;
						case 'p':   /* Store path with name           */
							break;   /* do nothing - this is default   */
						case 'q':   /* Quiet operation                */
							pG->noisy   = 0;    /* shut us up! */
							pG->verbose = 0;    /* override verbose option */
							break;
						case 'R':				  /* Do NOT recurse into subdirectories */
							pG->recurse = 0;    /* NEW v1.607, this allows us to specify multiple recurse options on one command line */
							break;
						case 'r':				  /* Recurse into subdirectories        */
							pG->recurse = 1;    /* we won't necessarily save dirnames */
							break;
						case 'S':
							pG->hidden_files = 1;
							break;
						case 't':   /* Exclude files earlier than specified date */
							if ( pG->before ) return( ziperr( ZEN_PARMS07, pG ) );
							k = 3;  /* look for a date */
							break;
						case 'u':   /* Update zip file--overwrite only if newer */
							if ( pG->action != ADD ) return( ziperr( ZEN_PARMS08, pG ) );
							pG->action = UPDATE;
							break;
						case 'v':   /* Verbose messages */
							pG->noisy = 1;
							pG->verbose++;
							break;
						case '$':   /* Include volume label */
							pG->volume_label = 1;
							break;
						case 'X':
							pG->extra_fields = 0;
							break;
						default:
							sprintf( pG->errbuf, "no such option: %02X hex", *p );
							return( ziperr( ZEN_PARMS09, pG ) );
					}  /* END switch/case  */
				}  /* end of inner for */
			} else {
				/* just a dash */
				switch ( k ) {
					case 0:   // need name of zipfile
					case 1:   // need name of tempdir
					case 2:   // need password
					case 3:   // need cutoff date
						return( ziperr( ZEN_PARMS10, pG ) );
					case 4:   // need arg filename
						// "procname" and "Wild" are in win32zip.c
						// for ADD, Wild is used
						if ( pG->action == ADD || pG->action == UPDATE )
							r = Wild( argv[i], pG );
						else
							r = procname( argv[ i ], pG->recurse, pG );
						if ( r != ZEN_OK ) {
							/* Below occurs if no file already in zip
							 * archive was matched by the fspec */
							if ( (int)(char)(r & 0xFF) == ZEN_MISS ) zipwarn( "Not found: ", argv[ i ] );
							else {
								diag( "this can't happen", pG );
								return( ziperr( r, pG ) );
							}
						}
				}  /* end switch */
			}    /* end inner for */
		} else { /* no dash was found in this arg */
			diag( "reached \"no dash was found\"; checking for zip name or fname", pG );

			/* k controls the simulated state machine
			 * k = 0   get spec of zip filename
			 * k = 1   get spec for tempdir
			 * k = 2   get next password for multiple password support NEW DLL 1.607, Component v1.60L
			 *         new 'P' option triggers this.
			 * k = 3   get spec for cutoff date
			 * k = 4   get spec of first arg filename
			 * k = 5   get specs of 2nd thru "n" arg filenames
			 */
			switch ( k ) {
				case 0:
					diag( "k was 0, need zipfname", pG );
					/* in this state, we need the name of the zip file */
					if ( (pG->zipfile = ziptyp( argv[ i ], pG )) == NULL ) return( ziperr( ZEN_MEM08, pG ) );
					diag( "ready to read zip file", pG );
					/* the read will be done in file: zipfile.c */

					if ( (r = readzipfile( pG )) != ZEN_OK ) {
						diag( "err returned from \"readzipfile\"", pG );
						return( ziperr( r, pG ) );
					}
					/* change to state 4: looking for arg filenames */
					k = 4;
					break;
				case 1:
					diag( "k was 1", pG );  // get tempdir spec
					if ( (pG->tempath = MALLOC( lstrlen( argv[ i ] ) + 1 )) == NULL )
						return( ziperr( ZEN_MEM09, pG ) );
					lstrcpy( pG->tempath, argv[ i ] );
					k = 0;  // tempdir spec OK, get zipfname
					break;
#					ifdef CRYPT
				case 2:	// NEW Dll 1.607, Component v1.60L
					diag( "k was 2", pG );  // get next password from command line
					if ( !pG->key && (pG->key = MALLOC( PWLEN + 1 )) == NULL )
						return( ziperr( ZEN_MEM35, pG ) );
					lstrcpy( pG->key, argv[ i ] );
					pG->key_len = lstrlen( pG->key );
					k = 4;  // Password OK, get next arg fname
					break;
#					endif
				case 3: { // get cutoff date
					int  yy, mm, dd;        /* results of sscanf() */

					diag( "k was 3", pG );
					if ( sscanf( argv[ i ], "%2d%2d%2d", &mm, &dd, &yy) != 3 || mm < 1 || mm > 12 || dd < 1 || dd > 31 )
						return( ziperr( ZEN_PARMS11, pG ) );
					// No "year 2000" problem here!
					pG->before = dostime( yy + (yy < 80 ? 2000 : 1900), mm, dd, 0, 0, 0 );
					k = 0;  // get zipfname
					break;
				}
				case 4:	// Changed v1.607 to accomodate case 2
					if ( !first_listarg ) {
						diag( "k was 4", pG );
						first_listarg = i;	// the first fname's argv index
					}
#					ifdef USE_STRM_INPUT
					if ( pG->UseInStream ) break;
#					endif
					k = 5;	// look for arg fnames 2..n */
				case 5:
					diag( "k was 5, look for next arg fnames", pG );
					if ( (pG->action == ADD) || (pG->action == UPDATE) )
						r = Wild( argv[ i ], pG );
					else
						r = procname( argv[ i ], pG->recurse, pG );
					if ( r != ZEN_OK ) {
						if ( (int)(char)(r & 0xFF ) == ZEN_MISS ) {
							/* this occurs if new file wasn't found */
							sprintf( pG->ewemsg, "File specification \"%s\" skipped", argv[ i ] );
							user_callback( 4, r, 0, pG->ewemsg, pG );
						} else return( ziperr( r, pG ) );
					}
         }  /* end switch( k ) */
		} /* end of "no dash" processing */
	}  /* end of outer "for" loop */

	switch ( pG->action ) {
		case ADD:     diag( "action = ADD", pG );     break;
		case UPDATE:  diag( "action = UPDATE", pG );  break;
		case FRESHEN: diag( "action = FRESHEN", pG ); break;
		case PURGE:   diag( "action = PURGE", pG );
	}

	/* zcount is no. of entries in linked-list */
	/* zfiles is name of the linked-list of filenames for the archive */
	sprintf( pG->ewemsg, "zcount=%d (no. of files in ZIP already)", pG->zcount );
	diag( pG->ewemsg, pG );

	/* Clean up selections ("4 <= k <= 6" now) */
	/* if first_listarg is 0, then we didn't got any fspecs on cmd line */
	if ( k != 5 && first_listarg == 0 && (pG->action == UPDATE || pG->action == FRESHEN) ) {
		/* if -update or -freshen with no args, do all, but, when present, apply filters */
		for ( z = pG->zfiles; z != NULL; z = z->nxt ) {
			z->mark = pG->pcount ? filter( z->zname, pG ) : 1;
		}
	}
	if ( (r = check_dup( pG )) != ZEN_OK )  /* remove duplicates in list */
		return( ziperr( r, pG ) );

	/* Check option combinations */
	if ( pG->action == PURGE && ( pG->dispose || pG->recurse || pG->key ) ) return( ziperr( ZEN_PARMS12, pG ) );
	if ( pG->linkput && pG->dosify ) {
		zipwarn( "can't use -y with -k, -y ignored", "" );
   	pG->linkput = 0;
	}

	/* d is the "allow append" indicator */
	/* if zcount is 0, then zipfile doesn't exist, or is empty */
	if ( pG->zcount == 0 && ( (pG->action != ADD && pG->action != UPDATE) || !d) ) {  //RCV150199 added UPDATE
		zipwarn( pG->zipfile, " not found or empty" );
		if ( pG->zcount ) FREE( pG->zsort );
		return 0;
	}

#	ifdef USE_STRM_INPUT
	if ( pG->UseInStream ) {
		if ( pG->UseOutStream ) {	// In-memory compression.
			user_callback( 5, 0, 1, NULL, pG );						// Pass total number off files always 1.
			user_callback( 6, 0, pG->InStreamSize, NULL, pG );	// Pass total filesize.
			pG->OutStreamSize = memcompress( pG->OutStream, pG->OutStreamSize, pG->InStream, pG->InStreamSize, pG );
			pG->files_acted_on++;
			user_callback( 3, 0, 0, NULL, pG );  // done with this in-memory compression.
			if ( pG->zcount ) FREE( pG->zsort );
			finish( pG );
			return 0;
		} else {	/* Here we fill in the FoundList from the input stream data */
			/* newname (fileio.c) adds to found list. If error m!=0 on return */
			if ( (r = newname( argv[ first_listarg ], pG->InStreamSize, pG )) != ZEN_OK ) {
				sprintf( pG->ewemsg, "Stream filename could not be added in newname call" );
				diag( pG->ewemsg, pG );
				if ( pG->zcount ) FREE( pG->zsort );
				return r;
			}
		}
	}
#	endif
	if ( pG->zcount ) {
		FREE( pG->zsort );
		pG->zsort = NULL;
	}

	/* If -b not specified, set temporary path to zipfile path  */
	if ( pG->tempath == NULL && ( 
									 (p = strrchr( pG->zipfile, '\\' )) != NULL ||
									 (p = strrchr( pG->zipfile, ':' )) != NULL) ) {
		if ( *p == ':' ) p++;
		if ( (pG->tempath = MALLOC( (int)(p - pG->zipfile) + 1) ) == NULL ) return( ziperr( ZEN_MEM10, pG ) );
		r  = *p;
		*p =  0;
		lstrcpy( pG->tempath, pG->zipfile );
		*p = (char)r;
	}

	// NOTE: "k" is being redefined below this point.  Now, it going to
	// track the no. of marked files in the "zfiles" linked list.

	/* For each marked entry in "zfiles" linked list, if not deleting,
	 * check if a corresponding "external" file exists.
	 * If updating or freshening, compare date of "external" file with
	 * entry in orig zipfile.
	 * Unmark if it external file doesn't exist or is too old, else mark it.
	 * Entries that are marked will cause that file to be rezipped.
	 */
	diag( "checking marked entries", pG );

	k = 0;   /* Initialize marked count */
	/* zfiles is name of the linked-list of filenames for the archive */
#	ifdef USE_STRM_INPUT
	if ( !pG->UseInStream )
#	endif
	for ( z = pG->zfiles; z != NULL; z = z->nxt ) if ( z->mark ) {
		ulg     FileAttr;
#		ifdef USE_EF_UX_TIME
		ztimbuf f_utim, z_utim;
#		endif

		if ( pG->action != PURGE && (
#			ifdef USE_EF_UX_TIME
				(t = filetime( z->name, &FileAttr, (long *)NULL, &f_utim, pG )) == 0 ||
				 t < pG->before ||
				 ( (pG->action == UPDATE || pG->action == FRESHEN) &&
				  (get_ef_ux_ztime(z, &z_utim) ? f_utim.modtime <= z_utim.modtime : t <= z->tim) ) ||
				 (pG->ArchiveFiles && pG->action == FRESHEN && !(FileAttr & A_ARCHIVE) )
				) ) {
#			else
				(t = filetime( z->name, &FileAttr, (long *)NULL, (ztimbuf *)NULL, pG )) == 0 ||
				 t < pG->before ||
				 ( (pG->action == UPDATE || pG->action == FRESHEN) && t <= z->tim ) ||
				 (pG->ArchiveFiles && pG->action == FRESHEN && !(FileAttr & A_ARCHIVE) )
				) ) {
#			endif
			z->mark  = 0;
			z->trash = t && t >= pG->before;    /* delete if -um or -fm */
			if ( pG->verbose ) {
				printf( "%s %s\n", z->name, z->trash ? "up to date" : "missing or early" );
			}
		} else k++;   /* incr. number of marked entries */
	}
#	ifdef USE_STRM_INPUT
	if ( pG->UseInStream ) k = 1;
#	endif
	/* Remove entries from "found" linked list if:
	 * Action is PURGE or FRESHEN or
	 * No "external" matching file is found,
	 * or if found, but is too old or
	 * The external file is equal to the ziparchive name while ziparchive name != "-"
	 *
	 * If filetime() returns any valid time, then at least we know the file was found.
	 */
	diag( "checking new entries", pG );

	/*============ fileio.c built the found list ==============*/
#	ifdef USE_STRM_INPUT
	if ( !pG->UseInStream )
#	endif
	for ( f = pG->found; f != NULL; ) {
		if ( pG->action == PURGE || pG->action == FRESHEN ||
				(t = filetime( f->name, (ulg *)NULL, (long *)NULL, (ztimbuf *)NULL, pG ) ) == 0 ||
				 t < pG->before || (namecmp( GetFullPath( pG, f->name ), pG->zipfile ) == 0 && strcmp( pG->zipfile, "-" )) ) {
			diag( "fexpel being called", pG );
			f = fexpel( f, pG );  /* delete an entry in the list. */
		} else               /* file found, and not too old. */
			f = f->nxt;       /* save this one, link it up.   */
	}
	if ( pG->found == NULL ) diag( "found list empty - a", pG );
	else diag( "found list has at least one entry - a", pG );

	/* Make sure there's something left to do */
	if ( k == 0 && pG->found == NULL && !(pG->zfiles != NULL && (pG->latest || pG->adjust || pG->junk_sfx )) ) {
		/* FOUND WAS NULL HERE, so just figure out which error message to show the user */
		if ( pG->action == UPDATE || pG->action == FRESHEN ) {
			finish( pG );
			return 0;
		} else if ( pG->zfiles == NULL && (pG->latest || pG->adjust || pG->junk_sfx) ) {
			return( ziperr( ZEN_NAME01, pG ) );
		} else if ( pG->recurse && (pG->pcount == 0) && (first_listarg > 0) ) {
			/* add the list of filenames from cmd line to error msg */
			for ( i = first_listarg; i < argc; i++ ) lstrcat( lstrcat( pG->errbuf, " " ), argv[i] );
			return( ziperr( ZEN_NONE01, pG ) );
		} else return( ziperr( ZEN_NONE02, pG ) );
	}
	d = (d && k == 0 && (pG->zipbeg || pG->zfiles != NULL)); /* d is true if appending */

	// continue on to add new files
#ifdef CRYPT
	/* Initialize the crc_32_tab pointer, when encryption was requested. */
	if ( pG->key )
		pG->crc_32_tab = (ulg *)get_crc_table();
#endif  /* CRYPT */

	a = 0;

	/* ignore self-extracting code in front of the zip file (for -J) */
	if ( pG->junk_sfx ) pG->zipbeg = 0;


	/* Count files and sizes which we have to process; RCV Added */
   // First the files in the old zip file...
	w = &pG->zfiles;
	while ( (z = *w) != NULL ) {
		if ( pG->global_abort_sw ) return( ziperr( ZEN_ABORT, pG ) );
		if ( z->mark == 1 ) {
			TotSize += z->len;
			TotFiles++;
		}
		w = &z->nxt;
	}
	// And the found list...
	for ( f = pG->found; f != NULL; f = f->nxt ) {
		if ( pG->global_abort_sw ) return( ziperr( ZEN_ABORT, pG ) );
		TotSize += f->len;
		TotFiles++;
	}
	user_callback( 5, 0, TotFiles, NULL, pG );	// Pass total number off files.
	user_callback( 6, 0, TotSize, NULL, pG );		// Pass total filesize.

	/* OPEN ZIP FILE and temporary output file */
	diag( "opening zip file and creating temporary zip file", pG );
	pG->x      = NULL;
	pG->tempzn = 0;
	if ( d ) {
		/* zipfile is not stdout, and we are allowed to append */
		/* d is true if we're just appending (-g) */
		diag( "in dllzip - ready to fopen in mode FOPM - point B", pG );
		if ( (pG->y = fopen( pG->zipfile, FOPM )) == NULL ) return( ziperr( ZEN_NAME02, pG ) );
		pG->tempzip = pG->zipfile;
		pG->tempzf  = pG->y;
		if ( fseek( pG->y, pG->cenbeg, SEEK_SET ) ) return( ziperr( ferror( pG->y ) ? ZEN_READ01 : ZEN_EOF01, pG ) );
		pG->tempzn = pG->cenbeg;
	} else {
		diag( "in dllzip - ready to fopen for FOPR_EX", pG );
		if ( (pG->zfiles != NULL || pG->zipbeg) && (pG->x = fopen( pG->zipfile, FOPR_EX )) == NULL )
			return( ziperr( ZEN_NAME03, pG ) );
		if ( (pG->tempzip = tempname( pG )) == NULL ) return( ziperr( ZEN_MEM11, pG ) );
		if ( (pG->tempzf = pG->y = fopen( pG->tempzip, FOPW )) == NULL )
			return( ziperr( ZEN_TEMP01, pG ) );
	}

	if ( !d ) {
		/* this must go after setvbuf */
		/* copy a compressed file from old archive to new archive */
		if ( pG->zipbeg && (r = fcopy( pG->x, pG->y, pG->zipbeg )) != ZEN_OK ) return( ziperr( r, pG ) );
		pG->tempzn = pG->zipbeg;
	}
	// o = 0;      /* no ZEN_OPEN errors yet */

	/* Process zip file, copying from old archive to new archive.
	 * Rezip any marked files */
	if ( pG->zfiles != NULL ) diag( "going through old zip file", pG );
	w = &pG->zfiles;

	while ( (z = *w) != NULL ) {
		if ( pG->global_abort_sw ) {
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( ZEN_ABORT, pG ) );
		}
		if ( z->mark == 1 ) {
			/* This file is marked */
			/* if not deleting, rezip it */
			if ( pG->action != PURGE ) {
				printf( "updating: %s", Oem2Iso( z->zname, pG ) );

				// zipup is in file zipup.c
				if ( (r = zipup( z, pG->y, pG )) != ZEN_OK && (int)(char)(r & 0xFF) != ZEN_OPEN && (int)(char)(r & 0xFF) != ZEN_MISS ) {
					user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
					return( ziperr( r, pG ) );
				}
				if ( (int)(char)(r & 0xFF) == ZEN_OPEN || (int)(char)(r & 0xFF) == ZEN_MISS ) {
					// o = 1;
					if ( (int)(char)(r & 0xFF) == ZEN_OPEN ) {
						zipwarn( "could not open for reading: ", z->name );
						user_callback( 4, r, 0, z->name, pG );
					} else {
						zipwarn( "file and directory with the same name: ", z->name );
						user_callback( 4, r, 0, z->name, pG );
					}
					zipwarn( "will just copy entry over: ", z->zname );
					if ( (r = zipcopy( z, pG->x, pG->y, pG )) != ZEN_OK ) {
						sprintf( pG->errbuf, "was copying %s", z->zname );
						user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
						return( ziperr( r, pG ) );
					}
					z->mark = 0;
				}
				w = &z->nxt;
				pG->files_acted_on++;
			} else {
				/* desired action is DELETE, this file marked  */
				// NOTE: no progress bar supt for DELETE yet
				printf( "deleting: %s\n", Oem2Iso( z->zname, pG ) );

				v = z->nxt;     /* delete entry from list */
				FREE( z->name );
				FREE( z->zname );
				if ( z->ext ) FREE( z->extra );
				if ( z->cext && z->cextra != z->extra ) FREE( z->cextra );
				if ( z->com ) FREE( z->comment );
				FREE( z );
				*w = v;
				pG->zcount--;
				pG->files_acted_on++;
			}
		} else {
			/* this file wasn't marked */
			/* copy the original entry verbatim */
			if ( !d && (r = zipcopy( z, pG->x, pG->y, pG )) != ZEN_OK ) {
				sprintf( pG->errbuf, "was copying %s", z->zname );
				user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
				return( ziperr( r, pG ) );
			}
			w = &z->nxt;
		}
	} /* end while */

	/* Process the "found" list, adding them to the zip file. */
	/* This is used to add files that weren't already in the archive. */
	sprintf( pG->ewemsg, "Zipping up %d NEW entries from found list", pG->fcount );
	diag( pG->ewemsg, pG );

	/* For each new file to add (src names in found list), make a new
	 * entry for it in the "zfiles" linked list, zip up the new file,
	 * then remove the entry from the found list. */
	/* The last item in the for loop control deallocates spc for fname
	 * that was just zipped up */
	for ( f = pG->found; f != NULL; f = fexpel( f, pG ) ) {
		/* add a new entry to "zfiles" list, before we zip up the file.
		 * That way we'll be ready to update the ZIP file's directory later. */
		if ( pG->global_abort_sw ) {
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( ZEN_ABORT, pG ) );
		}
		if ( (z = (struct zlist *)MALLOC( sizeof( struct zlist ) )) == NULL ) {
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( ZEN_MEM12, pG ) );
		}
		/* Similar names below are confusing:
		 *   f->name
		 *   f->zname
		 *   z->name
		 *   z->zname
		 */
		z->nxt = NULL;

		z->name = f->name;
		f->name = NULL;

		z->zname = f->zname;
		f->zname = NULL;

		z->ext     = z->cext = z->com = 0;
		z->extra   = z->cextra = NULL;
		z->mark    = 1;
		z->dosflag = f->dosflag;
      z->len     = f->len;		// RCV added.
		/* zip it up */
		printf( "  adding: %s", Oem2Iso( z->zname, pG ) );

		/************  This is it - try to zip up new file  **************/
		if ( (r = zipup( z, pG->y, pG )) != ZEN_OK && (int)(char)(r & 0xFF) != ZEN_OPEN && (int)(char)(r & 0xFF) != ZEN_MISS ) {
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( r, pG ) );
		}
		if ( (int)(char)(r & 0xFF) == ZEN_OPEN || (int)(char)(r & 0xFF) == ZEN_MISS ) {
			// o = 1;
			if ( (int)(char)(r & 0xFF) == ZEN_OPEN ) {
				zipwarn( "could not open for reading: ", z->name );
			} else {
				zipwarn( "file and directory with the same name: ", z->name );
			}
			FREE( z->name );
			FREE( z->zname );
			FREE( z );
		} else {	/* "zipup" of this file was good */
			*w = z;
			w  = &z->nxt;
			pG->zcount++;
			pG->files_acted_on++;
		}
	}

	/* Write central directory and end header to temporary zip */
	diag( "writing central directory", pG );
	k = 0;          /* keep count of new fnames for ZIPfile's end header */
	c = pG->tempzn; /* get start of central directory */
	n = t = 0;
	for ( z = pG->zfiles; z != NULL; z = z->nxt ) {
		// Handle callback and result for the filecomments... DLL v1.609, Component v1.60L
		if ( pG->action != PURGE ) {
			memset( pG->ewetmp, 0, 513 );					// Clear all
			strncpy( pG->ewetmp, z->zname, 255 );		// Copy external filename
			if ( z->com )										// Copy old comment if present
				strncpy( pG->ewetmp + 256, z->comment, min( z->com, 255 ) );
			user_callback( 12, 0, z->com, pG->ewetmp, pG );
			if ( pG->callbackdata.error_code ) {		// User changed the comment
				FREE( z->comment );
				z->com = pG->callbackdata.fsize;
				if ( (z->comment = (char *)MALLOC( z->com + 1 )) == NULL )
					return( ziperr( ZEN_MEM37, pG ) );
				strncpy( z->comment, pG->callbackdata.filenameormsg, z->com + 1 );
			}
		}
		if ( pG->files_acted_on && (r = putcentral( z, pG->y, pG )) != ZEN_OK ) {	// v1.6014
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( r, pG ) );
		}
		pG->tempzn += 4 + CENHEAD + z->nam + z->cext + z->com;
		n += z->len;
		t += z->siz;
		k++;
	}

	if ( k == 0 ) zipwarn( "zip file empty", "" );

	if ( (pG->verbose) && (pG->action == ADD) && (!pG->global_error_code ) && (pG->files_acted_on > 0) )
		printf( "Total Bytes=%lu, compr bytes=%lu -> %d%% savings", n, t, percent( n, t ) );

	t = pG->tempzn - c;   /* compute length of central */

	diag( "writing end of central directory", pG );
	if ( pG->files_acted_on && (r = putend( k, t, c, pG->zcomlen, pG->zcomment, pG->y )) != ZEN_OK ) {	// v1.6014
		user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
		return( ziperr( r, pG ) );
	}
	pG->tempzf = NULL;
	if ( fclose( pG->y ) ) {
		user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
		return( ziperr( d ? ZEN_WRITE03 : ZEN_TEMP02, pG ) );
	}
	if ( pG->x != NULL ) fclose( pG->x );

#	ifdef DYN_ALLOC
	lm_free( pG );
#	endif

	/* Replace old zip file with new zip file, leaving only the new one */
	if ( !d ) {
		diag( "replacing old zip file with new zip file", pG );
		if ( (r = replace( pG->zipfile, pG->tempzip, pG )) != ZEN_OK ) {
			zipwarn( "new zip file left as: ", pG->tempzip );
			FREE( pG->tempzip );
			pG->tempzip = NULL;
			user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
			return( ziperr( r, pG ) );
		}
		FREE( pG->tempzip );
	}
	pG->tempzip = NULL;
	if ( a ) setfileattr( pG->zipfile, a );

	/* Reset the archive bit when needed for all successfull zipped files */
	if ( pG->ResetArchiveBit && pG->action != PURGE ) {
		diag( "resetting archive bits", pG );
		for ( z = pG->zfiles; z != NULL; z = z->nxt ) if ( z->mark ) {
			char *fullname = GetFullPath( pG, z->name ); //v.16017

			if ( !SetFileAttributes( fullname, GetFileAttributes( fullname ) & ~FILE_ATTRIBUTE_ARCHIVE ) )
				zipwarn( "Archive bit could not be set for: ", z->name );
		}
	}
	user_callback( 3, 0, 0, NULL, pG );  // done with a batch of files
	/* Finish up (process -o, -m, clean up). */
	finish( pG );
	return 0;
}

/* Convert the internal zipname to a ISO compatible name to show to the
 * user via the callback and then in the message event.
 * If dosify is true then the name was converted to be OEM compatible
 * and that is not what we want to see.
 * v1.55 changed the local OutFileName to the global fnamebuf; the stack
 * was partly overwritten in case a printf call followed afterwards.
 */
char *Oem2Iso( char *InFileName, struct Globals *pG ) {
	if ( pG->dosify ) {
		int NameLen = lstrlen( InFileName );

		pG->fnamebuf[NameLen] = '\0';
		OemToCharBuff( InFileName, pG->fnamebuf, NameLen );
		return pG->fnamebuf;
	}
	return InFileName;
}
