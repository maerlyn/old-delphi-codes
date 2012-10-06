/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 * I'm not using the unzip LIST functionality, so I took it out.
 * The stuff still in this file is needed, anyway.
 */
/* ===========================================================================
 * list.c
 * This file contains the non-ZipInfo-specific listing routines for UnZip.
 * Contains:  //list_files()
 *            //ratio()
 *            //fnprint()    disable
 *===========================================================================*/

#include "UnZip.h"
#include <windows.h>

/* ===========================================================================
 */
//int list_files( struct Globals *pG ) {  /* return PK-type error code */
//	return 0;	// dummy function
//}

#ifdef TIMESTAMP
/* ===========================================================================
 *									Function time_stamp()
 */
int time_stamp( struct Globals *pG ) {   /* return PK-type error code */
	int do_this_file = false, error, error_in_archive = PK_COOL;
#if (defined(USE_EF_UX_TIME) || defined(UNIX))
	ztimbuf z_utime;
#endif
	time_t last_modtime = 0L;   /* assuming no zipfile data older than 1970 */
	ush j, members = 0;
	min_info info;

	/*---------------------------------------------------------------------------
	 * Unlike extract_or_test_files() but like //list_files(), this function works
	 * on information in the central directory alone.  Thus we have a single,
	 * large loop through the entire directory, searching for the latest time
	 * stamp.
	 *---------------------------------------------------------------------------*/
	pG->pInfo = &info;
	for ( j = 0; j < pG->ecrec.total_entries_central_dir; ++j ) {

		if ( readbuf( pG, pG->sig, 4) == 0 ) return PK_EOF;
		if ( strncmp( pG->sig, pG->central_hdr_sig, 4 ) ) {  /* just to make sure */
			return PK_BADERR;
		}
		/* process_cdir_file_hdr() sets pInfo->lcflag: */
		if ( (error = process_cdir_file_hdr( pG )) != PK_COOL )
			return error;       /* only PK_EOF defined */
		if ( (error = do_string( pG, pG->crec.filename_length, DS_FN )) != PK_OK ) {
			error_in_archive = error;		/*  ^-- (uses pInfo->lcflag) */
			if ( error > PK_WARN )			/* fatal:  can't continue */
				return error;
		}
		if ( pG->extra_field != (uch *)NULL ) {
			FREE( pG->extra_field );
			pG->extra_field = (uch *)NULL;
		}
		if ( (error = do_string( pG, pG->crec.extra_field_length, EXTRA_FIELD ) ) != 0 ) {
			pG->global_error_code = error;
			if ( error > PK_WARN )         /* fatal */
				return error;
		}
		if ( !pG->process_all_files ) {   /* check if specified on command line */
			fFileData *pfn = (fFileData *)pG->pfnames;
			int        k = 0;

			do_this_file = false;
			for ( ; k < pG->filespecs; k++, ++pfn ) {
				if ( match( pG->filename, pfn->fFileSpec ) ) {
					do_this_file = true;
					break;               /* found match, so stop looping      */
				}
			}
			if ( do_this_file ) {   /* check if this is an excluded file */
				fExFileData *pxn = (fExFileData *)pG->pxnames;

				for ( k = 0; k < pG->xfilespecs; k++, ++pxn ) {
					if ( match( pG->filename, pxn->fFileSpec ) ) {
						do_this_file = false;  /* ^-- ignore case in match */
						break;
					}
				}
			}
		}
		/* If current file was specified on command line, or if no names were
		 * specified, check the time for this file.  Either way, get rid of the
		 * file comment and go back for the next file.
		 */
		if ( pG->process_all_files || do_this_file ) {
#ifdef USE_EF_UX_TIME
			if ( pG->extra_field && ef_scan_for_izux( pG->extra_field,
					pG->crec.extra_field_length, &z_utime, NULL ) > 0 ) {
				if ( last_modtime < z_utime.modtime )
					last_modtime = z_utime.modtime;
			} else
#endif /* USE_EF_UX_TIME */
				{
				time_t modtime = dos_to_unix_time( pG->crec.last_mod_file_date,
															  pG->crec.last_mod_file_time );

				if ( last_modtime < modtime ) last_modtime = modtime;
			}
			++members;
		}
		SKIP_( pG->crec.file_comment_length )

	} /* end for-loop (j: files in central directory) */

	/*-----------------------------------------------------------------------------
	 * Set the modification (and access) time on the zipfile, assuming we have
	 * a modification time to set.
	 *---------------------------------------------------------------------------*/
	if ( members > 0 ) {
		z_utime.modtime = z_utime.actime = last_modtime;
	}

	/*-----------------------------------------------------------------------------
	 * Double check that we're back at the end-of-central-directory record.
	 *---------------------------------------------------------------------------*/
	if ( readbuf( pG, pG->sig, 4) == 0 ) return PK_EOF;
	if ( strncmp( pG->sig, pG->end_central_sig, 4 ) ) {     /* just to make sure again */
		error_in_archive = PK_WARN;
	}
	if ( members == 0 && pG->global_error_code <= PK_WARN )
		error_in_archive = PK_FIND;
	return error_in_archive;
} /* end function time_stamp() */
#endif /* TIMESTAMP */


/* ===========================================================================
 *										Function ratio()
 */
//int ratio( ulg uc, ulg c ) {
//	ulg denom;
//
//	if ( uc == 0 ) return 0;
//	if ( uc > 2000000L ) {    /* risk signed overflow if multiply numerator */
//		denom = uc / 1000L;
//		return ((uc >= c) ? (int)( (uc - c + (denom >> 1))/ denom) :
//			-( (int)((c - uc + (denom >> 1)) / denom)) );
//	} else {                  /* ^^^^^^^^ rounding */
//		denom = uc;
//		return ((uc >= c) ? (int)( (1000L *(uc - c) + (denom >> 1))/ denom ) :
//			-( (int)((1000L *(c - uc) + (denom >> 1))/ denom)) );
//	}                         /* ^^^^^^^^ rounding */
//}


/* ===========================================================================
 *									Function fnprint()
 */
//void fnprint( struct Globals *pG ) {   /* print filename (after filtering) and newline */
//	char *name = fnfilter( pG->filename, slide );

//	Trace( (pG, "%s", name) );
//}









