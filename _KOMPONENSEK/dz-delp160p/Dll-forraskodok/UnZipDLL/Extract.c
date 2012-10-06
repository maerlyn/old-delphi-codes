/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Sep 22, 2000.
 */
/*--------------------------------------------------------
 * extract.c
 * This file contains the high-level routines ("driver routines") for extrac-
 * ting and testing zipfile members.  It calls the low-level routines in files
 * explode.c, inflate.c, unreduce.c and unshrink.c.
 * Contains:  extract_or_test_files()
 *            store_info()
 *            extract_or_test_member()
 *            TestExtraField()   RCV Removed
 *            MemExtract()
 *            MemFlush()
 *            fnfilter()
 *-------------------------------------------------------------------------*/
#include "unzip.h"
#include "crypt.h"

static int store_info( struct Globals *pG );
static int extract_or_test_member( struct Globals *pG );
static void CloseOut( struct Globals *pG );
// static int TestExtraField( struct Globals *pG, uch *ef, unsigned ef_len );  // RCV Removed


// yy::Extract=01, Add=02  zz::skip reason  yyzz	RCV: 1.610
#define SKIPPED_ON_FRESHEN						0x0101
#define SKIPPED_NO_OVERWRITE					0x0102
#define SKIPPED_FILE_EXISTS					0x0103
#define SKIPPED_BAD_PASSWORD					0x0104
#define SKIPPED_NO_ENCRYPTION					0x0105
#define SKIPPED_COMPRESSION_UNKNOWN			0x0106
#define SKIPPED_UNKNOWN_ZIPHOST				0x0107
#define SKIPPED_FILEFORMAT_WRONG				0x0108
#define SKIPPED_EXTRACT_ERROR					0x0109

/* ===========================================================================
 *									Function extract_or_test_files()
 */
int extract_or_test_files( struct Globals *pG ) {   /* return PK-type error code */
	uch  *cd_inptr;
	int   i, j = 0, cd_incnt, filnum = (-1), blknum = 0;
	int   error, error_in_archive = PK_COOL, *fn_matched = NULL, *xn_matched = NULL;
	ush   members_remaining, num_skipped = 0, num_bad_pwd = 0;
	long  cd_bufstart, bufstart, inbuf_offset, request;
	long  old_extra_bytes = 0L;
	long  fsize;
	unsigned long TotSize = 0L;

	/*---------------------------------------------------------------------------
	 * Since the central directory lies at the end of the zipfile, and the
	 * member files lie at the beginning or middle or wherever, it is not very
	 * efficient to read a central directory entry, jump to the member file
	 * and extract it, and then jump back to the central directory.
	 *
	 * Instead, we read from the central directory the pertinent information
	 * for a block of files, then go extract/test the whole block.  Thus, this
	 * routine contains two small(er) loops within a very large outer loop:
	 * the first of the small ones reads a block of files from the central
	 * directory; the second extracts or tests each file; and the outer one
	 * loops over blocks.
	 *
	 * Because of this jumping around, we can afford to be lenient if an
	 * error occurs in one of the member files:  we should still be able to
	 * go find the other members, since we know the offset of each from the
	 * beginning of the zipfile.
	 *---------------------------------------------------------------------------*/
	diag( pG, "in extract_or_test_files" );
	pG->pInfo = pG->info;
	// We're going to check each member file inside the archive
	members_remaining = pG->ecrec.total_entries_central_dir;
	Trace( (pG, "members_remaining = %d, unmatched fspecs = %d", members_remaining, pG->filespecs) );

#	ifdef CRYPT
	pG->newzip = true;
	pG->reported_backslash = false;
#	endif

	/* malloc space for check on unmatched filespecs (OK if one or both NULL) */
	if ( pG->filespecs > 0 && (fn_matched = (int *)MALLOC( pG->filespecs * sizeof( int ) )) != (int *)NULL )
		for ( i = 0; i < pG->filespecs; ++i ) fn_matched[i] = false;

	/*---------------------------------------------------------------------------
	 *  RCV Added: Calculate number of files to process and the total file size.
	 *---------------------------------------------------------------------------*/
	cd_bufstart = pG->cur_zipfile_bufstart;
	cd_inptr    = pG->inptr;
	cd_incnt    = pG->incnt;
	pG->pInfo = &pG->info[0];		/* Needed in process... */
	while ( members_remaining ) {
		int DoThisFile = false;

		members_remaining--;
		if ( readbuf( pG, pG->sig, 4 ) == 0 ) break;
		if ( strncmp( pG->sig, pG->central_hdr_sig, 4 ) ) break;
		if ( (error = process_cdir_file_hdr( pG )) != PK_COOL ) break;
		do_string( pG, pG->crec.filename_length + pG->crec.extra_field_length + pG->crec.file_comment_length, DS_FN );
		if ( pG->process_all_files ) DoThisFile = true;
		else {
			fFileData *pfn = (fFileData *)pG->pfnames;
			int        k = 0;

			for ( ; k < pG->filespecs; k++, ++pfn ) {
				if ( match( pG->filename, pfn->fFileSpec ) ) {
					DoThisFile = true;
					break;       /* found match, so stop looping */
				}
			}
		}
		if ( DoThisFile && store_info( pG ) ) {
			j++;
			TotSize += pG->crec.ucsize;
		}
	}
	user_callback( pG, 5, 0, j, NULL );
	user_callback( pG, 6, 0, TotSize, NULL );
	// RCV Changed 29-1-99 USE_STREAM_INPUT
	pG->cur_zipfile_bufstart = vclSeek( pG, (long)cd_bufstart, SEEK_SET );
	vclRead( pG, (char *)pG->inbuf, INBUFSIZ );

	pG->inptr = cd_inptr;
	pG->incnt = cd_incnt;
	members_remaining = pG->ecrec.total_entries_central_dir;

	diag( pG, "starting main loop" );
	/*---------------------------------------------------------------------------
	 *    Begin main loop over blocks of member files.
	 *---------------------------------------------------------------------------*/
	while ( members_remaining ) {
		j = 0;

		/* Loop through files in central directory, storing offsets, file
		 * attributes, case-conversion and text-conversion flags until block
		 * size is reached.
		 */
		/*==========================================================*/
		while ( members_remaining && (j < DIR_BLKSIZ) ) {
			--members_remaining;
			pG->pInfo = &pG->info[j];

			Trace( (pG, "New iter: members_remaining after this pass = %d", members_remaining) );
			pG->pInfo->chdrseq = (ush)(pG->ecrec.total_entries_central_dir - members_remaining - 1);
			if ( readbuf( pG, pG->sig, 4 ) == 0 ) {
				UnzErr( pG, UEN_EOF02 );
				error_in_archive = PK_EOF;
				members_remaining = 0;  /* ...so no more left to do */
				break;
			}
			// validate the central hdr signature:
			if ( strncmp( pG->sig, pG->central_hdr_sig, 4 ) ) {  /* just to make sure */
				UnzErr( pG, UEN_FORM01 );
				error_in_archive = PK_BADERR;
				members_remaining = 0;  /* ...so no more left to do */
				break;
			}
			/* process_cdir_file_hdr() in process.c, sets pInfo->hostnum and pInfo->lcflag */
			if ( (error = process_cdir_file_hdr( pG )) != PK_COOL ) {
				UnzErr( pG, UEN_FORM02 );
				diag( pG, "a 'not cool' error in archive" );
				members_remaining = 0;  /* ...so no more left to do */
				break;
			}
			diag( pG, "back in extract_or_test_files" );
			if ( (error = do_string( pG, pG->crec.filename_length, DS_FN )) != PK_COOL ) {
				if ( error > error_in_archive ) error_in_archive = error;
				if ( error > PK_WARN ) {  /* fatal:  no more left to do */
					UnzErr( pG, UEN_FORM03 );
					members_remaining = 0;
					break;
				} else
					printf( "warning error: %d on file: %s", error, pG->filename );
			}
			if ( (error = do_string( pG, pG->crec.extra_field_length, EXTRA_FIELD )) != 0 ) {
				if ( error > error_in_archive ) error_in_archive = error;
				if ( error > PK_WARN ) {  /* fatal */
					UnzErr( pG, UEN_FORM04 );
					members_remaining = 0;
					break;
				} else
					printf( "warning error: %d on file: %s", error, pG->filename );
			}
			if ( (error = do_string( pG, pG->crec.file_comment_length, SKIP )) != PK_COOL ) {
				if ( error > error_in_archive ) error_in_archive = error;
				if ( error > PK_WARN ) {  /* fatal */
					UnzErr( pG, UEN_FORM05 );
					members_remaining = 0;
					break;
				} else
					printf( "warning error: %d on file: %s", error, pG->filename );
			}
			if ( pG->process_all_files ) {
				if ( store_info( pG ) ) {
					Trace( (pG, "all_files, Matched: %s", pG->filename) );
					++j;  /* file is OK; info[] stored; continue with next */
				} else {
					Trace( (pG, "Skipped: %s", pG->filename) );
					++num_skipped;
				}
			} else {	// We're not processing all files.
				int		  do_this_file = false, k = 0;
				fFileData *pfn = (fFileData *)pG->pfnames;

				for ( ; k < pG->filespecs; k++, ++pfn ) {
					if ( match( pG->filename, pfn->fFileSpec ) ) {
						do_this_file = true;   /* ^-- ignore case or not? */
						if ( fn_matched ) {
							Trace( (pG, "Matched %s", pG->filename) );
							fn_matched[k] = true;
						}
						break;       /* found match, so stop looping */
					}
				}
				if ( do_this_file ) if ( store_info( pG ) ) {
					Trace( (pG, "file is OK: %s", pG->filename) );
					++j;       /* file is OK */
				} else  {
					Trace( (pG, "file skipped: %s", pG->filename) );
					++num_skipped;  /* unsupp. compression or encryption */
				}
			} /* end if (process_all_files) */

		} /* end while-loop (adding files to current block) */

		/* save position in central directory so can come back later */
		cd_bufstart = pG->cur_zipfile_bufstart;
		cd_inptr    = pG->inptr;
		cd_incnt    = pG->incnt;

		if ( pG->vflag > 0 || pG->global_trace_opt )
			printf( "UNZDLL expects to %s %d files", pG->tflag ? "test" : "extract", j );
		/*-----------------------------------------------------------------------
		 *  Second loop:  process files in current block, extracting or testing
		 *  each one.
		 *-----------------------------------------------------------------------*/
		diag( pG, "starting second loop - THIS TIME we take action." );

		for ( i = 0; i < j; ++i ) {
			filnum = i + blknum * DIR_BLKSIZ;
			pG->pInfo = &pG->info[i];

			//          NOVELL_BUG_FAILSAFE:
			pG->dne = false;  /* assume file exists until stat() says otherwise */

			/* if the target position is not within the current input buffer
			 * (either haven't yet read far enough, or (maybe) skipping back-
			 * ward), skip to the target position and reset readbuf(). */
			request      = pG->pInfo->offset + pG->extra_bytes;
			inbuf_offset = request % INBUFSIZ;
			bufstart     = request - inbuf_offset;

			Trace( ( pG, "loc A: request = %ld, inbuf_offset = %ld\n", request, inbuf_offset) );
			if ( request < 0 ) {
				UnzErr( pG, UEN_FORM06 );
				error_in_archive = PK_ERR;
				if ( filnum == 0 && pG->extra_bytes != 0L ) {
					printf( "attempting to recompensate" );
					old_extra_bytes = pG->extra_bytes;
					pG->extra_bytes = 0L;
					request = pG->pInfo->offset; /* could also check if this != 0 */
					inbuf_offset = request % INBUFSIZ;
					bufstart = request - inbuf_offset;
					Trace( (pG, "loc B: request = %ld, inbuf_offset = %ld\n", request, inbuf_offset) );
				} else {
					error_in_archive = PK_BADERR;
					diag( pG, "loc A: hosed - try next file" );
					continue;  /* this one hosed; try next */
				}
			}

			/* try again */
			if ( request < 0 ) {
				Trace( ( pG, "the recompensated request is still < 0\n") );
				UnzErr( pG, UEN_FORM07 );
				error_in_archive = PK_BADERR;
				continue;
			} else if ( bufstart != pG->cur_zipfile_bufstart ) {
				Trace( (pG, "bufstart != cur_zipfile_bufstart\n") );

				// RCV Changed 29-1-99 USE_STRM_INPUT
				pG->cur_zipfile_bufstart = vclSeek( pG, (long)bufstart, SEEK_SET );
				pG->incnt = vclRead( pG, (char *)pG->inbuf, INBUFSIZ );
				if ( pG->incnt <= 0 ) {
					UnzErr( pG, UEN_FORM08 );
					error_in_archive = PK_BADERR;
					diag( pG, "B. hosed - try next file" );
					continue;   /* can still do next file */
				}
				pG->inptr = pG->inbuf + (int)inbuf_offset;
				pG->incnt -= (int)inbuf_offset;
			} else {
				pG->incnt += (int)(pG->inptr - pG->inbuf) - (int)inbuf_offset;
				pG->inptr = pG->inbuf + (int)inbuf_offset;
			}
			/* should be in proper position now, so check for sig */
			if ( readbuf( pG, pG->sig, 4 ) == 0 ) {  /* bad offset */
				UnzErr( pG, UEN_FORM09 );
				error_in_archive = PK_BADERR;
				continue;   /* but can still try next one */
			}
			if ( strncmp( pG->sig, pG->local_hdr_sig, 4 ) ) {
				UnzErr( pG, UEN_FORM10 );
				error_in_archive = PK_ERR;
				if ( (filnum == 0 && pG->extra_bytes != 0L) ||
						(pG->extra_bytes == 0L && old_extra_bytes != 0L) ) {
					printf( "Attempting to Recompensate" );
					if ( pG->extra_bytes ) {
						old_extra_bytes = pG->extra_bytes;
						pG->extra_bytes = 0L;
					} else
						pG->extra_bytes = old_extra_bytes;  /* third attempt */
					ZLSEEK( pG->pInfo->offset )
					if ( readbuf( pG, pG->sig, 4 ) == 0 ) {  /* bad offset */
						UnzErr( pG, UEN_FORM11 );
						error_in_archive = PK_BADERR;
						continue;   /* but can still try next one */
					}
					if ( strncmp( pG->sig, pG->local_hdr_sig, 4 ) ) {
						UnzErr( pG, UEN_FORM12 );
						error_in_archive = PK_BADERR;
						continue;
					}
				} else {
					diag( pG, "C: hosed - try next file" );
					continue;  /* this one hosed; try next */
				}
			}
			diag( pG, "about to process local file hdr" );
			if ( (error = process_local_file_hdr( pG )) != PK_COOL ) {
				UnzErr( pG, UEN_FORM13 );
				error_in_archive = error;   /* only PK_EOF defined */
				diag( pG, "D. hosed - try next file" );
				continue;   /* can still try next one */
			}
			if ( (error = do_string( pG, pG->lrec.filename_length, DS_FN )) != PK_COOL ) {
				if ( error > error_in_archive ) error_in_archive = error;
				if ( error > PK_WARN ) {
					UnzErr( pG, UEN_FORM14 );
					diag( pG, "E. hosed - try next file" );
					continue;   /* go on to next one */
				}
			}
			Trace( ( pG, "Good entry for: %s", pG->filename) );

			if ( pG->extra_field != (uch *)NULL ) {
				FREE( pG->extra_field );
				pG->extra_field = (uch *)NULL;
			}
			if ( (error = do_string( pG, pG->lrec.extra_field_length, EXTRA_FIELD )) != 0 ) {
				if ( error > error_in_archive ) error_in_archive = error;
				if ( error > PK_WARN ) {
					UnzErr( pG, UEN_FORM15 );
					diag( pG, "F. hosed - try next file" );
					continue;   /* go on */
				}
			}
			/* Just about to extract file:  if extracting to disk, check if
			 * already exists, and if so, take appropriate action according to
			 * fflag/uflag/overwrite_all/etc. (we couldn't do this in upper
			 * loop because we don't store the possibly renamed filename[] in
			 * info[])
			 */
#			ifdef USE_STRM_OUTPUT
			if ( !pG->tflag && !pG->redirect_data ) {
#			else
			if ( !pG->tflag ) {
#			endif
				/* for files from DOS FAT, check for use of backslash instead
				 * of slash as directory separator (bug in some zipper(s); so
				 * far, not a problem in HPFS, NTFS or VFAT systems)
				 */
				if ( pG->pInfo->hostnum == FS_FAT_ && !strchr( pG->filename, '/' ) ) {
					char *p = pG->filename - 1;

					diag( pG, "parsing a FAT file" );
					while ( *++p ) {
						if ( *p == '\\' ) {
							if ( !pG->reported_backslash ) {
								UnzErr( pG, UEN_FORM16 );
								pG->reported_backslash = true;
								if ( !error_in_archive ) error_in_archive = PK_WARN;
							}
							*p = '/';
						}
					}
				}
				/* Mapname can create dirs if not freshening or if renamed(now always false) */
				if ( (error = mapname( pG, false )) > PK_WARN ) {
					if ( error == IZ_CREATED_DIR ) {
						/* GRR:  add code to set times/attribs on dirs--
						 * save to list, sort when done (a la zip), set
						 * times/attributes on deepest dirs first */
					} else if ( error == IZ_VOL_LABEL ) {
						diag( pG, "file is a vol label" );
						UnzErr( pG, UEN_FORM17 );
					} else if ( error > PK_ERR && error_in_archive < PK_ERR ) {
						diag( pG, "pG-> hosed - try next file" );
						error_in_archive = PK_ERR;
					}
					Trace( ( pG, "mapname(%s) returns error = %d\n", pG->filename, error) );
					continue;   /* go on to next file */
				}

				// filename contains the name as stored in the archive if the user want
				// to extract directories too then this part of the path is also present.
				// So we only need to complement it with the currentdir or the extractbasedir.
				lstrcpy( pG->filename, GetFullPath( pG, pG->filename ) );	// v1.6024

				/* Now is the time to possibly adjust the filename and/or path v1.6016 */
				user_callback( pG, 7, 0, 1, pG->filename );
				if ( pG->CallBackData.error_code == 1 ) {
					lstrcpy( pG->filename, pG->CallBackData.filenameormsg );
					pG->lrec.filename_length = (ush)lstrlen( pG->filename );
				}

				diag( pG, "starting switch near Novell failsafe in extract.c" );
				switch ( check_for_newer( pG, pG->filename ) ) {
					case DOES_NOT_EXIST:
						// NOVELL_BUG_FAILSAFE:
						pG->dne = true;   /* stat() says file DOES NOT EXIST */
						/* if freshening, don't skip if just renamed */
						if ( pG->fflag ) {
							user_callback( pG, 11, 0, SKIPPED_ON_FRESHEN, pG->filename );
	                  continue;   /* freshen (no new files):  skip */
						}
						break;

					case EXISTS_AND_OLDER:
						// Ask the user what to do, take overwrite_all as default. RCV: 1.6010
						pG->CallBackData.fsize = pG->overwrite_all;	// If no event present w'll keep the default
						user_callback( pG, 10, 0x10000 + pG->pInfo->chdrseq, pG->overwrite_all, pG->filename );
						if ( !pG->CallBackData.fsize ) {	// RCV: 1.6010 was !pG->overwrite_all
							user_callback( pG, 11, 0, SKIPPED_NO_OVERWRITE, pG->filename );
							Trace( (pG, "File exists: %s, overwrite false", pG->filename ) );
							continue;   /* never overwrite:  skip file */
						}
						break;

					case EXISTS_AND_NEWER:	/* or equal */
						// Ask the user what to do, take overwrite_all as default. RCV: 1.6010
						// If no event handler present w'll keep the default.
						user_callback( pG, 10, 0x20000 + pG->pInfo->chdrseq, pG->overwrite_all && !(pG->uflag || pG->fflag), pG->filename );	//RCV:1.6012
						if ( !pG->CallBackData.fsize ) {	// RCV: 1.6010 was !pG->overwrite_all
							user_callback( pG, 11, 0, SKIPPED_FILE_EXISTS, pG->filename );
							Trace( (pG, "File exists: %s, skipped", pG->filename ) );
							continue;  /* skip if update/freshen & orig name */
						}
						break;
				} /* end switch */
			} /* end if (extracting to disk) */

#			ifdef CRYPT
			Trace( (pG, "in extract.c, about to call decrypt") ); //EWE
			if ( (pG->pwdarg != NULL) && (lstrlen( pG->pwdarg ) > 0) )
				Trace( (pG, "pG->pwdarg=%s", pG->pwdarg) );
			else
				Trace( (pG, "pG->pwdarg is NULL, or points to zero len password") );

			if ( pG->pInfo->encrypted && (error = decrypt( pG )) != PK_COOL ) {
				if ( error == PK_MEM2 ) {
					if ( error > error_in_archive ) error_in_archive = error;
				} else {  /* (error == PK_WARN) */
					++num_bad_pwd;
					user_callback( pG, 11, 0, SKIPPED_BAD_PASSWORD, pG->filename );
					Trace( (pG, "Skipping encrypted file %s, bad password", pG->filename ) );
				}
				if ( pG->global_abort_sw ) {	// v1.6024
					UnzErr( pG, UEN_ABORT02 );
					break;
				}
				continue;   /* go on to next file */
			}
#			endif /* CRYPT */
			pG->disk_full = 0;

			if ( pG->global_abort_sw ) {
				UnzErr( pG, UEN_ABORT01 );
				break;
			}
			fsize = pG->lrec.ucsize;
			user_callback( pG, 1, 0, fsize, pG->filename ); // initial progress call

			// ====================================================
			// Function:  extract_or_test_member() does the unzip
			// ====================================================
			if ( (error = extract_or_test_member( pG )) != PK_COOL ) {
				if (error & 0x09) { /* abort check v1.6026 */
					UnzErr(pG, error );
               break;
				}
				user_callback( pG, 11, error, SKIPPED_EXTRACT_ERROR, pG->filename );
				diag( pG, "error occured while extracting or testing" );
				if ( error > error_in_archive ) error_in_archive = error;  /* ...and keep going */
				if ( pG->disk_full > 1 ) {
#					ifdef DYNALLOC_CRCTAB
					FREE( CRC_32_TAB );
#					endif
					if ( fn_matched ) FREE( fn_matched );
					if ( xn_matched ) FREE( xn_matched );
					return error_in_archive;
				}
			} else {
				printf( "%s file %s of size %ld", pG->tflag ? "Tested" : "Unzipped", pG->filename, fsize );
				pG->files_acted_on++;
			}
		} /* end for-loop (i:  files in current block) */

		/* Jump back to where we were in the central directory, then go and do
		 * the next batch of files.
		 */
		diag( pG, "jump back in central dir to where we were" );
		// RCV Changed 29-1-99 USE_STRM_INPUT
		pG->cur_zipfile_bufstart = vclSeek( pG, (long)cd_bufstart, SEEK_SET );
		vclRead( pG, (char *)pG->inbuf, INBUFSIZ );  /* been here before... */

		pG->inptr = cd_inptr;
		pG->incnt = cd_incnt;
		++blknum;
	} /* end while-loop (blocks of files in central directory) */

	diag( pG, "done with big outer block" );
	user_callback( pG, 3, 0, 0, NULL );  // done with a batch of files
	/*---------------------------------------------------------------------------
	 *  Check for unmatched filespecs on command line and print warning if any
	 *  found.  Free allocated memory.
	 *---------------------------------------------------------------------------*/
	if ( fn_matched ) {
		diag( pG, "fn_matched was true" );
		for ( i = 0; i < pG->filespecs; ++i ) if ( !fn_matched[i] ) {
			sprintf( pG->ewemsg, "Filespec Not Matched: %s", ((fFileData *)(pG->pfnames))[i].fFileSpec );
			UnzErr( pG, UEN_MISC03 );
			if ( error_in_archive <= PK_WARN ) error_in_archive = PK_FIND;  /* some files not found */
		}
		FREE( fn_matched );
	}

	/*---------------------------------------------------------------------------
	 * Double-check that we're back at the end-of-central-directory record, and
	 * print quick summary of results, if we were just testing the archive.
	 *---------------------------------------------------------------------------*/
	if ( readbuf( pG, pG->sig, 4 ) == 0 ) {
		diag( pG, "bad signature at end of archive, or premature EOF" );
		error_in_archive = PK_EOF;
	}
	if ( strncmp( pG->sig, pG->end_central_sig, 4 ) ) {    /* just to make sure */
		printf( "Bad Ending Signature for Central dir" );
		if ( !error_in_archive )       /* don't overwrite stronger error */
			error_in_archive = PK_WARN;
	}

	++filnum;  /* initialized to -1, so now zero if no files found */
	Trace( ( pG, "filnum = %d", filnum) );

	if ( pG->tflag ) {
		// testing archive
		int num = filnum - num_bad_pwd;

		if ( pG->qflag < 2 ) {         /* GRR 930710:  was (pG->qflag == 1) */
			if ( error_in_archive ) {
				sprintf( pG->ewemsg, "Error In Archive %s %s",
						(error_in_archive == 1) ? "warning-" : "", pG->zipfn );
				UnzErr( pG, UEN_TEST01 );
			} else if ( num == 0 ) {
				sprintf( pG->ewemsg, "Zero Files Tested %s",  pG->zipfn );
				user_callback( pG, 4, 0, 0, pG->ewemsg );
			} else if ( pG->process_all_files && (num_skipped + num_bad_pwd == 0) ) {
				sprintf( pG->ewemsg, "no error in %s", pG->zipfn );
				user_callback( pG, 4, 0, 0, pG->ewemsg );
			} else {
				if ( num > 1 )
					sprintf( pG->ewemsg, "No Errors Found In %d Tested Files of %s", num, pG->zipfn );
				else
					sprintf( pG->ewemsg, "No Error Found In %d Tested File of %s", num, pG->zipfn );
				user_callback( pG, 4, 0, 0, pG->ewemsg ); // BUG WAS HERE
			}
		}
		if ( num_skipped > 0 ) {
			sprintf( pG->ewemsg, "Skipped %d Files of %s", num_skipped, pG->zipfn );
			UnzErr( pG, UEN_TEST02 );
		}
#		ifdef CRYPT
		if ( num_bad_pwd > 0 ) {
			sprintf( pG->ewemsg, "Files with bad pwd: %d", num_bad_pwd );
			UnzErr( pG, UEN_MISC05 );
		}
#		endif /* CRYPT */

		else if ( (pG->qflag == 0) && !error_in_archive && (num == 0) ) {
			sprintf( pG->ewemsg, "Zero Files Tested %s", pG->zipfn );
			user_callback( pG, 4, 0, 0, pG->ewemsg );
		}
	}

	/* give warning if files not tested or extracted (first condition can still
	 * happen if zipfile is empty and no files specified on command line) */
	if ( (filnum == 0) && error_in_archive <= PK_WARN ) {
		sprintf( pG->ewemsg, "no files found" );
		UnzErr( pG, UEN_MISC04 );
		error_in_archive = PK_FIND;   /* no files found at all */
	} else if ( (num_skipped > 0) && !error_in_archive ) {
		sprintf( pG->ewemsg, "some files skipped" );
		user_callback( pG, 4, 0, 0, pG->ewemsg );
		error_in_archive = PK_WARN;
	}

#	ifdef CRYPT
	else if ( (num_bad_pwd > 0) && !error_in_archive ) error_in_archive = PK_WARN;
#	endif

	return error_in_archive;
} /* end function extract_or_test_files() */


#ifdef COPYRIGHT_CLEAN  /* no reduced or tokenized files */
#  define UNKN_COMPR  (pG->crec.compression_method > SHRUNK && \
   pG->crec.compression_method != IMPLODED && pG->crec.compression_method != DEFLATED)
#else /* !COPYRIGHT_CLEAN */
#  define UNKN_COMPR \
   (pG->crec.compression_method > IMPLODED && pG->crec.compression_method != DEFLATED)
#endif /* ?COPYRIGHT_CLEAN */

/* ===========================================================================
 *   Check central directory info for version/compatibility requirements.
 *									Function store_info()
 */
static int store_info( struct Globals *pG ) {  /* return 0 if skipping, 1 if OK */
	pG->pInfo->encrypted  = pG->crec.general_purpose_bit_flag & 1;       /* bit field */
	pG->pInfo->ExtLocHdr  = (pG->crec.general_purpose_bit_flag & 8) == 8;/* bit field */
	pG->pInfo->textfile   = pG->crec.internal_file_attributes & 1;       /* bit field */
	pG->pInfo->crc        = pG->crec.crc32;
	pG->pInfo->compr_size = pG->crec.csize;

	switch ( pG->aflag ) {
		case 0:
			pG->pInfo->textmode = false;   /* bit field */
			break;

		case 1:
			pG->pInfo->textmode = pG->pInfo->textfile;   /* auto-convert mode */
			break;

		default:  /* case 2: */
			pG->pInfo->textmode = true;
			break;
	}

	// EWE note: all platforms define VMS_UNZIP_VERSION (currently 42)
	if ( pG->crec.version_needed_to_extract[1] == VMS_) {
		if ( pG->crec.version_needed_to_extract[0] > VMS_UNZIP_VERSION ) {
			user_callback( pG, 11, 0, SKIPPED_UNKNOWN_ZIPHOST, pG->filename );	// RCV: 1.610
			Trace( ( pG, "Unsupported zip version or hosttype" ) );
			return 0;
		}
#		ifndef VMS   /* won't be able to use extra field, but still have data */
		else if ( !pG->tflag && !pG->overwrite_all ) {   /* if -o, extract regardless */
			user_callback( pG, 11, 0, SKIPPED_FILEFORMAT_WRONG, pG->filename );	// RCV: 1.610
			Trace( ( pG, "Warning - file's format may be incorrect: %s", pG->filename ) );
			return 0;
		}
#		endif
		/* usual file type:  don't need VMS to extract */
	} else if ( pG->crec.version_needed_to_extract[0] > UNZIP_VERSION ) {
		user_callback( pG, 11, 0, SKIPPED_UNKNOWN_ZIPHOST, pG->filename );	// RCV: 1.610
		Trace( (pG, "Unsupported zip version or hosttype" ) );
		return 0;
	}
	if UNKN_COMPR {
		user_callback( pG, 11, 0, SKIPPED_COMPRESSION_UNKNOWN, pG->filename );	// RCV: 1.610
		Trace( (pG, "Unsupported compression type" ) );
		return 0;
	}
#	ifndef CRYPT
	if ( pG->pInfo->encrypted ) {
		user_callback( pG, 11, 0, SKIPPED_NO_ENCRYPTION, pG->filename );	// RCV: 1.610
		Trace( (pG, "Skipping encrypted file: %s", pG->filename ) );
		return 0;
	}
#	endif

	/* map whatever file attributes we have into the local format */
	mapattr( pG );   /* GRR:  worry about return value later */
	pG->pInfo->offset = (long)pG->crec.relative_offset_local_header;
	return 1;
} /* end function store_info() */


/* ===========================================================================
 *								Function extract_or_test_member()
 *
 * return PK-type error code or PK_COOL.
 * direct: PK_DISK (open error or full), PK_ERR, PK_WARN, PK_MEM3
 * indirect caused by unshrink:
 *
 */
static int extract_or_test_member( struct Globals *pG ) {
	//char *nul = "[empty] ", *txt = "[text]  ", *bin = "[binary]";  // RCV Removed
	register int b;
	int          r, error = PK_COOL;
	ulg          wsize;

	Trace( ( pG, "Start extract_or_test_member: %s", pG->filename) );
	/*---------------------------------------------------------------------------
	 *    Initialize variables, buffers, etc.
	 *---------------------------------------------------------------------------*/
	pG->bits_left = 0;
	pG->bitbuf    = 0L;       /* unreduce and unshrink only */
	pG->zipeof    = 0;
	pG->newfile   = true;
	pG->crc32val  = CRCVAL_INITIAL;
	Trace( (pG, "initializing pG->crc32val to %08X", CRCVAL_INITIAL) );

	if ( pG->tflag ) {      // if test desired
		if ( pG->vflag ) {   // if verbose
			sprintf( pG->ewemsg, "Testing %s", pG->filename );
			user_callback( pG, 4, 0, 0, pG->ewemsg );
		}
	} else if ( open_outfile( pG ) ) return PK_DISK;

	/*---------------------------------------------------------------------------
	 *    Unpack the file.
	 *---------------------------------------------------------------------------*/
	diag( pG, "unpack the file" );
	defer_leftover_input( pG );    /* so NEXTBYTE bounds check will work */

	switch ( pG->lrec.compression_method ) {
		case STORED:
#			ifdef USE_STRM_OUTPUT
			if ( pG->redirect_data ) {
				wsize = pG->redirect_size + 1;
				pG->outptr = pG->redirect_buffer;
			} else {
#			endif
				wsize      = WSIZE;
				pG->outptr = slide;
#			ifdef USE_STRM_OUTPUT
			}
#			endif
			pG->outcnt = 0L;
			while ( (b = NEXTBYTE) != EOF && !pG->disk_full ) {
				*pG->outptr++ = (uch)b;
				if ( ++pG->outcnt == wsize ) { // EWE: wsize = 32K
					flush( pG, slide, pG->outcnt, 0 );
					pG->outptr = slide;
					pG->outcnt = 0L;
					if ( pG->global_abort_sw ) { /* v1.6026 */
						CloseOut( pG );
						undefer_input( pG );
						return UEN_ABORT04;
					}
				}
#				ifdef USE_STRM_OUTPUT
 				if ( pG->redirect_data && !(pG->outcnt % 0x8000) )	// RCV1.6022
		 			user_callback( pG, 2, 0, 0x8000, NULL ); // bump up progress bar
#				endif
			}
#			ifdef USE_STRM_OUTPUT
			if ( pG->outcnt && !pG->redirect_data )
#			else
			if ( pG->outcnt )          /* flush final (partial) buffer */
#			endif
				flush( pG, slide, pG->outcnt, 0 );
			break;

		case SHRUNK:
			if ( (r = unshrink( pG )) != PK_COOL ) {
				printf( "Error unzipping files" );
				error = r;
			}
			break;

		case IMPLODED:
			if ( ((r = explode( pG )) != 0) && r != 5 ) {   /* treat 5 specially */
				printf( "Error unzipping files" );
				error = (r == 3) ? PK_MEM3 : PK_ERR;
			}
			if ( r == 5 ) {
				int warning = ((ulg)pG->used_csize <= pG->lrec.csize);
				printf( "Error unzipping files" );
				error = warning ? PK_WARN : PK_ERR;
			}
			break;

		case DEFLATED:
			if ( (r = inflate( pG )) != 0 ) {
				if (r & 0x09 ) {	/* user want to cancel operation v 1.6026 */
					CloseOut( pG );
					undefer_input( pG );
					return r;
				}
				printf( "Error unzipping files" );
				error = (r == 3) ? PK_MEM3 : PK_ERR;
			}
			break;

		default:   /* should never get to this point */
			diag( pG, "should NEVER get here" );
			printf( "Error unzipping files" );
			/* close and delete file before return? */
			undefer_input( pG );
			return PK_WARN;
	} /* end switch (compression method) */

	if ( pG->disk_full ) {            /* set by flush() */
		if ( pG->disk_full > 1 ) {
			undefer_input( pG );
			return PK_DISK;
		}
		error = PK_WARN;
	}

	if ( error != PK_COOL )
		Trace( (pG, "had an error of %d before closing file", error) );

	CloseOut( pG );

	/* GRR todo: CONVERT close_outfile() TO NON-VOID:  CHECK FOR ERRORS! */

	if ( error > PK_WARN ) {	/* don't print redundant CRC error if error already */
		undefer_input( pG );
		return error;
	}

	Trace( ( pG, "After extraction, pG->crc32val = %08X", pG->crc32val) );
	Trace( ( pG, "File's CRC in local hdr = %08X", pG->lrec.crc32) );
	if ( pG->crc32val != pG->lrec.crc32 ) {
		if ( pG->CallerVersion >= 160 ) {	// RCV: 1.609
			// Call the component with a request for what do with with this CRC error.
			user_callback( pG, 9, pG->crc32val, pG->lrec.crc32, pG->filename );
			if ( !pG->CallBackData.error_code ) {
				DeleteFile( pG->filename );
				error = PK_ERR;
			}
		}
		if ( pG->CallerVersion < 160 || pG->CallBackData.error_code == 2 ) {
			/* if quiet enough, we haven't output the filename yet:  do it */
			printf( "After extraction, file %s had a CRC error", pG->filename );
#			ifdef CRYPT
			if ( pG->pInfo->encrypted )
				printf( "May be Bad Password for file: %s", pG->filename );
#			endif
			error = PK_WARN;
		}
		//error = PK_ERR;		// In the old code there was always an error although extracting did take place?
	}
	undefer_input( pG );

	if ( error != PK_COOL )
		Trace( ( pG, "extract_or_test_member returning error: %d", error) );
	return error;
} /* end function extract_or_test_member() */


/*---------------------------------------------------------------------------
 * Close the file and set its date and time (not necessarily in that order),
 * and make sure the CRC checked out OK.  Logical-AND the CRC for 64-bit
 * machines (redundant on 32-bit machines).
 *---------------------------------------------------------------------------*/
static void CloseOut( struct Globals *pG ) {
#	ifdef USE_STRM_OUTPUT
	if ( !pG->tflag && (!pG->cflag || pG->redirect_data) )
		if ( pG->redirect_data )
			flush( pG, redirSlide, (ulg)(pG->outcnt), 0 );
		else
			close_outfile( pG );
#	else
	if ( !pG->tflag && !pG->cflag )   /* don't close NULL file or stdout */
		close_outfile( pG );
#	endif
}

/* ===========================================================================
 *									Function TestExtraField()
 *
static int TestExtraField( struct Globals *pG, ef, ef_len)
    uch *ef;
    unsigned ef_len;
{
//    ush ebID;  // RCV Removed
    unsigned ebLen;

    // we know the regular compressed file data tested out OK, or else we
    // wouldn't be here ==> print filename if any extra-field errors found
    //
    while (ef_len >= EB_HEADSIZE) {
//        ebID = makeword(ef);	// RCV Removed
        ebLen = (unsigned)makeword(ef+EB_LEN);

        if (ebLen > (ef_len - EB_HEADSIZE)) {
           // Discovered some extra field inconsistency!
            printf( "Bad length on file %s", pG->filename );
            return PK_ERR;
        }
        ef_len -= (ebLen + EB_HEADSIZE);
        ef += (ebLen + EB_HEADSIZE);
    }
    if (!pG->qflag)
        printf( "OK") ;
    return PK_COOL;
}
*/


/* ===========================================================================
 *									Function MemExtract()
 *
 * extract compressed stream
 *	Return PK-type error.
 */
int MemExtract( struct Globals *pG ) {
	uch *old_inptr = pG->inptr;
	int  old_incnt = pG->incnt, error = PK_OK, r;
	ush  Method = makeword( pG->InStream );
	ulg  ExtraFieldCRC = makelong( (unsigned char *)pG->InStream + 2 );

	pG->inptr     = (unsigned char *)pG->InStream + 2 + 4;      // Method and ExtraFieldCRC
	pG->incnt     = (int)(pG->csize = (long)(pG->InStreamSize - (2 + 4)));
	pG->mem_mode  = true;
	pG->outbufptr = pG->redirect_buffer;
	pG->outsize   = pG->buffer_size;

	user_callback( pG, 5, 0, 1, NULL );
	user_callback( pG, 6, 0, pG->redirect_size, NULL );
	user_callback( pG, 1, 0, pG->redirect_size, "InStream" );

	switch ( Method ) {
		case STORED:
			error = MemFlush( pG, pG->inptr, pG->incnt );
			break;

		case DEFLATED:
			pG->outcnt = 0L;
			if ( (r = inflate( pG )) != 0 ) {
				if ( !pG->tflag ) printf( "Error unzipping stream" );
				error = (r == 3) ? PK_MEM3 : PK_ERR;
			}
			break;

		default:
			if ( pG->tflag ) error = PK_ERR | ((int)Method << 8);
			else {
				printf( "Unsupported extract method" );
				error = PK_ERR;  // GRR:  should be passed on up via SetEAs()
			}
	}
	pG->inptr = old_inptr;
	pG->incnt = old_incnt;
	pG->mem_mode = false;
	if ( !error ) {
		register ulg crcval = crc32( CRCVAL_INITIAL, pG->redirect_buffer, (extent)pG->outcnt );
		if ( crcval != ExtraFieldCRC ) {
			if ( pG->tflag ) error = PK_ERR | (DEFLATED << 8);  // kludge for now
			else {
				user_callback( pG, 9, crcval, ExtraFieldCRC, "InStream" );
				if ( !pG->CallBackData.error_code ) {
					printf( "Bad extra field CRC" );
					error = PK_ERR;
				}
			}
		}
	}
	user_callback( pG, 13, 0, pG->outcnt, NULL );
	user_callback( pG,  3, 0, 0, NULL );  // done with a batch of files
	if ( !error ) pG->files_acted_on = 1;
	return error;
} /* end function MemExtract() */


/* ===========================================================================
 *									Function MemFlush()
 *
 * rawbuf = redirSlide = pG->redirect_pointer = pG->outbuf and
 * size = wsize = pG->redirect_size
 */
int MemFlush( struct Globals *pG, uch *rawbuf, ulg size ) {
	if ( size > pG->outsize ) {	// more data than output buffer can hold
		pG->buffer_size = pG->outcnt + size;	// New requested stream size
		// Reset the orinaly given stream sizes
		user_callback( pG,  6, 0, pG->buffer_size, NULL );
		user_callback( pG,  1, 0, pG->buffer_size, "InStream" );
		user_callback( pG,  2, 0, pG->outcnt, NULL );		// Progress upto now
		user_callback( pG, 13, 0, pG->buffer_size, NULL );	// Request new size
		if ( pG->CallBackData.error_code ) return 50;
		pG->outsize = size;
		pG->redirect_buffer = (uch *)pG->CallBackData.fsize;	// Possibly new location for the buffer
		pG->outbufptr = pG->redirect_buffer + pG->outcnt;		// Change the pointer inside this buffer
	}
	if ( size ) user_callback( pG, 2, 0, size, NULL );		 // bump up progress bar

	memcpy( (char *)pG->outbufptr, (char *)rawbuf, (extent)size );
	pG->outbufptr += (unsigned int)size;
	pG->outsize   -= size;
	pG->outcnt    += size;
	return 0;
}


/* ===========================================================================
 *									Function fnfilter()
 */
char *fnfilter( char *raw, uch *space ) {		/* convert name to safely printable form */
#	ifndef NATIVE   /* ASCII:  filter ANSI escape codes, etc. */
	register uch *r=(uch *)raw, *s=space;
	while ( *r )
		if ( *r < 32 ) *s++ = '^', *s++ = (uch)(64 + *r++);
		else           *s++ = *r++;
	*s = 0;
	return (char *)space;
#	else
	return raw;
#	endif
}



















