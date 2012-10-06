/* Globals.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
*/

#define GLOBALS         			/* Include definition of errors[] in zip.h 			*/

#include "Globals.h"

extern int extra_lbits[LENGTH_CODES]; /* extra bits for each length code */
extern int   extra_dbits[D_CODES];    /* extra bits for each distance code */
extern int   extra_blbits[BL_CODES];  /* extra bits for each bit length code */
extern uch   bl_order[BL_CODES];

extern DWORD TgbIndex;

/* ===========================================================================
 * Get the thread global data area, if not present create one first.
 */
struct Globals *GetGlobalPointer( int *ErrorNo ) {
	struct Globals *pG = TlsGetValue( TgbIndex );

	if ( !pG ) {
		if ( GetLastError() != NO_ERROR ) {
			*ErrorNo = ZEN_ABORT01;
			return NULL;
		}
		// We did not have a data area, w'll have to create it first.
		pG = (struct Globals *)MALLOC( sizeof( struct Globals ) );
		if ( pG && !TlsSetValue( TgbIndex, pG ) ) {
			FREE( pG );
			*ErrorNo = ZEN_ABORT02;
			return NULL;
		}
	}
	return pG;
}


/* ===========================================================================
 * Free the thread global data.
 */
void ReleaseGlobalPointer( void ) {
	struct Globals *pG = TlsGetValue( TgbIndex );

	if ( pG ) FREE( pG );				/* Free the thread global structure.  */
	TlsSetValue( TgbIndex, NULL );	/* Reset the index for this thread.   */
}


/* ===========================================================================
 * This initializes the global structure on a per thread basis.
 * We also need to call this function when we reenter from a particular thread.
 */
void GlobalsInit( struct Globals *pG ) {
	memset( pG, 0, sizeof( struct Globals ) );

	pG->pathput  = 1;
	pG->method   = DEFLATE;
	pG->level    = 9;
	pG->dirnames = 1;
	pG->noisy    = 1;
	/* linked list for new files to be added (not yet in ZIP) */
	pG->fnxt     = &pG->found;
	pG->adjust   = 1;
	/* User can specify if he wants to skip compressing these types: */
	/* List of special suffixes */
	// v1.6 removed: pG->special = ".gif:.png:.Z:.zip:.zoo:.arc:.lzh:.arj:.taz:.tgz:.lha";
	pG->dll_handles_errors = 1;	// By dflt, this DLL will generate error msg boxes
	init_upper( pG );					// build case map table
	pG->read_buf = file_read;		// a function
	pG->action	 = ADD;				// Must be ADD - the default action
	pG->zipstate = -1;
	pG->wild_match_all = "*.*";

	pG->l_desc.dyn_tree     = pG->dyn_ltree;
	pG->l_desc.static_tree  = pG->static_ltree;
	pG->l_desc.extra_bits   = extra_lbits;
	pG->l_desc.extra_base   = LITERALS + 1;
	pG->l_desc.elems        = L_CODES;
	pG->l_desc.max_length   = MAX_BITS;

	pG->d_desc.dyn_tree     = pG->dyn_dtree;
	pG->d_desc.static_tree  = pG->static_dtree;
	pG->d_desc.extra_bits   = extra_dbits;
	pG->d_desc.elems        = D_CODES;
	pG->d_desc.max_length   = MAX_BITS;

	pG->bl_desc.dyn_tree    = pG->bl_tree;
	pG->bl_desc.extra_bits  = extra_blbits;
	pG->bl_desc.elems       = BL_CODES;
	pG->bl_desc.max_length  = MAX_BL_BITS;
}


