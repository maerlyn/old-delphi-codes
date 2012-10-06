/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 */
/* InputBox Function for password */
#include "unzip.h"
#include "crypt.h"

#ifdef CRYPT /* If no crypt don't use */

#include "windows.h"
#include "globals.h"
#include "resource.h"


/* ===========================================================================
	hDlg    :: Window handle of the dialog box.
	message :: Type of message.
	wParam  :: Message-specific information.
	lParam
 */
#pragma argsused
bool CALLBACK PassProc( HWND hDlg, UINT message, UINT wParam, long lParam ) {
	struct Globals *pG = GetGlobalPointer();

	switch ( message ) {
		case WM_INITDIALOG:
			/* Set password character to an asterisk (*) */
			SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT, EM_SETPASSWORDCHAR, (WPARAM)'*', 0 );

			/* Set the default push button to "Cancel." */
			SendMessage( hDlg, DM_SETDEFID, (WPARAM)IDCANCEL, 0 );

			pG->pwork = true;
			pG->rcode = 0;
			return true;

		case WM_COMMAND:
			/*
			 * Set the default push button to "OK" when the user enters text.
			 */
			if ( HIWORD( wParam ) == EN_CHANGE && LOWORD( wParam ) == IDE_PASSWORDEDIT )
				SendMessage( hDlg, DM_SETDEFID, (WPARAM)IDOK, 0 );

			switch( wParam ) {
				case IDOK:
					/* Get number of characters. */
					pG->cchPassword = (WORD)SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT, EM_LINELENGTH, 0, 0 );

					if ( pG->cchPassword > PWLEN ) {
						MessageBox( hDlg, "Too many characters.", "Error", MB_OK );
						EndDialog( hDlg, true );
						pG->rcode = IZ_PW_ERROR;
						return false;
					} else if ( pG->cchPassword == 0 ) {
						MessageBox( hDlg, "No characters entered.", "Error", MB_OK );
						EndDialog( hDlg, true );
						pG->rcode = IZ_PW_CANCELALL;
						return false;
					}
					/*
					 * Put the number of characters into first word of buffer.
					 */
					*( (LPWORD)pG->lpszPassword) = pG->cchPassword;

					/* Get the characters. */
					SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT, EM_GETLINE, 0, (LPARAM)pG->lpszPassword );

					/* Null-terminate the string. */
					pG->lpszPassword[pG->cchPassword] = 0;

					/* Call a local password-parsing function. */
					pG->rcode = IZ_PW_ENTERED;
					EndDialog( hDlg, true );
					return true;

				case IDCANCEL:
					pG->rcode = IZ_PW_CANCELALL;
					EndDialog( hDlg, true );
					return true;
			}
			return 0;
	}
	return false;
}


/* ===========================================================================
 */
int UzpGetPassWrd( struct Globals *pG ) {	// RCV: 1.607 changed
	HINSTANCE hnd = GetModuleHandle( "unzdll.dll" );

	pG->pwork = false;
	if ( hnd ) {
		if ( DialogBox( hnd, MAKEINTRESOURCE( IDD_DIALOG1 ), pG->global_handle, (DLGPROC)PassProc ) == false )
			MessageBox( pG->global_handle, "DialogBox failed", "Procedure Failure", MB_OK );

		if ( !pG->pwork ) return IZ_PW_ERROR;

		if ( (memcpy( pG->key, pG->lpszPassword, pG->cchPassword )) == NULL ) return IZ_PW_ERROR;
		pG->key[pG->cchPassword] = '\0';
		return pG->rcode;
	}
	return 0;
}

#endif /* crypt */



