/* PassMsg.c
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 * InputBox Function for password
 */

#include "Zip.h"
#include "Globals.h"
#include "Crypt.h"

#ifdef CRYPT /* If no crypt, don't use */

#include "windows.h"
#include "Resource.h"

int getp( char *, char *, struct Globals *pG );

/* ===========================================================================
 * hDlg    :: Window handle of the dialog box.
	message :: Type of message.
	wParam  :: Message-specific information.
	lParam  ::
*/
#pragma argsused
bool CALLBACK PassProc( HWND hDlg, UINT message, UINT wParam, LONG lParam ) {
	int Error;
	struct Globals *pG = GetGlobalPointer( &Error );

	if ( !pG ) return false;
	switch ( message ) {
		case WM_INITDIALOG:
			/* Set Password character to an asterisk (*) */
			SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT, EM_SETPASSWORDCHAR, (WPARAM)'*', (LPARAM)0 );

			/* Set the prompt */
			SendDlgItemMessage( hDlg, IDC_PROMPT, WM_SETTEXT, (WPARAM)0, (LPARAM)pG->lpszPrompt );

			/* Set the default push button to "Cancel." */
			SendMessage( hDlg, DM_SETDEFID, (WPARAM)IDCANCEL, (LPARAM)0 );

			pG->pwork = true;
			pG->rcode = 0;
			return true;

		case WM_COMMAND:
			/*
			 * Set the default push button to "OK" when the user
			 * enters text.
			 */
			if ( HIWORD( wParam ) == EN_CHANGE && LOWORD( wParam ) == IDE_PASSWORDEDIT )
				SendMessage( hDlg, DM_SETDEFID, (WPARAM)IDOK, (LPARAM)0 );

			switch( wParam ) {
				case IDOK:
					/* Get number of characters. */
					pG->cchPassword = (WORD)SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT,
											EM_LINELENGTH, (WPARAM)0, (LPARAM)0 );

					if ( pG->cchPassword >= PWLEN + 1 ) {
						MessageBox( (HWND)pG->global_handle, "Too many characters.", "Error", MB_OK );
						EndDialog( hDlg, true );
						pG->rcode = ZEN_PW_ERROR;
						return false;
					} else if ( !pG->cchPassword ) {
						MessageBox( (HWND)pG->global_handle, "No characters entered.", "Error", MB_OK );
						EndDialog( hDlg, true );
						pG->rcode = ZEN_PW_CANCELALL;
						return false;
					}

					/*
					 * Put the number of characters into first word
					 * of buffer.
					 */
					*( (LPWORD)pG->lpszPassword ) = pG->cchPassword;

					/* Get the characters. */
					SendDlgItemMessage( hDlg, IDE_PASSWORDEDIT, EM_GETLINE, (WPARAM)0, /* line 0 */
												(LPARAM)pG->lpszPassword );

					/* Null-terminate the string. */
					pG->lpszPassword[pG->cchPassword] = 0;

					/* Call a local password-parsing function. */
					pG->rcode = ZEN_PW_ENTERED;
					EndDialog( hDlg, true );
					return true;

				case IDCANCEL:
					pG->rcode = ZEN_PW_CANCELALL;
					EndDialog( hDlg, true );
					return true;
			} // end inner switch
			return 0;
	} // end outer switch
	return false;
}

/* ===========================================================================
 * Sample call:
 * if getp("Enter password: ", key, pG ) == NULL) {
 * where key is a pointer to the global password buffer.
*/
int getp( char *msg, char *pkey, struct Globals *pG ) {
	HINSTANCE hnd;

	pG->pwork = false;
	pG->rcode = 0;
	pG->cchPassword = 0;

	diag( "in getp() of passmsg.c", pG );

	lstrcpy( pG->lpszPrompt, msg ); // prompt string follows the size
	pG->lpszPassword[0] = (char)0; // NULL out the initial password

   // hnd = global_handle; THIS HANDLE DID NOT WORK ONE TIME - WHY?
	hnd = GetModuleHandle( "zipdll.dll" );

	if ( hnd != 0 ) {
		diag( "trying to bring up the dialog", pG );
		if ( DialogBox( hnd, MAKEINTRESOURCE( IDD_DIALOG1 ),
				(HWND)pG->global_handle, (DLGPROC)PassProc) == false ) {
			MessageBox( (HWND)pG->global_handle, "DialogBox failed", "Procedure Failure", MB_OK );
			return ZEN_PW_ERROR;
		}

		if ( !pG->pwork ) return ZEN_PW_ERROR;

		pG->key_len = pG->cchPassword;
		memccpy( pkey, pG->lpszPassword, '\0', pG->key_len );

		sprintf( pG->ewemsg, "password=%s Len=%d", pG->lpszPassword, pG->key_len );
		diag( pG->ewemsg, pG );
		return pG->rcode;
	}
	return 0; /* no window handle - we can't prompt the user */
}
#endif /* crypt */

