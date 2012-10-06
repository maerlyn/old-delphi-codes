// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfCommon.pas' rev: 4.00

#ifndef UDbfCommonHPP
#define UDbfCommonHPP

#pragma delphiheader begin
#pragma option push -w-
#include <ComCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Graphics.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfcommon
{
//-- type declarations -------------------------------------------------------
typedef char TDbfFieldType;

typedef Exception EDBFError;
;

struct rBookmarkData;
typedef rBookmarkData *PBookMarkData;

#pragma pack(push, 4)
struct rBookmarkData
{
	int IndexBookmark;
	int RecNo;
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Shortint _MAJOR_VERSION = 0x4;
static const Shortint _MINOR_VERSION = 0xa;
extern PACKAGE void __fastcall FileCopy(AnsiString source, AnsiString dest);
extern PACKAGE void __fastcall FreeAndNil(void *v);

}	/* namespace Udbfcommon */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfcommon;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfCommon
