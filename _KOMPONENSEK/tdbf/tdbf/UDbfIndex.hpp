// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfIndex.pas' rev: 4.00

#ifndef UDbfIndexHPP
#define UDbfIndexHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfCommon.hpp>	// Pascal unit
#include <UDbfIndexFile.hpp>	// Pascal unit
#include <UDbfCursor.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfindex
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TIndexCursor;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TIndexCursor : public Udbfcursor::TVirtualCursor 
{
	typedef Udbfcursor::TVirtualCursor inherited;
	
public:
	__fastcall TIndexCursor(Udbfindexfile::TIndexFile* dbfIndexFile);
	virtual bool __fastcall Next(void);
	virtual bool __fastcall Prev(void);
	virtual void __fastcall First(void);
	virtual void __fastcall Last(void);
	virtual int __fastcall GetPhysicalRecno(void);
	virtual void __fastcall SetPhysicalRecno(int Recno);
	virtual int __fastcall GetSequentialRecordCount(void);
	virtual int __fastcall GetSequentialRecno(void);
	virtual void __fastcall SetSequentialRecno(int Recno);
	virtual void __fastcall GotoBookmark(const Udbfcommon::rBookmarkData &Bookmark);
	virtual Udbfcommon::rBookmarkData __fastcall GetBookMark();
	virtual void __fastcall Insert(int Recno, char * Buffer);
	virtual void __fastcall Update(int Recno, char * PrevBuffer, char * NewBuffer);
	Udbfcommon::rBookmarkData IndexBookmark;
	__fastcall virtual ~TIndexCursor(void);
};

#pragma pack(pop)

typedef Udbfindexfile::TIndexPage* *PIndexPosInfo;

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Udbfindex */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfindex;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfIndex
