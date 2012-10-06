// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfCursor.pas' rev: 4.00

#ifndef UDbfCursorHPP
#define UDbfCursorHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfCommon.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfcursor
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TVirtualCursor;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TVirtualCursor : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Udbfpagedfile::TPagedFile* _file;
	
public:
	__property Udbfpagedfile::TPagedFile* PagedFile = {read=_file};
	__fastcall TVirtualCursor(Udbfpagedfile::TPagedFile* pFile);
	__fastcall virtual ~TVirtualCursor(void);
	int __fastcall RecordSize(void);
	virtual bool __fastcall Next(void) = 0 ;
	virtual bool __fastcall Prev(void) = 0 ;
	virtual void __fastcall First(void) = 0 ;
	virtual void __fastcall Last(void) = 0 ;
	virtual int __fastcall GetPhysicalRecno(void) = 0 ;
	virtual void __fastcall SetPhysicalRecno(int Recno) = 0 ;
	virtual int __fastcall GetSequentialRecordCount(void) = 0 ;
	virtual int __fastcall GetSequentialRecno(void) = 0 ;
	virtual void __fastcall SetSequentialRecno(int Recno) = 0 ;
	virtual Udbfcommon::rBookmarkData __fastcall GetBookMark(void) = 0 ;
	virtual void __fastcall GotoBookmark(const Udbfcommon::rBookmarkData &Bookmark) = 0 ;
	virtual void __fastcall Insert(int Recno, char * Buffer) = 0 ;
	virtual void __fastcall Update(int Recno, char * PrevBuffer, char * NewBuffer) = 0 ;
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Udbfcursor */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfcursor;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfCursor
