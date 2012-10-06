// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfPagedFile.pas' rev: 4.00

#ifndef UDbfPagedFileHPP
#define UDbfPagedFileHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Dialogs.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfpagedfile
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum xBaseVersion { xUnknown, xClipper, xBaseIII, xBaseIV, xBaseV, xBaseVII, xFoxPro, xVisualFoxPro 
	};
#pragma option pop

typedef Exception EPagedFile;
;

#pragma option push -b-
enum TPagedFileMode { pfOpen, pfCreate };
#pragma option pop

class DELPHICLASS TPagedFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPagedFile : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TStream* _Stream;
	int _HeaderSize;
	int _RecordSize;
	int _RecordCount;
	char *_Header;
	bool _NeedRecalc;
	
protected:
	TPagedFileMode _Mode;
	bool _AutoCreate;
	bool _ReadOnly;
	AnsiString _Filename;
	virtual void __fastcall _SetRecordSize(int value);
	virtual void __fastcall _SetHeaderSize(int value);
	void __fastcall _FillHeader(Byte c);
	int __fastcall _GetRecordCount(void);
	
public:
	__fastcall TPagedFile(AnsiString lFileName, TPagedFileMode Mode, bool AutoCreate, bool ReadOnly);
	__fastcall virtual ~TPagedFile(void);
	void __fastcall Close(void);
	void __fastcall Open(TPagedFileMode Mode, bool AutoCreate, bool ReadOnly);
	void __fastcall ReadRecord(int IntRecNum, void * Buffer);
	void __fastcall WriteRecord(int IntRecNum, void * Buffer);
	virtual void __fastcall WriteHeader(void);
	void __fastcall _SetRecordCount(int value);
	void __fastcall WriteChar(Byte c);
	void __fastcall SeekPage(int page);
	__property int HeaderSize = {read=_HeaderSize, write=_SetHeaderSize, nodefault};
	__property int RecordSize = {read=_RecordSize, write=_SetRecordSize, nodefault};
	__property int RecordCount = {read=_GetRecordCount, write=_SetRecordCount, nodefault};
	__property char * Header = {read=_Header};
	__property AnsiString FileName = {read=_Filename};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Udbfpagedfile */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfpagedfile;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfPagedFile
