// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfMemo.pas' rev: 4.00

#ifndef UDbfMemoHPP
#define UDbfMemoHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Classes.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfmemo
{
//-- type declarations -------------------------------------------------------
struct rDbtHdr;
typedef rDbtHdr *PDbtHdr;

#pragma pack(push, 4)
struct rDbtHdr
{
	int NextBlock;
	Byte Dummy[4];
	Byte _dbfFile[8];
	Byte bVer;
	Byte Dummy2[3];
	Word BlockLen;
	Byte Dummy3[490];
} ;
#pragma pack(pop)

class DELPHICLASS TDBTFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDBTFile : public Udbfpagedfile::TPagedFile 
{
	typedef Udbfpagedfile::TPagedFile inherited;
	
protected:
	Udbfpagedfile::xBaseVersion _DbtVersion;
	
public:
	__fastcall TDBTFile(AnsiString lFileName, Udbfpagedfile::TPagedFileMode Mode, bool AutoCreate, bool 
		ReadOnly, int pRecordSize, Udbfpagedfile::xBaseVersion Ver);
	void __fastcall ReadMemo(int recno, Classes::TStream* Dst);
	void __fastcall WriteMemo(int &MemoRecno, int ReadSize, Classes::TStream* Src);
public:
	#pragma option push -w-inl
	/* TPagedFile.Destroy */ inline __fastcall virtual ~TDBTFile(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

typedef int *PInteger;

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Udbfmemo */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfmemo;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfMemo
