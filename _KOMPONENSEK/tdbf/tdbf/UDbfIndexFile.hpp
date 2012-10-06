// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfIndexFile.pas' rev: 4.00

#ifndef UDbfIndexFileHPP
#define UDbfIndexFileHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfCommon.hpp>	// Pascal unit
#include <UDbfCursor.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbfindexfile
{
//-- type declarations -------------------------------------------------------
typedef double *PDouble;

struct rMdxTag;
typedef rMdxTag *PMdxTag;

#pragma pack(push, 4)
struct rMdxTagHdr
{
	int RootPage;
	int FilePages;
	Byte KeyFormat;
	char KeyType;
	Word dummy;
	Word IndexKeyLength;
	Word MaxNbKeys;
	Word SecondKeyType;
	Word IndexKeyItemLen;
	Byte dummy2[3];
	Byte UniqueFlag;
} ;
#pragma pack(pop)

struct rNdxHdr;
typedef rNdxHdr *PNdxHdr;

#pragma pack(push, 4)
struct rNdxHdr
{
	int startpage;
	int nbPage;
	char keyformat;
	char keytype;
	Word dummy;
	Word keylen;
	Word nbkey;
	Word skeytype;
	Word keyreclen;
	Word dummy2;
	Byte dummy3;
	Byte Unique;
	char KeyDesc[232];
	Byte dummy4[256];
} ;
#pragma pack(pop)

#pragma pack(push, 4)
struct rMdxTag
{
	int pageno;
	char tagname[12];
	Byte keyformat;
	char forwardTag1;
	Byte forwardTag2;
	Byte backwardTag;
	Byte dummy;
	Byte keytype;
} ;
#pragma pack(pop)

#pragma option push -b-
enum NdxKeyType { N, C };
#pragma option pop

struct rNdxPage;
typedef rNdxPage *PNdxPage;

#pragma pack(push, 4)
struct rNdxPage
{
	int NbEntries;
	char Entries[508];
} ;
#pragma pack(pop)

struct rNdxentry;
typedef rNdxentry *PNdxentry;

#pragma pack(push, 4)
struct rNdxentry
{
	int _LowerPage;
	int RecNo;
	union
	{
		struct 
		{
			char CKey[504];
			
		};
		struct 
		{
			double NKey;
			
		};
		
	};
} ;
#pragma pack(pop)

class DELPHICLASS TIndexPage;
class DELPHICLASS TIndexFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TIndexFile : public Udbfpagedfile::TPagedFile 
{
	typedef Udbfpagedfile::TPagedFile inherited;
	
protected:
	Udbfpagedfile::xBaseVersion _IndexVersion;
	int _NbLevel;
	TIndexPage* _Root;
	TIndexPage* _Leaf;
	
public:
	int _FieldPos;
	int _FieldLen;
	bool __fastcall _Delete(void);
	void __fastcall Clear(void);
	void __fastcall Init(void);
	void __fastcall SetFieldInfo(int fieldStart, int fieldLen, AnsiString FieldDesc);
	void __fastcall AddNewLevel(void);
	__fastcall TIndexFile(AnsiString lFileName, Udbfpagedfile::TPagedFileMode Mode, bool AutoCreate, bool 
		ReadOnly);
	int __fastcall Find(int Recno, char * Buffer);
	void __fastcall Update(int Recno, char * PrevBuffer, char * NewBuffer);
	void __fastcall Insert(int Recno, char * Buffer);
	__fastcall virtual ~TIndexFile(void);
	int __fastcall GetRecordCount(void);
	bool __fastcall GotoBookmark(const Udbfcommon::rBookmarkData &IndexBookmark);
	void __fastcall GotoRecno(int Recno);
	Udbfcommon::rBookmarkData __fastcall GetBookMark();
	void __fastcall CheckPos(const Udbfcommon::rBookmarkData &IndexBookmark);
	void __fastcall First(void);
	void __fastcall Last(void);
	bool __fastcall Next(void);
	bool __fastcall Prev(void);
	int __fastcall PhysicalRecno(void);
	AnsiString __fastcall GetKey();
	int __fastcall GetSequentialRecordCount(void);
	int __fastcall GetSequentialRecno(void);
	void __fastcall SetSequentialRecNo(int Recno);
};

#pragma pack(pop)

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TIndexPage : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	TIndexFile* _IndexFile;
	int _PageNo;
	int _EntryNo;
	TIndexPage* _LowerLevel;
	TIndexPage* _UpperLevel;
	int _Weight;
	bool _Modified;
	rNdxentry *_Entry;
	rNdxPage _PageBuff;
	int _Magic;
	void __fastcall _setModified(bool v);
	__property bool Modified = {read=_Modified, write=_setModified, nodefault};
	int __fastcall LastEntryNo(void);
	bool __fastcall LocalInsert(int Recno, char * Buffer, int LowerPage);
	bool __fastcall LocalDelete(void);
	PNdxentry __fastcall GetPEntry(int vEntryNo);
	bool __fastcall RecurPrev(void);
	bool __fastcall RecurNext(void);
	void __fastcall RecurFirst(void);
	void __fastcall RecurLast(void);
	bool __fastcall RecurInsert(int Recno, char * Buffer, int LowerPage);
	
public:
	Udbfpagedfile::TPagedFile* _dbfFile;
	__fastcall TIndexPage(TIndexFile* Parent);
	__fastcall virtual ~TIndexPage(void);
	TIndexPage* __fastcall LowerLevel(void);
	void __fastcall SetPageNo(int page);
	__property TIndexPage* UpperLevel = {read=_UpperLevel};
	void __fastcall _SetEntryNo(int value);
	int __fastcall FindNearest(int Recno, char * Key);
	void __fastcall SetEntry(int Recno, char * key, int LowerPage);
	bool __fastcall Delete(void);
	__property int EntryNo = {read=_EntryNo, write=_SetEntryNo, nodefault};
	int __fastcall PhysicalRecno(void);
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const int RecEOF = 0x7fffffff;
static const Shortint RecBOF = 0x0;
static const Shortint BmInvalid = 0xffffffff;

}	/* namespace Udbfindexfile */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbfindexfile;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfIndexFile
