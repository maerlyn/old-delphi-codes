// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfFile.pas' rev: 4.00

#ifndef UDbfFileHPP
#define UDbfFileHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfIndexFile.hpp>	// Pascal unit
#include <UDbfIndex.hpp>	// Pascal unit
#include <UDbfMemo.hpp>	// Pascal unit
#include <UDbfFieldDef.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <UDbfCursor.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <UDbfCommon.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Dialogs.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbffile
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS EDbfFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION EDbfFile : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDbfFile(const AnsiString Msg) : Sysutils::Exception(Msg) { }
		
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDbfFile(const AnsiString Msg, const System::TVarRec * 
		Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDbfFile(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDbfFile(int Ident, const System::TVarRec * Args, const 
		int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDbfFile(const AnsiString Msg, int AHelpContext) : Sysutils::Exception(
		Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDbfFile(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDbfFile(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDbfFile(int Ident, const System::TVarRec * Args
		, const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDbfFile(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

class DELPHICLASS TDbfDatabase;
class DELPHICLASS TDbfFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfDatabase : public System::TObject 
{
	typedef System::TObject inherited;
	
protected:
	Classes::TList* _DbfFiles;
	
public:
	bool TrimLeft;
	bool TrimRight;
	__fastcall TDbfDatabase(void);
	__fastcall virtual ~TDbfDatabase(void);
	void __fastcall CloseDbf(TDbfFile* &DbfFile);
	TDbfFile* __fastcall OpenDbf(AnsiString lFileName, Udbfpagedfile::TPagedFileMode Mode, bool AutoCreate
		, bool ReadOnly);
};

#pragma pack(pop)

class DELPHICLASS EDbfFileError;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION EDbfFileError : public Sysutils::Exception 
{
	typedef Sysutils::Exception inherited;
	
public:
	#pragma option push -w-inl
	/* Exception.Create */ inline __fastcall EDbfFileError(const AnsiString Msg) : Sysutils::Exception(
		Msg) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmt */ inline __fastcall EDbfFileError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size) : Sysutils::Exception(Msg, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateRes */ inline __fastcall EDbfFileError(int Ident, Extended Dummy) : Sysutils::Exception(
		Ident, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmt */ inline __fastcall EDbfFileError(int Ident, const System::TVarRec * Args
		, const int Args_Size) : Sysutils::Exception(Ident, Args, Args_Size) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateHelp */ inline __fastcall EDbfFileError(const AnsiString Msg, int AHelpContext) : 
		Sysutils::Exception(Msg, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateFmtHelp */ inline __fastcall EDbfFileError(const AnsiString Msg, const System::TVarRec 
		* Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Msg, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResHelp */ inline __fastcall EDbfFileError(int Ident, int AHelpContext) : Sysutils::Exception(
		Ident, AHelpContext) { }
	#pragma option pop
	#pragma option push -w-inl
	/* Exception.CreateResFmtHelp */ inline __fastcall EDbfFileError(int Ident, const System::TVarRec * 
		Args, const int Args_Size, int AHelpContext) : Sysutils::Exception(Ident, Args, Args_Size, AHelpContext
		) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TObject.Destroy */ inline __fastcall virtual ~EDbfFileError(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfFile : public Udbfpagedfile::TPagedFile 
{
	typedef Udbfpagedfile::TPagedFile inherited;
	
protected:
	int _cntUse;
	int _RecordBufferSize;
	Udbfpagedfile::xBaseVersion _DbfVersion;
	Udbffielddef::TDbfFieldDefs* _FieldList;
	int _CurIndex;
	Classes::TList* _Indexes;
	char *_PrevBuffer;
	bool _ShowDeleted;
	TDbfDatabase* _Database;
	void __fastcall _ConstructFieldList(void);
	bool __fastcall _HasBlob(void);
	
public:
	Udbfmemo::TDBTFile* _dbtFile;
	__fastcall TDbfFile(AnsiString lFileName, Udbfpagedfile::TPagedFileMode Mode, bool AutoCreate, bool 
		ReadOnly);
	__fastcall virtual ~TDbfFile(void);
	void __fastcall FinishCreate(Udbffielddef::TDbfFieldDefs* FieldDefs, int MemoSize);
	Udbfindexfile::TIndexFile* __fastcall GetIndexByName(AnsiString lIndexFile);
	virtual void __fastcall _SetRecordSize(int value);
	__property Udbffielddef::TDbfFieldDefs* FieldList = {read=_FieldList};
	void __fastcall OpenIndex(AnsiString IndexFileName, AnsiString IndexField, Udbfpagedfile::TPagedFileMode 
		Mode, bool AutoCreate, bool ReadOnly);
	void __fastcall CloseIndex(AnsiString IndexFileName);
	void __fastcall Release(void);
	void __fastcall Insert(char * Buffer);
	void __fastcall Update(int Recno, char * Buffer);
	virtual void __fastcall WriteHeader(void);
	void __fastcall _WriteFieldHdr(void);
	__property Udbfpagedfile::xBaseVersion DbfVersion = {read=_DbfVersion, nodefault};
	void __fastcall FastPackTable(void);
	void __fastcall PackTable(void);
	Udbffielddef::TDbfFieldDef* __fastcall GetFieldInfo(AnsiString FieldName);
	bool __fastcall GetFieldData(int Column, Db::TFieldType DataType, void * Src, void * Dst);
	void __fastcall SetFieldData(int Column, Db::TFieldType DataType, void * Src, void * Dst);
	void __fastcall IncAutoInc(void);
	void __fastcall InitRecord(char * p);
	void __fastcall PackIndex(Udbfindexfile::TIndexFile* lIndexFile);
};

#pragma pack(pop)

class DELPHICLASS TDbfCursor;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfCursor : public Udbfcursor::TVirtualCursor 
{
	typedef Udbfcursor::TVirtualCursor inherited;
	
protected:
	int _PhysicalRecno;
	
public:
	__fastcall TDbfCursor(TDbfFile* DbfFile);
	virtual bool __fastcall Next(void);
	virtual bool __fastcall Prev(void);
	virtual void __fastcall First(void);
	virtual void __fastcall Last(void);
	virtual int __fastcall GetPhysicalRecno(void);
	virtual void __fastcall SetPhysicalRecNo(int Recno);
	virtual int __fastcall GetSequentialRecordCount(void);
	virtual int __fastcall GetSequentialRecNo(void);
	virtual void __fastcall SetSequentialRecNo(int Recno);
	virtual void __fastcall GotoBookmark(const Udbfcommon::rBookmarkData &Bookmark);
	virtual void __fastcall Insert(int Recno, char * Buffer);
	virtual void __fastcall Update(int Recno, char * PrevBuffer, char * NewBuffer);
	virtual Udbfcommon::rBookmarkData __fastcall GetBookMark();
public:
	#pragma option push -w-inl
	/* TVirtualCursor.destroy */ inline __fastcall virtual ~TDbfCursor(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE bool tDbf_TrimFields;
extern PACKAGE TDbfDatabase* DbfDefaultDatabase;
extern PACKAGE int __fastcall SwapInt(const void *Value);

}	/* namespace Udbffile */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbffile;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfFile
