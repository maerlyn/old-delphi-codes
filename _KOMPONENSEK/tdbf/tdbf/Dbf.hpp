// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'dbf.pas' rev: 4.00

#ifndef dbfHPP
#define dbfHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfCommon.hpp>	// Pascal unit
#include <UDbfFieldDef.hpp>	// Pascal unit
#include <UDbfCursor.hpp>	// Pascal unit
#include <UDbfMemo.hpp>	// Pascal unit
#include <UDbfIndexFile.hpp>	// Pascal unit
#include <UDbfIndex.hpp>	// Pascal unit
#include <UDbfFile.hpp>	// Pascal unit
#include <UDbfPagedFile.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
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

namespace Dbf
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMyBlobFile;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TMyBlobFile : public Classes::TMemoryStream 
{
	typedef Classes::TMemoryStream inherited;
	
public:
	Db::TBlobStreamMode Mode;
	Db::TField* Field;
	int MemoRecno;
	int ReadSize;
	__fastcall TMyBlobFile(Db::TBlobStreamMode ModeVal, Db::TField* FieldVal);
	__fastcall virtual ~TMyBlobFile(void);
};

#pragma pack(pop)

typedef char TDbfRecordContent[4001];

struct rDbfRecord;
typedef rDbfRecord *pDbfRecord;

#pragma pack(push, 4)
struct rDbfRecord
{
	Udbfcommon::rBookmarkData BookmarkData;
	Db::TBookmarkFlag BookmarkFlag;
	char DeletedFlag;
	char Fields[4001];
} ;
#pragma pack(pop)

class DELPHICLASS TDbf;
typedef void __fastcall (__closure *TCompareRecordEvent)(TDbf* Dbf, bool &Accept);

typedef void __fastcall (__closure *TTranslateEvent)(TDbf* Dbf, char * Src, char * Dest, bool ToOem)
	;

typedef void __fastcall (__closure *TOnIndexFilter)(char * First, char * Last, bool &Accept);

#pragma option push -b-
enum TDbfStorage { stoMemory, stoAuto, stoFile };
#pragma option pop

#pragma option push -b-
enum TDbfOpenMode { omNormal, omAutoCreate, omTemporary };
#pragma option pop

class DELPHICLASS TDbfIndexCollection;
class DELPHICLASS TDbfIndexDef;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfIndexCollection : public Classes::TCollection 
{
	typedef Classes::TCollection inherited;
	
public:
	Db::TDataSet* _Owner;
	
private:
	HIDESBASE TDbfIndexDef* __fastcall GetItem(int n);
	HIDESBASE void __fastcall SetItem(int n, TDbfIndexDef* Value);
	
protected:
	DYNAMIC Classes::TPersistent* __fastcall getowner(void);
	
public:
	__fastcall TDbfIndexCollection(Db::TDataSet* Owner);
	HIDESBASE TDbfIndexDef* __fastcall Add(void);
	TDbfIndexDef* __fastcall GetIndexByFilename(AnsiString FileName);
	__property TDbfIndexDef* Items[int n] = {read=GetItem, write=SetItem/*, default*/};
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TDbfIndexCollection(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfIndexDef : public Classes::TCollectionItem 
{
	typedef Classes::TCollectionItem inherited;
	
protected:
	AnsiString _IndexDefFileName;
	void __fastcall _SetIndexDefFileName(AnsiString value);
	
public:
	AnsiString _SortField;
	__fastcall virtual TDbfIndexDef(Classes::TCollection* Collection);
	__fastcall virtual ~TDbfIndexDef(void);
	
__published:
	__property AnsiString IndexFile = {read=_IndexDefFileName, write=_SetIndexDefFileName};
	__property AnsiString SortField = {read=_SortField, write=_SortField};
};

#pragma pack(pop)

typedef TDbfIndexDef* *PIndex;

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbf : public Db::TDataSet 
{
	typedef Db::TDataSet inherited;
	
private:
	Classes::TNotifyEvent _OnCompareRecord;
	TOnIndexFilter _OnIndexFilter;
	TDbfOpenMode _OpenMode;
	TDbfStorage _Storage;
	AnsiString _TableName;
	AnsiString _RelativePath;
	AnsiString _AbsolutePath;
	AnsiString _IndexName;
	bool _ReadOnly;
	Udbfcursor::TVirtualCursor* _Cursor;
	Udbffile::TDbfFile* _DbfFile;
	char *_FilterBuffer;
	TDbfIndexCollection* _IndexDefs;
	TTranslateEvent _OnTranslate;
	void __fastcall _SetIndexname(AnsiString IndexFile);
	void __fastcall _SetDbfIndexes(TDbfIndexCollection* Value);
	void __fastcall _SetRelativePath(AnsiString value);
	void __fastcall _SetTableName(const AnsiString s);
	void __fastcall _SetVersion(AnsiString s);
	AnsiString __fastcall _ComponentInfo();
	AnsiString __fastcall _GetIndexname();
	AnsiString __fastcall _GetVersion();
	int __fastcall _GetPhysicalRecno(void);
	void __fastcall _SetShowDeleted(bool Value);
	void __fastcall _GetFieldDefsFromDbfFieldDefs(void);
	void __fastcall _CreateTableFromFieldDefs(void);
	void __fastcall _CreateTableFromFields(void);
	char * __fastcall _GetCurrentBuffer(void);
	Udbffielddef::TDbfFieldDefs* __fastcall _GetDbfFieldList(void);
	
public:
	AnsiString easyfilter;
	bool _ShowDeleted;
	void __fastcall About(void);
	void __fastcall AddIndex(const AnsiString IndexFile, const AnsiString Fields, Db::TIndexOptions Options
		);
	void __fastcall OpenIndexFile(AnsiString IndexFile);
	void __fastcall DeleteIndex(const AnsiString IndexFile);
	void __fastcall CloseIndexFile(const AnsiString IndexFile);
	void __fastcall PackTable(void);
	__property int PhysicalRecno = {read=_GetPhysicalRecno, nodefault};
	virtual bool __fastcall Locate(const AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions 
		Options);
	bool __fastcall LocateRecord(const AnsiString KeyFields, const Variant &KeyValues, Db::TLocateOptions 
		Options, bool bSyncCursor);
	__property AnsiString AbsolutePath = {read=_AbsolutePath};
	__property Udbffielddef::TDbfFieldDefs* DbfFieldList = {read=_GetDbfFieldList};
	bool __fastcall IsDeleted(void);
	void __fastcall Undelete(void);
	void __fastcall CreateTable(void);
	void __fastcall CreateTableEx(Udbffielddef::TDbfFieldDefs* DbfFieldDefs);
	HIDESBASE void __fastcall InitFieldDefsFromFields(void);
	
__published:
	__property AnsiString Version = {read=_GetVersion, write=_SetVersion};
	__property TDbfIndexCollection* Indexes = {read=_IndexDefs, write=_SetDbfIndexes};
	__property TOnIndexFilter OnIndexFilter = {read=_OnIndexFilter, write=_OnIndexFilter};
	__property AnsiString IndexName = {read=_GetIndexname, write=_SetIndexname};
	__property TDbfOpenMode OpenMode = {read=_OpenMode, write=_OpenMode, nodefault};
	__property TDbfStorage Storage = {read=_Storage, write=_Storage, nodefault};
	
public:
	virtual bool __fastcall GetFieldData(Db::TField* Field, void * Buffer)/* overload */;
	virtual Classes::TStream* __fastcall CreateBlobStream(Db::TField* Field, Db::TBlobStreamMode Mode);
		
	virtual int __fastcall Translate(char * Src, char * Dest, bool ToOem);
	virtual void __fastcall ClearCalcFields(char * Buffer);
	
protected:
	virtual char * __fastcall AllocRecordBuffer(void);
	virtual void __fastcall FreeRecordBuffer(char * &Buffer);
	virtual void __fastcall GetBookmarkData(char * Buffer, void * Data);
	virtual Db::TBookmarkFlag __fastcall GetBookmarkFlag(char * Buffer);
	virtual Db::TGetResult __fastcall GetRecord(char * Buffer, Db::TGetMode GetMode, bool DoCheck);
	virtual Word __fastcall GetRecordSize(void);
	virtual void __fastcall InternalAddRecord(void * Buffer, bool Append);
	virtual void __fastcall InternalClose(void);
	virtual void __fastcall InternalDelete(void);
	virtual void __fastcall InternalFirst(void);
	virtual void __fastcall InternalGotoBookmark(void * Bookmark);
	virtual void __fastcall InternalHandleException(void);
	virtual void __fastcall InternalInitFieldDefs(void);
	virtual void __fastcall InternalInitRecord(char * Buffer);
	virtual void __fastcall InternalLast(void);
	virtual void __fastcall InternalOpen(void);
	virtual void __fastcall InternalPost(void);
	virtual void __fastcall InternalSetToRecord(char * Buffer);
	virtual bool __fastcall IsCursorOpen(void);
	virtual void __fastcall SetBookmarkFlag(char * Buffer, Db::TBookmarkFlag Value);
	virtual void __fastcall SetBookmarkData(char * Buffer, void * Data);
	virtual void __fastcall SetFieldData(Db::TField* Field, void * Buffer);
	virtual int __fastcall GetRecordCount(void);
	virtual int __fastcall GetRecNo(void);
	virtual void __fastcall SetRecNo(int Value);
	virtual bool __fastcall GetCanModify(void);
	virtual void __fastcall SetFiltered(bool Value);
	
public:
	virtual int __fastcall CompareBookmarks(void * Bookmark1, void * Bookmark2);
	__fastcall virtual TDbf(Classes::TComponent* AOwner);
	__fastcall virtual ~TDbf(void);
	
__published:
	__property AnsiString ComponentInfo = {read=_ComponentInfo};
	__property AnsiString TableName = {read=_TableName, write=_SetTableName};
	__property AnsiString FilePath = {read=_RelativePath, write=_SetRelativePath};
	__property bool ReadOnly = {read=_ReadOnly, write=_ReadOnly, default=0};
	__property bool ShowDeleted = {read=_ShowDeleted, write=_SetShowDeleted, nodefault};
	__property Classes::TNotifyEvent OnCompareRecord = {read=_OnCompareRecord, write=_OnCompareRecord};
		
	__property Active ;
	__property Filtered ;
	__property BeforeOpen ;
	__property AfterOpen ;
	__property BeforeClose ;
	__property AfterClose ;
	__property BeforeInsert ;
	__property AfterInsert ;
	__property BeforeEdit ;
	__property AfterEdit ;
	__property BeforePost ;
	__property AfterPost ;
	__property BeforeCancel ;
	__property AfterCancel ;
	__property BeforeDelete ;
	__property AfterDelete ;
	__property BeforeScroll ;
	__property AfterScroll ;
	__property OnCalcFields ;
	__property OnDeleteError ;
	__property OnEditError ;
	__property OnFilterRecord ;
	__property OnNewRecord ;
	__property OnPostError ;
	__property TTranslateEvent OnTranslate = {read=_OnTranslate, write=_OnTranslate};
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE AnsiString _DbfExePath;

}	/* namespace Dbf */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dbf;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// dbf
