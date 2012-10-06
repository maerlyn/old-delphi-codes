// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UDbfFieldDef.pas' rev: 4.00

#ifndef UDbfFieldDefHPP
#define UDbfFieldDefHPP

#pragma delphiheader begin
#pragma option push -w-
#include <SysUtils.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
#include <UDbfCommon.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Udbffielddef
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TDbfFieldDef;
typedef TDbfFieldDef* *PDbfFieldDef;

#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfFieldDef : public Classes::TCollectionItem 
{
	typedef Classes::TCollectionItem inherited;
	
private:
	AnsiString _FieldName;
	Db::TFieldType _FieldType;
	char _NativeFieldType;
	int _Size;
	int _Prec;
	void __fastcall _SetFieldName(AnsiString lFieldName);
	void __fastcall _SetNativeFieldType(char lFieldType);
	void __fastcall _SetFieldType(Db::TFieldType lFieldType);
	void __fastcall _SetSize(int lSize);
	void __fastcall _SetPrec(int lPrec);
	void __fastcall _BDEToNative(void);
	void __fastcall _NativeToBDE(void);
	
public:
	int Offset;
	unsigned AutoInc;
	bool Required;
	__fastcall virtual TDbfFieldDef(Classes::TCollection* Collection);
	__fastcall virtual ~TDbfFieldDef(void);
	void __fastcall _CheckSizePrec(void);
	virtual AnsiString __fastcall GetDisplayName();
	void __fastcall SetDefaultSize(void);
	bool __fastcall isBlob(void);
	
__published:
	__property AnsiString FieldName = {read=_FieldName, write=_SetFieldName};
	__property Db::TFieldType FieldType = {read=_FieldType, write=_SetFieldType, nodefault};
	__property char NativeFieldType = {read=_NativeFieldType, write=_SetNativeFieldType, nodefault};
	__property int Size = {read=_Size, write=_SetSize, nodefault};
	__property int Prec = {read=_Prec, write=_SetPrec, nodefault};
};

#pragma pack(pop)

class DELPHICLASS TDbfFieldDefs;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TDbfFieldDefs : public Classes::TCollection 
{
	typedef Classes::TCollection inherited;
	
private:
	Classes::TPersistent* _Owner;
	TDbfFieldDef* __fastcall _GetItem(int idx);
	
protected:
	DYNAMIC Classes::TPersistent* __fastcall getowner(void);
	
public:
	__fastcall TDbfFieldDefs(Classes::TPersistent* Owner);
	__property TDbfFieldDef* Items[int idx] = {read=_GetItem};
	HIDESBASE void __fastcall Add(const AnsiString Name, Db::TFieldType DataType, int Size, bool Required
		);
	TDbfFieldDef* __fastcall AddFieldDef(void);
public:
	#pragma option push -w-inl
	/* TCollection.Destroy */ inline __fastcall virtual ~TDbfFieldDefs(void) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------

}	/* namespace Udbffielddef */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Udbffielddef;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UDbfFieldDef
