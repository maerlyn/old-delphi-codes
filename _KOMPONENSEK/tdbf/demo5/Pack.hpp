// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Pack.pas' rev: 4.00

#ifndef PackHPP
#define PackHPP

#pragma delphiheader begin
#pragma option push -w-
#include <StdCtrls.hpp>	// Pascal unit
#include <dbf.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Pack
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TPackTableForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TPackTableForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Dbgrids::TDBGrid* DBGrid1;
	Dbctrls::TDBNavigator* DBNavigator1;
	Db::TDataSource* DataSource1;
	Dbf::TDbf* Dbf1;
	Extctrls::TPanel* Panel1;
	Stdctrls::TButton* Button2;
	Stdctrls::TButton* Button3;
	Db::TStringField* Dbf1Field1;
	Stdctrls::TGroupBox* GroupBox1;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* labnormal;
	Stdctrls::TLabel* labdeleted;
	Stdctrls::TButton* Button5;
	Stdctrls::TCheckBox* ShowDeleted;
	Db::TBooleanField* Dbf1Deleted;
	Dbctrls::TDBMemo* DBMemo1;
	Extctrls::TSplitter* Splitter1;
	Db::TMemoField* Dbf1Field2;
	Stdctrls::TGroupBox* GroupBox2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TLabel* Label4;
	Stdctrls::TLabel* Label5;
	Stdctrls::TLabel* Label6;
	Db::TFloatField* Dbf1Field3;
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall ClearTableClick(System::TObject* Sender);
	void __fastcall Button2Click(System::TObject* Sender);
	void __fastcall Button3Click(System::TObject* Sender);
	void __fastcall Button5Click(System::TObject* Sender);
	void __fastcall RefreshInfo(System::TObject* Sender);
	void __fastcall Dbf1AfterDelete(Db::TDataSet* DataSet);
	void __fastcall ShowDeletedClick(System::TObject* Sender);
	void __fastcall Dbf1AfterPost(Db::TDataSet* DataSet);
	void __fastcall Dbf1CalcFields(Db::TDataSet* DataSet);
	
public:
	bool batchmode;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TPackTableForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TPackTableForm(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TPackTableForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TPackTableForm(HWND ParentWindow) : Forms::TForm(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TPackTableForm* PackTableForm;

}	/* namespace Pack */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Pack;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Pack
