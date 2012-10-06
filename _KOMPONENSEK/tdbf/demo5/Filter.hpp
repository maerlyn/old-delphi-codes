// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Filter.pas' rev: 4.00

#ifndef FilterHPP
#define FilterHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Db.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Filter
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TFilterForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TFilterForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* Button1;
	Dbgrids::TDBGrid* DBGrid1;
	Stdctrls::TGroupBox* GroupBox1;
	Stdctrls::TCheckBox* cbITA;
	Stdctrls::TCheckBox* cbUSA;
	Stdctrls::TCheckBox* cbHOL;
	Stdctrls::TCheckBox* cbUK;
	Stdctrls::TCheckBox* cbGER;
	Stdctrls::TCheckBox* cbSWE;
	Stdctrls::TCheckBox* cbOTH;
	Stdctrls::TGroupBox* GroupBox2;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TEdit* Year_From;
	Stdctrls::TEdit* Year_To;
	Stdctrls::TCheckBox* cbBLANK;
	Stdctrls::TGroupBox* GroupBox3;
	Stdctrls::TRadioButton* Filter_on;
	Stdctrls::TRadioButton* Filter_off;
	void __fastcall Button1Click(System::TObject* Sender);
	void __fastcall FilterChange(System::TObject* Sender);
	void __fastcall Year_FromChange(System::TObject* Sender);
	void __fastcall Year_ToChange(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TFilterForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TFilterForm(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TFilterForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TFilterForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TFilterForm* FilterForm;

}	/* namespace Filter */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Filter;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Filter
