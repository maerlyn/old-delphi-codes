// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Simple.pas' rev: 4.00

#ifndef SimpleHPP
#define SimpleHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Buttons.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Mask.hpp>	// Pascal unit
#include <StdCtrls.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
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

namespace Simple
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSimpleForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TSimpleForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* Button1;
	Dbgrids::TDBGrid* DBGrid1;
	Stdctrls::TGroupBox* GroupBox1;
	Stdctrls::TLabel* Label1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TLabel* Label3;
	Stdctrls::TLabel* Label4;
	Stdctrls::TLabel* Label5;
	Stdctrls::TLabel* Label6;
	Stdctrls::TLabel* Label7;
	Stdctrls::TLabel* Label8;
	Dbctrls::TDBEdit* DBEdit1;
	Dbctrls::TDBEdit* DBEdit2;
	Dbctrls::TDBEdit* DBEdit3;
	Dbctrls::TDBEdit* DBEdit4;
	Dbctrls::TDBEdit* DBEdit5;
	Dbctrls::TDBEdit* DBEdit6;
	Dbctrls::TDBEdit* DBEdit7;
	Dbctrls::TDBEdit* DBEdit8;
	Stdctrls::TGroupBox* GroupBox2;
	Stdctrls::TLabel* Label9;
	Stdctrls::TLabel* Label10;
	Dbctrls::TDBNavigator* DBNavigator1;
	Dbctrls::TDBNavigator* DBNavigator2;
	Dbctrls::TDBNavigator* DBNavigator3;
	Stdctrls::TLabel* Label11;
	void __fastcall Button1Click(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TSimpleForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSimpleForm(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TSimpleForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TSimpleForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TSimpleForm* SimpleForm;

}	/* namespace Simple */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Simple;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Simple
