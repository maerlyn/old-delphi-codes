// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Calc.pas' rev: 4.00

#ifndef CalcHPP
#define CalcHPP

#pragma delphiheader begin
#pragma option push -w-
#include <DBGrids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
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

namespace Calc
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TCalcForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TCalcForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Dbctrls::TDBNavigator* DBNavigator1;
	Dbgrids::TDBGrid* DBGrid1;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TCalcForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TCalcForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TCalcForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TCalcForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TCalcForm* CalcForm;

}	/* namespace Calc */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Calc;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Calc
