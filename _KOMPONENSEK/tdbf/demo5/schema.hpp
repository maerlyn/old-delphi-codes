// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Schema.pas' rev: 4.00

#ifndef SchemaHPP
#define SchemaHPP

#pragma delphiheader begin
#pragma option push -w-
#include <StdCtrls.hpp>	// Pascal unit
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

namespace Schema
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TSchema1Form;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TSchema1Form : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Extctrls::TImage* Image1;
	Stdctrls::TButton* Button1;
	Stdctrls::TLabel* Label1;
	void __fastcall Button1Click(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TSchema1Form(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TSchema1Form(Classes::TComponent* AOwner, int 
		Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TSchema1Form(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TSchema1Form(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TSchema1Form* Schema1Form;

}	/* namespace Schema */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Schema;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Schema
