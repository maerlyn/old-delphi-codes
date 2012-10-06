// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'EditTopics.pas' rev: 4.00

#ifndef EditTopicsHPP
#define EditTopicsHPP

#pragma delphiheader begin
#pragma option push -w-
#include <Db.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
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

namespace Edittopics
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TEditTopicsForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TEditTopicsForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Dbgrids::TDBGrid* Grid1;
	Dbctrls::TDBNavigator* DBNavigator1;
	Stdctrls::TLabel* Label2;
	Stdctrls::TButton* Button1;
	Dbctrls::TDBRichEdit* DBRichEdit1;
	Extctrls::TPanel* Panel1;
	Buttons::TSpeedButton* SpeedButton1;
	Buttons::TSpeedButton* SpeedButton2;
	Buttons::TSpeedButton* SpeedButton3;
	Buttons::TSpeedButton* SpeedButton4;
	Buttons::TSpeedButton* SpeedButton5;
	Buttons::TSpeedButton* SpeedButton6;
	Buttons::TSpeedButton* SpeedButton7;
	Buttons::TSpeedButton* SpeedButton8;
	Buttons::TSpeedButton* SpeedButton9;
	Stdctrls::TButton* Pack;
	void __fastcall Button1Click(System::TObject* Sender);
	void __fastcall SpeedButton1Click(System::TObject* Sender);
	void __fastcall DBRichEdit1Enter(System::TObject* Sender);
	void __fastcall PackClick(System::TObject* Sender);
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TEditTopicsForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TEditTopicsForm(Classes::TComponent* AOwner, 
		int Dummy) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TEditTopicsForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TEditTopicsForm(HWND ParentWindow) : Forms::TForm(
		ParentWindow) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TEditTopicsForm* EditTopicsForm;

}	/* namespace Edittopics */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Edittopics;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// EditTopics
