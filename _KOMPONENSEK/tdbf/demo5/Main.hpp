// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Main.pas' rev: 4.00

#ifndef MainHPP
#define MainHPP

#pragma delphiheader begin
#pragma option push -w-
#include <UDbfCommon.hpp>	// Pascal unit
#include <dbf.hpp>	// Pascal unit
#include <ComCtrls.hpp>	// Pascal unit
#include <Buttons.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <DBGrids.hpp>	// Pascal unit
#include <Grids.hpp>	// Pascal unit
#include <DBCtrls.hpp>	// Pascal unit
#include <ExtCtrls.hpp>	// Pascal unit
#include <Db.hpp>	// Pascal unit
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

namespace Main
{
//-- type declarations -------------------------------------------------------
class DELPHICLASS TMainForm;
#pragma pack(push, 4)
class PASCALIMPLEMENTATION TMainForm : public Forms::TForm 
{
	typedef Forms::TForm inherited;
	
__published:
	Stdctrls::TButton* DemoButton;
	Dbf::TDbf* DbfDemo;
	Db::TDataSource* DataSourceDemo;
	Dbf::TDbf* DbfDisco;
	Db::TDataSource* DatasourceDisco;
	Stdctrls::TButton* Button1;
	Dbctrls::TDBNavigator* DBNavigator1;
	Dbctrls::TDBText* DBText1;
	Dbgrids::TDBGrid* DBGrid1;
	Extctrls::TBevel* Bevel1;
	Extctrls::TBevel* Bevel2;
	Stdctrls::TLabel* Label1;
	Extctrls::TBevel* Bevel3;
	Extctrls::TImage* Image1;
	Dbctrls::TDBRichEdit* DBRichEdit1;
	Db::TStringField* DbfDiscoAUTHOR;
	Db::TStringField* DbfDiscoTITLE;
	Db::TStringField* DbfDiscoCOMPANY;
	Db::TStringField* DbfDiscoCOUNTRY;
	Db::TSmallintField* DbfDiscoYEAR;
	Db::TFloatField* DbfDiscoPRICE;
	Db::TStringField* DbfDiscoNOTE;
	Db::TSmallintField* DbfDiscoQTY;
	Db::TStringField* DbfDemoID;
	Db::TStringField* DbfDemoTITLE;
	Db::TMemoField* DbfDemoDESCR;
	Db::TStringField* DbfDemoDEMO;
	Db::TCurrencyField* DbfDiscoCALCPRICE;
	void __fastcall DbfDemoAfterScroll(Db::TDataSet* DataSet);
	void __fastcall DemoButtonClick(System::TObject* Sender);
	void __fastcall DbfDiscoFilterRecord(Db::TDataSet* DataSet, bool &Accept);
	void __fastcall ButtonCloseClick(System::TObject* Sender);
	void __fastcall FormShow(System::TObject* Sender);
	void __fastcall DBGrid1MouseUp(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	void __fastcall DbfDiscoCalcFields(Db::TDataSet* DataSet);
	void __fastcall FormCreate(System::TObject* Sender);
	void __fastcall DataSourceDemoStateChange(System::TObject* Sender);
	void __fastcall Image1MouseDown(System::TObject* Sender, Controls::TMouseButton Button, Classes::TShiftState 
		Shift, int X, int Y);
	
public:
	Forms::TForm* lastForm;
public:
	#pragma option push -w-inl
	/* TCustomForm.Create */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner) : Forms::TForm(
		AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.CreateNew */ inline __fastcall virtual TMainForm(Classes::TComponent* AOwner, int Dummy
		) : Forms::TForm(AOwner, Dummy) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TCustomForm.Destroy */ inline __fastcall virtual ~TMainForm(void) { }
	#pragma option pop
	
public:
	#pragma option push -w-inl
	/* TWinControl.CreateParented */ inline __fastcall TMainForm(HWND ParentWindow) : Forms::TForm(ParentWindow
		) { }
	#pragma option pop
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TMainForm* MainForm;

}	/* namespace Main */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Main;
#endif
#pragma option pop	// -w-

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Main
