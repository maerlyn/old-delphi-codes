//---------------------------------------------------------------------------
// Delphi VCL Extensions (RX)
// Copyright (c) 1999 Master-Bank
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("rxctl4.bpi");
USEPACKAGE("vcldb40.bpi");
USEUNIT("RxDBCtrl.pas");
USEUNIT("RxLookup.pas");
USEUNIT("DBQBE.pas");
USEUNIT("DBFilter.pas");
USEUNIT("DBLists.pas");
USEUNIT("DBIndex.pas");
USEUNIT("DBPrgrss.pas");
USEUNIT("DBSecur.pas");
USEUNIT("RxRemLog.pas");
USEUNIT("RxLogin.pas");
USEUNIT("LoginDlg.pas");
USEUNIT("RxDConst.pas");
USEUNIT("ChPswDlg.pas");
USEUNIT("DBUtils.pas");
USEUNIT("BdeUtils.pas");
USEUNIT("RxQuery.pas");
USEUNIT("RxDBComb.pas");
USEUNIT("DBRichEd.pas");
USEUNIT("MemTable.pas");
USEUNIT("DBExcpt.pas");
USEUNIT("RxMemDS.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
