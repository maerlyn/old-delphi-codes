//---------------------------------------------------------------------------
// Delphi VCL Extensions (RX)
// Copyright (c) 1998 Master-Bank
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl35.bpi");
USEPACKAGE("vcldb35.bpi");
USEPACKAGE("rxctl.bpi");
USEPACKAGE("rxdb.bpi");
USEPACKAGE("dclrxctl.bpi");
USEUNIT("QBndDlg.pas");
USEUNIT("RxDBReg.pas");
USEUNIT("RxBDEReg.pas");
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
