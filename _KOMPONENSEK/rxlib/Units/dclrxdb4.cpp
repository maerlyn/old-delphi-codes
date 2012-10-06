//---------------------------------------------------------------------------
// Delphi VCL Extensions (RX)
// Copyright (c) 1999 Master-Bank
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl40.bpi");
USEPACKAGE("vcldb40.bpi");
USEPACKAGE("rxctl4.bpi");
USEPACKAGE("rxdb4.bpi");
USEPACKAGE("dclrx4.bpi");
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
