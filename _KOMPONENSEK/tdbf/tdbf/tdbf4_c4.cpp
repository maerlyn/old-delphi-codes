//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("tdbf4_c4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("dbf_reg.pas");
USEUNIT("UDbfCommon.pas");
USEUNIT("UDbfCursor.pas");
USEUNIT("UDbfEngine.pas");
USEUNIT("UDbfFieldDef.pas");
USEUNIT("UDbfFile.pas");
USEUNIT("UDbfIndex.pas");
USEUNIT("UDbfIndexFile.pas");
USEUNIT("UDbfMemo.pas");
USEUNIT("UDbfPagedFile.pas");
USEUNIT("UDbfStrings.pas");
USEUNIT("Dbf.pas");
USERES("Dbf.dcr");
USEPACKAGE("VCLX40.bpi");
USEPACKAGE("bcbsmp40.bpi");
USEPACKAGE("VCLDB40.bpi");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Source du paquet.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
