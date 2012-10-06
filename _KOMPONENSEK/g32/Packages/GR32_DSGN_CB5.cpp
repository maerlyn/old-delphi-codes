//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEFORMNS("..\G32_Dsgn_Bitmap.pas", G32_Dsgn_Bitmap, TPictureEditorForm);
USEUNIT("..\G32_Dsgn_Color.pas");
USEUNIT("..\G32_Reg.pas");
USERES("..\G32_Reg.dcr");
USEPACKAGE("dclstd50.bpi");
USEPACKAGE("GR32_CB5.bpi");
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
