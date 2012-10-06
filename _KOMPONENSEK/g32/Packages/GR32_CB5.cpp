//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USEPACKAGE("vcl50.bpi");
USEUNIT("..\G32_Transforms.pas");
USEUNIT("..\G32_Blend.pas");
USEUNIT("..\G32_ByteMaps.pas");
USEUNIT("..\G32_CPUid.pas");
USEUNIT("..\G32_DrawingEx.pas");
USEUNIT("..\G32_Filters.pas");
USEUNIT("..\G32_Image.pas");
USEUNIT("..\G32_Layers.pas");
USEUNIT("..\G32_LowLevel.pas");
USEUNIT("..\G32_Polygons.pas");
USEUNIT("..\G32_RangeBars.pas");
USEUNIT("..\G32.pas");
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
