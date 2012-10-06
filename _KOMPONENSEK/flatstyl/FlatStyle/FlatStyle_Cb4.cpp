//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("FlatStyle_Cb4.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("TFlatTabControlUnit.pas");
USEUNIT("FlatUtilitys.pas");
USEUNIT("HSLUtils.pas");
USEUNIT("TFlatAnimationUnit.pas");
USEUNIT("TFlatAnimWndUnit.pas");
USEUNIT("TFlatButtonUnit.pas");
USEUNIT("TFlatCheckBoxUnit.pas");
USEUNIT("TFlatComboBoxUnit.pas");
USEUNIT("TFlatEditUnit.pas");
USEUNIT("TFlatGaugeUnit.pas");
USEUNIT("TFlatHintUnit.pas");
USEUNIT("TFlatListBoxUnit.pas");
USEUNIT("TFlatMemoUnit.pas");
USEUNIT("TFlatProgressBarUnit.pas");
USEUNIT("TFlatRadioButtonUnit.pas");
USEUNIT("TFlatRegister.pas");
USERES("TFlatRegister.dcr");
USEUNIT("TFlatSoundUnit.pas");
USEUNIT("TFlatSpeedButtonUnit.pas");
USEUNIT("TFlatSplitterUnit.pas");
USEUNIT("FlatGraphics.pas");
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
