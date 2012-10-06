//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("WebServ.res");
USEFORM("..\WebServ1.cpp", WebServForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TWebServForm), &WebServForm);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
