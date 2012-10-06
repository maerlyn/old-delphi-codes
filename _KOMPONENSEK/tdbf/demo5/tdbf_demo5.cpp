//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("tdbf_demo4.res");
USEFORMNS("simple.pas", Simple, SimpleForm);
USEFORMNS("CopyTable.pas", Copytable, CopyTableForm);
USEFORMNS("CreateTable.pas", Createtable, CreateTableForm);
USEFORMNS("EditTopics.pas", Edittopics, EditTopicsForm);
USEFORMNS("Filter.pas", Filter, FilterForm);
USEFORMNS("Index.pas", Index, IndexForm);
USEFORMNS("Main.pas", Main, MainForm);
USEFORMNS("Pack.pas", Pack, PackTableForm);
USEFORMNS("schema.pas", Schema, Schema1Form);
USEFORMNS("schema2.pas", Schema2, Schema2Form);
USEFORMNS("search.pas", Search, SearchForm);
USEFORMNS("Calc.pas", Calc, CalcForm);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
    Application->Initialize();
    Application->CreateForm(__classid(TMainForm), &MainForm);
    Application->CreateForm(__classid(TSimpleForm), &SimpleForm);
    Application->CreateForm(__classid(TCopyTableForm), &CopyTableForm);
    Application->CreateForm(__classid(TCreateTableForm), &CreateTableForm);
    Application->CreateForm(__classid(TEditTopicsForm), &EditTopicsForm);
    Application->CreateForm(__classid(TFilterForm), &FilterForm);
    Application->CreateForm(__classid(TIndexForm), &IndexForm);
    Application->CreateForm(__classid(TPackTableForm), &PackTableForm);
    Application->CreateForm(__classid(TSchema1Form), &Schema1Form);
    Application->CreateForm(__classid(TSchema2Form), &Schema2Form);
    Application->CreateForm(__classid(TSearchForm), &SearchForm);
    Application->CreateForm(__classid(TCalcForm), &CalcForm);
    Application->Run();
  }
  catch (Exception &exception)
  {
    Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
