program tdbf_demo;

uses
  Forms,
  EditTopics in 'EditTopics.pas' {EditTopicsForm},
  Filter in 'Filter.pas' {FilterForm},
  Index in 'Index.pas' {IndexForm},
  Main in 'Main.pas' {MainForm},
  Schema in 'schema.pas' {Schema1Form},
  Schema2 in 'schema2.pas' {Schema2Form},
  Search in 'search.pas' {SearchForm},
  Simple in 'simple.pas' {SimpleForm},
  Pack in 'Pack.pas' {PackTableForm},
  CopyTable in 'CopyTable.pas' {CopyTableForm},
  dbf in '..\tdbf5\Dbf.pas',
  UDbfIndex in '..\tdbf5\UDbfIndex.pas',
  UDbfPagedFile in '..\tdbf5\UDbfPagedFile.pas',
  UDbfFile in '..\tdbf5\UDbfFile.pas',
  UDbfMemo in '..\tdbf5\UDbfMemo.pas',
  UDbfStrings in '..\tdbf5\UDbfStrings.pas',
  UDbfFieldDef in '..\tdbf5\UDbfFieldDef.pas',
  UDbfCursor in '..\tdbf5\UDbfCursor.pas',
  UDbfCommon in '..\tdbf5\UDbfCommon.pas',
  UDbfIndexFile in '..\tdbf5\UDbfIndexFile.pas',
  CreateTable in 'CreateTable.pas' {CreateTableForm},
  multipleuse in 'multipleuse.pas' {MultipleUseForm},
  Compatibility in 'Compatibility.pas' {CompatibilityForm},
  Calc in 'Calc.pas' {CalcForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEditTopicsForm, EditTopicsForm);
  Application.CreateForm(TFilterForm, FilterForm);
  Application.CreateForm(TIndexForm, IndexForm);
  Application.CreateForm(TSchema1Form, Schema1Form);
  Application.CreateForm(TSchema2Form, Schema2Form);
  Application.CreateForm(TSearchForm, SearchForm);
  Application.CreateForm(TSimpleForm, SimpleForm);
  Application.CreateForm(TPackTableForm, PackTableForm);
  Application.CreateForm(TCopyTableForm, CopyTableForm);
  Application.CreateForm(TCreateTableForm, CreateTableForm);
  Application.CreateForm(TMultipleUseForm, MultipleUseForm);
  Application.CreateForm(TCompatibilityForm, CompatibilityForm);
  Application.CreateForm(TCalcForm, CalcForm);
  Application.Run;
end.
