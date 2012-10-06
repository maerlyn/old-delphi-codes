unit dbf_reg;
{===============================================================================
||         TDbf Component         ||         http://tdbf.netfirms.com         ||
===============================================================================}
(*
  tDBF is supplied as is. The author disclaims all warranties,
  expressed or implied, including, without limitation, the warranties of
  merchantability and of fitness for any purpose. The author assumes no
  liability for damages, direct or consequential, which may result from the
  use of MyDBF.

  You are allowed to use this component in any project free of charge.
  You are
    NOT allowed to claim that you have created this component.  You are
    NOT allowed to copy this component's code into your own component and
  claim that the code is your idea.

*)
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,DsgnIntf,ExptIntf,
  Dbf,
  UDbfPagedFile, UDbfFile, UDbfIndex, UDbfMemo,
  UDbfCursor, UDbfEngine, UDbfFieldDef,UDbfCommon
  ;

{$i _dbfCommon.inc}
//====================================================================
// Delphi is a bit to permissive for me,  I mean protected doesn't work within
// one unit. So i decided that convention:
//    private member begins by '_'
// It's forbidden to access any '_something' except from the class where it
// is defined. To check that, I just have to look for '._' anywhere in the code.
//====================================================================

procedure Register;

implementation

//==========================================================
//============ DESIGNONLY =================================
//==========================================================

//==========================================================
//============ TTableNameProperty
//==========================================================
type
  TTableNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TTableNameProperty.Edit; {override;}
var
  FileOpen: TOpenDialog;
  Dbf: TDbf;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    with fileopen do begin
      Dbf:=GetComponent(0) as TDbf;
      FileOpen.InitialDir:=Dbf.AbsolutePath;
      Filename := GetValue;
      Filter := 'Dbf table|*.dbf';
      if Execute then begin
        SetValue(Filename);
      end;
    end;
  finally
    Fileopen.free;
  end;
end;

function TTableNameProperty.GetAttributes: TPropertyAttributes; {override;}
begin
  Result := [paDialog, paRevertable];
end;

//==========================================================
//============ TIndexFileNameProperty
//==========================================================

type
  TIndexFileNameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TIndexFileNameProperty.Edit; {override;}
var
  FileOpen: TOpenDialog;
  IndexDef: TDbfIndexDef;
  Indexes: TDbfIndexCollection;
  Dbf:TDbf;
begin
  FileOpen := TOpenDialog.Create(Application);
  try
    with fileopen do begin
      IndexDef:=GetComponent(0) as TDbfIndexDef;
      Indexes:=TDbfIndexCollection(IndexDef.Collection);
      Dbf:=TDbf(Indexes._Owner);
      FileOpen.InitialDir := Dbf.AbsolutePath;
      Filename := GetValue;
			Filter := 'Simple index (ndx)|*.ndx|Multiple index (mdx)|*.mdx';
			if Execute then begin
        SetValue(ExtractFileName(Filename));
      end;
    end;
  finally
    Fileopen.free;
  end;
end;

function TIndexFileNameProperty.GetAttributes: TPropertyAttributes; {override;}
begin
  Result := [paDialog, paRevertable];
end;

//==========================================================
//============ TSortFieldProperty
//==========================================================

type
  TSortFieldProperty = class(TStringProperty)
  public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
	end;


function TSortFieldProperty.GetAttributes: TPropertyAttributes; {override;}
begin
	Result := [paValueList, paSortList, paRevertable];
end;

procedure TSortFieldProperty.GetValues(Proc: TGetStrProc);
var
  IndexDef: TDbfIndexDef;
	Indexes: TDbfIndexCollection;
	Dbf:TDbf;
	i:integer;
begin
	IndexDef:=GetComponent(0) as TDbfIndexDef;
	Indexes:=TDbfIndexCollection(IndexDef.Collection);
	Dbf:=TDbf(Indexes._Owner);
	for i:=0 to Dbf.FieldCount-1 do begin
		Proc(Dbf.Fields[i].FieldName);
	end;
end;
//==========================================================
//============ TIndexNameProperty
//==========================================================

type
  TIndexNameProperty = class(TStringProperty)
  public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;


function TIndexNameProperty.GetAttributes: TPropertyAttributes; {override;}
begin
	Result := [paValueList, paRevertable];
end;

procedure TIndexNameProperty.GetValues(Proc: TGetStrProc);
var
	Dbf:TDbf;
	i:integer;
begin
	Dbf:=GetComponent(0) as TDbf;
	for i:=0 to Dbf.Indexes.Count-1 do begin
		Proc(Dbf.Indexes[i].IndexFile);
	end;
end;

procedure TIndexNameProperty.SetValue(const Value: string); {override}
var
	Dbf:TDbf;
begin
	Dbf:=GetComponent(0) as TDbf;
  Dbf.Indexname:=Value;
end;

function TIndexNameProperty.GetValue: string; {override;}
var
	Dbf:TDbf;
begin
	Dbf:=GetComponent(0) as TDbf;
  Result:=Dbf.Indexname;
end;
//==========================================================
//============ TVersionProperty
//==========================================================
type
  TVersionProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TVersionProperty.Edit; {override;}
var
  Dbf: TDbf;
begin
  Dbf:=GetComponent(0) as TDbf;
  ShowMessage(
    DBf.Version + ' : a dBase component'+#13+
    'for Delphi and c++ builder with no BDE.'+#13+
    #13 +
    'To get the latest version, please visit'+#13+
    'the website : http://tdbf.netfirms.com');
end;

function TVersionProperty.GetAttributes: TPropertyAttributes; {override;}
begin
  Result := [paDialog, paReadOnly, paRevertable];
end;

//==========================================================
//============ TNativeFieldTypeProperty
//==========================================================
type
  TNativeFieldTypeProperty = class(TCharProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value: string); override;
  end;

procedure TNativeFieldTypeProperty.SetValue(const Value: string);
var
  L: Longint;
begin
  if Length(Value) = 0 then L := 0 else
  if Value[1] = '#' then L := StrToInt(Copy(Value, 2, Maxint))
  else L := Ord(Value[1]);
  SetOrdValue(L);
end;

function TNativeFieldTypeProperty.GetAttributes: TPropertyAttributes; {override;}
begin
  result:=[paRevertable,paValueList];
end;

procedure TNativeFieldTypeProperty.GetValues(Proc: TGetStrProc);
begin
	Proc('C Character');
	Proc('N Numeric');
	Proc('D Date');
	Proc('L Logical');
	Proc('M Memo');
	Proc('B Blob');
	Proc('F Float');
	Proc('O Double');
	Proc('I Integer');
	Proc('G Graphic');
	Proc('+ AutoIncrement');
	Proc('@ DateTime');
end;

//==========================================================
//============ initialization
//==========================================================
procedure Register;
begin
	RegisterComponents('Goodies', [TDbf]);
	RegisterPropertyEditor(TypeInfo(string), TDbf, 'TableName', TTableNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbf, 'Version', TVersionProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbf, 'IndexFile', TIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbfIndexDef, 'IndexFile', TIndexFileNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TDbfIndexDef, 'SortField', TSortFieldProperty);
  RegisterPropertyEditor(TypeInfo(char), TDbfFieldDef, 'NativeFieldType', TNativeFieldTypeProperty);
end;


initialization

  _DbfExePath:=ExtractFilePath(ToolServices.GetProjectName);

finalization

end.



