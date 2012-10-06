unit dbf;
{ info in dbf_reg.pas }
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,
  UDbfPagedFile, UDbfFile, UDbfIndex, UDbfIndexFile, UDbfMemo,
  UDbfCursor, UDbfFieldDef,UDbfCommon
  ;
// If you got a compilation error here or asking for dsgntf.pas, then just add
// this file in your project:
// dsgnintf.pas in 'C:\Program Files\Borland\Delphi5\Source\Toolsapi\dsgnintf.pas'

{$i _dbfCommon.inc}
type

//====================================================================
//====================================================================
	TDbf = class;
//====================================================================
  TMyBlobFile = class(TMemoryStream)
  public
    Mode: TBlobStreamMode;
    Field:TField;
    MemoRecno:Integer;
    ReadSize:Integer;
    constructor Create(ModeVal:TBlobStreamMode; FieldVal:TField);
    destructor destroy;  override;
  end;
//====================================================================
  TDbfRecordContent = array[0..4000] of char;

  pDbfRecord = ^rDbfRecord;
  rDbfRecord = record
    BookmarkData: rBookmarkData;
    BookmarkFlag: TBookmarkFlag;
    DeletedFlag : char;
    Fields: TDbfRecordContent;
  end;
//====================================================================
	TCompareRecordEvent = procedure(Dbf: TDbf;
		var Accept: Boolean) of object;

	TTranslateEvent = procedure(Dbf: TDbf; Src, Dest: PChar; ToOem: Boolean) of object;

	TOnIndexFilter = procedure(First,Last:PChar;var Accept: Boolean) of object;

  TDbfStorage = (stoMemory,stoAuto,stoFile);
  TDbfOpenMode = (omNormal,omAutoCreate,omTemporary);
// and at LEAST the most useful class : TDbf
//====================================================================
  TDbfIndexDef = class;

  TDbfIndexCollection = class(TCollection)
  public
  	_Owner : TDataset;
	 private
    function GetItem(n:integer) : TDbfIndexDef;
		procedure SetItem(n:integer;Value:TDbfIndexDef);
	 protected
		function getowner : Tpersistent; override;
	 public
		constructor Create(Owner:TDataset);
		function Add : TDbfIndexDef;
    function GetIndexByFilename(FileName:string):TDbfIndexDef;
		property Items[n:integer]:TDbfIndexDef read GetItem write SetItem; default;
  end;
//====================================================================
	TDbfIndexDef = class(TCollectionItem)
  protected
		_IndexDefFileName: string;
    procedure _SetIndexDefFileName(value:string);
	public
		_SortField: string;
    constructor Create(Collection: TCollection); override;
		destructor Destroy; override;
  published
    property IndexFile:string read _IndexDefFileName write _SetIndexDefFileName;
    property SortField:string read _SortField write _SortField;
	end;
//====================================================================
  PIndex = ^TDbfIndexDef;
//====================================================================
	TDbf = class(TDataSet)
	private
		_OnCompareRecord:TNotifyEvent;
		_OnIndexFilter:TOnIndexFilter;
    _OpenMode:TDbfOpenMode;
    _Storage:TDbfStorage;
		_TableName: string;    // table path and file name
		_RelativePath: string;
    _AbsolutePath: string;
    _IndexName:string;
    _ReadOnly:boolean;
		_Cursor:TVirtualCursor;
    _DbfFile:TDbfFile;
		_FilterBuffer:pchar;
    _IndexDefs:TDbfIndexCollection;
    _OnTranslate:TTranslateEvent;

		procedure _SetIndexname(IndexFile: string);
		procedure _SetDbfIndexes(Value:TDbfIndexCollection);
    procedure _SetRelativePath(value:string);
		procedure _SetTableName(const s:string);
    procedure _SetVersion(s: string);

    function _ComponentInfo:string;
    function _GetIndexname: string;
    function _GetVersion: string;
    function _GetPhysicalRecno:integer;

		procedure _SetShowDeleted(Value: boolean);
    procedure _GetFieldDefsFromDbfFieldDefs;
    procedure _CreateTableFromFieldDefs;
    procedure _CreateTableFromFields;
    function  _GetCurrentBuffer:pchar;
    function  _GetDbfFieldList:TDbfFieldDefs;
	public
		{ my own methods and properties}
		{ most looks like ttable functions but they are not tdataset related
     I use the same syntax to facilitate the conversion between bde and tdbf  }
    easyfilter:string;
    _ShowDeleted:boolean;

    procedure About;
{$ifdef DELPHI_5}
		procedure AddIndex(const IndexFile, Fields: String; Options: TIndexOptions; const DescFields: String='');
{$else}
		procedure AddIndex(const IndexFile, Fields: String; Options: TIndexOptions);
{$endif}

// Index Support (use same syntax that tTable but is not related)
		procedure OpenIndexFile(IndexFile:string);
    procedure DeleteIndex(const IndexFile: string);
    procedure CloseIndexFile(const IndexFile: string);
    procedure PackTable;
		property PhysicalRecno:integer read _GetPhysicalRecno;
    function Locate (const KeyFields : String; const KeyValues : Variant; Options : TLocateOptions) : Boolean; override;
    function LocateRecord (const KeyFields : String; const KeyValues : Variant; Options : TLocateOptions; bSyncCursor : Boolean) : Boolean;
    property AbsolutePath: string read _AbsolutePath;
// new version 4.009
    property DbfFieldList: TDbfFieldDefs read _GetDbfFieldList;
    function IsDeleted:boolean;
    procedure Undelete;
    procedure CreateTable;
    procedure CreateTableEx(DbfFieldDefs:TDbfFieldDefs);
{$ifndef DELPHI_5}
    procedure InitFieldDefsFromFields;
{$endif}
  published
    property Version:string read _GetVersion write _SetVersion;
		property Indexes:TDbfIndexCollection read _IndexDefs write _SetDbfIndexes;
		property OnIndexFilter:TOnIndexFilter read _OnIndexFilter write _OnIndexFilter;
    property IndexName:string read _GetIndexname write _SetIndexname;
		property OpenMode:TDbfOpenMode read _OpenMode write _OpenMode;
    property Storage:TDbfStorage read _Storage write _Storage;
  public
    { abstract methods }
		function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override; {virtual abstract}
    {virtual methods (mostly optionnal) }
		function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override; {virtual}
{$ifdef DELPHI_4}
    function Translate(Src, Dest: PChar; ToOem: Boolean): Integer; override; {virtual}
{$else}
    procedure Translate(Src, Dest: PChar; ToOem: Boolean); override; {virtual}
{$endif}
    procedure ClearCalcFields(Buffer : PChar); override;
  protected
    { abstract methods }
    function AllocRecordBuffer: PChar; override; {virtual abstract}
		procedure FreeRecordBuffer(var Buffer: PChar); override; {virtual abstract}
		procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override; {virtual abstract}
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override; {virtual abstract}
		function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override; {virtual abstract}
		function GetRecordSize: Word; override; {virtual abstract}
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override; {virtual abstract}
    procedure InternalClose; override; {virtual abstract}
    procedure InternalDelete; override; {virtual abstract}
		procedure InternalFirst; override; {virtual abstract}
    procedure InternalGotoBookmark(Bookmark: Pointer); override; {virtual abstract}
		procedure InternalHandleException; override; {virtual abstract}
    procedure InternalInitFieldDefs; override; {virtual abstract}
    procedure InternalInitRecord(Buffer: PChar); override; {virtual abstract}
    procedure InternalLast; override; {virtual abstract}
    procedure InternalOpen; override; {virtual abstract}
    procedure InternalPost; override; {virtual abstract}
    procedure InternalSetToRecord(Buffer: PChar); override; {virtual abstract}
    function IsCursorOpen: Boolean; override; {virtual abstract}
		procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override; {virtual abstract}
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override; {virtual abstract}
    procedure SetFieldData(Field: TField; Buffer: Pointer); override; {virtual abstract}
		{virtual methods (mostly optionnal) }

		function GetRecordCount: Integer; override; {virtual}
		function GetRecNo: Integer; override; {virtual}
		procedure SetRecNo(Value: Integer); override; {virual}
    function GetCanModify: Boolean; override; {virtual}
    procedure SetFiltered(Value: Boolean); override; {virtual;}
  public
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

	published
    property ComponentInfo: string  read _ComponentInfo;
    property TableName: string  read _TableName write _SetTableName;
    property FilePath: string  read _RelativePath write _SetRelativePath;
    property ReadOnly : Boolean read _ReadOnly write _Readonly default False;
    property ShowDeleted:boolean read _ShowDeleted write _SetShowDeleted;
		property OnCompareRecord:TNotifyEvent read _OnCompareRecord write _OnCompareRecord;
		// redeclared data set properties
    property Active;
    property Filtered;
    property BeforeOpen;
		property AfterOpen;
    property BeforeClose;
		property AfterClose;
		property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
		property BeforePost;
    property AfterPost;
		property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
		property OnPostError;
    property OnTranslate:TTranslateEvent read _OnTranslate write _OnTranslate;
	end;

var
  _DbfExePath:string;

implementation

uses
  UDbfStrings;
// OH 2000-11-15 load the icon resource
//$R dbf.dcr

//====================================================================
// Some types and consts which are not useful in the interface.
//====================================================================

type
 	EDbfError = class (Exception);

//====================================================================
function _ExpandPath(base,path:string):string;
begin
  if ((length(path)>=1) and (path[1]='\'))
    or ((length(path)>=2) and (path[2]=':')) then begin
    // if the _FilePath is absolute...
    // it is either \ or \blahblah or c:\
    base:=path;
  end else begin
    path:= base + path;
  end;
  path:=ExpandFileName(path);
  if (length(path)>0)
     and (path[length(path)]<>'\') then begin
     path:=path+'\';
  end;
  Result:=UpperCase(path);
end;

//====================================================================
function _ExpandFileName(base:string;FileName:string):string;
var
  lpath:string;
  lfile:string;
begin
  lpath:=_ExpandPath(base,ExtractFilePath(FileName));
  lfile:=ExtractFileName(FileName);
  lpath:=lpath+lfile;
  UpperCase(lpath);
  result:=lpath;
end;

//====================================================================
procedure TDbf._SetRelativePath(value:string);
begin
	CheckInactive;
  _RelativePath:=value;
  _AbsolutePath:=_ExpandPath(_DbfExePath,_RelativePath);
end;

procedure TDbf._SetTableName(const s:string);
var
  lpath:string;
begin
  _TableName:=ExtractFileName(s);
  lpath:=ExtractFilePath(s);
  if length(lpath)>0 then begin
    lpath:=ExtractRelativePath(_DbfExePath,lpath);
    FilePath:=lpath;
  end;
end;

procedure TDbf._SetDbfIndexes(Value:TDbfIndexCollection);
begin
	_IndexDefs.Assign(Value);
end;

function TDbf._ComponentInfo:string;
begin
  Result:=Format(STRING_VERSION,[_MAJOR_VERSION,_MINOR_VERSION]);
end;

function TDbf._GetIndexname: string;
begin
  result:=_IndexName;
end;

//==========================================================
//============ TMyBlobFile
//==========================================================
constructor TMyBlobFile.Create(ModeVal:TBlobStreamMode;FieldVal:TField);
begin
  Mode:=ModeVal;
  Field:=FieldVal;
end;

destructor TMyBlobFile.destroy;
var
  Dbf:TDbf;
begin
  if (Mode=bmWrite) then begin
    Size:=Position; // Strange but it leave tailing trash bytes if I do not write that.
    Dbf:=TDbf(Field.DataSet);
    Dbf._DbfFile._dbtFile.WriteMemo(MemoRecno,ReadSize,Self);

    Dbf._DbfFile.SetFieldData(Field.FieldNo-1,
      ftInteger,@MemoRecno,@pDbfRecord(TDbf(Field.DataSet).ActiveBuffer).deletedflag);
    // seems not bad
    Dbf.SetModified(true);
    // but would that be better
    //if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
    //  DataEvent(deFieldChange, Longint(Field));
    //end;
  end;
	inherited Destroy;
end;

//====================================================================
// TDbf = TDataset Descendant.
//====================================================================
constructor TDbf.Create(AOwner: TComponent); {override;}
begin
	inherited create(aOwner);
//  _DbfFile := TDbfEngine.Create(Self);

	BookmarkSize:=sizeof(rBookmarkData)*1;
  _IndexDefs:= TDbfIndexCollection.Create(self);
  _Storage := stoFile;
  _OpenMode := omAutoCreate;

  _IndexName:='';
  _SetRelativePath('');
  _OnTranslate:=nil;
end;

destructor TDbf.Destroy; {override;}
//var
//	i:integer;
begin
	_IndexName:='';
	inherited Destroy;
  FreeAndNil(_IndexDefs);
end;

procedure TDbf.About;
begin
  ShowMessageFmt(STRING_ABOUT,[_MAJOR_VERSION,_MINOR_VERSION]);
end;

function TDbf.AllocRecordBuffer: PChar; {override virtual abstract from TDataset}
var
  p:pointer;
begin
  GetMem(p,sizeof(rDbfRecord)-sizeof(TDbfRecordContent)+_DbfFile.RecordSize+CalcFieldsSize);
  Result:=p;
end;

procedure TDbf.FreeRecordBuffer(var Buffer: PChar); {override virtual abstract from TDataset}
var
  p:pointer;
begin
  p:=Buffer;
  FreeMem(p);
end;

procedure TDbf.GetBookmarkData(Buffer: PChar; Data: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  pBookMarkData(Data)^:=prec.BookMarkData;
end;

function TDbf.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  result:=prec.BookMarkFlag;
end;

function TDbf._GetCurrentBuffer:pchar;
begin
	if State=dsFilter then begin
		result:=_FilterBuffer;
	end else   if State = dsCalcFields then  begin
		result:=@(pDbfRecord(CalcBuffer).deletedflag);
	end else begin
		if IsEmpty then begin
      result:=nil;
    end else begin
  		result:=@(pDbfRecord(ActiveBuffer).deletedflag);
    end;
	end;
end;


function TDbf.GetFieldData(Field: TField; Buffer: Pointer): Boolean; {override virtual abstract from TDataset}
var
	src:PChar;
begin
	src:=_GetCurrentBuffer;
	if src=nil then begin
    result:=false;
    exit;
  end;

	if Field.FieldNo>0 then begin
		Result:=_DbfFile.GetFieldData(Field.FieldNo-1,Field.DataType,Src,Buffer);
	end else begin { calculated fields.... }
		Inc(PChar(Src), Field.Offset + GetRecordSize);
		Result := Boolean(PChar(Buffer)[0]);
		if Result and (Src <> nil) then
      // A ftBoolean was 1 byte in Delphi 3
      // it is now 2 byte in Delphi 5
      // Not sure about delphi 4.
{$ifdef delphi_5}
  			Move(PChar(Src)[1], Buffer^, Field.DataSize);
{$else}

      if field.DataType=ftBoolean then
  			Move(PChar(Src)[1], Buffer^, 1)
      else
  			Move(PChar(Src)[1], Buffer^, Field.DataSize);
{$endif}
	end;
end;



function TDbf.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; {override virtual abstract from TDataset}
var
	prec:pDBFRecord;
  acceptable:boolean;
  SaveState:TDataSetState;
  lPhysicalRecno:integer;
  s:string;
begin
	if (_DbfFile.RecordCount<1) or (_Cursor=nil) then	Result := grEOF;
	prec:=pDBFRecord(Buffer);
  acceptable:=false;
  repeat
		result := grOk;
		case GetMode of
			gmCurrent :
				begin
          //if prec.BookmarkData.Recno=_PhysicalRecno then begin
					//	exit;		// try to fasten a bit...
					//end;
				end;
			gmNext :
				begin
          Acceptable:=_Cursor.Next;
					if Acceptable then begin
						result:= grOk;
					end else begin
						//_Cursor.Last;
						result:= grEOF
					end;
				end;
			gmPrior :
				begin
          Acceptable:=_Cursor.Prev;
					if Acceptable then begin
						result:= grOk;
					end else begin
						//_Cursor.First;
						result:= grBOF
					end;
				end;
		end;

		if (result=grOk) then begin
			lPhysicalRecno:=_Cursor.GetPhysicalRecno;
      if (lPhysicalRecno>_DbfFile.RecordCount)
				or (lPhysicalRecno<=0) then begin
				result:=grError;
			end else begin
				_DbfFile.ReadRecord(lPhysicalRecno,@prec.DeletedFlag);
        acceptable:=(_ShowDeleted or (prec.DeletedFlag = ' '))
			end;
		end;

    if (Result=grOk) and acceptable then begin
      if Assigned(OnFilterRecord) and Filtered then begin
        _FilterBuffer:=@prec.DeletedFlag;
        SaveState:=SetTempState(dsFilter);
        OnFilterRecord(self,acceptable);
        RestoreState(SaveState);
      end else if Length(easyfilter)<>0 then begin
        SetString(s,buffer,RecordSize);
        s:=UpperCase(s);
        if Pos(easyfilter,s)>0 then begin
          acceptable:=true;
        end;
      end else acceptable:=true;
    end;
    if (GetMode=gmCurrent) and (acceptable=false) then Result:=grError;
	until (Result <> grOK) or Acceptable;

  if Result = grOK then begin
	 	ClearCalcFields(Buffer); //run automatically
    try
  	  GetCalcFields(Buffer);
    finally
	   	prec.BookmarkData:=_Cursor.GetBookMark;
     	prec.BookmarkFlag := bfCurrent;
    end;
    if (prec.BookMarkData.Recno<=0) then begin
	   	prec.BookmarkData:=_Cursor.GetBookMark;
    end;
  end else begin
   	prec.BookmarkData.Recno:=0;
   	prec.BookmarkData.IndexBookmark:=-1;
  end;
  //else if (Result = grError) and DoCheck then
  //raise eDbfError.Create ('GetRecord: Invalid record');
end;

function TDbf.GetRecordSize: Word; {override virtual abstract from TDataset}
begin
  Result:=_DbfFile.RecordSize;
end;

procedure TDbf.InternalAddRecord(Buffer: Pointer; Append: Boolean); {override virtual abstract from TDataset}
begin
  // Nothing to do
end;

procedure TDbf.InternalClose; {override virtual abstract from TDataset}
var
  i:integer;
	lindex:TVirtualCursor;
begin
  // disconnect field objects
  BindFields(False);
  // destroy field object (if not persistent)
  if DefaultFields then begin
    DestroyFields;
  end;

  if (_dbfFile<>nil) and (_ReadOnly=false) then begin
    _DbfFile.WriteHeader;
  end;
  FreeAndNil(_Cursor);
  if _IndexDefs<>nil then begin
    for i:=_IndexDefs.Count-1 downto 0 do begin
     	lindex:=TVirtualCursor(_IndexDefs.Items[i]);
      lindex.Free;
    end;
    _IndexDefs.Clear;
  end;
  _DbfFile:=nil;
end;

procedure TDbf.InternalDelete; {override virtual abstract from TDataset}
var
  lRecord:pDbfRecord;
begin
  CheckActive;
  lRecord:=pDbfRecord(ActiveBuffer);
  lRecord.DeletedFlag := '*'; //_DataHdr.LastDeleted;
  _DbfFile.WriteRecord(_Cursor.GetPhysicalRecNo, @lRecord.DeletedFlag);
end;

procedure TDbf.InternalFirst; {override virtual abstract from TDataset}
begin
  _Cursor.First;
end;

procedure TDbf.InternalGotoBookmark(Bookmark: Pointer); {override virtual abstract from TDataset}
var
	RecInfo: rBookmarkData;
begin
	RecInfo := rBookmarkData(Bookmark^);
  _Cursor.GotoBookmark(RecInfo);
end;

procedure TDbf.InternalHandleException; {override virtual abstract from TDataset}
begin
	Application.HandleException(Self);
end;

procedure TDbf._GetFieldDefsFromDbfFieldDefs;
var
  Il : Integer;
  MyFieldDef:TDbfFieldDef;
begin
  FieldDefs.Clear;

  for Il:=0 to _DbfFile.FieldList.Count-1 do begin
    MyFieldDef:=_DbfFile.FieldList.Items[Il];
    if MyFieldDef.FieldType in [ftString,ftBCD] then
      FieldDefs.Add(MyFieldDef.FieldName,MyFieldDef.FieldType,MyFieldDef.Size,false)
    else
      FieldDefs.Add(MyFieldDef.FieldName,MyFieldDef.FieldType,0,false);
    // AutoInc fields are readonly
    {$ifdef DELPHI_5}
      if MyFieldDef.FieldType = ftAutoInc then
        FieldDefs[Il].Attributes :=[faReadOnly];
    {$endif}
  end;
end;

procedure TDbf.InternalInitFieldDefs; {override virtual abstract from TDataset}
var
  MustReleaseDbfFile:boolean;
begin
	CheckInactive;
  MustReleaseDbfFile:=false;
	with FieldDefs do
	begin
    if _DbfFile=nil then begin
      _DbfFile:=DbfDefaultDatabase.OpenDbf(
        _AbsolutePath + _TableName,
        pfOpen,
        false, //AutoCreate
        true); //ReadOnly);
      MustReleaseDbfFile:=true;
    end;
    _GetFieldDefsFromDbfFieldDefs;
    if MustReleaseDbfFile then begin
      _DbfFile.Release;
      _DbfFile:=nil;
    end;
  end;
end;

procedure TDbf.InternalInitRecord(Buffer: PChar); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
	prec.BookmarkData.IndexBookmark:=-1;
  prec.BookmarkFlag:=TBookmarkFlag(0);
// OH 2000-11-15 dBase7 support.
// Init Record with zero and set autoinc field with next value
  if _DbfFile.DbfVersion < xBaseVII then begin
    fillchar(prec.DeletedFlag,_DbfFile.RecordSize,' ');
  end else begin
    _DbfFile.InitRecord(@prec.DeletedFlag);
  end;
end;

procedure TDbf.InternalLast; {override virtual abstract from TDataset}
begin
  _Cursor.Last;
end;

procedure TDbf.InternalOpen; {override virtual abstract from TDataset}
var
  lIndex:TDbfIndexDef;
  i:integer;
  lIndexFileName:string;
begin
  _DbfFile:=DbfDefaultDatabase.OpenDbf(
    _AbsolutePath + _TableName,
    pfOpen,
    _OpenMode in [omAutoCreate,omTemporary], //AutoCreate
    _ReadOnly or (csDesigning in ComponentState)); //ReadOnly);

  if (_DbfFile.HeaderSize=0) or (_DbfFile.FieldList.Count=0) then begin
    if FieldDefs.Count>0 then begin
      _CreateTableFromFieldDefs;
    end else begin
      _CreateTableFromFields;
    end;
  end else begin
    _GetFieldDefsFromDbfFieldDefs;
  end;
  // create the fields dynamically
  if DefaultFields then begin
    CreateFields; // Create fields from fielddefs.
  end;

  BindFields (True);


  for i:=0 to _IndexDefs.Count-1 do begin
    lIndex:=_IndexDefs.Items[i];
    if length(lIndex._IndexDefFileName)=0 then begin
      lIndex._IndexDefFileName:=lIndex._SortField;
    end;
    lIndexFileName:=_ExpandFileName(_AbsolutePath,lIndex._IndexDefFileName);
    lIndexFileName:=ChangeFileExt(lIndexFileName,'.NDX');
    _DbfFile.OpenIndex(lIndexFileName,lIndex._SortField,pfOpen,true,_ReadOnly  or (csDesigning in ComponentState));
  end;

  _SetIndexname(_IndexName);
  if _Cursor=nil then _Cursor:=TDbfCursor.Create(_DbfFile);

  InternalFirst;

//  _DbfFile.SetIndex(_IndexName);
//  _DbfFile._IsCursorOpen:=true;
end;

procedure TDbf.InternalPost; {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
	CheckActive;
	prec:=pDbfRecord(ActiveBuffer);
	prec.DeletedFlag:=' ';

	if State = dsEdit then
	begin
    _DbfFile.Update(_Cursor.GetPhysicalRecNo,@prec.DeletedFlag);
	end else begin
    _Cursor.SetPhysicalRecNo(_DbfFile.RecordCount+1);
    _DbfFile.Insert(@prec.DeletedFlag);
	end;
end;

{$ifndef DELPHI_5}
procedure TDbf.InitFieldDefsFromFields;
var
  I: Integer;
  F: TField;
  FieldDef: TFieldDef;
begin
  { Create FieldDefs from persistent fields if needed }
  for I := 0 to FieldCount - 1 do
  begin
    F := Fields[I];
    with F do
    if FieldKind = fkData then begin
      FieldDefs.Add(FieldName,DataType,Size,Required);
    end;
  end;
end;
{$endif}

procedure TDbf._CreateTableFromFields;
begin
  FieldDefs.Clear;
  InitFieldDefsFromFields;
  _CreateTableFromFieldDefs;
end;

procedure TDbf._CreateTableFromFieldDefs;
var
  Il:integer;
  aDbfFieldDefs:TDbfFieldDefs;
begin
  aDbfFieldDefs:=TDbfFieldDefs.Create(self);
  try
    for Il:=0 to FieldDefs.Count-1 do begin
      with aDbfFieldDefs.AddFieldDef do begin
        FieldName:=FieldDefs.Items[Il].Name;
        FieldType:=FieldDefs.Items[Il].DataType;
        if FieldDefs.Items[Il].Size>0 then begin
          Size:=FieldDefs.Items[Il].Size;
          Prec:=FieldDefs.Items[Il].Precision;
        end else begin
          SetDefaultSize;
        end;
      end;
    end;
    _DbfFile.FinishCreate(aDbfFieldDefs,512);
  finally
    aDbfFieldDefs.Free;
  end;
end;

procedure TDbf.CreateTable;
begin
     CreateTableEx(nil);
end;

procedure TDbf.CreateTableEx(DbfFieldDefs:TDbfFieldDefs);
var
	ix:integer;
  lIndex:TDbfIndexDef;
begin
	CheckInactive;
	//  InternalInitFieldDefs;
  try
    _DbfFile:=DbfDefaultDatabase.OpenDbf(
      _AbsolutePath + _TableName,
      pfCreate,
      true, //AutoCreate
      false); //ReadOnly);
    if DbfFieldDefs=nil then begin
      if FieldDefs.Count>0 then begin
        _CreateTableFromFieldDefs;
      end else begin
        _CreateTableFromFields;
      end;
    end else begin
      _DbfFile.FinishCreate(DbfFieldDefs,512);
    end;

    // Create all indexes;
    for ix:=0 to _IndexDefs.Count-1 do begin
      lIndex:=_IndexDefs.Items[ix];
      _DbfFile.OpenIndex(_ExpandFileName(_AbsolutePath,lIndex._IndexDefFileName),lIndex._SortField,pfCreate,true,_ReadOnly  or (csDesigning in ComponentState));
    end;
  finally
    _DbfFile.Release;
    _DbfFile:=nil;
  end;
end;

procedure TDbf.PackTable;
begin
  CheckBrowseMode;
	_DbfFile.PackTable;
	Resync([]);
end;

function TDbf.LocateRecord (const KeyFields : String; const KeyValues : Variant;
                              Options : TLocateOptions; bSyncCursor : Boolean) : Boolean;
var
  lstKeys               : TList;
  iIndex                : Integer;
  ReturnBookMark        : TBookMarkStr;
  Field                 : TField;
  bMatchedData          : Boolean;
  bVarIsArray           : Boolean;
  varCompare            : Variant;

  function CompareValues : Boolean;
  var
    sCompare            : String;
  begin
    If (Field.DataType = ftString) Then
    Begin
      sCompare          := VarToStr (varCompare);
      If loCaseInsensitive in Options Then
      Begin
        Result      := CompareText (Field.AsString,sCompare) = 0;
        If Not Result and (iIndex = lstKeys.Count - 1) And (loPartialKey in Options) And
          (Length (sCompare) < Length (Field.AsString)) Then
        Begin
          If Length (sCompare) = 0 Then
            Result  := True
          Else
            Result  := CompareText (Copy (Field.AsString,1,Length (sCompare)),sCompare) = 0;
        End;
      End
      Else
      Begin
        Result        := Field.AsString = sCompare;
        If Not Result and (iIndex = lstKeys.Count - 1) And (loPartialKey in Options) And
          (Length (sCompare) < Length (Field.AsString)) Then
        Begin
          If Length (sCompare) = 0 Then
            Result    := True
          Else
            Result    := Copy (Field.AsString,1,Length (sCompare)) = sCompare;
        End;
      End;
    End
    Else
      Result        := Field.Value = varCompare;
  end;

begin
  Result                := False;
  bVarIsArray           := False;
  CheckBrowseMode;
  CursorPosChanged;
  lstKeys               := TList.Create;
  Try
    GetFieldList (lstKeys,KeyFields);
    If VarArrayDimCount (KeyValues) = 0 Then
      bMatchedData      := lstKeys.Count = 1
    Else If VarArrayDimCount (KeyValues) = 1 Then
    Begin
      bMatchedData      := VarArrayHighBound (KeyValues,1) + 1 = lstKeys.Count;
      bVarIsArray       := True;
    End
    Else
      bMatchedData      := False;
    If bMatchedData Then
    Begin
      ReturnBookMark    := BookMark;
      DisableControls;
      Try
        (*
          When we have indexes, add code here to chose the best index
        *)
        First;
        While Not Eof and Not Result Do
        Begin
          Result            := True;
          iIndex            := 0;
          While Result And (iIndex < lstKeys.Count) Do
          Begin
            Field           := TField (lstKeys [iIndex]);
            If bVarIsArray Then
              varCompare    := KeyValues [iIndex]
            Else
              varCompare    := KeyValues;
            Result          := CompareValues;
            iIndex          := iIndex + 1;
          End;
          If Not Result Then
            Next;
        End;
        If Not Result Then
          BookMark          := ReturnBookMark;
      Finally
        EnableControls;
      End;
    End;
  Finally
    lstKeys.Free;
  End;
end;

function TDbf.Locate (const KeyFields : string; const KeyValues : Variant; Options : TLocateOptions) : Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord (KeyFields,KeyValues,Options,True);
  If Result Then
    DoAfterScroll;
end;

function TDbf.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; {override virtual}
var
  Memoi:array[1..32] of char;
  lBlob:TMyBlobFile;
begin
  lBlob:=TMyBlobFile.Create(Mode,Field);
  if _DbfFile.GetFieldData(Field.FieldNo-1, ftString,@pDbfRecord(ActiveBuffer).deletedflag,@Memoi[1]) then begin
    lBlob.MemoRecno:=StrToIntDef(Memoi,0);
    _DbfFile._dbtFile.ReadMemo(lBlob.MemoRecno,lBlob);
    lBlob.ReadSize:=lBlob.Size;
  end else lBlob.MemoRecno:=0;
  Result:=lBlob;
end;

{$ifdef DELPHI_4}
function TDbf.Translate(Src, Dest: PChar; ToOem: Boolean): Integer; {override virtual}
{$else}
procedure TDbf.Translate(Src, Dest: PChar; ToOem: Boolean); {override virtual}
{$endif}
begin
  if (Src <> nil) and (Dest<>nil) then begin
    if Assigned(_OnTranslate) then begin
      _OnTranslate(Self,Src,Dest,ToOem);
    end else begin
      if ToOem then CharToOem(Src,Dest)
      else OemToChar(Src,Dest);
    end;
{$ifdef DELPHI_5}
    result:= StrLen(Dest);
  end else result:=0;
{$else}
  end;
{$endif}
end;

procedure TDbf.ClearCalcFields(Buffer: PChar);
var
  realbuff,calcbuff:pchar;
begin
  realbuff:=@pDbfRecord(Buffer).DeletedFlag;
  calcbuff:=@realbuff[_DbfFile.RecordSize];
  FillChar(calcbuff^, CalcFieldsSize,0);
end;

procedure TDbf.InternalSetToRecord(Buffer: PChar); {override virtual abstract from TDataset}
var
	prec:pDbfRecord;
begin
	if Buffer=nil then exit;
	prec:=pDbfRecord(Buffer);
  if prec.BookMarkFlag=bfInserted then begin
    // do what ???
  end else begin
  	_Cursor.GotoBookmark(prec.BookmarkData);
  end;
end;

function TDbf.IsCursorOpen: Boolean; {override virtual abstract from TDataset}
begin
  result:=_Cursor<>nil;
end;

procedure TDbf.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  prec.BookMarkFlag:=Value;
end;

procedure TDbf.SetBookmarkData(Buffer: PChar; Data: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
begin
  prec:=pDbfRecord(Buffer);
  prec.BookMarkData:=pBookMarkData(Data)^;
end;

procedure TDbf.SetFieldData(Field: TField; Buffer: Pointer); {override virtual abstract from TDataset}
var
  prec:pDbfRecord;
	dst:pointer;
begin
	if (Field.FieldNo >= 0) then begin
		prec:=pDbfRecord(ActiveBuffer);
		dst:=@prec.DeletedFlag;
		_DbfFile.SetFieldData(Field.FieldNo - 1,Field.DataType,Buffer,Dst);
	end else begin    { ***** fkCalculated, fkLookup ***** }
		prec:=pDbfRecord(CalcBuffer);
		dst:=@prec.DeletedFlag;
		Inc(integer(dst), RecordSize + Field.Offset);
		Boolean(dst^) := LongBool(Buffer);
		if Boolean(dst^) then begin
      Inc(integer(dst), 1);
  		Move(Buffer^, dst^, Field.DataSize);
		end;
	end;     { end of ***** fkCalculated, fkLookup ***** }
	if not (State in [dsCalcFields, dsFilter, dsNewValue]) then begin
		DataEvent(deFieldChange, Longint(Field));
	end;
end;


// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecordCount: Integer; {override virtual}
begin
  result:=_Cursor.GetSequentialRecordCount;
end;

// this function is just for the grid scrollbars
// it doesn't have to be perfectly accurate, but fast.
function TDbf.GetRecNo: Integer; {override virtual}
begin
	//UpdateCursorPos;
  result:=_Cursor.GetSequentialRecno
end;

procedure TDbf.SetRecNo(Value: Integer); {override virual}
begin
  _Cursor.SetSequentialRecno(Value);
	Resync([]);
end;

function tDbf.GetCanModify: Boolean; {override;}
begin
  if _ReadOnly  or (csDesigning in ComponentState) then Result:=false
  else Result := inherited GetCanModify;
end;

procedure tDbf.SetFiltered(Value: Boolean); {override;}
begin
  if _Cursor=nil then exit;
  inherited;
  Resync([]);
end;

function Tdbf.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
   RetCodes: array[Boolean, Boolean] of ShortInt = ((2,-1),(1,0));
var
   b1,b2:integer;
begin
  // Check for uninitialized bookmarks
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if (Result = 2) then
  begin
    b1:=PInteger(Bookmark1)^;
    b2:=PInteger(Bookmark2)^;
    if b1<b2 then Result:=-1
    else if b1>b2 then Result:=1
    else Result:=0;
  end;
end;

function TDbf._GetVersion: string;
begin
  Result:='tDbf v' + Format('%2.3f',[_MAJOR_VERSION + _MINOR_VERSION / 1000.0]);
end;

procedure TDbf._SetVersion(s: string);
begin
  // What an idea...
end;

procedure TDbf.OpenIndexFile(IndexFile:string);
var
  lIndexFileName:string;
begin
	CheckActive;
  lIndexFileName:=_ExpandFileName(_AbsolutePath,IndexFile);
  _DbfFile.OpenIndex(lIndexFileName,'',pfOpen,true,_ReadOnly  or (csDesigning in ComponentState));
end;

{$ifdef DELPHI_5}
procedure TDbf.AddIndex(const IndexFile, Fields: String; Options: TIndexOptions; const DescFields: String='');
{$else}
procedure TDbf.AddIndex(const IndexFile, Fields: String; Options: TIndexOptions);
{$endif}
var
  lIndexFileName:string;
begin
	CheckActive;
  lIndexFileName:=_ExpandFileName(_AbsolutePath,IndexFile);
  _DbfFile.OpenIndex(lIndexFileName,Fields,pfCreate,true,_ReadOnly  or (csDesigning in ComponentState));
end;

procedure TDbf._SetIndexname(IndexFile: string);
var
  DbfIndex:TIndexFile;
  Recno:integer;
begin
  _IndexName:=IndexFile;
  if _DbfFile=nil then exit;
  IndexFile:=_ExpandFileName(_AbsolutePath,IndexFile);
  IndexFile:=ChangeFileExt(IndexFile,'.NDX');
  DbfIndex:=_DbfFile.GetIndexByName(IndexFile);
  if _Cursor=nil then Recno:=1
  else Recno:=_Cursor.GetPhysicalRecno;
  if DbfIndex<>nil then begin
    FreeAndNil(_Cursor);
    _Cursor:=TIndexCursor.Create(DbfIndex);
  end else begin
    FreeAndNil(_Cursor);
    _Cursor:=TDbfCursor.Create(_DbfFile);
  end;
  _Cursor.SetPhysicalRecno(Recno);
  if State = dsBrowse then Refresh;
end;

procedure TDBf.DeleteIndex(const IndexFile: string);
var
  lIndexFileName:string;
begin
  CloseIndexFile(IndexFile);
  lIndexFileName:=_ExpandFileName(_AbsolutePath,IndexFile);
  if fileexists(lIndexFileName) then DeleteFile(lIndexFileName)
  else raise eDbfError.Create('Index does not exist');
end;

procedure TDbf.CloseIndexFile(const IndexFile: string);
var
  lIndexFileName:string;
begin
  lIndexFileName:=_ExpandFileName(_AbsolutePath,IndexFile); // Extract absolute path.

  _DbfFile.CloseIndex(IndexFile);
  _DbfFile.CloseIndex(lIndexFileName);
  _DbfFile.CloseIndex(lIndexFileName);
end;

function TDbf._GetPhysicalRecno:integer;
begin
  Result:=_Cursor.GetPhysicalRecNo;
end;

function TDbf._GetDbfFieldList:TDbfFieldDefs;
begin
  Result:=nil;
  if _DbfFile<>nil then begin
    Result:=_DbfFile.FieldList;
  end;
end;

procedure TDbf._SetShowDeleted(Value: boolean);
begin
  _ShowDeleted:=Value;
end;

function TDbf.IsDeleted:boolean;
var
  src:pchar;
begin
	src:=_GetCurrentBuffer;
  IsDeleted:=(src=nil) or (src^<>' ')
end;

procedure TDbf.Undelete;
var
  src:pchar;
begin
	src:=_GetCurrentBuffer;
  if (src<>nil) and (src^<>' ') then begin
    src^:='*';
    _DbfFile.WriteRecord(_Cursor.GetPhysicalRecNo, src);
  end;
end;

//==========================================================
//============ TDbfIndexCollection
//==========================================================
constructor TDbfIndexCollection.Create(Owner:TDataset);
begin
 inherited Create(TDbfIndexDef);
 _Owner := Owner;
end;

function TDbfIndexCollection.Add : TDbfIndexDef;
begin
 Result := TDbfIndexDef(inherited Add);
end;

Procedure TDbfIndexCollection.Setitem(n:integer;Value:TDbfIndexDef);
begin
 inherited Setitem(n,Value);
end;

function TDbfIndexCollection.GetItem(n:integer) : TDbfIndexDef;
begin
  Result := TDbfIndexDef(inherited GetItem(n));
end;

function TDbfIndexCollection.getowner : tpersistent;
begin
 Result := _Owner;
end;

function TDbfIndexCollection.GetIndexByFilename(FileName:string):TDbfIndexDef;
var
   i:integer;
   lIndex:TDbfIndexDef;
begin
  filename := Uppercase(filename);
  for i:=0 to count-1 do begin
    lIndex:=TDbfIndexDef(inherited GetItem(i));
    if lIndex._IndexDefFileName=filename then begin
       result:=lIndex;
       exit;
    end
  end;
  result:=nil;
end;

//==========================================================
//============ TDbfIndexDef
//==========================================================
constructor TDbfIndexDef.Create(Collection: TCollection); {override;}
begin
  inherited Create(Collection);
end;

destructor TDbfIndexDef.Destroy; {override;}
begin
	inherited Destroy;
end;

procedure TDbfIndexDef._SetIndexDefFileName(value:string);
begin
  _IndexDefFileName:=UpperCase(value);
end;

initialization
  tDbf_TrimFields := true;
  if _DbfExePath='' then _DbfExePath:=extractfilepath(Application.Exename);

finalization

end.



