unit UDbfPagedFile;

interface

uses
  classes, SysUtils, Dialogs;

type
	xBaseVersion = (xUnknown,xClipper,xBaseIII,xBaseIV,xBaseV,xBaseVII,xFoxPro,xVisualFoxPro);

  EPagedFile = Exception;

  TPagedFileMode = (pfOpen,pfCreate);



	TPagedFile = class(TObject)
  private
    _Stream : TStream;
    _HeaderSize : Integer;
    _RecordSize : Integer;
    _RecordCount:integer;
    _Header:pchar;
    _NeedRecalc:boolean;
  protected
    _Mode:TPagedFileMode;
    _AutoCreate:boolean;
    _ReadOnly:boolean;
    _Filename:string;
	protected
    procedure _SetRecordSize(value:integer); virtual;
    procedure _SetHeaderSize(value:integer); virtual;
    procedure _FillHeader(c:byte);
    function _GetRecordCount:integer;
  public
    constructor Create(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
    destructor Destroy; override;

    procedure Close;
    procedure Open(Mode:TPagedFileMode;AutoCreate:boolean;ReadOnly:Boolean);
		procedure ReadRecord(IntRecNum:Integer;Buffer:Pointer);
		procedure WriteRecord(IntRecNum:Integer;Buffer:Pointer);
    procedure WriteHeader; virtual;

    procedure _SetRecordCount(value:Integer);
    procedure WriteChar(c:byte);
		procedure SeekPage(page:Integer);

    property HeaderSize : Integer read _HeaderSize write _SetHeaderSize;
    property RecordSize : Integer read _RecordSize write _SetRecordSize;
    property RecordCount : integer read _GetRecordCount write _SetRecordCount;
    property Header : PChar read _Header;
    property FileName : string read _Filename;
 	end;

implementation

uses
    UDbfStrings;
//====================================================================
// TPagedFile
//====================================================================

constructor TPagedFile.Create(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
begin
  _filename:=Uppercase(lFileName);
  Open(Mode,AutoCreate,ReadOnly);
	_HeaderSize:=0;
  _RecordSize:=0;
  _RecordCount:=0;
  _Mode:=Mode;
  _AutoCreate:=AutoCreate;
  _ReadOnly:=ReadOnly;
  _Header:=nil;
end;

destructor TPagedFile.Destroy;
begin
  Close;
  inherited;
end;

Procedure TPagedFile.Open(Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
var
  fileopenmode:word;
begin
  if not fileExists(_FileName) then begin
    if AutoCreate or (Mode = pfCreate) then fileopenmode:=fmCreate
    else raise EPagedFile.CreateFmt(STRING_FILE_NOT_FOUND,[_FileName]);
  end else begin
    if ReadOnly  then fileopenmode := fmOpenRead + fmShareDenyNone
    else fileopenmode := fmOpenReadWrite + fmShareDenyNone; // + fmShareDenyWrite;
  end;
  _Stream:=TFileStream.Create(_FileName, fileopenmode);
  if Mode=pfCreate then _Stream.Size:=0;
end;

Procedure TPagedFile.Close;
begin
  _Stream.Free;
  _Stream:=nil;
end;


procedure TPagedFile.SeekPage(page:Integer);
var
  p:Integer;
begin
  p:=_HeaderSize + (_RecordSize * (page-1) );
  _Stream.Position := p;
end;

Procedure TPagedFile.ReadRecord(IntRecNum:Integer; Buffer:Pointer);
begin
  SeekPage(IntRecNum);
  _Stream.Read(Buffer^,_RecordSize);
end;

procedure TPagedFile.WriteRecord(IntRecNum:Integer; Buffer:Pointer);
begin
  SeekPage(IntRecNum);
	_Stream.Write(Buffer^, _RecordSize);
  if IntRecNum>=_RecordCount then _RecordCount:=IntRecNum;
end;

procedure TPagedFile.WriteHeader;
begin
  _Stream.Position := 0;
	_Stream.Write(_Header^, _HeaderSize);
end;


procedure TPagedFile._SetHeaderSize(value:integer);
begin
  if _HeaderSize<>value then begin
    if _Header<>nil then FreeMem(_Header);
    _HeaderSize:=value;
    if _HeaderSize<>0 then GetMem(_Header,_HeaderSize)
    else _Header:=nil;
    _NeedRecalc:=true;
    _FillHeader(0);
    _Stream.Position := 0;
    _Stream.Read(_Header^,_HeaderSize);
  end;
end;

procedure TPagedFile._FillHeader(c:byte);
begin
  if _Header=nil then exit;
  FillChar(_Header^,_HeaderSize,c);
end;


procedure TPagedFile._SetRecordSize(value:integer);
begin
  if _RecordSize<>value then begin
    _RecordSize:=value;
    _NeedRecalc:=true;
  end;
end;

function TPagedFile._GetRecordCount:integer;
begin
  if _NeedRecalc then begin
    if (_RecordSize=0) or (_Stream=nil)  then _RecordCount:=0
    else _RecordCount:=(_Stream.Size - _HeaderSize) div _RecordSize;
    if _RecordCount<0 then _RecordCount:=0;
    _NeedRecalc:=false;
  end;
  result:=_RecordCount;
end;

procedure TPagedFile._SetRecordCount(value:Integer);
begin
  if (_RecordCount<>Value) then begin
    _Stream.Size:=_HeaderSize + _RecordSize * value;
    _RecordCount:=value;
  end;
end;

procedure TPagedFile.WriteChar(c:byte);
begin
	_Stream.Write(c, 1);
end;

end.

