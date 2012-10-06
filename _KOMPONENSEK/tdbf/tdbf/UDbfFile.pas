
unit UDbfFile;

interface
uses
  Classes,Dialogs, SysUtils, Windows, UDbfCommon,db,
  UDbfCursor, UDbfPagedFile, UDbfFieldDef, UDbfMemo, UDbfIndex, UDbfIndexFile;

{$I _DbfCommon.inc}
//====================================================================
//=== Dbf support (first part)
//====================================================================
//  TxBaseVersion = (xUnknown,xClipper,xBaseIII,xBaseIV,xBaseV,xFoxPro,xVisualFoxPro);
//  TPagedFileMode = (pfOpen,pfCreate);
//  TDbfGetMode = (xFirst,xPrev,xCurrent, xNext, xLast);
//  TDbfGetResult = (xOK, xBOF, xEOF, xError);

type
  TDbfFile = class;
 	EDbfFile = class (Exception);


//====================================================================
  TDbfDatabase=class
  protected
    _DbfFiles:TList;
  public
    TrimLeft:boolean;
    TrimRight:boolean;
    constructor Create;
    destructor Destroy; override;
    procedure CloseDbf(var DbfFile:TDbfFile);
    function OpenDbf(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean):TDbfFile;
  end;

//====================================================================
	EDbfFileError = class (Exception);
//====================================================================
  TDbfFile = class(TPagedFile)
  protected
    _cntUse:integer;
    _RecordBufferSize:integer;
    _DbfVersion : xBaseVersion;
    _FieldList: TDbfFieldDefs;
    _CurIndex:integer;
		_Indexes:TList;
//		_MdbFile : TIndexFile;
		_PrevBuffer:pchar;
		_ShowDeleted : Boolean;
    _Database:tDbfDatabase;
//    _DbfFieldDefs:TDbfFieldDefs;
    procedure _ConstructFieldList;
    function _HasBlob:boolean;
  public
		_dbtFile : TDbtFile;

    constructor Create(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
		destructor Destroy; override;
    procedure FinishCreate(FieldDefs:TDbfFieldDefs;MemoSize:integer);
    function GetIndexByName(lIndexFile:string):TIndexFile;
    procedure _SetRecordSize(value:integer); override;
    property FieldList: TDbfFieldDefs read _FieldList;

    procedure OpenIndex(IndexFileName,IndexField:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
    procedure CloseIndex(IndexFileName:string);
    procedure Release;
    procedure Insert(Buffer:pchar);
    procedure Update(Recno:integer;Buffer:pchar);
    procedure WriteHeader; override;
    procedure _WriteFieldHdr; // OH 2000-11-15 dBase7 support. Writeback last next-autoinc value
    property DbfVersion : xBaseVersion read _DbfVersion;
    procedure FastPackTable;
    procedure PackTable;
    function GetFieldInfo(FieldName:string):TDbfFieldDef;
    function GetFieldData(Column:Integer;DataType:TFieldType; Src,Dst:Pointer): Boolean;
    procedure SetFieldData(Column:integer;DataType:TFieldType; Src,Dst:Pointer);
    procedure IncAutoInc;
    procedure InitRecord(p:pchar);
    procedure PackIndex(lIndexFile:TIndexFile);
  end;
//====================================================================
  TDbfCursor = class(TVirtualCursor)
  protected
    _PhysicalRecno:integer;
  public
    constructor Create(DbfFile:TDbfFile);
    function Next:boolean; override;
    function Prev:boolean; override;
    procedure First; override;
    procedure Last; override;

    function GetPhysicalRecno:integer; override;
    procedure SetPhysicalRecNo(Recno:integer); override;

    function GetSequentialRecordCount:integer; override;
    function GetSequentialRecNo:integer; override;
    procedure SetSequentialRecNo(Recno:integer); override;

    procedure GotoBookmark(Bookmark:rBookmarkData); override;
		procedure Insert(Recno:integer; Buffer:PChar); override;
		procedure Update(Recno: integer; PrevBuffer,NewBuffer: PChar); override;
    function GetBookMark:rBookmarkData; override;
  end;

// OH 2000-11-15 dBase7 support. Swap byte order for 4 and 8 Byte Integer
function SwapInt  ( const Value ) : Integer;

{$ifdef DELPHI_5}
function SwapInt64( const Value ) : Int64;
{$endif}

var
	tDbf_TrimFields : boolean;
  DbfDefaultDatabase : tDbfDatabase;

implementation

uses
  UDbfStrings;
    
const
  sDBF_DEC_SEP= '.';
//====================================================================
type
  PDbfHdr = ^rDbfHdr;
	rDbfHdr = record
		VerDBF      : byte;   // 0
		Year        : byte;   // 1
		Month       : byte;   // 2
		Day         : byte;   // 3
    RecordCount : Integer;  // 4-7
    FullHdrSize : word;   // 8-9
    RecordSize  : word;   // 10-11
    Dummy1      : Word;   // 12-13
    IncTrans    : byte;   // 14
    Encrypt     : byte;   // 15
    Dummy2      : Integer; // 16-19
    Dummy3      : array[20..27] of byte; // 20-27
    MDXFlag     : char; // 28
		Language    : char; // 29
    dummy4      : word; // 30-31
  end;
type
  PFieldHdrIII = ^rFieldHdrIII;
  rFieldHdrIII = record
    FieldName   : array[0..10] of char;
    FieldType   : char; // 11
		Dummy        : array[12..15] of byte;
    FieldSize   : byte; // 16
    FieldPrecision  : byte; //17
    dummy2      : array[18..31] of byte;
  end;
//====================================================================
// OH 2000-11-15 dBase7 support. Header Update (add fields like Next AutoInc Value)
  rFieldHdrV = packed record
    FieldName      : array [0..31] of char;
    FieldType      : char;  // 32
    FieldSize      : byte;  // 33
    FieldPrecision : byte;  // 34
    Reserved1      : Word;  // 35-36
    MDXFlag        : Byte;  // 37
    Reserved2      : DWord; // 38-39
    NextAutoInc    : DWord; // 40-43
    Reserved3      : Word;  // 44-47
  end;
//====================================================================
  rAfterHdrIII = record // Empty
  end;
//====================================================================
  rAfterHdrV = record
    Dummy   : array[32..67] of byte;
  end;

function SwapInt  ( const Value ) : Integer;
var i : Integer;
begin
  PByteArray(@i)[0] := PByteArray(@Value)[3];
  PByteArray(@i)[1] := PByteArray(@Value)[2];
  PByteArray(@i)[2] := PByteArray(@Value)[1];
  PByteArray(@i)[3] := PByteArray(@Value)[0];
  result := i;
end;

{$ifdef DELPHI_5}
function SwapInt64( const Value ) : Int64;
var i : Int64;
begin
  PByteArray(@i)[0] := PByteArray(@Value)[7];
  PByteArray(@i)[1] := PByteArray(@Value)[6];
  PByteArray(@i)[2] := PByteArray(@Value)[5];
  PByteArray(@i)[3] := PByteArray(@Value)[4];
  PByteArray(@i)[4] := PByteArray(@Value)[3];
  PByteArray(@i)[5] := PByteArray(@Value)[2];
  PByteArray(@i)[6] := PByteArray(@Value)[1];
  PByteArray(@i)[7] := PByteArray(@Value)[0];
  result := i;
end;
{$endif}
//====================================================================
// International separator
// thanks to Bruno Depero from Italy
// and Andreas Wöllenstein from Denmark
//====================================================================
function DbfStrToFloat(s: string): Extended;
var iPos: integer;
     eValue: extended;
begin
    iPos:= Pos(sDBF_DEC_SEP, s);
    if iPos> 0 then
      s[iPos]:= DecimalSeparator;
    if TextToFloat(pchar(s), eValue, fvExtended) then
			Result:= eValue
    else Result:= 0;
end;

function FloatToDbfStr(f: Extended; size, prec: integer): string;
var iPos: integer;
begin
    Result:= FloatToStrF(f, ffFixed, Size, prec);
    iPos:= Pos(DecimalSeparator, Result);
    if iPos> 0 then
      Result[iPos]:= sDBF_DEC_SEP;
end;

//====================================================================
// TDbfFile
//====================================================================
constructor TDbfFile.Create(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
var
  lMemoFileName:string;
begin
  // check if the file exists
  inherited Create(lFileName,Mode,AutoCreate,ReadOnly);

  _FieldList:=TDbfFieldDefs.Create(nil);
  _Indexes:=TList.Create;
  _ReadOnly:=ReadOnly;

  if (Mode <> pfCreate) then begin
    HeaderSize:=sizeof(rDbfHdr); // temporary
    // OH 2000-11-15 dBase7 support. I build dBase Tables with different
    // BDE dBase Level (1. without Memo, 2. with Memo)
    //                          Header Byte ($1d hex) (29 dec) -> Language driver ID.
    //  $03,$83 xBaseIII        Header Byte $1d=$00, Float -> N($13.$04) DateTime C($1E)
    //  $03,$8B xBaseIV/V       Header Byte $1d=$58, Float -> N($14.$04)
    //  $04,$8C xBaseVII        Header Byte $1d=$00  Float -> O($08)     DateTime @($08)
    //  $03,$F5 FoxPro Level 25 Header Byte $1d=$03, Float -> N($14.$04)
    // Access 97
    //  $03,$83 dBaseIII        Header Byte $1d=$00, Float -> N($13.$05) DateTime D($08)
    //  $03,$8B dBaseIV/V       Header Byte $1d=$00, Float -> N($14.$05) DateTime D($08)
    //  $03,$F5 FoxPro Level 25 Header Byte $1d=$09, Float -> N($14.$05) DateTime D($08)

    case PDbfHdr(Header).VerDBF of
    $83:                  _DbfVersion:=xBaseIII;
    $03,$8B,$8E,$7B:      _DbfVersion:=xBaseIV;
    $04,$8C :             _DbfVersion:=xbaseVII;
    $30 :                 _DbfVersion:=xVisualFoxPro;
    $F5 :                 _DbfVersion:=xFoxPro;
    else
      _DbfVersion:=xBaseIV; // The most compatible with other programs ?
    end;
    RecordSize:=PDbfHdr(Header).RecordSize;
    HeaderSize:=PDbfHdr(Header).FullHdrSize;
    if (HeaderSize=0) or (RecordSize=0) then begin
      HeaderSize:=0;
      RecordSize:=0;
      RecordCount:=0;
      exit;
    end;
    if PDbfHdr(Header).RecordCount <> RecordCount then begin
      // This message was annoying
      // and was not understood by most people
      // ShowMessage('Invalid Record Count,'+^M+
      //             'RecordCount in Hdr : '+IntToStr(PDbfHdr(Header).RecordCount)+^M+
      //             'expected : '+IntToStr(lRecordCount));
      PDbfHdr(Header).RecordCount := RecordCount; // Correct it
    end;
    _ConstructFieldList;
    lMemoFileName:=ChangeFileExt(_FileName,'.dbt');
    if _HasBlob then begin
      _dbtFile:=TDbtFile.Create(lMemoFileName,_Mode,_AutoCreate,
        _ReadOnly,0,_DbfVersion);
    end;
  end;
end;

destructor TDbfFile.Destroy;
begin
  FreeAndNil(_FieldList);
  FreeAndNil(_indexes);
  FreeAndNil(_DbtFile);
  if _PrevBuffer<>nil then begin
    FreeMem(_PrevBuffer);
  end;
	inherited Destroy;
end;

procedure TDbfFile.FinishCreate(FieldDefs:TDbfFieldDefs;MemoSize:integer);
var
  ix:Integer;
  Offs:Integer;
  lFieldHdrIII:rFieldHdrIII;
  lFieldDef:TDbfFieldDef;
  lMemoFileName:string;
begin
  // first reset file.
  try
    RecordCount:= 0;
    RecordSize := SizeOf(rDbfHdr) + SizeOf(rAfterHdrIII);
    HeaderSize := SizeOf(rFieldHdrIII);
    PDbfHdr(Header).VerDBF:=$03; // Default version xBaseV without memo
//		PDbfHdr(Header).Language:='X';
    _FieldList.Clear;
    Offs:=1; // deleted mark count 1.
    for Ix:=1 to FieldDefs.Count do
    begin
      lFieldDef:=FieldDefs.Items[Ix-1];
      FillChar(lFieldHdrIII,SizeOf(lFieldHdrIII),#0);

      lFieldHdrIII.FieldType:=lFieldDef.NativeFieldType; //DataType;
      StrPLCopy(lFieldHdrIII.FieldName,lFieldDef.FieldName,sizeof(lFieldHdrIII.FieldName)-1);
      lFieldHdrIII.FieldSize:=lFieldDef.Size;
      lFieldHdrIII.FieldPrecision:=lFieldDef.Prec;

      with _FieldList.AddFieldDef do begin
        FieldName:=lFieldDef.FieldName;
        Size:=lFieldDef.Size;
        Prec:=lFieldDef.Prec;
        NativeFieldType:=lFieldDef.NativeFieldType;
      end;

      WriteRecord(Ix,@lFieldHdrIII);
      Inc(Offs,lFieldDef.Size);
    end;
    // end of header
    WriteChar($0d);

    // update header
    PDbfHdr(Header).RecordSize := Offs;
    PDbfHdr(Header).FullHdrSize := HeaderSize + RecordSize * FieldDefs.Count + 1;
    inherited WriteHeader;
  finally
    RecordSize := PDbfHdr(Header).RecordSize;
    HeaderSize := PDbfHdr(Header).FullHdrSize;
    WriteHeader;
    // write the updated header
  end;
  if _HasBlob then begin
    lMemoFileName:=ChangeFileExt(_FileName,'.dbt');
    _dbtFile:=TDbtFile.Create(lMemoFileName,_Mode,_AutoCreate,_ReadOnly,
      MemoSize,_DbfVersion);
  end;
end;

function TDbfFile._HasBlob:boolean;
var
  i:integer;
  HasBlob:boolean;
begin
  HasBlob:=false;
  for i:=0 to _FieldList.Count-1 do begin
    if _FieldList.Items[i].IsBlob then HasBlob:=true;
  end;
  result:=HasBlob;
end;

procedure TDbfFile.WriteHeader;
var
	SystemTime: TSystemTime;
  lDataHdr:PDbfHdr;
begin
  if (HeaderSize=0) then exit;
  //FillHeader(0);
  lDataHdr:=PDbfHdr(Header);
  GetLocalTime(SystemTime);
  lDataHdr.Year := SystemTime.wYear - 1900;
  lDataHdr.Month := SystemTime.wMonth;
  lDataHdr.Day := SystemTime.wDay;
  lDataHdr.RecordCount := RecordCount;
  inherited WriteHeader;
// OH 2000-11-15 dBase7 support. Writeback last next-autoinc value
  if DbfVersion >= xBaseVII then
    _WriteFieldHdr;
  SeekPage(RecordCount+1); // last byte usually...
  WriteChar($1A); // Terminator...
end;

procedure TDbfFile._WriteFieldHdr;
var
  SaveHeaderSize : Integer;
  SaveRecordSize : Integer;
  i              : Integer;
  lFieldHdrV     : rFieldHdrV;
begin
  SaveHeaderSize := HeaderSize;
  SaveRecordSize := RecordSize;
  HeaderSize :=SizeOf(rAfterHdrV) + SizeOf(rDbfHdr);
  RecordSize :=SizeOf(rFieldHdrV);
  for i := 0 to _FieldList.Count-1 do begin
    with TDbfFieldDef(_FieldList.Items[i]) do begin
      if NativeFieldType = '+' then begin
        ReadRecord(i+1, @lFieldHdrV);
        if lFieldHdrV.NextAutoInc <> AutoInc then begin
          lFieldHdrV.NextAutoInc := AutoInc;
          WriteRecord(i+1, @lFieldHdrV);
        end;
      end;
    end;
  end;
  HeaderSize := SaveHeaderSize;
  RecordSize := SaveRecordSize;
end;

procedure TDbfFile._ConstructFieldList;
var
  lColumnCount,lHeaderSize,lFieldSize:Integer;
  Il : Integer;
  lFieldOffset : Integer;
  lFieldHdrIII:rFieldHdrIII;
  lFieldHdrV:rFieldHdrV;
  lSize,lPrec:integer;
  lNativeFieldType:char;
begin
  _FieldList.Clear;
  if DbfVersion>=xBaseV then begin
    lHeaderSize:=SizeOf(rAfterHdrV) + SizeOf(rDbfHdr);
    lFieldSize:=SizeOf(rFieldHdrV);
  end else begin
    lHeaderSize:=SizeOf(rAfterHdrIII) + SizeOf(rDbfHdr);
    lFieldSize:=SizeOf(rFieldHdrIII);
  end;
  HeaderSize:=lHeaderSize;
  RecordSize:=lFieldSize;

  lColumnCount:= (PDbfHdr(Header).FullHdrSize - lHeaderSize) div lFieldSize;

  if (lColumnCount <= 0) or (lColumnCount > 4096) then
      Raise EDbfFileError.Create('Invalid field count : ' + IntToStr(lColumnCount) + ' (must be between 1 and 4095)');

  lFieldOffset := 1;
  try
    for Il:=1 to lColumnCount do begin
      with _FieldList.AddFieldDef do begin
        Offset:=lFieldOffset;
        if _DbfVersion>=xBaseV then begin
          ReadRecord(Il,@lFieldHdrV);
          FieldName:=UPPERcase(PCHAR(@lFieldHdrV.FieldName[0]));
          lSize:=lFieldHdrV.FieldSize;
          lPrec:=lFieldHdrV.FieldPrecision;
          lNativeFieldType:=lFieldHdrV.FieldType;
          AutoInc := lFieldHdrV.NextAutoInc;
        end else begin
          ReadRecord(Il,@lFieldHdrIII);
          FieldName:=UPPERcase(PCHAR(@lFieldHdrIII.FieldName[0]));
          lSize:=lFieldHdrIII.FieldSize;
          lPrec:=lFieldHdrIII.FieldPrecision;
          lNativeFieldType:=lFieldHdrIII.FieldType;
        end;
        Size:=lSize;
        Prec:=lPrec;
        NativeFieldType:=lNativeFieldType;
        Inc(lFieldOffset,lSize);
      end;
    end;
    if (lFieldOffset <> PDbfHdr(Header).RecordSize) then begin
      // I removed the message because it confuses end-users.
      // Though there is a major problem if the value is wrong...
      // I try to fix it but it is likely to crash
      PDbfHdr(Header).RecordSize := lFieldOffset;
    end;
  finally
    HeaderSize:=PDbfHdr(Header).FullHdrSize;
    RecordSize:=PDbfHdr(Header).RecordSize;
  end;
end;

{
  I fill the holes with the last records.
  by this way I read each records only once.
}
procedure TDbfFile.FastPackTable;
var
  iDel,iNormal:integer;
  pDel,pNormal: pChar;
  function FindFirstDel: boolean;
  begin
    while iDel<=iNormal do begin
      ReadRecord(iDel, pDel);
      if (PCHAR(pDel)^ <> ' ') then begin
        Result:= TRUE;
        exit;
      end;
      Inc(iDel);
    end;
    Result:= FALSE;
  end;
  function FindLastNormal: boolean;
  begin
    while iNormal>=iDel do begin
      ReadRecord(iNormal, pNormal);
      if (PCHAR(pNormal)^= ' ') then begin
        Result:= TRUE;
        exit;
      end;
      dec(iNormal);
    end;
    Result:= FALSE;
  end;
var
  lIndexNo:integer;
begin
  if RecordSize< 1 then Exit;

  GetMem(pNormal, RecordSize);
  GetMem(pDel, RecordSize);
  try
    iDel:=1;
    iNormal:=RecordCount;

    while FindFirstDel do begin
      // iDel is definitely deleted
      if FindLastNormal then begin
        // but is not anymore
        WriteRecord(iDel, pNormal);
        PCHAR(pNormal)^:= '*';
        WriteRecord(iNormal,pNormal);
      end else begin
        // Cannot found a record after iDel so iDel must be deleted
        dec(iDel);
        break;
      end;
    end;
    // FindFirstDel failed means than iDel is full
    RecordCount:=iDel;
    for lIndexNo:=0 to _Indexes.Count-1 do begin
      PackIndex(TIndexFile(_Indexes.Items[lIndexNo]));
    end;
    // Pack Memofields
  finally
    FreeMem(pNormal);
    FreeMem(pDel);
  end;

end;

procedure TDbfFile.PackTable;
var
  DbfFileName,DbtFileName:string;
  pbfFileName,pbtFileName:string;
  obfFileName,obtFileName:string;
  lRecNo,lFieldNo,lBlobRecno,lWRecno,lIndexNo:integer;
  pbfFile:TDbfFile;
  pBuff: pChar;
  pBlobRecnoBuff:array[1..11] of char;
  BlobStream:TMemoryStream;
begin
  if RecordSize< 1 then Exit;

  DbfFileName:=_Filename;
  DbtFileName:=ChangeFileExt(_Filename,'.dbt');
  pbfFileName:=ChangeFileExt(_Filename,'.pbf');
  pbtFileName:=ChangeFileExt(_Filename,'.pbt');
  obfFileName:=ChangeFileExt(_Filename,'.obf');
  obtFileName:=ChangeFileExt(_Filename,'.obt');

  pbfFile:=TDbfFile.Create(pbfFileName,pfCreate,true,false);
  GetMem(pBuff, RecordSize);
  BlobStream:=TMemoryStream.Create;
  try
    pbfFile.FinishCreate(_FieldList,512);
    pbfFile._DbfVersion:=_DbfVersion;
    if _HasBlob then begin
      _dbtFile:=TDbtFile.Create(pbtFileName,_Mode,_AutoCreate,_ReadOnly,
        _DbtFile.RecordSize,_DbfVersion);
    end;
    lRecNo:=1;
    lWRecno:=1;
    while lRecNo<=RecordCount  do begin
      ReadRecord(lRecNo,pBuff);
      if (pBuff^ = ' ') then begin
        if _DbtFile<>nil then begin
          for lFieldNo:=0 to _FieldList.Count-1 do begin
            if _FieldList.Items[lFieldNo].IsBlob then begin
              lBlobRecno:=0;
              GetFieldData(lFieldNo, ftString,pBuff,@pBlobRecnoBuff[1]);
              lBlobRecno:=StrToIntDef(pBlobRecnoBuff,0);
              if lBlobRecno>0 then begin
                BlobStream.Clear;
                _dbtFile.ReadMemo(lBlobRecno,BlobStream);
                BlobStream.Position:=0;
                pbfFile._dbtFile.WriteMemo(lBlobRecno,
                  0, // always append
                  BlobStream);
              end;
              pbfFile.SetFieldData(lFieldNo, ftInteger,@lBlobRecno,pBuff)
            end;
          end;
        end;
        pbfFile.WriteRecord(lWRecno,pBuff);
        inc(lWRecno);
      end;
      Inc(lRecno);
    end;
    FreeAndNil(pbfFile);
    Close;
    _DbtFile.Close;
    // Delete the previous backup files if exists
    SysUtils.DeleteFile(obfFileName);
    SysUtils.DeleteFile(obtFileName);

    // Rename the old files
    RenameFile(dbfFileName,obfFileName);
    RenameFile(dbtFileName,obtFileName);
    // Then rename the new files
    RenameFile(pbfFileName,dbfFileName);
    RenameFile(pbtFileName,dbtFileName);

    // Then if everything worked, delete the backup files again
    SysUtils.DeleteFile(obfFileName);
    SysUtils.DeleteFile(obtFileName);

    Open(pfOpen,False,False);
    _DbtFile.Open(pfOpen,False,False);

    RecordCount:=lWRecno-1;
    WriteHeader;
    // And now recreate every index
    for lIndexNo:=0 to _Indexes.Count-1 do begin
      PackIndex(TIndexFile(_Indexes.Items[lIndexNo]));
    end;
    // Pack Memofields
  finally
    FreeAndNil(pbfFile);
    FreeMem(pBuff);
    FreeAndNil(BlobStream);
  end;
end;

function TDbfFile.GetFieldInfo(FieldName:string):TDbfFieldDef;
var
  i:Integer;
  lfi:TDbfFieldDef;
begin
  FieldName:=UpperCase(FieldName);
  for i:=0 to _FieldList.Count-1 do begin
    lfi:=TDbfFieldDef(_FieldList.Items[i]);
    if lfi.fieldName = FieldName then begin
      result:=lfi;
      exit;
    end;
  end;
  result:=nil;
end;

function TDbfFile.GetFieldData(Column:Integer;DataType:TFieldType; Src,Dst:Pointer): Boolean;
var
  FieldOffset: Integer;
  FieldSize: Integer;
  s:string;
  d:TDateTime;
  ld,lm,ly: word;
  MyFieldDef:TDbfFieldDef;
	function TrimStr(const s: string): string;
	begin
		if DataType=ftString then
		begin
			if tDbf_TrimFields then Result:=Trim(s)
			else Result:=TrimRight(s);
		end
		else Result:= Trim(s);
	end;
  procedure CorrectYear(var wYear: word);
  var wD, wM, wY, CenturyBase: word;
{$ifndef DELPHI_5}
  // Delphi 3 standard-behavior no change possible
  const TwoDigitYearCenturyWindow= 0;
{$endif}
  begin
     if wYear>= 100 then
       Exit;
     DecodeDate(Date, wY, wm, wD);
     // use Delphi-Date-Window
     CenturyBase := wY{must be CurrentYear} - TwoDigitYearCenturyWindow;
     Inc(wYear, CenturyBase div 100 * 100);
     if (TwoDigitYearCenturyWindow > 0) and (wYear < CenturyBase) then
        Inc(wYear, 100);
  end;
begin
  Result:=false;
  MyFieldDef:=TDbfFieldDef(_FieldList.Items[Column]);
  FieldOffset := MyFieldDef.Offset;
	FieldSize := MyFieldDef.Size;
// OH 2000-11-15 dBase7 support. Read values for new fieldtypes
  if MyFieldDef.NativeFieldType in ['+', 'I', 'O', '@'] then begin
    Src := PChar(Src) + FieldOffset;
    if MyFieldDef.NativeFieldType in ['+', 'I'] then begin
      Integer(Dst^) := SwapInt(Integer(Src^));
      result := DWord(Dst^) <> 0;
      if result then
        Integer(Dst^) := Integer( DWord(Dst^) - $80000000);
    end else begin
{$ifdef DELPHI_5}
      if (Int64(Src^) <> 0) then begin
        Int64(Dst^) := SwapInt64(Int64(Src^));
        if MyFieldDef.NativeFieldType = 'O' then begin
          if Int64(Dst^) > 0 then
            Int64(Dst^) := not Int64(Dst^)
          else
            Double(Dst^) := Double(Dst^)*-1;
        end;
        result := True;  // Field is not NULL
      end else begin
        result := False; // Field is NULL
      end;
{$endif}
    end;
  end else begin
    SetString(s, PChar(Src) + FieldOffset, FieldSize );
    s:=TrimStr(s);
    result:=length(s)>0; // return if field is empty
    if Result and (Dst<>nil) then// data not needed if Result= FALSE or Dst=nil
      case DataType of
      ftBoolean:
        begin
          // in DBase- FileDescription lowercase t is allowed too
          // with asking for Result= TRUE s must be longer then 0
          // else it happens an AV, maybe field is NULL
{$ifdef DELPHI_5}
          if (s[1]='T') or ((s[1]='t')) then Word(Dst^) := 1
          else Word(Dst^) := 0;
{$else}
          if (s[1]='T') or ((s[1]='t')) then Byte(Dst^) := 1
          else Byte(Dst^) := 0;
{$endif}
        end;
      ftInteger, ftSmallInt{$ifdef DELPHI_5},ftLargeInt{$endif}:
        begin
          case DataType of
          ftSmallInt : SmallInt(Dst^):= StrToIntDef(s, 0);
          {$ifdef DELPHI_5}
          ftLargeint : LargeInt(Dst^):= StrToInt64Def(s, 0);
          {$endif}
          else // ftInteger :
            Integer(Dst^):= StrToIntDef(s, 0);
          end; // case
        end;
      ftFloat:
        begin
          Double(Dst^) := DBFStrToFloat(s);
        end;
      ftCurrency:
        begin
          Double(Dst^) := DBFStrToFloat(s);
        end;
      ftDate:
        begin
          ld:=StrToIntDef(Copy(s,7,2),1);
          lm:=StrToIntDef(Copy(s,5,2),1);
          ly:=StrToIntDef(Copy(s,1,4),0);
          if ld=0 then ld:=1;
          if lm=0 then lm:=1;
  //           if (ly<1900) or (ly>2100) then ly:=1900;
  //           Year from 0001 to 9999 is possible
  //           everyting else is an error, an empty string too
  //           Do DateCorrection with Delphis possibillities for one or two digits
          if (ly< 100) and (Length(Trim(Copy(s,1,4)))in [1, 2]) then CorrectYear(ly);
          try
            d:=EncodeDate(ly,lm,ld);
            if Assigned(Dst) then  Integer(Dst^) := DateTimeToTimeStamp(d).Date;
          except
            Integer(Dst^) := 0;
          end;
        end;
          ftString: begin
          StrPCopy(Dst,s);
        end;
     end;
  end;
end;

procedure TDbfFile.SetFieldData(Column:integer;DataType:TFieldType; Src,Dst:Pointer);
var
  FieldSize,FieldPrec: Integer;
	s:string;
  fl:Double;
  ts:TTimeStamp;
  MyFieldDef:TDbfFieldDef;
  IntValue  : Integer;
begin
  MyFieldDef:=TDbfFieldDef(_FieldList.Items[Column]);
  FieldSize := MyFieldDef.Size;
  FieldPrec := MyFieldDef.Prec;

	Dst:=PChar(Dst)+MyFieldDef.Offset;
// OH 2000-11-15 dBase7 support. Write values for new fieldtypes
  if MyFieldDef.NativeFieldType in ['+', 'I', 'O', '@'] then begin
    if MyFieldDef.NativeFieldType in ['+', 'I'] then begin
      if src = nil then
        IntValue := 0 // Field = NULL
      else begin
        IntValue := Integer(DWord(Src^) + $80000000);
        Integer(Dst^) := SwapInt(IntValue);
      end;
    end else
{$ifdef DELPHI_5}
    if MyFieldDef.NativeFieldType = 'O' then begin
      if src = nil then
        Int64(Dst^) := 0 // Field = NULL
      else begin
        if Double(Src^) < 0 then
          Int64(Dst^) := not Int64(Src^)
        else
          Double(Dst^):= Double(Src^)*-1;
        Int64(Dst^) := SwapInt64(Dst^);
      end;
    end else
    if MyFieldDef.NativeFieldType = '@' then begin
      if src = nil then
        Int64(Dst^) := 0 // Field = NULL
      else
        Int64(Dst^) := SwapInt64(Src^);
    end;
{$endif}
  end else begin
    if src<>nil then begin
      case DataType of
      ftBoolean:
        begin
          if Word(Src^) = 1 then s:='T'
          else s:='F';
        end;
      ftInteger, ftSmallInt{$ifdef DELPHI_5},ftLargeInt{$endif}:
        begin
          case DataType of
          ftSmallInt : s:= IntToStr(SmallInt(Src^));
          {$ifdef DELPHI_5}
          ftLargeInt: s:= IntToStr(LargeInt(Src^));
          {$endif}
          else //ftInteger
            s:= IntToStr(Integer(Src^));
          end;
          // left filling
          if Length(s)<FieldSize then s:=StringOfChar(' ',FieldSize-Length(s)) + s;
        end;
      ftFloat,ftCurrency:
        begin
          fl := Double(Src^);
          s:=FloatToDbfStr(fl,FieldSize,FieldPrec);
          if Length(s)<FieldSize then s:=StringOfChar(' ',FieldSize-Length(s)) + s;
        end;
      ftDate:
        begin
          ts.Time:=0;
          ts.Date:=Integer(Src^);
          s:= FormatDateTime('yyyymmdd', TimeStampToDateTime(ts));
        end;
      ftString:
        begin
          s:=PChar(Src); // finish with first 0
        end;
      end; // case
    end; // if src<>nil (thanks andreas)
    if Length(s)<FieldSize then begin
      s:=s+StringOfChar(' ',FieldSize-Length(s));
    end else if (Length(s)>FieldSize) then begin
      if DataType= ftString then begin
        // never raise for strings to long, its not customary
        // TTable never raises
        SetLength(s, FieldSize)
      end else begin
        raise EDbfFileError.CreateFmt(STRING_FIELD_TOO_LONG,[length(s),FieldSize]);
      end;
    end;
    Move(PChar(s)^, Dst^, FieldSize);
  end;
end;

procedure TDbfFile.IncAutoInc;
var
  i:integer;
begin
  for i :=0 to _FieldList.Count-1 do begin
    with _FieldList.Items[i] do begin
      if NativeFieldType = '+' then
        inc(AutoInc);
    end;
  end;
end;

procedure TDbfFile.InitRecord(p:pchar);
var
  MyFieldDef:TDbfFieldDef;
  i:integer;
  IntValue:integer;
begin
  fillchar(p,RecordSize,#0);
  p[0] := ' ';

  for i := 0 to _FieldList.Count-1 do begin
    MyFieldDef:=_FieldList.Items[i];
    if (MyFieldDef.NativeFieldType = '+') then begin
      IntValue := MyFieldDef.AutoInc+$80000000;
      PInteger(p+MyFieldDef.Offset)^:= SwapInt(IntValue);
    end;
  end;
end;

procedure TDbfFile.Release;
begin
  if _CntUse=0 then self.free;
end;

procedure TDbfFile.OpenIndex(IndexFileName,IndexField:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
var
//  lFileName:string;
  lIndexFile:TIndexFile;
  lFieldInfo:TDbfFieldDef;
begin
//  lFileName:=IndexFileName;
  lIndexFile:=GetIndexByName(IndexFileName);
  if lIndexFile=nil then begin
    if not fileexists(IndexFileName) then Mode:=pfCreate;
    if mode=pfOpen then begin
      lIndexFile:=TIndexFile.Create(IndexFileName,pfOpen,false,false);
      lFieldInfo:=GetFieldInfo(PNdxHdr(lIndexFile.Header).KeyDesc);
      if lFieldInfo=nil then raise EDbfFile.CreateFmt(STRING_INDEX_BASED_ON_UNKNOWN_FIELD,[PNdxHdr(lIndexFile.Header).KeyDesc]);
      lIndexFile._FieldPos := lFieldInfo.Offset;
      lIndexFile._FieldLen := lFieldInfo.Size;
    end else begin
      lFieldInfo:=GetFieldInfo(IndexField);
      if lFieldInfo=nil then raise EDbfFile.CreateFmt(STRING_INDEX_BASED_ON_UNKNOWN_FIELD,[IndexField]);
      lIndexFile:=TIndexFile.Create(IndexFileName,pfCreate,false,false);
      lIndexFile.SetFieldInfo(lFieldInfo.Offset,lFieldInfo.Size,IndexField);
    end;
    if lIndexFile<>nil then _Indexes.Add(lIndexFile);
  end;
  if lIndexFile=nil then raise EDbfFile.CreateFmt(STRING_CANNOT_OPEN_INDEX,[IndexFileName]);
  if (mode=pfCreate) then begin
    PackIndex(lIndexFile);
  end;
end;

procedure TDbfFile.PackIndex(lIndexFile:TIndexFile);
var
  cur,last:integer;
  lIndexCursor:TIndexCursor;
begin
  lIndexFile.Clear;
  cur:=1;
  try
    lIndexCursor:=TIndexCursor.Create(lIndexFile);
    last:=RecordCount;
    while cur<=last do begin
      ReadRecord(cur, _PrevBuffer);
      lIndexCursor.Insert(cur,_PrevBuffer);
      inc(cur);
    end;
  finally
    FreeAndNil(lIndexCursor);
  end;
end;

procedure TDbfFile.CloseIndex(IndexFileName:string);
var
  i,lIndexNo:integer;
  lIndexFile:TIndexFile;
  lIndex:TIndexFile;
begin
  lIndexNo:=-1;
  lIndex:=nil;
  for i:=0 to _Indexes.Count-1 do begin
    lIndexFile:=TIndexFile(_Indexes.Items[i]);
    if lIndexFile.FileName=IndexFileName then begin
      lIndexNo:=i;
      lIndex:=lIndexFile;
      break;
    end;
  end;
  if (lIndex<>nil) then begin
    _Indexes.Delete(lIndexNo);
    lIndex.Free;
    if (_CurIndex=lIndexNo) then begin
      _CurIndex:=-1;
      //_Cursor:=_DbfCursor;
    end;
  end;
end;

procedure TDbfFile.Insert(Buffer:pchar);
var
  i:integer;
  newrecord:integer;
  lindex:TIndexFile;
begin
  newRecord:=RecordCount+1;
//  _DbfCursor.SetRecNo(newRecord);
	WriteRecord(newRecord,Buffer);
	for i:=0 to _indexes.Count-1 do begin
		lindex:=TIndexFile(_indexes.Items[i]);
     lindex.Insert(newRecord,Buffer);
	end;
// OH 2000-11-15 dBase7 support.
// After insert a new record we have to inc the autoinc fields.
  IncAutoInc;
end;

procedure TDbfFile.Update(Recno:integer;Buffer:pchar);
var
  i:integer;
  lindex:TIndexFile;
begin
  //_Engine._Cursor.Append;
  if _indexes.Count>0 then begin
		ReadRecord(Recno,_PrevBuffer);
  end;
  WriteRecord(Recno,Buffer);
  if _indexes.Count>0 then begin
		for i:=0 to _indexes.Count-1 do begin
			lindex:=TIndexFile(_indexes.Items[i]);
			lindex.Update(Recno,_PrevBuffer,Buffer);
		end;
	end;
end;

procedure TDbfFile._SetRecordSize(value:integer);
begin
  if value<>RecordSize then begin
    if _PrevBuffer<>nil then FreeMem(_PrevBuffer)
    else _PrevBuffer:=nil;
    if value>0 then GetMem(_PrevBuffer,value);
  end;
  inherited;
end;

function TDbfFile.GetIndexByName(lIndexFile:string):TIndexFile;
var
 lCurFile:TIndexFile;
 i:integer;
begin
  Result:=nil;
  lIndexFile:=UPPERCASE(lIndexFile);
  for i:=0 to _Indexes.count-1 do begin
    lCurFile:=TIndexFile(_Indexes.Items[i]);
    if lCurFile.FileName = lIndexFile then begin
      Result:=lCurFile;
      break;
    end;
  end;
end;

//====================================================================
// tDbfCursor
//====================================================================
constructor TDbfCursor.Create(DbfFile:TDbfFile);
begin
  inherited Create(DbfFile);
end;

function TDbfCursor.Next:boolean;
var
  max:integer;
begin
  max:=TDbfFile(PagedFile).RecordCount;
  if _PhysicalRecno<=max then inc(_PhysicalRecno)
  else _PhysicalRecno:=max+1;
	result:=(_PhysicalRecno<=max);
end;

function TDbfCursor.Prev:boolean;
begin
  if _PhysicalRecno>0 then dec(_PhysicalRecno)
  else _PhysicalRecno:=0;
  result:=(_PhysicalRecno>0);
end;

procedure TDbfCursor.First;
begin
  _PhysicalRecno:=0;
end;

procedure TDbfCursor.Last;
var
  max:integer;
begin
  max:=TDbfFile(PagedFile).RecordCount;
  if max=0 then _PhysicalRecno:=0
  else _PhysicalRecno:=max+1;
end;

function TDbfCursor.GetPhysicalRecno:integer;
begin
	result:=_PhysicalRecno;
end;

procedure TDbfCursor.SetPhysicalRecNo(Recno:integer);
begin
	_PhysicalRecno:=Recno;
end;

function TDbfCursor.GetSequentialRecordCount:integer;
begin
  result:=TDbfFile(PagedFile).RecordCount;
end;

function TDbfCursor.GetSequentialRecNo:integer;
begin
	result:=_PhysicalRecno;
end;

procedure TDbfCursor.SetSequentialRecNo(Recno:integer);
begin
	_PhysicalRecno:=Recno;
end;

procedure TDbfCursor.GotoBookmark(Bookmark:rBookmarkData);
begin
  _PhysicalRecno:=Bookmark.Recno;
end;

procedure TDbfCursor.Insert(Recno:integer; Buffer:PChar); {override;}
begin
	_PhysicalRecno:=TDbfFile(PagedFile).RecordCount;
end;

procedure TDbfCursor.Update(Recno: integer; PrevBuffer,NewBuffer: PChar); {override;}
begin
end;

function TDbfCursor.GetBookMark:rBookmarkData; {override;}
begin
  result.IndexBookmark:=-1;
  result.RecNo:=_PhysicalRecno;
end;


//====================================================================
// tDbfDatabase
//====================================================================
constructor TDbfDatabase.Create;
begin
 _DbfFiles:=TList.Create;
 TrimLeft:=false;
 TrimRight:=true;
end;

destructor TDbfDatabase.Destroy; {override;}
begin
 FreeAndNil(_DbfFiles);
end;

procedure TDbfDatabase.CloseDbf(var DbfFile:TDbfFile);
begin
  if DbfFile=nil then exit;
  dec(DbfFile._cntUse);
  if (DbfFile._cntUse<=0) then begin
    FreeAndNil(DbfFile);
  end;
end;

function TDbfDatabase.OpenDbf(lFileName:string;Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean):TDbfFile;
var
  i:integer;
  aDbfFile:TDbfFile;
begin
  lFilename:=lowercase(lFilename);
  lFilename:=ChangeFileExt(lFilename,'.dbf');
  for i:=0 to _DbfFiles.Count-1 do begin
    aDbfFile:=TDbfFile(_DbfFiles.Items[i]);
    if aDbfFile._Filename=lFilename then begin
      inc(aDbfFile._cntUse);
      result:=aDbfFile;
      exit;
    end;
  end;
  try
    Result:=TDbfFile.Create(lFileName,Mode,AutoCreate,ReadOnly);
  except
    raise;
  end;
end;
(*
first
  if TDbfFile(_File).CalcRecordCount=0 then InternalLast
  elseif _curIndex<0 then
  else _curIndex.First;
*)

(*
PhysicalRecno
if _curIndex>=0 then _PhysicalRecno:=_CurIndex.GetRealRecNo;*)

(**
Next
					if _curIndex>=0 then begin
						Acceptable:=_curIndex.Next;
					end else begin
					end;
Prev
					if _curIndex>=0 then begin
						Acceptable:=_curIndex.Prev;
					end else begin
					end;

**)



initialization
  DbfDefaultDatabase := tDbfDatabase.Create;

finalization
  FreeAndNil(DbfDefaultDatabase);

(*
  Stuffs non implemented yet
  TDBaseMDXHeader       = Record
    MDXVersion          : Byte;
    Year                : Byte;
    Month               : Byte;
    Day                 : Byte;
    FileName            : TMDXFileName;
    BlockSize           : Word;
    BlockSizeAdder      : Word;
    ProductionIndexFlag : Byte;
    NumTags             : Byte;
    TagLength           : Byte;
    Reserved_27         : Byte;
    NumTagsUsed         : Word;
    Reserved_30_31      : Word;
    NumPages            : Cardinal;
    FreePage            : Cardinal;
    FreeBlocks          : Cardinal;
    UpdateYear          : Byte;
    UpdateMonth         : Byte;
    UpdateDay           : Byte;
    Reserved_47         : Byte;
  End;
  PDBaseMDXHeader       = ^TDBaseMDXHeader;

  TFoxCDXHeader         = Record
    PointerRootNode     : Integer;
    PointerFreeList     : Integer;
    Reserved_8_11       : Cardinal;
    KeyLength           : Word;
    IndexOption         : Byte;
    IndexSignature      : Byte;
    Reserved_Null       : TFoxReservedNull;
    SortOrder           : Word;
    TotalExpressionLen  : Word;
    ForExpressionLen    : Word;
    Reserved_506_507    : Word;
    KeyExpressionLen    : Word;
    KeyForExpression    : TKeyForExpression;
  End;
  PFoxCDXHeader         = ^TFoxCDXHeader;

  TFoxCDXNodeCommon     = Record
    NodeAttributes      : Word;
    NumberOfKeys        : Word;
    PointerLeftNode     : Integer;
    PointerRightNode    : Integer;
  End;

  TFoxCDXNodeNonLeaf    = Record
    NodeCommon          : TFoxCDXNodeCommon;
    TempBlock           : Array [12..511] of Byte;
  End;
  PFoxCDXNodeNonLeaf    = ^TFoxCDXNodeNonLeaf;

  TFoxCDXNodeLeaf       = Packed Record
    NodeCommon          : TFoxCDXNodeCommon;
    BlockFreeSpace      : Word;
    RecordNumberMask    : Integer;
    DuplicateCountMask  : Byte;
    TrailByteCountMask  : Byte;
    RecNoBytes          : Byte;
    DuplicateCountBytes : Byte;
    TrailByteCountBytes : Byte;
    HoldingByteCount    : Byte;
    DataBlock           : TDataBlock;
  End;
  PFoxCDXNodeLeaf       = ^TFoxCDXNodeLeaf;

*)
end .

