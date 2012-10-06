unit UDbfIndexFile;

interface

uses
  sysutils,classes, db,
  UDbfPagedFile,UDbfCursor,UDbfCommon;

const
  RecEOF=MAXINT;
  RecBOF=0;
  BmInvalid=-1;
  
type
  TIndexFile = class;
  TIndexPage = class;

  PDouble = ^double;

  PMdxTag = ^rMdxTag;
  rMdxTagHdr = record
    RootPage        : longint;// 0..3
    FilePages        : longint;// 4..7
    KeyFormat        : byte;   // 8
    KeyType          : char;   // 9
    dummy            : word;   // 10..11
    IndexKeyLength   : word;   // 12..13
    MaxNbKeys       : word;   // 14..15
    SecondKeyType   : word;   // 16..17
    IndexKeyItemLen  : word;   // 18..19
    dummy2           : array [20..22] of byte;
    UniqueFlag      : byte;   // 23
  end;

  PNdxHdr = ^rNdxHdr;
	rNdxHdr = record
		startpage : Integer; // 0..3
    nbPage : Integer; // 4..7
		keyformat: Char; //8
    keytype : char; //9
    dummy : Word; // 10..11
    keylen : Word; // 12..13
    nbkey : Word; // 14..15
    skeytype : Word; // 16..17
    keyreclen : Word; // 18..19
    dummy2 : Word; // 20..21
    dummy3 : Byte; // 22
    Unique : Byte; // 23
		KeyDesc : array[0..231] of char; // 24...255
    dummy4 : array[256..511] of byte;
  end;

  rMdxTag = record
    pageno      : Integer; // 0..3
    tagname      : array [0..11] of char; // 4..14
    keyformat    : byte; // 15
    forwardTag1  : char; // 16
    forwardTag2 : byte; // 17
    backwardTag : byte; // 18
		dummy       : byte; // 19
    keytype     : byte; // 20
  end;

  NdxKeyType = (N,C);
  PNdxPage  = ^rNdxPage;
  rNdxPage  = record
    NbEntries : longint;  //  0..3 lower page
    Entries   : ARRAY [0..507] OF char;
  end;

  PNdxentry  = ^rNdxentry;
  rNdxentry  = record
    _LowerPage : longint;  //  0..3 lower page
    RecNo     : Longint;  //  4..7 recno
    case NdxKeyType of
      N: ( NKey: double);
      C: ( CKey: array [0..503] of char);
  end;
//------------------------------------------------------------------------------
  TIndexPage = class
  protected
    _IndexFile : TIndexFile;
    _PageNo : Integer;
    _EntryNo : Integer;
    _LowerLevel : TIndexPage;
    _UpperLevel : TIndexPage;
    _Weight : integer;
    _Modified: boolean;
    _Entry : PNdxentry;
		_PageBuff:rNdxPage;
    _Magic:longint;
    procedure _setModified(v:boolean);
    property Modified:boolean read _Modified write _SetModified;

    function  LastEntryNo:integer;
    function  LocalInsert(Recno:Integer; Buffer:PChar; LowerPage:integer):boolean;
    function  LocalDelete:boolean;

    function  GetPEntry(vEntryNo:integer):PNdxEntry;


    function RecurPrev:boolean;
    function RecurNext:boolean;
    procedure RecurFirst;
    procedure RecurLast;
    function RecurInsert(Recno:integer; Buffer:pchar; LowerPage:integer):boolean;
  public
		_dbfFile:TPagedFile;


    constructor Create(Parent:TIndexFile);
    destructor Destroy; override;

    function LowerLevel:TIndexPage;
    procedure SetPageNo(page:Integer);
    property UpperLevel:TIndexPage read _UpperLevel;
    procedure _SetEntryNo(value:Integer);
    function FindNearest(Recno:integer; Key:PChar):integer;
    procedure SetEntry(Recno:integer; key:pchar; LowerPage:integer);
    function Delete:boolean;
    property EntryNo:integer read _EntryNo write _SetEntryNo;
    function PhysicalRecno:integer;
  end;
//==============================================================================
(*
	rMdxHdr = record
    MdxHdr   : byte;       // 0
    Year        : byte;       // 1
    Month       : byte;       // 2
    Day         : byte;      // 3
    FileName    : array[0..15] of char; // 4..19 of byte
    BlockSize    : word; // 20 21
    BlockAdder  : word; // 22 23
    IndexFlag   : byte; // 24
    NoTag       : byte; // 25
    TagSize     : byte; // 26
    Dummy1      : byte; // 27
    TagUsed     : word; // 28..29
    Dummy2      : word; // 30..31
    NbPage      : Integer; // 32..35
    FreePage    : Integer; // 36..39
    BlockFree   : Integer; // 40..43
    UpdYear     : byte; // 44
    UpdMonth    : byte; // 45
    UpdDay      : byte; // 46
	end;
*)
//==============================================================================
  TIndexFile = class(TPagedFile)
  protected
    _IndexVersion : xBaseVersion;
//    _MdxHdr : rMdxHdr;
		_NbLevel : integer;
		_Root:TIndexPage;
    _Leaf:TIndexPage;
  public
		_FieldPos : integer;
		_FieldLen : integer;
		function _Delete:boolean;
    procedure Clear;
    procedure Init;
    procedure SetFieldInfo(fieldStart,fieldLen:integer; FieldDesc:string);
    procedure AddNewLevel;
    constructor Create(lFileName:string; Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
		function Find(Recno:integer; Buffer:PChar):integer;
		procedure Update(Recno: integer; PrevBuffer,NewBuffer: PChar);
		procedure Insert(Recno:integer; Buffer:PChar);
    destructor Destroy; override;
    function GetRecordCount:integer;
    function GotoBookmark(IndexBookmark:rBookmarkData):boolean;
    procedure GotoRecno(Recno:Integer);
    function GetBookMark:rBookmarkData;
    procedure CheckPos(IndexBookmark:rBookmarkData);
    procedure First;
    procedure Last;
    function Next:boolean;
    function Prev:boolean;
    function PhysicalRecno:integer;
    function GetKey:string;
    function GetSequentialRecordCount:integer; 
    function GetSequentialRecno:integer;
    procedure SetSequentialRecNo(Recno:integer);
  end;

implementation
var
  ENTRY_BOF:rNdxentry;
  ENTRY_EOF:rNdxentry;

procedure MyMove(Source, Dest:PChar; Count: Integer);
var
  c:char;
  i:integer;
begin
  i:=0;
  while i<Count do begin
    c:=PChar(Source)[i];
    if c=#0 then break;
    PChar(Dest)[i]:=c;
    Inc(i);
  end;
  while i<Count do begin
    PChar(Dest)[i]:=' ';
    Inc(i);
  end;
end;

//==========================================================
//============ TIndexPage
//==========================================================
constructor TIndexPage.Create(Parent:TIndexFile);
begin
  _Magic:=123456789;
  _LowerLevel:=nil;
  _UpperLevel:=nil;
  _IndexFile:=Parent;
  _PageNo:=-1;
  _EntryNo:=-1;
  _Entry:=@ENTRY_BOF;
  modified:=false;
end;

destructor TIndexPage.Destroy;
begin
  if (_LowerLevel<>nil) then LowerLevel.Free;
  if Modified then SetPageNo(1);
	inherited Destroy;
end;

function  TIndexPage.GetPEntry(vEntryNo:integer):PNdxEntry;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  Result:=PNdxentry(@_PageBuff.Entries[PNdxHdr(_IndexFile.Header).keyreclen*vEntryno]);
end;

function  TIndexPage.LocalInsert(Recno:integer; Buffer:Pchar;LowerPage:integer):boolean;
var
  src,dst:pointer;
  siz:integer;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  assert(_PageBuff.NbEntries>=0);
  if _PageBuff.NbEntries < PNdxHdr(_IndexFile.Header).nbkey then begin
    src:=_Entry;
    dst:=GetPEntry(EntryNo+1);
    siz:=(_PageBuff.NbEntries - EntryNo)
      * PNdxHdr(_IndexFile.Header).keyreclen + 8;
    Move(Src^, Dst^, Siz);
    inc(_PageBuff.NbEntries);
    SetEntry(Recno,Buffer,LowerPage);
    Result:=true;
  end else begin
    Result:=false;
  end;
end;


function  TIndexPage.LocalDelete:boolean;
var
  src,dst:pointer;
  siz:integer;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  if _PageBuff.NbEntries >=0 then begin
    if EntryNo<_PageBuff.NbEntries then begin
      src:=GetPEntry(EntryNo+1);
      dst:=_Entry;
      siz:=(_PageBuff.NbEntries - EntryNo - 1)
        * PNdxHdr(_IndexFile.Header).keyreclen + 8;
      Move(Src^, Dst^, Siz);
    end;
    dec(_PageBuff.NbEntries);
    if (EntryNo>LastEntryNo) then begin
      EntryNo:=LastEntryNo; // We just removed the last on this page.
      if UpperLevel<>nil then begin
        UpperLevel.SetEntry(0,_Entry.CKey,_PageNo);
      end;
    end;
    if ((_PageBuff.NbEntries=0) and (_LowerLevel=nil))
      or (_PageBuff.NbEntries<0) then begin
      if UpperLevel<>nil then begin
        UpperLevel.LocalDelete;
      end;
    end;
    modified:=true;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TIndexPage._setModified(v:boolean);
begin
  if v then begin
    _IndexFile.WriteRecord(_PageNo,@_PageBuff);
    v:=false;
  end;
  _modified:=v;
end;

function TIndexPage.LastEntryNo:integer;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  if (_LowerLevel=nil) then result:=_PageBuff.NbEntries - 1
  else result:=_PageBuff.NbEntries;
  if result<0 then result:=0;
end;

function TIndexPage.FindNearest(Recno:integer; Key:pchar):integer;
var
  cmpres:integer;
  v1,v2:double;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  Result:=-1;
  if @Key=nil then begin
    Exit;
  end;
  EntryNo:=0;
  while EntryNo<_PageBuff.NbEntries do begin
    if PNdxHdr(_IndexFile.Header).keytype='C' then begin
//      if Assigned(TDbf(Self. OnCompareRecord) then begin
//      end else begin
        cmpres:=StrLIComp(PChar(Key),_Entry.CKey,_IndexFile._FieldLen);
//      end;
    end else begin
      // Numeric field... to do
      v1:=PDouble(Key)^;
      v2:=_Entry.NKey;
      if v1>v2 then cmpres:=1
      else if v1<v2 then cmpres:=-1
      else cmpres:=0;
    end;
    if cmpres=0 then begin
      if (_LowerLevel=nil) then begin
        if (_Entry.RecNo=Recno) then begin
          result:=0;
          Exit;
        end else if (_Entry.Recno>Recno) then begin
          result:=-1;
          Exit;
        end;
      end else begin
{        p:=self;
        while p._HasLowerLevel do begin
          p:=p.LowerLevel;
          p.LocalLast;
        end;
        if (p._Entry.Recno>=Recno) then begin
          result:=-1;
          Exit;
        end;}
      end;
    end else if cmpres<0 then begin
      result:=-1;
      exit;
    end;
    EntryNo:=EntryNo+1;
  end;
  result:=1;
  Exit;
end;

procedure TIndexPage.SetEntry(Recno:Integer; key:PChar; LowerPage:integer);
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  assert((EntryNo>=0) and (EntryNo<=_PageBuff.NbEntries));
  if (EntryNo=_PageBuff.NbEntries) then begin
    if (UpperLevel<>nil)  then begin
      UpperLevel.SetEntry(0,key,Self._PageNo);
    end;
  end else begin
    if PNdxHdr(_IndexFile.Header).keytype='C' then begin
      mymove(key,_Entry.CKey,PNdxHdr(_IndexFile.Header).keylen);
    end else begin
      _Entry.NKey:=PDouble(key)^;
    end;
  end;
  _Entry.RecNo:=RecNo;
  _Entry._LowerPage:=LowerPage;
  Modified:=true;
end;

function TIndexPage.LowerLevel:TIndexPage;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  assert(_LowerLevel<>nil);
  assert(EntryNo>=0);
  assert(_Entry<>nil);
  assert(_Entry._LowerPage<>0);
  if (_LowerLevel._PageNo<>_Entry._LowerPage) then begin
    _LowerLevel.SetPageNo(_Entry._LowerPage);
  end;
  result:=_LowerLevel;
end;

function TIndexPage.RecurInsert(Recno:Integer; Buffer:PChar; LowerPage:integer):boolean;
var
  src,dst:PNdxEntry;
  sizb:integer;
  split,old_entry:integer;
  siz1,siz2:integer;
  lSpare:TIndexPage;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));

  if LocalInsert(recno,buffer,lowerpage) then begin
    Write;
  end else begin
    // The _Entry is FULL so we will split this page
    // 1 - Check parent exist
    if UpperLevel=nil then begin
      _IndexFile.AddNewLevel;
      Upperlevel.EntryNo:=0;
      Upperlevel.SetEntry(0,nil,_PageNo);
    end;

    old_entry:=EntryNo;
    split:=EntryNo;
    if split < 2 then begin
      split:=2;
    end;
    if split > _PageBuff.NbEntries-2 then begin
      split:=_PageBuff.NbEntries-2;
    end;
    siz1:=split;
    siz2:=_PageBuff.NbEntries-split;

    lSpare:=TIndexPage.Create(_IndexFile);
    try
      // 2 - Create new page with first part
      inc(PNdxHdr(_IndexFile.Header).nbPage);
      lSpare._PageNo:=PNdxHdr(_IndexFile.Header).nbPage;
      _IndexFile.WriteHeader;
      sizb:=siz1*PNdxHdr(_IndexFile.Header).keyreclen;
      if (_LowerLevel<>nil) then begin
        // size = split - 1 complete records
        //        + just 8 byte for last record
        dec(siz1);
      end;
      src:=@_PageBuff.Entries;
      dst:=@lSpare._PageBuff.Entries;
      Move(src^,dst^,sizb);
      lSpare._PageBuff.NbEntries:=siz1;

      // 3 - Keep only end-part in this page
      sizb:=siz2*PNdxHdr(_IndexFile.Header).keyreclen+8;
      if (_LowerLevel<>nil) then begin
        // size = split - 1 complete records
        //        + just 8 byte for last record
        //dec(siz2);
      end;

      EntryNo:=split;
      src:=_Entry;
      EntryNo:=0;
      dst:=_Entry;
      Move(src^,dst^,sizb);
      _PageBuff.NbEntries:=siz2;

      Modified:=true;
      lSpare.Modified:=true;
      // Now insert the new node into its parent.
      if (_LowerLevel<>nil) then lSpare.EntryNo:=lSpare._PageBuff.NbEntries
      else lSpare.EntryNo:=lSpare._PageBuff.NbEntries-1;
      UpperLevel.RecurInsert(0,lSpare._Entry.CKey,lSpare._PageNo);
      // We need to insert the record now
      if old_entry>=split then begin
        UpperLevel.EntryNo:=UpperLevel.EntryNo+1;
        assert(Upperlevel._Entry._LowerPage=_PageNo);
        EntryNo:=old_entry - split;
        assert(entryno<>-1);
        LocalInsert(Recno,Buffer,LowerPage);
      end else begin
        UpperLevel.LowerLevel; //<-does the trick
        EntryNo:=old_entry;
        LocalInsert(Recno,Buffer,LowerPage);
      end;
    finally
      lspare.free;
    end;
  end;
  // And finally let's check we are on the inserted item.
  assert(_Entry.Recno=Recno);
  assert(_Entry._LowerPage=Lowerpage);
  Result:=true;
end;

function TIndexPage.Delete:boolean;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  Result:=LocalDelete;
end;

procedure TIndexPage.SetPageNo(page:Integer);
begin
  if (_PageNo<>page) then begin
    if (modified) then begin
      assert(_PageNo>0);
      _IndexFile.WriteRecord(_PageNo,@_PageBuff);
      Modified:=false;
    end;
    _PageNo:=page;
    _Entry:=@ENTRY_BOF;
    if (page>0) and (page<=_IndexFile.RecordCount) then begin
      _IndexFile.ReadRecord(Page,@_PageBuff);
    end else begin
      //page:=0;
    end;
    modified:=false;
  end;
end;

procedure TIndexFile.AddNewLevel;
var
  lNewPage:TIndexPage;
begin
  lNewPage:=TIndexPage.Create(self);
  inc(PNdxHdr(Header).nbPage);
  lNewPage._PageNo:= PNdxHdr(Header).nbPage;
  PNdxHdr(Header).startpage:= PNdxHdr(Header).nbPage;
  WriteHeader;

  lNewPage._PageBuff.NbEntries:=0;
  lNewPage._UpperLevel:=nil;
  lNewPage._LowerLevel:=_Root;
  lNewPage._Weight :=_Root._Weight * (PNdxHdr(Header).nbkey+1);


  _Root._UpperLevel:=lNewPage;
  _Root:=lNewPage;
end;

procedure TIndexPage._SetEntryNo(value:Integer);
var
  off:integer;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  if (value<0) then begin
    if _LowerLevel=nil then 
    _EntryNo:=-1;
    _Entry:=@ENTRY_BOF;
  end else if (value>_PageBuff.NbEntries) then begin
    _EntryNo:=_PageBuff.NbEntries+1;
    _Entry:=@ENTRY_EOF;
  end else begin
    off:=PNdxHdr(_IndexFile.Header).keyreclen*value;
    ASSERT((off>=0) and (off<=_IndexFile.RecordSize));
    _EntryNo:=value;
    _Entry:=PNdxentry(@_PageBuff.Entries[off]);
  end;
end;

function TIndexPage.PhysicalRecno:integer;
begin
  if (_EntryNo>=0) and (_EntryNo<_PageBuff.NbEntries) then Result:=_Entry.RecNo
  else Result:=-1;
end;

//==============================================================================
//============ TIndexFile
//==============================================================================
constructor TIndexFile.Create(lFileName:string; Mode:TPagedFileMode;AutoCreate,ReadOnly:Boolean);
var
  ext:string;
begin
  inherited Create(lFileName,Mode,AutoCreate,ReadOnly);

  HeaderSize:=512;
  RecordSize:=512;

  ext:=ExtractFileExt(lFileName);
  if (ext='.mdx') then begin
    _IndexVersion:=xBaseIV;
  end else begin
    _IndexVersion:=xBaseIII;
  end;
  _Root:=TIndexPage.Create(self);
  if Mode=pfCreate then Clear
  else _Root.SetPageNo(PNdxHdr(Header).startpage);
  Init;
end;

procedure TIndexFile.Clear;
begin
  PNdxHdr(Header).startpage:=1;
  PNdxHdr(Header).nbPage:=1;
  WriteHeader;
  RecordCount:=1;
  _Root._PageNo:=1;
  fillchar(_Root._PageBuff,sizeof(_Root._PageBuff),0);
  Init;
end;

procedure TIndexFile.Init;
var
  lPos:TIndexPage;
  lChild:TIndexPage;
begin
  lPos:=_Root;
  _nblevel:=1;
  _Root._Weight:=1;

  if _Root._LowerLevel<>nil then begin
    _Root._LowerLevel.Free;
    _Root._LowerLevel:=nil;
  end;
  repeat
    lPos.EntryNo:=0;
    if lPos._Entry._LowerPage=0 then break;
    inc(_nblevel);
    lChild:=TIndexPage.Create(self);
    lChild._UpperLevel:=lPos;
    lPos._LowerLevel:=lChild;
    lChild.SetPageNo(lPos._Entry._LowerPage);
    lPos:=lChild;
  until false;
  _Leaf:=lPos;
  lPos._Weight:=1;
  repeat
    if lPos.UpperLevel=nil then break;
    lPos.UpperLevel._Weight:=lPos._Weight * (PNdxHdr(Header).nbkey+1);
    lPos:=lPos.UpperLevel;
  until false;
end;

procedure TIndexFile.SetFieldInfo(fieldStart,fieldLen:integer; FieldDesc:string);
begin
  PNdxHdr(Header).keytype:='C';
  PNdxHdr(Header).keylen:=fieldLen;
  PNdxHdr(Header).keyreclen:=fieldLen+8;

  PNdxHdr(Header).startpage:=1;
  PNdxHdr(Header).nbPage:=1;
  PNdxHdr(Header).keyformat:=#0;

  PNdxHdr(Header).dummy:=$5800;
  PNdxHdr(Header).Unique:=0;
  PNdxHdr(Header).KeyDesc[0]:=' ';
  PNdxHdr(Header).nbkey:=(512-8) div (fieldLen+8);
  StrLCopy(PNdxHdr(Header).KeyDesc,PChar(UpperCase(fieldDesc)),255);
  WriteHeader;
  RecordCount:=0;
  _FieldPos := fieldstart;
  _FieldLen := fieldlen;
  _Root.SetPageNo(PNdxHdr(Header).startpage);
end;

destructor TIndexFile.Destroy;
begin
	inherited Destroy;
end;

procedure TIndexFile.Insert(Recno:integer; Buffer:PChar); {override;}
begin
  if _FieldLen=0 then exit;
  inc(Buffer,_FieldPos);
  Find(Recno,Buffer);
  _Leaf.RecurInsert(Recno,Buffer,0);
end;

function TIndexFile._Delete:boolean;
var
  lPos:TIndexPage;
begin
  lpos:=_Leaf;
  if lpos<>nil then begin
    lPos.Delete;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

procedure TIndexFile.Update(Recno: integer; PrevBuffer,NewBuffer: PChar);
(*
function TIndexFile.Find(Recno:integer; Buffer:PChar; var pPos:TIndexPage):integer;
var
  res:integer;
begin
  pPos:=_Root;
  repeat
    res:=pPos.FindNearest(Recno,Buffer);
    if res<>0 then begin
      if pPos._Entry._LowerPage<>0 then begin
        pPos:=pPos.LowerLevel;
        res:=2;
      end;
    end;
  until res<>2;
  Result:=res;
*)
var
  lPos:TIndexPage;
begin
  if _FieldLen=0 then exit;

  inc(PrevBuffer,_FieldPos);
  inc(NewBuffer,_FieldPos);

  if StrLComp(PrevBuffer,NewBuffer,_FieldLen)<>0 then begin
    _Delete;
    Find(Recno,NewBuffer);
    _Leaf.RecurInsert(Recno,NewBuffer,0);
  end;
end;


function TIndexFile.Find(Recno:integer; Buffer:PChar):integer;
var
  res:integer;
  lPos:TIndexPage;
begin
  lPos:=_Root;
  repeat
    res:=lPos.FindNearest(Recno,Buffer);
    if res<>0 then begin
      if lPos._LowerLevel<>nil then begin
        lPos:=lPos.LowerLevel;
        res:=2;
      end;
    end;
  until res<>2;
  Result:=res;
end;

function TIndexFile.GetRecordCount:integer;
begin
  result:=(_Root._PageBuff.NbEntries + 1)* _Root._Weight;
end;

function TIndexFile.GotoBookmark(IndexBookmark:rBookmarkData):boolean;
var
  lpos : TIndexPage;
  n:longint;
  r:longint;
begin
  if (IndexBookmark.RecNo<=0) then begin
    First;
  end else if (IndexBookmark.RecNo=MAXINT) then begin
    Last;
  end else if IndexBookmark.IndexBookmark>=0 then begin
    lpos:=_Root;
    r:=IndexBookmark.IndexBookmark;
    repeat
      if lPos._Weight=0 then begin
        lPos._Weight:=1;
      end;
      n:=r div lPos._Weight;
      r:=r mod lPos._Weight;
      lPos.EntryNo:=n;
      if lpos._LowerLevel=nil then break;
      if lpos._Entry._LowerPage=0 then begin
        GotoRecno(IndexBookmark.Recno);
        exit;
      end;
      lpos:=lpos.LowerLevel;
    until false;
  end;
  if (_Leaf._Entry.RecNo<>IndexBookmark.RecNo) and
    (IndexBookmark.RecNo>0) then begin
    GotoRecno(IndexBookmark.RecNo);
  end;
end;

procedure TIndexFile.GotoRecno(Recno:Integer);
begin
  First;
  repeat
    if _Leaf._Entry.RecNo=Recno then exit;
  until Next=false;
end;

function TIndexFile.GetBookMark:rBookmarkData;
var
  lpos : TIndexPage;
  r,b:integer;
begin
  if _leaf._EntryNo<0 then begin
    r:=RecBOF; // BOF
    b:=bmInvalid;
  end else if _leaf._EntryNo>(_leaf._PageBuff.NbEntries-1) then begin
    r:=RecEOF; // EOF;
    b:=bmInvalid;
  end else begin
    r:=_Leaf._Entry.RecNo;
    lpos:=_Root;
    b:=0;
    repeat
      inc(b,lPos.EntryNo * lPos._Weight);
      if lpos._Lowerlevel=nil then break;
      lpos:=lpos.LowerLevel;
    until lpos=nil;
  end;

  //result.BookmarkFlag:=TBookmarkFlag(-1);// I should not worry with this
  result.IndexBookmark:=b;
  result.RecNo:=r;
end;

procedure TIndexFile.CheckPos(IndexBookmark:rBookmarkData);
begin
  if (_leaf._Entry.RecNo<>IndexBookMark.RecNo) then begin
    GotoBookmark(IndexBookMark);
  end;
{$ifdef debug}
  if (_leaf._Entry.RecNo<>IndexBookMark.RecNo) then begin
    MessageBox('Problem there');
  end;
{$endif}
end;

function TIndexPage.RecurPrev:boolean;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  if EntryNo>0 then begin
    EntryNo:=EntryNo-1;
    if (_LowerLevel<>nil) then begin
      LowerLevel.RecurLast;
    end;
    Result:=true;
  end else if _UpperLevel<>nil then begin
    Result:=_UpperLevel.RecurPrev;
  end else begin
    Result:=false;
  end;
end;

function TIndexPage.RecurNext:boolean;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));

  if (EntryNo<LastEntryNo) then begin
    EntryNo:=EntryNo+1;
    if (_LowerLevel<>nil) then begin
      LowerLevel.RecurFirst;
    end;
    Result:=true;
  end else if _UpperLevel<>nil then begin
    Result:=_UpperLevel.RecurNext;
  end else begin
    Result:=false;
  end;
end;

procedure TIndexPage.RecurFirst;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  EntryNo:=0;
  if (_LowerLevel<>nil) then begin
    LowerLevel.RecurFirst;
  end;
end;

procedure TIndexPage.RecurLast;
begin
  assert((UpperLevel=nil) or ((UpperLevel._Entry<>nil) and (UpperLevel._Entry._LowerPage=_PageNo)));
  EntryNo:=LastEntryNo;
  if (_LowerLevel<>nil) then begin
    LowerLevel.RecurLast;
  end;
end;

procedure TIndexFile.First;
begin
  _Root.RecurFirst;
  _Leaf._EntryNo:=-1; // out of index - BOF
end;

procedure TIndexFile.Last;
begin
  _Root.RecurLast;
  _Leaf._EntryNo:=(_leaf._PageBuff.NbEntries-1)+1; // out of index - EOF
end;

function TIndexFile.Prev:boolean;
begin
  Result:=_Leaf.RecurPrev;
end;

function TIndexFile.Next:boolean;
begin
  Result:=_Leaf.RecurNext;
end;

function TIndexFile.PhysicalRecno:integer;
begin
  if (_Leaf._EntryNo>=0)
    and (_Leaf._EntryNo<=_leaf._PageBuff.NbEntries) then begin
    result:=_Leaf._Entry.RecNo;
  end else begin
    result:=-1;
  end;
end;

function TIndexFile.GetKey:string;
begin
  result:=_Leaf._Entry.CKey;
end;

function TIndexFile.GetSequentialRecordCount:integer;
begin
  Result:=_Root._Weight * _Root.LastEntryNo;
end;

function TIndexFile.GetSequentialRecno:integer;
begin
  Result:=GetBookMark.IndexBookmark;
end;

procedure TIndexFile.SetSequentialRecNo(Recno:integer);
var
  IndexBookmark:rBookmarkData;
begin
  //IndexBookmark.BookmarkFlag:=TBookmarkFlag(-1);
  IndexBookmark.IndexBookmark:=Recno;
  IndexBookmark.RecNo:=-1;
  GotoBookmark(IndexBookmark);
end;

initialization
  ENTRY_BOF._LowerPage:=0;
  ENTRY_BOF.RecNo:=RecBOF;
  ENTRY_BOF.CKey[0]:=#0;

  ENTRY_EOF._LowerPage:=0;
  ENTRY_EOF.RecNo:=RecEOF;
  ENTRY_EOF.CKey[0]:=#0;
end.
