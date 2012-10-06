unit UDbfMemo;

interface

uses UDbfPagedFile,classes;

//====================================================================
//=== Memo and binary fields support
//====================================================================
type
  PDbtHdr = ^rDbtHdr;
  rDbtHdr = record
    NextBlock:Longint;
    Dummy : array [4..7] of byte;
    _dbfFile : array [0..7] of Byte; //8..15
    bVer : Byte; //16
    Dummy2 : array [17..19] of byte;
    BlockLen:  Word;
    Dummy3 : array [22..511] of byte;
  end;
//====================================================================
  TDBTFile = class(TPagedFile)
  protected
    _DbtVersion:xBaseVersion;
  public
    constructor Create(lFileName:string;Mode:TPagedFileMode;AutoCreate,
    ReadOnly:Boolean;pRecordSize:integer;Ver:xBaseVersion);
    procedure ReadMemo(recno:Integer;Dst:TStream);
    procedure WriteMemo(var MemoRecno:Integer;ReadSize:Integer;Src:TStream);
  end;
  PInteger = ^integer;

implementation
type
  PLONG = ^longint;
//==========================================================
//============ dbtfile
//==========================================================
constructor TDbtFile.Create(lFileName:string;Mode:TPagedFileMode;AutoCreate
  ,ReadOnly:Boolean;pRecordSize:integer;Ver:xBaseVersion);
//(const FileName: string; Mode: Word; );
begin
	inherited Create(lFileName,Mode,AutoCreate,ReadOnly);
	_DbtVersion:=Ver;
  HeaderSize:=SizeOf(rDbtHdr);
  if (_mode = pfCreate) or (HeaderSize=0) then begin
    PDbtHdr(Header).NextBlock := 1;
    PDbtHdr(Header).BlockLen:=pRecordSize;
    WriteHeader;
  end;
  RecordSize:=PDbtHdr(Header).BlockLen;


  if (RecordSize=0) or ((RecordSize mod 128)<>0) then begin
    PDbtHdr(Header).BlockLen := $200;
    RecordSize := $200;
  end;
  // Can you tell me why the header of dbase3 memo contains 1024 and is 512 ?
  if _DbtVersion=xBaseIII then RecordSize:=512;
  HeaderSize:=RecordSize;
end;


procedure TDbtFile.ReadMemo(recno:Integer;Dst:TStream);
var
  Buff:array[0..4095] of char;
  i,lsize:integer;
  finish:boolean;
  lastc:char;
  bytebefore:integer;
begin
  if (recno=0) or (RecordSize=0) then Exit;
  if _DbtVersion >= xBaseIV then begin // dBase4 memofiles
    self.ReadRecord(recno,@Buff[0]);           {#LWL#}
    if (Buff[0]=#$ff) and  (Buff[1]=#$ff) and
      (Buff[2]=#$08) and (Buff[3]=#$00) then begin
          // dbase IV memo
      lsize:=(PInteger(@Buff[4])^)-8;
      bytebefore:=8;
    end else begin
      bytebefore:=0;
      lsize:=0;
    end;
    repeat
      if lsize>RecordSize-bytebefore then begin
        Dst.Write((@buff[bytebefore])^,RecordSize-bytebefore);
        Dec(lsize,RecordSize-bytebefore);
        bytebefore:=0;
        inc(recno);
        ReadRecord(recno,@buff[0]);  {#LWL#}
      end else if lsize>0 then begin
        Dst.Write(buff[bytebefore],lsize);
        lsize:=0;
      end;
    until lsize=0;
  end else begin
    finish:=False;
    lastc:=#0;
    repeat
      ReadRecord(recno,@buff[0]);  {#LWL#}
      for i:=0 to RecordSize-2 do begin
        if ((Buff[i]=#$1A) and
          ((Buff[i+1]=#$1A) or ((i=0) and (lastc=#$1A))))
          or (Buff[i]=#$0)
          then begin
          if i>0 then Dst.Write(buff,i);
          finish:=True;
          break;
        end;
      end;
      if finish then Break;
      Dst.Write(buff,512);
      lastc:=Buff[511];
      inc(recno);
    until finish;
  end;
  Dst.Position:=0;
end;

procedure TDbtFile.WriteMemo(var MemoRecno:Integer;ReadSize:Integer;Src:TStream);
var
  ByteBefore:Integer;
  ByteAfter:Integer;
  Buff:array[0..4095] of char;
  totsize:Integer;
  read:Integer;
  Append:Boolean;
  tmpRecNo: Integer;
begin
  if _DbtVersion >= xBaseIV then begin // dBase4 memofiles
    ByteBefore:=8;
    ByteAfter:=0;
  end else begin // stupid files
    ByteBefore:=0;
    ByteAfter:=2;
  end;
  if Src.Size = 0 then begin
    MemoRecno:=0;
  end else begin
    if ((ByteBefore+Src.Size+ByteAfter+PDbtHdr(Header).BlockLen-1) div PDbtHdr(Header).BlockLen)
      <= ((ReadSize+PDbtHdr(Header).BlockLen-1) div PDbtHdr(Header).BlockLen)
      then begin
      Append:=false;
    end else begin
      Append:=True;
      MemoRecno:=PDbtHdr(Header).NextBlock;
      if MemoRecno=0 then begin
        PDbtHdr(Header).NextBlock:=1;
        MemoRecno:=1;
      end;
    end;
    tmpRecNo := MemoRecno;
    Src.Position:=0;
    fillChar(Buff[0],RecordSize,' ');
    if ByteBefore=8 then begin
      totsize:=Src.Size+ByteBefore+ByteAfter;
      PLONG(@buff[0])^:=$0008ffff;
      PLONG(@buff[4])^:=totsize;
    end;
    repeat
      read:=Src.Read(buff[bytebefore],PDbtHdr(Header).BlockLen-bytebefore);
      if read=0 then break;
      Inc(PDbtHdr(Header).NextBlock);
      if read>=512-bytebefore-byteafter then begin
        bytebefore:=0;
        read:=0;
        WriteRecord(TmpRecno,@Buff[0]); {#LWL#}
        inc(TmpRecno);
      end else break;
      fillChar(Buff[0],RecordSize,' ');
    until false;

    if ByteAfter=2 then begin
      Buff[read]:=#$1A;
      Buff[read+1]:=#$1A;
    end;
    WriteRecord(TmpRecNo,@Buff[0]);     {#LWL}
    if Append then begin
      WriteHeader;
    end;
  end;
end;

end.
