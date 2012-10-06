unit UDbfEngine;

interface
uses
  SysUtils,db, classes, ExptIntf, Forms,
  UDbfPagedFile, UDbfFile, UDbfIndex, UDbfMemo,
  UDbfCursor,UDbfFieldDef,UDbfCommon;

{$i _DbfCommon.inc}
type
 	EDbfEngineError = class (Exception);

implementation
(*

procedure TDbfEngine._CloseFiles;
var
  i:integer;
	lindex:TVirtualCursor;
begin
  if _ReadOnly=false then _DbfFile.WriteHeader;


  if _DbfCursor<>nil then begin
    _DbfCursor.Free;
    _DbfCursor:=nil;
  end;

  for i:=_Indexes.Count-1 downto 0 do begin
  	lindex:=TVirtualCursor(_indexes.Items[i]);
    lindex.Free;
  end;
  _Indexes.Clear;

  if _dbtFile<>nil then begin
    _dbtFile.Release;
    _dbtFile:=nil;
  end;

  if (_PrevBuffer<>nil) then begin
    StrDispose(_PrevBuffer);
    _PrevBuffer:=nil;
  end;
  _IsCursorOpen:=false;
end;



procedure TDbfEngine.SetIndex(IndexFileName:string);
var
  i,lIndexNo:integer;
  lIndex,lIndexCursor:TVirtualCursor;
begin
  lIndexNo:=-1;
  for i:=0 to _Indexes.Count-1 do begin
    lIndexCursor:=TIndexCursor(_Indexes.Items[i]);
    if lIndexCursor.PagedFile.FileName=IndexFileName then begin
      lIndexNo:=i;
      lIndex:=lIndexCursor;
      break;
    end;
  end;
  _CurIndex:=lIndexNo;
  if lIndexNo>=0 then _Cursor:=lIndexCursor
  else _Cursor:=_DbfCursor;
  if _Owner.Active then _Owner.Resync([]);
end;


*)

end.





