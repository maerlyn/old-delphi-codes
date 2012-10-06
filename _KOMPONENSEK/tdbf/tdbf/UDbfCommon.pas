unit UDbfCommon;

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, ExtCtrls, DBCtrls, Grids, DBGrids, Menus, Buttons,
  ComCtrls;

type
  TDbfFieldType = char;


{$I _DbfCommon.inc}

{$ifndef DELPHI_5}
procedure FreeAndNil(var v);
{$endif}

procedure FileCopy(source,dest:string);

const
  // _MAJOR_VERSION
  _MAJOR_VERSION = 4;
  // _MINOR_VERSION
  _MINOR_VERSION = 010;

type
  EDBFError = Exception;

  PBookMarkData = ^rBookMarkData;
	rBookmarkData = record
    IndexBookmark:longint;
    RecNo:longint;
  end;

implementation

procedure FileCopy(source,dest:string);
begin
  CopyFile(PCHAR(source),PCHAR(dest),FALSE);
end;


{$ifndef DELPHI_5}
procedure FreeAndNil(var v);
begin
  try
    TObject(v).Free;
  finally
    TObject(v):=nil;
  end;
end;
{$endif}


initialization

end.
