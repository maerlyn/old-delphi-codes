unit UDbfCursor;
interface

uses
  classes,db,
  UDbfPagedFile,UDbfCommon;

type
//====================================================================
  TVirtualCursor = class
  private
    _file:TPagedFile;
  public
    property PagedFile:TPagedFile read _file;
    constructor Create(pFile:TPagedFile);
    destructor destroy; override;

    function RecordSize : Integer;
    function Next:boolean; virtual; abstract;
    function Prev:boolean; virtual; abstract;
    procedure First; virtual; abstract;
    procedure Last; virtual; abstract;
    function GetPhysicalRecno:integer; virtual; abstract;
    procedure SetPhysicalRecno(Recno:integer); virtual; abstract;
    function GetSequentialRecordCount:integer; virtual; abstract;
    function GetSequentialRecno:integer; virtual; abstract;
    procedure SetSequentialRecno(Recno:integer); virtual; abstract;
    function GetBookMark:rBookmarkData; virtual; abstract;
    procedure GotoBookmark(Bookmark:rBookmarkData); virtual; abstract;
		procedure Insert(Recno:integer; Buffer:PChar); virtual; abstract;
		procedure Update(Recno: integer; PrevBuffer,NewBuffer: PChar); virtual; abstract;
  end;

implementation

function TVirtualCursor.RecordSize : Integer;
begin
  if _file = nil then result:=0 else result:=_file.recordsize;
end;

constructor TVirtualCursor.Create(pFile:TPagedFile);
begin
  _File:=pFile;
end;

destructor TVirtualCursor.destroy; {override;}
begin
end;

end.


