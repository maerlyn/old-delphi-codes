unit ClientUtils;

interface

uses
  SysUtils,
  Classes,
  SDL_Net;

type
  TClientRecord = packed record
    Name : string;
    Socket : TTCPSocket;
  end;
  PClientRecord = ^TClientRecord;

  TClientList = class
  private
    FList : TList;
    function GetName( index : integer ) : string;
    function GetCount : Integer;
    function GetSocket( index : integer ) : PTCPSocket;
  protected
    function CreateList : TList; dynamic;
    property List : TList read FList;
  public
    constructor Create;
    destructor Destroy; override;
    function Add( Name : string; Socket : PTCPSocket ) : Integer; dynamic;
    function Remove( Index : integer ) : Boolean; overload;
    function Remove( Name : string ) : Boolean; overload;
    function Find( Name : string ) : Integer;
    procedure Clear; dynamic;
    property Name[ index : integer ] : string read GetName;
    property Socket[ index : integer ] : PTCPSocket read GetSocket;
    property Count : Integer read GetCount;
  end;

implementation

{ TClientList }

function TClientList.Add( Name : string; Socket : PTCPSocket ) : Integer;
var
  Item : PClientRecord;
begin
  GetMem( Item, Sizeof( TClientRecord ) );
  Item^.Name := Trim( Name );
  Item^.Socket := Socket^;
  try
    Result := List.Add( Item );
    if Result < 0 then
      raise EOutOfResources.Create( 'Out of Memory - Could not add new Client' );
  except
    Result := -1;
    FreeMem( Item );
  end;
end;

procedure TClientList.Clear;
var
  index : integer;
begin
  for index := 0 to List.Count - 1 do
  begin
    FreeMem( List[ index ] );
  end;
  List.Clear;
end;

constructor TClientList.Create;
begin
  inherited Create;
  FList := CreateList;
end;

function TClientList.CreateList : TList;
begin
  Result := TList.Create;
end;

destructor TClientList.Destroy;
begin
  Clear;
  List.Free;
  inherited Destroy;
end;

function TClientList.Find( Name : string ) : Integer;
var
  Index : Integer;
begin
  Result := -1;
  for index := 0 to List.Count - 1 do
  begin
    if TClientRecord( List[ index ]^ ).Name = Name then
    begin
      Result := index;
      Break;
    end;
  end;
end;

function TClientList.GetCount : Integer;
begin
  Result := List.Count;
end;

function TClientList.GetName( index : integer ) : string;
begin
  try
    Result := TClientRecord( List[ index ]^ ).Name;
  except
    Result := EmptyStr;
  end;
end;

function TClientList.GetSocket( index : integer ) : PTCPSocket;
begin
  try
    Result := @TClientRecord( List[ index ]^ ).Socket;
  except
    Result := nil;
  end;
end;

function TClientList.Remove( Name : string ) : Boolean;
var
  index : integer;
begin
  Result := False;
  index := Find( Name );
  if index <> -1 then
  begin
    Result := Remove( index );
  end;
end;

function TClientList.Remove( Index : integer ) : Boolean;
var
  Item : PClientRecord;
begin
  Item := List[ index ];
  List.Delete( index );
  List.Pack;
  FreeMem( Item );
  Result := True;
end;

end.

