unit TNT_File;

// Gestion des fichiers
// --------------------
//  TFile: fichier
//  Read, ReadByte, ReadInt, ReadChar, ReadFloat, ReadStr

interface

type
  TFile = class
  private
    Data: File;
  public
    constructor Open(FileName: String); overload;
    procedure Read(var Buf; Size: Integer);
    function ReadByte: Byte;
    function ReadInt: Integer;
    function ReadChar: Char;
    function ReadFloat: Single;
    function ReadStr: String;
    function Eof: Boolean;
    destructor Close;
  end;

implementation

uses
  TNT_3D, SysUtils;

constructor TFile.Open(FileName: String);
begin
  inherited Create;
  FileName := LowerCase(FileName);
  AssignFile(Data, 'data/data.pak');
  Reset(Data, 1);
  if ReadStr <> 'TNT-PAK' then
    Console.Log('ERROR reading ' + FileName + ': invalid TNT-PAK!');
  while ReadStr <> FileName do
    Seek(Data, ReadInt+FilePos(Data));
  ReadInt;
end;

procedure TFile.Read(var Buf; Size: Integer);
begin
  BlockRead(Data, Buf, Size);
end;

function TFile.ReadByte: Byte;
begin
  BlockRead(Data, Result, SizeOf(Byte));
end;

function TFile.ReadInt: Integer;
begin
  BlockRead(Data, Result, SizeOf(Integer));
end;

function TFile.ReadChar: Char;
begin
  BlockRead(Data, Result, SizeOf(Char));
end;

function TFile.ReadFloat: Single;
begin
  BlockRead(Data, Result, SizeOf(Single));
end;

function TFile.ReadStr: String;
var
  C: Char;
begin
  Result := '';
  repeat
    C := ReadChar;
    if C <> #0 then
      Result := Result + C;
  until C = #0;
end;

function TFile.Eof: Boolean;
begin
  Result := System.Eof(Data);
end;

destructor TFile.Close;
begin
  CloseFile(Data);
  inherited Destroy;
end;

end.

