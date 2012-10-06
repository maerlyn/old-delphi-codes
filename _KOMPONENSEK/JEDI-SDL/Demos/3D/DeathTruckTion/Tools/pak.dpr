program pak;

{$APPTYPE CONSOLE}

uses
  SysUtils;

const
  Header = 'TNT-PAK'#0;

procedure Error(str: String);
begin
  WriteLn('FATAL ERROR: ' + str);
  Halt;
end;

var
  ListName, PakName, FileName: String;
  List: Text;
  PakFile, InFile: File;
  Size: Integer;
  Data: PChar;

begin
  ListName := ParamStr(1);

  if not FileExists(ListName) then
    Error(ListName + ' not found');

  PakName := ListName;
  PakName[Length(PakName)-2] := 'p';
  PakName[Length(PakName)-1] := 'a';
  PakName[Length(PakName)] := 'k';

  WriteLn(ListName + ' -> ' + PakName);

  AssignFile(List, ListName);
  Reset(List);

  AssignFile(PakFile, PakName);
  Rewrite(PakFile, 1);
  BlockWrite(PakFile, Header, 8);

  while not EOF(List) do
  begin
    ReadLn(List, FileName);
    WriteLn('  - ' + FileName);
    AssignFile(InFile, FileName);
    Reset(InFile, 1);
    FileName := FileName + #0;
    Size := FileSize(InFile);
    GetMem(Data, Size);
    BlockRead(InFile, Data^, Size);
    CloseFile(InFile);
    BlockWrite(PakFile, FileName[1], Length(FileName));
    BlockWrite(PakFile, Size, SizeOf(Integer));
    BlockWrite(PakFile, Data^, Size);
    FreeMem(Data);
  end;
  Size := 0;
  BlockWrite(PakFile, Size, 1);
  CloseFile(List);
  CloseFile(PakFile);
end.

