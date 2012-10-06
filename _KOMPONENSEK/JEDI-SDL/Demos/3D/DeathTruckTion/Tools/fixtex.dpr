program fixtex;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  TexName: String;
  TexFile: File;
  W, H, B, Size: Integer;
  Data: PChar;

begin
  TexName := ParamStr(1);
  if TexName='' then
    WriteLn('Usage: fixtex.exe file.tex')
  else
  begin
    if FileExists(TexName) then
    begin
      AssignFile(TexFile, TexName);
      Reset(TexFile, 1);
      BlockRead(TexFile, W, SizeOf(Integer));
      BlockRead(TexFile, H, SizeOf(Integer));
      BlockRead(TexFile, B, SizeOf(Integer));
      Size := W*H*B;
      GetMem(Data, Size);
      BlockRead(TexFile, Data^, Size);
      Rewrite(TexFile, 1);
      BlockWrite(TexFile, W, SizeOf(Integer));
      BlockWrite(TexFile, H, SizeOf(Integer));
      BlockWrite(TexFile, B, SizeOf(Integer));
      B := 1;
      BlockWrite(TexFile, B, SizeOf(Integer));
      BlockWrite(TexFile, B, SizeOf(Integer));
      BlockWrite(TexFile, Data^, Size);
      CloseFile(TexFile);
      FreeMem(Data);
      WriteLn(TexName + ' fixed');
    end
    else
      WriteLn(TexName + ' not found');
  end;
end.

