program convmdl;

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  XName, MdlName: String;
  XFile, MdlFile: File;
  Str: String;
  Int: Integer;
  TexName: array [0..127] of Char;
  NumMeshs, NumFrames, NumVerts, NumFaces: Integer;
  Verts: array of Single;
  Vertices: array of array of Single;
  u: Single;
  v: Integer;

procedure LoadUniMesh;
var
  i, j: Integer;
begin
  BlockRead(XFile, TexName[0], 5);
  BlockRead(XFile, NumMeshs, SizeOf(Integer));
  BlockWrite(MdlFile, NumMeshs, SizeOf(Integer));
  WriteLn('Meshs: '+IntToStr(NumMeshs));
  for i := 0 to NumMeshs-1 do
  begin
    Seek(XFile, 5+4+i*128);
    BlockRead(XFile, TexName, 128);
    Str := ExtractFileName(String(TexName));
    Str := Copy(Str, 1, Pos('.', Str)) + 'tex';
    WriteLn('Texture '+IntToStr(i+1)+': '+Str);
    Str := Str + #0;
    BlockWrite(MdlFile, Str[1], Length(Str));
  end;
  Seek(XFile, 4+5+NumMeshs*128);
  BlockRead(XFile, NumFrames, SizeOf(Integer));
  SetLength(Vertices, NumFrames);
  for i := 0 to NumFrames-1 do
  begin
    BlockRead(XFile, TexName[0], 16);
    BlockRead(XFile, NumVerts, SizeOf(Integer));
    SetLength(Verts, NumVerts*3);
    BlockRead(XFile, NumFaces, SizeOf(Integer));
    SetLength(Vertices[i], NumFaces*3*8);
    BlockRead(XFile, Verts[0], SizeOf(Single)*NumVerts*3);
    for j := 0 to NumFaces-1 do
    begin
      BlockRead(XFile, Int, SizeOf(Integer));
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[i, j*3*8+5] := Verts[v*3+0];
        Vertices[i, j*3*8+6] := Verts[v*3+1];
        Vertices[i, j*3*8+7] := Verts[v*3+2];
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[i, (j*3+1)*8+5] := Verts[v*3+0];
        Vertices[i, (j*3+1)*8+6] := Verts[v*3+1];
        Vertices[i, (j*3+1)*8+7] := Verts[v*3+2];
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[i, (j*3+2)*8+5] := Verts[v*3+0];
        Vertices[i, (j*3+2)*8+6] := Verts[v*3+1];
        Vertices[i, (j*3+2)*8+7] := Verts[v*3+2];
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, j*3*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, (j*3+1)*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, (j*3+2)*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, j*3*8+1] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, (j*3+1)*8+1] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[i, (j*3+2)*8+1] := u;
      BlockRead(XFile, Int, SizeOf(Integer));
    end;
  end;
  Int := 0;
  BlockWrite(MdlFile, Int, SizeOf(Integer));
  Int := NumFaces*3;
  BlockWrite(MdlFile, Int, SizeOf(Integer));
  BlockWrite(MdlFile, NumFrames, SizeOf(Integer));
  WriteLn('Frames: '+IntToStr(NumFrames));
  Int := NumFaces*3*8;
  BlockWrite(MdlFile, Int, SizeOf(Integer));
  WriteLn('Faces: '+IntToStr(NumFaces));
  for i := 0 to NumFrames-1 do
  begin
    BlockWrite(MdlFile, Vertices[i, 0], Length(Vertices[i]) * SizeOf(Single));
    Vertices[i] := nil;
  end;
  Vertices := nil;
  Verts := nil;
end;

procedure LoadMultiMesh;
var
  i, j: Integer;
  Tex: array [0..255] of String;
  Start: array [0..255] of Integer;
  Count: array [0..255] of Integer;
begin
  BlockRead(XFile, TexName[0], 5);
  BlockRead(XFile, NumMeshs, SizeOf(Integer));
  for i := 0 to NumMeshs-1 do
  begin
    Seek(XFile, 5+4+i*128);
    BlockRead(XFile, TexName, 128);
    Str := ExtractFileName(String(TexName));
    Str := Copy(Str, 1, Pos('.', Str)) + 'tex';
    WriteLn('Texture '+IntToStr(i+1)+': '+Str);
    Tex[i] := Str + #0;
  end;
  Seek(XFile, 4+5+NumMeshs*128);
  NumFrames := 1;
  BlockRead(XFile, NumMeshs, SizeOf(Integer));
  WriteLn('Meshs: '+IntToStr(NumMeshs));
  SetLength(Vertices, NumFrames);
  for i := 0 to NumMeshs-1 do
  begin
    if i = 0 then
      Start[i] := 0
    else
      Start[i] := Start[i-1] + Count[i-1];
    BlockRead(XFile, TexName[0], 16);
    BlockRead(XFile, NumVerts, SizeOf(Integer));
    SetLength(Verts, NumVerts*3);
    BlockRead(XFile, NumFaces, SizeOf(Integer));
    SetLength(Vertices[0], Length(Vertices[0])+NumFaces*3*8);
    BlockRead(XFile, Verts[0], SizeOf(Single)*NumVerts*3);
    for j := 0 to NumFaces-1 do
    begin
      BlockRead(XFile, Int, SizeOf(Integer));
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[0, Start[i]*8+j*3*8+5] := Verts[v*3+0];
        Vertices[0, Start[i]*8+j*3*8+6] := Verts[v*3+1];
        Vertices[0, Start[i]*8+j*3*8+7] := Verts[v*3+2];
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[0, Start[i]*8+(j*3+1)*8+5] := Verts[v*3+0];
        Vertices[0, Start[i]*8+(j*3+1)*8+6] := Verts[v*3+1];
        Vertices[0, Start[i]*8+(j*3+1)*8+7] := Verts[v*3+2];
      BlockRead(XFile, v, SizeOf(Integer));
        Vertices[0, Start[i]*8+(j*3+2)*8+5] := Verts[v*3+0];
        Vertices[0, Start[i]*8+(j*3+2)*8+6] := Verts[v*3+1];
        Vertices[0, Start[i]*8+(j*3+2)*8+7] := Verts[v*3+2];
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+j*3*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+(j*3+1)*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+(j*3+2)*8] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+j*3*8+1] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+(j*3+1)*8+1] := u;
      BlockRead(XFile, u, SizeOf(Single));
        Vertices[0, Start[i]*8+(j*3+2)*8+1] := u;
      BlockRead(XFile, Int, SizeOf(Integer));
    end;
    Count[i] := NumFaces*3;
  end;
  BlockWrite(MdlFile, NumMeshs, SizeOf(Integer));
  for i := 0 to NumMeshs-1 do
  begin
    BlockWrite(MdlFile, Tex[i][1], Length(Tex[i]));
    BlockWrite(MdlFile, Start[i], SizeOf(Integer));
    BlockWrite(MdlFile, Count[i], SizeOf(Integer));
  end;
  BlockWrite(MdlFile, NumFrames, SizeOf(Integer));
  WriteLn('Frames: '+IntToStr(NumFrames));
  Int := (Start[NumMeshs-1]+Count[NumMeshs-1])*8;
  BlockWrite(MdlFile, Int, SizeOf(Integer));
  WriteLn('Faces: '+IntToStr(Int div 24));
  BlockWrite(MdlFile, Vertices[0, 0], Length(Vertices[0]) * SizeOf(Single));
  Vertices[0] := nil;
  Vertices := nil;
  Verts := nil;
end;

begin
  if ParamStr(1) = '' then
    WriteLn('Usage: convmdl.exe file.x Vis&Col-coeff [-m for 3DS->X]')
  else
  begin
    XName := ParamStr(1);
    if FileExists(XName) then
    begin
      MdlName := Copy(XName, 1, Pos('.', XName)) + 'tnt';
      AssignFile(XFile, XName);
      Reset(XFile, 1);
      AssignFile(MdlFile, MdlName);
      Rewrite(MdlFile, 1);
      Str := 'TNT-MDL'#0;
      BlockWrite(MdlFile, Str[1], Length(Str));
      if FindCmdLineSwitch('m', ['-'], False) then
        LoadMultiMesh
      else
        LoadUniMesh;
      CloseFile(XFile);
      u := StrToFloat(ParamStr(2));
      BlockWrite(MdlFile, u, SizeOf(Single));
      u := StrToFloat(ParamStr(3));
      BlockWrite(MdlFile, u, SizeOf(Single));
      CloseFile(MdlFile);
    end
    else
      WriteLn(XName + ' not found!');
  end;
end.

