program convtex;        // Convertisseur de textures .TGA -> .TEX

{$APPTYPE CONSOLE}

uses
  SysUtils;

var
  i,j,n: Integer;
  PicName, TexName, Ext: String;
  PicFile, TexFile: File;
  Width, Height, Bpp, Size, Speed: Integer;
  Data: PChar;
  Ok, updown: Boolean;

function LoadTGA: Boolean;
const
  TGAheader: array [0..11] of Byte = (0,0,2,0,0,0,0,0,0,0,0,0);
  TGA8header: array [0..11] of Byte = (0,0,3,0,0,0,0,0,0,0,0,0);
var
  TGAcompare: array [0..11] of Byte;
  Header: array [0..5] of Byte;
  BytesPerPixel, i, j: Integer;
  t: Char;
begin
  Result := False;
  BlockRead(PicFile, TGAcompare, SizeOf(TGAcompare));
  if (TGAcompare[2] = 2) or (TGAcompare[2] = 3) then
  begin
    if ((TGAcompare[2] = 2) and not CompareMem(@TGAheader, @TGAcompare, SizeOf(TGAcompare))) or
       ((TGAcompare[2] = 3) and not CompareMem(@TGA8header, @TGAcompare, SizeOf(TGAcompare))) then
      WriteLn(#13#10 + PicName + ': invalid format')
    else
    begin
      BlockRead(PicFile, Header[0], SizeOf(Header));
      Width := Header[1]*256 + Header[0];
      Height := Header[3]*256 + Header[2];
      Bpp := Header[4];
      if (Bpp <> 8) and (Bpp <> 24) and (Bpp <> 32) then
        WriteLn(#13#10 + PicName + ': invalid BPP')
      else
      begin
        BytesPerPixel := Bpp div 8;
        Size := Width * Height * BytesPerPixel;
        GetMem(Data, Size);
        BlockRead(PicFile, Data^, Size);
        i := 0;
        while i < Size do
        begin
          t := Data[i];
          Data[i] := Data[i+2];
          Data[i+2] := t;
          Inc(i, BytesPerPixel);
        end;
        if updown then
        for j := 0 to (Height div 2)-1 do
          for i := 0 to Width*BytesPerPixel-1 do
          begin
            t := Data[j*Width*BytesPerPixel+i];
            Data[j*Width*BytesPerPixel+i] := Data[(Height-j-1)*Width*BytesPerPixel+i];
            Data[(Height-j-1)*Width*BytesPerPixel+i] := t;
          end;
        Result := True;
      end;
    end;
  end
  else
    WriteLn(#13#10 + PicName + ': invalid format');
end;


begin
  if ParamStr(1) = '' then
  begin
    WriteLn('Usage: convtex.exe texture1.tga texture2.tga -XX animtex ...');
    Exit;
  end;

  updown := True;
  if FindCmdLineSwitch('u', ['-'], False) then
    updown := False;

  i := 1;
  while ParamStr(i) <> '' do
  begin
    if ParamStr(i)[1] = '-' then
    begin
      if ParamStr(i)[2] = 'u' then
        Exit;
      n := StrToInt(Copy(ParamStr(i),2,2))+1;
      Inc(i);
      Speed := StrToInt(ParamStr(i));
      Inc(i);
      TexName := ParamStr(i) + '.tex';
      AssignFile(TexFile, TexName);
      Rewrite(TexFile, 1);

      PicName := ParamStr(i) + '00.tga';
      Write(PicName + ' -> ' + TexName);
      AssignFile(PicFile, PicName);
      Reset(PicFile, 1);
      Ok := LoadTGA;
      CloseFile(PicFile);
      if Ok then
      begin
        WriteLn(' (' + IntToStr(Width) + 'x' + IntToStr(Height) + 'x' + IntToStr(Bpp) + ')');
        BlockWrite(TexFile, Width, SizeOf(Width));
        BlockWrite(TexFile, Height, SizeOf(Height));
        Bpp := Bpp div 8;
        BlockWrite(TexFile, Bpp, SizeOf(Bpp));
        BlockWrite(TexFile, n, SizeOf(Bpp));
        BlockWrite(TexFile, Speed, SizeOf(Bpp));
        BlockWrite(TexFile, Data^, Size);
        FreeMem(Data);
      end;

      for j:=1 to n-1 do
      begin
        if j<10 then
          PicName := ParamStr(i) + '0' + IntToStr(j) + '.tga'
        else
          PicName := ParamStr(i) + IntToStr(j) + '.tga';
        WriteLn(PicName + ' -> ' + TexName);
        AssignFile(PicFile, PicName);
        Reset(PicFile, 1);
        Ok := LoadTGA;
        CloseFile(PicFile);
        if Ok then
        begin
          BlockWrite(TexFile, Data^, Size);
          FreeMem(Data);
        end;
      end;
      CloseFile(TexFile);
    end
    else
    begin
      PicName := ParamStr(i);
      if FileExists(PicName) then
      begin
        Ext := Copy(PicName, Pos('.', PicName)+1, 255);
        if Ext = 'tga' then
        begin
          TexName := Copy(PicName, 1, Pos('.', PicName)) + 'tex';
          Write(PicName + ' -> ' + TexName);
          AssignFile(PicFile, PicName);
          Reset(PicFile, 1);
          Ok := LoadTGA;
          CloseFile(PicFile);
          if Ok then
          begin
            Bpp := Bpp div 8;
            WriteLn(' (' + IntToStr(Width) + 'x' + IntToStr(Height) + 'x' + IntToStr(Bpp*8) + ')');
            AssignFile(TexFile, TexName);
            Rewrite(TexFile, 1);
            BlockWrite(TexFile, Width, SizeOf(Width));
            BlockWrite(TexFile, Height, SizeOf(Height));
            BlockWrite(TexFile, Bpp, SizeOf(Bpp));
            Bpp := 1;
            BlockWrite(TexFile, Bpp, SizeOf(Bpp));
            BlockWrite(TexFile, Bpp, SizeOf(Bpp));
            BlockWrite(TexFile, Data^, Size);
            CloseFile(TexFile);
            FreeMem(Data);
          end;
        end
        else
          WriteLn(PicName + ': Unknown extension');
      end
      else
        WriteLn(PicName + ' not found');
    end;
    Inc(i);
  end;
end.
