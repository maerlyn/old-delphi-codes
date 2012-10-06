unit Text3D;

interface

uses Windows, SysUtils, OpenGL, JPEG, Graphics;

type
     PPixelArray = ^TPixelArray;
     TPixelArray = array [0..0] of Byte;

     procedure Init3DFont(dc: HDC);
     procedure Init3DTextureFont(dc: HDC);
     procedure Draw3DText(Text: String; X, Y, Z, AX, AY, AZ: GLFloat);
     procedure Draw3DTextureText(Text: String; X, Y, Z, AX, AY, AZ: GLFloat);

     function CreateTexture(Texture : String): cardinal;

const
  GLF_START_LIST = 1000;

var
  lf : TLOGFONT;
  hFontNew, hOldFont : HFONT;
  agmf : Array [0..255] of TGLYPHMETRICSFLOAT ;
  lpMsgBuf: PChar;

implementation

procedure Init3DFont(dc: HDC);
begin
  FillChar(lf, SizeOf(lf), 0);
  lf.lfHeight               :=   -38 ;
  lf.lfWeight               :=   FW_NORMAL ;
  lf.lfCharSet              :=   ANSI_CHARSET ;
  lf.lfOutPrecision         :=   OUT_DEFAULT_PRECIS ;
  lf.lfClipPrecision        :=   CLIP_DEFAULT_PRECIS ;
  lf.lfQuality              :=   DEFAULT_QUALITY ;
  lf.lfPitchAndFamily       :=   FF_DONTCARE OR DEFAULT_PITCH;
  lstrcpy (lf.lfFaceName, 'Times New Roman') ;

  hFontNew := CreateFontIndirect(lf);
  hOldFont := SelectObject(DC, hFontNew);

  wglUseFontOutlines(DC, 0, 255, GLF_START_LIST, 0.0, 0.15,
                     WGL_FONT_LINES, @agmf);

  DeleteObject(SelectObject(DC,hOldFont));
  DeleteObject(SelectObject(DC,hFontNew));
end;

procedure Init3DTextureFont(dc: HDC);
begin
  FillChar(lf, SizeOf(lf), 0);
  lf.lfHeight               :=   -38 ;
  lf.lfWeight               :=   FW_NORMAL ;
  lf.lfCharSet              :=   ANSI_CHARSET ;
  lf.lfOutPrecision         :=   OUT_DEFAULT_PRECIS ;
  lf.lfClipPrecision        :=   CLIP_DEFAULT_PRECIS ;
  lf.lfQuality              :=   DEFAULT_QUALITY ;
  lf.lfPitchAndFamily       :=   FF_DONTCARE OR DEFAULT_PITCH;
  lstrcpy (lf.lfFaceName, 'Times New Roman') ;

  hFontNew := CreateFontIndirect(lf);
  hOldFont := SelectObject(DC, hFontNew);

  wglUseFontOutlines(DC, 0, 255, GLF_START_LIST+255, 0.0, 0.15,
                     WGL_FONT_POLYGONS, @agmf);

  DeleteObject(SelectObject(DC,hOldFont));
  DeleteObject(SelectObject(DC,hFontNew));
end;

procedure Draw3DTextureText(Text: String; X, Y, Z, AX, AY, AZ: GLFloat);
begin
 glPushMatrix;
  glTranslatef(Y, X, Z);
 glPushMatrix;
  glRotatef(AX, 1, 0, 0);
  glRotatef(AY, 0, 1, 0);
  glRotatef(AZ, 0, 0, 1);
 glPushMatrix;
  glTranslatef(-0.46, -0.31, 0);
  glListBase(GLF_START_LIST+255);
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, pChar(Text));
 glPopMatrix;
 glPopMatrix;
 glPopMatrix;
end;

procedure Draw3DText(Text: String; X, Y, Z, AX, AY, AZ: GLFloat);
begin
 glPushMatrix;
  glTranslatef(Y, X, Z);
 glPushMatrix;
  glRotatef(AX, 1, 0, 0);
  glRotatef(AY, 0, 1, 0);
  glRotatef(AZ, 0, 0, 1);
 glPushMatrix;
  glTranslatef(-0.46, -0.31, 0);
  glListBase(GLF_START_LIST);
  glCallLists(Length(Text), GL_UNSIGNED_BYTE, pChar(Text));
 glPopMatrix;
 glPopMatrix;
 glPopMatrix;
end;

function CreateTexture(Texture : String): cardinal;
var
  bitmap: TBitmap;
  BMInfo : TBitmapInfo;
  I, ImageSize : Integer;
  Temp : Byte;
  MemDC : HDC;
  Picture: TJpegImage;
  Tex: PPixelArray;
begin
  glenable(GL_TEXTURE_2D);
  glGenTextures(1, @Result);
  glBindTexture(GL_TEXTURE_2D, Result);

  Bitmap:=TBitMap.Create;
  Picture:=TJpegImage.Create;

  Picture.LoadFromFile(Texture);
  Bitmap.Assign(Picture);
  with BMinfo.bmiHeader do begin
    FillChar (BMInfo, SizeOf(BMInfo), 0);
    biSize := sizeof (TBitmapInfoHeader);
    biBitCount := 24;
    biWidth := Picture.Width;
    biHeight := Picture.Height;
    ImageSize := biWidth * biHeight;
    biPlanes := 1;
    biCompression := BI_RGB;

    MemDC := CreateCompatibleDC (0);
    GetMem (Tex, ImageSize *3);
    try
      GetDIBits (MemDC, Bitmap.Handle, 0, biHeight, Tex, BMInfo, DIB_RGB_COLORS);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);
      glTexImage2d(GL_TEXTURE_2D, 0, 3, biwidth, biheight, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, tex);
      For I := 0 to ImageSize - 1 do begin
          Temp := tex [I * 3];
          tex [I * 3] := tex [I * 3 + 2];
          tex [I * 3 + 2] := Temp;
      end;
     finally
      DeleteDC (MemDC);
      Bitmap.Free;
      Picture.Free;
      FreeMem(tex);
   end;
  end;
end;

end.
