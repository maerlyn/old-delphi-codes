program Font3D;

uses
  Windows, SysUtils, Messages, Text3D, OpenGL;

var
  h_RC: HGLRC;
  h_DC: HDC;
  h_Wnd:HWND;
  keys: array [0..255] of BOOL;

  active : boolean = true;
  FullScreen:boolean;
  Texture, Factor: Cardinal;
  X, Y: GLFloat;

function InitGL: boolean;
begin
  glClearColor(1, 1, 1, 1);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_ALPHA_TEST);
  glEnable(GL_BLEND);
  glEnable(GL_LINE_SMOOTH);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glLineWidth(0.5);
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  gluPerspective(45.0,1024/768,0.1,100.0);

  Init3DFont(h_DC);
  Init3DTextureFont(h_DC);

  Texture:=CreateTexture('Texture.jpg');

  factor:=GL_SPHERE_MAP;

  glMatrixMode(GL_MODELVIEW);

  Result:=True;
end;

procedure ReSizeGLScene(Width: GLsizei; Height: GLsizei);
var
  fWidth, fHeight: GLfloat;
begin
  if (Height=0)
    then Height:=1;

  glViewport(0, 0, Width, Height);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  fWidth := Width;
  fHeight := Height;
  gluPerspective(45.0,fWidth/fHeight,0.1,100.0);
  glMatrixMode(GL_MODELVIEW);
end;

function DrawGLScene: boolean;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();

   glEnable(GL_TEXTURE_2D);
   glBindTexture(GL_TEXTURE_2D, Texture);
   glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_BLEND);
   glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, factor);
   glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, factor);
   glEnable(GL_TEXTURE_GEN_S);
   glEnable(GL_TEXTURE_GEN_T);
    Draw3DTextureText('Program by:', 0.5, -5, -35, Y, X, 0);
    Draw3DTextureText('Tele von Zsinór', -0.5, -2, -5, Y, X, 0);
   glDisable(GL_TEXTURE_GEN_S);
   glDisable(GL_TEXTURE_GEN_T);
   glDisable(GL_TEXTURE_2D);

   factor := GL_SPHERE_MAP;

  If Keys[VK_LEFT] then X:=X+1;
  If Keys[VK_RIGHT] then X:=X-1;
  If Keys[VK_UP] then Y:=Y+1;
  If Keys[VK_DOWN] then Y:=Y-1;

  If X>360 then X:=0;
  If Y>360 then Y:=0;

  If X<0 then X:=360;
  If Y<0 then Y:=360;
  Result:=True;
end;

function WndProc(hWnd: HWND; message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if message=WM_SYSCOMMAND then
    begin
      case wParam of
        SC_SCREENSAVE,SC_MONITORPOWER:
          begin
            result:=0;
            exit;
          end;
      end;
    end;

  case message of
    WM_ACTIVATE:
      begin
        if (Hiword(wParam)=0) then
          active:=true
        else
          active:=false;
        Result:=0;
      end;
    WM_CLOSE:
      Begin
        PostQuitMessage(0);
        result:=0
      end;
    WM_KEYDOWN:
      begin
        keys[wParam] := TRUE;
        result:=0;
      end;
    WM_KEYUP:
      begin
    	keys[wParam] := FALSE;
        result:=0;
      end;
    WM_SIZe:
      begin
    	ReSizeGLScene(LOWORD(lParam),HIWORD(lParam));
        result:=0;
      end
    else
      begin
      	Result := DefWindowProc(hWnd, message, wParam, lParam);
      end;
    end;
end;

procedure KillGLWindow;
begin
  if FullScreen then
    begin
      ChangeDisplaySettings(devmode(nil^),0);
      showcursor(true);
    end;
  if h_rc<>0 then
    begin
      if (not wglMakeCurrent(h_Dc,0)) then
        MessageBox(0,'A DC és az RC felszabadítása meghiúsult..','Bezárási hiba',MB_OK or MB_ICONERROR);
      if (not wglDeleteContext(h_Rc)) then
        begin
          MessageBox(0,'Az RC felszabadítása meghiúsult.','Bezárási hiba',MB_OK or MB_ICONERROR);
          h_Rc:=0;
        end;
    end;
  if (h_Dc=1) and (releaseDC(h_Wnd,h_Dc)<>0) then
    begin
      MessageBox(0,'A DC felszabadítása meghiúsult.','Bezárási hiba',MB_OK or MB_ICONERROR);
      h_Dc:=0;
    end;
  if (h_Wnd<>0) and (not destroywindow(h_Wnd))then
    begin
      MessageBox(0,'Nem tudom felszabadítani a hWnd-t.','Bezárási hiba',MB_OK or MB_ICONERROR);
      h_Wnd:=0;
    end
end;

function CreateGlWindow(title : Pchar; width, height, bits : integer; FullScreenflag:boolean):boolean stdcall;
var
  Pixelformat: GLuint;
  wc:TWndclass;
  dwExStyle:dword;
  dwStyle:dword;
  pfd: pixelformatdescriptor;
  dmScreenSettings: Devmode;
  h_Instance:hinst;
begin
  h_instance:=getmodulehandle(nil);
  FullScreen:=FullScreenflag;
  with wc do
    begin
      style:=CS_HREDRAW or CS_VREDRAW or CS_OWNDC;
      lpfnWndProc:=@WndProc;
      cbClsExtra:=0;
      cbWndExtra:=0;
      hInstance:=h_Instance;
      hIcon:=LoadIcon(0,IDI_WINLOGO);
      hCursor:=LoadCursor(0,IDC_ARROW);
      hbrBackground:=0;
      lpszMenuName:=nil;
      lpszClassName:='OpenGl';
    end;
  if  RegisterClass(wc)=0 then
    begin
      MessageBox(0,'Nem tudom regisztráli az ablak osztályát.','Hiba',MB_OK or MB_ICONERROR);
      CreateGLwindow:=false;
      exit;
    end;
  if FullScreen then
    begin
      ZeroMemory( @dmScreenSettings, sizeof(dmScreenSettings) );
      with dmScreensettings do
        begin
          dmSize := sizeof(dmScreenSettings);
          dmPelsWidth  := width;
	  dmPelsHeight := height;
          dmBitsPerPel := bits;
          dmFields     := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        end;

      if (ChangeDisplaySettings(dmScreenSettings, CDS_FullScreen))<>DISP_CHANGE_SUCCESSFUL THEN
        Begin
          if MessageBox(0,'A teljesképernyõs mód nem támogatott. Akarod inkább ablakban futtatni?'
                                             ,'Névjegy - Nyomogasd a kurzorgombokat!',MB_YESNO or MB_ICONEXCLAMATION)= IDYES then
                FullScreen:=false
          else
            begin
              MessageBox(0,'A program nem fog bezárulni.','Hiba',MB_OK or MB_ICONERROR);
              Result := false;
            end;
          end;
    end;
  if FullScreen then
    begin
      dwExStyle:=WS_EX_APPWINDOW;
      dwStyle:=WS_popup or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
      Showcursor(false);
    end
  else
    begin
      dwExStyle:=WS_EX_APPWINDOW or WS_EX_WINDOWEDGE;
      dwStyle:=WS_OVERLAPPEDWINDOW or WS_CLIPSIBLINGS or WS_CLIPCHILDREN;
    end;

  H_wnd:=CreateWindowEx(dwExStyle, 'OpenGl', Title, dwStyle, 0, 0, width, height, 0, 0, HInstance, nil);

  if h_Wnd=0 then
  begin
    KillGlWindow();
    MessageBox(0,'Ablak-létrehozási hiba.','Hiba',MB_OK or MB_ICONEXCLAMATION);
    CreateGLWindow:=false;
    exit;
  end;

  with pfd do
    begin
      nSize:= SizeOf( PIXELFORMATDESCRIPTOR );
      nVersion:= 1;
      dwFlags:= PFD_DRAW_TO_WINDOW
        or PFD_SUPPORT_OPENGL
        or PFD_DOUBLEBUFFER;
      iPixelType:= PFD_TYPE_RGBA;
      cColorBits:= 32;
      cRedBits:= 0;
      cRedShift:= 0;
      cGreenBits:= 0;
      cBlueBits:= 0;
      cBlueShift:= 0;
      cAlphaBits:= 0;
      cAlphaShift:= 0;
      cAccumBits:= 0;
      cAccumRedBits:= 0;
      cAccumGreenBits:= 0;
      cAccumBlueBits:= 0;
      cAccumAlphaBits:= 0;
      cDepthBits:= 16;
      cStencilBits:= 0;
      cAuxBuffers:= 0;
      iLayerType:= PFD_MAIN_PLANE;
      bReserved:= 0;
      dwLayerMask:= 0;
      dwVisibleMask:= 0;
      dwDamageMask:= 0;
    end;
  h_Dc := GetDC(h_Wnd);
  if h_Dc=0 then
    begin
      KillGLWindow();
      MessageBox(0,'Nem tudom létrehozni a GL DC-t.','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  PixelFormat := ChoosePixelFormat(h_Dc, @pfd);
  if (PixelFormat=0) then
    begin
      KillGLWindow();
      MessageBox(0,'Nem találok alkalmazható pixelformátumot.','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  if (not SetPixelFormat(h_Dc,PixelFormat,@pfd)) then
    begin
      KillGLWindow();
      MessageBox(0,'Nem tudom beállítani a pixelformátumot.','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  h_Rc := wglCreateContext(h_Dc);
  if (h_Rc=0) then
    begin
      KillGLWindow();
      MessageBox(0,'Nem tudok GL RC-t létrehozni..','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  if (not wglMakeCurrent(h_Dc, h_Rc)) then
    begin
      KillGLWindow();
      MessageBox(0,'Nem tudom aktiválni a GL RC-t.','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  ShowWindow(h_Wnd,SW_SHOW);
  SetForegroundWindow(h_Wnd);
  SetFOcus(h_Wnd);
  if (not InitGl) then
    begin
      KillGLWindow();
      MessageBox(0,'Az inicializáció meghiúsult.','Hiba',MB_OK or MB_ICONEXCLAMATION);
      CreateGLWindow:=false;
      exit;
    end;
  ReSizeGLScene(width,height);

  CreateGLWindow:=true;
  Y := 0;
  X := 10;
end;

function WinMain(hInstance: HINST; hPrevInstance: HINST; pCmdLine: PChar; nCmdShow: integer): integer; stdcall;
var
  msg: TMsg;
  done: boolean;
begin
  done := false;

//  FullScreen := (MessageBox(0,'Teljesképernyõs legyen?','Kérdés', MB_YESNO or MB_ICONQUESTION) = IDYES);
  if CreateGLWindow('Névjegy',583,324,16,false) = false then //FullScreen) then
  begin
    Result := 0;
    exit;
  end;

  while not done do
    begin
      if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then
        begin
          if msg.message=WM_QUIT then
            done:=true
          else
            begin
	      TranslateMessage(msg);
	      DispatchMessage(msg);
	    end;
        end
      else
        begin
          if (active and not(DrawGLScene()) or keys[VK_ESCAPE]) then
            done:=true
          else
            begin
              SwapBuffers(h_Dc);
              if (keys[VK_ESCAPE]) then SendMessage(h_Wnd,WM_CLOSE,0,0);
            end;
        end;
    end;

  killGLwindow();
  result:=msg.wParam;
end;

begin
  InitOpenGL;
  WinMain( hInstance, hPrevInst, CmdLine, CmdShow );
end.


