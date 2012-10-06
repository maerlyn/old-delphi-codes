program POPper;

uses
  Forms,
  Windows,
  Unit1 in 'Unit1.pas' {frmMainForm},
  Unit2 in 'Unit2.pas' {frmCimlista},
  About in 'About.pas' {frmAbout},
  Unit3 in 'Unit3.pas' {frmSplash};

{$R *.RES}

var hMutex: THandle;
    FoundWnd: THandle;
    ModuleName: string;
    SplashAbout: TfrmSplash;

const WM_USER = $0400;

function EnumWndProc(hwnd: THandle; Param: cardinal):boolean;stdcall;
var ClassName, WinModuleName: string;
    WinInstance: integer;
begin
 Result := true;
 SetLength(ClassName,100);
 GetClassName(hwnd,PChar(ClassName),length(ClassName));
 ClassName := PChar(ClassName);
 if ClassName = frmMainForm.ClassName then
 begin
  SetLength(WinModuleName,200);
  WinInstance := GetWindowLong(hwnd,GWL_HINSTANCE);
  GetModuleFileName(WinInstance,PCHar(WinModuleName),length(WinModuleName));
  WinModuleName := PChar(WinModuleName);
  if WinModuleName = ModuleName then
  begin
   FoundWnd := Hwnd;
   Result := false;
  end;
 end;
end;

begin
  hMutex := CreateMutex(nil,false,'PutraPOPPerMutex');
  if WaitForSingleObject(hMutex,0) <> wait_TimeOut then
  begin
    Application.Initialize;
    SplashAbout := TfrmSplash.Create(Application);
    SplashAbout.Show;
    SplashAbout.Update;
    Application.Title := 'Putra POPper';
    Application.CreateForm(TfrmMainForm, frmMainForm);
    Application.CreateForm(TfrmAbout, frmAbout);
    frmAbout.Hide;
    SplashAbout.Timer1.Enabled := true;
    Application.Run;
  end
  else
  begin
    SetLength(ModuleName,200);
    GetModuleFileName(hInstance,PChar(ModuleName),length(ModuleName));
    ModuleName := PCHar(ModuleName);
    EnumWindows(@EnumWndProc,0);
    if FoundWnd <> 0 then
    begin
     if not IsWindowVisible(FoundWnd) then
      PostMessage(FoundWnd,wm_User,0,0);
     SetForegroundWindow(FoundWnd);
    end;
  end;
end.
