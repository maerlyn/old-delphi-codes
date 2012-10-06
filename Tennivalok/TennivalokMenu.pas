unit TennivalokMenu;

interface

uses
  Windows, ActiveX, ComObj, ShlObj, ShellApi, ComServ, Messages, SysUtils, Registry;

type
  TTennivalokMenu = class(TComObject, IUnknown, IContextMenu, IShellExtInit)
  private
    fFileName: string;
  protected
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HRESULT;stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HRESULT;stdcall;
    function GetCommandString(idCmd, uType: UINT;pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;stdcall;
    function IShellExtInit.Initialize = InitShellExt;
    function InitShellExt(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult;stdcall;
  end;

  TTennivalokMenuFactory = class (TComObjectFactory)
  public
    procedure UpdateRegistry (Register: Boolean); override;
  end;

const
  Class_TennivalokMenuMenu: TGUID =
    '{6013C006-251B-474E-BDC9-918799B8C1BA}';

implementation

{ TToDoMenu }

function TTennivalokMenu.GetCommandString(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
begin
 if idCmd = 0 then
 begin
  strCopy(pszName,'A file Tennivalók adatbázishoz való hozzáadása');
  Result := NOERROR;
 end
 else
  Result := E_INVALIDARG;
end;

function TTennivalokMenu.InitShellExt(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var medium : TStgMedium;
    fe: TFormatEtc;
begin
 Result := E_FAIL;
 if Assigned(lpdobj) then
 begin
  with fe do
  begin
   cfFormat := CF_HDROP;
   ptd := nil;
   dwAspect := DVASPECT_CONTENT;
   tymed := TYMED_HGLOBAL;
  end;
  Result := lpdobj.GetData(fe,medium);
  if not Failed(Result) then
  begin
   if DragQueryFile(medium.hGlobal,$FFFFFFFF,nil,0) = 1 then
   begin
    SetLength(fFileName,1000);
    DragQueryFile(medium.hGlobal,0,PChar(fFileName),1000);
    fFileName := PChar(fFileName);
    Result := NOERROR;
   end
   else
    Result := E_FAIL;
  end;
  ReleaseStgMedium(medium);
 end;
end;

function TTennivalokMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HRESULT;
var hwnd: THandle;
    cds: CopyDataStruct;
begin
 Result := NOERROR;
 if HiWord(Integer(lpici.lpVerb)) <> 0 then
 begin
  Result := E_FAIL;
  Exit;
 end;
 if LoWord(lpici.lpVerb) > 0 then
 begin
  Result := E_INVALIDARG;
  Exit;
 end;
 if LoWord(lpici.lpVerb) = 0 then
 begin
  hwnd := FindWindow('TTennivalokForm',nil);
  if hwnd <> 0 then
  begin
   cds.dwData := 0;
   cds.cbData := Length(fFileName);
   cds.lpData := PChar(fFileName);
   SetForegroundWindow(hwnd);
   SendMessage(hwnd,wm_CopyData,lpici.hWnd,Integer(@cds));
  end;
 end;
end;

function TTennivalokMenu.QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HRESULT;
begin
 if FindWindow('TTennivalokMenu',nil) <>0 then
 begin
  InsertMenu(Menu,indexMenu,MF_STRING + MF_BYPOSITION,idCmdFirst,'Tennivalók filehoz adás');
  Result := 1;
 end
 else
  Result := 0;
end;

{ TTennivalokMenuFactory }

procedure TTennivalokMenuFactory.UpdateRegistry(Register: Boolean);
var Reg: TRegistry;
begin
 inherited UpdateRegistry(Register);
 Reg := TRegistry.Create;
 try
  if Register then
   Reg.CreateKey('\HKEY_CLASSES_ROOT\*\ShellEx\ContextMenuHandler\' + GUIDToString(Class_TennivalokMenuMenu))
  else
   Reg.DeleteKey('HKEY_CLASSES_ROOT\*\ShellEx\ContextMenuHandler\' + GUIDToString(Class_TennivalokMenuMenu));
 finally
  Reg.Free;
 end;
end;

initialization
 TTennivalokMenuFactory.Create(
  ComServer, TTennivalokMenu, Class_TennivalokMenuMenu,
  'TennivalokMenu','Tennivalók menü shell kiterjesztése',
  ciMultiInstance,tmApartment);

end.
