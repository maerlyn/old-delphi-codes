unit Overbyte.Ics.Component platform;

interface

uses
    Borland.Vcl.Windows,
    Borland.Vcl.Messages,
    Borland.Vcl.WinUtils,
    Borland.Vcl.Classes
{$IFNDEF NOFORMS}
    , Borland.Vcl.Forms
{$ENDIF}
    ;

type
    TIcsBgExceptionEvent    = procedure (Sender       : TObject;
                                         E            : Exception;
                                         var CanClose : Boolean) of object;
    EIcsException = class(Exception);
{$IFDEF ICS_COMPONENT}
    TIcsComponent = class(TComponent)
{$ELSE}
    TIcsComponent = class(TObject)
{$ENDIF}
    protected
{$IFNDEF ICS_COMPONENT}
        FName                 : String;
{$ENDIF}
        FWindowHandle         : HWND;
        FThreadId             : DWORD;
        FTerminated           : Boolean;
        FMultiThreaded        : Boolean;
        FOnBgException        : TIcsBgExceptionEvent;
        FOnMessagePump        : TNotifyEvent;
        procedure   WndProc(var MsgRec: TMessage); virtual;
        procedure   HandleBackGroundException(E: Exception); virtual;
        procedure   AllocateHWnd; virtual;
        procedure   DeallocateHWnd; virtual;
{$IFNDEF ICS_COMPONENT}
        procedure   Notification(AComponent: TIcsComponent; Operation: TOperation); virtual;
{$ELSE}
        procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
        procedure   AbortComponent; virtual; //abstract;
        property MultiThreaded   : Boolean              read  FMultiThreaded
                                                        write FMultiThreaded;
    public
        constructor Create(AOwner: {$IFDEF ICS_COMPONENT}TComponent); override;
                                   {$ELSE}TObject); virtual;{$ENDIF}
        destructor  Destroy; override;
        procedure   ThreadAttach; virtual;
        procedure   ThreadDetach; virtual;
        procedure   MessageLoop; virtual;
        function    ProcessMessage : Boolean; virtual;
        procedure   ProcessMessages; virtual;
        procedure   MessagePump; virtual;
{$IFNDEF ICS_COMPONENT}
        property    Name               : String         read  FName
                                                        write FName;
{$ELSE}
        //property    Name;
{$ENDIF}
        property Handle          : HWND                   read  FWindowHandle;
        property OnBgException   : TIcsBgExceptionEvent   read  FOnBgException
                                                          write FOnBgException;
    end;



implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsComponent.Create(
    AOwner: {$IFDEF ICS_COMPONENT}TComponent
            {$ELSE}TObject{$ENDIF});
begin
    inherited Create{$IFDEF ICS_COMPONENT}(AOwner){$ENDIF};
    Self.AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsComponent.Destroy;
begin
    Self.DeallocateHWnd;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.Notification(
    AComponent : {$IFDEF ICS_COMPONENT}TComponent{$ELSE}TIcsComponent{$ENDIF};
    Operation  : TOperation);
begin
{$IFDEF ICS_COMPONENT}
    inherited;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.WndProc(var MsgRec: TMessage);
begin
    try
        MsgRec.Result := DefWindowProc(FWindowHandle, MsgRec.Msg,
                                       MsgRec.wParam, MsgRec.lParam);
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ All exceptions *MUST* be handled. If an exception is not handled, the       }
{ application will be shut down !                                             }
procedure TIcsComponent.HandleBackGroundException(E: Exception);
var
    CanAbort : Boolean;
begin
    CanAbort := TRUE;
    { First call the error event handler, if any }
    if Assigned(FOnBgException) then begin
        try
            FOnBgException(Self, E, CanAbort);
        except
        end;
    end;
    { Then abort the component }
    if CanAbort then begin
        try
            Self.AbortComponent;
        except
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.AllocateHWnd;
begin
    FThreadId     := GetCurrentThreadId;
    FWindowHandle := Borland.Vcl.WinUtils.AllocateHWnd(Self.WndProc);
    if FWindowHandle = 0 then
        raise EIcsException.Create(
                   'Cannot create a hidden window for ICS component');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.DeallocateHWnd;
begin
    if FWindowHandle = 0 then
        Exit;

    Borland.Vcl.WinUtils.DeallocateHWnd(FWindowHandle);
    FWindowHandle := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.ThreadAttach;
begin
    if FWindowHandle <> 0 then
        raise EIcsException.Create('Cannot attach when not detached');
    Self.AllocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.ThreadDetach;
begin
    if GetCurrentThreadID <> FThreadID then
        raise EIcsException.Create('Cannot detach from another thread');
    Self.DeallocateHWnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Loop thru message processing until the WM_QUIT message is received        }
{ This is intended for multithreaded application using TWSocket.            }
{ MessageLoop is different from ProcessMessages because it actually block   }
{ if no message is available. The loop is broken when WM_QUIT is retrieved. }
procedure TIcsComponent.MessageLoop;
var
    MsgRec : TMsg;
begin
    { If GetMessage retrieves the WM_QUIT, the return value is FALSE and    }
    { the message loop is broken.                                           }
    while GetMessage(MsgRec, 0, 0, 0) do begin
        TranslateMessage(MsgRec);
        DispatchMessage(MsgRec)
    end;
    FTerminated := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function is very similar to TApplication.ProcessMessage              }
{ You can also use it if your application has no TApplication object (Forms }
{ unit not referenced at all).                                              }
function TIcsComponent.ProcessMessage : Boolean;
var
    Msg : TMsg;
begin
    Result := FALSE;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
        Result := TRUE;
        if Msg.Message = WM_QUIT then
            FTerminated := TRUE
        else begin
            TranslateMessage(Msg);
            DispatchMessage(Msg);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Loop thru message processing until all messages are processed.            }
{ This function is very similar to TApplication.ProcessMessage              }
{ This is intended for multithreaded application using TWSocket.            }
{ You can also use it if your application has no TApplication object (Forms }
{ unit not referenced at all).                                              }
procedure TIcsComponent.ProcessMessages;
begin
    while Self.ProcessMessage do { loop };
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.MessagePump;
begin
{$IFDEF NOFORMS}
    { The Forms unit (TApplication object) has not been included.           }
    { We used either an external message pump or our internal message pump. }
    { External message pump has to set Terminated property to TRUE when the }
    { application is terminated.                                            }
    if Assigned(FOnMessagePump) then
        FOnMessagePump(Self)
    else
        Self.ProcessMessages;
{$ELSE}
    if FMultiThreaded then
        Self.ProcessMessages
    else
        Application.ProcessMessages;
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsComponent.AbortComponent;
begin
    // To be overriden in derived classes
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

