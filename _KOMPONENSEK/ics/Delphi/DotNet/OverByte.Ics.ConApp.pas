{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Class encapsulate event driven console mode application
Creation:     Dec 07, 2003
Version:      0.99
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2005 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Dec 07, 2003 V0.99.0 Pre-release for Delphi 8 for .NET framework
Jan 26, 2004 V0.99.1 Cosmetic changes


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverByte.Ics.ConApp platform;

interface

uses
  Borland.Vcl.SysUtils,
  Borland.Vcl.Classes,
  Borland.Vcl.Windows,
  Borland.Vcl.Messages,
  System.Text;

const
    ConApplicationVerion = 099;
    CopyRight : String = ' TConApplication (c) 2003-2005 F. Piette V0.99 ';
    WM_STARTUP   = WM_USER + 1;

type
    TKeyboardThread = class(TThread)
    protected
        FEvent : THandle;
        procedure   Execute; override;
    public
        ConAppThreadID : Integer;
        constructor Create(Suspended : Boolean);
        destructor  Destroy; override;
        procedure   Terminate;
    end;

    TConApplicationClass = class of TConApplication;
    TConApplication = class(TComponent)
    private
        FThreadID           : Integer;
        FTerminated         : Boolean;
        FKbdThread          : TKeyboardThread;
        FLineMode           : Boolean;
        FLineBuffer         : String;
        FLineEcho           : Boolean;
        FIsInputRedirected  : Boolean;
        FIsOutputRedirected : Boolean;
        //FHandle             : HWND;
    protected
        procedure   MessageLoop; virtual;
        function    ProcessMessage: Boolean; virtual;
        procedure   ProcessMessages; virtual;
        procedure   WndProc(var Msg: TMsg); virtual;
        procedure   WMKeyDown(var MsgRec : TMsg); virtual;
        procedure   WMKeyUp(var MsgRec : TMsg); virtual;
        function    GetExeName: String;
    public
        class procedure CreateInstance(AppClass : TConApplicationClass); virtual;
        class procedure Run; virtual;
        class procedure Done; virtual;
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy; override;
        procedure   Execute; virtual;
        procedure   Terminate; virtual;
        procedure   ConsoleWrite(const S : String); overload; virtual;
        procedure   ConsoleWrite(B : Boolean); overload; virtual;
        procedure   ConsoleWriteLn(B : Boolean); overload; virtual;
        procedure   ConsoleWriteLn(const S : String); overload; virtual;
        procedure   ConsoleWriteLn; overload; virtual;
        procedure   DoLineReceived(const Line : String); virtual;
        procedure   DoCharReceived(Ch : Char); virtual;
        property ThreadID   : Integer  read  FThreadID   write FThreadID;
        property Terminated : Boolean  read  FTerminated write FTerminated;
        property LineMode   : Boolean  read  FLineMode   write FLineMode;
        property LineEcho   : Boolean  read  FLineEcho   write FLineEcho;
        property IsInputRedirected  : Boolean  read  FIsInputRedirected;
        property IsOutputRedirected : Boolean  read  FIsOutputRedirected;
        property ExeName            : String   read  GetExeName;
    end;


var
    ConApplication : TConApplication;

implementation



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConApplication.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FLineMode := TRUE;
    FLineEcho := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConApplication.Destroy;
begin
    FreeAndNil(FKbdThread);
    inherited;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TConApplication.CreateInstance(
    AppClass: TConApplicationClass);
var
    CMode : DWORD;
begin
    FreeAndNil(ConApplication);
    ConApplication                     := AppClass.Create(nil);
    ConApplication.FIsInputRedirected  := not GetConsoleMode(GetStdHandle(
                                                  STD_INPUT_HANDLE), CMode);
    ConApplication.FIsOutputRedirected := not GetConsoleMode(GetStdHandle(
                                                  STD_OUTPUT_HANDLE), CMode);
    ConApplication.FThreadID           := GetCurrentThreadID;
    ConApplication.FKbdThread          := TKeyboardThread.Create(TRUE);
    ConApplication.FKbdThread.ConAppThreadID := ConApplication.FThreadID;
    //SetConsoleCtrlHandler(@CtrlHandlerRoutine, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Execute;
begin
    ConsoleWriteLn('Error: You must override TConApplication.Execute !');
    Terminate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Terminate;
begin
    PostThreadMessage(FThreadID, WM_QUIT, 0, 0);
    FTerminated := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.MessageLoop;
var
    Msg : TMsg;
begin
    // If GetMessage retrieves the WM_QUIT, the return value is FALSE and
    // the message loop is broken.
    while (not FTerminated) and GetMessage(Msg, 0, 0, 0) do
        WndProc(Msg);
    FTerminated := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TConApplication.ProcessMessage: Boolean;
var
    Msg    : TMsg;
begin
    Result := FALSE;
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
        Result := TRUE;
        if Msg.Message = WM_QUIT then
            FTerminated := TRUE
        else
            WndProc(Msg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ProcessMessages;
begin
    while Self.ProcessMessage do
        { loop };
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TConApplication.Run;
begin
    ConApplication.ProcessMessage;         // This will create message queue
    PostThreadMessage(ConApplication.FThreadID, WM_STARTUP, 0, 0);
    ConApplication.FKbdThread.Resume;
    ConApplication.MessageLoop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
class procedure TConApplication.Done;
begin
    //tConsoleCtrlHandler(nil, FALSE);
    FreeAndNil(ConApplication);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.WndProc(var Msg: TMsg);
begin
    if Msg.hwnd = 0 then begin        // We process thread's messages
        case Msg.message of
        WM_STARTUP:
            Execute;
        WM_KEYDOWN:
            WMKeyDown(Msg);
        WM_KEYUP:
            WMKeyUp(Msg);
        else
           begin
//            defaulthandler(msg);
            TranslateMessage(Msg);
            DispatchMessage(Msg);
           end;
        end;
    end
    else begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.WMKeyDown(var MsgRec: TMsg);
var
    Ascii : Char;
begin
    Ascii := Char(MsgRec.wParam and 255);

//    WriteLn('WMKeyDown ' +
//            IntToHex(MsgRec.WParam, 8) + ' ' +
//            IntToHex(MsgRec.lParam, 8));
    if not FLineMode then begin
        DoCharReceived(Ascii);
    end
    else begin
        case Ascii of
        #0: begin                      // Not an ASCII char
            end;
        #9: begin                      // TabChar
                repeat
                    if FLineEcho then
                        ConsoleWrite(' ');
                    FLineBuffer := FLineBuffer + ' ';
                until (Length(FLineBuffer) and 7) = 0;
            end;
        #8: begin                     // BackSpace
                if Length(FLineBuffer) > 0 then begin
                     if FLineEcho then
                         ConsoleWrite(#8#32#8);
                     SetLength(FLineBuffer, Length(FLineBuffer) - 1);
                end;
            end;
        #13: begin                     // Return
                if FLineEcho then
                    ConsoleWrite(#13#10);
                DoLineReceived(FLineBuffer);
                FLineBuffer := '';    // Clear line
            end;
        else
            if FLineEcho then
                ConsoleWrite(Char(MsgRec.WParam));
            FLineBuffer := FLineBuffer + Char(MsgRec.WParam);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.WMKeyUp(var MsgRec: TMsg);
begin

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.DoLineReceived(const Line : String);
begin
    // By default: nothing to do
    // Todo: Override this method in your derived class
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.DoCharReceived(Ch: Char);
begin
    // By default: nothing to do
    // Todo: Override this method in your derived class
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ConsoleWrite(const S: String);
begin
    Write(S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ConsoleWrite(B : Boolean);
begin
    Write(B);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ConsoleWriteLn(const S: String);
begin
    WriteLn(S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ConsoleWriteLn(B : Boolean);
begin
    WriteLn(B);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.ConsoleWriteLn;
begin
    WriteLn;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TConApplication.GetExeName: String;
var
    SB: System.Text.StringBuilder;
begin
    SB := System.Text.StringBuilder.Create(256);
    GetModuleFileName(GetModuleHandle(''), SB, SB.Capacity);
    Result := SB.ToString;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// Console mode applications do not receive keyboard messages as GUI apps.
// We use a thread to wait for keyboard activity and generate keyboard
// messages as in a GUI application.
constructor TKeyboardThread.Create(Suspended: Boolean);
begin
    inherited Create(TRUE);
    FEvent := CreateEvent(nil, TRUE, FALSE, '');
    if not Suspended then
        Resume;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TKeyboardThread.Destroy;
begin
    if FEvent <> 0 then begin
        SetEvent(FEvent);
        Sleep(0);
        CloseHandle(FEvent);
        FEvent := 0;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TKeyboardThread.Execute;
var
    hConsole    : THandle;
    Status      : DWORD;
    InputBuffer : TInputRecord;
    KeyEvent    : TKeyEventRecord;
    Count       : DWORD;
    Ascii       : Char;
    I           : Integer;
    Handles     : array [0..1] of THandle;
begin
    hConsole   := GetStdHandle(STD_INPUT_HANDLE);
    Handles[0] := hConsole;
    Handles[1] := FEvent;
    while not Terminated do begin
        Status := MsgWaitForMultipleObjects(2, Handles, FALSE, INFINITE, 0);
        if Status = WAIT_FAILED then begin
            //WriteLn('WAIT_FAILED');
            break;                             // Wait failed
        end
        else if Status = WAIT_OBJECT_0 + 1 then begin
            //WriteLn('FEvent is signaled');
            break;                            // FEvent is signaled
        end
        else if Status = WAIT_OBJECT_0 then begin  // Console is signaled
            if ReadConsoleInput(hConsole, InputBuffer, 1, Count) then begin
                if InputBuffer.EventType = KEY_EVENT then begin
{$IFDEF VER90}  { Delphi 2 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER93}  { Bcb 1    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER100} { Delphi 3 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER110} { Bcb 3    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$ENDIF}
{ Starting from Delphi 4 and Bcb4, they changed definition }
                    KeyEvent := InputBuffer.Event.KeyEvent;
{$ENDIF}
{$ENDIF}
{$ENDIF}
                    Ascii := Char(KeyEvent.AsciiChar);
                    for I := 1 to KeyEvent.wRepeatCount do begin
                        if KeyEvent.bKeyDown then begin
                            PostThreadMessage(
                                ConAppThreadID,
                                WM_KEYDOWN,
                                Ord(Ascii) + (KeyEvent.wVirtualKeyCode shl 16),
                                KeyEvent.dwControlKeyState);
                        end
                        else begin
                            PostThreadMessage(
                                ConAppThreadID,
                                WM_KEYUP,
                                Ord(Ascii) + (KeyEvent.wVirtualKeyCode shl 16),
                                KeyEvent.dwControlKeyState);
                        end;
                    end;
                end;
            end;
        end
        else begin
            WriteLn('Unknown status ', Status);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TKeyboardThread.Terminate;
begin
    inherited Terminate;
    if FEvent <> 0 then
        SetEvent(FEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

