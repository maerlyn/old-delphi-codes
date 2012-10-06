{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo for TWSocket component for Delphi 8 for the Microsoft
              for .NET framework.
              This demo show how to use FTP protocol.
Creation:     December 2003
Version:      1.00
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

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program IcsConsoleFtpClient;

{$APPTYPE CONSOLE}

uses
    Borland.Vcl.SysUtils,
    Borland.Vcl.Windows,
    Borland.Vcl.Classes,
    OverByte.Ics.ConApp,
    OverByte.Ics.WSocket,
    Overbyte.Ics.FtpClient;

const
    IcsConsoleFtpClientVersion = 100;
    CopyRight : String = ' IcsConsoleFtpClient (c) 2003-2005 F. Piette V1.00 ';

type
    TWSocketApplication = class(TConApplication)
    protected
        FtpClient1 : TFtpClient;
        DataStream : TStream;
        procedure WndProc(var Msg: TMsg); override;
    public
        procedure DoLineReceived(const Line : String); override;
        procedure Display(const Msg : String);
        procedure Help;
        procedure OpenSession(const Prm : String);
        procedure CloseSession;
        procedure SendUser(const Prm : String);
        procedure SendPassword(const Prm : String);
        procedure SetType(const Prm : String);
        procedure SetBinaryMode;
        procedure SetTextMode;
        procedure GetFile(const Prm : String);
        procedure SetPassiveMode(const Prm : String);
        procedure ListDir(const Prm : String);
        procedure Execute; override;
        procedure FtpClientDisplay(Sender : TObject; var MSg : String);
        procedure FtpClientRequestDone(Sender    : TObject;
                                       RqType    : TFtpRequest;
                                       ErrCode   : Word);
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WndProc(var Msg: TMsg);
begin
//    if Msg.message = WM_ASYNCSELECT then
//        ConsoleWriteLn('WM_ASYNCSELECT');
    inherited WndProc(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Display(const Msg : String);
begin
    ConsoleWriteLn(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.OpenSession(const Prm : String);
var
    I : Integer;
begin
    if FtpClient1.State = ftpNotConnected then begin
       ConsoleWriteLn('You must first close the connection');
       Exit;
    end;

    try
        if Prm = '' then begin
            FtpClient1.HostName     := 'localhost';
            FtpClient1.Port         := 'ftp';
        end
        else begin
            I := Pos('/', Prm);
            if I > 0 then begin
                FtpClient1.HostName := Trim(Copy(Prm, 1, I - 1));
                FtpClient1.Port     := Trim(Copy(Prm, I + 1, Length(Prm)));
            end
            else begin
                FtpClient1.HostName := Prm;
                FtpClient1.Port     := 'ftp';
            end;
        end;

        FtpClient1.OpenAsync;
        ConsoleWriteLn('Connecting to ' + FtpClient1.HostName + '/' +
                                          FtpClient1.Port + '...');
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SetType(const Prm : String);
var
    Value : String;
begin
    try
        Value := UpperCase(Prm);
        if (Value = 'I') or (Value = 'BINARY') or (Value = 'IMAGE') then
            FtpClient1.Binary := TRUE
        else if (Value = 'A') or (Value = 'ASCII') then
            FtpClient1.Binary := FALSE
        else begin
            ConsoleWriteLn('Unsupported type. Use BINARY or ASCII');
            Exit;
        end;
        FtpClient1.TypeSetAsync;
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SetPassiveMode(const Prm : String);
var
    Value : String;
begin
    try
        Value := UpperCase(Prm);
        if (Value = 'ON') or (Value = 'TRUE') or (Value = 'T') then
            FtpClient1.Passive := TRUE
        else if (Value = 'OFF') or (Value = 'FALSE') or (Value = 'F') then
            FtpClient1.Passive := FALSE
        else begin
            ConsoleWriteLn('Invalid mode. Use PASV ON or PASV OFF.');
            Exit;
        end;
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SendUser(const Prm : String);
begin
    try
        FtpClient1.UserName     := Prm;
        FtpClient1.UserAsync;
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SendPassword(const Prm : String);
begin
    try
        FtpClient1.Password     := Prm;
        FtpClient1.PassAsync;
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.GetFile(const Prm: String);
var
    I : Integer;
begin
    I := Pos(' ', Prm);
    if I <= 0 then begin
        FtpCLient1.HostFileName  := Prm;
        FtpClient1.LocalFileName := Prm;
    end
    else begin
        FtpCLient1.HostFileName  := Copy(Prm, 1, I - 1);
        FtpClient1.LocalFileName := Trim(Copy(Prm, I, Length(Prm)));
    end;
    try
        FtpClient1.GetAsync;
    except
        on E:Exception do begin
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
            DataStream.Free;
            DataStream := nil;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ListDir(const Prm : String);
begin
    try
        DataStream := TMemoryStream.Create;
        FtpClient1.HostFileName  := Prm;
        FtpClient1.LocalStream   := DataStream;
        FtpClient1.DirAsync;
    except
        on E:Exception do begin
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
            DataStream.Free;
            DataStream := nil;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SetBinaryMode;
begin
    try
        FtpClient1.Binary := TRUE;
        FtpClient1.TypeSetAsync;
    except
        on E:Exception do begin
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
            DataStream.Free;
            DataStream := nil;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SetTextMode;
begin
    try
        FtpClient1.Binary := FALSE;
        FtpClient1.TypeSetAsync;
    except
        on E:Exception do begin
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
            DataStream.Free;
            DataStream := nil;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.CloseSession;
begin
    try
        FtpClient1.QuitAsync;
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.FtpClientDisplay(
    Sender : TObject;
    var MSg : String);
begin
    ConsoleWriteLn(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.FtpClientRequestDone(
    Sender    : TObject;
    RqType    : TFtpRequest;
    ErrCode   : Word);
var
    Count  : Integer;
    Buffer : TBytes;
begin
    if ErrCode <> 0 then
        ConsoleWriteLn('Request done. Error #' + IntToStr(ErrCode) +
                       ': ' + FtpClient1.ErrorMessage)
    else
        ConsoleWriteLn('Request done.');

    if RqType = ftpDirAsync then begin
        SetLength(Buffer, 100);
        DataStream.Seek(0, soFromBeginning);
        while TRUE do begin
            Count := DataStream.Read(Buffer, Length(Buffer));
            if Count <= 0 then
                break;
            ConsoleWrite(Buffer);
        end;
        ConsoleWriteLn;
        DataStream.Free;
        DataStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   QUIT                     Quit program');
    ConsoleWriteLn('   OPEN host/port           Open session to host/port');
    ConsoleWriteLn('   CLOSE                    Close session');
    ConsoleWriteLn('   USER user                Send username');
    ConsoleWriteLn('   PASS password            Send password');
    ConsoleWriteLn('   DIR  wildcard            Get directory list');
    ConsoleWriteLn('   GET  HostFile LocalFile  Retrieve a file');
    ConsoleWriteLn('   BINARY                   Transfert using binary mode');
    ConsoleWriteLn('   TEXT                     Transfert using text mode');
    ConsoleWriteLn('   PASV on/off              Set passive mode ON or OFF');
    ConsoleWriteLn('   HELP                     Display this help text');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.DoLineReceived(const Line : String);
var
    Cmd : String;
    Prm : String;
    I   : Integer;
begin
    Cmd := '';
    I := 1;
    // Skip white spaces
    while (I <= Length(Line)) and (Line[I] = ' ') do
        Inc(I);
    // Copy first word
    while (I <= Length(Line)) and (Line[I] <> ' ') do begin
        Cmd := Cmd + UpperCase(Line[I]);
        Inc(I);
    end;
    // Skip white spaces
    while (I <= Length(Line)) and (Line[I] = ' ') do
        Inc(I);
    Prm := Copy(Line, I, Length(Line));
    if Cmd = '' then begin
        ConsoleWrite('> ');
        Exit;
    end;

    if (Cmd = 'Q') or (Cmd = 'QUIT') or (Cmd = 'BYE') then
        Terminate
    else if (Cmd = 'O') or (Cmd = 'OPEN') then
        OpenSession(Trim(Prm))
    else if (Cmd = 'C') or (Cmd = 'CLOSE') then
        CloseSession
    else if (Cmd = 'U') or (Cmd = 'USER') then
        SendUser(Trim(Prm))
    else if (Cmd = 'T') or (Cmd = 'TYPE') then
        SetType(Trim(Prm))
    else if (Cmd = 'G') or (Cmd = 'GET') then
        GetFile(Trim(Prm))
    else if (Cmd = 'B') or (Cmd = 'BIN') or (Cmd = 'BINARY') then
        SetBinaryMode
    else if (Cmd = 'TE') or (Cmd = 'TXT') or (Cmd = 'TEXT') then
        SetTextMode
    else if (Cmd = 'PASV') or (Cmd = 'PASSIVE') then
        SetPassiveMode(Trim(Prm))
    else if (Cmd = 'D') or (Cmd = 'DIR') or (Cmd = 'LS') or (Cmd = 'LIST') then
        ListDir(Trim(Prm))
    else if (Cmd = 'P') or (Cmd = 'PW') or (Copy(Cmd, 1, 4) = 'PASS') then
        SendPassword(Trim(Prm))
    else if (Cmd = 'H') or (Cmd = '?') or (Cmd = 'HELP') then
        Help
    else
        ConsoleWriteLn('Unknown command. Type HELP for command list.');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    ConsoleWriteLn('IcsConsoleFtpClient Starting !');
    Help;
    WSocketForceLoadWinsock;

    FtpClient1               := TFtpClient.Create(nil);
    FtpClient1.OnDisplay     := FtpClientDisplay;
    FtpClient1.OnRequestDone := FtpClientRequestDone;

    ConsoleWriteLn;
    ConsoleWriteLn('LocalHost = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        ConsoleWriteLn('IP #' + IntToStr(I + 1) + ' = ' + MyIP.Strings[I]);
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugEvent(Sender : TObject; const Msg : String);
begin
    WriteLn('DebugEvent :"', Msg, '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
  OverByte.Ics.WSocket.WSocketDebugEvent := DebugEvent;

  try
    TConApplication.CreateInstance(TWSocketApplication);
    TConApplication.Run;
    TConApplication.Done;
  except
    on E:Exception do begin
        WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
        if Assigned(ConApplication) then
            ConApplication.Terminate;
    end;
  end;
end.
