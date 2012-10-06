{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo for TWSocket component for Delphi 8 for the Microsoft
              for .NET framework.
              This demo show how to receive UDP datagrams.
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
program IcsSocketConsoleUdpReceiver;

{$APPTYPE CONSOLE}

uses
  System.IO,
  Borland.Vcl.SysUtils,
  Borland.Vcl.Classes,
  Borland.Vcl.Windows,
  OverByte.Ics.ConApp,
  OverByte.Ics.WSocket,
  OverByte.Ics.WinSock;

const
    IcsSocketConsoleUdpReceiverVersion = 100;
    CopyRight : String = ' IcsSocketConsoleUdpReceiver (c) 2003-2005 F. Piette V1.00 ';

type
    TWSocketApplication = class(TConApplication)
    protected
        WSocket1       : TWSocket;
        FLogFile       : TextFile;
        FLogFileName   : String;
        FLogOpened     : Boolean;
        FPort          : String;
        FRecvBuf       : TBytes;
        FCount         : Integer;
    public
        procedure Help;
        procedure Execute; override;
        procedure Display(const Msg : String);
        procedure Log(const Msg : String);
        procedure LogClose;
        procedure DoLineReceived(const Line : String); override;
        procedure WSocket1DataAvailable(Sender  : TObject; ErrCode : WORD);
        procedure StartListen(const Prm : String);
    end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Display(const Msg : String);
begin
    ConsoleWriteLn(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    ConsoleWriteLn('IcsSocketConsoleUdpReceiver Starting !');
    Help;

    WSocket1 := TWSocket.Create(nil);
    ConsoleWriteLn;
    ConsoleWriteLn('LocalHost     = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        ConsoleWriteLn(MyIP.Strings[I]);
    FLogFileName := 'IcsSocketConsoleUdpReceiver.log';
    StartListen('1721');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.StartListen(const Prm : String);
begin
    if (WSocket1.State = wsListening) or (WSocket1.State = wsConnected) then begin
        Display('Already listening on port ' + FPort);
        Display('You must first close the socket');
        Exit;
    end;
    
    Log('----- ' + FormatDateTime('YYYYMMDD HHNNSS ', Now) + 'Listenning');
    LogClose;

    if Length(FRecvBuf) <> 4096 then
        SetLength(FRecvBuf, 4096);

    if Prm <> '' then
        FPort := Trim(Prm);
    WSocket1.Proto           := 'udp';
    WSocket1.Port            := FPort;
    WSocket1.Addr            := '0.0.0.0';
    WSocket1.OnDataAvailable := WSocket1DataAvailable;
    WSocket1.Listen;
    Display('Listenning port ' + FPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DataToString(Data : TBytes; Len : Integer) : String;
var
    I  : Integer;
    Ch : Byte;
begin
    Result := '';
    I := 0;
    while I < Len do begin
         Ch := Data[I];
         if (Ch < 32) or (Ch > 126) or (Ch = 35) then
             Result := Result + '#' + Format('%02.2d', [Ch])
         else
             Result := Result + Char(Ch);
         Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Log(const Msg : String);
begin
    if (not FLogOpened) and (FLogFileName <> '') then begin
        try
            AssignFile(FLogFile, FLogFileName);
            if not FileExists(FLogFileName) then
                Rewrite(FLogFile)
            else
                Append(FLogFile);
            FLogOpened := TRUE;
        except
            on E:Exception do begin
                Display('Unable to open logfile "' + FLogFileName + '". ' +
                        E.ClassName + ': ' + E.Message);
            end;
        end;
    end;
    if FLogOpened then begin
        WriteLn(FLogFile, Msg);
        //Timer1.Enabled  := FALSE;
        //Timer1.Interval := 15000;
        //Timer1.Enabled  := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.LogClose;
begin
    if FLogOpened then begin
        CloseFile(FLogFile);
        FLogOpened := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1DataAvailable(
    Sender: TObject;
    ErrCode: Word);
var
    Remote    : TSockAddr;
    RemoteLen : Integer;
    Len       : Integer;
    LogString : String;
begin
    RemoteLen := SizeOf(TSockAddr);
    Len := (Sender as TWSocket).ReceiveFrom(FRecvBuf, Length(FRecvBuf),
                                            Remote, RemoteLen);
    if Len <= 0 then
        Exit;
    Inc(FCount);
    LogString := Format('%05.5d ', [FCount]) +
                 FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                 Format('%-15.15s : "', [WSocket_inet_ntoa(Remote.sin_addr)]) +
                 DataToString(FRecvBuf, Len) + '"';
    Log(LogString);
    Display(LogString);
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

    if (Cmd = 'Q') or (Cmd = 'QUIT') then
        Terminate
    else if (Cmd = 'C') or (Cmd = 'CLOSE') then
        WSocket1.Close
    else if (Cmd = 'L') or (Cmd = 'LISTEN') then
        StartListen(Prm)
    else if (Cmd = 'H') or (Cmd = 'HELP') or (Cmd = '?') then
        Help
    else
        ConsoleWriteLn('Unknown command');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   QUIT                    Quit program');
    ConsoleWriteLn('   LISTEN port             Listen UDP port ' +
                                               '(default to 1721)');
    ConsoleWriteLn('   CLOSE                   Close socket');
    ConsoleWriteLn('   HELP                    Display this help text');
    ConsoleWriteLn('   Note: You can use the first letter alone.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
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

