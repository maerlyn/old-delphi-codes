{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo for TWSocket component for Delphi 8 for the Microsoft
              for .NET framework.
              This demo show how to send UDP datagrams.
Creation:     December 2003
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-205 by François PIETTE
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
program IcsSocketConsoleUdpSender;

{$APPTYPE CONSOLE}

uses
  Borland.Vcl.SysUtils,
  Borland.Vcl.Classes,
  Borland.Vcl.Windows,
  OverByte.Ics.ConApp,
  OverByte.Ics.WSocket,
  OverByte.Ics.WinSock;

const
    IcsSocketConsoleUdpSenderVersion = 100;
    CopyRight : String = ' IcsSocketConsoleUdpSender (c) 2003-2005 F. Piette V1.00 ';

type
    TWSocketApplication = class(TConApplication)
    protected
        WSocket1       : TWSocket;
        FHostTo        : String;
        FPortTo        : String;
    public
        procedure Help;
        procedure Execute; override;
        procedure AddressTo(const Prm : String);
        procedure SendData(const Prm : String);
        procedure DoLineReceived(const Line : String); override;
    end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    ConsoleWriteLn('IcsSocketConsoleUdpSender Starting !');
    Help;

    WSocket1 := TWSocket.Create(nil);
    ConsoleWriteLn;
    ConsoleWriteLn('LocalHost     = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        ConsoleWriteLn(MyIP.Strings[I]);
    FHostTo := 'localhost';
    FPortTo := '1721';
    ConsoleWrite('> ');
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
    else if (Cmd = 'S') or (Cmd = 'SEND') then
        SendData(Prm)
    else if (Cmd = 'A') or (Cmd = 'ADDR') or (Cmd = 'ADDRESS') then
        AddressTo(Prm)
    else if (Cmd = 'C') or (Cmd = 'CLOSE') then
        WSocket1.Close
    else if (Cmd = 'H') or (Cmd = 'HELP') or (Cmd = '?') then
        Help
    else
        ConsoleWriteLn('Unknown command');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SendData(const Prm : String);
begin
    if (Prm = '') or (Prm = '?') then begin
        ConsoleWriteLn('Please enter your text with the command.');
        Exit;
    end;

    if WSocket1.State <> wsConnected then begin
        WSocket1.Proto := 'udp';
        WSocket1.Addr  := FHostTo;
        WSocket1.Port  := FPortTo;
        WSocket1.Connect;
    end;
    WSocket1.SendStr(Prm);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.AddressTo(const Prm : String);
var
    I : Integer;
begin
    if (Prm <> '') and (Prm <> '?') then begin
        I := Pos('/', Prm);
        if I > 0 then begin
            FHostTo := Trim(Copy(Prm, 1, I - 1));
            FPortTo := Trim(Copy(Prm, I + 1, Length(Prm)));
        end
        else begin
            FHostTo := Prm;
            FPortTo := '1721';
        end;
        if WSocket1.State = wsConnected then
            WSocket1.Close;
    end;
    ConsoleWriteLn('Addressed to ' + FHostTo + '/' + FPortTo)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   QUIT                Quit program');
    ConsoleWriteLn('   ADDR host/port      Address UDP to socket to host/port');
    ConsoleWriteLn('                       default to localhost/1721');
    ConsoleWriteLn('   SEND text           Send text followed by CR/LF');
    ConsoleWriteLn('   CLOSE               Close socket');
    ConsoleWriteLn('   HELP                Display this help text');
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

