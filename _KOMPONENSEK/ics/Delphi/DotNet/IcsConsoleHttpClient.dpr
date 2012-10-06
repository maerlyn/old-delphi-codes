{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo for TWSocket component for Delphi 8 for the Microsoft
              for .NET framework.
              This demo show how to handle HTTP requests.
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
program IcsConsoleHttpClient;

{$APPTYPE CONSOLE}

uses
  Borland.Vcl.SysUtils,
  Borland.Vcl.Windows,
  Borland.Vcl.Classes,
  OverByte.Ics.ConApp,
  OverByte.Ics.WSocket,
  Overbyte.Ics.HttpClient;

const
    IcsConsoleHttpClientVersion = 100;
    CopyRight : String = ' IcsConsoleHttpClient (c) 2003-2005 F. Piette V1.00 ';

type
    TWSocketApplication = class(TConApplication)
    protected
        HttpClient1 : THttpClient;
        ProxyServer : String;
        ProxyPort   : String;
        procedure WndProc(var Msg: TMsg); override;
    public
        procedure DoLineReceived(const Line : String); override;
        procedure Display(const Msg : String);
        procedure Help;
        procedure Get(const Url : String);
        procedure Proxy(const Prm : String);
        procedure Execute; override;
        procedure HttpClientDisplay(Sender : TObject; var MSg : String);
        procedure HttpClientRequestDone(Sender    : TObject;
                                        RqType    : THttpRequest;
                                        ErrCode   : Word);
        procedure HttpClientDocBegin(Sender: TObject);
        procedure HttpClientDocData(Sender       : TObject;
                                    const Buffer : TBytes;
                                    Offset       : Integer;
                                    Len          : Integer);
        procedure HttpClientDocEnd(Sender: TObject);
        procedure HttpClientHeaderBegin(Sender: TObject);
        procedure HttpClientHeaderData(Sender: TObject);
        procedure HttpClientHeaderEnd(Sender: TObject);
        procedure HttpClientCookie(Sender       : TObject;
                                   const Data   : String;
                                   var   Accept : Boolean);
    end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WndProc(var Msg: TMsg);
begin
    inherited WndProc(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Display(const Msg : String);
begin
    ConsoleWriteLn(Msg);
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
    else if (Cmd = 'H') or (Cmd = '?') or (Cmd = 'HELP') then
        Help
    else if (Cmd = 'G') or (Cmd = 'GET') then
        Get(Trim(Prm))
    else if (Cmd = 'P') or (Cmd = 'PROXY') then
        Proxy(Trim(Prm))
    else
        ConsoleWriteLn('Unknown command, type HELP to get command list.');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Proxy(const Prm : String);
var
    I : Integer;
begin
    if Prm = '' then begin
        ProxyServer := '';
        ProxyPort   := '';
    end
    else begin
        I := Pos('/', Prm);
        if I < 1 then
            ProxyServer := Prm
        else begin
            ProxyServer := Copy(Prm, 1, I - 1);
            ProxyPort   := Copy(Prm, I + 1, Length(Prm));
        end;
    end;
    if ProxyServer = '' then
        ConsoleWriteLn('Proxy set to none')
    else
        ConsoleWriteLn('Proxy set to ' + ProxyServer + '/' + ProxyPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Get(const Url : String);
begin
    try
        if Url = '' then
            HttpClient1.Url := 'http://localhost'
        else
            HttpClient1.Url := Url;
        HttpClient1.Proxy     := ProxyServer;
        HttpClient1.ProxyPort := ProxyPort;
        HttpClient1.GetAsync;
        ConsoleWriteLn('Getting "' + Url + '"');
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientDisplay(
    Sender  : TObject;
    var MSg : String);
begin
    ConsoleWriteLn(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientDocData(
    Sender       : TObject;
    const Buffer : TBytes;
    Offset       : Integer;
    Len          : Integer);
var
    Buf : String;
    I   : Integer;
begin
    if Len <= 0 then
        Exit;
    ConsoleWrite('DocData: ');
    SetLength(Buf, Len);
    for I := 1 to Len do
        Buf[I] := Char(Buffer[Offset + I - 1]);
    ConsoleWriteLn(Buf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientDocBegin(Sender: TObject);
begin
    ConsoleWriteLn('DocBegin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientDocEnd(Sender: TObject);
begin
    ConsoleWriteLn('DocEnd');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientHeaderBegin(Sender: TObject);
begin
    ConsoleWriteLn('HeaderBegin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientHeaderData(Sender: TObject);
begin
    ConsoleWriteLn('HeaderData: ' + HttpClient1.LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientHeaderEnd(Sender: TObject);
begin
    ConsoleWriteLn('HeaderEnd');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientCookie(
    Sender       : TObject;
    const Data   : String;
    var   Accept : Boolean);
begin
    ConsoleWriteLn('Cookie: ' + Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.HttpClientRequestDone(
    Sender    : TObject;
    RqType    : THttpRequest;
    ErrCode   : Word);
begin
    if ErrCode <> 0 then
        ConsoleWriteLn('Request done. Error #' + IntToStr(ErrCode))
    else
        ConsoleWriteLn('Request done.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   Q                   Quit program');
    ConsoleWriteLn('   G url               Get the URL');
    ConsoleWriteLn('   P server/port       Set HTTP proxy server/port');
    ConsoleWriteLn('   H                   Display this help text');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    ConsoleWriteLn('IcsConsoleHttpClient Starting !');
    Help;
    WSocketForceLoadWinsock;

    HttpClient1               := THttpClient.Create(nil);
    HttpClient1.OnDocBegin    := HttpClientDocBegin;
    HttpClient1.OnDocData     := HttpClientDocData;
    HttpClient1.OnDocEnd      := HttpClientDocEnd;
    HttpClient1.OnHeaderBegin := HttpClientHeaderBegin;
    HttpClient1.OnHeaderData  := HttpClientHeaderData;
    HttpClient1.OnHeaderEnd   := HttpClientHeaderEnd;
    HttpClient1.OnRequestDone := HttpClientRequestDone;
    HttpClient1.OnCookie      := HttpClientCookie;

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
    WriteLn('DebugEvent: ', Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
  //OverByte.Ics.WSocket.WSocketDebugEvent := DebugEvent;

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
