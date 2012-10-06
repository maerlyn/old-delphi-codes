program IcsSocketClientConsoleDemo;

{$APPTYPE CONSOLE}

//%DotNetAssemblyCompiler 'e:\winnt\microsoft.net\framework\v1.1.4322\system.dll'}
//%DotNetAssemblyCompiler 'e:\winnt\microsoft.net\framework\v1.1.4322\system.data.dll'}
//%DotNetAssemblyCompiler 'e:\winnt\microsoft.net\framework\v1.1.4322\system.xml.dll'}

uses
  //Borland.Vcl.SysUtils,
  Borland.Vcl.Classes,
  OverByte.Ics.ConApp,
  OverByte.Ics.WSocket,
  OverByte.Ics.WinSock;

type
    TWSocketApplication = class(TConApplication)
    protected
        WSocket1       : TWSocket;
        FSocksServer   : String;
        FSocksPort     : String;
    public
        procedure Help;
        procedure Execute; override;
        procedure SendData(const Data : String);
        procedure DnsLookup(const HostName : String);
        procedure ReverseDnsLookup(const IP: String);
        procedure ConnectSocket(const Prm : String);
        procedure ListenSocket(const Prm : String);
        procedure DoLineReceived(const Line : String); override;
        procedure SetSocks5(const Prm : String);
        procedure LineMode(const Prm : String);
        procedure WSocket1SessionConnected(Sender  : TObject; ErrCode : WORD);
        procedure WSocket1SessionAvailable(Sender  : TObject; ErrCode : WORD);
        procedure WSocket1SessionClosed(Sender  : TObject; ErrCode : WORD);
        procedure WSocket1DataAvailable(Sender  : TObject; ErrCode : WORD);
        procedure WSocket1DnsLookupDone(Sender  : TObject; ErrCode : WORD);
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1SessionConnected(
    Sender  : TObject;
    ErrCode : WORD);
begin
    if ErrCode = 0 then
        ConsoleWriteLn('Connected')
    else
        ConsoleWriteLn('Unable to connect. Error #' + ErrCode.ToString);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1SessionAvailable(
    Sender  : TObject;
    ErrCode : WORD);
var
    NewSocket     : TSocket;
    WSocketClient : TWSocket;
    FromName      : TSockAddr;
    FromNameLen   : Integer;
begin
    if ErrCode <> 0 then begin
        ConsoleWriteLn('Session Available Error #' + ErrCode.ToString);
        Exit;
    end;

    ConsoleWriteLn('Session Available');
    NewSocket := (Sender as TWSocket).Accept;
    ConsoleWriteLn('NewSocket = ' + NewSocket.ToString);

    FromNameLen := SizeOf(FromName);
    WSocket_getsockname(NewSocket, FromName, FromNameLen);
    ConsoleWriteLn('Remote = ' + WSocket_inet_ntoa(FromName.sin_addr));

    WSocketClient := TWSocket.Create(nil);
    WSocketClient.OnDataAvailable := WSocket1DataAvailable;
    WSocketClient.Dup(NewSocket);
    ConsoleWriteLn('Remote = ' + WSocketClient.GetPeerAddr + '/' +
                                 WSocketClient.GetPeerPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1SessionClosed(
    Sender  : TObject;
    ErrCode : WORD);
begin
    if ErrCode = 0 then
        ConsoleWriteLn('Disconnected')
    else
        ConsoleWriteLn('Disconnected. Error #' + ErrCode.ToString);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1DnsLookupDone(
    Sender  : TObject;
    ErrCode : WORD);
var
    I : Integer;
begin
    ConsoleWriteLn('DnsLookupDone');
    if ErrCode <> 0 then
        ConsoleWriteLn('Error #' + ErrCode.ToString)
    else begin
        if (Sender as TWSocket).DnsResultList.Count = 0 then
            ConsoleWriteLn('No IP found')
        else
            for I := 1 to (Sender as TWSocket).DnsResultList.Count do
                ConsoleWriteLn('IP #' + I.ToString + ' = ' +
                               (Sender as TWSocket).DnsResultList.Strings[I - 1]);
    end;
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocket1DataAvailable(
    Sender  : TObject;
    ErrCode : WORD);
var
    Buf     : TBytes;
    Len     : Integer;
    BufStr  : String;
    I       : Integer;
begin
    SetLength(Buf, 1024);
    Len := (Sender as TWSocket).Receive(Buf, Length(Buf));
    if Len <= 0 then
        Exit;
    BufStr := '';
    for I := 0 to Len - 1 do
        BufStr := BufStr + Char(Buf[I]);
    ConsoleWriteLn('DataAvailable Len=' + Len.ToString +
                   ' Data="' + BufStr + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SendData(const Data : String);
begin
    ConsoleWriteLn('Sending "' + Data + '"');
    WSocket1.SendStr(Data + #13#10);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.DnsLookup(const HostName : String);
begin
    WSocket1.DnsLookup(HostName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ReverseDnsLookup(const IP: String);
begin
    WSocket1.ReverseDnsLookup(IP);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ConnectSocket(const Prm : String);
var
    I : Integer;
begin
    if (WSocket1.State = wsConnected) or
       (WSocket1.State = wsListening) then begin
       ConsoleWriteLn('You must first close the socket !');
       Exit;
    end;

    try
        if Prm = '' then begin
            WSocket1.Port     := 'telnet';
            WSocket1.Addr     := 'localhost';
        end
        else begin
            I := Pos('/', Prm);
            if I > 0 then begin
                WSocket1.Addr := Prm.Substring(0, I - 1).Trim;
                WSocket1.Port := Prm.SubString(I, Length(Prm) - I).Trim;
                //WSocket1.Addr := Trim(Copy(Prm, 1, I - 1));
                //WSocket1.Port := Trim(Copy(Prm, I + 1, Length(Prm)));
            end
            else begin
                WSocket1.Addr := Prm;
                WSocket1.Port := 'telnet';
            end;
        end;

        WSocket1.SocksServer := FSocksServer;
        WSocket1.SocksPort   := FSocksPort;
        WSocket1.SocksLevel  := '5';
        WSocket1.Proto       := 'tcp';
        WSocket1.Connect;
        ConsoleWriteLn('Connecting to ' + WSocket1.Addr + '/' +
                                          WSocket1.Port + '...');
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.SetSocks5(const Prm : String);
var
    I : Integer;
begin
    if Prm = '' then begin
        FSocksServer := '';
        FSocksPort   := '';
    end
    else if Prm <> '?' then begin
        I := Pos('/', Prm);
        if I > 0 then begin
            FSocksServer := Prm.Substring(0, I - 1).Trim;
            FSocksPort   := Prm.SubString(I, Length(Prm) - I).Trim;
            //FSocksServer := Trim(Copy(Prm, 1, I - 1));
            //FSocksPort   := Trim(Copy(Prm, I + 1, Length(Prm)));
        end
        else begin
            FSocksServer := Prm;
            FSocksPort   := '1080';
        end;
        if FSocksPort = '' then
            FSocksPort := '1080';
    end;
    if FSocksServer = '' then
        ConsoleWriteLn('Socks disabled')
    else
        ConsoleWriteLn('Socks ' + FSocksServer + '/' + FSocksPort);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.LineMode(const Prm : String);
begin
    if (Prm = '') or (Prm = '?') then begin
         ConsoleWrite('Line mode is ');
         ConsoleWriteLn(WSocket1.LineMode);
         Exit;
    end;
    WSocket1.LineMode:= (Prm.SubString(0, 1).ToUpper = 'T');
    //WSocket1.LineMode:= (UpperCase(Copy(Prm, 1, 1)) = 'T');
    ConsoleWrite('Line mode set to ');
    ConsoleWriteLn(WSocket1.LineMode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ListenSocket(const Prm : String);
begin
    if Prm = '' then
        WSocket1.Port           := 'telnet'
    else
        WSocket1.Port           := Prm;
    WSocket1.Proto              := 'tcp';
    WSocket1.Addr               := '0.0.0.0';
    try
        WSocket1.Listen;
        ConsoleWriteLn('Listenning port ' + WSocket1.GetXPort);
    except
        on E:Exception do
            ConsoleWriteLn('Exception' + E.ClassNAme + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Execute;
var
    MyIP : TStrings;
    I    : Integer;
begin
    ConsoleWriteLn('IcsSocketClientConsoleDemo Starting !');
    Help;

    WSocket1                    := TWSocket.Create(nil);
    WSocket1.OnSessionConnected := WSocket1SessionConnected;
    WSocket1.OnSessionAvailable := WSocket1SessionAvailable;
    WSocket1.OnSessionClosed    := WSocket1SessionClosed;
    WSocket1.OnDataAvailable    := WSocket1DataAvailable;
    WSocket1.OnDnsLookupDone    := WSocket1DnsLookupDone;
    ConsoleWriteLn;
    ConsoleWriteLn('LocalHost     = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        ConsoleWriteLn(MyIP.Strings[I]);
    FSocksServer := '';
    FSocksPort   := '1080';
    ConnectSocket('localhost/23');
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
        Cmd := Cmd + System.Char.ToUpper(Line[I]);
        //Cmd := Cmd + UpperCase(Line[I]);
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

    if Cmd = 'Q' then
        Terminate
    else if (Cmd = 'H') or (Cmd = 'HELP') or (Cmd = '?') then
        Help
    else if (Cmd = 'S') or (Cmd = 'SEND') then
        SendData(Prm)
    else if (Cmd = 'D') or (Cmd = 'DNS') then
        DnsLookup(Prm)
    else if (Cmd = 'R') or (Cmd = 'RDNS') then
        ReverseDnsLookup(Prm)
    else if (Cmd = 'CLOSE') or (Cmd = 'CL') or (Cmd = 'F') then
        WSocket1.Close
    else if (Cmd = 'C') or (Cmd = 'CONNECT') then
        ConnectSocket(Prm.Trim)
    else if (Cmd = 'SOCKS5') then
        SetSocks5(Prm.Trim)
    else if (Cmd = 'L') or (Cmd = 'LISTEN') then
        ListenSocket(Prm.Trim)
    else if (Cmd = 'LM') or (Cmd = 'LINEMODE') then
        LineMode(Prm.Trim)
    else
        ConsoleWriteLn('Unknown command');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   QUIT                Quit program');
    ConsoleWriteLn('   CONNECT host/port   Connect socket to host/port, ' +
                                          'default to localhost/23');
    ConsoleWriteLn('   CLOSE               Close socket');
    ConsoleWriteLn('   LISTEN port         Listen on port');
    ConsoleWriteLn('   DNS hostname        DNS lookup for hostname');
    ConsoleWriteLn('   RDNS IP             Reverse DNS lookup for IP');
    ConsoleWriteLn('   SEND text           Send text followed by CR/LF');
    ConsoleWriteLn('   SOCKS5 ?            Show socks config');
    ConsoleWriteLn('   SOCKS5              Disable socks');
    ConsoleWriteLn('   SOCKS5 server/port  Set socks config');
    ConsoleWriteLn('   LINEMODE            Select line mode operation');
    ConsoleWriteLn('   HELP                Display this help text');
    ConsoleWrite('> ');
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
