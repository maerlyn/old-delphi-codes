program IcsSocketServerConsoleDemo;

{$APPTYPE CONSOLE}

uses
  Borland.Vcl.SysUtils,
  Borland.Vcl.Classes,
  Borland.Vcl.Windows,
  OverByte.Ics.ConApp,
  OverByte.Ics.WSocket,
  OverByte.Ics.WinSock,
  OverByte.Ics.WSocketServer;

type
    { TTcpSrvClient is the class which will be instanciated by server component }
    { for each new client. N simultaneous clients means N TTcpSrvClient will be }
    { instanciated. Each being used to handle only a single client.             }
    { We can add any data that has to be private for each client, such as       }
    { receive buffer or any other data needed for processing.                   }
    TTcpSrvClient = class(TWSocketClient)
    public
        RcvdLine    : String;
        ConnectTime : TDateTime;
    end;

    TWSocketApplication = class(TConApplication)
    protected
        WSocketServer1 : TWSocketServer;
        procedure WSocketServer1ClientConnect(Sender  : TObject;
                                              Client  : TWSocketClient;
                                              ErrCode : Word);
        procedure WSocketServer1ClientDisconnect(Sender  : TObject;
                                                 Client  : TWSocketClient;
                                                 ErrCode : Word);
        procedure WSocketServer1BgException(Sender       : TObject;
                                            E            : Exception;
                                            var CanClose : Boolean);
        procedure ClientBgException(Sender       : TObject;
                                    E            : Exception;
                                    var CanClose : Boolean);
        procedure ClientDataAvailable(Sender: TObject; ErrCode: Word);
        procedure ProcessData(Client : TTcpSrvClient);
        procedure WndProc(var Msg: TMsg); override;
    public
        procedure DoLineReceived(const Line : String); override;
        procedure StartServer;
        procedure Display(const Msg : String);
        procedure Help;
        procedure Execute; override;
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
{ This event handler is called when listening (server) socket experienced   }
{ a background exception. Should normally never occurs.                     }
procedure TWSocketApplication.WSocketServer1BgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Server exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  { Hoping that server will still work ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a client socket experience a background }
{ exception. It is likely to occurs when client aborted connection and data }
{ has not been sent yet.                                                    }
procedure TWSocketApplication.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   { Goodbye client ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocketServer1ClientConnect(
    Sender  : TObject;
    Client  : TWSocketClient;
    ErrCode : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connected.' +
                ' Remote: ' + PeerAddr + '/' + PeerPort +
                ' Local: '  + GetXAddr + '/' + GetXPort);
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount) +
                ' clients connected.');
        LineMode            := TRUE;
        LineEdit            := TRUE;
        LineLimit           := 80; { Do not accept long lines }
        OnDataAvailable     := ClientDataAvailable;
//      OnLineLimitExceeded := ClientLineLimitExceeded;
        OnBgException       := ClientBgException;
        ConnectTime         := Now;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.WSocketServer1ClientDisconnect(
    Sender  : TObject;
    Client  : TWSocketClient;
    ErrCode : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client disconnecting: ' + PeerAddr + '   ' +
                'Duration: ' + FormatDateTime('hh:nn:ss',
                Now - ConnectTime));
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount - 1) +
                ' clients connected.');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ClientDataAvailable(
    Sender  : TObject;
    ErrCode : Word);
begin
    with Sender as TTcpSrvClient do begin
        { We use line mode. We will receive complete lines }
        RcvdLine := ReceiveStr;
        { Remove trailing CR/LF }
        while (Length(RcvdLine) > 0) and
              (Byte(RcvdLine[Length(RcvdLine)]) in [13, 10]) do
            RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
        Display('Received from ' + GetPeerAddr + ': ''' + RcvdLine + '''');
        ProcessData(Sender as TTcpSrvClient);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.ProcessData(Client : TTcpSrvClient);
var
    I       : Integer;
    AClient : TTcpSrvClient;
begin
    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.RcvdLine, 'help') = 0 then
        Client.SendStr('Commands are:' + #13#10 +
                       '  exit' + #13#10 +
                       '  who' + #13#10 +
                       '  time' + #13#10 +
                       '  exception' + #13#10)
    else if CompareText(Client.RcvdLine, 'exit') = 0 then
        { We can't call Client.Close here because we will immediately }
        { reenter DataAvailable event handler with same line because  }
        { a line is removed from buffer AFTER it has been processed.  }
        { Using CloseDelayed will delay Close until we are out of     }
        { current event handler.                                      }
        Client.CloseDelayed
    else if CompareText(Client.RcvdLine, 'time') = 0 then
        { Send server date and time to client }
        Client.SendStr(DateTimeToStr(Now) + #13#10)
    else if CompareText(Client.RcvdLine, 'who') = 0 then begin
        { Send client list to client }
        Client.SendStr('There are ' + IntToStr(WSocketServer1.ClientCount) +
                       ' connected users:' + #13#10);
        for I := WSocketServer1.ClientCount - 1 downto 0 do begin
            AClient := TTcpSrvClient(WSocketServer1.Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
    else if CompareText(Client.RcvdLine, 'exception') = 0 then
        { This will trigger a background exception for client }
        PostMessage(Client.Handle, WM_TRIGGER_EXCEPTION, 0, 0)
    else
        if Client.State = wsConnected then
            Client.SendStr('Unknown command: ''' + Client.RcvdLine + '''' + #13#10);
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

    if Cmd = 'Q' then
        Terminate
    else
        ConsoleWriteLn('Unknown command');
    ConsoleWrite('> ');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.Help;
begin
    ConsoleWriteLn('Commands:');
    ConsoleWriteLn('   Q                   Quit program');
    ConsoleWriteLn('   H                   Display this help text');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketApplication.StartServer;
begin
    try
        WSocketServer1.Proto              := 'tcp';
        WSocketServer1.Addr               := '0.0.0.0';
        WSocketServer1.Port               := 'telnet';
        WSocketServer1.ClientClass        := TTcpSrvClient;
        WSocketServer1.OnClientConnect    := WSocketServer1ClientConnect;
        WSocketServer1.OnClientDisconnect := WSocketServer1ClientDisconnect;
        WSocketServer1.OnBgException      := WSocketServer1BgException;
        WSocketServer1.Listen;
        ConsoleWriteLn('Listenning port ' + WSocketServer1.Port);
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
    ConsoleWriteLn('IcsSocketConsoleDemo Starting !');
    Help;

    WSocketServer1              := TWSocketServer.Create(nil);

    ConsoleWriteLn;
    ConsoleWriteLn('LocalHost = ' + LocalHostName);
    MyIP := LocalIPList;
    for I := 0 to MyIP.Count - 1 do
        ConsoleWriteLn('IP #' + IntToStr(I + 1) + ' = ' + MyIP.Strings[I]);
    ConsoleWrite('> ');
    StartServer;
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
