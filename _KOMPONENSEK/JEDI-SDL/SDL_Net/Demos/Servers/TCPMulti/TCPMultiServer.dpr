program TCPMultiServer;
{$APPTYPE CONSOLE}
{%File 'TCPMultiServer.txt'}

uses
  SysUtils,
  Classes,
  SDL,
  SDL_Net,
  TCPUtils,
  Logger,
  ClientUtils;

var
  Port : UInt16;
  IP : TIPAddress;
  IpAddr : UInt32;
  Server, Socket : PTCPSocket;
  SocketSet : PSDLNet_SocketSet;
  Host : string;
  NumReady, Index : Integer;
  Clients : TClientList;
  Name, Msg : string;
  Done : Boolean = False;
  kMaxClients : Integer = 10;


procedure PostMessage( Buffer : string );
var
  index : Integer;
begin
  for index := 0 to Clients.Count - 1 do
  begin
    if Clients.Socket[ index ] <> nil then
      SendMessage( Clients.Socket[ index ], Buffer );
  end;
end;

function FindIndex( Name : string ) : integer;
begin
  Result := Clients.Find( Name );
end;

function Find( Name : string ) : Boolean;
begin
  Result := FindIndex( Name ) > -1;
end;

function AddClient( Socket : PTCPSocket; Name : string ) : boolean;
var
  I : integer;
begin
  Result := False;
  if ( Name = EmptyStr ) or Find( Name ) or ( Clients.Count >= kMaxClients ) then
  begin
    SendMessage( Socket, '#Full' );
    SDLNet_TCP_Close( Socket );
    Result := False;
  end
  else
  begin
    i := Clients.Add( Name, Socket );
    if Clients.Socket[ i ] <> nil then
    begin
      if SDLNet_TCP_AddSocket( SocketSet, Clients.Socket[ i ] ) = -1 then
        Writeln( Format( 'Add Socket Error : %s', [ SDLNet_GetError ] ) )
      else
      begin
        Writeln( 'New Client : ' + Name );
        PostMessage( 'Welcome ' + Name );
        Result := True;
      end;
    end;
  end;
end;

procedure RemoveClient( Index : Integer );
var
  N : string;
begin
  N := Clients.Name[ index ];
  if SDLNet_TCP_DelSocket( SocketSet, Clients.Socket[ index ] ) = -1 then
    Writeln( 'Error removing Socket' );
  Clients.Remove( Index );
  PostMessage( 'Logged out : ' + N );
  Writeln( 'Client Logged out : ' + N );
end;


begin
  // Insert user code here
  if SDL_Init( 0 ) = 0 then
  begin
    try
      if SDLNet_Init = 0 then
      begin
        try
          if ParamCount < 1 then
          begin
            Writeln( Format( 'Usage  %s Port [numclients]', [ ParamStr( 0 ) ] ) );
            Exit;
          end;
          Port := StrToInt( ParamStr( 1 ) );
          if ParamCount = 2 then
            kMaxClients := StrToInt( ParamStr( 2 ) );
          if SDLNet_ResolveHost( IP, nil, Port ) = 0 then
          begin
            ipaddr := SDL_Swap32( IP.Host );
            Writeln( 'IP Address : ' + IPToString( IP ) );
            Host := SDLNet_ResolveIP( IP );
            if host <> EmptyStr then
              Writeln( Format( 'Hostname   : %s ', [ host ] ) )
            else
              Writeln( Format( 'No Hostname found', [ ] ) );
            Writeln( Format( 'Maximum Number of Clients : %d', [ kMaxClients ] ) );
            Server := SDLNet_TCP_Open( IP );
            Clients := TClientList.Create;
            try
              if Server <> nil then
              begin
                SocketSet := CreateTCPSocketSet( kMaxClients + 1, Server );
                try
                  while not Done do
                  begin
                    NumReady := SDLNet_CheckSockets( SocketSet, -1 );
                    if NumReady > 0 then
                    begin
                      if SDLNet_SocketReady( PSDLNet_GenericSocket( Server ) ) then
                      begin
                        dec( NumReady );
                        Socket := SDLNet_TCP_Accept( Server );
                        if Socket <> nil then
                        begin
                          if ReadMessage( Socket, Name ) then
                          begin
                            AddClient( Socket, Name );
                          end
                          else
                          begin
                            SDLNet_TCP_Close( Socket );
                          end;
                        end;
                      end;
                      for index := 0 to Clients.Count - 1 do
                      begin
                        if SDLNet_SocketReady( PSDLNet_GenericSocket( Clients.Socket[ index ] ) ) then
                        begin
                          // got a message from client
                          if ReadMessage( Clients.Socket[ index ], Msg ) then
                          begin
                            if Msg[ 1 ] = '/' then
                            begin
                              // execute a command of some sort
                              if Msg = '/Q' then
                              begin
                                RemoveClient( index );
                              end;
                            end
                            else
                            begin
                              PostMessage( '<' + Clients.Name[ index ] + '> ' + Msg );
                            end;
                          end
                          else
                          begin
                            RemoveClient( Index );
                          end;
                        end;
                      end;
                    end;
                  end;
                finally
                  SDLNet_FreeSocketSet( SocketSet );
                end;
              end;
            finally
              Clients.Free;
            end;
          end;
        finally
          SDLNet_Quit;
        end;
      end;
    finally
      SDL_Quit;
    end;
  end;
  Readln;
end.
