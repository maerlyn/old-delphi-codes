program TCPMultiClient;
{$APPTYPE CONSOLE}
{%File 'TCPMultiClient.txt'}

uses
  SysUtils,
  SDL,
  SDL_Net,
  TCPUtils;

const
  kMaxLength = ( 1 * 1024 * 1024 );

var
  Port : UInt16;
  IP : TIPAddress;
  IpAddr : UInt32;
  Server, Socket : PTCPSocket;
  SocketSet : PSDLNet_SocketSet;
  Host : string;
  NumReady, Index : Integer;
  Name, Data, Text : string;
  Chr : char;
  Event : TSDL_Event;
  Done : Boolean = False;

begin
  if SDL_Init( 0 ) = 0 then
  begin
    try
      if SDLNet_Init = 0 then
      begin
        try
          if ParamCount < 3 then
          begin
            Writeln( Format( 'Usage %s Host Port Username', [ ParamStr( 0 ) ] ) );
            Exit;
          end;
          Name := ParamStr( 3 );
          SocketSet := SDLNet_AllocSocketSet( 1 );
          if SocketSet <> nil then
          begin
            try
              Port := StrToInt( ParamStr( 2 ) );
              Writeln( Format( 'Connecting to %s Port %d', [ ParamStr( 1 ), Port ] ) );
              if SDLNet_ResolveHost( IP, PChar( ParamStr( 1 ) ), Port ) = 0 then
              begin
                ipaddr := SDL_Swap32( IP.Host );
                Writeln( 'IP Address : ' + IPToString( IP ) );
                Host := SDLNet_ResolveIP( IP );
                if host <> EmptyStr then
                  Writeln( Format( 'Hostname   : %s ', [ host ] ) )
                else
                  Writeln( Format( 'No Hostname found', [ ] ) );
                Server := SDLNet_TCP_Open( IP );
                if Server <> nil then
                begin
                  if SDLNet_TCP_AddSocket( SocketSet, Server ) <> -1 then
                  begin;
                    if SendMessage( Server, Name ) then
                    begin
                      Writeln( Format( 'Logged in as %s', [ Name ] ) );
                      while not Done do
                      begin
                        // get txt from user and send it
                        NumReady := SDLNet_CheckSockets( SocketSet, 100 );
                        if ( NumReady > 0 ) and SDLNet_SocketReady( PSDLNet_GenericSocket( Server ) ) then
                        begin
                          if ReadMessage( Server, Data ) then
                          begin
                            Writeln( Format( '%s', [ Data ] ) );
                          end;
                        end;
                        // read text and send
                        Read( chr );
                        if Chr = #13 then
                        begin
                          Read( chr ); // read line feed
                          SendMessage( Server, Text );
                          if Text = '/Q' then
                            Done := True
                          else
                            Text := EmptyStr;
                        end
                        else
                          Text := Text + Chr;
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
          SDLNet_Quit;
        end;
      end;
    finally
      SDL_Quit;
    end;
  end;
end.
