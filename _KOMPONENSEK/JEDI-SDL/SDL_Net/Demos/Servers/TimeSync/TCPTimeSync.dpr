program TCPTimeSync;
{$APPTYPE CONSOLE}
{%File 'CCode.txt'}
{$DEFINE DO_BUSY}

uses
  SysUtils,
  SDL,
  SDL_Net,
  TCPUtils;

var

  Ip : TIPAddress;
  RemoteIP : PIPAddress;
  Server, Client : PTCPSocket;
  Port : UInt16;
  ipaddr, now, last : Uint32;
  Lag : Single;
  SyncPacket : TSync_Packet;
  Done : Boolean = False;
  PacketDone : Boolean = false;
  minoff : Integer = 5; {+ or -}

begin
  // Insert user code here
  if SDL_Init( 0 ) = 0 then
  begin
    try
      if SDLNet_Init = 0 then
      begin
        try
          if ParamCount = 2 then
          begin
            Port := StrToInt( ParamStr( 1 ) );
            minoff := StrToInt( ParamStr( 2 ) );

            if SDLNet_ResolveHost( Ip, nil, Port ) = 0 then
            begin
              Server := SDLNet_TCP_Open( Ip );
              if Server <> nil then
              begin
                try
                  while not Done do
                  begin
                    Client := nil;
                    Writeln( 'Waiting for connections...' );
                    while Client = nil do
                    begin
                      Client := SDLNet_TCP_Accept( Server );
                      if Client = nil then
                        SDL_Delay( 100 );
                    end;
                    RemoteIP := SDLNet_TCP_GetPeerAddress( Client );
                    if RemoteIP <> nil then
                    begin
                      ipaddr := SDL_Swap32( RemoteIP^.host );
                      Writeln( Format( 'Accepted connection from %d.%d.%d.%d port %x',
                        [ ipaddr shr 24,
                        ( ipaddr shr 16 ) and $000000FF,
                          ( ipaddr shr 16 ) and $000000FF,
                          ipaddr and $000000FF,
                          remoteip^.port
                          ] ) );
                      Lag := 0;
                      SyncPacket.wtype := 0;
                      SyncPacket.data.u.s := SDL_GetTicks;
                      sendSyncPacket( Client^, @SyncPacket );
                      now := SDL_GetTicks;
                      while not PacketDone do
                      begin
                        last := now;
                        recvSyncPacket( Client^, @SyncPacket );
                        now := SDL_GetTicks;
                        Lag := ( now - last ) / 2.0;
                        printSyncPacket( @SyncPacket );
                        case SyncPacket.wtype of
                          0 :
                            begin
                              SyncPacket.wtype := 1;
                              SyncPacket.data.u.s := SDL_GetTicks( ) + Round( lag );
                              sendSyncPacket( client^, @SyncPacket );
                            end;
                          1..2 :
                            begin
                              if SyncPacket.wtype = 1 then SyncPacket.wtype := 2;
                              SyncPacket.data.s.c := now - Round( lag ) - SyncPacket.data.u.c;
                              if ( SyncPacket.data.s.c < minoff ) and
                                ( SyncPacket.data.s.c > -minoff ) then
                              begin
                                SyncPacket.wtype := 3;
                                SyncPacket.data.u.s := SDL_GetTicks + Round( Lag ) + Minoff * 100;
                              end
                              else
                              begin
                                SyncPacket.data.u.s := SDL_GetTicks + Round( Lag );
                              end;
                              sendSyncPacket( Client^, @SyncPAcket );
                              if SyncPacket.wtype = 3 then
                              begin
                                Writeln( Format( 'Wait Until %u', [ SyncPacket.data.u.s ] ) );
{$IFDEF DO_BUSY}
                                while SDL_GetTicks < SyncPacket.data.u.s do
                                  ;
{$ELSE}
                                SDL_Delay( SyncPacket.data.u.s - SDL_GetTicks );
{$ENDIF}
                                Writeln( Format( 'TIME = %u', [ SDL_GetTicks ] ) );
                                PacketDone := True;
                              end;
                            end;
                        else
                          PacketDone := True;
                        end;
                        SyncPacket.wtype := 10;
                      end;
                    end;
                  end;
                finally
                  SDLNet_TCP_Close( server );
                end;
              end;
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

