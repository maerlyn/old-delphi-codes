program TCPTimeSyncClient;
{$APPTYPE CONSOLE}
{%File 'TimeSyncClient.txt'}

uses
  SysUtils,
  SDL,
  SDL_Net,
  TCPUtils;

var
  Ip : TIPAddress;
  HostName : string;
  Socket : PTCPSocket;
  Port : UInt16;
  base, now : Sint32;
  Lag : Single;
  SyncPacket : TSync_Packet;

  Done : Boolean = False;
  PacketDone : Boolean = false;
  simlag : Integer = 0; {+ or -}
  last : UInt32 = 0;

begin
  if SDL_Init( 0 ) = 0 then
  begin
    try
      if SDLNet_Init = 0 then
      begin
        try
          if ParamCount > 2 then
          begin
            Port := StrToInt( ParamStr( 2 ) );
            HostName := ParamStr( 1 );
            if ParamCount > 3 then
              simlag := StrToInt( ParamStr( 3 ) );
            if SDLNet_ResolveHost( Ip, PChar( HostName ), Port ) = 0 then
            begin
              Writeln( 'Connecting...' );
              Socket := SDLNet_TCP_Open( IP );
              if Socket <> nil then
              begin
                try
                  Writeln( 'Connected..' );
                  while not Done do
                  begin
                    last := now;
                    recvSyncPacket( Socket^, @SyncPacket );
                    if simlag > 0 then SDL_Delay( simlag );
                    now := SDL_GEtTicks;
                    lag := ( now - last ) / 2.0;
                    printSyncPacket( @SyncPacket );
                    if simlag > 0 then SDL_Delay( simlag );
                    case SyncPacket.wtype of
                      0 :
                        begin
                          base := now - SyncPacket.data.u.s;
                          SyncPacket.data.u.c := now;
                          sendSyncPacket( Socket^, @SyncPacket );
                        end;
                      1 :
                        begin
                          base := base - Trunc( lag );
                          SyncPacket.data.u.c := SDL_GetTicks - base;
                          SyncPacket.data.s.s := ( Sint32( now - base - SyncPacket.data.s.s ) ) div 2;
                          sendSyncPacket( Socket^, @SyncPacket );
                        end;
                      2 :
                        begin
                          base := base - SyncPacket.data.s.c;
                          SyncPacket.data.u.c := SDL_GetTicks - base;
                          SyncPacket.data.s.s := ( Round( now - base - SyncPacket.data.s.s + lag ) ) div 2;
                          sendSyncPacket( Socket^, @SyncPacket );
                        end;
                      3 :
                        begin
                          Writeln( Format( 'Wait Until %u', [ SyncPacket.data.u.s ] ) );
{$IFDEF DO_BUSY}
                          while SDL_GetTicks < SyncPacket.data.u.s do
                            ;
{$ELSE}
                          SDL_Delay( SyncPacket.data.u.s - ( SDL_GetTicks - base ) );
{$ENDIF}
                          Writeln( Format( 'TIME = %u', [ SDL_GetTicks - base ] ) );
                          Done := True;
                        end;
                    else
                      Done := True;
                    end;
                    Writeln( Format( 'now-base=%u (lag=%.2f)', [ now - base, lag ] ) );
                    SyncPacket.wType := 10;
                  end;
                finally
                  SDLNet_TCP_Close( Socket );
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
d.
