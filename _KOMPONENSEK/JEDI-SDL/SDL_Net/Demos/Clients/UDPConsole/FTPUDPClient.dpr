program FTPUDPClient;
{$APPTYPE CONSOLE}
{%File 'FTPUDPClient.txt'}

uses
  SysUtils,
  Classes,
  SDL,
  SDL_Net,
  TCPUtils;

const
  kERROR = $00000FF;
  kTIMEOUT = 5000;

  kRequest = 1 shl 4;
  kStart = 2 shl 4;
  kAcknowledge = 3 shl 4;
  kData = 2;
  kFinish = 0;

type
  TDataArray = array of UInt8;
  PDataArray = ^TDataArray;

var
  Port : UInt16;
  Socket : PUDPSocket;
  pin, pout : PUDPpacket;
  outs : array[ 0..31 ] of TUDPpacket;
  Packets : PUDPPacket;
  IP : TIPAddress;
  host, filename : string;
  ipaddr, filesize, blocksize, len : UInt32;
  Stream : TFileStream;
  FilePos, PacketIndex : integer;
  pData : Pointer;
  GotPacket : Boolean;
  Done : Boolean = False;

begin
  // Insert user code here
  if ParamCount < 3 then
  begin
    Writeln( Format( '%s host port file', [ ParamStr( 0 ) ] ) );
    Exit;
  end;
  if SDL_Init( 0 ) = 0 then
  begin
    if SDLNet_Init = 0 then
    begin
      host := ParamStr( 1 );
      Port := StrToInt( ParamStr( 2 ) );
      filename := ParamStr( 3 );
      if SDLNet_ResolveHost( IP, PChar( host ), Port ) = 0 then
      begin
        Socket := SDLNet_UDP_Open( 0 );
        try
          pout := SDLNet_AllocPacket( 65535 );
          if pout = nil then
          begin
            Writeln( Format( 'SDLNet_AllocPacket: %s', [ SDLNet_GetError ] ) );
            Exit;
          end;
          pin := SDLNet_AllocPacket( 65535 );
          if pin = nil then
          begin
            Writeln( Format( 'SDLNet_AllocPacket: %s', [ SDLNet_GetError ] ) );
            Exit;
          end;
          try
            if SDLNet_UDP_Bind( Socket, 0, IP ) = 0 then
            begin
              // port bound
              Writeln( Format( 'requesting %s', [ filename ] ) );
              TDataArray( pout.data )[ 0 ] := kRequest;
              StrPCopy( PChar( integer( pout.data ) + 1 ), filename );
              pout.len := Length( filename ) + 2;
              if udpsend( Socket, 0, pout, pin, 200, 1, kTIMEOUT ) then
              begin
                filesize := SDLNet_Read32( @TDataArray( pin.data )[ 1 ] );
                len := SDLNet_Read32( @TDataArray( pin.data )[ 5 ] );
                Blocksize := ( filesize + len + 1 ) div len;
                Writeln( Format( 'FileSize = %d BlockSize = %d Length = %d', [ filesize, Blocksize, len ] ) );
                Writeln( 'starting Transfere...' );
                TDataArray( pout.data )[ 0 ] := kStart;
                pout.len := 1;
                if udpsend( Socket, 0, pout, pin, 10, kData, kTIMEOUT ) then
                begin
                  if filesize > 0 then
                  begin
                    Stream := TFileStream.Create( ExtractFileName( Filename ), fmCreate );
                    try
                      while not Done do
                      begin
                        GotPacket := udprecv( Socket, pin, 10, kData, kTIMEOUT );
                        if GotPacket then
                        begin
                          if TDataArray( pin.data )[ 0 ] = kData then
                          begin
                            PacketIndex := TDataArray( pin.data )[ 1 ];
                            FilePos := SDLNet_Read32( Pointer( integer( pin.data ) + 2 ) );
                            pData := Pointer( integer( pin.data ) + 6 );
                            Stream.Position := FilePos;
                            Stream.Write( pData, pin.len - 6 );
                          end;
                          // got a data packet need to place it in the file
                          if ( TDataArray( pin.data )[ 1 ] = kFinish ) then
                          begin
                            Done := True;
                            TDataArray( pout.data )[ 0 ] := kFinish;
                            SDLNet_UDP_Send( Socket, 0, pout );
                          end
                          else
                          begin
                            TDataArray( pout.data )[ 0 ] := kAcknowledge;
                            SDLNet_Write32( PacketIndex, Pointer( integer( pout.data ) + 1 ) );
                            SDLNet_Write32( FilePos, Pointer( integer( pout.data ) + 5 ) );
                            pout.len := 9;
                            SDLNet_UDP_Send( Socket, 0, pout );
                          end;
                        end;
                      end;
                    finally
                      FreeAndNil( Stream );
                    end;
                  end;
                end;
              end;
            end;
          finally
            SDLNet_FreePacket( pin );
            SDLNet_FreePacket( pout );
          end;
        finally
          SDLNet_UDP_Close( Socket );
        end;
      end;
    end;
    SDLNet_Quit;
  end;
  SDL_Quit;
  Readln;
end.
d.
