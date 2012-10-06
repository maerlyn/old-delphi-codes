program FTPUDPServer;
{$APPTYPE CONSOLE}
{%File 'FTPUDPServer.txt'}

uses
  SysUtils,
  Classes,
  SDL,
  SDL_Net,
  TCPUtils;

const
  kRequest = 1 shl 4;
  kStart = 2 shl 4;
  kAcknowledge = 3 shl 4;
  kData = 2;
  kFinish = 0;

type

  TRequest = packed record
    rtype : byte;
    filename : string;
  end;

type
  TUDPPackets = array[ 0..1024 ] of TUDPPacket;
  PUDPPackets = ^TUDPPackets;

function CreateUDPPackets( var pa : PUDPPackets; Count, Size : integer ) : Boolean;
var
  P : PUDPpacket;
begin
  P := SDLNet_AllocPacketV( Count, Size );
  pa := PUDPPAckets( P );
end;

function SendUDPPackets( Socket : PUDPSocket; var pa : PUDPPackets; npackets : integer ) : Boolean;
var
  P : PUDPpacket;
begin
  P := PUDPPacket( pa );
  Result := SDLNet_UDP_SendV( Socket, @P, npackets ) = npackets;
end;

function FreeUDPPackets( var pa : PUDPPackets ) : Boolean;
var
  P : PUDPpacket;
begin
  P := PUDPPacket( pa );
  SDLNet_FreePacketV( P );
  pa := nil;
end;

function Min( A, B : integer ) : Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

var
  kLen : UInt32 = 4096;
  Done : Boolean = False;

  Port : UInt16;
  Socket : PUDPSocket;
  pin, pout : PUDPpacket;
  outs : array[ 0..31 ] of TUDPpacket;
  Packets : PUDPPackets;
  IP : TIPAddress;
  host, filename : string;
  ipaddr, currPacket : UInt32;
  Stream : TFileStream;
  Req : TRequest;
  pData : Pointer;
  numPackets, total : integer;

begin
  // Insert user code here
  if ParamCount < 1 then
  begin
    Writeln( Format( '%s port [packet length]', [ ParamStr( 0 ) ] ) );
    Exit;
  end;
  if SDL_Init( 0 ) = 0 then
  begin
    if SDLNet_Init = 0 then
    begin
      Port := StrToInt( ParamStr( 1 ) );
      if ParamCount = 2 then
        kLen := StrToInt( ParamStr( 2 ) );
      Socket := SDLNet_UDP_Open( Port );
      if Socket <> nil then
      begin
        Writeln( Format( 'Port %d opened', [ port ] ) );
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
        CreateUDPPackets( Packets, 1024, kLen );
        if Packets = nil then
        begin
          Writeln( Format( 'SDLNet_AllocPacket: %s', [ SDLNet_GetError ] ) );
          Exit;
        end;
        while not Done do
        begin
          SDLNet_UDP_Unbind( Socket, 0 );
          TDataArray( pin.data )[ 0 ] := 0;
          Writeln( 'Waiting....' );
          while SDLNet_UDP_Recv( Socket, pin ) = 0 do
          begin
            SDL_Delay( 100 );
          end;
          if TDataArray( pin.data )[ 0 ] <> kRequest then
          begin
            TDataArray( pin.data )[ 0 ] := kError;
            pin.len := -1;
            SDLNet_UDP_Send( Socket, -1, pin );
            Continue;
          end;
          IP := pin.address;
          host := SDLNet_ResolveIP( IP );
          ipaddr := SDL_Swap32( ip.host );
          port := SDL_Swap32( ip.port );
          if host <> EmptyStr then
            Writeln( Format( 'request from host=%s port=%d', [ host, port ] ) )
          else
          begin
            Writeln( Format( 'request from host=%s port=%d', [ IPToString( IP ), port ] ) )
          end;
          if SDLNet_UDP_Bind( Socket, 0, IP ) = -1 then
          begin
            //error
          end;
          Filename := StrPas( @TDataArray( pin.data )[ 1 ] );
          Writeln( Format( 'File Requested = %s', [ Filename ] ) );
          if FileExists( Filename ) then
          begin
            if Assigned( Stream ) then FreeAndNil( Stream );
            Stream := TFileStream.Create( Filename, fmOpenRead );
            try
              Writeln( Format( 'sending filesize=%d blocksize=%d', [ Stream.Size, kLen ] ) );
              // alloca
              numPackets := Round( Stream.Size / kLen ) + 1;
              TDataArray( pout.data )[ 0 ] := 1;
              SDLNet_Write32( Stream.Size, @TDataArray( pout.data )[ 1 ] );
              SDLNet_Write32( kLen - 6, @TDataArray( pout.data )[ 5 ] );
              pout.len := 9;
              if not udpsend( Socket, 0, pout, pin, 10, kStart, kTIMEOUT ) then
                Continue;
              if Stream.Size <= 0 then
                Continue;
              for currPacket := 0 to numPackets - 1 do
              begin
                Stream.Position := 0;
                while ( SDLNet_UDP_Recv( Socket, pin ) = 0 ) do
                  ;
                with Packets[ currPacket ], Stream do
                begin
                  channel := 0;
                  TDataArray( data )[ 0 ] := 2;
                  TDataArray( data )[ 1 ] := currPacket;
                  len := Stream.Read( pData, kLen );
                  SDLNet_Write32( Cardinal( pData ), @TDataArray( data )[ 6 ] );
                end;
              end;
              // send the packets
              currPacket := 0;
              Total := 0;
              while total < numPackets do
              begin
                repeat
                  if not SendUDPPackets( Socket, Packets, numPackets ) then
                  begin
                    // error
                  end;
                until udprecv( Socket, pin, 10, kAcknowledge, kTIMEOUT );
                if TDataArray( pin.data )[ 0 ] = kAcknowledge then
                begin
                  currPacket := TDataArray( pin.data )[ 1 ];
                  if Packets[ currPacket ].len <> 0 then
                  begin
                    Packets[ currPacket ].len := 0;
                    Writeln( Format( 'Acknowledged : %d', [ currPacket ] ) );
                    inc( Total );
                  end;
                end;
              end;
              TDataArray( pout.data )[ 0 ] := kData;
              TDataArray( pout.data )[ 1 ] := kFinish;
              pout.len := 0;
              udpsend( Socket, 0, pout, pin, 10, kFinish, kTIMEOUT );
            finally
              FreeAndNil( Stream );
            end;
          end;
        end;
        FreeUDPPackets( Packets );
      end;
    end;
  end;
end.
