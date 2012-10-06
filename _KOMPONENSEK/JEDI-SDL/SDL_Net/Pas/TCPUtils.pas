unit TCPUtils;

interface

uses SysUtils,
  SDL,
  SDL_Net;

const
  kERROR = $00000FF;
  kTIMEOUT = 50000;

type
  TDataArray = array of UInt8;
  PDataArray = ^TDataArray;

  TPacket = packed record
    s, c : Uint32;
  end;

  TPacketData = packed record
    u, s : TPacket;
  end;

  TSync_Packet = packed record
    data : TPacketData;
    wtype : Word;

  end;
  PSync_Packet = ^TSync_Packet;

procedure sendSyncPacket( var sock : TTCPsocket; sp : PSync_Packet );
procedure recvSyncPacket( var sock : TTCPsocket; sp : PSync_Packet );
procedure printSyncPacket( sp : PSync_Packet );
function IPToString( ip : TIPAddress ) : string;
function CreateTCPSocketSet( NumClients : Integer; var Server : PTCPSocket ) : PSDLNet_SocketSet;
function SendMessage( Socket : PTCPSocket; Name : string ) : boolean;
function ReadMessage( Socket : PTCPSocket; var Data : string ) : boolean;
function udprecv( Socket : PUDPSocket; Packetin : PUDPpacket; delay : UInt32;
  expect : UInt8; timeout : Integer ) : Boolean;
function udpsend( Socket : PUDPSocket; channel : Integer; Packetout, Packetin : PUDPpacket;
  delay : UInt32; expect : UInt8; timeout : Integer ) : Boolean;

implementation

procedure sendSyncPacket( var sock : TTCPsocket; sp : PSync_Packet );
var
  Tmp : array[ 1..8 ] of char;
begin
  SDLNet_TCP_Send( @sock, @sp.wtype, 1 );
  SDLNet_Write32( sp.data.u.s, @tmp[ 1 ] );
  SDLNet_Write32( sp.data.u.c, @tmp[ 4 ] );
  SDLNet_TCP_Send( @sock, @tmp, 8 );
end;

procedure recvSyncPacket( var sock : TTCPsocket; sp : PSync_Packet );
var
  Tmp : array[ 1..8 ] of char;
begin
  SDLNet_TCP_Recv( @sock, @sp.wtype, 1 );
  SDLNet_TCP_Recv( @sock, @tmp, 8 );
  sp.data.u.s := SDLNet_Read32( @tmp[ 1 ] );
  sp.data.u.c := SDLNet_Read32( @tmp[ 4 ] );
end;

procedure printSyncPacket( sp : PSync_Packet );
begin
  Writeln( Format( 'Sync Packet %p', [ sp ] ) );
  Writeln( Format( '\stype %d', [ sp^.wtype ] ) );
  Writeln( Format( '\data.u.s %u', [ sp^.data.u.s ] ) );
  Writeln( Format( '\data.u.c %u', [ sp^.data.u.c ] ) );
  Writeln( Format( '\data.s.s %u', [ sp^.data.s.s ] ) );
  Writeln( Format( '\data.s.c %u', [ sp^.data.s.c ] ) );
end;

function IPToString( ip : TIPAddress ) : string;
var
  ipaddr : UInt32;
begin
  ipaddr := SDL_Swap32( ip.host );
  // output the IP address nicely
  Result := format( '%d.%d.%d.%d', [ ipaddr shr 24, ( ipaddr shr 16 ) and $000000FF,
    ( ipaddr shr 8 ) and $000000FF, ipaddr and $000000FF ] );
end;

function CreateTCPSocketSet( NumClients : Integer; var Server : PTCPSocket ) : PSDLNet_SocketSet;
begin
  Result := SDLNet_AllocSocketSet( NumClients );
  if Result <> nil then
  begin
    if SDLNet_TCP_AddSocket( Result, Server ) = -1 then
      Writeln( Format( 'Add Socket Error : %s', [ SDLNet_GetError ] ) );
  end
  else
    Writeln( Format( 'Create Socket Set Error : %s', [ SDLNet_GetError ] ) );
end;

function SendMessage( Socket : PTCPSocket; Name : string ) : boolean;
var
  i, Res : UInt32;
  cdata : array[ 0..255 ] of Char;
begin
  Result := False;
  i := Length( Name );
  i := SDL_Swap32( i );
  Res := SDLNet_TCP_Send( Socket, @i, Sizeof( i ) );
  if Res < Sizeof( i ) then
  begin
    Writeln( Format( 'Error : %s', [ SDLNet_GetError ] ) );
    exit;
  end;
  StrPCopy( cData, Name );
  i := SDL_Swap32( i );
  Res := SDLNet_TCP_Send( Socket, @cdata, i );
  if Res < i then
  begin
    Writeln( Format( 'Error : %s', [ SDLNet_GetError ] ) );
    exit;
  end;
  Result := Res > 0;
end;

function ReadMessage( Socket : PTCPSocket; var Data : string ) : boolean;
var
  i, Res : UInt32;
  cdata : array[ 0..255 ] of Char;
begin
  Result := False;
  Res := SDLNet_TCP_Recv( Socket, @i, Sizeof( i ) );
  if Res < Sizeof( i ) then
  begin
    Writeln( Format( 'Error : %s', [ SDLNet_GetError ] ) );
    exit;
  end;
  i := SDL_Swap32( i );
  SetLength( Data, i );
  Res := SDLNet_TCP_Recv( Socket, @cdata, i );
  if Res < i then
  begin
    Writeln( Format( 'Error : %s', [ SDLNet_GetError ] ) );
    exit;
  end;
  StrLCopy( PChar( Data ), cData, i );
  Result := Length( Data ) > 0;
end;

function udpsend( Socket : PUDPSocket; channel : Integer; Packetout, Packetin : PUDPpacket;
  delay : UInt32; expect : UInt8; timeout : Integer ) : Boolean;
var
  t, t2 : UInt32;
  Error : Integer;
  aData : TDataArray;
begin
  aData := TDataArray( Packetin.data );
  aData[ 0 ] := 0;
  t := SDL_GetTicks;
  repeat
    t2 := SDL_GetTicks;
    if ( t2 - t ) > timeout then
    begin
      Writeln( Format( 'Timed Out', [ ] ) );
      Break;
    end;
    if SDLNet_UDP_Send( Socket, channel, Packetout ) = 0 then
    begin
      Writeln( Format( 'SDLNet_UDP_Send: %s', [ SDLNet_GetError ] ) );
      Break;
    end;
    Error := SDLNet_UDP_Recv( Socket, Packetin );
    if ( Error = 0 ) then SDL_Delay( delay );
  until ( Error <> 0 ) or ( ( aData[ 0 ] = Expect ) and ( aData[ 0 ] = kERROR ) );
  if ( aData[ 0 ] = kERROR ) then
    Writeln( Format( 'Returned Error Code', [ ] ) );
  if aData[ 0 ] = kError then
    Result := False
  else
    Result := True;
end;

function udprecv( Socket : PUDPSocket; Packetin : PUDPpacket; delay : UInt32;
  expect : UInt8; timeout : Integer ) : Boolean;
var
  t, t2 : UInt32;
  Error : Integer;
  aData : TDataArray;
begin
  aData := TDataArray( Packetin.Data );
  aData[ 0 ] := 0;
  t := SDL_GetTicks;
  repeat
    t2 := SDL_GetTicks;
    if ( t2 - t ) > timeout then
    begin
      Writeln( Format( 'Timed Out', [ ] ) );
      Break;
    end;
    Error := SDLNet_UDP_Recv( Socket, Packetin );
    if ( Error = 0 ) then SDL_Delay( delay );
  until ( Error <> 0 ) or ( ( aData[ 0 ] = Expect ) and ( aData[ 0 ] = kERROR ) );
  if ( aData[ 0 ] = kERROR ) then
    Writeln( Format( 'Returned Error Code', [ ] ) );
  if aData[ 0 ] = kError then
    Result := False
  else
    Result := True;
end;

end.

