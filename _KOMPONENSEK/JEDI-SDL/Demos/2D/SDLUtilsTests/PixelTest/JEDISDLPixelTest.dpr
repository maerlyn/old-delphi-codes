program JEDISDLPixelTest;

uses
  SDL,
  SDLUtils,
  SysUtils;

const
  TICK_INTERVAL = trunc( 1000 / 30 );

var
  Screen, FigureSurface : PSDL_Surface;
  Next_Time : cardinal;
  SrcRect : TSDL_Rect;
  Figures : array[ 0..15 ] of record
    x, y : integer;
    SrcRect : TSDL_Rect;
  end;
  x, y : integer;
  video_flags : Cardinal;

function TimeLeft : UInt32;
var
  now : cardinal;
begin
  now := SDL_GetTicks;
  if next_time <= now then
  begin
    next_time := now + TICK_INTERVAL;
    result := 0;
    exit;
  end;
  result := next_time - now;
end;

function CollisionTest( _x, _y : integer ) : boolean;
var
  i : integer;
begin
  for i := 0 to 15 do
    if SDL_PixelTest( FigureSurface, @SrcRect, FigureSurface, @Figures[ i ].SrcRect, _x, _y, Figures[ i ].x, Figures[ i ].y ) then
    begin
      Result := true;
      exit;
    end;
  Result := false;
end;

procedure Test;
var
  Buffer : PSDL_Surface;
  keys : PKeyStateArr;
  Event : TSDL_Event;
  DestRect : TSDL_Rect;
  Quit : boolean;
  i : integer;
  px, py : integer;
begin
  // Load the image of figures
  FigureSurface := SDL_LoadBMP( 'Images/Figures.bmp' );
  FigureSurface := SDL_DisplayFormat( FigureSurface );
  SDL_SetColorKey( FigureSurface, SDL_SRCCOLORKEY, SDL_MapRGB( FigureSurface.format, 255, 0, 255 ) );
  Buffer := SDL_CreateRGBSurface( SDL_SWSURFACE, 640, 480, 8, 0, 0, 0, 0 );
  Buffer := SDL_DisplayFormat( Buffer );
  for py := 0 to 479 do
    for px := 0 to 639 do
      SDL_PutPixel( Buffer, px, py, SDL_MapRGB( Buffer.format, trunc( px * ( 256 / 640 ) ), trunc( py * 256 / 480 ), 0 ) );
  // Circle
  x := 0;
  y := 0;
  SrcRect := SDLRect( 12, 12, 8, 8 );
  for i := 0 to 15 do
  begin
    Figures[ i ].x := Random( 600 );
    Figures[ i ].y := random( 420 );
    case random( 5 ) of
      0 : Figures[ i ].SrcRect := SDLRect( 0, 0, 32, 32 );
      1 : Figures[ i ].SrcRect := SDLRect( 32, 0, 32, 32 );
      2 : Figures[ i ].SrcRect := SDLRect( 12, 12, 8, 8 );
      3 : Figures[ i ].SrcRect := SDLRect( 44, 16, 8, 8 );
      4 : Figures[ i ].SrcRect := SDLRect( 64, 0, 26, 32 );
    end;
  end;
  Quit := false;
  repeat
    SDL_BlitSurface( Buffer, nil, Screen, nil );
    // Draw all other figures
    for i := 0 to 15 do
    begin
      DestRect.x := Figures[ i ].x;
      DestRect.y := Figures[ i ].y;
      SDL_BlitSurface( FigureSurface, @Figures[ i ].SrcRect, Screen, @DestRect );
    end;
    // Draw Circle
    DestRect := SDLRect( x, y, 32, 32 );
    SDL_BlitSurface( FigureSurface, @SrcRect, Screen, @DestRect );
    while SDL_PollEvent( @Event ) > 0 do
    begin
      case Event.key.type_ of
        SDL_QUITEV :
          begin
            Quit := True;
          end;

        SDL_KeyDown :
          begin
            case Event.key.keysym.sym of
              SDLK_ESCAPE :
                Quit := true;
            end;
          end;
      end;
    end;
    keys := PKeyStateArr( SDL_GetKeyState( nil ) );
    if Keys[ SDLK_UP ] = 1 then
    begin
      if y > 0 then
        if not CollisionTest( x, y - 1 ) then dec( y );
    end
    else if Keys[ SDLK_DOWN ] = 1 then
    begin
      if y + 8 < Screen.h then
        if not CollisionTest( x, y + 1 ) then inc( y );
    end;
    if Keys[ SDLK_LEFT ] = 1 then
    begin
      if x > 0 then
        if not CollisionTest( x - 1, y ) then dec( x );
    end
    else if Keys[ SDLK_RIGHT ] = 1 then
    begin
      if x + 8 < Screen.w then
        if not CollisionTest( x + 1, y ) then inc( x );
    end;
    SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    SDL_Delay( TimeLeft );
  until Quit = true;
  SDL_FreeSurface( FigureSurface );
  SDL_FreeSurface( Buffer );
end;

begin
  randomize;
  SDL_Init( SDL_INIT_VIDEO );
  SDL_WM_SETCAPTION( 'Move the ball with cursor keys, ESC to quit', nil );
  // fire and forget...
  if ( ParamStr( 1 ) = '-fullscreen' ) or ( ParamStr( 1 ) = '-fs' ) then
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
  else
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE;
  Screen := SDL_SetVideoMode( 640, 480, 32, video_flags );
  Test;
  SDL_Quit;
end.

