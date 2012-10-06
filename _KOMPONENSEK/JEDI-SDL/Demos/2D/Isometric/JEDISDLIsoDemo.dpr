program JEDISDLIsoDemo;

uses
  SysUtils,
  Classes,
  Logger,
  SDL;

const
  WINDOW_TITLE = 'JEDI-SDL Isometric Demo';
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

type
  TMapTile = record
    Num : Byte;                     // number of tiles at one coordinate (allows stacking of tiles)
    Tile : array[ 0..9 ] of Byte;   // type of tile (0=grass, 1=path, etc.)
    Height : array[ 0..9 ] of Byte; // how tall the tile reaches
    Layer : array[ 0..9 ] of Byte;  // used to make sure the sprites doesn't block.
    Walkable : boolean;             // Whether the tile at this location is walkable
  end;

  TMap = array[0..31, 0..31] of TMapTile;
  
  TImageList = array[0..16] of PSDL_Surface;

var
  MouseX : Integer = 320;      //Mouse position for cursor
  MouseY : Integer = 240;      //Mouse position for cursor
  Map : TMap;
  viewx : Smallint = 0;
  viewy: Smallint = 0;
  sprite_x: Smallint = 0;
  player_x: Smallint = 320;
  sprite_y: Smallint = 0;
  player_y: Smallint = 240;
  xvel: Smallint = 1;
  yvel : Smallint = 2;
  sprite_layer, sprite_frame: Byte;
  player_frame : Byte = 0;
  scroll_speed : Byte = 2;
  ScrollEdge : Byte = 20;
  ImageList : TImageList;
  Screen : PSDL_Surface;
  flags: UInt32;

procedure TerminateApplication;
var
  i : integer;
begin
  // Free Surfaces
  for i := Low(ImageList) to High(ImageList) do
    SDL_FreeSurface( ImageList[i] );
  SDL_QUIT;
  Halt(0);
end;

function LoadImage( Filename: string ) : PSDL_SURFACE;
var
  image: PSDL_SURFACE;
begin
  //image := nil;
  image := SDL_LoadBMP( PChar( FileName ) );
  if image <> nil then
    Result := image
  else
  begin
    Result := nil;
    Log.LogError( Format( 'Unable to Load Image : %s', [SDL_GetError]
      ),
      'LoadImage' );
  end;
end; // LoadImage

procedure Loadimages;
begin
  ImageList[0]  := LoadImage( 'images/grass.bmp' );
  SDL_SetColorKey(ImageList[0], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[0].format, 0, 0, 0));

  ImageList[1]  := LoadImage( 'images/dirtpath.bmp' );
  SDL_SetColorKey(ImageList[1], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[1].format, 0, 0, 0));

  ImageList[2]  := LoadImage( 'images/water.bmp' );
  SDL_SetColorKey(ImageList[2], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[2].format, 0, 0, 0));

  ImageList[3]  := LoadImage( 'images/leftp.bmp' );
  SDL_SetColorKey(ImageList[3], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[3].format, 0, 0, 0));

  ImageList[4]  := LoadImage( 'images/rightp.bmp' );
  SDL_SetColorKey(ImageList[4], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[4].format, 0, 0, 0));

  ImageList[5]  := LoadImage( 'images/brown.bmp' );
  SDL_SetColorKey(ImageList[5], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[5].format, 0, 0, 0));

  ImageList[6]  := LoadImage( 'images/lefts.bmp' );
  SDL_SetColorKey(ImageList[6], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[6].format, 0, 0, 0));

  ImageList[7]  := LoadImage( 'images/rights.bmp' );
  SDL_SetColorKey(ImageList[7], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[7].format, 0, 0, 0));

  ImageList[8]  := LoadImage( 'images/middles.bmp' );
  SDL_SetColorKey(ImageList[8], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[8].format, 0, 0, 0));

  ImageList[9]  := LoadImage( 'images/gray.bmp' );
  SDL_SetColorKey(ImageList[9], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[9].format, 0, 0, 0));

  ImageList[10] := LoadImage( 'images/leftdoor.bmp' );
  SDL_SetColorKey(ImageList[10], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[10].format, 0, 0, 0));

  ImageList[11] := LoadImage( 'images/rightdor.bmp' );
  SDL_SetColorKey(ImageList[11], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[11].format, 0, 0, 0));

  ImageList[12] := LoadImage( 'images/lefttwst.bmp' );
  SDL_SetColorKey(ImageList[12], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[12].format, 0, 0, 0));

  ImageList[13] := LoadImage( 'images/righttwst.bmp' );
  SDL_SetColorKey(ImageList[13], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[13].format, 0, 0, 0));

  ImageList[14] := LoadImage( 'images/cursor.bmp' );
  SDL_SetColorKey(ImageList[14], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[14].format, 0, 0, 0));

  ImageList[15] := LoadImage( 'images/plant.bmp' );
  SDL_SetColorKey(ImageList[15], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[15].format, 0, 0, 0));

  ImageList[16] := LoadImage( 'images/pine.bmp' );
  SDL_SetColorKey(ImageList[16], SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(ImageList[16].format, 0, 0, 0));
end;

procedure SetupMap( FileName : string );
var
  FileStream : TFileStream;
  C : Char;
  x, y : Word;
  tempbuffer : array[ 0..31, 0..31 ] of Char;
begin
  FileStream := TFileStream.Create( FileName, fmOpenRead );
  try
    // Do things with FileStream
    try
      for y := 0 to 31 do
      begin
        for x := 0 to 31 do
        begin
          FileStream.ReadBuffer( C, SizeOf( C ) );
          tempbuffer[ y ][ x ] := C;
        end;
      end;
    except on E: Exception do
      Log.LogError( E.Message, 'SetupMap' );
    end;
  finally
    FileStream.Free;
  end;

  // Transform Map into Map Structure
  for y := 0 to 31 do
  begin
    for x := 0 to 31 do
    begin
      case tempbuffer[ y ][ x ] of
        '0' :  // 0 -> Grass
        begin
          Map[ y ][ x ].Num := 1;
          Map[ y ][ x ].Tile[0] := 0;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 0;
          Map[ y ][ x ].Walkable := true;
        end;

        '1' : // 1 -> Path
        begin
          Map[ y ][ x ].Num := 1;
          Map[ y ][ x ].Tile[ 0 ] := 1;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 0;
          Map[ y ][ x ].Walkable := true;
        end;

        '2' : // 2 -> Water
        begin
          Map[ y ][ x ].Num := 1;
          Map[ y ][ x ].Tile[ 0 ] := 2;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 0;
          Map[ y ][ x ].Walkable := false;
        end;

        '3' : // 3 -> Builing Corner
        begin
          Map[ y ][ x ].Num := 3;
          Map[ y ][ x ].Tile[ 0 ] := 3;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 1;
          Map[ y ][ x ].Tile[ 1 ] := 4;
          Map[ y ][ x ].Height[ 1 ] := 0;
          Map[ y ][ x ].Layer[ 1 ] := 1;
          Map[ y ][ x ].Tile[ 2 ] := 5;
          Map[ y ][ x ].Height[ 2 ] := 8;
          Map[ y ][ x ].Layer[ 2 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        '4' : // 4 -> Builing Wall and Roof
        begin
          Map[ y ][ x ].Num := 2;
          Map[ y ][ x ].Tile[ 0 ] := 3;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 1;
          Map[ y ][ x ].Tile[ 1 ] := 5;
          Map[ y ][ x ].Height[ 1 ] := 8;
          Map[ y ][ x ].Layer[ 1 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        '5' : // 5 -> Builing Wall and roof
        begin
          Map[ y ][ x ].Num := 2;
          Map[ y ][ x ].Tile[ 0 ] := 4;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 1;
          Map[ y ][ x ].Tile[ 1 ] := 5;
          Map[ y ][ x ].Height[ 1 ] := 8;
          Map[ y ][ x ].Layer[ 1 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        '6' : // 6 -> Builing roof
        begin
          Map[ y ][ x ].Num := 1;
          Map[ y ][ x ].Tile[ 0 ] := 5;
          Map[ y ][ x ].Height[ 0 ] := 8;
          Map[ y ][ x ].Layer[ 0 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        '7' : // 7 -> Builing door and stairs
        begin
          Map[ y ][ x ].Num := 3;
          Map[ y ][ x ].Tile[ 0 ] := 3;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 1;
          Map[ y ][ x ].Tile[ 1 ] := 10;
          Map[ y ][ x ].Height[ 1 ] := 0;
          Map[ y ][ x ].Layer[ 1 ] := 1;
          Map[ y ][ x ].Tile[ 2 ] := 5;
          Map[ y ][ x ].Height[ 2 ] := 8;
          Map[ y ][ x ].Layer[ 2 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        '8' : // 8 -> Builing door and roof
        begin
          Map[ y ][ x ].Num := 3;
          Map[ y ][ x ].Tile[ 0 ] := 4;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 1;
          Map[ y ][ x ].Tile[ 1 ] := 11;
          Map[ y ][ x ].Height[ 1 ] := 0;
          Map[ y ][ x ].Layer[ 1 ] := 1;
          Map[ y ][ x ].Tile[ 2 ] := 5;
          Map[ y ][ x ].Height[ 2 ] := 8;
          Map[ y ][ x ].Layer[ 2 ] := 2;
          Map[ y ][ x ].Walkable := false;
        end;

        'A' : // A -> Plant
        begin
          Map[ y ][ x ].Num := 2;
          Map[ y ][ x ].Tile[ 0 ] := 0;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 0;
          Map[ y ][ x ].Tile[ 1 ] := 15;
          Map[ y ][ x ].Height[ 1 ] := 28;
          Map[ y ][ x ].Layer[ 1 ] := 1;
          Map[ y ][ x ].Walkable := false;
        end;

        'B' : // B -> Palm Tree
        begin
          Map[ y ][ x ].Num := 2;
          Map[ y ][ x ].Tile[ 0 ] := 0;
          Map[ y ][ x ].Height[ 0 ] := 0;
          Map[ y ][ x ].Layer[ 0 ] := 0;
          Map[ y ][ x ].Tile[ 1 ] := 16;
          Map[ y ][ x ].Height[ 1 ] := 61;
          Map[ y ][ x ].Layer[ 1 ] := 1;
          Map[ y ][ x ].Walkable := false;
        end;
      end;
    end;
  end;
end;

procedure DrawIsoMap( x : Word ; y : Word );
var
  i, j, k : Word;
  current_layer, highest_layer, next_layer, last_layer : Byte;
  mapx, mapy, mx, my, tx, ty, xo, yo, xa, ya : SmallInt;
  screenx, screeny : Integer;
  length : Integer;
  tile_to_draw, height_to_draw : Integer;
  sprite_mx, sprite_my, xx, yy : Integer;
  Rect: TSDL_Rect;
begin
  // initialise variables
  current_layer := 0;
  highest_layer := 0;
  next_layer := 0;
  last_layer := 0;

  sprite_mx := sprite_x div 16;
  sprite_my := sprite_y div 16;

  // Convert to fine co-ordinates
  mapx := x div 16;
  xo := x and 15;
  mapy := y div 16;
  yo := y and 15;
  xa := xo - yo;
  ya := ( xo shr 1 ) + ( yo shr 1 );

  while true do
  begin
    next_layer := 255;
    mx := mapx;
    my := mapy;
    screeny := 8 - ya;

    for i := 0 to 69 do // Vertical screen draw
    begin
      tx := mx;
      ty := my;
      screenx := 16 - xa;
      length := 24;

      if i mod 2 <> 0 then // if line is odd
      begin
        inc( length ); // draw extra tile
        dec( screenx, 16 ); // pre-step 16 pixels left
      end;

      for j := 0 to length - 1 do // horizontal screen draw
      begin
        for k := 0 to Map[ ty ][ tx ].Num - 1 do
        begin
          if Map[ ty ][ tx ].layer[ k ] = current_layer then
          begin
            tile_to_draw := Map[ ty ][ tx ].tile[ k ];
            height_to_draw := Map[ ty ][ tx ].height[ k ];
            Rect.x := screenx - 16;
            Rect.y := screeny - height_to_draw - 16;
            SDL_UpperBlit(ImageList[ tile_to_draw ], nil, Screen, @Rect);
          end;
          if ( tx = sprite_mx ) and ( ty = sprite_my ) and (current_layer = sprite_layer) then
          begin
            xo := sprite_x and 15;
            yo := sprite_y and 15;
            xx := xo - yo;
            yy := ( xo shr 1) + ( yo shr 1 );
            Rect.x := screenx - 32 + xx;
// Now the sprite's position is ok, but you have to do something with overlapping!!! -KiCHY 
            Rect.y := screeny - 16 + yy - ImageList[ 12 + sprite_frame ].h; //!!!
            SDL_UpperBlit(ImageList[ 12 + sprite_frame ], nil, Screen, @Rect);
          end;

          if Map[ ty ][ tx ].layer[ k ] > highest_layer then
            highest_layer := Map[ ty ][ tx ].layer[ k ];

          if( Map[ ty ][ tx ].layer[ k ] < next_layer  ) and ( Map[ ty ][ tx ].layer[ k ] > last_layer  ) then
            next_layer := Map[ ty ][ tx ].layer[ k ];
        end;
        inc( screenx, 32 );// Move right 32 pixels for next tile

        inc( tx );
        if tx > 31 then
          tx := 0;

        dec( ty );
        if ty < 0 then
          ty := 31;
      end;

      inc( screeny, 8 );
      if i mod 2 <> 0  then
      begin
        inc( mx );
        if( mx > 31 ) then
          mx := 0;
      end
      else
      begin
        inc( my );
        if( my > 31 ) then
          my := 0;
      end;
    end;
    last_layer := current_layer;
    if current_layer = 0 then
     current_layer := 1
    else
      current_layer := next_layer;
    if current_layer > highest_layer then
      Break;
  end;
end;

// function to handle key press events
procedure HandleKeyPress( keysym : PSDL_keysym );
begin
  case keysym.sym of
    SDLK_ESCAPE :
      // ESC key was pressed
      TerminateApplication;

    SDLK_RETURN :
    begin
      if (keysym.Modifier and KMOD_ALT <> 0) then
      begin
        {* Alt+Enter key was pressed
         * this toggles fullscreen mode
         *}
        SDL_WM_ToggleFullScreen( Screen );
      end;
    end;

    SDLK_LEFT :
    begin
      dec( viewx, scroll_speed );
      inc( viewy, scroll_speed );
    end;

    SDLK_RIGHT :
    begin
      inc( viewx, scroll_speed );
      dec( viewy, scroll_speed );
    end;

    SDLK_UP :
    begin
      dec( viewx, scroll_speed );
      dec( viewy, scroll_speed );
    end;

    SDLK_DOWN :
    begin
      inc( viewx, scroll_speed );
      inc( viewy, scroll_speed );
    end;

    SDLK_KP0 :
    begin
      scroll_speed := 2;
    end;

    SDLK_KP1 :
    begin
      scroll_speed := 4;
    end;

    SDLK_KP2 :
    begin
      scroll_speed := 6;
    end;

    SDLK_KP3 :
    begin
      scroll_speed := 8;
    end;

    SDLK_KP4 :
    begin
      scroll_speed := 10;
    end;

    SDLK_KP5 :
    begin
      scroll_speed := 12;
    end;

    SDLK_KP6 :
    begin
      scroll_speed := 14;
    end;

    SDLK_KP7 :
    begin
      scroll_speed := 16;
    end;

    SDLK_KP8 :
    begin
      scroll_speed := 18;
    end;

    SDLK_KP9 :
    begin
      scroll_speed := 20;
    end;
  end;
end;



var
  Done : Boolean;
  event: TSDL_EVENT;
  Rect : TSDL_Rect;
begin
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Could not initialize SDL: %s',
      [SDL_GetError]), 'Main');
      TerminateApplication;
    exit;
  end;

  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
    flags := SDL_DOUBLEBUF or SDL_SWSURFACE or SDL_FULLSCREEN
  else
    flags := SDL_DOUBLEBUF or SDL_SWSURFACE;

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption( WINDOW_TITLE, nil);

  Screen := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, flags);
  if (Screen = nil) then
  begin
    Log.LogError(Format('Could not set video mode: %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
  end;

  // Hide Mouse Cursor
  SDL_SHOWCURSOR(0);

  SetupMap( 'maps/level1.map' );

  LoadImages;

  Done := False;
  while ( not Done ) do
  begin
    // This could go in a separate function
    while ( SDL_PollEvent( @event ) = 1 ) do
    begin
      case event.type_ of
        SDL_QUITEV :
        begin
          Done := true;
        end;

        SDL_KEYDOWN :
        begin
          // handle key presses
          HandleKeyPress( @event.key.keysym );
        end;

        SDL_MOUSEMOTION :
        begin
          MouseX := event.motion.x;
          MouseY := event.motion.y;
        end;
      end;
    end;
    
    inc(sprite_frame);
    if sprite_frame > 1  then
    begin
      sprite_frame := 0;
    end;

    // Sort out sprite stuff
    Inc(sprite_x, xvel);
    if (sprite_x < 0) or (sprite_x > 512) then
          xvel := -xvel;
    Inc(sprite_y, yvel);
    if (sprite_y < 0) or (sprite_y > 512) then
          yvel := -yvel;

    //Check to see if the Mouse in the ScrollEdge
    if ( MouseX > ( Screen.w - ScrollEdge ) ) then
    begin
      inc( viewx, scroll_speed );
      dec( viewy, scroll_speed );
    end
    else if ( MouseX < ScrollEdge ) then
    begin
      dec( viewx, scroll_speed );
      inc( viewy, scroll_speed );
    end
    else if ( MouseY > ( Screen.H - ScrollEdge ) ) then
    begin
      inc( viewx, scroll_speed );
      inc( viewy, scroll_speed );
    end
    else if ( MouseY < ScrollEdge ) then
    begin
      dec( viewx, scroll_speed );
      dec( viewy, scroll_speed );
    end;

    // Sort out view stuff
    if ( viewx < 0 ) then
      inc( viewx , 512 );
    if ( viewx >= 512 ) then
      dec( viewx , 512 );
    if ( viewy < 0 ) then
      inc( viewy , 512 );
    if ( viewy  >= 512 ) then
      dec( viewy , 512 );
    //Draw the Map image
    DrawIsoMap( viewx, viewy );

    //Draw the cursor at the current mouse position
    Rect.x := MouseX;
    Rect.y := MouseY;
    SDL_UpperBlit(ImageList[ 14 ], nil, Screen, @Rect);

    // Don't forget to flip to the main surface
    SDL_Flip(Screen);
  end;

  TerminateApplication;
end.
