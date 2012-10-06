program SDL_Rotate;

uses
  SysUtils,
  SDL,
  SDLUtils,
  SDL_Image;

var
  Screen_ : PSDL_Surface;
  SourceSurface : PSDL_Surface;
  ErrorMessage : string;
  LoadSurface : PSDL_Surface;

  BlitRect : TSDL_Rect;
  SpriteRect : TSDL_Rect;
  DrawAngle : integer;
begin

  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    ErrorMessage := 'Couldnt initialize SDL: ' + SDL_GetError( );
    Writeln( ErrorMessage );
    SDL_Quit;
    Exit;
  end;

  // Initialize the display
  screen_ := SDL_SetVideoMode( 640, 480, 32, 0 );
  if ( screen_ = nil ) then
  begin
    ErrorMessage := 'Couldnt set 640x480x16 video mode: ' + SDL_GetError( );
    Writeln( ErrorMessage );

    SDL_Quit;
    exit;
  end;

  LoadSurface := IMG_Load( 'images/robotfront.png' );
  SourceSurface := SDL_DisplayFormat( loadsurface );

  SDL_SetColorKey( SourceSurface, SDL_SRCCOLORKEY, SDL_GetPixel( SourceSurface, 0, 0 ) );

  BlitRect.x := 100;
  BlitRect.y := 100;
  BlitRect.w := 100;
  BlitRect.h := 100;

  SpriteRect.x := 0;
  SpriteRect.Y := 0;
  SpriteRect.w := 100;
  SpriteRect.h := 100;

  for DrawAngle := 0 to 360 do
  begin

    SDL_FillRect( Screen_, nil, 0 );

    SDL_RotateDeg( screen_, SourceSurface, @SpriteRect, 320, 200, 50, 25, DrawAngle );

    SDL_RotateDeg( screen_, SourceSurface, @SpriteRect, 100, 100, 50, 25, -DrawAngle );

    SDL_UpdateRect( screen_, 0, 0, 640, 480 );
  end;

  SDL_FreeSurface( SourceSurface );

  SDL_Quit;
end.
