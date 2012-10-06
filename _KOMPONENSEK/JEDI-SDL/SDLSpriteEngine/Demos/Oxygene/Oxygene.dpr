program Oxygene;

{
    Copyright (C) 2001  Róbert Kisnémeth

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

// Version 1.1
// An acrcade game by KiCHY dESiGN. Use at your own risk. Distribute freely.
// Hints, tips, bugreports, comments to:
// mikrobi@freemail.hu

// History:
// 26 Sept 2001 RK: Version 1.0
//  2  Oct 2001 RK: Version 1.1 Added sound effects

// Uncomment this to use BASS v1.1 instead of SDL_Mixer (only under Windows)
//{$define BASS}
// Sound effects currently supported only with SDL_MIXER.
// You can download BASS at: www.un4seen.com (for Windows)

// Uncomment this to enable Logging
// Never know when we need this option... and this is a good example too...
{$DEFINE LOG}

uses
  SysUtils,
  SDL,
  SDL_Image,
  SDLSprites in '../../Pas/SDLSprites.pas',
  Logger,
{$IFDEF BASS}
  Bass;
{$ELSE}
  SDL_Mixer;
{$ENDIF}

const
  Version_no = 'v1.1';
  // Number of levels in game
  MaxCaverns = 20;

  // Charset string
  Chars = 'abcdefghijklmnopqrstuvwxyz0123456789O';

  // Directories
  GfxDir = 'Gfx/';
  MusicDir = 'Music/';
  SoundDir = 'Sounds/';

  // Number of Sound effects
  NumberOfSounds = 7;

  // Filenames
  SoundFilenames : array[1..NumberOfSounds] of string =
    ('Clyde_Hurt.wav', 'Diamond_Break.wav', 'Get_Air.wav', 'Get_Diamond.wav',
     'Push_Rock.wav', 'Spikie_Death.wav', 'Medusa_Death.wav');
  PaletteFilename = GfxDir + 'Game.pal';
  TitleMusic = Musicdir + 'Title.xm';
  IngameMusic = Musicdir + 'Ingame.xm';
  NextMusic = MusicDir + 'Next.xm';
  GameOverMusic = Musicdir + 'Game Over.xm';
  WellDoneMusic = Musicdir + 'Well Done.xm';

  SND_Clyde_Hurt = 1;
  SND_Diamond_Break = 2;
  SND_Get_Air = 3;
  SND_Get_Diamond = 4;
  SND_Push_Rock = 5;
  SND_Spikie_Death = 6;
  SND_Medusa_Death = 7;

  // Codes

  BubbleCode = 42;
  RockCode = 43;

  // Z values
  RockZ = 10;
  DiamondZ = 20;
  WhaleZ = 30;
  SpikieZ = 40;
  MedusaZ = 50;
  PlayerZ = 60;
  BubbleZ = 70;
  PlantZ = 80;
  LogoZ = 90;
  LetterZ = 100;
  QuitBubbleZ = 110;
  FishZ = 120;
  FakePlayerZ = 130;

  // IDs
  idMINIBUBBLE = 1;
  idWHALE = 2;
  idMEDUSA = 3;
  idBUBBLE = 4;
  idROCK = 5;
  idDIAMOND = 6;
  idSPIKIE = 7;
  idO2 = 8;

  // Map codes
  WALL = 1;
  STANDROCK = 2;
  GRASS = 4;
  STANDBUBBLE = 8;

  TICK_INTERVAL = 1000 div 30;

  codes = ' abcdefgh<>ijklmnop[]qrstuvwx{}yz012345()#BR';
  MapID : array[0..43] of byte =
  ( 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1,
    1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, GRASS, STANDBUBBLE,
    STANDROCK );

type
  TTextAlign = ( alLeft, alCenter, alRight );

  TMap = array[0..39, 0..29] of byte;

  // Objects
  TPlayer = class( TSprite )
    xv, yv : integer;
    AnimSpeed : integer;
    AnimCounter : integer;
    Direction : integer;
    Wait : integer;
    Glass : PSDL_Surface;
    GlassRects : array[0..7] of TSDL_Rect;
    constructor Create;
    procedure Free; override;
    procedure GetCollisionRect( Rect : PSDL_Rect ); override;
    procedure RollRockLeft( mx, my : integer );
    procedure RollRockRight( mx, my : integer );
    procedure Hit;
    procedure Draw; override;
    procedure DeleteGrass( mx, my : integer );
    procedure Left;
    procedure Right;
    procedure Up;
    procedure Down;
    procedure Move; override;
    procedure DetectCollision;
  end;

  TFakePlayer = class( TSprite )
    AnimSpeed : integer;
    AnimCounter : integer;
    Direction : integer;
    Wait : integer;
    Glass : PSDL_Surface;
    GlassRects : array[0..7] of TSDL_Rect;
    constructor Create;
    procedure Move; override;
    procedure Free; override;
    procedure Draw; override;
  end;

  TSpikie = class( TSprite )
    AnimSpeed : integer;
    AnimCounter : integer;
    Direction : integer;
    constructor Create;
    procedure GetCollisionRect( Rect : PSDL_Rect ); override;
    procedure DetectCollision;
    procedure Move; override;
  end;

  TBubble = class( TSprite )
    mx, my : integer;
    Stage : integer;
    AnimSpeed : integer;
    AnimCounter : integer;
    constructor Create;
    procedure DetectCollision;
    procedure Kill2;
    procedure Draw; override;
    procedure Move; override;
  end;

  TMiniBubble = class( TSprite )
    Counter : integer;
    constructor Create;
    procedure Draw; override;
    procedure Move; override;
  end;

  TRock = class( TSprite )
    Stage : integer;
    constructor Create;
    procedure StartRollLeft;
    procedure StartRollRight;
    procedure GetCollisionRect( Rect : PSDL_Rect ); override;
    procedure Move; override;
  end;

  TDiamond = class( TSprite )
    State : integer;
    constructor Create;
    procedure GetCollisionRect( Rect : PSDL_Rect ); override;
    procedure DetectCollision;
    procedure Hit;
    procedure Move; override;
  end;

  TWhale = class( TSprite )
    Direction : integer;
    constructor Create;
    procedure Move; override;
  end;

  TMedusa = class( TSprite )
    Direction : integer;
    constructor Create;
    procedure Draw; override;
    procedure Move; override;
    procedure DetectCollision;
  end;

  TPlant = class( TSprite )
    MaxSpeed, AnimSpeed, AnimCounter, Kind : integer;
    constructor Create;
    procedure Move; override;
  end;

  TLogoLetter = class( TSprite )
    SinCounter, AnimCounter : integer;
    constructor Create( const _Image : string; Width, Height : integer );
    procedure Move; override;
    procedure Draw; override;
  end;

  TLetter = class( TSprite )
    XCounter, YCounter : integer;
    constructor Create( _Image : PSDL_Surface; ch : char; Width, Height : integer
      );
    procedure SetChar( ch : char );
    procedure Move; override;
    procedure Draw; override;
    procedure Free; override;
  end;

  TSwimmingLetter = class( TSprite )
    XCounter, YCounter : integer;
    constructor Create( _Image : PSDL_Surface; ch : char; Width, Height : integer
      );
    procedure SetChar( ch : char );
    procedure Move; override;
    procedure Draw; override;
    procedure Free; override;
  end;

  TQuitBubble = class( TSprite )
    XCounter, YCounter : integer;
    constructor Create;
    procedure Move; override;
    procedure Draw; override;
  end;

  TFish = class( TSprite )
    MaxAnim, SwimSpeed, AnimSpeed, AnimCounter, Direction : integer;
    constructor Create;
    procedure Move; override;
  end;

  // Game state
  TScene = ( scTitle, scGame, scGameOver, scWellDone, scQuitApp );

var
  Rooms : array[0..4, 0..3] of boolean;
  RedTile : PSDL_Surface = nil;
  GreenTile : PSDL_Surface = nil;
  Screen : PSDL_Surface = nil;
  BackTiles : PSDL_Surface = nil;
  Background : PSDL_Surface = nil;
  Tiles : PSDL_Surface = nil;
  Chars15x16 : PSDL_Surface = nil;
  Chars19x13 : PSDL_Surface = nil;
  SpriteEngine : TSpriteEngine = nil;
  FullScreen : cardinal = SDL_FULLSCREEN;
  Next_Time : UInt32 = 0;
  GameMap : TMap;
  Palette : array[0..255] of TSDL_Color;
  keys : PKeyStateArr;
  bpp : integer = 0;
  NextScene : TScene;
  O2Rects : array[0..2] of TSDL_Rect;
  ScoreRects : array[0..5] of TSDL_Rect;
  LivesRect : TSDL_Rect;
  BackTileType : integer;

  // Player stuffz
  PlayerStartX, PlayerStartY : integer;
  HighScore : cardinal = 0;
  Lives, Score, Cavern, MaxO2, O2 : cardinal;
  PlayerAlive : boolean;
  NumOfDiamonds : integer;

  // Music stuffz
{$IFDEF BASS}
  BASS_SoundFlags : cardinal = 0;
  Music : HMusic;
  Handle : cardinal;
  Sounds : array[1..NumberOfSounds] of HSample;
{$ELSE}
  Music : PMix_Music = nil;
  Sounds : array[1..NumberOfSounds] of PMix_Chunk;
{$ENDIF}
  Freq : integer = 44100;
  AudioFormat : cardinal = AUDIO_S16LSB;
  Channels : cardinal = 2;
  SoundEnabled : boolean;

procedure UpdatePanel; forward;

// Give a sprite at screen position mx,my

function SearchSpriteAt( mx, my : integer ) : TSprite;
var
  i : integer;
begin
  Result := nil;
  for i := 0 to SpriteEngine.Sprites.Count - 1 do
    if ( SpriteEngine.Sprites[i].x = mx ) and ( SpriteEngine.Sprites[i].y = my )
      then
    begin
      Result := SpriteEngine.Sprites[i];
      exit;
    end;
end;

procedure StartSound(Index: integer);
begin
{$IFDEF BASS}
  if Sounds[Index] = 0 then
    exit;
  BASS_SamplePlay( Sounds[Index] );
{$ELSE}
  if Sounds[Index] = nil then
    exit;
  Mix_PlayChannel( -1, Sounds[Index], 0 );
{$ENDIF}  
end;
{ - TPlayer -------------------------------------------------------------- }

constructor TPlayer.Create;
var
  i : integer;
begin
  inherited Create( GfxDir + 'Player.png', 28, 25 );
  PlayerAlive := true;
  AnimPhase := 0;
  AnimCounter := 0;
  if bpp > 8 then
  begin
    Glass := IMG_Load( GfxDir + 'Player_Glass.png' );
    Glass := SDL_DisplayFormat( Glass );
    SDL_SetColorKey( Glass, SDL_SRCCOLORKEY or SDL_HWACCEL, SDL_MapRGB(
      Glass.format, 255, 0, 255 ) );
    SDL_SetAlpha( Glass, SDL_SRCALPHA, 128 );
  end
  else
    Glass := nil;
  z := PlayerZ;
  for i := 0 to 7 do
  begin
    GlassRects[i].x := i * 28;
    GlassRects[i].y := 0;
    GlassRects[i].w := 28;
    GlassRects[i].h := 25;
  end;
  Direction := 2;
  xv := 0;
  yv := 0;
end;

procedure TPlayer.Free;
begin
  if Glass <> nil then
    SDL_FreeSurface( Glass );
  PlayerAlive := false;
  inherited Free;
end;

procedure TPlayer.GetCollisionRect( Rect : PSDL_Rect );
begin
  Rect.x := x + 4;
  Rect.y := y + 6;
  Rect.w := 20;
  Rect.h := 16;
end;

procedure TPlayer.Hit;
begin
  if Direction <> 1 then
  begin
    StartSound(SND_Clyde_Hurt);
    Direction := 1;
    Wait := 0;
  end;
end;

procedure TPlayer.DetectCollision;
var
  i : integer;
  Rect1, Rect2 : TSDL_Rect;
begin
  GetCollisionRect( @Rect1 );
  // Check collision with all other sprites
  for i := 0 to ParentList.Count - 1 do
    if ( ParentList[i] <> self ) and ( TSprite( ParentList[i] ).ID > 0 ) then
    begin
      ParentList[i].GetCollisionRect( @Rect2 );
      if isCollideRects( @Rect1, @Rect2 ) then
      begin
        // Collision with various objects
        case ParentList[i].ID of
          // Player death
          idWHALE, idMEDUSA, idSPIKIE : Hit;
          // If Bubble is moving then player death
          idBUBBLE : if TBubble( ParentList[i] ).Stage = 1 then
              Hit;
          // If rock is falling then player death
          idROCK : if TRock( ParentList[i] ).Stage = 17 then
              Hit;
          // Collect a diamond
          idDIAMOND :
            begin
              TDiamond( ParentList[i] ).Kill;
              if Score div 5000 < ( Score + 100 ) div 5000 then
                if Lives < 9 then
                  inc( Lives );
              inc( Score, 100 );
              dec( NumOfDiamonds );
              StartSound(SND_Get_Diamond);
              UpdatePanel;
            end;
          // Collect a small bubble
          idO2 :
            begin
              TMiniBubble( ParentList[i] ).Kill;
              inc( O2, 5 );
              inc( Score );
              StartSound(SND_Get_Air);
              UpdatePanel;
            end;
        end;
      end;
    end;
end;

procedure TPlayer.Move;
const
  AnimLeft : array[0..3] of integer = ( 3, 4, 5, 4 );
  AnimRight : array[0..3] of integer = ( 0, 1, 2, 1 );
  AnimAhead : array[0..3] of integer = ( 6, 7, 6, 7 );
var
  MiniBubble : TMiniBubble;
  rnd : integer;
begin
  inc( AnimSpeed );
  if AnimSpeed > 3 then
  begin
    AnimSpeed := 0;
    inc( AnimCounter );
    if AnimCounter > 3 then
      AnimCounter := 0;
  end;
  if Direction <> 1 then
  begin
    rnd := 100;
    if keys[SDLK_LEFT] = 1 then
    begin
      // Go left
      Direction := 0;
      if xv > -8 then
        dec( xv );
    end
    else if keys[SDLK_RIGHT] = 1 then
    begin
      // Go right
      Direction := 2;
      if xv < 8 then
        inc( xv );
    end
    else
    begin
      // Horizontal slow-down
      if xv < 0 then
        inc( xv )
      else if xv > 0 then
        dec( xv );
    end;
    if xv < 0 then
      Left
    else if xv > 0 then
      Right;
    if keys[SDLK_UP] = 1 then
    begin
      // Go up
      if yv > -8 then
        dec( yv );
    end
    else if keys[SDLK_DOWN] = 1 then
    begin
      // Go down
      if yv < 8 then
        inc( yv );
    end
    else
      if yv < 0 then
        // Vertical slow-down
        inc( yv )
      else if yv > 0 then
        dec( yv );
    if yv < 0 then
      Up
    else if yv > 0 then
      Down;
  end
  else
  begin // Death
    rnd := 5;
    inc( wait );
    if Wait = 60 then
      Kill;
  end;
  if random( rnd ) = 0 then
  begin
    // Put some small bubbles
    MiniBubble := TMiniBubble.Create;
    MiniBubble.x := x + 5 + Direction * 6;
    MiniBubble.y := y;
    SpriteEngine.AddSprite( MiniBubble );
  end;
  case Direction of
    0 : AnimPhase := AnimLeft[AnimCounter];
    1 : AnimPhase := AnimAhead[AnimCounter];
    2 : AnimPhase := AnimRight[AnimCounter];
  end;
  if Direction <> 1 then
    DetectCollision;
end;

procedure TPlayer.RollRockLeft( mx, my : integer );
var
  Rock : TRock;
begin
  // Let's roll left that rock left to player
  Rock := TRock( SearchSpriteAt( mx * 16, my * 16 ) );
  if Rock <> nil then
    if Rock.ID = idROCK then
      Rock.StartRollLeft;
end;

procedure TPlayer.RollRockRight( mx, my : integer );
var
  Rock : TRock;
begin
  // Let's roll right that rock right to player
  Rock := TRock( SearchSpriteAt( mx * 16, my * 16 ) );
  if Rock <> nil then
    if Rock.ID = idROCK then
      Rock.StartRollRight;
end;

procedure TPlayer.Left;
var
  mx, my1, my2 : integer;
  Coll : byte;
  NewX : integer;
begin
  NewX := x + xv div 3;
  if NewX < 0 then
  begin
    // Player reaches the left side of screen (Just for SAFETY). It never
    // could be happen!
    x := 0;
    xv := 0;
    exit;
  end;
  if ( ( NewX + 4 ) and 15 ) < 2 then
  begin
    // Player at block border
    mx := ( NewX + 4 ) div 16;
    my1 := ( y + 6 ) div 16;
    Coll := MapID[GameMap[mx - 1, my1]];
    my2 := ( y + 21 ) div 16;
    Coll := Coll or MapID[GameMap[mx - 1, my2]];
    if ( Coll and ( WALL or STANDROCK or STANDBUBBLE ) ) > 0 then
    begin
      // Stop the player
      xv := 0;
      NewX := ( NewX and $FFFFFFF0 ) + 12;
      // Roll a rock
      if GameMap[mx - 1, my1] = RockCode then
        RollRockLeft( mx - 1, my1 );
      if my1 <> my2 then
        if GameMap[mx - 1, my2] = RockCode then
          RollRockLeft( mx - 1, my2 );
    end;
  end;
  if ( ( NewX + 4 ) and 15 ) >= 12 then
  begin
    mx := ( NewX + 4 ) div 16;
    my1 := ( y + 6 ) div 16;
    Coll := MapID[GameMap[mx, my1]];
    my2 := ( y + 21 ) div 16;
    Coll := Coll or MapID[GameMap[mx, my2]];
    if ( Coll and GRASS ) = GRASS then
    begin
      // The player swims into grassy area, clear his way
      if MapID[GameMap[mx, my1]] = GRASS then
        DeleteGrass( mx, my1 );
      if MapID[GameMap[mx, my2]] = GRASS then
        DeleteGrass( mx, my2 );
    end;
  end;
  x := NewX;
end;

procedure TPlayer.Right;
var
  mx, my1, my2 : integer;
  Coll : byte;
  NewX : integer;
begin
  NewX := x + xv div 3;
  if NewX > 640 - w then
  begin
    // Player reaches the right side of screen (Just for SAFETY). It never
    // could be happen!
    x := 640 - w;
    xv := 0;
    exit;
  end;
  if ( ( NewX + 23 ) and 15 ) < 4 then
  begin
    // Player at right side of block border
    mx := ( NewX + 23 ) div 16;
    my1 := ( y + 6 ) div 16;
    Coll := MapID[GameMap[mx, my1]];
    my2 := ( y + 21 ) div 16;
    Coll := Coll or MapID[GameMap[mx, my2]];
    if ( Coll and ( WALL or STANDROCK or STANDBUBBLE ) ) > 0 then
    begin
      // Stop player
      xv := 0;
      NewX := NewX and $FFFFFFF0 + 8;
      // Roll a rock
      if GameMap[mx, my1] = RockCode then
        RollRockRight( mx, my1 );
      if my1 <> my2 then
        if GameMap[mx, my2] = RockCode then
          RollRockRight( mx, my2 );
    end;
  end;
  if ( ( NewX + 27 ) and 15 ) > 4 then
  begin
    mx := ( NewX + 23 ) div 16;
    my1 := ( y + 6 ) div 16;
    Coll := MapID[GameMap[mx, my1]];
    my2 := ( y + 21 ) div 16;
    Coll := Coll or MapID[GameMap[mx, my2]];
    if ( Coll and GRASS ) = GRASS then
    begin
      // Grass
      if MapID[GameMap[mx, my1]] = GRASS then
        DeleteGrass( mx, my1 );
      if MapID[GameMap[mx, my2]] = GRASS then
        DeleteGrass( mx, my2 );
    end;
  end;
  x := NewX;
end;

procedure TPlayer.Up;
var
  mx1, mx2, mx3, my : integer;
  Coll : byte;
  NewY : integer;
begin
  NewY := y + yv div 3;
  if NewY < 0 then
  begin
    // Player reaches the top of screen (Just for SAFETY). It never
    // could be happen!
    y := 0;
    yv := 0;
    exit;
  end;
  if ( ( NewY + 6 ) and 15 ) > 9 then
  begin
    // Player at block border
    my := ( NewY + 6 ) div 16;
    mx1 := ( x + 4 ) div 16;
    Coll := MapID[GameMap[mx1, my]];
    mx2 := ( x + 14 ) div 16;
    Coll := Coll or MapID[GameMap[mx2, my]];
    mx3 := ( x + 23 ) div 16;
    Coll := Coll or MapID[GameMap[mx3, my]];
    if ( Coll and ( WALL or STANDROCK or STANDBUBBLE ) ) > 0 then
    begin
      // Stop
      yv := 0;
      NewY := ( NewY and $FFFFFFF0 ) + 10;
    end;
  end;
  if ( ( NewY + 6 ) and 15 ) > 11 then
  begin
    my := ( NewY + 6 ) div 16;
    mx1 := ( x + 4 ) div 16;
    Coll := MapID[GameMap[mx1, my]];
    mx2 := ( x + 14 ) div 16;
    Coll := Coll or MapID[GameMap[mx2, my]];
    mx3 := ( x + 23 ) div 16;
    Coll := Coll or MapID[GameMap[mx3, my]];
    if ( Coll and GRASS ) = GRASS then
    begin
      // Grass
      if MapID[GameMap[mx1, my]] = GRASS then
        DeleteGrass( mx1, my );
      if MapID[GameMap[mx2, my]] = GRASS then
        DeleteGrass( mx2, my );
      if MapID[GameMap[mx3, my]] = GRASS then
        DeleteGrass( mx3, my );
    end;
  end;
  y := NewY;
end;

procedure TPlayer.Down;
var
  mx1, mx2, mx3, my : integer;
  Coll : byte;
  NewY : integer;
begin
  NewY := y + yv div 3;
  if NewY > 480 - h then
  begin
    // Player reaches the bottom of screen (Just for SAFETY). It never
    // could be happen!
    y := 480 - h;
    yv := 0;
    exit;
  end;
  if ( ( NewY + 21 ) and 15 ) < 4 then
  begin
    // Block border
    my := ( NewY + 21 ) div 16;
    mx1 := ( x + 4 ) div 16;
    Coll := MapID[GameMap[mx1, my]];
    mx2 := ( x + 14 ) div 16;
    Coll := Coll or MapID[GameMap[mx2, my]];
    mx3 := ( x + 23 ) div 16;
    Coll := Coll or MapID[GameMap[mx3, my]];
    if ( Coll and ( WALL or STANDROCK or STANDBUBBLE ) ) > 0 then
    begin
      // Stop
      yv := 0;
      NewY := ( NewY and $FFFFFFF0 ) + 10;
    end;
  end;
  if ( ( NewY + 21 ) and 15 ) < 3 then
  begin
    my := ( NewY + 21 ) div 16;
    mx1 := ( x + 4 ) div 16;
    Coll := MapID[GameMap[mx1, my]];
    mx2 := ( x + 14 ) div 16;
    Coll := Coll or MapID[GameMap[mx2, my]];
    mx3 := ( x + 23 ) div 16;
    Coll := Coll or MapID[GameMap[mx3, my]];
    if ( Coll and GRASS ) = GRASS then
    begin
      // Grass
      if MapID[GameMap[mx1, my]] = GRASS then
        DeleteGrass( mx1, my );
      if MapID[GameMap[mx2, my]] = GRASS then
        DeleteGrass( mx2, my );
      if MapID[GameMap[mx3, my]] = GRASS then
        DeleteGrass( mx3, my );
    end;
  end;
  y := NewY;
end;

procedure TPlayer.DeleteGrass( mx, my : integer );
var
  SrcRect, DestRect : TSDL_Rect;
  alpha : integer;
begin
  if my > 0 then
    case GameMap[mx, my - 1] of
      1..4, 11..14, 21..24, 31..34 : GameMap[mx, my] := GameMap[mx, my - 1] + 4;
    else
      GameMap[mx, my] := 0;
    end
  else
    GameMap[mx, my] := 0;
  // Delete grass by redrawing a piece of background
  SrcRect.x := ( mx and 3 ) shl 4 + BackTileType shl 6;
  SrcRect.y := ( my and 3 ) shl 4;
  SrcRect.w := 16;
  SrcRect.h := 16;
  DestRect.x := mx shl 4;
  DestRect.y := my shl 4;
  SDL_UpperBlit( BackTiles, @SrcRect, Background, @DestRect );
  SDL_UpperBlit( BackTiles, @SrcRect, Screen, @DestRect );
  SrcRect.y := 0;
  if bpp > 8 then
  begin
    Alpha := 128 - ( ( y shr 4 ) * 4 );
    if Alpha > 0 then
    begin
      SrcRect.x := ( mx and 7 ) shl 4;
      SDL_SetAlpha( GreenTile, SDL_SRCALPHA, alpha );
      SDL_UpperBlit( GreenTile, @SrcRect, Background, @DestRect );
      SDL_UpperBlit( GreenTile, @SrcRect, Screen, @DestRect );
    end;
  end;
  SrcRect.x := GameMap[mx, my] * 16 - 16;
  DestRect.x := mx shl 4;
  DestRect.y := my shl 4;
  SDL_UpperBlit( Tiles, @SrcRect, Background, @DestRect );
  SDL_UpperBlit( Tiles, @SrcRect, Screen, @DestRect );
end;

procedure TPlayer.Draw;
var
  DestRect : TSDL_Rect;
begin
  // Draw the player
  inherited Draw;
  if Glass <> nil then
  begin
    DestRect.x := x;
    DestRect.y := y;
    SDL_UpperBlit( Glass, @GlassRects[AnimPhase], Surface, @DestRect );
  end;
end;

{ - TFakePlayer ---------------------------------------------------------- }

constructor TFakePlayer.Create;
var
  i : integer;
begin
  inherited Create( GfxDir + 'Player.png', 28, 25 );
  PlayerAlive := true;
  AnimPhase := 0;
  AnimCounter := 0;
  if bpp > 8 then
  begin
    Glass := IMG_Load( GfxDir + 'Player_Glass.png' );
    Glass := SDL_DisplayFormat( Glass );
    SDL_SetColorKey( Glass, SDL_SRCCOLORKEY or SDL_HWACCEL, SDL_MapRGB(
      Glass.format, 255, 0, 255 ) );
    SDL_SetAlpha( Glass, SDL_SRCALPHA, 128 );
  end
  else
    Glass := nil;
  z := FakePlayerZ;
  for i := 0 to 7 do
  begin
    GlassRects[i].x := i * 28;
    GlassRects[i].y := 0;
    GlassRects[i].w := 28;
    GlassRects[i].h := 25;
  end;
  Direction := 2;
end;

procedure TFakePlayer.Move;
const
  AnimLeft : array[0..3] of integer = ( 3, 4, 5, 4 );
  AnimRight : array[0..3] of integer = ( 0, 1, 2, 1 );
  AnimAhead : array[0..3] of integer = ( 6, 7, 6, 7 );
begin
  inc( AnimSpeed );
  if AnimSpeed > 3 then
  begin
    AnimSpeed := 0;
    inc( AnimCounter );
    if AnimCounter > 3 then
      AnimCounter := 0;
  end;
  case Direction of
    0 : AnimPhase := AnimLeft[AnimCounter];
    1 : AnimPhase := AnimAhead[AnimCounter];
    2 : AnimPhase := AnimRight[AnimCounter];
  end;
end;

procedure TFakePlayer.Free;
begin
  if Glass <> nil then
    SDL_FreeSurface( Glass );
  inherited Free;
end;

procedure TFakePlayer.Draw;
var
  DestRect : TSDL_Rect;
begin
  inherited Draw;
  if Glass <> nil then
  begin
    DestRect.x := x;
    DestRect.y := y;
    SDL_UpperBlit( Glass, @GlassRects[AnimPhase], Surface, @DestRect );
  end;
end;

{ - TPlant --------------------------------------------------------------- }

constructor TPlant.Create;
begin
  inherited Create( GfxDir + 'Plants.png', 16, 24 );
  Kind := random( 2 );
  AnimCounter := random( 4 );
  AnimSpeed := 0;
  MaxSpeed := 4 + random( 3 );
  z := PlantZ;
end;

procedure TPlant.Move;
const
  Anim : array[0..3] of integer = ( 0, 1, 2, 1 );
var
  MiniBubble : TMiniBubble;
begin
  inc( AnimSpeed );
  if AnimSpeed = MaxSpeed then
  begin
    AnimSpeed := 0;
    inc( AnimCounter );
    if AnimCounter = 4 then
      AnimCounter := 0;
  end;
  if random( 500 ) = 0 then
  begin
    MiniBubble := TMiniBubble.Create;
    MiniBubble.x := x + random( 9 );
    MiniBubble.y := y + 10;
    MiniBubble.ID := idO2;
    SpriteEngine.AddSprite( MiniBubble );
  end;
  AnimPhase := Kind * 3 + Anim[AnimCounter];
end;

{ - TSpikie -------------------------------------------------------------- }

constructor TSpikie.Create;
begin
  inherited Create( GfxDir + 'Spikie.png', 24, 20 );
  z := SpikieZ;
  AnimPhase := 0;
  AnimCounter := random( 4 );
  AnimSpeed := random( 4 );
  ID := idSPIKIE;
  Direction := 4;
end;

procedure TSpikie.GetCollisionRect( Rect : PSDL_Rect );
begin
  Rect.x := x + 4;
  Rect.y := y + 2;
  Rect.w := 16;
  Rect.h := 16;
end;

procedure TSpikie.DetectCollision;
var
  i : integer;
  Rect1, Rect2 : TSDL_Rect;
begin
  GetCollisionRect( @Rect1 );
  // Check collision with other sprites
  for i := 0 to ParentList.Count - 1 do
    if ( ParentList[i] <> self ) and ( TSprite( ParentList[i] ).ID > 0 ) then
    begin
      ParentList[i].GetCollisionRect( @Rect2 );
      if isCollideRects( @Rect1, @Rect2 ) then
        if ParentList[i].ID = idROCK then
          // Is the rock falling?
          if TRock( ParentList[i] ).Stage = 17 then
          begin
            // Death
            StartSound(SND_Spikie_Death);
            Kill;
            inc( Score, 50 );
          end;
    end;
end;

procedure TSpikie.Move;
const
  Anim : array[0..3] of integer = ( 0, 1, 2, 1 );
var
  mx, my : integer;
begin
  inc( AnimSpeed );
  if AnimSpeed = 4 then
  begin
    AnimSpeed := 0;
    inc( AnimCounter );
    if AnimCounter = 4 then
      AnimCounter := 0;
  end;
  AnimPhase := Anim[AnimCounter];
  case Direction of
    0 :
      begin // Up
        if ( ( x and 15 ) = 12 ) and ( ( y and 15 ) = 14 ) then
        begin
          mx := x div 16 + 1;
          my := y div 16 + 1;
          if MapID[GameMap[mx - 1, my]] = 0 then
          begin
            Direction := 3;
            dec( x );
          end
          else if MapID[GameMap[mx, my - 1]] > 0 then
            Direction := 1
          else
            dec( y );
        end
        else
          dec( y );
      end;
    1 :
      begin // Right
        if ( ( x and 15 ) = 12 ) and ( ( y and 15 ) = 14 ) then
        begin
          mx := x div 16 + 1;
          my := y div 16 + 1;
          if MapID[GameMap[mx, my - 1]] = 0 then
          begin
            Direction := 0;
            dec( y );
          end
          else if MapID[GameMap[mx + 1, my]] > 0 then
            Direction := 2
          else
            inc( x );
        end
        else
          inc( x );
      end;
    2 :
      begin // Down
        if ( ( x and 15 ) = 12 ) and ( ( y and 15 ) = 14 ) then
        begin
          mx := x div 16 + 1;
          my := y div 16 + 1;
          if MapID[GameMap[mx + 1, my]] = 0 then
          begin
            Direction := 1;
            inc( x );
          end
          else if MapID[GameMap[mx, my + 1]] > 0 then
            Direction := 3
          else
            inc( y );
        end
        else
          inc( y );
      end;
    3 :
      begin // Left
        if ( ( x and 15 ) = 12 ) and ( ( y and 15 ) = 14 ) then
        begin
          mx := x div 16 + 1;
          my := y div 16 + 1;
          if MapID[GameMap[mx, my + 1]] = 0 then
          begin
            Direction := 2;
            inc( y );
          end
          else if MapID[GameMap[mx - 1, my]] > 0 then
            Direction := 0
          else
            dec( x );
        end
        else
          dec( x );
      end;
    4 :
      begin // Choose a direction
        mx := x div 16 + 1;
        my := y div 16 + 1;
        if MapID[GameMap[mx, my - 1]] > 0 then
          Direction := 1
        else if MapID[GameMap[mx + 1, my]] > 0 then
          Direction := 2
        else if MapID[GameMap[mx, my + 1]] > 0 then
          Direction := 3
        else if MapID[GameMap[mx - 1, my]] > 0 then
          Direction := 0;
      end;
  end;
  DetectCollision;
end;

{ - TMedusa -------------------------------------------------------------- }

constructor TMedusa.Create;
begin
  inherited Create( GfxDir + 'Medusa.png', 18, 21 );
  z := MedusaZ;
  ID := idMEDUSA;
  if bpp > 8 then
    SDL_SetAlpha( Image, SDL_SRCALPHA, 204 );
  Direction := random( 2 );
end;

procedure TMedusa.DetectCollision;
var
  i : integer;
  Rect1, Rect2 : TSDL_Rect;
begin
  GetCollisionRect( @Rect1 );
  // Check collision
  for i := 0 to ParentList.Count - 1 do
    if ( ParentList[i] <> self ) and ( TSprite( ParentList[i] ).ID > 0 ) then
    begin
      ParentList[i].GetCollisionRect( @Rect2 );
      if isCollideRects( @Rect1, @Rect2 ) then
        // Ralling rock
        if ParentList[i].ID = idROCK then
          if TRock( ParentList[i] ).Stage = 17 then
          begin
            // Death
            StartSound(SND_Medusa_Death);
            Kill;
            inc( Score, 75 );
          end;
    end;
end;

procedure TMedusa.Move;
var
  mx, my : integer;
begin
  case AnimPhase of
    1 :
      begin // Up
        if y < 2 then
          AnimPhase := 0
        else if ( y and 15 ) < 2 then
        begin
          mx := ( x + 1 ) div 16;
          my := y div 16;
          if MapID[GameMap[mx, my - 1]] > 0 then
            AnimPhase := 0
          else
            dec( y, 2 );
        end
        else
          dec( y, 2 );
      end;
    0 :
      begin // Down
        if y > 480 + h then
          AnimPhase := 1
        else if ( y and 15 ) < 1 then
        begin
          mx := ( x + 1 ) div 16;
          my := y div 16;
          if MapID[GameMap[mx, my + 1]] > 0 then
            AnimPhase := 1
          else
            inc( y, 1 );
        end
        else
          inc( y, 1 );
      end;
  end;
  DetectCollision;
end;

procedure TMedusa.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, -1, -1,
    -1, -1, -1, -1 );
var
  DestRect : TSDL_Rect;
begin
  SrcRect.x := AnimPhase * w;
  DestRect.x := x + SinTable[y and 15];
  DestRect.y := y;
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

{ - TWhale --------------------------------------------------------------- }

constructor TWhale.Create;
begin
  z := WhaleZ;
  inherited Create( GfxDir + 'Whale.png', 48, 21 );
  ID := idWHALE;
  Direction := random( 2 );
end;

procedure TWhale.Move;
const
  AnimLeft : array[0..3] of integer = ( 0, 1, 2, 1 );
  AnimRight : array[0..3] of integer = ( 3, 4, 5, 4 );
var
  mx, my : integer;
begin
  case Direction of
    0 :
      begin // Left
        if x < 1 then
          Direction := 1
        else if ( x and 15 ) = 0 then
        begin
          mx := x div 16;
          my := ( y + 4 ) div 16;
          if MapID[GameMap[mx - 1, my]] > 0 then
            Direction := 1
          else
            dec( x );
        end
        else
          dec( x );
        AnimPhase := AnimLeft[( x and 15 ) shr 2];
      end;
    1..6 :
      begin // Wait & turn
        AnimPhase := 6;
        inc( Direction );
      end;
    7 :
      begin // Right
        if x > 640 - w then
          Direction := 8
        else if ( x and 15 ) = 15 then
        begin
          mx := x div 16;
          my := ( y + 4 ) div 16;
          if MapID[GameMap[mx + 4, my]] > 0 then
            Direction := 8
          else
            inc( x );
        end
        else
          inc( x );
        AnimPhase := AnimRight[( x and 15 ) shr 2];
      end;
    8..13 :
      begin // Wait & turn
        AnimPhase := 7;
        inc( Direction );
        if Direction = 14 then
          Direction := 0;
      end;
  end;
end;

{ - TBubble -------------------------------------------------------------- }

constructor TBubble.Create;
begin
  inherited Create( GfxDir + 'Bubble.png', 16, 16 );
  ID := idBUBBLE;
  z := BubbleZ;
  Stage := 0;
end;

procedure TBubble.DetectCollision;
var
  i : integer;
  Rect1, Rect2 : TSDL_Rect;
begin
  GetCollisionRect( @Rect1 );
  // Check collision
  for i := 0 to ParentList.Count - 1 do
    if ( ParentList[i] <> self ) and ( TSprite( ParentList[i] ).ID > 0 ) then
    begin
      ParentList[i].GetCollisionRect( @Rect2 );
      if isCollideRects( @Rect1, @Rect2 ) then
      begin
        case ParentList[i].ID of
          // Kill the bubble and add a little animation
          idSPIKIE : Kill2;
          idROCK : if TRock( ParentList[i] ).Stage = 17 then
              Kill2;
        end;
      end;
    end;
end;

procedure TBubble.Move;
const
  AnimStages : array[0..3] of integer = ( 7, 8, 9, 8 );
begin
  case Stage of
    0 :
      begin // Grows
        inc( AnimSpeed );
        if AnimSpeed = 4 then
        begin
          AnimSpeed := 0;
          inc( AnimPhase );
          if AnimPhase = 7 then
          begin
            Stage := 1;
            AnimCounter := random( 4 );
          end;
        end;
      end;
    1 :
      begin // Goes up
        inc( AnimSpeed );
        if AnimSpeed = 4 then
        begin
          AnimSpeed := 0;
          inc( AnimCounter );
          if AnimCounter = 4 then
            AnimCounter := 0;
          AnimPhase := AnimStages[AnimCounter];
        end;
        if y > 15 then
          if ( y and 15 ) = 0 then
          begin
            mx := x div 16;
            my := y div 16;
            if MapID[GameMap[mx, my - 1]] > 0 then
            begin
              Stage := 2; // Stops
              GameMap[mx, my] := BubbleCode;
            end;
          end;
        if Stage = 1 then
        begin
          y := y - 1;
          if y < -16 then
            Kill;
        end;
      end;
    2 :
      begin // Stands
        if MapID[GameMap[mx, my - 1]] = 0 then
        begin
          Stage := 10;
          GameMap[mx, my] := 0;
        end
        else if random( 100 ) = 0 then
          Kill2;
      end;
    3..8, 10..15 : inc( stage ); // Waits
    9 :
      begin // Death
        GameMap[mx, my] := 0;
        Kill;
      end;
    16 : Stage := 1;
  end;
  DetectCollision;
end;

procedure TBubble.Kill2;
begin
  AnimPhase := 10;
  if Stage in [2, 3..8, 10..15] then
    GameMap[mx, my] := 0;
  Stage := 3;
end;

procedure TBubble.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 1, 1, 2, 2, 2, 1, 1, 0, -1, -2, -3,
    -3, -3, -2, -1 );
var
  DestRect : TSDL_Rect;
begin
  SrcRect.x := AnimPhase * w;
  DestRect.x := x + SinTable[y and 15];
  DestRect.y := y;
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

{ - TMiniBubble ---------------------------------------------------------- }

constructor TMiniBubble.Create;
begin
  inherited Create( GfxDir + 'MiniBubbles.png', 7, 7 );
  ID := idMINIBUBBLE;
  AnimPhase := random( 2 );
  Counter := 0;
  z := BubbleZ;
end;

procedure TMiniBubble.Move;
var
  mx, my : integer;
begin
  case Counter of
    0 :
      begin // Go up
        if y > 0 then
          if ( y and 15 ) = 0 then
          begin
            mx := ( x + 3 ) div 16;
            my := ( y - 1 ) div 16;
            if MapID[GameMap[mx, my]] > 0 then
            begin
              Counter := 1;
              AnimPhase := 2;
              exit;
            end;
          end;
        y := y - 1;
        if y = -7 then
          Kill;
      end;
    1..10 : inc( Counter ); // Wait
    11 : Kill; // Death
  end;
end;

procedure TMiniBubble.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, -1, -1,
    -1, -1, -1, -1 );
var
  DestRect : TSDL_Rect;
begin
  SrcRect.x := AnimPhase * w;
  DestRect.x := x + SinTable[y and 15];
  DestRect.y := y;
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

{ - TDiamond ------------------------------------------------------------- }

constructor TDiamond.Create;
begin
  inherited Create( GfxDir + 'Diamond.png', 22, 13 );
  z := DiamondZ;
  ID := idDIAMOND;
  if bpp > 8 then
    SDL_SetAlpha( Image, SDL_SRCALPHA, 192 );
  State := 0;
end;

procedure TDiamond.GetCollisionRect( Rect : PSDL_Rect );
begin
  Rect.x := x + 2;
  Rect.y := y;
  Rect.w := w - 4;
  Rect.h := h;
end;

procedure TDiamond.DetectCollision;
var
  i : integer;
  Rect1, Rect2 : TSDL_Rect;
begin
  GetCollisionRect( @Rect1 );
  // Detect collision
  for i := 0 to ParentList.Count - 1 do
    if ( ParentList[i] <> self ) and ( TSprite( ParentList[i] ).ID > 0 ) then
    begin
      ParentList[i].GetCollisionRect( @Rect2 );
      if isCollideRects( @Rect1, @Rect2 ) then
        if ParentList[i].ID = idROCK then
          if ( TRock( ParentList[i] ).Stage >= 1 ) and ( TRock( ParentList[i] ).y
            < y ) then
            // Hit by a falling rock going from top
            Hit;
    end;
end;

procedure TDiamond.Hit;
begin
  StartSound(SND_Diamond_Break);
  State := 7;
end;

procedure TDiamond.Move;
var
  mx, my : integer;
begin
  case State of
    0 :
      begin // Stands
        mx := ( x + 11 ) div 16;
        my := ( y + 12 ) div 16;
        if MapID[GameMap[mx, my]] = 0 then
          State := 1;
      end;
    1..5 : inc( State ); // Wait
    6 :
      begin // Falls
        mx := ( x + 11 ) div 16;
        my := ( y + 12 ) div 16;
        if MapID[GameMap[mx, my]] > 0 then
          State := 0
        else
          inc( y );
      end;
    7..9 :
      begin // Cracks
        AnimPhase := 1;
        inc( State );
      end;
    10..12 :
      begin // cracks
        AnimPhase := 2;
        inc( State );
      end;
    13 : Kill; // Death
  end;
  if State < 7 then
    DetectCollision;
end;

{ - TRock ---------------------------------------------------------------- }

constructor TRock.Create;
begin
  inherited Create( GfxDir + 'Rock.png', 16, 16 );
  z := RockZ;
  ID := idROCK;
  Stage := 0;
end;

procedure TRock.GetCollisionRect( Rect : PSDL_Rect );
begin
  Rect.x := x + 2;
  Rect.y := y;
  Rect.w := w - 4;
  Rect.h := h;
end;

procedure TRock.StartRollLeft;
var
  mx, my : integer;
begin
  mx := x div 16;
  my := y div 16;
  // If there is nothing left to the rock, then roll it
  if MapID[GameMap[mx - 1, my]] = 0 then
  begin
    Stage := 18;
    StartSound(SND_Push_Rock);
  end;  
end;

procedure TRock.StartRollRight;
var
  mx, my : integer;
begin
  mx := x div 16;
  my := y div 16;
  // If there is nothing right to the rock, then roll it
  if MapID[GameMap[mx + 1, my]] = 0 then
  begin
    Stage := 22;
    StartSound(SND_Push_Rock);
  end;  
end;

procedure TRock.Move;
var
  mx, my : integer;
  roll : integer;
begin
  case Stage of
    0 :
      begin // Stands
        mx := x div 16;
        my := y div 16;
        GameMap[mx, my] := RockCode;
        if MapID[GameMap[mx, my + 1]] = 0 then
          Stage := 1;
      end;
    1..16 : inc( Stage );
    17 :
      begin // Falls
        mx := x div 16;
        my := y div 16;
        if ( ( y and 15 ) = 0 ) and ( GameMap[mx, my + 1] > 0 ) then
        begin
          GameMap[mx, my] := 43;
          Stage := 0;
          if GameMap[mx, my + 1] = 43 then
          begin
            if ( GameMap[mx - 1, my] = 0 ) and ( GameMap[mx - 1, my + 1] = 0 )
              then
              roll := 1
            else
              roll := 0;
            if ( GameMap[mx + 1, my] = 0 ) and ( GameMap[mx + 1, my + 1] = 0 )
              then
              roll := roll or 2;
            if roll = 3 then
              roll := 1 + random( 2 );
            if roll = 1 then
              StartRollLeft
            else if roll = 2 then
              StartRollRight;
          end;
        end
        else
        begin
          if ( y and 15 ) > 13 then
            GameMap[mx, my] := 0;
          inc( y, 2 );
        end;
      end;
    18 :
      begin // Rolls left
        x := x - 4;
        AnimPhase := 3;
        inc( Stage );
      end;
    19 :
      begin // Roll left
        x := x - 4;
        AnimPhase := 2;
        inc( Stage );
        mx := x div 16;
        my := y div 16;
        GameMap[mx, my] := RockCode;
        GameMap[mx + 1, my] := 0;
      end;
    20 :
      begin // Roll left
        x := x - 4;
        AnimPhase := 1;
        inc( Stage );
      end;
    21 :
      begin // Roll left
        x := x - 4;
        AnimPhase := 0;
        Stage := 17;
      end;
    22 :
      begin // Roll right
        x := x + 4;
        AnimPhase := 1;
        inc( Stage );
      end;
    23 :
      begin // Roll right
        x := x + 4;
        AnimPhase := 2;
        inc( Stage );
        mx := x div 16;
        my := y div 16;
        GameMap[mx + 1, my] := RockCode;
        GameMap[mx, my] := 0;
      end;
    24 :
      begin // Roll right
        x := x + 4;
        AnimPhase := 3;
        inc( Stage );
      end;
    25 :
      begin // Roll right
        x := x + 4;
        AnimPhase := 0;
        Stage := 17;
      end;
  end;
end;

{ - TLogoLetter ---------------------------------------------------------- }

constructor TLogoLetter.Create( const _Image : string; Width, Height : integer
  );
begin
  inherited Create( _Image, Width, Height );
  z := LogoZ;
  if bpp > 8 then
    SDL_SetAlpha( Image, SDL_SRCALPHA, 192 );
  AnimCounter := random( 360 );
  SinCounter := random( 64 );
end;

procedure TLogoLetter.Move;
begin
  inc( AnimCounter, 1 + random( 10 ) );
  if AnimCounter > 359 then
    AnimCounter := AnimCounter - 360;
  inc( SinCounter );
  if SinCounter > 127 then
    SinCounter := 0;
end;

procedure TLogoLetter.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 1, 1, 2, 2, 2, 1, 1, 0, -1, -2, -3,
    -3, -3, -2, -1 );
var
  DestRect : TSDL_Rect;
begin
  DestRect.x := x + SinTable[SinCounter shr 3];
  DestRect.y := y + round( 10 * sin( AnimCounter * pi / 180 ) );
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

{ - TLetter -------------------------------------------------------------- }

constructor TLetter.Create( _Image : PSDL_Surface; ch : char; Width, Height :
  integer );
begin
  inherited Create( '', Width, Height );
  Image := _Image;
  z := LetterZ;
  SetChar( ch );
  XCounter := random( 64 );
  YCounter := random( 64 );
end;

procedure TLetter.SetChar( ch : char );
const
  Letters = 'abcdefghijklmnopqrstuvwxyz0123456789.?!":@/';
var
  i : integer;
begin
  i := pos( ch, Letters ) - 1;
  SrcRect.x := i * w;
end;

procedure TLetter.Move;
begin
  inc( XCounter, 1 + random( 2 ) );
  if XCounter > 63 then
    XCounter := XCounter - 64;
  inc( YCounter, 1 + random( 2 ) );
  if YCounter > 63 then
    YCounter := YCounter - 64;
end;

procedure TLetter.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, -1, -1,
    -1, -1, -1, -1 );
var
  DestRect : TSDL_Rect;
begin
  DestRect.x := x + SinTable[XCounter shr 2];
  DestRect.y := y + SinTable[YCounter shr 2];
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

procedure TLetter.Free;
begin
  Image := nil;
  inherited Free;
end;

{ - TSwimmingLetter ------------------------------------------------------ }

constructor TSwimmingLetter.Create( _Image : PSDL_Surface; ch : char; Width,
  Height : integer );
begin
  inherited Create( '', Width, Height );
  Image := _Image;
  z := LetterZ;
  SetChar( ch );
  XCounter := random( 64 );
  YCounter := random( 64 );
end;

procedure TSwimmingLetter.SetChar( ch : char );
const
  Letters = 'abcdefghijklmnopqrstuvwxyz0123456789.?!":@';
var
  i : integer;
begin
  i := pos( ch, Letters ) - 1;
  SrcRect.x := i * w;
end;

procedure TSwimmingLetter.Move;
begin
  inc( XCounter, 1 + random( 2 ) );
  if XCounter > 63 then
    XCounter := XCounter - 64;
  inc( YCounter, 1 + random( 2 ) );
  if YCounter > 63 then
    YCounter := YCounter - 64;
  // Swim left until the letter goes out from screen
  x := x - 2;
  if x < -w then
    // Then kill it
    Kill;
end;

procedure TSwimmingLetter.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, -1, -1,
    -1, -1, -1, -1 );
var
  DestRect : TSDL_Rect;
begin
  DestRect.x := x + SinTable[XCounter shr 2];
  DestRect.y := y + SinTable[YCounter shr 2];
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

procedure TSwimmingLetter.Free;
begin
  Image := nil;
  inherited Free;
end;

{ - TQuitBubble ---------------------------------------------------------- }

constructor TQuitBubble.Create;
begin
  inherited Create( GfxDir + 'QuitBubble.png', 193, 102 );
  if bpp > 8 then
    SDL_SetAlpha( Image, SDL_SRCALPHA, 224 );
  XCounter := random( 64 );
  YCounter := random( 64 );
  x := ( 640 - 193 ) div 2;
  y := ( 480 - 102 ) div 2;
  z := QuitBubbleZ;
end;

procedure TQuitBubble.Move;
begin
  inc( XCounter, 1 + random( 2 ) );
  if XCounter > 63 then
    XCounter := XCounter - 64;
  inc( YCounter, 1 + random( 2 ) );
  if YCounter > 63 then
    YCounter := YCounter - 64;
end;

procedure TQuitBubble.Draw;
const
  SinTable : array[0..15] of integer = ( 0, 1, 1, 2, 2, 2, 1, 1, 0, -1, -2, -3,
    -3, -3, -2, -1 );
var
  DestRect : TSDL_Rect;
begin
  DestRect.x := x + SinTable[XCounter shr 2];
  DestRect.y := y + SinTable[YCounter shr 2];
  SDL_UpperBlit( Image, @SrcRect, Surface, @DestRect );
  PrevRect := DestRect;
end;

{ - TFish ---------------------------------------------------------------- }

constructor TFish.Create;
begin
  case random( 4 ) of
    0 : inherited Create( GfxDir + 'Fish_Silver.png', 24, 14 );
    1 : inherited Create( GfxDir + 'Fish_Red.png', 24, 14 );
    2 : inherited Create( GfxDir + 'Fish_Green.png', 24, 14 );
    3 : inherited Create( GfxDir + 'Fish_Blue.png', 24, 14 );
  end;
  z := FishZ;
  SwimSpeed := 1 + random( 2 );
  if SwimSpeed = 1 then
    MaxAnim := 4
  else
    MaxAnim := 2;
  AnimSpeed := random( 4 );
  Direction := random( 2 );
  AnimCounter := random( 4 );
  case Direction of
    0 : x := 640;
    1 : x := -24;
  end;
  y := random( 480 - h );
end;

procedure TFish.Move;
const
  FishAnim : array[0..3] of integer = ( 0, 1, 2, 1 );
begin
  inc( AnimSpeed );
  if AnimSpeed >= MaxAnim then
  begin
    AnimSpeed := 0;
    inc( AnimCounter );
    if AnimCounter = 4 then
      AnimCounter := 0;
    AnimPhase := FishAnim[AnimCounter] + Direction * 3;
  end;
  case Direction of
    0 :
      begin
        x := x - SwimSpeed;
        if ( x < -24 ) then
          Kill;
      end;
    1 :
      begin
        x := x + SwimSpeed;
        if ( x > 640 ) then
          Kill;
      end;
  end;
  case random( 4 ) of
    0 : y := y - 1;
    3 : y := y + 1;
  end;
end;

{ ------------------------------------------------------------------------ }

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

function LoadPalette : boolean;
var
  i : integer;
  f : text;
begin
  if FileExists( PaletteFilename) then
  begin
    // Header
    readln( f );
    // Version
    readln( f );
    // Number of stored colours
    readln( f );
    for i := 0 to 255 do
      readln( f, Palette[i].r, Palette[i].g, Palette[i].b );
    closefile( f );
    result := true;
{$IFDEF LOG}
    log.LogStatus( 'GAME.PAL file successfully loaded', 'LoadPalette' );
{$ENDIF}
  end
  else
  begin
    // returns false if the file doesn't exist
    result := false;
{$IFDEF LOG}
    log.LogWarning( 'GAME.PAL file not found!', 'LoadPalette' );
{$ENDIF}
  end;
end;

procedure CheckCommandLine;
var
  i : integer;
begin
  if ParamCount > 0 then
  begin
{$IFDEF LOG}
    log.LogStatus( 'Commandline found. Processing...', 'CheckCommandLine' );
{$ENDIF}
    for i := 1 to ParamCount do
    begin
      if ParamStr( i ) = '8bpp' then
      begin
        bpp := 8;
{$IFDEF LOG}
        log.LogStatus(
          'User selected 8 bits per pixel videomode in command line', 'CheckCommandLine'
          );
{$ENDIF}
      end;
      if ParamStr( i ) = '15bpp' then
      begin
        bpp := 15;
{$IFDEF LOG}
        log.LogStatus(
          'User selected 15 bits per pixel videomode in command line', 'CheckCommandLine'
          );
{$ENDIF}
      end;
      if ParamStr( i ) = '16bpp' then
      begin
        bpp := 16;
{$IFDEF LOG}
        log.LogStatus(
          'User selected 16 bits per pixel videomode in command line', 'CheckCommandLine'
          );
{$ENDIF}
      end;
      if ParamStr( i ) = '24bpp' then
      begin
        bpp := 24;
{$IFDEF LOG}
        log.LogStatus(
          'User selected 24 bits per pixel videomode in command line', 'CheckCommandLine'
          );
{$ENDIF}
      end;
      if ParamStr( i ) = '32bpp' then
      begin
        bpp := 32;
{$IFDEF LOG}
        log.LogStatus(
          'User selected 32 bits per pixel videomode in command line', 'CheckCommandLine'
          );
{$ENDIF}
      end;
      if ParamStr( i ) = 'windowed' then
      begin
        FullScreen := 0;
{$IFDEF LOG}
        log.LogStatus( 'User selected windowed videomode in command line',
          'CheckCommandLine' );
{$ENDIF}
      end;
      if ParamStr( i ) = '11khz' then
      begin
        Freq := 11025;
{$IFDEF LOG}
        log.LogStatus( 'User selected 11KHz audiomode in command line',
          'CheckCommandLine' );
{$ENDIF}
      end;
      if ParamStr( i ) = '22khz' then
      begin
        Freq := 22050;
{$IFDEF LOG}
        log.LogStatus( 'User selected 22KHz audiomode in command line',
          'CheckCommandLine' );
{$ENDIF}
      end;
      if ParamStr( i ) = '8bit' then
      begin
{$IFDEF BASS}
        BASS_SoundFlags := BASS_DEVICE_8BITS;
{$ELSE}
        AudioFormat := AUDIO_S8;
{$ENDIF}
{$IFDEF LOG}
        log.LogStatus( 'User selected 8bit audio format in command line',
          'CheckCommandLine' );
{$ENDIF}
      end;
      if ParamStr( i ) = 'mono' then
      begin
{$IFDEF BASS}
        BASS_SoundFlags := BASS_SoundFlags or BASS_DEVICE_MONO;
{$ELSE}
        Channels := 1;
{$ENDIF}
{$IFDEF LOG}
        log.LogStatus( 'User selected mono audio format in command line',
          'CheckCommandLine' );
{$ENDIF}
      end;
      if ParamStr( i ) = 'nosound' then
      begin
        SoundEnabled := false;
{$IFDEF LOG}
        log.LogStatus( 'User selected silence', 'CheckCommandLine' );
{$ENDIF}
      end;
    end;
  end
  else
  begin
{$IFDEF LOG}
    log.LogStatus( 'Command line not found', 'CheckCommandLine' );
{$ENDIF}
  end;
end;

procedure Finalize;
var
  i : integer;
begin
{$IFDEF LOG}
  log.LogStatus( 'Starting finalization', 'Finalize' );
{$ENDIF}
{$IFDEF BASS}
  for i := 1 to NumberOfSounds do
    BASS_SampleFree( Sounds[i] );
  BASS_Free( );
{$ELSE}
  if SoundEnabled then
  begin
    for i := 1 to NumberOfSounds do
      if Sounds[i] <> nil then
        Mix_FreeChunk( Sounds[i] );
    Mix_CloseAudio;
  end;  
{$ENDIF}
  if SpriteEngine <> nil then
    SpriteEngine.Free;
  if Background <> nil then
    SDL_FreeSurface( Background );
  if Tiles <> nil then
    SDL_FreeSurface( Tiles );
  if BackTiles <> nil then
    SDL_FreeSurface( BackTiles );
  if Chars15x16 <> nil then
    SDL_FreeSurface( Chars15x16 );
  if Chars19x13 <> nil then
    SDL_FreeSurface( Chars19x13 );
  if GreenTile <> nil then
    SDL_FreeSurface( GreenTile );
  if RedTile <> nil then
    SDL_FreeSurface( RedTile );
  if Screen <> nil then
    SDL_FreeSurface( Screen );
  SDL_Quit;
{$IFDEF LOG}
  log.LogStatus( 'Finalization went OK', 'Finalize' );
{$ENDIF}
end;

procedure RestartMusic;
begin
  // This is a hook for restarting music automatically when it ends
{$IFNDEF BASS}
  if Mix_PlayingMusic = 0 then
    Mix_PlayMusic( music, 0 );
{$ENDIF}
{$IFDEF LOG}
  log.LogStatus( 'RestartMusic HOOK executed', 'RestartMusic' );
{$ENDIF}
end;

function MakeDword(A, B: Word): cardinal;
begin
  Result := A or B shl 16;
end;

procedure Initialize;
const
  LoadImageError = 'Couldn''t load image : %s';
  StrInit = 'Initialize';
var
  TempFlag: cardinal;
  PaletteLoaded : boolean;
  i : integer;
begin
  SoundEnabled := true;
{$IFDEF LOG}
  log.LogStatus( 'Starting Initialization', StrInit );
{$ENDIF}
  CheckCommandLine;
  TempFlag := 0;
{$IFNDEF BASS}
  // If we use SDL_MIXER and sound is enabled then init audio
  if SoundEnabled then
    TempFlag := SDL_INIT_AUDIO;
{$ENDIF}
  if SDL_Init( SDL_INIT_VIDEO or TempFlag ) = -1 then
  begin
{$IFDEF LOG}
    log.LogError( 'SDL init failed', StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end;

  // Set Audio before SDL_SetVideoMode is called
  if SoundEnabled then
  begin
{$IFDEF BASS}
// BASS Sound stuff
{$IFDEF LOG}
    log.LogStatus( 'Initializing BASS...', StrInit );
{$ENDIF}
    if BASS_GetVersion( ) = MakeDword( 1, 1 ) then
    begin
      Handle := 0;
      if BASS_SoundFlags <> 0 then
      begin
        if not BASS_Init( -1, Freq, BASS_SoundFlags, handle ) then { User selected }
          BASS_Init( -2, 44100, 0, handle ); { no sound }
      end
      else
      begin
        if not BASS_Init( -1, 44100, 0, handle ) then { 44100hz 16 bits stereo }
          if not BASS_Init( -1, 22050, 0, handle ) then { 22050hz 16 bits stereo }
            if not BASS_Init( -1, 22050, BASS_DEVICE_8BITS, handle ) then
              { 22050hz 8 bits stereo }
              if not BASS_Init( -1, 22050, BASS_DEVICE_MONO or BASS_DEVICE_8BITS,
                handle ) then
              begin { 22050hz 8 bits mono }
                BASS_Init( -2, 44100, 0, handle ); { no sound }
{$IFDEF LOG}
                log.LogWarning(
                  'BASS: auto selected audiomode initialization failed', StrInit
                  );
                SoundEnabled := false;
{$ENDIF}
              end;
      end;
      if SoundEnabled then
      begin
        BASS_Start;
{$IFDEF LOG}
        log.LogStatus( 'BASS: starting was successful', StrInit );
{$ENDIF}
        for i := 1 to NumberOfSounds do
        begin
          Sounds[i] := BASS_SampleLoad(false, PChar(SoundDir + SoundFilenames[i]), 0, 0, 3, 0);
{$IFDEF LOG}
          if Sounds[i] = 0 then
            Log.LogWarning('BASS: Can''t load sound: ' + SoundDir + SoundFilenames[i], StrInit);
{$ENDIF}
        end;
      end;
    end;

{$ELSE}

// SDL_MIXER Sound stuff
    if Mix_OpenAudio( Freq, AudioFormat, Channels, 1024 ) < 0 then
    begin
{$IFDEF LOG}
      log.LogWarning( 'SDL_MIXER: Initialization failed', StrInit );
{$ENDIF}
      // If Audio initialization failed then turn off audio
      SoundEnabled := false;
    end
    else
    begin
{$IFDEF LOG}
      log.LogStatus( 'SDL_MIXER Initialization was successful', StrInit );
{$ENDIF}
      Mix_HookMusicFinished( @RestartMusic );
{$IFDEF LOG}
      log.LogStatus( 'SDL_MIXER: hooking RestartMusic was successful',
        StrInit );
{$ENDIF}
      // Loading sound effects
      for i := 1 to NumberOfSounds do
      begin
        Sounds[i] := Mix_LoadWav(PChar(SoundDir + SoundFilenames[i]));
{$IFDEF LOG}
        if Sounds[i] = nil then
          Log.LogWarning('SDL_MIXER: Can''t load sound: ' + SoundDir + SoundFilenames[i], StrInit);
{$ENDIF}
      end;
    end;
{$ENDIF}
  end;

  SDL_WM_SetCaption( 'Oxygene ' + Version_no + ' by KiCHY dESiGN', nil );
  SDL_WM_SetIcon( IMG_Load( GfxDir + 'Icon.png' ), 0 );
  if bpp = 0 then
  begin // Select best colordepth
{$IFDEF LOG}
    log.LogStatus( 'Selecting automatically best videomode...', StrInit );
{$ENDIF}
    // One of these values MUST be good.
    bpp := SDL_VideoModeOk( 640, 480, 15, 0 );
    if bpp = 0 then
      bpp := SDL_VideoModeOk( 640, 480, 16, 0 );
    if bpp = 0 then
      bpp := SDL_VideoModeOk( 640, 480, 24, 0 );
    if bpp = 0 then
      bpp := SDL_VideoModeOk( 640, 480, 32, 0 );
    if bpp = 0 then
      bpp := SDL_VideoModeOk( 640, 480, 8, 0 );
{$IFDEF LOG}
    log.LogStatus( 'Selected videomode: ' + inttostr( bpp ) + ' bits per pixel',
      StrInit );
{$ENDIF}
  end;
  // Try to set videomode
  if bpp in [8, 15, 16, 24, 32] then
    Screen := SDL_SetVideoMode( 640, 480, bpp, SDL_SWSURFACE or SDL_HWPALETTE or
      FullScreen );
  if Screen = nil then
  begin
    // But it was unsuccessful
{$IFDEF LOG}
    log.LogError( 'Setting video mode 640x480 failed', StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
  begin
{$IFDEF LOG}
    log.LogStatus( 'Setting video mode 640x480 was successful', StrInit );
{$ENDIF}
  end;

  // Try to load palette
  if BPP = 8 then
    if LoadPalette = false then
    begin
      PaletteLoaded := false;
      log.LogWarning( Format( 'Couldn''t load palette file : %s', [SDL_GetError]
        ), StrInit );
    end
    else
    begin
      PaletteLoaded := true;
      SDL_SetColors( Screen, @Palette, 0, 256 );
    end;

{$IFDEF LOG}
  log.LogStatus( 'Creating surfaces...', StrInit );
{$ENDIF}

  // Try to create the background surface
  Background := SDL_CreateRGBSurface( SDL_SWSURFACE or SDL_HWPALETTE, 640, 480,
    bpp, 0, 0, 0, 0 );
  if Background = nil then
  begin
{$IFDEF LOG}
    log.LogError( Format( 'Creating background surface failed : %s',
      [SDL_GetError] ), StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
  begin
    // Convert it to the current pixel format for fast blitting
    Background := SDL_DisplayFormat( Background );
    // Set its palette (no effect if not 8 bits color mode)
    if BPP = 8 then
    begin
      if PaletteLoaded then
        SDL_SetColors( Background, @Palette, 0, 256 )
      else
        SDL_SetColors( Background, @Screen.format.Palette, 0, 256 );
    end;
  end;

  // Try to load the Tile graphics
  Tiles := IMG_Load( GfxDir + 'Tiles.png' );
  if Tiles = nil then
  begin
{$IFDEF LOG}
    log.LogError( Format( LoadImageError, [SDL_GetError] ), StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
  begin
    // Convert it to the current pixel format for fast blitting
    Tiles := SDL_DisplayFormat( Tiles );
    SDL_SetColorKey( Tiles, SDL_SRCCOLORKEY or SDL_HWACCEL, SDL_MapRGB(
      Tiles.format, 255, 0, 255 ) );
  end;

  // Try to load the background tile graphics
  BackTiles := IMG_Load( GfxDir + 'BackTiles.png' );
  if BackTiles = nil then
  begin
{$IFDEF LOG}
    log.LogError( 'Loading & creating BACKTILES surface failed', StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
    // Convert it to the current pixel format for fast blitting
    BackTiles := SDL_DisplayFormat( BackTiles );

  // Try to load the 15x16 charset
  Chars15x16 := IMG_Load( GfxDir + 'Chars_15x16.png' );
  if Chars15x16 = nil then
  begin
{$IFDEF LOG}
    log.LogError( 'Loading & creating CHARS15X16 surface failed', StrInit
      );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
  begin
    // Convert it to the current pixel format for fast blitting
    Chars15x16 := SDL_DisplayFormat( Chars15x16 );
    SDL_SetColorKey( Chars15x16, SDL_SRCCOLORKEY or SDL_HWACCEL, SDL_MapRGB(
      Chars15x16.format, 255, 0, 255 ) );
  end;

  // Try to load the 19x13 charset
  Chars19x13 := IMG_Load( GfxDir + 'Chars_19x13.png' );
  if Chars19x13 = nil then
  begin
{$IFDEF LOG}
    log.LogError( 'Loading & creating CHARS19X13 surface failed', StrInit
      );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
  begin
    Chars19x13 := SDL_DisplayFormat( Chars19x13 );
    SDL_SetColorKey( Chars19x13, SDL_SRCCOLORKEY or SDL_HWACCEL, SDL_MapRGB(
      Chars19x13.format, 255, 0, 255 ) );
  end;

  // Try to load the green tile
  GreenTile := IMG_Load( GfxDir + 'WellBackTile.png' );
  if GreenTile = nil then
  begin
{$IFDEF LOG}
    log.LogError( 'Loading & creating GREENTILE surface failed', StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
    // Convert it to the current pixel format for fast blitting
    GreenTile := SDL_DisplayFormat( GreenTile );

  // Try to load the red tile
  RedTile := IMG_Load( GfxDir + 'OverBackTile.png' );
  if RedTile = nil then
  begin
{$IFDEF LOG}
    log.LogError( 'Loading & creating REDTILE surface failed', StrInit );
{$ENDIF}
    Finalize;
    Halt( 1 );
  end
  else
    // Convert it to the current pixel format for fast blitting
    RedTile := SDL_DisplayFormat( RedTile );

{$IFDEF LOG}
  log.LogStatus( 'Creating surfaces was successful', StrInit );
{$ENDIF}

  SDL_ShowCursor( SDL_DISABLE );
  SpriteEngine := TSpriteEngine.Create( Screen );
  SpriteEngine.BackgroundSurface := Background;

{$IFDEF LOG}
  log.LogStatus( 'Initialization was successful', StrInit );
{$ENDIF}
end;

procedure LoadCavern( Cavern : integer );
var
  Diamond : TDiamond;
  Whale : TWhale;
  Medusa : TMedusa;
  Spikie : TSpikie;
  Plant : TPlant;
  Rock : TRock;
  f : text;
  Line : string;
  x, y : integer;
  filename : string;
begin
  NumOfDiamonds := 0;
  fillchar( GameMap, sizeof( GameMap ), 0 );
  filename := 'Caverns/' + inttostr( Cavern ) + '.lev';
  if not fileexists( filename ) then
  begin
{$IFDEF LOG}
    Log.LogError( 'File not found: ' + filename, 'LoadCavern' );
{$ENDIF}
    exit;
  end;
  assignfile( f, filename );
  reset( f );
  for y := 0 to 29 do
  begin
    readln( f, Line );
    for x := 0 to length( Line ) - 1 do
      case Line[x + 1] of
        'D' :
          begin
            // Add a diamond to the object list of map
            Diamond := TDiamond.Create;
            Diamond.x := x shl 4 - 3;
            Diamond.y := y shl 4 + 4;
            SpriteEngine.AddSprite( Diamond );
            inc( NumOfDiamonds );
          end;
        'W' :
          begin
            // Add a whale to the object list of map
            Whale := TWhale.Create;
            Whale.x := x shl 4;
            Whale.y := y shl 4;
            SpriteEngine.AddSprite( Whale );
          end;
        'M' :
          begin
            // Add a medusa to the object list of map
            Medusa := TMedusa.Create;
            Medusa.x := x shl 4 - 1;
            Medusa.y := y shl 4;
            SpriteEngine.AddSprite( Medusa );
          end;
        'R' :
          begin
            // Add a rock to the object list of map
            Rock := TRock.Create;
            Rock.x := x shl 4;
            Rock.y := y shl 4;
            SpriteEngine.AddSprite( Rock );
          end;
        'S' :
          begin
            // Add a spikie to the object list of map
            Spikie := TSpikie.Create;
            Spikie.x := x shl 4 - 4;
            Spikie.y := y shl 4 - 2;
            SpriteEngine.AddSprite( Spikie );
          end;
        'P' :
          begin
            // Add a plant to the object list of map
            Plant := TPlant.Create;
            Plant.x := x shl 4;
            Plant.y := y shl 4 - 8;
            SpriteEngine.AddSprite( Plant );
          end;
        'a'..'d', 'i'..'l', 'q'..'t', 'y', 'z', '0', '1' :
          begin
            GameMap[x, y] := pos( Line[x + 1], Codes ) - 1;
            if y < 29 then
              GameMap[x, y + 1] := pos( Line[x + 1], Codes ) - 1 + 4;
          end;
      else
        if Line[x + 1] <> ' ' then
          GameMap[x, y] := pos( Line[x + 1], Codes ) - 1;
      end;
  end;
  // read initial coords of player
  if not eof( f ) then
    readln( f, PlayerStartX, PlayerStartY )
  else
  begin // This never should be happened
    PlayerStartX := 1;
    PlayerStartY := 1;
  end;
  // read amount of available oxygene level
  if not eof( f ) then
    readln( f, MaxO2 )
  else
    MaxO2 := 100; // This never should be happened
  closefile( f );
{$IFDEF LOG}
  Log.LogStatus( 'Loading was successful: ' + filename, 'Initialize' );
{$ENDIF}
end;

procedure WriteText_19x13( Surf : PSDL_Surface; const Txt : string; x : integer;
  Align : TTextAlign );
var
  StartX, i : integer;
  SrcRect, DestRect : TSDL_Rect;
begin
  StartX := 0;
  SrcRect.y := 0;
  SrcRect.w := 19;
  SrcRect.h := 13;
  DestRect.y := 0;
  // Calculate left position of the whole text
  case Align of
    alLeft : StartX := x;
    alCenter : StartX := x - length( txt ) * 10;
    alRight : StartX := x - length( txt ) * 20 + 1;
  end;
  for i := 0 to Length( txt ) - 1 do
  begin
    SrcRect.x := pos( Txt[i + 1], Chars ) * 19 - 19;
    DestRect.x := StartX + i * 20;
    SDL_UpperBlit( Chars19x13, @SrcRect, Surf, @DestRect );
  end;
end;

procedure DrawBackground;
var
  SrcRect, DestRect : TSDL_Rect;
  al, x, y : integer;
begin
  SrcRect.x := BackTileType * 64;
  SrcRect.y := 0;
  SrcRect.w := 64;
  SrcRect.h := 64;
  for y := 0 to 7 do
    for x := 0 to 9 do
    begin
      DestRect.x := x shl 6;
      DestRect.y := y shl 6;
      SDL_UpperBlit( BackTiles, @SrcRect, Background, @DestRect );
    end;
  if bpp > 8 then
  begin
    al := 128;
    for y := 0 to 29 do
    begin
      for x := 0 to 4 do
        if al > 0 then
        begin
          DestRect.x := x * 128;
          DestRect.y := y * 16;
          SDL_SetAlpha( GreenTile, SDL_SRCALPHA, al );
          SDL_UpperBlit( GreenTile, nil, Background, @DestRect );
        end;
      al := al - 4;
    end;
  end;
  SrcRect.w := 16;
  SrcRect.h := 16;
  SrcRect.y := 0;
  for y := 0 to 29 do
    for x := 0 to 39 do
    begin
      if GameMap[x, y] > 0 then
      begin
        SrcRect.x := GameMap[x, y] shl 4 - 16;
        DestRect.x := x shl 4;
        DestRect.y := y shl 4;
        SDL_UpperBlit( Tiles, @SrcRect, Background, @DestRect );
      end;
    end;
end;

procedure BubbleBirth;
var
  Bubble : TBubble;
  x, y : integer;
  i : integer;
begin
  // Search a position for birthing bubble
  for i := 1 to 100 do
  begin
    x := random( 40 );
    y := 1 + random( 28 );
    if ( MapID[GameMap[x, y]] = 0 ) and ( MapID[GameMap[x, y + 1]] = 1 ) then
    begin
      Bubble := TBubble.Create;
      Bubble.x := x * 16;
      Bubble.y := y * 16;
      SpriteEngine.AddSprite( Bubble );
      exit;
    end;
  end;
end;

procedure WriteText_15x16( Engine : TSpriteEngine; const Txt : string; x, y :
  integer; Align : TTextAlign );
var
  StartX, i : integer;
  Letter : TLetter;
begin
  StartX := 0;
  // Calculate left position of the whole text
  case Align of
    alLeft : StartX := x;
    alCenter : StartX := x - length( txt ) * 8;
    alRight : StartX := x - length( txt ) * 16;
  end;
  for i := 0 to Length( txt ) - 1 do
  begin
    Letter := TLetter.Create( Chars15x16, Txt[i + 1], 15, 16 );
    Letter.x := StartX + i * 16;
    Letter.y := y;
    Engine.AddSprite( Letter );
  end;
end;

procedure WriteScroll( const Txt : string; y : integer );
var
  i : integer;
  SwimmingLetter : TSwimmingLetter;
begin
  for i := 0 to Length( txt ) - 1 do
  begin
    SwimmingLetter := TSwimmingLetter.Create( Chars15x16, Txt[i + 1], 15, 16 );
    SwimmingLetter.x := 640 + i * 16;
    SwimmingLetter.y := y;
    SpriteEngine.AddSprite( SwimmingLetter );
  end;
end;

procedure UpdatePanel;
var
  i : integer;
  Str : string[6];
begin
  LivesRect.x := pos( IntToStr( Lives ), Chars ) * 19 - 19;
  LivesRect.y := 0;
  LivesRect.w := 19;
  LivesRect.h := 13;
  Str := inttostr( O2 ) + '   ';
  for i := 0 to 2 do
  begin
    O2Rects[i].x := pos( Str[i + 1], Chars ) * 19 - 19;
    O2Rects[i].y := 0;
    O2Rects[i].w := 19;
    O2Rects[i].h := 13;
  end;
  Str := inttostr( Score ) + '     ';
  for i := 0 to 5 do
  begin
    ScoreRects[i].x := pos( Str[i + 1], Chars ) * 19 - 19;
    ScoreRects[i].y := 0;
    ScoreRects[i].w := 19;
    ScoreRects[i].h := 13;
  end;
end;

procedure DrawPanel;
var
  Dest : TSDL_Rect;
  i : integer;
begin
  Dest.y := 0;
  Dest.h := 13;
  // lives
  Dest.x := 108;
  Dest.w := 19;
  SDL_UpperBlit( Background, @Dest, Screen, @Dest );
  SDL_UpperBlit( Chars19x13, @LivesRect, Screen, @Dest );
  // Oxygene
  Dest.x := 255;
  Dest.w := 59;
  SDL_UpperBlit( Background, @Dest, Screen, @Dest );
  Dest.w := 19;
  for i := 0 to 2 do
  begin
    Dest.x := 255 + i * 20;
    if O2Rects[i].x >= 0 then
      SDL_UpperBlit( Chars19x13, @O2Rects[i], Screen, @Dest );
  end;
  // Score
  Dest.x := 520;
  Dest.w := 119;
  SDL_UpperBlit( Background, @Dest, Screen, @Dest );
  Dest.w := 19;
  for i := 0 to 5 do
  begin
    Dest.x := 520 + i * 20;
    if ScoreRects[i].x >= 0 then
      SDL_UpperBlit( Chars19x13, @ScoreRects[i], Screen, @Dest );
  end;
end;

procedure LoadMusic( const Filename : string );
const
  StrLoad = 'LoadMusic';
begin
  if not SoundEnabled then
    exit; // Sound enabled?
{$IFDEF BASS}
  Music := BASS_MusicLoad( FALSE, pchar( Filename ), 0, 0, BASS_MUSIC_RAMPS or
    BASS_MUSIC_LOOP );
  if Music = 0 then
  begin
{$IFDEF LOG}
    Log.LogError( 'BASS: loading ' + filename + ' failed', StrLoad );
{$ENDIF}
  end
  else
  begin
{$IFDEF LOG}
    Log.LogStatus( 'BASS: loading ' + filename + ' was successful', StrLoad );
{$ENDIF}
  end;
{$ELSE}
  music := Mix_LoadMUS( PChar( Filename ) );
  if Music = nil then
  begin
{$IFDEF LOG}
    Log.LogError( 'SDL_MIXER: loading ' + filename + ' failed', StrLoad );
{$ENDIF}
  end
  else
  begin
{$IFDEF LOG}
    Log.LogStatus( 'SDL_MIXER: loading ' + filename + ' was successful',
      StrLoad );
{$ENDIF}
  end;
{$ENDIF}
end;

procedure StartMusic;
begin
  if not SoundEnabled then
    exit; // Sound enabled?
{$IFDEF BASS}
  BASS_MusicPlay( Music );
{$ELSE}
  Mix_PlayMusic( music, 0 );
{$ENDIF}
end;

procedure StopMusic;
begin
  if not SoundEnabled then
    exit; // Sound enabled?
{$IFDEF BASS}
  BASS_ChannelStop( Music );
  BASS_MusicFree( Music );
{$ELSE}
  Mix_FreeMusic( Music );
{$ENDIF}
  Music := nil; // It's not neccessary but who knows...?
end;

procedure Title;
const
  Logo = 'OXYGENE';
  MaxLines = 13;
  // This is the scroll text
  Scroll : array[0..12] of string = (
    'welcome to oxygene! this arcade game was written by kichy design.',
    'use cursor keys to move clyde',
    'collect all diamonds in all caverns',
    'press esc to quit',
    'credits:',
    'programming and coding:',
    'kichy design',
    'kichy design: robert kisnemeth',
    'email: mikrobi@freemail.hu',
    'all musics were written by worrior',
    'worrior: zoltan toth',
    'email: worrior@freemail.hu',
    'graphics: some from commodore 64 and some was drawn by kichy' );
var
  LogoLetter : TLogoLetter;
  Fish : TFish;
  i : integer;
  Event : TSDL_Event;
  QuitBubble : TQuitBubble;
  ScrollCounter, LineCounter : cardinal;
begin
  BackTileType := 1;
  QuitBubble := nil;
  ScrollCounter := 0;
  LineCounter := 0;
  LoadCavern( 0 );
  // Build the screen
  WriteText_15x16( SpriteEngine, 'kichy design presents', 320, 10, alCenter );
  for i := 0 to 6 do
  begin
    LogoLetter := TLogoLetter.Create( GfxDir + 'Logo_' + Logo[i + 1] + '.png',
      72, 120 );
    LogoLetter.x := ( 640 - 552 ) div 2 + i * 80;
    LogoLetter.y := 40 + random( 10 );
    SpriteEngine.AddSprite( LogoLetter );
  end;
  WriteText_15x16( SpriteEngine, 'a game from hungary', 320, 185, alCenter );
  WriteText_15x16( SpriteEngine, Version_no + ' finished on 10.2001', 320, 205,
    alCenter );
  WriteText_15x16( SpriteEngine, 'today"s high score:' + inttostr( HighScore ),
    320, 260, alCenter );
  WriteText_15x16( SpriteEngine, 'press space to play', 320, 370, alCenter );
  WriteText_15x16( SpriteEngine, 'email:mikrobi@freemail.hu', 320, 440, alCenter
    );
  DrawBackground;
  SDL_UpperBlit( Background, nil, Screen, nil );
  SpriteEngine.Draw;
  LoadMusic( TitleMusic );
  StartMusic;
  repeat
    SpriteEngine.Move;
    if Random( 100 ) = 0 then
      // Put a bubble
      BubbleBirth;
    if Random( 200 ) = 0 then
    begin
      // Put a fish
      Fish := TFish.Create;
      SpriteEngine.AddSprite( Fish );
    end;
    if ScrollCounter = 0 then
    begin
      ScrollCounter := length( Scroll[LineCounter] ) * 10 + 100;
      WriteScroll( Scroll[LineCounter], 315 );
      inc( LineCounter );
      if LineCounter = MaxLines then
        LineCounter := 0;
    end
    else
      dec( ScrollCounter );
    while SDL_PollEvent( @Event ) > 0 do
    begin
      case Event.key.type_  of
        SDL_KeyDown :
        begin
          if Event.key.keysym.sym = SDLK_SPACE then
            NextScene := scGame
          else if QuitBubble <> nil then
          begin
            if ( ( Event.key.keysym.sym = SDLK_ESCAPE ) or ( Event.key.keysym.sym
              = SDLK_N ) ) then
            begin
              SpriteEngine.RemoveSprite( QuitBubble );
              QuitBubble.Free;
              QuitBubble := nil;
            end
            else if ( Event.key.keysym.sym = SDLK_Y ) or ( Event.key.keysym.sym =
              SDLK_RETURN ) then
              NextScene := scQuitApp;
          end
          else if ( Event.key.keysym.sym = SDLK_ESCAPE ) then
          // QuitBubble=nil
          begin
            QuitBubble := TQuitBubble.Create;
            SpriteEngine.AddSprite( QuitBubble );
            SpriteEngine.SortSprites;
          end;
        end;
      end;
    end;
    SpriteEngine.Draw;
    SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    SDL_Delay( TimeLeft );
  until NextScene <> scTitle;
  // Blank screen
  SDL_FillRect( Screen, nil, 0 );
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  SpriteEngine.Clear;
  StopMusic;
end;

procedure Pause;
var
  Buffer : PSDL_Surface;
  Quit : boolean;
  Event : TSDL_Event;
  Text : TSpriteEngine;
begin
  Quit := false;
  // Create a buffer with the current display format, so we don't need to use SDL_DisplayFormat()
  Buffer := SDL_CreateRGBSurface( SDL_SWSURFACE, 640, 480,
    Screen.format.BitsPerPixel, Screen.format.RMask, Screen.format.GMask,
    Screen.format.BMask, Screen.format.AMask );
  // Build the screen
  SDL_SetAlpha( Screen, SDL_SRCALPHA, 128 );
  SDL_UpperBlit( Screen, nil, Buffer, nil );
  SDL_SetAlpha( Screen, 0, 0 );
  SDL_UpperBlit( Buffer, nil, Screen, nil );
  Text := TSpriteEngine.Create( Screen );
  Text.BackgroundSurface := Buffer;
  WriteText_15x16( Text, 'the game is now paused.', 320, 240 - 8 - 20, alCenter
    );
  WriteText_15x16( Text, 'drink a coffee then hit space', 320, 240 - 8, alCenter
    );
  WriteText_15x16( Text, 'to continue playing.', 320, 240 - 8 + 20, alCenter );
  Text.Draw;
  repeat
    while SDL_PollEvent( @Event ) > 0 do
      if Event.key.type_ = SDL_KeyDown then
        if Event.key.keysym.sym = SDLK_SPACE then
          Quit := true;
    Text.Move;
    Text.Draw;
    SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    SDL_Delay( TimeLeft );
  until Quit = true;
  // Blank screen
  SDL_SetAlpha( Buffer, 0, 0 );
  SDL_UpperBlit( Buffer, nil, Screen, nil );
  SDL_FreeSurface( Buffer );
  Text.Free;
  SDL_UpperBlit( Background, nil, Screen, nil );
end;

procedure Map;
const
  MapDir : array[0..4, 0..3] of byte = ( ( 2, 6, 3, 2 ),
    ( 12, 11, 12, 11 ),
    ( 6, 9, 4, 11 ),
    ( 10, 4, 3, 10 ),
    ( 12, 5, 13, 9 ) );
var
  Buffer : PSDL_Surface;
  Event : TSDL_Event;
  Text : TSpriteEngine;
  FakePlayer : TFakePlayer;
  Distance, xi, yi, mx, my, kx, ky : integer;
  L, R, U, D : boolean;
  c : char;
  Map : TSprite;
begin
  LoadMusic( NextMusic );
  StartMusic;
  //L := false;
 // R := false;
 // U := false;
 // D := false;
  Buffer := SDL_CreateRGBSurface( SDL_SWSURFACE, 640, 480,
    Screen.format.BitsPerPixel, Screen.format.RMask, Screen.format.GMask,
    Screen.format.BMask, Screen.format.AMask );
  // Build screen
  if bpp > 8 then
  begin
    SDL_SetAlpha( Screen, SDL_SRCALPHA, 128 );
    SDL_UpperBlit( Screen, nil, Buffer, nil );
    SDL_SetAlpha( Screen, 0, 0 );
  end;
  SDL_UpperBlit( Buffer, nil, Screen, nil );
  Text := TSpriteEngine.Create( Screen );
  Text.BackgroundSurface := Buffer;
  WriteText_15x16( Text, 'you have collected all diamonds', 320, 112, alCenter
    );
  WriteText_15x16( Text, 'in this cavern. well done!', 320, 132, alCenter );
  WriteText_15x16( Text, 'choose the next cavern!', 320, 334, alCenter );
  mx := Cavern mod 5;
  my := Cavern div 5;
  Rooms[mx, my] := true;
  for ky := 0 to 3 do
    for kx := 0 to 4 do
    begin
      if Rooms[kx, ky] = true then
        c := '/'
      else
        c := '?';
      WriteText_15x16( Text, c, 240 + 8 + kx * 32, 176 + 8 + ky * 32, alLeft );
    end;
  FakePlayer := TFakePlayer.Create;
  FakePlayer.x := 240 + mx * 32 + 2;
  FakePlayer.y := 176 + my * 32 + 3;
  Text.AddSprite( FakePlayer );
  Map := TSprite.Create( GfxDir + 'Map.png', 181, 152 );
  Map.x := 230;
  Map.y := 163;
  Map.z := 0;
  Text.AddSprite( Map );
  Text.SortSprites;
  Text.Draw;
  repeat
    Distance := 0;
    if ( MapDir[mx, my] and 1 ) = 1 then
      U := true
    else
      U := false;
    if ( MapDir[mx, my] and 2 ) = 2 then
      R := true
    else
      R := false;
    if ( MapDir[mx, my] and 4 ) = 4 then
      D := true
    else
      D := false;
    if ( MapDir[mx, my] and 8 ) = 8 then
      L := true
    else
      L := false;
    xi := 0;
    yi := 0;
    repeat
      if ( xi = 0 ) and ( yi = 0 ) then
      begin
        while SDL_PollEvent( @Event ) > 0 do
          if Event.key.type_ = SDL_KeyDown then
          begin
            if ( Event.key.keysym.sym = SDLK_LEFT ) and ( L = true ) then
            begin
              xi := -1;
              FakePlayer.Direction := 0;
            end;
            if ( Event.key.keysym.sym = SDLK_RIGHT ) and ( R = true ) then
            begin
              xi := 1;
              FakePlayer.Direction := 2;
            end;
            if ( Event.key.keysym.sym = SDLK_UP ) and ( U = true ) then
              yi := -1;
            if ( Event.key.keysym.sym = SDLK_DOWN ) and ( D = true ) then
              yi := 1;
          end;
      end
      else
      begin
        inc( Distance );
        FakePlayer.x := FakePlayer.x + xi;
        FakePlayer.y := FakePlayer.y + yi;
      end;
      Text.Move;
      Text.Draw;
      SDL_UpdateRect( Screen, 0, 0, 0, 0 );
      SDL_Delay( TimeLeft );
    until Distance = 32;
    mx := mx + xi;
    my := my + yi;
  until Rooms[mx, my] = false;
  // The new level
  Cavern := my * 5 + mx;
  SDL_FreeSurface( Buffer );
  Text.Free;
  StopMusic;
end;

procedure Game;
var
  Player : TPlayer;
  Fish : TFish;
  O2Counter : integer;
  PlayMusic, NeedLoadMusic : boolean;
  Event : TSDL_Event;
  NumOfCompletedCaverns : integer;
begin
  Lives := 3;
  Score := 0;
  Cavern := random( 20 ); // Starting cavern
  NumOfCompletedCaverns := 0;
  fillchar( rooms, sizeof( rooms ), false );
  NeedLoadMusic := true;
  PlayMusic := false;
  BackTileType := random( 4 );

  repeat // One life
    repeat // One Cavern
      SpriteEngine.Clear;
      LoadCavern( Cavern + 1 );
      Player := TPlayer.Create;
      Player.x := PlayerStartX * 16 + 2;
      Player.y := PlayerStartY * 16 + 3;
      SpriteEngine.AddSprite( Player );
      O2 := MaxO2;
      O2Counter := 0;
      DrawBackground;
      WriteText_19x13( Background, 'lives', 0, alLeft );
      WriteText_19x13( Background, 'O', 227, alLeft );
      WriteText_19x13( Background, 'score', 638 - 7 - 6 * 20, alright );
      SpriteEngine.BackgroundSurface := Background;
      SDL_UpperBlit( Background, nil, Screen, nil );
      SpriteEngine.Draw;
      UpdatePanel;
      if NeedLoadMusic then
      begin
        LoadMusic( IngameMusic );
        NeedLoadMusic := false;
      end;
      if not PlayMusic then
      begin
        PlayMusic := true;
        StartMusic;
      end;
      repeat // Game process
        while SDL_PollEvent( @Event ) > 0 do
          if ( Event.active.State = SDL_APPACTIVE ) or ( Event.active.State =
            SDL_APPINPUTFOCUS ) then
            if Event.active.gain = 0 then
              Pause;
        keys := PKeyStateArr( SDL_GetKeyState( nil ) );
        if Random( 100 ) = 0 then
          BubbleBirth;
        if Random( 200 ) = 0 then
        begin
          Fish := TFish.Create;
          SpriteEngine.AddSprite( Fish );
        end;
        inc( O2Counter );
        if O2Counter = 30 then
        begin
          O2Counter := 0;
          if O2 > 0 then
            dec( O2 )
          else
            Player.Hit;
          UpdatePanel;
        end;
        SpriteEngine.Move;
        DrawPanel;
        SpriteEngine.Draw;
        SDL_UpdateRect( Screen, 0, 0, 0, 0 );
        SDL_Delay( TimeLeft );
        if Keys[SDLK_ESCAPE] = 1 then
          Player.Hit;
        if ( ( Keys[SDLK_LALT] = 1 ) or ( Keys[SDLK_RALT] = 1 ) ) and (
          Keys[SDLK_TAB] = 1 ) then
          Pause
        else if Keys[SDLK_P] = 1 then
          Pause;
        if Keys[SDLK_N] = 1 then
          NumOfDiamonds := 0; // CHEATING!!!
      until ( not PlayerAlive ) or ( NumOfDiamonds = 0 );
        // Cavern finised or player dead
      if PlayerAlive then
      begin // Finish Cavern
        inc( NumOfCompletedCaverns );
        if NumOfCompletedCaverns = MaxCaverns then
          NextScene := scWellDone
        else
        begin
          StopMusic;
          Map;
          NeedLoadMusic := true;
          PlayMusic := false;
          BackTileType := random( 4 );
        end;
      end;
    until ( not PlayerAlive ) or ( NextScene = scWellDone );
    if not PlayerAlive then
    begin
      if Lives > 0 then
        dec( Lives )
      else
        NextScene := scGameOver;
    end;
  until NextScene <> scGame;
  // Blank screen
  SDL_FillRect( Screen, nil, 0 );
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  SpriteEngine.Clear;
  if Score > HighScore then
    HighScore := Score;
  StopMusic;
end;

procedure GameOver;
var
  Quit : boolean;
  Event : TSDL_Event;
  a, x, y, z : integer;
  DestRect : TSDL_Rect;
  Fish : TFish;
begin
  Quit := false;
  BackTileType := 2;
  LoadCavern( 0 );
  // Build screen
  DrawBackground;
  a := 255;
  for y := 0 to 28 do
  begin
    DestRect.y := y * 16;
    x := -random( 128 );
    for z := 0 to 5 do
    begin
      DestRect.x := x + z * 128;
      SDL_SetAlpha( RedTile, SDL_SRCALPHA, a );
      SDL_UpperBlit( RedTile, nil, Background, @DestRect );
    end;
    if bpp > 8 then
      a := a - 8;
  end;
  SDL_UpperBlit( Background, nil, Screen, nil );
  WriteText_15x16( SpriteEngine, 'bad luck!', 320, 240 - 8 - 20, alCenter );
  WriteText_15x16( SpriteEngine, 'game over.', 320, 240 - 8, alCenter );
  WriteText_15x16( SpriteEngine, 'hit space!', 320, 240 - 8 + 20, alCenter );
  SpriteEngine.Draw;
  LoadMusic( GameOverMusic );
  StartMusic;
  repeat
    while SDL_PollEvent( @Event ) > 0 do
      if Event.key.type_ = SDL_KeyDown then
        if Event.key.keysym.sym = SDLK_SPACE then
          Quit := true;
    SpriteEngine.Move;
    if random( 100 ) = 0 then
      BubbleBirth;
    if Random( 200 ) = 0 then
    begin
      Fish := TFish.Create;
      SpriteEngine.AddSprite( Fish );
    end;
    SpriteEngine.Draw;
    SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    SDL_Delay( TimeLeft );
  until Quit = true;
  NextScene := scTitle;
  // Blank screen
  SDL_FillRect( Screen, nil, 0 );
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  SpriteEngine.Clear;
  StopMusic;
end;

procedure WellDone;
var
  Quit : boolean;
  Event : TSDL_Event;
  a, x, y, z : integer;
  DestRect : TSDL_Rect;
  Fish : TFish;
begin
  Quit := false;
  BackTileType := 3;
  LoadCavern( 0 );
  // Build screen
  DrawBackground;
  a := 255;
  for y := 0 to 28 do
  begin
    DestRect.y := y * 16;
    x := -random( 128 );
    for z := 0 to 5 do
    begin
      DestRect.x := x + z * 128;
      SDL_SetAlpha( GreenTile, SDL_SRCALPHA, a );
      SDL_UpperBlit( GreenTile, nil, Background, @DestRect );
    end;
    if bpp > 8 then
      a := a - 8;
  end;
  SDL_UpperBlit( Background, nil, Screen, nil );
  WriteText_15x16( SpriteEngine, 'yeah!!!', 320, 240 - 8 - 20, alCenter );
  WriteText_15x16( SpriteEngine, 'well done!', 320, 240 - 8, alCenter );
  WriteText_15x16( SpriteEngine, 'hit space!', 320, 240 - 8 + 20, alCenter );
  SpriteEngine.Draw;
  LoadMusic( WellDoneMusic );
  StartMusic;
  repeat
    while SDL_PollEvent( @Event ) > 0 do
      if Event.key.type_ = SDL_KeyDown then
        if Event.key.keysym.sym = SDLK_SPACE then
          Quit := true;
    SpriteEngine.Move;
    if random( 100 ) = 0 then
      BubbleBirth;
    if Random( 200 ) = 0 then
    begin
      Fish := TFish.Create;
      SpriteEngine.AddSprite( Fish );
    end;
    SpriteEngine.Draw;
    SDL_UpdateRect( Screen, 0, 0, 0, 0 );
    SDL_Delay( TimeLeft );
  until Quit = true;
  NextScene := scTitle;
  // Blank screen
  SDL_FillRect( Screen, nil, 0 );
  SDL_UpdateRect( Screen, 0, 0, 0, 0 );
  SpriteEngine.Clear;
  StopMusic;
end;

begin
  Randomize;
  initialize;
  // Starting screen
  NextScene := scTitle;
  // Main cycle
  repeat
    case NextScene of
      scTitle : Title;
      scGame : Game;
      scGameOver : GameOver;
      scWellDone : WellDone;
    end;
  until NextScene = scQuitApp;
  Finalize;
end.

