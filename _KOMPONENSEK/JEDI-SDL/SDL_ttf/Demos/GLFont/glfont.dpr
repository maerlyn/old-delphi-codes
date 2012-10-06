program glfont;
{
    glfont:  An example of using the SDL_ttf library with OpenGL.
    Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga
    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.
    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    The SDL_GL_* functions in this file are available in the public domain.
    Sam Lantinga
    slouken@libsdl.org
}
{ $Id: glfont.dpr,v 1.4 2002/04/29 21:37:41 savage Exp $ }
{ A simple program to test the text rendering feature of the TTF library }
uses
  SysUtils,
  SDL,
  SDL_ttf,
  OpenGL12;

(* Undefine this if you want a flat cube instead of a rainbow cube *)
{$DEFINE SHADED_CUBE}

const
  DEFAULT_PTSIZE = 18;
  DEFAULT_TEXT = 'The quick brown fox jumped over the lazy dog';
  NUM_COLORS = 256;
  Usage = 'Usage: %s <font>.ttf [-solid] [-utf8 or -unicode] [-b] [-i] [-u] [-fgcol r g b] [-bgcol r g b]  [-ptsize nn] [-text "message"]';
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 0;

procedure ShutDownApplication( HaltStatus : integer );
begin
  UnLoadOpenGL;
  TTF_Quit;
  SDL_Quit;
  Halt( HaltStatus );
end;

procedure SDL_GL_Enter2DMode;
var
  screen : PSDL_Surface;
begin
  screen := SDL_GetVideoSurface;
  (* Note, there may be other things you need to change,
     depending on how you have your OpenGL state set up.
  *)
  glPushAttrib( GL_ENABLE_BIT );
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  glEnable( GL_TEXTURE_2D );
  glViewport( 0, 0, screen.w, screen.h );
  glMatrixMode( GL_PROJECTION );
  glPushMatrix;
  glLoadIdentity;
  glOrtho( 0.0, screen.w, screen.h, 0.0, 0.0, 1.0 );
  glMatrixMode( GL_MODELVIEW );
  glPushMatrix;
  glLoadIdentity;
  glTexEnvf( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL );
end;

procedure SDL_GL_Leave2DMode;
begin
  glMatrixMode( GL_MODELVIEW );
  glPopMatrix;
  glMatrixMode( GL_PROJECTION );
  glPopMatrix;
  glPopAttrib;
end;
(* Quick utility function for texture creation *)

function power_of_two( input : integer ) : integer;
var
  value : integer;
begin
  value := 1;
  while ( value < input ) do
  begin
    value := value shl 1;
  end;
  result := value;
end;

function SDL_GL_LoadTexture( surface : PSDL_Surface; var texcoord : array of TGlFloat ) :
TGLuint;
var
  texture : TGLuint;
  w, h : integer;
  image : PSDL_Surface;
  area : TSDL_Rect;
  saved_flags : Uint32;
  saved_alpha : Uint8;
begin
  (* Use the surface width and height expanded to powers of 2 *)
  w := power_of_two( surface.w );
  h := power_of_two( surface.h );
  texcoord[ 0 ] := 0.0; (* Min X *)
  texcoord[ 1 ] := 0.0; (* Min Y *)
  texcoord[ 2 ] := surface.w / w; (* Max X *)
  texcoord[ 3 ] := surface.h / h; (* Max Y *)
  image := SDL_CreateRGBSurface(
    SDL_SWSURFACE,
    w, h,
    32,
{$IFDEF IA32} (* OpenGL RGBA masks *)
    $000000FF,
    $0000FF00,
    $00FF0000,
    $FF000000
{$ELSE}
    $FF000000,
    $00FF0000,
    $0000FF00,
    $000000FF
{$ENDIF}
    );
  if ( image = nil ) then
  begin
    result := 0;
    exit;
  end;
  (* Save the alpha blending attributes *)
  saved_flags := surface.flags and ( SDL_SRCALPHA or SDL_RLEACCELOK );
  saved_alpha := surface.format.alpha;
  if ( ( saved_flags and SDL_SRCALPHA ) = SDL_SRCALPHA ) then
  begin
    SDL_SetAlpha( surface, 0, 0 );
  end;
  (* Copy the surface into the GL texture image *)
  area.x := 0;
  area.y := 0;
  area.w := surface.w;
  area.h := surface.h;
  SDL_BlitSurface( surface, @area, image, @area );
  (* Restore the alpha blending attributes *)
  if ( ( saved_flags and SDL_SRCALPHA ) = SDL_SRCALPHA ) then
  begin
    SDL_SetAlpha( surface, saved_flags, saved_alpha );
  end;
  (* Create an OpenGL texture for the image *)
  glGenTextures( 1, @texture );
  glBindTexture( GL_TEXTURE_2D, texture );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
  glTexImage2D( GL_TEXTURE_2D,
    0,
    GL_RGBA,
    w, h,
    0,
    GL_RGBA,
    GL_UNSIGNED_BYTE,
    image.pixels );
  SDL_FreeSurface( image ); (* No longer needed *)
  result := texture;
end;

type
  TRenderType = ( rtLatin1, rtUTF8, rtUnicode );

var
  //fontfile : string;
  gl_error : GLenum;
  texture : TGLuint;
  x, y, w, h : integer;
  //texcoord : array[ 0..4 ] of TGLfloat;
  texMinX, texMinY : TGLfloat;
  texMaxX, texMaxY : TGLfloat;
  color : array[ 0..7, 0..2 ] of single = ( ( 1.0, 1.0, 0.0 ),
    ( 1.0, 0.0, 0.0 ),
    ( 0.0, 0.0, 0.0 ),
    ( 0.0, 1.0, 0.0 ),
    ( 0.0, 1.0, 1.0 ),
    ( 1.0, 1.0, 1.0 ),
    ( 1.0, 0.0, 1.0 ),
    ( 0.0, 0.0, 1.0 ) );
  cube : array[ 0..7, 0..2 ] of single = ( ( 0.5, 0.5, -0.5 ),
    ( 0.5, -0.5, -0.5 ),
    ( -0.5, -0.5, -0.5 ),
    ( -0.5, 0.5, -0.5 ),
    ( -0.5, 0.5, 0.5 ),
    ( 0.5, 0.5, 0.5 ),
    ( 0.5, -0.5, 0.5 ),
    ( -0.5, -0.5, 0.5 ) );
  screen : PSDL_Surface = nil;
  font : PTTF_Font;
  temp : PSDL_Surface = nil;
  ptsize : integer = 0;
  i : integer;
  done : Boolean = false;
  //rdiff, gdiff, bdiff : integer;
  //colors : array[ 0..NUM_COLORS - 1 ] of TSDL_Color;
  white : TSDL_Color = ( r : $FF; g : $FF; b : $FF; unused : 0 );
  black : TSDL_Color = ( r : $00; g : $00; b : $00; unused : 0 );
  forecol : TSDL_Color;
  backcol : TSDL_Color;
  //dstrect : TSDL_Rect;
  event : TSDL_Event;
  rendersolid : Boolean = false;
  renderstyle : integer = TTF_STYLE_NORMAL;
  rendertype : TRenderType = rtLatin1;
  dump : Boolean;
  message : string;
  glyph : PSDL_Surface = nil;
  r, g, b : integer;
  outname : string;
  //texcoord : array[ 0..3 ] of TGLfloat;
  aspect : single;

procedure DrawTextTexture;
var
  //screen : PSDL_Surface;
  image : PSDL_Surface;
  texcoord : array[ 0..3 ] of TGlFloat;
begin
  //screen := SDL_GetVideoSurface;
  if ( texture = 0 ) then
  begin
    case rendertype of
      rtLatin1 :
        begin
          if ( rendersolid ) then
          begin
            image := TTF_RenderText_Solid( font, PChar( message ), forecol );
          end
          else
          begin
            image := TTF_RenderText_Shaded( font, PChar( message ), forecol, backcol );
          end;
        end;

      rtUTF8 :
        begin
          if ( rendersolid ) then
          begin
            image := TTF_RenderUTF8_Solid( font, PChar( message ), forecol );
          end
          else
          begin
            image := TTF_RenderUTF8_Shaded( font, PChar( message ), forecol, backcol );
          end;
        end;

      rtUNICODE :
        begin
          { This doesn't actually work because you can't pass UNICODE text in via command line, AFAIK, but...}
           {Uint16 unicode_text[BUFSIZ];
           int index;
            for index := 0 to Lneght(message) do
            begin
              unicode_text[index] := ((Uint8 *)message)[0];
              unicode_text[index] := unicode_text[index] shl 8;
              unicode_text[index] := unicode_text[index] or ((Uint8 *)message)[1];
              message := message + 2;
            end;
            if ( rendersolid ) then
            begin
              text := TTF_RenderUNICODE_Solid(font, unicode_text, @forecol);
            end
            else
            begin
              text := TTF_RenderUNICODE_Shaded( font, unicode_text, @forecol, @backcol);
            end;}
        end;
    else
      begin
        image := nil;
        { This shouldn't happen }
      end;
    end;

    if ( image = nil ) then
    begin
      //fprintf(stderr, 'Couldn't render text: %s', [SDL_GetError]);
      TTF_CloseFont( font );
      ShutDownApplication( 2 );
    end;


    w := image.w;
    h := image.h;
    (* Convert the image into an OpenGL texture *)
    texture := SDL_GL_LoadTexture( image, texcoord );
    (* Make texture coordinates easy to understand *)
    texMinX := texcoord[ 0 ];
    texMinY := texcoord[ 1 ];
    texMaxX := texcoord[ 2 ];
    texMaxY := texcoord[ 3 ];
    (* We don't need the original image anymore *)
    SDL_FreeSurface( image );
    (* Make sure that the texture conversion is okay *)
    if ( texture = 0 ) then
    begin
      exit;
    end;
  end;

  (* Show the image on the screen *)
  SDL_GL_Enter2DMode;
  glBindTexture( GL_TEXTURE_2D, texture );
  glBegin( GL_TRIANGLE_STRIP );
    glTexCoord2f( texMinX, texMinY );
    glVertex2i( x, y );
    glTexCoord2f( texMaxX, texMinY );
    glVertex2i( x + w, y );
    glTexCoord2f( texMinX, texMaxY );
    glVertex2i( x, y + h );
    glTexCoord2f( texMaxX, texMaxY );
    glVertex2i( x + w, y + h );
  glEnd;
  SDL_GL_Leave2DMode;
end;

begin
  { Default is black and white }
  forecol := black;
  backcol := white;

  for i := 1 to ParamCount - 1 do
  begin
    if ( ParamStr( i ) = '-solid' ) then
    begin
      rendersolid := true;
    end
    else if ( ParamStr( i ) = '-utf8' ) then
    begin
      rendertype := rtUTF8;
    end
    else if ( ParamStr( i ) = '-unicode' ) then
    begin
      rendertype := rtUnicode;
    end
    else if ( ParamStr( i ) = '-b' ) then
    begin
      renderstyle := renderstyle or TTF_STYLE_BOLD;
    end
    else if ( ParamStr( i ) = '-i' ) then
    begin
      renderstyle := renderstyle or TTF_STYLE_ITALIC;
    end
    else if ( ParamStr( i ) = '-u' ) then
    begin
      renderstyle := renderstyle or TTF_STYLE_UNDERLINE;
    end
    else if ( ParamStr( i ) = '-dump' ) then
    begin
      dump := true;
    end
    else if ( ParamStr( i ) = '-fgcol' ) then
    begin
      try
        r := StrToInt( ParamStr( i + 1 ) );
        g := StrToInt( ParamStr( i + 2 ) );
        b := StrToInt( ParamStr( i + 3 ) );
      except
        ShutDownApplication( 1 )
      end;
      forecol.r := r;
      forecol.g := g;
      forecol.b := b;
    end
    else if ( ParamStr( i ) = '-bgcol' ) then
    begin
      try
        r := StrToInt( ParamStr( i + 1 ) );
        g := StrToInt( ParamStr( i + 2 ) );
        b := StrToInt( ParamStr( i + 3 ) );
      except
        ShutDownApplication( 1 )
      end;
      forecol.r := r;
      forecol.g := g;
      forecol.b := b;
    end
    else if ( ParamStr( i ) = '-ptsize' ) then
    begin
      ptsize := StrToInt( ParamStr( i + 1 ) );
    end
    else if ( ParamStr( i ) = '-text' ) then
    begin
      message := ParamStr( i + 1 );
    end
    else
    begin
      //fprintf(stderr, Usage, argv0);
    end;
  end;

  { Check usage }
  {if ( ! argv[ 0 ] ) {
  fprintf(stderr, Usage, argv0);
  return(1);
 }

 { Initialize SDL }
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    //fprintf(stderr, 'Couldn't initialize SDL: %s',SDL_GetError);
    SDL_Quit;
  end;

  LoadOpenGL;

  { Initialize the TTF library }
  if ( TTF_Init < 0 ) then
  begin
    //fprintf(stderr, 'Couldn't initialize TTF: %s',SDL_GetError);
    SDL_Quit;
  end;
  //atexit( TTF_Quit );
  { Open the font file with the requested point size }
  if ( ptsize = 0 ) then
  begin
    //i := 2;
    ptsize := DEFAULT_PTSIZE;
  end
  else
  begin
    //i := 3;
  end;

  font := TTF_OpenFont( PChar( ParamStr( 1 ) ), ptsize );
  if ( font = nil ) then
  begin
    //fprintf(stderr, 'Couldn''t load %d pt font from %s: %s',[ptsize, ParamStr(0), SDL_GetError]);
    ShutDownApplication( 2 )
  end;

  TTF_SetFontStyle( font, renderstyle );

  if ( dump ) then
  begin
    for i := 48 to 122 do
    begin
      glyph := TTF_RenderGlyph_Shaded( font, i, forecol, backcol );
      if ( glyph <> nil ) then
      begin
        outname := 'glyph-' + IntToStr( i ) + '.bmp';
        SDL_SaveBMP( glyph, PChar( outname ) );
      end;
    end;
    ShutDownApplication( 0 );
    exit;
  end;

  { Set a 640x480x8 video mode }
  screen := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, SDL_OPENGL );
  if ( screen = nil ) then
  begin
    //fprintf(stderr, 'Couldn''t set 640x480 OpenGL mode: %s',       SDL_GetError);
    ShutDownApplication( 2 );
  end;

  // Set the window manager title bar
  SDL_WM_SetCaption( 'JEDI-SDL 3D TTF Demo', 'glfont' );

  { Render and center the message }
  if ( message = '' ) then
  begin
    message := DEFAULT_TEXT;
  end;

  { Initialize the GL state }
  glViewport( 0, 0, screen.w, screen.h );
  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;
  // This makes the cube rectangular on screen */
  //glOrtho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0 );
  // This makes the cube equisided on screen */
  aspect := 2.0 / ( screen.w / screen.h );
  glOrtho( -2.0, 2.0, -aspect, aspect, -20.0, 20.0 );
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
  glEnable( GL_DEPTH_TEST );
  glDepthFunc( GL_LESS );
  glShadeModel( GL_SMOOTH );

  { Wait for a keystroke, and blit text on mouse press }
  done := false;
  while ( not done ) do
  begin
    while ( SDL_PollEvent( @event ) <> 0 ) do
    begin
      case event.type_ of
        SDL_MOUSEBUTTONDOWN :
          begin
            x := event.motion.x - w div 2;
            y := event.motion.y - h div 2;
          end;

        SDL_KEYDOWN, SDL_QUITEV :
          begin
            done := true;
          end;
      end;
    end;

    { Clear the screen }
    glClearColor( 1.0, 1.0, 1.0, 1.0 );
    glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );
    { Draw the spinning cube }
    glBegin( GL_QUADS );
{$IFDEF SHADED_CUBE}
    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );
    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );

    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );

    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );

    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 5 ] );
    glVertex3fv( @cube[ 5 ] );
    glColor3fv( @color[ 0 ] );
    glVertex3fv( @cube[ 0 ] );
    glColor3fv( @color[ 3 ] );
    glVertex3fv( @cube[ 3 ] );
    glColor3fv( @color[ 4 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3fv( @color[ 6 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3fv( @color[ 1 ] );
    glVertex3fv( @cube[ 1 ] );
    glColor3fv( @color[ 2 ] );
    glVertex3fv( @cube[ 2 ] );
    glColor3fv( @color[ 7 ] );
    glVertex3fv( @cube[ 7 ] );
{$ELSE} // flat cube
    glColor3f( 1.0, 0.0, 0.0 );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 1 ] );
    glVertex3fv( @cube[ 2 ] );
    glVertex3fv( @cube[ 3 ] );

    glColor3f( 0.0, 1.0, 0.0 );
    glVertex3fv( @cube[ 3 ] );
    glVertex3fv( @cube[ 4 ] );
    glVertex3fv( @cube[ 7 ] );
    glVertex3fv( @cube[ 2 ] );

    glColor3f( 0.0, 0.0, 1.0 );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 6 ] );
    glVertex3fv( @cube[ 1 ] );

    glColor3f( 0.0, 1.0, 1.0 );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 4 ] );
    glVertex3fv( @cube[ 7 ] );
    glVertex3fv( @cube[ 6 ] );
    glColor3f( 1.0, 1.0, 0.0 );
    glVertex3fv( @cube[ 5 ] );
    glVertex3fv( @cube[ 0 ] );
    glVertex3fv( @cube[ 3 ] );
    glVertex3fv( @cube[ 4 ] );
    glColor3f( 1.0, 0.0, 1.0 );
    glVertex3fv( @cube[ 6 ] );
    glVertex3fv( @cube[ 1 ] );
    glVertex3fv( @cube[ 2 ] );
    glVertex3fv( @cube[ 7 ] );
{$ENDIF} (* SHADED_CUBE *)
    glEnd;

    { Rotate the cube }
    glMatrixMode( GL_MODELVIEW );
    glRotatef( 5.0, 1.0, 1.0, 1.0 );
    { Show the text on the screen }
    DrawTextTexture;
    {SDL_GL_Enter2DMode;
    glBindTexture( GL_TEXTURE_2D, texture );
    glBegin( GL_TRIANGLE_STRIP );
      glTexCoord2f( texMinX, texMinY );
      glVertex2i( x, y );
      glTexCoord2f( texMaxX, texMinY );
      glVertex2i( x + w, y );
      glTexCoord2f( texMinX, texMaxY );
      glVertex2i( x, y + h );
      glTexCoord2f( texMaxX, texMaxY );
      glVertex2i( x + w, y + h );
    glEnd;
    SDL_GL_Leave2DMode;}
    { Swap the buffers so everything is visible }
    SDL_GL_SwapBuffers;
  end;
  TTF_CloseFont( font );
  ShutDownApplication( 0 );
end.

