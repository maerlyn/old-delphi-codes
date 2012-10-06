program showfont;
{
    showfont:  An example of using the SDL_ttf library with 2D graphics.
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
    Sam Lantinga     slouken@libsdl.org
}

{
$Id: showfont.dpr,v 1.3 2002/04/18 22:40:05 savage Exp $
}

{
A simple program to test the text rendering feature of the TTF library
}

uses
  SysUtils,
  SDL,
  SDL_ttf;

const
  DEFAULT_PTSIZE = 18;
  DEFAULT_TEXT = 'The quick brown fox jumped over the lazy dog';
  NUM_COLORS = 256;
  Usage = 'Usage: %s <font>.ttf [-solid] [-utf8 or -unicode] [-b] [-i] [-u] [-fgcol r g b] [-bgcol r g b]  [-ptsize nn] [-text "message"]';
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 8;

type
  TRenderType = ( rtLatin1, rtUTF8, rtUnicode );

procedure ShutDownApplication( HaltStatus : integer );
begin
  TTF_Quit;
  SDL_Quit;
  Halt( HaltStatus );
end;

var
  screen : PSDL_Surface = nil;
  font : PTTF_Font;
  text : PSDL_Surface = nil;
  temp : PSDL_Surface = nil;
  ptsize : integer = 0;
  i : integer;
  done : Boolean = false;
  rdiff, gdiff, bdiff : integer;
  colors : array[ 0..NUM_COLORS - 1 ] of TSDL_Color;
  white : TSDL_Color = ( r : $FF; g : $FF; b : $FF; unused : 0 );
  black : TSDL_Color = ( r : $00; g : $00; b : $00; unused : 0 );
  forecol : TSDL_Color;
  backcol : TSDL_Color;
  dstrect : TSDL_Rect;
  event : TSDL_Event;
  rendersolid : Boolean = false;
  renderstyle : integer = TTF_STYLE_NORMAL;
  rendertype : TRenderType = rtLatin1;
  dump : Boolean;
  message : string;
  glyph : PSDL_Surface = nil;
  r, g, b : integer;
  outname : string;
begin
  { Look for special execution mode }
  {dump := false;
  { Look for special rendering types }
  {rendersolid := false;
  renderstyle := TTF_STYLE_NORMAL;
  rendertype := rtLatin1;}

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
  {
  if ( ! argv[0] ) then
  begin
    fprintf(stderr, Usage, argv0);
    return(1);
  end;
  }

 { Initialize SDL }
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    //fprintf(stderr, 'Couldn''t initialize SDL: %s',SDL_GetError);
    ShutDownApplication( 2 )
  end;

  //atexit(SDL_Quit);

  { Initialize the TTF library }
  if ( TTF_Init( ) < 0 ) then
  begin
    //fprintf(stderr, 'Couldn''t initialize TTF: %s',SDL_GetError);
    ShutDownApplication( 2 )
  end;

  //atexit(TTF_Quit);

  { Open the font file with the requested point size }
  if ( ptsize = 0 ) then
  begin
    i := 2;
    ptsize := DEFAULT_PTSIZE;
  end
  else
  begin
    i := 3;
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
    for i := 48 to 123 do
    begin
      glyph := TTF_RenderGlyph_Shaded( font, i, forecol, backcol );
      if ( glyph <> nil ) then
      begin
        //sprintf( outname, 'glyph-%d.bmp', i );
        outname := 'glyph-' + IntToStr( i ) + '.bmp';
        SDL_SaveBMP( glyph, PChar( outname ) );
      end;
    end;
    ShutDownApplication( 0 );
    exit;
  end;

  { Set a 640x480x8 video mode }
  screen := SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, SDL_SWSURFACE );
  if ( screen = nil ) then
  begin
    //fprintf(stderr, 'Couldn't set 640x480x8 video mode: %s',[SDL_GetError]);
    ShutDownApplication( 2 );
  end;

  // Set the window manager title bar
  SDL_WM_SetCaption( 'JEDI-SDL 2D TTF Demo', 'showfont' );

  { Set a palette that is good for the foreground colored text }
  rdiff := backcol.r - forecol.r;
  gdiff := backcol.g - forecol.g;
  bdiff := backcol.b - forecol.b;

  for i := 0 to NUM_COLORS - 1 do
  begin
    colors[ i ].r := forecol.r + ( i * rdiff ) div 4;
    colors[ i ].g := forecol.g + ( i * gdiff ) div 4;
    colors[ i ].b := forecol.b + ( i * bdiff ) div 4;
  end;

  SDL_SetColors( screen, @colors, 0, NUM_COLORS );

  { Clear the background to background color }

  SDL_FillRect( screen, nil, SDL_MapRGB( screen.format, backcol.r, backcol.g, backcol.b ) );

  SDL_UpdateRect( screen, 0, 0, 0, 0 );

  { Show which font file we're looking at }
  //sprintf(string, 'Font file: %s', argv[0]);

  { Render and center the message }
  if message = '' then
  begin
    message := DEFAULT_TEXT;
  end;

  case rendertype of
    rtLatin1 :
      begin
        if ( rendersolid ) then
        begin
          text := TTF_RenderText_Solid( font, PChar( message ), forecol );
        end
        else
        begin
          text := TTF_RenderText_Shaded( font, PChar( message ), forecol, backcol );
        end;
      end;

    rtUTF8 :
      begin
        if ( rendersolid ) then
        begin
          text := TTF_RenderUTF8_Solid( font, PChar( message ), forecol );
        end
        else
        begin
          text := TTF_RenderUTF8_Shaded( font, PChar( message ), forecol, backcol );
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
      text := nil;
      { This shouldn't happen }
    end;
  end;

  if ( text = nil ) then
  begin
    //fprintf(stderr, 'Couldn''t render text: %s', [SDL_GetError]);
    TTF_CloseFont( font );
    ShutDownApplication( 2 );
  end;

  dstrect.x := ( screen.w - text.w ) div 2;
  dstrect.y := ( screen.h - text.h ) div 2;
  dstrect.w := text.w;
  dstrect.h := text.h;
  //printf('Font is generally %d big, and string is %hd big', TTF_FontHeight(font), text.h);

  { Blit the text surface }
  if ( SDL_BlitSurface( text, nil, screen, @dstrect ) < 0 ) then
  begin
    //fprintf(stderr, 'Couldn't blit text to display: %s', [SDL_GetError]);
    TTF_CloseFont( font );
    ShutDownApplication( 2 );
  end;

  SDL_UpdateRect( screen, 0, 0, 0, 0 );
  { Set the text colorkey and convert to display format }
  if ( SDL_SetColorKey( text, SDL_SRCCOLORKEY or SDL_RLEACCEL, 0 ) < 0 ) then
  begin
    //fprintf(stderr, 'Warning: Couldn't set text colorkey: %s', [SDL_GetError]);
  end;

  temp := SDL_DisplayFormat( text );
  if ( temp <> nil ) then
  begin
    SDL_FreeSurface( text );
    text := temp;

    { Wait for a keystroke, and blit text on mouse press }
    done := false;
    while ( not done ) do
    begin
      while ( SDL_PollEvent( @event ) <> 0 ) do
      begin
        case ( event.type_ ) of
          SDL_MOUSEBUTTONDOWN :
            begin
              dstrect.x := event.button.x - text.w div 2;
              dstrect.y := event.button.y - text.h div 2;
              dstrect.w := text.w;
              dstrect.h := text.h;
              if ( SDL_BlitSurface( text, nil, screen, @dstrect ) = 0 ) then
              begin
                SDL_UpdateRects( screen, 1, @dstrect );
              end
              else
              begin
                //fprintf(stderr, 'Couldn''t blit text to display: %s', [SDL_GetError]);
              end;
            end;

          SDL_KEYDOWN, SDL_QUITEV :
            begin
              done := true;
            end;

        end;
      end;
    end;
  end;
  SDL_FreeSurface( text );
  TTF_CloseFont( font );
  ShutDownApplication( 0 );
end.

