program MonoFonts_Test;

uses
  Classes,
  SDL,
  SDLMonoFonts in'../../Pas/SDLMonoFonts.pas',
  SDLUtils;

var
 Screen: PSDL_Surface;
 Font: PFont;
 Szoveg: TStringList;
 Flags: cardinal;
 Rect: TSDL_Rect;

procedure WaitForSpace;
var
 Event: TSDL_Event;
 Quit: boolean;
begin
 Quit:=false;
 repeat
  while SDL_PollEvent(@Event) > 0 do
  begin
    case Event.key.type_ of
      SDL_QUITEV :
      begin
        Quit := True;
      end;
      SDL_KeyDown :
      begin
        case Event.key.keysym.sym of
          SDLK_SPACE :
            Quit := true;
        end;
      end;
    end;
  end;
 until Quit=true;
end;

begin
  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
   flags:=SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
 else
   flags:=SDL_SWSURFACE or SDL_HWPALETTE;

 SDL_Init(SDL_INIT_VIDEO);
 SDL_WM_SetCaption('SDL_MonoFonts standalone demonstration. Press Space to continue', nil);

 Szoveg:=TStringList.Create;
 Szoveg.LoadFromFile('MonoFonts_Test.dpr');

 Screen:=SDL_SetVideoMode(640, 480, 8, Flags);

 // Create the Font object
 New(Font, Initialize('../../Images/Times New Roman Regular 8.bmp'));

 // Set the surface we draw on
 Font.Surface:=Screen;

 // Write the text on the full screen centered
 // Set the color if you want, the default is black
 Font.TextColor:=SDL_MapRGB(Screen.format, 0, 128, 0);

 // Write something great on screen
 Font.WriteTextWrapped(nil, PChar(Szoveg.Text), taCenter);
 SDL_UpdateRect(Screen, 0, 0, 0, 0);
 WaitForSpace;

 // Write the text on upper-left corner with LeftJustify
 // We use the target surface's clipping rectangle
 Screen.Clip_Rect:=SDLRect(0, 0, 320, 240);

 // Now we change to another font
 Font.LoadFont('../../Images/MS Sans Serif Regular 10.bmp');
 Font.TextColor:=SDL_MapRGB(Screen.format, 255, 0, 128);
 Font.WriteTextWrapped(nil, PChar(Szoveg.Text), taLeftJustify);
 SDL_UpdateRect(Screen, 0, 0, 0, 0);
 WaitForSpace;

 // Write the text on right-bottom corner with RightJustify
 // Restore the screen's clipping rectangle
 Screen.Clip_Rect:=SDLRect(0, 0, Screen.w, Screen.h);

 // ...and use our own clipping rectangle at the right-bottom corner of screen
 Rect:=SDLRect(320, 240, 320, 240);

 // Change font again
 Font.LoadFont('../../Images/Copperplate Gothic Bold 16.bmp');
 Font.TextColor:=SDL_MapRGB(Screen.format, 64, 255, 128);
 Font.WriteTextWrapped(@Rect, PChar(Szoveg.Text), taRightJustify);
 SDL_UpdateRect(Screen, 0, 0, 0, 0);
 WaitForSpace;

 Font.TextColor:=SDL_MapRGB(Screen.format, 255, 255, 255);

 // The WRITETEXT always use the target surface's clipping!!!
 // And no word wrapping...
 // And the text is aligned to the given x position
 Font.WriteText(0, 0, Pchar('This is a long sentence, which sometimes reaches the border of screen and goes out of it.'), taLeftJustify);
 Font.WriteText(Screen.w shr 1, 20, Pchar('This is a long sentence, which is sometimes reaches the border of screen and goes out of it.'), taCenter);
 Font.WriteText(Screen.w-1, 40, Pchar('This is a long sentence, which is sometimes reaches the border of screen and goes out of it.'), taRightJustify);
 SDL_UpdateRect(Screen, 0, 0, 0, 0);
 WaitForSpace;

 Szoveg.Free;
 Font.Finalize;
 SDL_Quit;
end.

