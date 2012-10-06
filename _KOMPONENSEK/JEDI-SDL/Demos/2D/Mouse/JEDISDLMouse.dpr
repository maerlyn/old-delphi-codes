program JEDISDLMouse;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Mouse Example                           }
{       Conversion of the SDL Mouse Example                        }
{                                                                  }
{ Portions created by Bini Michele <mibin@tin.it>,  are            }
{ Copyright (C) 1998 Bini Michele.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : Mouse.c                                 }
{                                                                  }
{ The original Pascal code is : JEDISDLMouse.dpr                 }
{ The initial developer of the Pascal code is :                    }
{ Wojciech <brombs@wp.pl>                                          }
{                                                                  }
{ Portions created by Wojciech are                                 }
{ Copyright (C) 2001 Wojciech.                                     }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Obtained through:                                                }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )            }
{                                                                  }
{ You may retrieve the latest version of this file at the Project  }
{ JEDI home page, located at http://delphi-jedi.org                }
{                                                                  }
{ The contents of this file are used with permission, subject to   }
{ the Mozilla Public License Version 1.1 (the "License"); you may  }
{ not use this file except in compliance with the License. You may }
{ obtain a copy of the License at                                  }
{ http://www.mozilla.org/NPL/NPL-1_1Final.html                     }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{ Description                                                      }
{ -----------                                                      }
{  Shows how to use the a Bitmap Mouse Cursor and how to check for }
{  Mouse clicks.                                                   }
{                                                                  }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{                                                                  }
{ Programming Notes                                                }
{ -----------------                                                }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{                                                                  }
{ Revision History                                                 }
{ ----------------                                                 }
{   April   02 2001 - DL : Initial translation.                    }
{   June    26 2001 - DL : Combined Delphi and Kylix Demo.         }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  Logger,
  SDL;

const          
  //WndClassName = 'SDL Mouse Example'; // The Name Of Window's Class
  //TITLE = 'SDL : Move mouse Example'; // Window Title
  SCREEN_WIDTH = 640;
  SCREEN_HEIGHT = 480;
  SCREEN_BPP = 16;

type
  PSDL_SPRITE = record
    image: PSDL_SURFACE;
    bgsave: PSDL_SURFACE;
    x, y, ox, oy: LongInt;
  end;

var
  path: string; // program's path
  screen_: PSDL_SURFACE;
  flags: UInt32;

procedure RS_Blit(bitmap: PSDL_SURFACE; x, y: LongInt);
var
  dest: SDL_RECT;
begin
  dest.x := x;
  dest.y := y;
  dest.w := bitmap.w;
  dest.h := bitmap.h;
  SDL_BlitSurface(bitmap, nil, screen_, @dest);
end; //RS_Blit

function prep_image(filename: string): PSDL_SURFACE;
var
  image: PSDL_SURFACE;
begin
  //image := nil;
  image := SDL_LoadBMP(PChar(filename));
  if image <> nil then
    Result := image
  else
    Result := nil;
end; // prep_image

//* Blits a surface sized chunk of background to that surface *//

procedure get_bg(var surface: PSDL_SURFACE; x, y: LongInt);
var
  src, dst: SDL_RECT;
begin
  src.x := x;
  src.y := y;
  src.w := surface.w;
  src.h := surface.h;

  dst.x := 0;
  dst.y := 0;
  dst.w := surface.w;
  dst.h := surface.h;
  SDL_BlitSurface(screen_, @src, surface, @dst);
end; // get_bg

procedure play_game;
var
  event: TSDL_EVENT;
  //keys: PKeyStateArr;
  x, y: LongInt;
  //ox, oy: LongInt;
  done: boolean;
  translucent: boolean;
  Mousedown : Boolean;
  //* The buttons and mouse cursor surfaces *//
  button1: PSDL_Surface;
  button2: PSDL_Surface;
  cursor: PSDL_Surface;
  //* Used to hold background oblitered when mouse is blitted *//
  cursor_save: PSDL_Surface;
begin
  SDL_SHOWCURSOR(0);
  cursor := prep_image(path + 'images/cursor.bmp');
  SDL_SetColorKey(cursor, SDL_SRCCOLORKEY or SDL_RLEACCEL,
    SDL_MapRGB(cursor.format, 0,
    0, 255));
  cursor_save := prep_image(path + 'images/cursor.bmp');
  button1 := prep_image(path + 'images/button1.bmp');
  button2 := prep_image(path + 'images/button2.bmp');

  x := 0;
  y := 0;
  get_bg(cursor_save, x, y);
  RS_Blit(button2, 0, 0);
  RS_Blit(button1, SCREEN_WIDTH - button1.w, SCREEN_HEIGHT - button1.h);
  SDL_UpdateRect(screen_, 0, 0, 0, 0);
  done := false;
  translucent := False;
  Mousedown := False;
  while not done do
  begin

    while (SDL_PollEvent(@event) = 1) do
    begin
      case event.type_ of

        SDL_QUITEV:
        begin
          done := true;
        end;

        SDL_KEYDOWN :
        begin
          if (event.key.keysym.sym = SDLK_ESCAPE) then
          begin
            Done := true;
          end;
        end;

        SDL_MOUSEBUTTONDOWN:
        begin
          Mousedown := true;
        end;

        SDL_MOUSEBUTTONUP:
        begin
          Mousedown := False;
        end;
        
      end;
    end; //(SDL_PollEvent(@event) = 1)

    SDL_GetMouseState(x, y);
    if MouseDown then
    begin
      if (x > SCREEN_WIDTH - button1.w) and (x < SCREEN_WIDTH)
      and (y > SCREEN_HEIGHT - button1.h) and (y < SCREEN_HEIGHT) then
        done := true;

      if (x > 0) and (x < button2.w) then
      begin
        if (y > 0) and (y < button2.h) then
        begin
          if translucent then
            SDL_SetAlpha(cursor, SDL_SRCALPHA, 255)
          else
            SDL_SetAlpha(cursor, SDL_SRCALPHA, 127);
          translucent := not translucent;
        end;
      end;
      
      // Update just the part of the display that we've changed
      SDL_UpdateRect(screen_, x, y, 1, 1);
      Mousedown := False;
    end;

    get_bg(cursor_save, x, y);
    RS_Blit(cursor, x, y);
    SDL_UpdateRect(screen_, 0, 0, 0, 0);
    RS_Blit(cursor_save, x, y);
  end;

  SDL_FreeSurface(cursor);
  SDL_FreeSurface(cursor_save);
  SDL_FreeSurface(button1);
  SDL_FreeSurface(button2);
end;

procedure TerminateApplication;
begin
  SDL_QUIT;
  Halt(0);
end;

begin
  path := ExtractFileDir(ParamStr(0)) + '/';
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Could not initialize SDL: %s',
      [SDL_GetError]), 'Main');
      TerminateApplication;
    exit;
  end;

  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
    flags := SDL_SWSURFACE or SDL_FULLSCREEN
  else
    flags := SDL_SWSURFACE;

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption('Wojciech''s JEDI-SDL Mouse Example', nil);

  screen_ := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, flags);
  if (screen_ = nil) then
  begin
    Log.LogError(Format('Could not set video mode: %s',
      [SDL_GetError]), 'Main');
    TerminateApplication;
    exit;
  end;

  play_game;
  TerminateApplication;
end.

