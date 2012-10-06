program JediSFontTest4;
{******************************************************************}
{                                                                  }
{       Borland Delphi SFont Test4                                 }
{       Conversion of the Linux Games- SFont Library for SDL       }
{                                                                  }
{ Original work created by Karl Bartel  <karlb@gmx.net>            }
{ Copyright (C) 2001 Karl Bartel.                                  }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : test1.c                                 }
{                                                                  }
{ The original Pascal code is : JediSFontTest1.dpr                 }
{ The initial developer of the Pascal code is :                    }
{ Jason Farmer <jason@cerebral-bicycle.co.uk>                      }
{                                                                  }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{ Róbert Kisnémeth <mikrobi@freemail.hu>                           }
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
{  Loads a background image and demonstrates PutStringAdd and      }
{  PutStringSub                                                    }
{                                                                  }
{ Requires                                                         }
{ --------                                                         }
{   SDL runtime libary somewhere in your path                      }
{   The Latest SDL runtime can be found on http://www.libsdl.org   }
{   Latest SDLUtils.pas                                            }
{   Latest SDLImage.pas                                            }
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
{   Sept    29 2001 - JF : First written      .                    }
{                                                                  }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  SDL,
  SDL_Image,
  SFont,
  SDLUtils;

var
  Font_: PSDL_Surface;
  BackGround : PSDL_Surface;
  screen_: PSDL_Surface;
  FillRect: SDL_Rect;
  //Col : Uint32;
  ErrorMessage: string;
  Time : Uint32;
begin
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    ErrorMessage := 'Couldnt initialize SDL: ' + SDL_GetError();
    Writeln(ErrorMessage);
    SDL_Quit;
    Exit;
  end;

  // Initialize the display
  screen_ := SDL_SetVideoMode(640, 480, 32, 0);
  if (screen_ = nil) then
  begin
    ErrorMessage := 'Couldnt set 640x480x16 video mode: ' + SDL_GetError();
    Writeln(ErrorMessage);

    SDL_Quit;
    exit;
  end;

  BackGround := IMG_Load('images/iceberg.jpg');

  // Set the window manager title bar
  SDL_WM_SetCaption('SFont Test', 'SFont');
  // Load the font - You don't have to use the IMGlib for this
  Font_ := IMG_Load('images/24P_Copperplate_embosed.png');
  // Prepare the font for use
  InitFont(Font_);

  FillRect.x := 0;
  FillRect.y := 0;
  FillRect.w := screen_.w;
  FillRect.h := screen_.h;


  SDL_BlitSurface(background, nil,screen_,nil);

  time :=0;
  while time < 100 do
  begin

  // a simple text blit to (0/0)
  PutStringadd(screen_, random(640), random(480), 'PutStringAdd...');

  // Update the screen
  SDL_UpdateRect(screen_, 0, 0, 0, 0);
  inc(time);

  SDL_Delay(100);
  end;

  SDL_BlitSurface(background,nil,Screen_,nil);

  time :=0;
  while time < 100 do
  begin

  // a simple text blit to (0/0)
  PutStringsub(screen_, random(640), random(480), 'PutStringSub...');

  // Update the screen
  SDL_UpdateRect(screen_, 0, 0, 0, 0);
  inc(time);

  SDL_Delay(100);
  end;



  // Let the user time to look at the font
  //SDL_Delay(4000);
  // Bye
  SDL_Quit;
  exit;

end.

