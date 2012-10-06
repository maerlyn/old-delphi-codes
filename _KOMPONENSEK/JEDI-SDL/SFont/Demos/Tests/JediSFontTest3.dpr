program JediSFontTest3;
{******************************************************************}
{                                                                  }
{       Borland Delphi SFont Test3                                 }
{       Conversion of the Linux Games- SFont Library for SDL       }
{                                                                  }
{ Original work created by Karl Bartel  <karlb@gmx.net>            }
{ Copyright (C) 2001 Karl Bartel.                                  }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : test3.c                                 }
{                                                                  }
{ The original Pascal code is : JediSFontTest3.dpr                 }
{ The initial developer of the Pascal code is :                    }
{ Jason Farmer <jason@cerebral-bicycle.co.uk>                      }
{                                                                  }
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
{  Shows how to load multiple bitmap fonts and display text        }
{  in different parts of the screen.                               }                     
{                                                                  }
{  Also shows the usage of the Sfont_input routine                 }
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
{   July    04 2001 - JF : Initial translation.                    }
{                                                                  }
{                                                                  }
{                                                                  }
{******************************************************************}
uses
  SysUtils,
  SDL,
  SDL_Image,
  SFont;
var
  Font_: PSDL_Surface;
  screen_: PSDL_Surface;
  ErrorMessage: string;
  Text: array[0..100] of char;

  NeonData: TSFont_FontInfo;
  Neon: PSFont_FontInfo;

begin

  Neon := @NeonData;

  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    ErrorMessage := 'Couldnt initialize SDL: ' + SDL_GetError();
    Writeln(ErrorMessage);
    SDL_Quit;
    Exit;
  end;

  // Initialize the display
  screen_ := SDL_SetVideoMode(640, 480, 0, 0);
  if (screen_ = nil) then
  begin
    ErrorMessage := 'Couldnt set 640x480x16 video mode: ' + SDL_GetError();
    Writeln(ErrorMessage);

    SDL_Quit;
    exit;
  end;

  // Set the window manager title bar
  SDL_WM_SetCaption('SFont Test3', 'SFont');
  Text := 'You can modify this text!';

  // Load the font - You don't have to use the IMGlib for this
  Font_ := IMG_Load('images/24P_Copperplate_Blue.png');
  Neon.Surface := IMG_Load('images/24P_Arial_NeonYellow.png');
  // Prepare the font for use
  InitFont(Font_);
  InitFont2(Neon);

  // a simple text blit to (0/0) with Neon font
  PutString2(screen_, Neon, 0, 0, 'Top Left');
  // License Info...
  PutString(screen_, 60, 120, 'SFont by Karl Bartel is GPL''ed!');
  // show some special chars
  PutString(screen_, 300, 260, '@--~!%&''_*,.:;');
  // demonstrates the use of TextWidth
  PutString(screen_, 640 - TextWidth('Bottom Right!'), 480 - Font_.h,
    'Bottom Right!');
  // Update the screen
  SDL_UpdateRect(screen_, 0, 0, 0, 0);
  // Let the user modify the text, which is blitted with the Neon font
  SFont_Input2(screen_, Neon, 100, 170, 500, text);

  // Bye
  exit;

end.
