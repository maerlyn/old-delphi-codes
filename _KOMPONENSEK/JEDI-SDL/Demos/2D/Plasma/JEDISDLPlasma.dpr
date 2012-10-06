program JEDISDLPlasma;
{******************************************************************}
{                                                                  }
{       Borland Delphi SDL Plasma Example                          }
{       Conversion of the SDL Plasma Demo                          }
{                                                                  }
{ Portions created by Bini Michele <mibin@tin.it>,  are            }
{ Copyright (C) 1998 Bini Michele.                                 }
{ All Rights Reserved.                                             }
{                                                                  }
{ The original files are : plasma.c                                }
{                                                                  }
{ The original Pascal code is : JEDISDLPlasma.dpr                }
{ The initial developer of the Pascal code is :                    }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                }
{                                                                  }
{ Portions created by Dominique Louis are                          }
{ Copyright (C) 2000 - 2001 Dominique Louis.                       }
{                                                                  }
{ Contributor(s)                                                   }
{ --------------                                                   }
{ <Contributer Name> ( contributer@sp.sw )                         }
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
{   Shows how to create a plasma effect using pallette rotation.   }
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
{   March   07 2001 - DL : Initial translation.                    }
{   June    26 2001 - DL : Combined Delphi and Kylix Demos         }
{   October 25 2001 - DL : Removed use of dynamic arrays to keep   }
{                          compatability with FreePascal.          }
{                                                                  }
{                                                                  }
{******************************************************************}

uses
  SysUtils,
  Logger,
  SDL;

//-----------------------------------------------------------------------------
// Global constants
//-----------------------------------------------------------------------------
const
  TITLE         =  'JEDI-SDL Plasma Demo'; // Window Title

  SCREEN_WIDTH    =  640;
  SCREEN_HEIGHT   =  480;
  SCREEN_BPP      =  8;

  TABLEX        =  SCREEN_WIDTH * 2;
  TABLEY        =  SCREEN_HEIGHT * 2;
  TONES         =  256;

procedure do_plasma(surface : PSDL_Surface;
	       x1 : double;
	       y1 : double;
	       x2 : double;
	       y2 : double;
	       x3 : double;
	       y3 : double;
	       t  : PByteArray);
type
  PByte = ^Byte;
var
  X1_, X2_, X3_, Y1_, Y2_, Y3_ : Integer;
  {x,} y, tmin, tmax : Integer;
  t1, t2, t3 : PByteArray;
  tmp: PByte;

begin
  X1_ := trunc(x1 * (TABLEX / 2));
  Y1_ := trunc(y1 * (TABLEY / 2));
  X2_ := trunc(x2 * (TABLEX / 2));
  Y2_ := trunc(y2 * (TABLEY / 2));
  X3_ := trunc(x3 * (TABLEX / 2));
  Y3_ := trunc(y3 * (TABLEY / 2));

  t1 := Pointer(Integer(t) + X1_ + Y1_ * TABLEX);
  t2 := Pointer(Integer(t) + X2_ + Y2_ * TABLEX);
  t3 := Pointer(Integer(t) + X3_ + Y3_ * TABLEX);

  for y := 0 to SCREEN_HEIGHT - 1  do
  begin
    tmp :=  PByte(Integer(surface.pixels) + y * surface.pitch);

    tmin := y * TABLEX;
    tmax := tmin + SCREEN_WIDTH;

    while tmin < tmax - 1  do
    begin
      tmp^ := t1[tmin] + t2[tmin] + t3[tmin];
      inc(tmin);
      inc(tmp);
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Name: . Such as WinMain() in VC++
// Desc: The application's entry point
//-----------------------------------------------------------------------------
var
  video_flags: Uint32;
  r : array[0..2] of double;
  R1 : array[0..5] of double;
  screen_: PSDL_Surface;
  colors: array[0..(TONES * 2)-1] of TSDL_Color;
  t: PByteArray; //unsigned char *;
  starttime: TDateTime;
  state, c, x, y: integer;
  tmp : double;
  length : Cardinal;
  
begin
  state := 0;
  Randomize;
  begin
    for c := 0 to 2 do
      r[ c ] := ( ( Random(5000) mod 1000 ) + 1 )  / 300000;
    for c := 0 to 5 do
      R1[ c ] := ( ( Random(5000) mod 1000 ) + 1 ) / 5000;
  end;

  length := TABLEY * TABLEX;
  GetMem(t, length );
  if ( t = nil ) then
  begin
    Log.LogError(Format('Out of memory : %s', [SDL_GetError]), 'Main');
    SDL_Quit;
    exit;
  end;
  begin
    for y := 0 to TABLEY - 1 do
    begin
      for x := 0 to TABLEX - 1 do
      begin
        tmp := (((x-(TABLEX/2))*(x-(TABLEX/2))+(y-(TABLEX/2))*(y-(TABLEX/2)))
		    *(PI/(TABLEX*TABLEX+TABLEY*TABLEY)));
	t[y*TABLEX+x] := Round((sin(sqrt(tmp)*12)+1)*TONES/6);
      end;
    end;
  end;

  // Inizialize the SDL library */
  if ( SDL_Init(SDL_INIT_VIDEO) < 0 ) then
  begin
    Log.LogError(Format('Could not initialize SDL: %s', [SDL_GetError]), 'Main');
    SDL_Quit;
    exit;
  end;

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption(TITLE, nil);
  
  // fire and forget...
  if ( ParamStr(1) = '-fullscreen' ) or ( ParamStr(1) = '-fs' ) then
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE or SDL_FULLSCREEN
  else
    video_flags := SDL_SWSURFACE or SDL_HWPALETTE;

  screen_ := SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, video_flags);
  if ( screen_  = nil ) then
  begin
    Log.LogError(Format('Could not initialize video mode: %s', [SDL_GetError]), 'Main');
    SDL_Quit;
    exit;
  end;
  SDL_EventState(SDL_ACTIVEEVENT, SDL_IGNORE);
  SDL_EventState(SDL_MOUSEMOTION, SDL_IGNORE);

  starttime := now;

  // Main loop 
  while (SDL_PollEvent( nil ) = 0) do
  begin
    inc( state );

    for c := 0  to TONES - 1 do
    begin
      colors[ c ].r := Round((sin((c)/TONES*6*PI+r[0]*PI*state*PI)+1)*127);
      colors[ c ].g := Round((sin((c)/TONES*6*PI+r[1]*state*PI)+1)*127);
      colors[ c ].b := Round((sin((c)/TONES*6*PI+r[2]*state*PI)+1)*127);
    end;

    SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, TONES);

    if (SDL_LockSurface(screen_) < 0 ) then
    begin
      continue;
    end;
    do_plasma(screen_,
	(sin((state)*R1[0])+1)/2,
	(sin((state)*R1[1])+1)/2,
	(sin((state)*R1[2])+1)/2,
	(sin((state)*R1[3])+1)/2,
	(sin((state)*R1[4])+1)/2,
	(sin((state)*R1[5])+1)/2, t);
    SDL_UnlockSurface(screen_);
    SDL_Flip(screen_);
  end;
  Log.LogStatus(Format('Frame rate: %g frames/sec', [(state)/( now - starttime)/(20*60*60)]), 'Main');
end.
