unit SDLi386Utils;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                SDL Utility functions                                         }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Tom Jones <tigertomjones@gmx.de>                                             }
{                                                                              }
{ Portions created by Tom Jones are                                            }
{ Copyright (C) 2000 - 2001 Tom Jones.                                         }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{ Róbert Kisnémeth <mikrobi@freemail.hu>                                       }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Helper functions...                                                        }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{               2000 - TJ : Initial creation                                   }
{                                                                              }
{   July   13   2001 - DL : Added PutPixel and GetPixel routines.              }
{                                                                              }
{   Sept   14   2001 - RK : Added flipping routines.                           }
{                                                                              }
{   Sept   19   2001 - RK : Added PutPixel & line drawing & blitting with ADD  }
{                           effect. Fixed a bug in SDL_PutPixel & SDL_GetPixel }
{                           Added PSDLRect()                                   }
{   Sept   22   2001 - DL : Removed need for Windows.pas by defining types here}
{                           Also removed by poor attempt or a dialog box       }
{                                                                              }
{   Sept   25   2001 - RK : Added PixelTest, NewPutPixel, SubPixel, SubLine,   }
{                           SubSurface, MonoSurface & TexturedSurface          }
{                                                                              }
{   Sept   26   2001 - DL : Made change so that it refers to native Pascal     }
{                           types rather that Windows types. This makes it more}
{                           portable to Linix.                                 }
{                                                                              }
{   Sept   27   2001 - RK : SDLUtils now can be compiled with FreePascal       }
{                                                                              }
{   Oct    27   2001 - JF : Added ScrollY function                             }
{                                                                              }
{   Jan    21   2002 - RK : Added SDL_ZoomSurface and SDL_WarpSurface          }
{                                                                              }
{   Mar    28   2002 - JF : Added SDL_RotateSurface                            }
{                                                                              }
{   May    13   2002 - RK : Improved SDL_FillRectAdd & SDL_FillRectSub         }
{                                                                              }
{   May    27   2002 - YS : GradientFillRect function                          }
{                                                                              }
{   May    30   2002 - RK : Added SDL_2xBlit, SDL_Scanline2xBlit               }
{                           & SDL_50Scanline2xBlit                             }
{******************************************************************************}
interface

uses
{$IFDEF LINUX}
  Types,
  Xlib,
{$ENDIF}
  SysUtils,
  SDL;

type
  TGradientStyle = ( gsHorizontal, gsVertical );

  // Pixel procedures
function SDL_GetPixel( surface : PSDL_Surface; x : cardinal; y : cardinal ) : Uint32;

procedure SDL_PutPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

procedure SDL_AddPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

procedure SDL_SubPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );

// Surface procedures
procedure SDL_AddSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_SubSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );

procedure SDL_MonoSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Color : cardinal );

procedure SDL_TexturedSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Texture : PSDL_Surface;
  TextureRect : PSDL_Rect );

procedure SDL_RotateDeg( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Integer );

procedure SDL_RotateRad( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Single );

procedure SDL_GradientFillRect( DstSurface : PSDL_Surface; const Rect : PSDL_Rect; const StartColor, EndColor : TSDL_Color; const Style : TGradientStyle );

// NOTE for All SDL_2xblit... function : the dest surface must be 2x of the source surface!
{procedure SDL_2xBlit( Src, Dest : PSDL_Surface );

procedure SDL_Scanline2xBlit( Src, Dest : PSDL_Surface );

procedure SDL_50Scanline2xBlit( Src, Dest : PSDL_Surface ); }

implementation

uses
  Math;

function SDL_GetPixel( surface : PSDL_Surface; x : cardinal; y : cardinal ) : Uint32;
begin
  result := 0; // shouldn't happen, but avoids warnings
end;

procedure SDL_PutPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  Addr, Pitch, BPP : cardinal;
begin
  Addr := cardinal( Surface.Pixels );
  Pitch := Surface.Pitch;
  BPP := Surface.format.BytesPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    mov ecx, Color
    cmp BPP, 1
    jne @Not1BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov [eax], cl
    jmp @Quit
   @Not1BPP:
    cmp BPP, 2
    jne @Not2BPP
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov [eax], cx
    jmp @Quit
   @Not2BPP:
    cmp BPP, 3
    jne @Not3BPP
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov edx, [eax]
    and edx, $ff000000
    or edx, ecx
    mov [eax], edx
    jmp @Quit
   @Not3BPP:
    mul BPP   // EAX := x * BPP
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * BPP
    mov [eax], ecx
   @Quit:
  end;
end;

procedure SDL_AddPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  SrcColor, FinalColor : cardinal;
  Addr, Pitch, Bits : cardinal;
begin
  if Color = 0 then
    exit;
  Addr := cardinal( Surface.Pixels );
  Pitch := Surface.Pitch;
  Bits := Surface.format.BitsPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    cmp Bits, 8
    jne @Not8bit
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov cl, [eax]
    movzx ecx, cl
    mov SrcColor, ecx
    mov edx, Color
    and ecx, 3
    and edx, 3
    add ecx, edx
    cmp ecx, 3
    jbe @Skip1_8bit
    mov ecx, 3
   @Skip1_8bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $1c
    and edx, $1c
    add ecx, edx
    cmp ecx, $1c
    jbe @Skip2_8bit
    mov ecx, $1c
   @Skip2_8bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $e0
    and edx, $e0
    add ecx, edx
    cmp ecx, $e0
    jbe @Skip3_8bit
    mov ecx, $e0
   @Skip3_8bit:
    or ecx, FinalColor
    mov [eax], cl
    jmp @Quit
   @Not8bit:
    cmp Bits, 15
    jne @Not15bit
    shl eax, 1
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    and ecx, $00007fff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    add ecx, edx
    cmp ecx, $1f
    jbe @Skip1_15bit
    mov ecx, $1f
   @Skip1_15bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $03e0
    and edx, $03e0
    add ecx, edx
    cmp ecx, $03e0
    jbe @Skip2_15bit
    mov ecx, $03e0
   @Skip2_15bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $7c00
    and edx, $7c00
    add ecx, edx
    cmp ecx, $7c00
    jbe @Skip3_15bit
    mov ecx, $7c00
   @Skip3_15bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not15Bit:
    cmp Bits, 16
    jne @Not16bit
    shl eax, 1
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    and ecx, $0000ffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    add ecx, edx
    cmp ecx, $1f
    jbe @Skip1_16bit
    mov ecx, $1f
   @Skip1_16bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $07e0
    and edx, $07e0
    add ecx, edx
    cmp ecx, $07e0
    jbe @Skip2_16bit
    mov ecx, $07e0
   @Skip2_16bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $f800
    and edx, $f800
    add ecx, edx
    cmp ecx, $f800
    jbe @Skip3_16bit
    mov ecx, $f800
   @Skip3_16bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not16Bit:
    cmp Bits, 24
    jne @Not24bit
    mov ecx, 0
    add ecx, eax
    shl ecx, 1
    add ecx, eax
    mov eax, ecx
    jmp @32bit
   @Not24bit:
    shl eax, 2
   @32bit:
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x * 2
    mov ecx, [eax]
    mov FinalColor, ecx
    and FinalColor, $ff000000
    and ecx, $00ffffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $000000ff
    and edx, $000000ff
    add ecx, edx
    cmp ecx, $000000ff
    jbe @Skip1_32bit
    mov ecx, $000000ff
   @Skip1_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $0000ff00
    and edx, $0000ff00
    add ecx, edx
    cmp ecx, $0000ff00
    jbe @Skip2_32bit
    mov ecx, $0000ff00
   @Skip2_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $00ff0000
    and edx, $00ff0000
    add ecx, edx
    cmp ecx, $00ff0000
    jbe @Skip3_32bit
    mov ecx, $00ff0000
   @Skip3_32bit:
    or ecx, FinalColor
    mov [eax], ecx
   @Quit:
  end;
end;

procedure SDL_SubPixel( Surface : PSDL_Surface; x : integer; y : integer; Color :
  cardinal );
var
  SrcColor, FinalColor : cardinal;
  Addr, Pitch, Bits : cardinal;
begin
  if Color = 0 then
    exit;
  Addr := cardinal( Surface.Pixels );
  Pitch := Surface.Pitch;
  Bits := Surface.format.BitsPerPixel;
  asm
    mov eax, y
    mul Pitch      // EAX := y * Pitch
    add Addr, eax  // Addr:= Addr + (y * Pitch)
    mov eax, x
    cmp Bits, 8
    jne @Not8bit
    add eax, Addr  // Now:   EAX:= Addr + (y * Pitch) + x
    mov cl, [eax]
    movzx ecx, cl
    mov SrcColor, ecx
    mov edx, Color
    and ecx, 3
    and edx, 3
    sub ecx, edx
    jns @Skip1_8bit
    mov ecx, 0
   @Skip1_8bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $1c
    and edx, $1c
    sub ecx, edx
    jns @Skip2_8bit
    mov ecx, 0
   @Skip2_8bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $e0
    and edx, $e0
    sub ecx, edx
    jns @Skip3_8bit
    mov ecx, 0
   @Skip3_8bit:
    or ecx, FinalColor
    mov [eax], cl
    jmp @Quit
   @Not8bit:
    cmp Bits, 15
    jne @Not15bit
    shl eax, 1
    add eax, Addr
    mov ecx, [eax]
    and ecx, $00007fff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    sub ecx, edx
    jns @Skip1_15bit
    mov ecx, 0
   @Skip1_15bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $03e0
    and edx, $03e0
    sub ecx, edx
    jns @Skip2_15bit
    mov ecx, 0
   @Skip2_15bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $7c00
    and edx, $7c00
    sub ecx, edx
    jns @Skip3_15bit
    mov ecx, 0
   @Skip3_15bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not15Bit:
    cmp Bits, 16
    jne @Not16bit
    shl eax, 1
    add eax, Addr
    mov ecx, [eax]
    and ecx, $0000ffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $1f
    and edx, $1f
    sub ecx, edx
    jns @Skip1_16bit
    mov ecx, 0
   @Skip1_16bit:
    mov FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $07e0
    and edx, $07e0
    sub ecx, edx
    jns @Skip2_16bit
    mov ecx, 0
   @Skip2_16bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $f800
    and edx, $f800
    sub ecx, edx
    jns @Skip3_16bit
    mov ecx, 0
   @Skip3_16bit:
    or ecx, FinalColor
    mov [eax], cx
    jmp @Quit
   @Not16Bit:
    cmp Bits, 24
    jne @Not24bit
    mov ecx, 0
    add ecx, eax
    shl ecx, 1
    add ecx, eax
    mov eax, ecx
    jmp @32bit
   @Not24bit:
    shl eax, 2
   @32bit:
    add eax, Addr
    mov ecx, [eax]
    mov FinalColor, ecx
    and FinalColor, $ff000000
    and ecx, $00ffffff
    mov SrcColor, ecx
    mov edx, Color
    and ecx, $000000ff
    and edx, $000000ff
    sub ecx, edx
    jns @Skip1_32bit
    mov ecx, 0
   @Skip1_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $0000ff00
    and edx, $0000ff00
    sub ecx, edx
    jns @Skip2_32bit
    mov ecx, 0
   @Skip2_32bit:
    or FinalColor, ecx
    mov ecx, SrcColor
    mov edx, Color
    and ecx, $00ff0000
    and edx, $00ff0000
    sub ecx, edx
    jns @Skip3_32bit
    mov ecx, 0
   @Skip3_32bit:
    or ecx, FinalColor
    mov [eax], ecx
   @Quit:
  end;
end;
// This procedure works on 8, 15, 16, 24 and 32 bits color depth surfaces.
// In 8 bit color depth mode the procedure works with the default packed
//  palette (RRRGGGBB). It handles all clipping.

procedure SDL_AddSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  // TransparentColor: cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    //    TransparentColor := format.ColorKey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 or AL=transparent color then skip everything
        mov esp, eax          // ESP - source color
        mov bl, [edi]         // BL := destination color
        mov dl, bl            // DL := destination color
        and ax, $03           // Adding BLUE
        and bl, $03
        add al, bl
        cmp al, $03
        jbe @Skip1
        mov al, $03
       @Skip1:
        mov cl, al
        mov eax, esp          // Adding GREEN
        mov bl, dl
        and al, $1c
        and bl, $1c
        add al, bl
        cmp al, $1c
        jbe @Skip2
        mov al, $1c
       @Skip2:
        or cl, al
        mov eax, esp          // Adding RED
        mov bl, dl
        and ax, $e0
        and bx, $e0
        add ax, bx
        cmp ax, $e0
        jbe @Skip3
        mov al, $e0
       @Skip3:
        or cl, al
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $001F         // Adding BLUE
        and bx, $001F
        add ax, bx
        cmp ax, $001F
        jbe @Skip1
        mov ax, $001F
       @Skip1:
        mov cx, ax
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $3E0
        and bx, $3E0
        add ax, bx
        cmp ax, $3E0
        jbe @Skip2
        mov ax, $3E0
       @Skip2:
        or cx, ax
        mov eax, esp          // Adding RED
        mov bx, dx
        and ax, $7C00
        and bx, $7C00
        add ax, bx
        cmp ax, $7C00
        jbe @Skip3
        mov ax, $7C00
       @Skip3:
        or cx, ax
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $1F           // Adding BLUE
        and bx, $1F
        add ax, bx
        cmp ax, $1F
        jbe @Skip1
        mov ax, $1F
       @Skip1:
        mov cx, ax
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $7E0
        and bx, $7E0
        add ax, bx
        cmp ax, $7E0
        jbe @Skip2
        mov ax, $7E0
       @Skip2:
        or cx, ax
        mov eax, esp          // Adding RED
        mov bx, dx
        and eax, $F800
        and ebx, $F800
        add eax, ebx
        cmp eax, $F800
        jbe @Skip3
        mov ax, $F800
       @Skip3:
        or cx, ax
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        add bx, ax
        cmp bx, $00ff
        jb @Skip
        mov bl, $ff
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        add bx, ax
        cmp bx, $00ff
        jb @Skip
        mov bl, $ff
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;

procedure SDL_SubSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        cmp al, 0
        je @SkipColor         // if AL=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bl, [edi]         // BL := destination color
        mov dl, bl            // DL := destination color
        and al, $03           // Subtract BLUE
        and bl, $03
        sub bl, al
        jns @Skip1
        mov bl, 0
       @Skip1:
        mov cl, bl
        mov eax, esp          // Subtract GREEN
        mov bl, dl
        and al, $1c
        and bl, $1c
        sub bl, al
        jns @Skip2
        mov bl, 0
       @Skip2:
        or cl, bl
        mov eax, esp          // Subtract RED
        mov bl, dl
        and ax, $e0
        and bx, $e0
        sub bx, ax
        jns @Skip3
        mov bl, 0
       @Skip3:
        or cl, bl
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    15 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax          // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $001F         // Subtract BLUE
        and bx, $001F
        sub bx, ax
        jns @Skip1
        mov bx, 0
       @Skip1:
        mov cx, bx
        mov eax, esp          // Subtract GREEN
        mov bx, dx
        and ax, $3E0
        and bx, $3E0
        sub bx, ax
        jns @Skip2
        mov bx, 0
       @Skip2:
        or cx, bx
        mov eax, esp          // Subtract RED
        mov bx, dx
        and ax, $7C00
        and bx, $7C00
        sub bx, ax
        jns @Skip3
        mov bx, 0
       @Skip3:
        or cx, bx
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    16 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        cmp ax, 0
        je @SkipColor         // if AX=0 then skip everything
        mov esp, eax          // ESP - source color
        mov bx, [edi]         // BX := destination color
        mov dx, bx            // DX := destination color
        and ax, $1F           // Subtracting BLUE
        and bx, $1F
        sub bx, ax
        jns @Skip1
        mov bx, 0
       @Skip1:
        mov cx, bx
        mov eax, esp          // Adding GREEN
        mov bx, dx
        and ax, $7E0
        and bx, $7E0
        sub bx, ax
        jns @Skip2
        mov bx, 0
       @Skip2:
        or cx, bx
        mov eax, esp          // Adding RED
        mov bx, dx
        and eax, $F800
        and ebx, $F800
        sub ebx, eax
        jns @Skip3
        mov bx, 0
       @Skip3:
        or cx, bx
        mov [edi], cx
       @SkipColor:
        add esi, 2
        add edi, 2
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    24 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       add WorkX, ax         // WorkX := Src.w * 2
       add WorkX, ax         // WorkX := Src.w * 3
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        sub bx, ax
        jns @Skip
        mov bl, 0
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
    32 :
      asm
      mov _ebx, ebx
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       shl ax, 2
       mov WorkX, ax         // WorkX := Src.w * 4
       @Loopx:
        mov bl, [edi]         // BX := destination color
        mov al, [esi]         // AX := source color
        cmp al, 0
        je @Skip              // if AL=0 then skip COMPONENT
        mov ah, 0             // AX := COLOR COMPONENT
        mov bh, 0
        sub bx, ax
        jns @Skip
        mov bl, 0
       @Skip:
        mov [edi], bl
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx,_ebx
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;

procedure SDL_MonoSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Color : cardinal );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod : cardinal;
  SrcTransparentColor : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    SrcTransparentColor := format.colorkey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  case bits of
    8 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        movzx eax, al
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov [edi], cl
       @SkipColor:
        inc esi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    15, 16 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AX := source color
        movzx eax, ax
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AX=Transparent color then skip everything
        mov [edi], cx
       @SkipColor:
        inc esi
        inc esi
        inc edi
        inc edi
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    24 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      and ecx, $00ffffff
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // EAX := source color
        and eax, $00ffffff
        cmp eax, SrcTransparentColor
        je @SkipColor         // if EAX=Transparent color then skip everything
        mov ebx, [edi]
        and ebx, $ff000000
        or ebx, ecx
        mov [edi], ecx
       @SkipColor:
        add esi, 3
        add edi, 3
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp, _esp
      mov edi, _edi
      mov esi, _esi
      mov ebx, _ebx
      end;
    32 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      mov ecx, Color
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // EAX := source color
        cmp eax, SrcTransparentColor
        je @SkipColor         // if EAX=Transparent color then skip everything
        mov [edi], ecx
       @SkipColor:
        add esi, 4
        add edi, 4
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       dec WorkY
      jnz @LoopY
      mov esp, _esp
      mov edi, _edi
      mov esi, _esi
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
end;
// TextureRect.w and TextureRect.h are not used.
// The TextureSurface's size MUST larger than the drawing rectangle!!!

procedure SDL_TexturedSurface( SrcSurface : PSDL_Surface; SrcRect : PSDL_Rect;
  DstSurface : PSDL_Surface; DestRect : PSDL_Rect; Texture : PSDL_Surface;
  TextureRect : PSDL_Rect );
var
  Src, Dest : TSDL_Rect;
  Diff : integer;
  SrcAddr, DestAddr, TextAddr : cardinal;
  _ebx, _esi, _edi, _esp : cardinal;
  WorkX, WorkY : word;
  SrcMod, DestMod, TextMod : cardinal;
  SrcTransparentColor : cardinal;
  Bits : cardinal;
begin
  if ( SrcSurface = nil ) or ( DstSurface = nil ) then
    exit; // Remove this to make it faster
  if ( SrcSurface.Format.BitsPerPixel <> DstSurface.Format.BitsPerPixel ) then
    exit; // Remove this to make it faster
  if SrcRect = nil then
  begin
    with Src do
    begin
      x := 0;
      y := 0;
      w := SrcSurface.w;
      h := SrcSurface.h;
    end;
  end
  else
    Src := SrcRect^;
  if DestRect = nil then
  begin
    Dest.x := 0;
    Dest.y := 0;
  end
  else
    Dest := DestRect^;
  Dest.w := Src.w;
  Dest.h := Src.h;
  with DstSurface.Clip_Rect do
  begin
    // Source's right side is greater than the dest.cliprect
    if Dest.x + Src.w > x + w then
    begin
      smallint( Src.w ) := x + w - Dest.x;
      smallint( Dest.w ) := x + w - Dest.x;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's bottom side is greater than the dest.clip
    if Dest.y + Src.h > y + h then
    begin
      smallint( Src.h ) := y + h - Dest.y;
      smallint( Dest.h ) := y + h - Dest.y;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
    // Source's left side is less than the dest.clip
    if Dest.x < x then
    begin
      Diff := x - Dest.x;
      Src.x := Src.x + Diff;
      smallint( Src.w ) := smallint( Src.w ) - Diff;
      Dest.x := x;
      smallint( Dest.w ) := smallint( Dest.w ) - Diff;
      if smallint( Dest.w ) < 1 then
        exit;
    end;
    // Source's Top side is less than the dest.clip
    if Dest.y < y then
    begin
      Diff := y - Dest.y;
      Src.y := Src.y + Diff;
      smallint( Src.h ) := smallint( Src.h ) - Diff;
      Dest.y := y;
      smallint( Dest.h ) := smallint( Dest.h ) - Diff;
      if smallint( Dest.h ) < 1 then
        exit;
    end;
  end;
  with SrcSurface^ do
  begin
    SrcAddr := cardinal( Pixels ) + UInt32( Src.y ) * Pitch + UInt32( Src.x ) *
      Format.BytesPerPixel;
    SrcMod := Pitch - Src.w * Format.BytesPerPixel;
    SrcTransparentColor := format.colorkey;
  end;
  with DstSurface^ do
  begin
    DestAddr := cardinal( Pixels ) + UInt32( Dest.y ) * Pitch + UInt32( Dest.x ) *
      Format.BytesPerPixel;
    DestMod := Pitch - Dest.w * Format.BytesPerPixel;
    Bits := DstSurface.Format.BitsPerPixel;
  end;
  with Texture^ do
  begin
    TextAddr := cardinal( Pixels ) + UInt32( TextureRect.y ) * Pitch +
      UInt32( TextureRect.x ) * Format.BytesPerPixel;
    TextMod := Pitch - Src.w * Format.BytesPerPixel;
  end;
  SDL_LockSurface( SrcSurface );
  SDL_LockSurface( DstSurface );
  SDL_LockSurface( Texture );
  case bits of
    8 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ebx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov al, [esi]         // AL := source color
        movzx eax, al
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov al, [ebx]
        mov [edi], al
       @SkipColor:
        inc esi
        inc edi
        inc ebx
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ebx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx, _ebx
      end;
    15, 16 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ecx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov ax, [esi]         // AL := source color
        movzx eax, ax
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov ax, [ecx]
        mov [edi], ax
       @SkipColor:
        inc esi
        inc esi
        inc edi
        inc edi
        inc ecx
        inc ecx
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ecx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
    24 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov _ebx, ebx
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ebx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // AL := source color
        and eax, $00ffffff
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov eax, [ebx]
        and eax, $00ffffff
        mov ecx, [edi]
        and ecx, $ff000000
        or ecx, eax
        mov [edi], eax
       @SkipColor:
        add esi, 3
        add edi, 3
        add ebx, 3
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ebx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      mov ebx, _ebx
      end;
    32 :
      asm
      mov _esi, esi
      mov _edi, edi
      mov _esp, esp
      mov esi, SrcAddr      // ESI - Source Offset
      mov edi, DestAddr     // EDI - Destination Offset
      mov ecx, TextAddr
      mov ax, Src.h         //  WorkY := Src.h
      mov WorkY, ax
      @LoopY:
       mov ax, Src.w
       mov WorkX, ax         // WorkX := Src.w
       @Loopx:
        mov eax, [esi]         // AL := source color
        cmp eax, SrcTransparentColor
        je @SkipColor         // if AL=Transparent color then skip everything
        mov eax, [ecx]
        mov [edi], eax
       @SkipColor:
        add esi, 4
        add edi, 4
        add ecx, 4
        dec WorkX
       jnz @LoopX
       add esi, SrcMod
       add edi, DestMod
       add ecx, TextMod
       dec WorkY
      jnz @LoopY
      mov esp,_esp
      mov edi,_edi
      mov esi,_esi
      end;
  end;
  SDL_UnlockSurface( SrcSurface );
  SDL_UnlockSurface( DstSurface );
  SDL_UnlockSurface( Texture );
end;

procedure SDL_RotateRad( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Single );
var
  aSin, aCos : Single;
  MX, MY, DX, DY, NX, NY, SX, SY, OX, OY, Width, Height, TX, TY, RX, RY, ROX, ROY : Integer;
  Colour, TempTransparentColour : UInt32;
  MAXX, MAXY : Integer;
begin
  // Rotate the surface to the target surface.
  TempTransparentColour := SrcSurface.format.colorkey;
  if srcRect.w > srcRect.h then
  begin
    Width := srcRect.w;
    Height := srcRect.w;
  end
  else
  begin
    Width := srcRect.h;
    Height := srcRect.h;
  end;

  maxx := DstSurface.w;
  maxy := DstSurface.h;
  aCos := cos( Angle );
  aSin := sin( Angle );

  Width := round( abs( srcrect.h * acos ) + abs( srcrect.w * asin ) );
  Height := round( abs( srcrect.h * asin ) + abs( srcrect.w * acos ) );

  OX := Width div 2;
  OY := Height div 2; ;
  MX := ( srcRect.x + ( srcRect.x + srcRect.w ) ) div 2;
  MY := ( srcRect.y + ( srcRect.y + srcRect.h ) ) div 2;
  ROX := ( -( srcRect.w div 2 ) ) + Offsetx;
  ROY := ( -( srcRect.h div 2 ) ) + OffsetY;
  Tx := ox + round( ROX * aSin - ROY * aCos );
  Ty := oy + round( ROY * aSin + ROX * aCos );
  SX := 0;
  for DX := DestX - TX to DestX - TX + ( width ) do
  begin
    Inc( SX );
    SY := 0;
    for DY := DestY - TY to DestY - TY + ( Height ) do
    begin
      RX := SX - OX;
      RY := SY - OY;
      NX := round( mx + RX * aSin + RY * aCos ); //
      NY := round( my + RY * aSin - RX * aCos ); //
      // Used for testing only
     //SDL_PutPixel(DstSurface.SDLSurfacePointer,DX,DY,0);
      if ( ( DX > 0 ) and ( DX < MAXX ) ) and ( ( DY > 0 ) and ( DY < MAXY ) ) then
      begin
        if ( NX >= srcRect.x ) and ( NX <= srcRect.x + srcRect.w ) then
        begin
          if ( NY >= srcRect.y ) and ( NY <= srcRect.y + srcRect.h ) then
          begin
            Colour := SDL_GetPixel( SrcSurface, NX, NY );
            if Colour <> TempTransparentColour then
            begin
              SDL_PutPixel( DstSurface, DX, DY, Colour );
            end;
          end;
        end;
      end;
      inc( SY );
    end;
  end;
end;

procedure SDL_RotateDeg( DstSurface, SrcSurface : PSDL_Surface; SrcRect :
  PSDL_Rect; DestX, DestY, OffsetX, OffsetY : Integer; Angle : Integer );
begin
  SDL_RotateRad( DstSurface, SrcSurface, SrcRect, DestX, DestY, OffsetX, OffsetY, DegToRad( Angle ) );
end;

procedure SDL_GradientFillRect( DstSurface : PSDL_Surface; const Rect : PSDL_Rect; const StartColor, EndColor : TSDL_Color; const Style : TGradientStyle );
var
  FBC : array[ 0..255 ] of Cardinal;
  // temp vars
  i, YR, YG, YB, SR, SG, SB, DR, DG, DB : Integer;

  TempStepV, TempStepH : Single;
  TempLeft, TempTop, TempHeight, TempWidth : integer;
  TempRect : TSDL_Rect;

begin
  // calc FBC
  YR := StartColor.r;
  YG := StartColor.g;
  YB := StartColor.b;
  SR := YR;
  SG := YG;
  SB := YB;
  DR := EndColor.r - SR;
  DG := EndColor.g - SG;
  DB := EndColor.b - SB;
  {for i := 0 to 255 do
  begin
    FBC[ i ] := VRMColor_RGB( YR, YG, YB ).Color;
    YR := SR + round( DR / 255 * i );
    YG := SG + round( DG / 255 * i );
    YB := SB + round( DB / 255 * i );
  end;}

  //  if aStyle = 1 then begin
  TempStepH := Rect.w / 255;
  TempStepV := Rect.h / 255;
  TempHeight := Trunc( TempStepV + 1 );
  TempWidth := Trunc( TempStepH + 1 );
  TempTop := 0;
  TempLeft := 0;
  TempRect.x := Rect.x;
  TempRect.y := Rect.y;
  TempRect.h := Rect.h;
  TempRect.w := Rect.w;

  case Style of
    gsHorizontal :
      begin
        for i := 0 to 255 do
        begin
          TempRect.y :=
            TempTop;
          TempRect.h := TempTop
            + TempHeight;

          SDL_FillRect( DstSurface, @TempRect, FBC[ i ] );
          TempTop :=
            Trunc( TempStepV * i );
        end;
      end;
    gsVertical :
      begin
        for i := 0 to 255 do
        begin
          TempRect.x :=
            TempLeft;
          TempRect.w :=
            TempLeft + TempWidth;

          SDL_FillRect( DstSurface, @TempRect, FBC[ i ] );
          TempLeft :=
            Trunc( TempStepH * i );
        end;
      end;
  end;
end;

{procedure SDL_2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BytesPerPixel of
    1 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx], al // PUInt8(WriteAddr + DestPitch)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx + 1], al // PUInt8(WriteAddr + DestPitch + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    2 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    3 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and [edx], $ff000000
             or [edx], eax
             and [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax
             and [edx + ebx], $00ffffff // (PUInt32(WriteAddr + DestPitch)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx], eax
             and [edx + ebx + 3], $00ffffff // (PUInt32(WriteAddr + DestPitch + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    4 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx], eax // PUInt32(WriteAddr + DestPitch)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx + 4], eax // PUInt32(WriteAddr + DestPitch + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end; }

{procedure SDL_Scanline2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BytesPerPixel of
    1 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    2 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], eax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    3 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and [edx], $ff000000
             or [edx], eax
             and [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    4 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end;}

{procedure SDL_50Scanline2xBlit( Src, Dest : PSDL_Surface );
var
  ReadAddr, WriteAddr, ReadRow, WriteRow : UInt32;
  SrcPitch, DestPitch, x, y, w, h : UInt32;
begin
  if ( Src = nil ) or ( Dest = nil ) then
    exit;
  if ( Src.w shl 1 ) < Dest.w then
    exit;
  if ( Src.h shl 1 ) < Dest.h then
    exit;

  if SDL_MustLock( Src ) then
    SDL_LockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_LockSurface( Dest );

  ReadRow := UInt32( Src.Pixels );
  WriteRow := UInt32( Dest.Pixels );

  SrcPitch := Src.pitch;
  DestPitch := Dest.pitch;

  w := Src.w;
  h := Src.h;

  case Src.format.BitsPerPixel of
    8 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov al, [ecx] // PUInt8(WriteAddr)^ := PUInt8(ReadAddr)^;
             mov [edx], al
             mov [edx + 1], al // PUInt8(WriteAddr + 1)^ := PUInt8(ReadAddr)^;
             shr al, 1
             and al, $6d
             mov [edx + ebx], al // PUInt8(WriteAddr + DestPitch)^ := PUInt8(ReadAddr)^;
             mov [edx + ebx + 1], al // PUInt8(WriteAddr + DestPitch + 1)^ := PUInt8(ReadAddr)^;

             inc ecx // inc(ReadAddr);
             add edx, 2 // inc(WriteAddr, 2);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    15 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             shr ax, 1
             and ax, $3def
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    16 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov ax, [ecx] // PUInt16(WriteAddr)^ := PUInt16(ReadAddr)^;
             mov [edx], ax
             mov [edx + 2], ax // PUInt16(WriteAddr + 2)^ := PUInt16(ReadAddr)^;
             shr ax, 1
             and ax, $7bef
             mov [edx + ebx], ax // PUInt16(WriteAddr + DestPitch)^ := PUInt16(ReadAddr)^;
             mov [edx + ebx + 2], ax // PUInt16(WriteAddr + DestPitch + 2)^ := PUInt16(ReadAddr)^;

             add ecx, 2 // inc(ReadAddr, 2);
             add edx, 4 // inc(WriteAddr, 4);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    24 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // (PUInt32(WriteAddr)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             and eax, $00ffffff
             and [edx], $ff000000
             or [edx], eax
             and [edx + 3], $00ffffff // (PUInt32(WriteAddr + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + 3], eax
             shr eax, 1
             and eax, $007f7f7f
             and [edx + ebx], $00ffffff // (PUInt32(WriteAddr + DestPitch)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx], eax
             and [edx + ebx + 3], $00ffffff // (PUInt32(WriteAddr + DestPitch + 3)^ and $ff000000) or (PUInt32(ReadAddr)^ and $00ffffff);
             or [edx + ebx + 3], eax

             add ecx, 3 // inc(ReadAddr, 3);
             add edx, 6 // inc(WriteAddr, 6);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
    32 :
      asm
         push ebx
         mov eax, h // for y := 1 to Src.h do
         mov y, eax
         @LoopY:
           mov eax, ReadRow // ReadAddr := ReadRow;
           mov ReadAddr, eax

           mov eax, WriteRow // WriteAddr := WriteRow;
           mov WriteAddr, eax

           mov eax, w // for x := 1 to Src.w do
           mov x, eax

           mov ecx, ReadAddr
           mov edx, WriteAddr
           mov ebx, DestPitch

           @LoopX:
             mov eax, [ecx] // PUInt32(WriteAddr)^ := PUInt32(ReadAddr)^;
             mov [edx], eax
             mov [edx + 4], eax // PUInt32(WriteAddr + 4)^ := PUInt32(ReadAddr)^;
             shr eax, 1
             and eax, $7f7f7f7f
             mov [edx + ebx], eax // PUInt32(WriteAddr + DestPitch)^ := PUInt32(ReadAddr)^;
             mov [edx + ebx + 4], eax // PUInt32(WriteAddr + DestPitch + 4)^ := PUInt32(ReadAddr)^;

             add ecx, 4 // inc(ReadAddr, 4);
             add edx, 8 // inc(WriteAddr, 8);

             dec x
           jnz @LoopX

           mov eax, SrcPitch // inc(UInt32(ReadRow), SrcPitch);
           add ReadRow, eax

           mov eax, DestPitch // inc(UInt32(WriteRow), DestPitch * 2);
           add WriteRow, eax
           add WriteRow, eax

           dec y
         jnz @LoopY
         pop ebx
      end;
  end;

  if SDL_MustLock( Src ) then
    SDL_UnlockSurface( Src );
  if SDL_MustLock( Dest ) then
    SDL_UnlockSurface( Dest );
end; }

end.


