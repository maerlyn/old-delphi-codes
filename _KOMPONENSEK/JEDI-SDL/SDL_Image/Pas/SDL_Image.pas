unit SDL_Image;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL_Image - An example image loading library for use    }
{                                  with SDL                                    }
{       Conversion of the Simple DirectMedia Layer Image Headers               }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : SDL_image.h                                         }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Matthias Thoma <ma.thoma@gmx.de>                                             }
{                                                                              }
{ Portions created by Matthias Thoma are                                       }
{ Copyright (C) 2000 - 2001 Matthias Thoma.                                    }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
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
{   A simple library to load images of various formats as SDL surfaces         }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.pas in your search path.                                               }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   See the Aliens Demo on how to make use of this libaray                     }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   May      08 2001 - DL : Added ExternalSym derectives and copyright header  }
{                                                                              }
{   April    02 2001 - MT : Initial Translation                                }
{                                                                              }
{******************************************************************************}

{$WEAKPACKAGEUNIT ON}

{$ALIGN ON}

{$IFDEF FPC}
{$PACKRECORDS 4}
{$ENDIF FPC}

interface

uses
{$IFDEF LINUX}
  Types,
  LibC,
{$ENDIF}
  SDL;

{ Load an image from an SDL data source.
   The 'type' may be one of: "BMP", "GIF", "PNG", etc.

   If the image format supports a transparent pixel, SDL will set the
   colorkey for the surface.  You can enable RLE acceleration on the
   surface afterwards by calling:
        SDL_SetColorKey(image, SDL_RLEACCEL, image.format.colorkey);
}
function IMG_LoadTyped_RW(src: PSDL_RWops; freesrc: Integer; _type: PChar): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadTyped_RW}
{ Convenience functions }
function IMG_Load(const _file: PChar): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_Load}
function IMG_Load_RW(src: PSDL_RWops; freesrc: Integer): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_Load_RW}

{ Invert the alpha of a surface for use with OpenGL
  This function is now a no-op, and only provided for backwards compatibility. }
function IMG_InvertAlpha(_on: Integer): Integer; cdecl;
{$EXTERNALSYM IMG_InvertAlpha}

{ Functions to detect a file type, given a seekable source }
function IMG_isBMP(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isBMP}
function IMG_isPNM(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isPNM}
function IMG_isXPM(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isXPM}
function IMG_isXCF(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isXCF}
function IMG_isPCX(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isPCX}
function IMG_isGIF(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isGIF}
function IMG_isJPG(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isJPG}
function IMG_isTIF(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isTIF}
function IMG_isPNG(src: PSDL_RWops): Integer; cdecl;
{$EXTERNALSYM IMG_isPNG}

{ Individual loading functions }
function IMG_LoadBMP_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadBMP_RW}
function IMG_LoadPNM_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadPNM_RW}
function IMG_LoadXPM_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadXPM_RW}
function IMG_LoadXCF_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadXCF_RW}
function IMG_LoadPCX_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadPCX_RW}
function IMG_LoadGIF_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadGIF_RW}
function IMG_LoadJPG_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadJPG_RW}
function IMG_LoadTIF_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadTIF_RW}
function IMG_LoadPNG_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadPNG_RW}
function IMG_LoadTGA_RW(src: PSDL_RWops): PSDL_Surface; cdecl;
{$EXTERNALSYM IMG_LoadTGA_RW}

{ used internally, NOT an exported function }
function IMG_string_equals( const str1 : PChar; const str2 : PChar ) : integer; cdecl;
{$EXTERNALSYM IMG_string_equals}

{ Error Macros }
{ We'll use SDL for reporting errors }
procedure IMG_SetError(fmt: PChar);

function IMG_GetError: PChar;


implementation

const
{$IFDEF WIN32}
  Modulename =  'SDL_Image.dll';
{$ENDIF}
{$IFDEF LINUX}
  Modulename =  'libSDL_image.so';
{$ENDIF}
{$IFDEF MACOS}
  Modulename =  'libSDL_image.dylib';
{$ENDIF}

function IMG_LoadTyped_RW;     external ModuleName;
function IMG_Load;             external ModuleName;
function IMG_Load_RW;          external ModuleName;

function IMG_InvertAlpha;      external ModuleName;

function IMG_isBMP;            external ModuleName;
function IMG_isPNM;            external ModuleName;
function IMG_isXPM;            external ModuleName;
function IMG_isXCF;            external ModuleName;
function IMG_isPCX;            external ModuleName;
function IMG_isGIF;            external ModuleName;
function IMG_isJPG;            external ModuleName;
function IMG_isTIF;            external ModuleName;
function IMG_isPNG;            external ModuleName;

function IMG_LoadBMP_RW;       external ModuleName;
function IMG_LoadPNM_RW;       external ModuleName;
function IMG_LoadXPM_RW;       external ModuleName;
function IMG_LoadXCF_RW;       external ModuleName;
function IMG_LoadPCX_RW;       external ModuleName;
function IMG_LoadGIF_RW;       external ModuleName;
function IMG_LoadJPG_RW;       external ModuleName;
function IMG_LoadTIF_RW;       external ModuleName;
function IMG_LoadPNG_RW;       external ModuleName;
function IMG_LoadTGA_RW;       external ModuleName;

function IMG_string_equals;    external ModuleName;

procedure IMG_SetError(fmt: PChar);
begin
  SDL_SetError(fmt);
end;

function IMG_GetError: PChar;
begin
  result := SDL_GetError;
end;

end.