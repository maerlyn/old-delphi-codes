{******************************************************************************}
{                                                                              }
{       Borland Delphi SMPEG - SDL MPEG Player Library                         }
{       Conversion of the SMPEG - SDL MPEG Player Library                      }
{                                                                              }
{ Portions created by Sam Lantinga <slouken@devolution.com> are                }
{ Copyright (C) 1997, 1998, 1999, 2000, 2001  Sam Lantinga                     }
{ 5635-34 Springhouse Dr.                                                      }
{ Pleasanton, CA 94588 (USA)                                                   }
{                                                                              }
{ All Rights Reserved.                                                         }
{                                                                              }
{ The original files are : smpeg.h                                             }
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
{ Tom Jones <tigertomjones@gmx.de>  His Project inspired this conversion       }
{ Matthias Thoma <ma.thoma@gmx.de>                                             }
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
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   The SDL Runtime libraris on Win32  : SDL.dll on Linux : libSDL-1.2.so.0    }
{   They are available from...                                                 }
{   http://www.libsdl.org .                                                    }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   May      08 2001 - MT : Initial conversion                                 }
{                                                                              }
{   October  12 2001 - DA : Various changes as suggested by David Acklam       }
{                                                                              }
{******************************************************************************}

{$WEAKPACKAGEUNIT ON}

// Linux and Windows C-Compilers use different byte-allignment
{$IFDEF Linux}
  {$ALIGN 4} // Linux uses DWORD alignment
{$ENDIF}
{$IFDEF Win32}
  {$IFDEF VER140}
    {$ALIGN 8} // Windows uses Quad-Word alignment
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
{$PACKRECORDS 4}
{$ENDIF FPC}

unit smpeg;

interface
uses
  SDL;
//------------------------------------------------------------------------------
// MPEGFilter.h
//------------------------------------------------------------------------------
{ SMPEG filter info flags }
const
  SMPEG_FILTER_INFO_MB_ERROR = 1;
  SMPEG_FILTER_INFO_PIXEL_ERROR = 2;
  
{ Filter info from SMPEG }
type
  SMPEG_FilterInfo = record
    yuv_mb_square_error: PUint16;
    yuv_pixel_square_error: PUint16;
  end;
  TSMPEG_FilterInfo = SMPEG_FilterInfo;
  PSMPEG_FilterInfo = ^SMPEG_FilterInfo;

{ MPEG filter definition }
  PSMPEG_Filter = ^TSMPEG_Filter;

{ Callback functions for the filter }
  TSMPEG_FilterCallback = function(dest, source: PSDL_Overlay; region: PSDL_Rect; filter_info: PSMPEG_FilterInfo; data: Pointer): Pointer; cdecl;
  TSMPEG_FilterDestroy = function(Filter: PSMPEG_Filter): Pointer; cdecl;
  
{ The filter definition itself }
  TSMPEG_Filter = record
    flags: Uint32;
    data: Pointer;
    callback: TSMPEG_FilterCallback;
    destroy: TSMPEG_FilterDestroy;
  end;

{ The null filter (default). It simply copies the source rectangle to the video overlay. }
function SMPEGfilter_null: PSMPEG_Filter; cdecl;

{ The bilinear filter. A basic low-pass filter that will produce a smoother image. }
function SMPEGfilter_bilinear: PSMPEG_Filter; cdecl;

{ The deblocking filter. It filters block borders and non-intra coded blocks to reduce blockiness }
function SMPEGfilter_deblocking: PSMPEG_Filter; cdecl;

//------------------------------------------------------------------------------
// SMPEG.h
//------------------------------------------------------------------------------
const
  SMPEG_MAJOR_VERSION = 0;
  SMPEG_MINOR_VERSION = 4;
  SMPEG_PATCHLEVEL = 2;

type
  SMPEG_version = record
    major: UInt8;
    minor: UInt8;
    patch: UInt8;
  end;
  TSMPEG_version = SMPEG_version;
  PSMPEG_version = ^TSMPEG_version;

  // This is the actual SMPEG object
  _SMPEG = record
    //obj: PMPEG;
  end;
  TSMPEG = _SMPEG;
  PSMPEG = ^_SMPEG;

  { Used to get information about the SMPEG object }
  __SMPEG_Info = record
    has_audio: Integer;
    has_video: Integer;
    width: Integer;
    height: Integer;
    current_frame: Integer;
    current_fps: double;
    audio_string: array[0..79] of char;
    audio_current_frame: Integer;
    current_offset: UInt32;
    total_size: UInt32;
    current_time: double;
    total_time: double;
  end;
  _SMPEG_Info = __SMPEG_Info;
  SMPEG_Info = _SMPEG_Info;
  TSMPEG_Info = _SMPEG_Info;
  PSMPEG_Info = ^_SMPEG_Info;
  
{ Possible MPEG status codes }
const
  SMPEG_ERROR = -1;
  SMPEG_STOPPED = 0;
  SMPEG_PLAYING = 1;

type
  SMPEGstatus = Integer;
  TSMPEGstatus = Integer;
  PSMPEGstatus = ^Integer;

  { Matches the declaration of SDL_UpdateRect() }
  TSMPEG_DisplayCallback = function(dst: PSDL_Surface; x, y: Integer; w, h: Cardinal): Pointer; cdecl;

{ Create a new SMPEG object from an MPEG file.
  On return, if 'info' is not NULL, it will be filled with information
  about the MPEG object.
  This function returns a new SMPEG object.  Use SMPEG_error() to find out
  whether or not there was a problem building the MPEG stream.
  The sdl_audio parameter indicates if SMPEG should initialize the SDL audio
  subsystem. If not, you will have to use the SMPEG_playaudio() function below
  to extract the decoded data. }
function SMPEG_new(const _file: PChar; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; cdecl;

{ The same as above for a file descriptor }
function SMPEG_new_descr(_file: Integer; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; cdecl

{  The same as above but for a raw chunk of data.  SMPEG makes a copy of the
   data, so the application is free to delete after a successful call to this
   function. }
function SMPEG_new_data(data: Pointer; size: Integer; info: PSMPEG_Info; sdl_audio: Integer): PSMPEG; cdecl;

{ Get current information about an SMPEG object }
procedure SMPEG_getinfo(mpeg: PSMPEG; info: PSMPEG_Info); cdecl;

//procedure SMPEG_getinfo(mpeg: PSMPEG; info: Pointer); cdecl;
{ Enable or disable audio playback in MPEG stream }
procedure SMPEG_enableaudio(mpeg: PSMPEG; enable: Integer); cdecl;

{ Enable or disable video playback in MPEG stream }
procedure SMPEG_enablevideo(mpeg: PSMPEG; enable: Integer); cdecl;

{ Delete an SMPEG object }
procedure SMPEG_delete(mpeg: PSMPEG); cdecl;

{ Get the current status of an SMPEG object }
function SMPEG_status(mpeg: PSMPEG): TSMPEGstatus; cdecl;
                                  // status
{ Set the audio volume of an MPEG stream, in the range 0-100 }
procedure SMPEG_setvolume(mpeg: PSMPEG; volume: Integer); cdecl;

{ Set the destination surface for MPEG video playback
  'surfLock' is a mutex used to synchronize access to 'dst', and can be NULL.
  'callback' is a function called when an area of 'dst' needs to be updated.
  If 'callback' is NULL, the default function (SDL_UpdateRect) will be used. }
procedure SMPEG_setdisplay(mpeg: PSMPEG; dst: PSDL_Surface; surfLock: PSDL_mutex; callback: TSMPEG_DisplayCallback); cdecl;

{ Set or clear looping play on an SMPEG object }
procedure SMPEG_loop(mpeg: PSMPEG; _repeat: Integer); cdecl;

{ Scale pixel display on an SMPEG object }
procedure SMPEG_scaleXY(mpeg: PSMPEG; width, height: Integer); cdecl;
procedure SMPEG_scale(mpeg: PSMPEG; scale: Integer); cdecl;

procedure SMPEG_double(mpeg : PSMPEG; doubleit : Boolean );

{ Move the video display area within the destination surface }
procedure SMPEG_move(mpeg: PSMPEG; x, y: Integer); cdecl;

{ Set the region of the video to be shown }
procedure SMPEG_setdisplayregion(mpeg: PSMPEG; x, y, w, h: Integer); cdecl;

{ Play an SMPEG object }
procedure SMPEG_play(mpeg: PSMPEG); cdecl;

{ Pause/Resume playback of an SMPEG object}
procedure SMPEG_pause(mpeg: PSMPEG); cdecl;

{ Stop playback of an SMPEG object }
procedure SMPEG_stop(mpeg: PSMPEG); cdecl;

{ Rewind the play position of an SMPEG object to the beginning of the MPEG }
procedure SMPEG_rewind(mpeg: PSMPEG); cdecl;

{ Seek 'bytes' bytes in the MPEG stream }
procedure SMPEG_seek(mpeg: PSMPEG; bytes: Integer); cdecl;

{ Skip 'seconds' seconds in the MPEG stream }
procedure SMPEG_skip(mpeg: PSMPEG; seconds: single); cdecl;

{ Render a particular frame in the MPEG video
   API CHANGE: This function no longer takes a target surface and position.
               Use SMPEG_setdisplay() and SMPEG_move() to set this information. }
procedure SMPEG_renderFrame(mpeg: PSMPEG; framenum: Integer); cdecl;

{ Render the last frame of an MPEG video }
procedure SMPEG_renderFinal(mpeg: PSMPEG; dst: PSDL_Surface; x, y: Integer);

{ Set video filter }
function SMPEG_filter(mpeg: PSMPEG; filter: PSMPEG_Filter): PSMPEG_Filter; cdecl;

{ Return NULL if there is no error in the MPEG stream, or an error message
   if there was a fatal error in the MPEG stream for the SMPEG object. }
function _SMPEG_error(mpeg: PSMPEG): PChar; cdecl;

{ Exported callback function for audio playback.
   The function takes a buffer and the amount of data to fill, and returns
   the amount of data in bytes that was actually written.  This will be the
   amount requested unless the MPEG audio has finished.
}
function SMPEG_playAudio(mpeg: PSMPEG; stream: PUInt8; len: Integer): Integer; cdecl;

{ Wrapper for SMPEG_playAudio() that can be passed to SDL and SDL_mixer }
procedure SMPEG_playAudioSDL(mpeg: Pointer; stream: PUInt8; len: Integer); cdecl;

{ Get the best SDL audio spec for the audio stream }
function SMPEG_wantedSpec(mpeg: PSMPEG; wanted: PSDL_AudioSpec): Integer; cdecl;

{ Inform SMPEG of the actual SDL audio spec used for sound playback }
procedure SMPEG_actualSpec(mpeg: PSMPEG; spec: PSDL_AudioSpec); cdecl;

{ This macro can be used to fill a version structure with the compile-time
  version of the SDL library. }
procedure SMPEG_GETVERSION( var X : TSMPEG_version );


implementation

const
{$IFDEF WIN32}
  SMPEGModuleName = 'smpeg.dll';
{$ENDIF}
{$IFDEF LINUX}
  SMPEGModuleName = 'libsmpeg.so';
{$ENDIF}
{$IFDEF MACOS}
  SMPEGModuleName = 'libsmpeg.dylib';
{$ENDIF}


function SMPEG_new; external SMPEGModuleName;
function SMPEG_new_descr; external SMPEGModuleName;
function SMPEG_new_data; external SMPEGModuleName;
procedure SMPEG_getinfo; external SMPEGModuleName;
procedure SMPEG_enableaudio; external SMPEGModuleName;
procedure SMPEG_enablevideo; external SMPEGModuleName;
procedure SMPEG_delete; external SMPEGModuleName;
procedure SMPEG_setvolume; external SMPEGModuleName;
procedure SMPEG_setdisplay; external SMPEGModuleName;
procedure SMPEG_loop; external SMPEGModuleName;
procedure SMPEG_scaleXY; external SMPEGModuleName;
procedure SMPEG_scale; external SMPEGModuleName;
procedure SMPEG_move; external SMPEGModuleName;
procedure SMPEG_setdisplayregion; external SMPEGModuleName;
procedure SMPEG_play; external SMPEGModuleName;
procedure SMPEG_pause; external SMPEGModuleName;
procedure SMPEG_stop; external SMPEGModuleName;
procedure SMPEG_rewind; external SMPEGModuleName;
procedure SMPEG_seek; external SMPEGModuleName;
procedure SMPEG_skip; external SMPEGModuleName;
procedure SMPEG_renderFrame; external SMPEGModuleName;
procedure SMPEG_renderFinal; external SMPEGModuleName;
procedure SMPEG_actualSpec; external SMPEGModuleName;
procedure SMPEG_playAudioSDL; external SMPEGModuleName;
function SMPEG_playAudio; external SMPEGModuleName;
function SMPEG_wantedSpec; external SMPEGModuleName;
function SMPEG_status; external SMPEGModuleName;
function SMPEGfilter_null; external SMPEGModuleName;
function SMPEGfilter_bilinear; external SMPEGModuleName;
function SMPEGfilter_deblocking; external SMPEGModuleName;
function SMPEG_filter; external SMPEGModuleName;
function _SMPEG_error; external SMPEGModuleName name 'SMPEG_error';

procedure SMPEG_double(mpeg : PSMPEG; doubleit : Boolean );
begin
  if doubleit then
    SMPEG_scale( mpeg, 2 )
  else
    SMPEG_scale( mpeg, 1 );
end;

procedure SMPEG_GETVERSION( var X : TSMPEG_version );
begin
  X.major := SMPEG_MAJOR_VERSION;
  X.minor := SMPEG_MINOR_VERSION;
  X.patch := SMPEG_PATCHLEVEL;
end;

end.
