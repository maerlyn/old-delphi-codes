{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Time functions.
Creation:     Nov 24, 1999 from Bruce Christensen <bkc51831234@hotmail.com>
              code used with his permission. Thanks.
Version:      1.11
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
                                           francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2005 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Apr 02, 2000  V1.01 Added definition for TIME_ZONE_ID_STANDARD for BCB1 and BCB3
May 20, 2000  V1.01 Added definition for TIME_ZONE_ID_STANDARD for Delphi 3
Oct 23, 2003  V1.10 Added UTCToLocalDT by Angus Robertson, angus@magsys.co.uk
Jan 12, 2004  V1.11 Made function TimeDateStr and DateTimeToUTC public.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FtpSrvT;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I ICSDEFS.INC}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

const
    FtpSrvT_Unit       = 111;
    CopyRight : String = ' FtpSrvT  (c) 1999-2005 F. Piette V1.11 ';

function GetLocalBiasUTC : LongInt;
function FileUtcStr(cFileName : String) : String;
function UTCToLocalDT(dtDT : TDateTime) : TDateTime;
function UpdateFileAge (const FName: String; const NewDT: TDateTime): boolean;
function UpdateUFileAge (const FName: String; const NewDT: TDateTime): boolean;
function MDTM2Date (S: String): TDateTime;
function DecodeMlsResp (Response: String; var Fname, FType, FAttr: String;
                            var FSize: Integer; var FileUDT: TDateTime): boolean;
function TimeDateStr(dDateTime : TDateTime) : String;
function DateTimeToUTC(dtDT : TDateTime) : TDateTime;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
implementation
uses
{$IFDEF UseWindows}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(value : String) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PadIntZero(nWord  : Word;
                    nWidth : Byte): String;
var
    cResult : String;
begin
    cResult := IntToStr(nWord);
    while Length(cResult) < nWidth do
        cResult := '0' + cResult;

    Result := cResult;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TimeDateStr(dDateTime : TDateTime) : String;
var
    nYear, nMonth, nDay, nHours, nMinutes, nSeconds, nMilliSecs : Word;
begin
    DecodeDate(dDateTime, nYear, nMonth, nDay);
    DecodeTime(dDateTime, nHours, nMinutes, nSeconds, nMilliSecs);

    Result := PadIntZero(nYear,  4) +
              PadIntZero(nMonth, 2) +
              PadIntZero(nDay,   2) +
              PadIntZero(nHours,   2) +
              PadIntZero(nMinutes, 2) +
              PadIntZero(nSeconds, 2) + '.' +
              PadIntZero(nMilliSecs, 3);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetLocalBiasUTC : LongInt;
{$IFDEF VER80}
{ Delphi 1 doesn't support GetTimeZoneInformation }
begin
    Result := 0;
end;
{$ELSE}
var
    tzInfo : TTimeZoneInformation;

{$IFNDEF COMPILER4_UP}
const
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}

begin
    case GetTimeZoneInformation(tzInfo) of
    TIME_ZONE_ID_STANDARD: Result := tzInfo.Bias + tzInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := tzInfo.Bias + tzInfo.DaylightBias;
    else
        Result := tzInfo.Bias;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DateTimeToUTC(dtDT : TDateTime) : TDateTime;
begin
    Result := dtDT + GetLocalBiasUTC / (60.0 * 24.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileAge(cFile : String) : Integer;
begin
    if cFile[Length(cFile)] in ['\', '/'] then
        cFile := cFile + '.';
    Result := FileAge(cFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FileUtcStr(cFileName : String) : String;
begin
    Result := TimeDateStr(
                  DateTimeToUTC(
                      FileDateToDateTime(GetFileAge(cFileName))));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UTCToLocalDT(dtDT : TDateTime) : TDateTime;
begin
    Result := dtDT - GetLocalBiasUTC / (60.0 * 24.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Set file time stamp, local time                                           }
function UpdateFileAge(const FName: String; const NewDT: TDateTime): boolean;
var
    H: Integer;
begin
    Result := FALSE;
    H := FileOpen(FName, fmOpenWrite);
    if H < 0 then
        Exit;
    FileSetDate(H, DateTimeToFileDate (NewDT));
    FileClose(H);
    Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Set file time stamp, UTC time                                             }
function UpdateUFileAge(const FName: String; const NewDT: TDateTime): boolean;
{$IFDEF VER80}
begin
    Result := FALSE;
end;
{$ELSE}
var
    H, Age   : Integer;
    FileTime : TFileTime;
begin
    Result := FALSE;
    H      := FileOpen(FName, fmOpenWrite);
    if H < 0 then
        Exit;
    Age := DateTimeToFileDate (NewDT);
    if DosDateTimeToFileTime(LongRec(Age).Hi, LongRec (Age).Lo, FileTime) then begin
        if SetFileTime(H, nil, nil, @FileTime) then
            Result := TRUE;
    end;
    FileClose(H);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MDTM2Date(S: String): TDateTime;
{ yyyymmddhhnnss.zzz  }
{ 20030909221537.100  }
{ 1234567890123456789 }
var
    yy, mm, dd, hh, nn, ss, zz: Integer;

    function GetNum(offset, len: Integer): Integer;
    var
        E: Integer;
    begin
        Val(Copy(S, offset, len), Result, E);
    end;

begin
    Result := 0;
    if Length(S) < 14 then
        Exit;
    yy := GetNum (1, 4);
    mm := GetNum (5, 2);
    if (mm = 0) or (mm > 12) then
        Exit;
    dd := GetNum(7, 2);
    if (dd = 0) or (dd > 31) then
        Exit;
{   if NOT TryEncodeDate (yy, mm, dd, Result) then      D6 only
    begin
        Result := -1;
        Exit;
    end;  }
    try
        Result := EncodeDate(yy, mm, dd);
    except
        Result := -1;
        Exit;
    end;
    hh := GetNum(9, 2);
    nn := GetNum(11, 2);
    ss := GetNum(13, 2);
    zz := 0;
    if Length(S) >= 18 then
        ZZ := GetNum(16, 3);
{    if NOT TryEncodeTime (hh, nn, ss, 0, timeDT) then Exit;   // D6 only }
{    Result := Result + timeDT;                                           }
    try
        Result := Result + EncodeTime(hh, nn, ss, zz);
    except
        Result := -1;
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FindMlsFact(const response, fact: String): String;
var
    I: Integer;
    S: String;
begin
    Result := '';
    I := Pos(fact, response);   { ie type=, size=, modify=, perm=  }
    if I <= 0 then
        Exit;
    I := I + Length(fact);
    if I > Length(response) then
        Exit;
    S := Copy(response, I, 999);   { ie size=183977;type=fil }
    I := Pos(';', S);  { fact terminator }
    if I <= 0 then
        Exit;
    Result := Copy(S, 1, Pred (I));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DecodeMlsResp(
    Response: String;   var Fname, FType, FAttr: String;
    var FSize: Integer; var FileUDT: TDateTime): boolean;
var
    I: Integer;
begin
    Result  := FALSE;
    FName   := '';
    FType   := '';
    FAttr   := '';
    FSize   := 0;
    FileUDT := 0;
    I := Pos(#32, Response);  { file name follows first space in line, may be mixed case }
    if I = 1 then
        Exit;
    if Length(Response) < Succ (I) then
        Exit;
    FName    := Copy(Response, Succ (I), 999);
    Response := LowerCase(Response);  { remaining arguments all case insensitive }
    FType    := FindMlsFact(Response, 'type=');
    FSize    := atoi(FindMlsFact(Response, 'size='));
    FileUDT  := MDTM2Date(FindMlsFact(Response, 'modify='));
    FAttr    := FindMlsFact(Response, 'perm=');
    Result   := TRUE;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

