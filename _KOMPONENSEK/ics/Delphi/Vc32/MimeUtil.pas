{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       Mime support routines (RFC2045).
Creation:     May 03, 2003  (Extracted from SmtpProt unit)
Version:      1.03
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2005 by François PIETTE
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
May 03, 2003  V1.00 Initial release
Jun 19, 2003  V1.01 Fixed SplitQuotedPrintableString. Thanks to Arno Garrels
                    <arno.garrels@gmx.de>
Jan 12, 2004  V1.02 Marc HUBAUT <mhu@wanadoo.fr> fixed DoFileEncBase64 in case
                    of file size is a multple of 3.
May 31, 2004  V1.03 Used ICSDEFS.INC, added const with version and copyright

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
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

unit MimeUtil;

interface

{$R-}

uses
    SysUtils, Classes;

const
    TMimeUtilsVersion = 103;
    CopyRight : String = ' MimeUtils (c) 1997-2005 F. Piette V1.03 ';

{ Functions to encode/decode string as a "quoted-printable" string RFC2045}
function  EncodeQuotedPrintable(const S: String) : String;
function  DecodeQuotedPrintable(const S: String) : String;
function  SplitQuotedPrintableString(const S : String) : String;
{ Find a Content-Type from a file name                                   }
function  FilenameToContentType(FileName : String) : String;
{ Base 64 encoding }
function  Base64Encode(Input : String) : String;
function  Base64Decode(Input : String) : String;
function  InitFileEncBase64(const FileName : String;
                            ShareMode      : Word) : TStream;
function  DoFileEncBase64(var Stream     : TStream;
                          var More       : Boolean) : String;
procedure EndFileEncBase64(var Stream : TStream);
{ Dot a start of line escaping for SMTP and NNTP (double the dot) }
procedure DotEscape(var S : String);

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ See also SplitQuotedPrintableString !                                     }
function EncodeQuotedPrintable(const S: String) : String;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and
              (S[I] <> '=') and
              (S[I] >= ' ') and
              (Ord(S[I]) <= 126) do
            Inc(I);
        if I > Length(S) then begin
            if J = 1 then
                Result := S     { Optimisation }
            else
                Result := Result + Copy(S, J, I - J);
            Exit;
        end;
        Result := Result + Copy(S, J, I - J) + '=' +
                  UpperCase(IntToHex(Ord(S[I]), 2));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A line ending with an equal sign is continued on the next line. This is   }
{ what RFC2045 refers as a "soft line break".                               }
{ This routine doesn't take care of the equal sign at the end of string.    }
{ It is simply ignored. The caller must check that condition and merge      }
{ successives lines. But the routine handle embedded soft line break.       }
function DecodeQuotedPrintable(const S: String) : String;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and (S[I] <> '=') do
            Inc(I);
        Result := Result + Copy(S, J, I - J);
        if I >= Length(S) then
            break;
        if S[I + 1] = #13 then  { Could also check for #10 }
            { Soft line break, nothing to do except continuing }
        else
            Result := Result + Char(StrToInt('$' + Copy(S, I + 1, 2)));
        Inc(I, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SplitQuotedPrintableString(const S : String) : String;
var
    I, J : Integer;
begin
    if Length(S) <= 76 then begin
        { No need to split }
        Result := S;
        Exit;
    end;
    Result := '';
    J      := 1;
    I      := 76;
    while TRUE do begin
        if S[I - 1] = '=' then
            Dec(I)
        else if S[I - 2] = '=' then
            Dec(I, 2);
        Result := Result + Copy(S, J, I - J) + '=' + #13#10;
        J      := I;
        Inc(I, 75);
        if I > Length(S) then begin
            Result := Result + Copy(S, J, I - J);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DotEscape(var S : String);
var
    I : Integer;
begin
    if S = '' then
        Exit;
    if S[1] = '.' then begin
        Insert('.', S, 1);
        I := 3;
    end
    else
        I := 1;
    while I < (Length(S) - 2) do begin
        if (S[I] = #13) and (S[I + 1] = #10) and (S[I + 2] = '.') then begin
            Insert('.', S, I + 2);
            Inc(I, 4);
            continue;
        end;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FilenameToContentType(FileName : String) : String;
var
    Ext : String;
begin
    { We probably should the registry to find MIME type for known file types }
    Ext := LowerCase(ExtractFileExt(FileName));
    if Length(Ext) > 1 then
        Ext := Copy(Ext, 2, Length(Ext));
    if (Ext = 'htm') or (Ext = 'html') then
        Result := 'text/html'
    else if Ext = 'gif' then
        Result := 'image/gif'
    else if Ext = 'bmp' then
        Result := 'image/bmp'
    else if (Ext = 'jpg') or (Ext = 'jpeg') then
        Result := 'image/jpeg'
    else if Ext = 'txt' then
        Result := 'text/plain'
    else
        Result := 'application/octet-stream';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  InitFileEncBase64(
    const FileName : String;
    ShareMode      : Word) : TStream;
begin
    Result := TFileStream.Create(FileName, fmOpenRead or ShareMode);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    Base64Out: array [0..64] of Char = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
    );
    Base64In: array[0..127] of Byte = (
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
        255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
         56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
          0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
         13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
        255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
         33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
         46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
    );


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    HLX_MULTIPLIER        = 3;  { for three lines at once }
    MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MAX_LENGTH_MULTIPLIED = (MAX_LENGTH + 2) * HLX_MULTIPLIER;
    MAX_READ              = ((MAX_LENGTH * 3)div 4) * HLX_MULTIPLIER;
    MAX_READ_MOD          = (MAX_LENGTH * 3) div 4;
var
    Count, Place : Integer;
    DataIn       : array [0..MAX_READ]  of Byte;
    DataOut      : array [0..MAX_LENGTH_MULTIPLIED + 8] of Byte;
    ByteCount    : Integer;
    I            : Integer;
{ HLX: The following code is rewritten, so it loads data in MAX_READ chunks and
  encodes all loaded data. The trick relies on the fact that TriggerGetData's
  MsgLine buffer can hold up to 1024 chars. We'll encode 3 lines at once,
  add CRLF's, and return all three as one: component will see it as one,
  server will still see it as three.
  I've noticed a strange behavior: having HLX_MULTIPLIER > 3, data aren't
  sent completely, although it shouldn't occur
  (see: TCustomSmtpClient.DataNext) }
begin
    Count     := 0;
    Place     := 0;
    ByteCount := Stream.Read(DataIn, MAX_READ);
    while Place < ByteCount do begin
        DataOut[Count] := (DataIn[Place] and $FC) shr 2;
        Inc(Count);
        DataOut[Count] := (DataIn[Place] and $03) shl 4;
        Inc(Place);
        if Place < ByteCount then begin
            DataOut[Count] := DataOut[Count] + (DataIn[Place] and $F0) shr 4;
            Inc(Count);
            DataOut[Count] := (DataIn[Place] and $0F) shl 2;
            Inc(Place);
            if Place < ByteCount then begin
                DataOut[Count] := DataOut[Count] + (DataIn[Place] and $C0) shr 6;
                Inc(Count);
                DataOut[Count] := (DataIn[Place] and $3F);
                Inc(Place);
                Inc(Count);
            end
            else begin
                Inc(Count);
                DataOut[Count] := $40;
                Inc(Count);
            end;
        end
        else begin
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
        end;
    end;
    { Moved out of the main loop, so it has the chance to work in the }
    { processor's L1 Cache                                            }
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
        DataOut[I] := Byte(Base64Out[DataOut[I]]);
    Move(DataOut[0], Result[1], Count);
    { Splitting lines }
    I := MAX_LENGTH + 1;
    while I < Count do begin;
        Insert(#13#10, Result, I);
        Inc(I, MAX_LENGTH + 2);
        Inc(Count);
    end;
    More := (ByteCount = MAX_READ);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    MAX_LENGTH = 72;
var
    Count     : Integer;
    DataIn    : array [0..2]  of Byte;
    DataOut   : array [0..MAX_LENGTH + 8] of Byte;
    ByteCount : Integer;
    I         : Integer;
begin
    Count     := 0;
    ByteCount := 0;
    while Count < MAX_LENGTH do begin
        ByteCount          := Stream.Read(DataIn, 3);
        if ByteCount = 0 then                            {<=MHU}
           Break;                                        {<=MHU}
        DataOut[Count]     := (DataIn[0] and $FC) shr 2;
        DataOut[Count + 1] := (DataIn[0] and $03) shl 4;
        if ByteCount > 1 then begin
            DataOut[Count + 1] := DataOut[Count + 1] +
                                  (DataIn[1] and $F0) shr 4;
            DataOut[Count + 2] := (DataIn[1] and $0F) shl 2;
            if ByteCount > 2 then begin
                DataOut[Count + 2] := DataOut[Count + 2] +
                                      (DataIn[2] and $C0) shr 6;
                DataOut[Count + 3] := (DataIn[2] and $3F);
            end
            else begin
                DataOut[Count + 3] := $40;
            end;
        end
        else begin
            DataOut[Count + 2] := $40;
            DataOut[Count + 3] := $40;
        end;

        for I := 0 to 3 do
            DataOut[Count + I] := Byte(Base64Out[DataOut[Count + I]]);

        Count := Count + 4;
        if (Count > MAX_LENGTH) or (ByteCount < 3) then
            break;
    end;

    DataOut[Count] := $0;
    Result         := StrPas(@DataOut[0]);
    More           := (ByteCount = 3);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EndFileEncBase64(var Stream : TStream);
begin
    if Assigned(Stream) then begin
        Stream.Destroy;
        Stream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Encode(Input : String) : String;
var
    Final : String;
    Count : Integer;
    Len   : Integer;
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        Final := Final + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Final := Final + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                       ((Byte(Input[Count+1]) and $F0) shr 4)];
            if (Count+2) <= Len then begin
                Final := Final + Base64Out[((Byte(Input[Count+1]) and $0F) shl 2) +
                                           ((Byte(Input[Count+2]) and $C0) shr 6)];
                Final := Final + Base64Out[(Byte(Input[Count+2]) and $3F)];
            end
            else begin
                Final := Final + Base64Out[(Byte(Input[Count+1]) and $0F) shl 2];
                Final := Final + '=';
            end
        end
        else begin
            Final := Final + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Final := Final + '==';
        end;
        Count := Count + 3;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(Input : String) : String;
var
    Final   : String;
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Final := '';
    Count := 1;
    Len := Length(Input);
    while Count <= Len do begin
        DataIn0 := Base64In[Byte(Input[Count])];
        DataIn1 := Base64In[Byte(Input[Count+1])];
        DataIn2 := Base64In[Byte(Input[Count+2])];
        DataIn3 := Base64In[Byte(Input[Count+3])];

        Final := Final + Char(((DataIn0 and $3F) shl 2) +
                              ((DataIn1 and $30) shr 4));
        if DataIn2 <> $40 then begin
            Final := Final + Char(((DataIn1 and $0F) shl 4) +
                                  ((DataIn2 and $3C) shr 2));
            if DataIn3 <> $40 then
                Final := Final + Char(((DataIn2 and $03) shl 6) +
                                      (DataIn3 and $3F));
        end;
        Count := Count + 4;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
