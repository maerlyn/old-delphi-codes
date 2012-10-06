{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TBuffer is an object wich buffers data in a single dynamically
              allocated memory block. It is a kind of FIFO wich manages
              characters in blocks of various sizes.
Creation:     Dec 2003 from win32 version created april 1996
Version:      6.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1996-2005 by François PIETTE
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
Mar 06, 1998  V2.00 Added a property and a parameter for the create method
              to select the buffer size. Using a 0 value will make the object
              use the default 1514 bytes (the largest size for an ethernet
              packet).
Jul 08, 1998  V2.01 Adadpted for Delphi 4
Jan 19, 2003  V5.00 First pre-release for ICS-SSL. New major version number.
              Skipped version numbers to mach wsocket.pas major version number.
Dec 07, 2003  V6.00 Pre-release for Delphi 8 for .NET framework


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverByte.Ics.WSockBuf;

interface

uses
  SysUtils;

const
  WSockBufVersion    = 600;
  CopyRight : String = ' TWSockBuf (c) 1996-2005 Francois Piette V6.00 ';

type
  TBuffer = class(TObject)
    Buf      : TBytes;
    FBufSize : Integer;
    WrCount  : Integer;
    RdCount  : Integer;
  public
    constructor Create(nSize : Integer); virtual;
    destructor  Destroy; override;
    function    Write(const Data: TBytes; Len : Integer) : Integer; overload;
    function    Write(const Data: TBytes; Offset : Integer; Len : Integer) : Integer; overload;
    function    Read(out Data : TBytes; Len : Integer) : Integer;
    function    Peek(out Data : TBytes) : Integer;
    function    Remove(Len : Integer) : Integer;
    procedure   SetBufSize(newSize : Integer);
    property    BufSize : Integer read FBufSize write SetBufSize;
  end;

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TBuffer.Create(nSize : Integer);
begin
    inherited Create;
    WrCount  := 0;
    RdCount  := 0;
    BufSize := nSize;
    SetLength(Buf, BufSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TBuffer.Destroy;
begin
    //if Assigned(Buf) then
    //    FreeMem(Buf, FBufSize);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBuffer.SetBufSize(newSize : Integer);
begin
    if newSize <= 0 then
        newSize := 1514;

    if newSize = FBufSize then
        Exit;

    FBufSize := newSize;
    SetLength(Buf, FBufSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Write(const Data : TBytes; Len : Integer) : Integer;
begin
    Result := Write(Data, 0, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Write(const Data: TBytes; Offset : Integer; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
    I         : Integer;
begin
    Remaining := FBufSize - WrCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;

        //Move(Data^, (PChar(Buf) + WrCount)^, Copied);
        for I := 0 to Copied - 1 do
            Buf[WrCount + I] := Data[Offset + I];
        WrCount := WrCount + Copied;
        Result  := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Read(out Data : TBytes; Len : Integer) : Integer;
var
    Remaining : Integer;
    Copied    : Integer;
    I         : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len <= Remaining then
            Copied := Len
        else
            Copied := Remaining;
        //Move((PChar(Buf) + RdCount)^, Data^, Copied);
        for I := 0 to Copied - 1 do
            Data[I] := Buf[RdCount + I];
        RdCount := RdCount + Copied;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Copied;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Peek(out Data : TBytes) : Integer;
var
    Remaining : Integer;
    I         : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then begin
        SetLength(Data, 0);
        Result := 0;
    end
    else begin
        Result := Remaining;
        SetLength(Data, Remaining);
        for I := 0 to Remaining - 1 do
            Data[I] := Buf[RdCount + I];
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TBuffer.Remove(Len : Integer) : Integer;
var
    Remaining : Integer;
    Removed   : Integer;
begin
    Remaining := WrCount - RdCount;
    if Remaining <= 0 then
        Result := 0
    else begin
        if Len < Remaining then
            Removed := Len
        else
            Removed := Remaining;
        RdCount := RdCount + Removed;

        if RdCount = WrCount then begin
            RdCount := 0;
            WrCount := 0;
        end;

        Result := Removed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

