{
----------------------------------------------------------
MAS-CompMaker was used to generate this code
MAS-CompMaker, copyright(c) 2000, Mats Asplund / MAs Prod.
----------------------------------------------------------

Component Name: THPCounter
        Author: Mats Asplund
      Creation: 2000-09-17
       Version: 1.1
   Description: A high-precision counter/timer. Retrieves time differences
                downto microsec.
        Credit:
        E-mail: mats.asplund@telia.com
          Site: http://go.to/masdp
  Legal issues: Copyright (C) 2000 by Mats Asplund


         Usage: This software is provided 'as-is', without any express or
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

                4. If you decide to use this software in any of your applications.
                   Send me an EMail address and tell me about it.

Quick Reference:
                THPCounter inherits from TComponent.

                Key-Methods:
                  Start:    Starts the counter. Place this call just before the
                            code you want to measure.

                  Read:     Reads the counter as a string. Place this call just
                            after the code you want to measure.

                  ReadInt:  Reads the counter as an Int64. Place this call just
                            after the code you want to measure.
--------------------------------------------------------------------------------
}
unit HPCounter;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, ExtCtrls;

type
  TInt64 = TLargeInteger;
  THPCounter = class(TComponent)
private
  Frequency: TLargeInteger;
  lpPerformanceCount1: TLargeInteger;
  lpPerformanceCount2: TLargeInteger;
  FCopyright: string;
  procedure SetCop(Value: string);
  { Private declarations }
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Start;
  function Read: string;
  function ReadInt: TLargeInteger;
  { Private declarations }
published
  property Copyright: string read FCopyright write SetCop;
  { Published declarations }
end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MAs Prod.', [THPCounter]);
end;

constructor THPCounter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCopyright:='Copyright (c) 2000, MAs Prod. / Mats Asplund';
end;

destructor THPCounter.Destroy;
begin
  inherited Destroy;
end;

function THPCounter.Read: string;
begin
  QueryPerformanceCounter(TInt64((@lpPerformanceCount2)^));
  QueryPerformanceFrequency(TInt64((@Frequency)^));
  Result:=IntToStr(Round(1000000 * (lpPerformanceCount2 -
                       lpPerformanceCount1) / Frequency));
end;

function THPCounter.ReadInt: TLargeInteger;
begin
  QueryPerformanceCounter(TInt64((@lpPerformanceCount2)^));
  QueryPerformanceFrequency(TInt64((@Frequency)^));
  Result:=Round(1000000 * (lpPerformanceCount2 -
                       lpPerformanceCount1) / Frequency);
end;

procedure THPCounter.SetCop(Value: string);
begin
  Exit;
end;

procedure THPCounter.Start;
begin
  QueryPerformanceCounter(TInt64((@lpPerformanceCount1)^));
end;

end.
