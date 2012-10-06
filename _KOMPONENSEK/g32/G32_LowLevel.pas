unit G32_LowLevel;

{*********************************************}
{  This unit is a part of Graphics32 library  }
{  Copyright © 2000-2001 Alex Denisov         }
{  See License.txt for licence information    }
{*********************************************}
// $Id: G32_LowLevel.pas,v 1.2 2001/08/26 07:56:45 alex Exp $

interface

{$I G32.INC}

uses
  Graphics, G32;

{ Clamp function restricts Value to [0..255] range }
function Clamp(Value: Integer): TColor32;

{ An analogue of FillChar for 32 bit values }
procedure FillLongword(var X; Count: Integer; Value: Longword);

{ Exchange two 32-bit values }
procedure Swap(var A, B: Integer);

{ Exhange A <-> B only if B < A }
procedure TestSwap(var A, B: Integer);

{ Exhange A <-> B only if B < A then restrict both to [0..Size-1] range }
{ returns true if resulting range has common points with [0..Size-1] range }
function TestClip(var A, B: Integer; Size: Integer): Boolean;

{ Returns Value constrained to [Lo..Hi] range}
function Constrain(Value, Lo, Hi: Integer): Integer;

{ shift right with sign conservation }
function SAR_4(Value: Integer): Integer;
function SAR_8(Value: Integer): Integer;
function SAR_9(Value: Integer): Integer;
function SAR_12(Value: Integer): Integer;
function SAR_16(Value: Integer): Integer;

{ Colorswap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;

{ MulDiv a faster implementation of Windows.MulDiv funtion }
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

implementation

{$R-}

function Clamp(Value: Integer): TColor32;
begin
  if Value < 0 then Result := 0
  else if Value > 255 then Result := 255
  else Result := Value;
end;

procedure FillLongword(var X; Count: Integer; Value: Longword);
asm
// EAX = X
// EDX = Count
// ECX = Value
        PUSH    EDI

        MOV     EDI,EAX  // Point EDI to destination              
        MOV     EAX,ECX
        MOV     ECX,EDX
        TEST    ECX,ECX
        JS      @exit

        REP     STOSD    // Fill count dwords
@exit:
        POP     EDI
end;

procedure Swap(var A, B: Integer);
asm
// EAX = [A]
// EDX = [B]
        MOV     ECX,[EAX]     // ECX := [A]
        XCHG    ECX,[EDX]     // ECX <> [B];
        MOV     [EAX],ECX     // [A] := ECX
end;

procedure TestSwap(var A, B: Integer);
asm
// EAX = [A]
// EDX = [B]
        MOV     ECX,[EAX]     // ECX := [A]
        CMP     ECX,[EDX]     
        JLE     @exit        // ECX <= [B]? Exit
        XCHG    ECX,[EDX]     // ECX <-> [B];
        MOV     [EAX],ECX     // [A] := ECX
@exit:
end;

function TestClip(var A, B: Integer; Size: Integer): Boolean;
begin
  TestSwap(A, B); // now A = min(A,B) and B = max(A, B)
  if A < 0 then A := 0;
  if B >= Size then B := Size - 1;
  Result := B >= A;
end;

function Constrain(Value, Lo, Hi: Integer): Integer;
begin
  if Value < Lo then Result := Lo
  else if Value > Hi then Result := Hi
  else Result := Value;
end;

{ shift right with sign conservation }
function SAR_4(Value: Integer): Integer;
asm
        SAR EAX,4
end;

function SAR_8(Value: Integer): Integer;
asm
        SAR EAX,8
end;

function SAR_9(Value: Integer): Integer;
asm
        SAR EAX,9
end;

function SAR_12(Value: Integer): Integer;
asm
        SAR EAX,12
end;

function SAR_16(Value: Integer): Integer;
asm
        SAR EAX,16
end;

{ Colorswap exchanges ARGB <-> ABGR and fill A with $FF }
function ColorSwap(WinColor: TColor): TColor32;
asm
// EAX = WinColor
        MOV     ECX,EAX         // this function swaps R and B bytes in ABGR
        SHR     EAX,16
        XCHG    AL,CL
        MOV     AH,$FF          // and writes $FF into A component
        SHL     EAX,16
        MOV     AX, CX
end;

function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
asm
        PUSH    EBX             // Imperative save
        PUSH    ESI             // of EBX and ESI

        MOV     EBX,EAX         // Result will be negative or positive so set rounding direction
        XOR     EBX,EDX         //  Negative: substract 1 in case of rounding
        XOR     EBX,ECX         //  Positive: add 1

        OR      EAX,EAX         // Make all operands positive, ready for unsigned operations
        jns     @m1Ok           // minimizing branching
        neg     eax
@m1Ok:
        or      edx,edx
        jns     @m2Ok
        neg     edx
@m2Ok:
        or      ecx,ecx
        jns     @DivOk
        neg     ecx
@DivOK:
        mul     edx             // Unsigned multiply (Multiplicand*Multiplier)

        mov     esi,edx         // Check for overflow, by comparing
        shl     esi,1           // 2 times the high-order 32 bits of the product (edx)
        cmp     esi,ecx         // with the Divisor.
        jae     @Overfl         // If equal or greater than overflow with division anticipated

        div     ecx             // Unsigned divide of product by Divisor

        sub     ecx,edx         // Check if the result must be corregized by adding or substracting
        cmp     ecx,edx         // 1 (*.5 -> nearest integer), by comparing the difference of
        ja      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
        inc     eax             // no rounding needed; add 1 to result otherwise
@NoAdd:
        or      ebx,ebx         // From unsigned operations back the to original sign of the result
        jns     @exit           // must be positive
        neg     eax             // must be negative
        JMP     @exit
@Overfl:
        OR      EAX,-1          //  3 bytes alternative for mov eax,-1. Windows.MulDiv "overflow"
                                //  and "zero-divide" return value
@exit:
        POP     ESI             // Restore
        POP     EBX             // esi and ebx
end;


end.
 