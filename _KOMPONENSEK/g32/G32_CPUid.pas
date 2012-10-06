unit G32_CPUid;

{*********************************************}
{  This unit is a part of Graphics32 library  }
{  Copyright © 2000-2001 Alex Denisov         }
{  See License.txt for licence information    }
{*********************************************}
// $Id: G32_CPUid.pas,v 1.2 2001/08/26 07:56:45 alex Exp $

interface

{$I G32.INC}

// HasMMX returns 'true' if CPU supports MMX instructions
function HasMMX: Boolean;

implementation
 
function CPUID_Available: Boolean;
asm
        MOV       EDX,False
        PUSHFD
        POP       EAX
        MOV       ECX,EAX
        XOR       EAX,$00200000
        PUSH      EAX
        POPFD
        PUSHFD
        POP       EAX
        XOR       ECX,EAX
        JZ        @1
        MOV       EDX,True
@1:     PUSH      EAX
        POPFD
        MOV       EAX,EDX
end;

function CPU_Signature: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
end;

function CPU_Features: Integer;
asm
        PUSH    EBX
        MOV     EAX,1
        DW      $A20F   // CPUID
        POP     EBX
        MOV     EAX,EDX
end;

function HasMMX: Boolean;
begin
  Result := False;
  if not CPUID_Available then Exit;              // no CPUID available
  if CPU_Signature shr 8 and $0F < 5 then Exit;  // not a Pentium class
  if CPU_Features and $800000 = 0 then Exit;     // no MMX
  Result := True;
end;
 
end.
