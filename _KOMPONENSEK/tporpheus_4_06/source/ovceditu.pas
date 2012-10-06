{*********************************************************}
{*                  OVCEDITU.PAS 4.06                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Orpheus                                    *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C)1995-2002    *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{.W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

unit ovceditu;
  {-Editor utility routines}

interface

uses
  OvcBase;

type
  TOvcEditBase = class(TOvcCustomControlEx);

type
  TMarker = packed record
    Para  : LongInt;  {number of paragraph}
    Pos   : Integer;  {position in paragraph}
  end;
  TMarkerArray = array[0..9] of TMarker;

type
  {text position record}
  TOvcTextPos = packed record
    Line : LongInt;
    Col  : Integer;
  end;

function edBreakPoint(S : PAnsiChar; MaxLen : Word): Word;
  {-return the position to word break S}
procedure edDeleteSubString(S : PAnsiChar; SLen, Count, Pos : Integer);
  {-delete Cound characters from S starting at Pos}
function edEffectiveLen(S : PAnsiChar; Len : Word; TabSize : Byte) : Word;
  {-compute effective length of S, accounting for tabs}
function edFindNextLine(S : PAnsiChar; WrapCol : Integer) : PAnsiChar;
  {-find the start of the next line}
function edFindPosInMap(Map : Pointer; Lines, Pos : Integer) : Integer;
  {-return the para position}
function edGetActualCol(S : PAnsiChar; Col : Word; TabSize : Byte) : Word;
  {-compute actual column for effective column Col, accounting for tabs}
function edHaveTabs(S : PAnsiChar; Len : Word) : Boolean;
  {Return True if tab are found in S}
procedure edMoveBlock(var Src, Dest; Count : Word);
  {-move block of data from Src to Dest}
procedure edMoveFast(var Src, Dest; Count : Word);
  {-move block of data from Src to Dest fastly}
function edPadChPrim(S : PAnsiChar; C : AnsiChar; Len : Word) : PAnsiChar;
  {-return S padded with C to length Len}
function edPadPrim(S : PAnsiChar; Len : Word) : PAnsiChar;
  {-return a string right-padded to length len with blanks}
function edScanToEnd(P : PAnsiChar; Len : Word) : Word;
  {-return position of end of para P}
function edStrStInsert(Dest, S : PAnsiChar; DLen, SLen, Pos : Word) : PAnsiChar;
  {-insert S into Dest}
function edWhiteSpace(C : AnsiChar) : Boolean;
  {-return True if C is a white space character}


implementation


uses
   Windows;

function edBreakPoint(S : PAnsiChar; MaxLen : Word): Word;
  {-return the position to word break S}
var
  I : Word;
begin
  I := MaxLen;
  while (I > 0) and not edWhiteSpace(S[I-1]) do
    Dec(I);
  if I = 0 then
    Result := MaxLen
  else
    Result := I;
end;

procedure edDeleteSubString(S : PAnsiChar; SLen, Count, Pos : Integer);
  {-delete Count characters from S starting at Pos}
begin
  if SLen+1 >= 1024 then
    edMoveBlock(S[Pos+Count], S[Pos], (SLen+1)-(Pos+Count))
  else
    edMoveFast(S[Pos+Count], S[Pos], (SLen+1)-(Pos+Count));
end;

function edEffectiveLen(S : PAnsiChar; Len : Word; TabSize : Byte) : Word; register;
  {-compute effective length of S, accounting for tabs}
asm
  push   edi            {save}
  push   esi            {save}
  push   ebx            {save}

  mov    esi,eax        {esi = S}
  xor    ebx,ebx        {clear}
  mov    bl,cl          {ebx = TabSize}
  xor    ecx,ecx        {clear}
  mov    cx,dx          {ecx = Len}
  xor    edi,edi        {temp length storage}
  xor    edx,edx

@@1:
  jcxz   @@2            {done if ecx is 0}
  dec    ecx            {decrement length}
  lodsb                 {get next character}
  or     al,al          {is it a null?}
  jz     @@2            {done if so}
  inc    edi            {increment length}
  cmp    al,9           {is it a tab?}
  jne    @@1            {if not, get next}
  dec    edi            {decrement length}
  mov    eax,edi        {ax has length}
  div    ebx            {divide by tabsize}
  inc    eax            {add one}
  mul    ebx            {multiply by tabsize}
  mov    edi,eax        {save result in edi}
  jmp    @@1            {get next character}
@@2:
  mov    eax,edi        {put effective length in eax}

  pop    ebx            {restore}
  pop    esi            {restore}
  pop    edi            {restore}
end;

function edFindNextLine(S : PAnsiChar; WrapCol : Integer) : PAnsiChar; register;
  {-find the start of the next line}
asm
  push   esi            {save}
  push   edi            {save}

  mov    esi,eax        {esi = S}
  mov    ecx,edx        {ecx = WrapCol}
  add    esi,ecx        {point to default wrap point}
  mov    edi,esi        {save esi in edi}

  std                   {go backward}
  inc    ecx
  cmp    byte ptr [esi],0 {is default wrap point a null?}
  jne    @@1
  mov    eax,edi        {force a break at the default wrap point}
  jmp    @@7

@@1:
  lodsb                 {next byte into al}
  cmp    al,'-'         {is it a hyphen?}
  ja     @@2
  je     @@3
  cmp    al,' '         {is it a space?}
  je     @@4
  cmp    al,9           {is it a tab?}
  je     @@4

@@2:
  loop   @@1            {try previous character}
  mov    eax,edi        {force a break at the default wrap point}
  jmp    @@7

@@3:
  inc    esi            {skip the hyphen}

@@4:
  cld                   {clear direction flag}
  inc    esi            {point to next character}

@@5:
  lodsb                 {next character into al}
  cmp    al,' '         {is it > than a space?}
  ja     @@6            {if so, we're done}
  je     @@5            {if it's a space, keep going}
  cmp    al,9           {if it's a tab, keep going}
  je     @@5            {otherwise, we're done}

@@6:
  dec    esi            {point to previous character}
  mov    eax,esi        {wrap point in eax}

@@7:
  pop    edi            {restore}
  pop    esi            {restore}
  cld                   {clear direction flag}
end;

function edFindPosInMap(Map : Pointer; Lines, Pos : Integer) : Integer; register;
  {-return the para position}
asm
  push   esi            {save}
  push   ebx            {save}

  mov    esi,eax        {esi = Map}
  mov    ebx,ecx        {ebx = Pos}
  and    ebx,0FFFFh     {clear high word}
  dec    ebx            {ebx = Pos-1}
  mov    ecx,edx        {ecx = Lines}
  and    ecx,0FFFFh     {clear high word}
  mov    eax,ecx        {eax = Lines}
  dec    eax            {prepare for word access}
  shl    eax,1
  add    esi,eax        {point to position in Map}

  std                   {go backwards}
@@1:
  lodsw
  cmp    bx,ax
  jae    @@2
  loop   @@1

@@2:
  mov    eax,ecx        {result in eax}
  and    eax,0FFFFh     {clear high word}

  pop    ebx            {restore}
  pop    esi            {restore}
  cld                   {clear direction flag}
end;

function edGetActualCol(S : PAnsiChar; Col : Word; TabSize : Byte) : Word; register;
  {-compute actual column for effective column Col, accounting for tabs}
asm
  push   esi            {save}
  push   edi            {save}
  push   ebx            {save}

  mov    esi,eax        {esi = S}
  and    edx,0FFFFh     {clear high word}
  mov    edi,edx        {edi = Col}
  {and    edi,0FFh}       {clear all except low byte}
  xor    ebx,ebx        {length = 0}
  mov    edx,ecx        {dl = TabSize}
  mov    dh,9           {dh = Tab char}
  and    edx,0FFFFh     {clear high word}
  xor    ecx,ecx        {ecx = actual column}

  cld                   {go forward}
@@1:
  inc    ecx            {increment column}
  lodsb                 {get next character}
  or     al,al          {is it a null?}
  jz     @@3            {done if so}
  inc    ebx            {increment effective length}
  cmp    al,dh          {is it a tab?}
  jne    @@2            {if not, check the column}
  dec    ebx            {decrement length}
  mov    eax,ebx        {eax has length}

  {determine integral offset}
  push   edx            {save}
  push   ecx            {save}
  xor    cx,cx          {use cx for division}
  mov    cl,dl          {cx=tab size}
  xor    dx,dx          {clear remainder register}
  div    cx             {divide by tabsize}
  inc    ax             {add one}
  mul    cx             {multiply by tabsize}
  pop    ecx            {restore}
  pop    edx            {restore - ignore upper 16 bits}

  mov    ebx,eax        {put result in ebx}

@@2:
  cmp    ebx,edi        {have we reached the target column yet?}
  jb     @@1            {get next character}

@@3:
  mov    eax,ecx        {put result in eax}

  pop    ebx            {restore}
  pop    edi            {restore}
  pop    esi            {restore}
end;

function edHaveTabs(S : PAnsiChar; Len : Word) : Boolean; register;
  {-return True if tabs are found in S}
asm
  {Note: this routine returns true if Len=0}
  push   edi            {save}
  mov    edi,eax        {edi = S}
  mov    al,9           {al = Tab character}
  mov    ecx,edx        {ecx = Len}
  and    ecx,0FFFFh     {clear high word}
  cld                   {go forward}
  repne  scasb          {search for the character}
  mov    eax,0          {assume False}
  jne    @@1
  inc    eax            {else return True}
@@1:
  pop    edi            {restore}
end;

procedure edMoveBlock(var Src, Dest; Count : Word);
  {-move block of data from Src to Dest}
begin
  Move(Src, Dest, Count);
end;

procedure edMoveFast(var Src, Dest; Count : Word);
  {-move block of data from Src to Dest fastly}
begin
  Move(Src, Dest, Count);
end;

function edPadChPrim(S : PAnsiChar; C : AnsiChar; Len : Word) : PAnsiChar; register;
  {-return S padded with C to length Len}
asm
  push   esi            {save}
  push   edi            {save}
  push   ebx            {save}

  mov    edi,eax        {edi = S}
  mov    esi,eax        {esi = S}
  mov    ebx,ecx        {save Len}
  and    ebx,0FFFFh     {clear high word}

  cld
  xor    eax, eax        {null}
  or     ecx, -1
  repne  scasb           {find null terminator}
  not    ecx             {calc length of S}
  dec    ecx             {backup one character}
  dec    edi
  mov    eax,ebx         {eax = Len}
  sub    eax,ecx         {find difference}
  jbe    @@ExitPoint     {nothing to do}
  mov    ecx,eax         {count of character to add}
  mov    al,dl           {al=C}
  rep    stosb           {add ecx characters}

@@ExitPoint:
  mov    byte ptr [edi],0
  mov    eax,esi

  pop    ebx            {restore}
  pop    edi            {restore}
  pop    esi            {restore}
end;

function edPadPrim(S : PAnsiChar; Len : Word) : PAnsiChar;
  {-return a string right-padded to length len with blanks}
begin
  Result := edPadChPrim(S, ' ', Len);
end;

function edScanToEnd(P : PAnsiChar; Len : Word) : Word; register;
  {-return position of end of para P}
asm
  push   edi            {save}
  push   ebx            {save}

  mov    edi,eax        {edi = P}
  mov    ebx,edi        {save edi}
  mov    ecx,edx        {ecx = Len}
  and    ecx,0FFFFh     {clear high word}
  mov    edx,ecx        {default for exit}
  mov    al, 0Ah
  cld
  jecxz  @@9
  repne  scasb
  jne    @@9
  mov    edx,edi
  sub    edx,ebx        {find difference}
@@9:
  mov    eax,edx

  pop    ebx            {restore}
  pop    edi            {restore}
end;

function edStrStInsert(Dest, S : PAnsiChar; DLen, SLen, Pos : Word) : PAnsiChar; register;
  {-insert S into Dest}
asm
  push   esi            {save}
  push   edi            {save}
  push   ebx            {save}

  push   eax            {save Dest}
  push   edx            {save S}

  mov    bx,Pos         {ebx = Pos}
  and    ebx,0FFFFh     {clear high word}
  mov    esi,eax        {eax = Dest}
  mov    edi,eax        {eax = Dest}
  and    ecx,0FFFFh     {ecx = DLen}
  inc    ecx            {ecx = DLen+1}
  add    edi,ecx        {point di one past terminating null}
  mov    dx,SLen
  and    edx,0FFFFh     {clear high word}
  cmp    dx,0           {if str to insert has 0 len then exit}
  je     @@1
  std                   {backwards string ops}
  add    edi,edx
  dec    edi
  add    esi,ecx
  dec    esi            {point to end of source string}
  sub    ecx,ebx        {calculate number to do}
  jb     @@1            {exit if Pos greater than strlen + 1}
  test   edi,1
  jnz    @@0
  movsb
  dec    ecx

@@0:
  dec    esi
  dec    edi
  shr    ecx,1
  rep    movsw
  jnc    @@2
  inc    esi
  inc    edi
  movsb
  jmp    @@3
@@2:
  inc    edi
@@3:
  pop    esi            {esi = S}
  push   esi
  add    esi,edx
  dec    esi
  mov    ecx,edx
  rep    movsb
@@1:
  cld
  pop    eax            {remove S}
  pop    eax            {eax = Dest}

  pop    ebx            {restore}
  pop    edi            {restore}
  pop    esi            {restore}
end;

function edWhiteSpace(C : AnsiChar) : Boolean; register;
  {-return True if C is a white space character}
asm
  {Result := C in [' ', #9];}
  cmp    al,' '
  je     @@001
  cmp    al,09
  je     @@001
  xor    eax,eax
  jmp    @@002
@@001:
  mov    eax,01
@@002:
end;

end.
