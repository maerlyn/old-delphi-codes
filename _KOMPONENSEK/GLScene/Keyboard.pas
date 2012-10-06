// Keyboard
{: Egg<p>

	Provides on demand state of any key on the keyboard as well as a set of
   utility functions for working with virtual keycodes.<p>

   Note that windows maps the mouse buttons to virtual key codes too, and you
   can use the functions/classes in this unit to check mouse buttons too.<br>
   See "Virtual-Key Codes" in the Win32 programmers réferences for a list of
   key code constants (VK_* constants are declared in the "Windows" unit).<p>

	<b>Historique : </b><font size=-1><ul>
      <li>09/10/00 - Egg - Fixed VirtualKeyCodeToKeyName
	   <li>03/08/00 - Egg - Creation, partly based Silicon Commander code
	</ul></font>
}
unit Keyboard;

interface

uses
  Windows;

type

   TVirtualKeyCode = Integer;

{: Check if the key corresponding to the given Char is down.<p>
   The character is mapped to the <i>main keyboard</i> only, and not to the
   numeric keypad.<br>
   The Shift/Ctrl/Alt state combinations that may be required to type the
   character are ignored (ie. 'a' is equivalent to 'A', and on my french
   keyboard, '5' = '(' = '[' since they all share the same physical key). }
function IsKeyDown(c : Char) : Boolean; overload;
{: Check if the given virtual key is down.<p>
   This function is just a wrapper for GetAsyncKeyState. }
function IsKeyDown(vk : TVirtualKeyCode) : Boolean; overload;
{: Returns the first pressed key whose virtual key code is >= to minVkCode.<p>
   If no key is pressed, the return value is -1, this function does NOT
   wait for user input.<br>
   If you don't care about multiple key presses, just don't use the parameter. }
function KeyPressed(minVkCode : TVirtualKeyCode = 0) : TVirtualKeyCode;

{: Converts a virtual key code to its name.<p>
   The name is expressed using the locale windows options. }
function VirtualKeyCodeToKeyName(vk : TVirtualKeyCode) : String;
{: Converts a key name to its virtual key code.<p>
   The comparison is case-sensitive, if no match is found, returns -1.<p>
   The name is expressed using the locale windows options, except for mouse
   buttons which are translated to 'LBUTTON', 'MBUTTON' and 'RBUTTON'. }
function KeyNameToVirtualKeyCode(const keyName : String) : TVirtualKeyCode;
{: Returns the virtual keycode corresponding to the given char.<p>
   The returned code is untranslated, f.i. 'a' and 'A' will give the same
   result. A return value of -1 means that the characted cannot be entered
   using the keyboard. }
function CharToVirtualKeyCode(c : Char) : TVirtualKeyCode;

// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
implementation
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------
// ---------------------------------------------------------------------

uses SysUtils;

const
   cLBUTTON = 'LBUTTON';
   cMBUTTON = 'MBUTTON';
   cRBUTTON = 'RBUTTON';

// IsKeyDown
//
function IsKeyDown(c : Char) : Boolean;
var
   vk : Integer;
begin
   // '$FF' filters out translators like Shift, Ctrl, Alt
   vk:=VkKeyScan(c) and $FF;
   if vk<>$FF then
      Result:=(GetAsyncKeyState(vk)<0)
   else Result:=False;
end;

// IsKeyDown
//
function IsKeyDown(vk : TVirtualKeyCode) : Boolean;
begin
   Result:=(GetAsyncKeyState(vk)<>0);
end;

// KeyPressed
//
function KeyPressed(minVkCode : TVirtualKeyCode = 0) : TVirtualKeyCode;
var
   i : Integer;
   buf : TKeyboardState;
begin
   Assert(minVkCode>=0);
   Result:=-1;
   if GetKeyboardState(buf) then begin
      for i:=minVkCode to High(buf) do begin
         if (buf[i] and $80)<>0 then begin
            Result:=i;
            Break;
         end;
      end;
   end;
end;

// VirtualKeyCodeToKeyName
//
function VirtualKeyCodeToKeyName(vk : TVirtualKeyCode) : String;
var
   nSize : Integer;
begin
   // Win32 API can't translate mouse button virtual keys to string
   case vk of
      VK_LBUTTON : Result:=cLBUTTON;
      VK_MBUTTON : Result:=cMBUTTON;
      VK_RBUTTON : Result:=cRBUTTON;
   else
      nSize:=32; // should be enough
      SetLength(Result, nSize);
      vk:=MapVirtualKey(vk, 0);
      nSize:=GetKeyNameText((vk and $FF) shl 16, PChar(Result), nSize);
      SetLength(Result, nSize);
   end;
end;

// KeyNameToVirtualKeyCode
//
function KeyNameToVirtualKeyCode(const keyName : String) : TVirtualKeyCode;
var
   i : Integer;
begin
   if keyName=cLBUTTON then
      Result:=VK_LBUTTON
   else if keyName=cMBUTTON then
      Result:=VK_MBUTTON
   else if keyName=cRBUTTON then
      Result:=VK_RBUTTON
   else begin
      // ok, I admit this is plain ugly. 8)
      Result:=-1;
      for i:=0 to 255 do begin
         if CompareText(VirtualKeyCodeToKeyName(i), keyName)=0 then begin
            Result:=i;
            Break;
         end;
      end;
   end;
end;

// CharToVirtualKeyCode
//
function CharToVirtualKeyCode(c : Char) : TVirtualKeyCode;
begin
   Result:=VkKeyScan(c) and $FF;
   if Result=$FF then Result:=-1;
end;

end.
