program Window;

{
   This is an example of making an application
   without using the Forms unit. Forms.pas is the
   Delphi unit that makes your programs so damn
   huge! Forms.pas has all the code for translating
   the forms you create with Delphi w/components
   into windows. If you ask me, anything that adds
   200k(@%#$!) to your app has got to be some damn
   inefficient code.

   GoRDy <gfody@jps.net>
   www.jps.net/gfody
}


uses Windows, Messages;

{$R *.RES}

var
wClass:   TWndClass;  // class struct for main window
hFont,                // handle of font
hInst,                // handle of program (hinstance)
Handle,               // handle of main window
hEncrypt,             // handle of encrypt button
hDecrypt,             // handle of decrypt button
hEdit,                // handle of main edit
hLabel,               // handle of password label
hPW:      HWND;       // handle of password edit
Msg:      TMSG;       // message struct
dEncrypt,
dDecrypt: Pointer;    // default button procedures

(*------------------------------------------------*)

// This lines everything up
procedure Resize;
var
RCT:TRect;
begin
  GetWindowRect(Handle,RCT);
  MoveWindow(hPW,230,5,RCT.Right-RCT.Left-245,24,True);
  MoveWindow(hEdit,5,34,RCT.Right-RCT.Left-20,RCT.Bottom-RCT.Top-66,True);
end;

(*------------------------------------------------*)

// This is to cleanup and stop the program
procedure ShutDown;
begin
  DeleteObject(hFont);
  UnRegisterClass('Sample Class',hInst);
  ExitProcess(hInst); //end program
end;

(*------------------------------------------------*)

// Decrypts the text in hEdit with the text in hPW
procedure Decrypt;
var
x,i,                // count variables
sText,sPW: Integer; // size of Text, PW
Text,PW:   PChar;   // buffer for Text, PW
begin
  sText:=GetWindowTextLength(hEdit)+1;
  sPW:=GetWindowTextLength(hPW)+1;
  GetMem(Text,sText);
  GetMem(PW,sPW);
  GetWindowText(hEdit,Text,sText);
  GetWindowText(hPW,PW,sPW);
  x:=0; // initialize count
  for i:=0 to sText-2 do
  begin
    Text[i]:=Chr(Ord(Text[i])-Ord(PW[x]));
    Inc(x);
    if x=(sPW-1)then x:=0;
  end;
  SetWindowText(hEdit,Text);
  FreeMem(Text);
  FreeMem(PW);
end;

(*------------------------------------------------*)

// Encrypts the text in hEdit with the text in hPW
procedure Encrypt;
var
x,i,                // count variables
sText,sPW: Integer; // size of Text, PW
Text,PW:   PChar;   // buffer for Text, PW
begin
  sText:=GetWindowTextLength(hEdit)+1;
  sPW:=GetWindowTextLength(hPW)+1;
  GetMem(Text,sText);
  GetMem(PW,sPW);
  GetWindowText(hEdit,Text,sText);
  GetWindowText(hPW,PW,sPW);
  x:=0; // initialize count
  for i:=0 to sText-2 do
  begin
    Text[i]:=Chr(Ord(Text[i])+Ord(PW[x]));
    Inc(x);
    if x=(sPW-1)then x:=0;
  end;
  SetWindowText(hEdit,Text);
  FreeMem(Text);
  FreeMem(PW);
end;

(*------------------------------------------------*)

// This function processes every message sent to the Encrypt Button
function EncryptProc(hWnd,Msg,wParam,lParam:Longint):Longint; stdcall;
var
i: Integer;
begin
  // Always pass the message to the Default procedure
  Result:=CallWindowProc(dEncrypt,hWnd,Msg,wParam,lParam);

  case Msg of
  // If the user presses TAB we're gunna switch the
  // focus over to the Decrypt button.
  WM_KEYDOWN: if wParam=9 then SetFocus(hDecrypt);
  end;

end;

(*------------------------------------------------*)

// This function processes every message sent to the Decrypt Button
function DecryptProc(hWnd,Msg,wParam,lParam:Longint):Longint; stdcall;
begin
  // Always pass the message to the Default procedure
  Result:=CallWindowProc(dEncrypt,hWnd,Msg,wParam,lParam);

  case Msg of
  // if the user presses TAB we're gunna switch
  // the focus back to the Encrypt button.
  WM_KEYDOWN: if wParam=9 then SetFocus(hEncrypt);
  end;

end;

(*------------------------------------------------*)

// This function processes every message sent to our Main window
function WindowProc(hWnd,Msg,wParam,lParam:Longint):Longint; stdcall;
begin
  // Always pass the message to the Default procedure
  Result:=DefWindowProc(hWnd,Msg,wParam,lParam);

  case Msg of
  WM_SIZE:    Resize;
  // When buttons are clicked the message is passed to
  // the parent window, so we handle it here.
  WM_COMMAND: if      lParam=hEncrypt then Encrypt
              else if lParam=hDecrypt then Decrypt;
  WM_DESTROY: ShutDown;
  end;

end;

(*------------------------------------------------*)

// This is the main program function (WinMain)
begin

  hInst:=GetModuleHandle(nil); // get the application instance
                               // hInstance returns zilch

  with wClass do
  begin
    Style:=         CS_PARENTDC;
    hIcon:=         LoadIcon(hInst,'MAINICON');
    lpfnWndProc:=   @WindowProc;
    hInstance:=     hInst;
    hbrBackground:= COLOR_BTNFACE+1;
    lpszClassName:= 'Sample Class';
    hCursor:=       LoadCursor(0,IDC_ARROW);
  end;

  // Once our class is registered we
  // can start making windows with it
  RegisterClass(wClass);

  // Create our main window
  Handle:=CreateWindow(
    'Sample Class',          // Registered Class Name
    'Encrypter - By: GoRDy', // Title of Window
    WS_OVERLAPPEDWINDOW or   // Basic Window Style
    WS_VISIBLE,              // Make it Visible
    10,                      // Left
    10,                      // Top
    400,                     // Width
    300,                     // Height
    0,                       // Parent Window Handle
    0,                       // Handle of Menu
    hInst,                   // Application Instance
    nil);                    // Structure for Creation Data

  // Create the Encrypt button
  hEncrypt:=CreateWindow(
    'Button',
    'Encrypt',
    WS_VISIBLE or WS_CHILD or BS_PUSHLIKE or BS_TEXT,
    5,5,65,24,Handle,0,hInst,nil);

  // Create the Decrypt button
  hDecrypt:=CreateWindow(
    'Button',
    'Decrypt',
    WS_VISIBLE or WS_CHILD or BS_PUSHLIKE or BS_TEXT,
    75,5,65,24,Handle,0,hInst,nil);

  // Create the main Edit
  hEdit:=CreateWindowEx(
    WS_EX_CLIENTEDGE, // this EX style is for the beveled edge
    'Edit',
    '',
    WS_VISIBLE or WS_CHILD or ES_LEFT or ES_MULTILINE or ES_WANTRETURN or ES_AUTOVSCROLL or WS_VSCROLL,
    5,34,380,234,Handle,0,hInst,nil);

  // Create the password Edit
  hPW:=CreateWindowEx(
    WS_EX_CLIENTEDGE,
    'Edit',
    '',
    WS_VISIBLE or WS_CHILD or ES_LEFT or ES_AUTOHSCROLL or ES_PASSWORD,
    230,5,155,24,Handle,0,hInst,nil);

  hLabel:=CreateWindow(
    'Static',
    'Password:',
    WS_VISIBLE or WS_CHILD or SS_LEFT,
    160,10,70,20,Handle,0,hInst,nil);

  // Create a custom font for our window otherwise
  // everything would use the system font (blech!)
  hFont:=CreateFont(
    -12,                           // Height
    0,                             // Width
    0,                             // Angle of Rotation
    0,                             // Orientation
    0,                             // Weight
    0,                             // Italic
    0,                             // Underline
    0,                             // Strike Out
    DEFAULT_CHARSET,               // Char Set
    OUT_DEFAULT_PRECIS,            // Precision
    CLIP_DEFAULT_PRECIS,           // Clipping
    DEFAULT_QUALITY,               // Render Quality
    DEFAULT_PITCH or FF_DONTCARE,  // Pitch & Family
    'MS Sans Serif');              // Font Name

  // Set the fonts for all our controls
  SendMessage(hEncrypt,WM_SETFONT,hFont,0);
  SendMessage(hDecrypt,WM_SETFONT,hFont,0);
  SendMessage(hEdit,WM_SETFONT,hFont,0);
  SendMessage(hPW,WM_SETFONT,hFont,0);
  SendMessage(hLabel,WM_SETFONT,hFont,0);

  // Subclass Encrypt Button (assign it a custom WindowProc)
  dEncrypt:=Pointer(GetWindowLong(hEncrypt,GWL_WNDPROC));
  SetWindowLong(hEncrypt,GWL_WNDPROC,Longint(@EncryptProc));

  // Subclass Decrypt Button
  dDecrypt:=Pointer(GetWindowLong(hDecrypt,GWL_WNDPROC));
  SetWindowLong(hDecrypt,GWL_WNDPROC,Longint(@DecryptProc));

  // The reason I don't subclass the Edit controls here
  // is because they don't do anything custom. If I wanted
  // them to Beep or something whenever you typed a "G" then
  // I would subclass them.

  // Focus on first control (otherwise people with no mouse are screwed)
  SetFocus(hEncrypt);

  // Now we loop GetMessage to process each Message in
  // our main window's message list. Every time the main
  // window recieves a message its added to the list, so
  // this loop here will eventually process it.

  while(GetMessage(Msg,Handle,0,0))do
  begin
    TranslateMessage(Msg);             // Translate any keyboard Msg's
    DispatchMessage(Msg);              // Send it to our WindowProc
  end;                                 // for processing.

end.
