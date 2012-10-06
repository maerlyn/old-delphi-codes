{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Copyright:    You can use this software freely, at your own risks
Creation:     April 4, 1997
Version:      2.04
Object:       Demo program to show how to use TWSocket object to listen
              UDP messages from the network. Use UDPSend or any other
              program to send UDP messages.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
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

Updates:
Jul 23, 1997 Adapted for Delphi 1, 2 and 3
Sep 06, 1997 Version 2.01
Sep 27, 1997 Updated for TWSocket changes
             Replace loopback address by real localhost IP addr
Dec 12, 1998 V2.02 Added icomming IP and port number display
Mar 07, 1999 V2.03 Corrected compatibility bug with Delphi 1
Jan 11, 2004 V2.04 Beautified code. Removed FormPos dependency.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UdpLstn1;

{$J+}

interface

uses
  WinTypes, Messages, SysUtils, Classes, Controls, Forms, StdCtrls, IniFiles,
  WinSock, WSocket;

const
  UdpLstnVersion     = 204;
  CopyRight : String = ' UdpLstn (c) 1997-2005 F. Piette V2.04 ';

type
  TMainForm = class(TForm)
    WSocket: TWSocket;
    StartButton: TButton;
    DataAvailableLabel: TLabel;
    InfoLabel: TLabel;
    StopButton: TButton;
    PortEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SenderEdit: TEdit;
    AnyServerCheckBox: TCheckBox;
    procedure StartButtonClick(Sender: TObject);
    procedure WSocketDataAvailable(Sender: TObject; Error: Word);
    procedure WSocketSessionConnected(Sender: TObject; Error: Word);
    procedure StopButtonClick(Sender: TObject);
    procedure WSocketSessionClosed(Sender: TObject; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure AnyServerCheckBoxClick(Sender: TObject);
    procedure SenderEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SenderEditExit(Sender: TObject);
  private
    FIniFileName   : String;
    FInitialized   : Boolean;
    FSenderAddr    : TInAddr;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

const
    SectionWindow = 'MainForm';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    SectionData   = 'Data';
    KeyPort       = 'Port';
    KeySender     = 'Sender';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormShow(Sender: TObject);
var
    IniFile   : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIniFile.Create(FIniFileName);
        Width   := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height  := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top     := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left    := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);
        PortEdit.Text   := IniFile.ReadString(SectionData, KeyPort,   '600');
        SenderEdit.Text := IniFile.ReadString(SectionData, KeySender, '0.0.0.0');
        IniFile.Free;
        DataAvailableLabel.Caption := '';
        InfoLabel.Caption          := 'Click on Start button';
        StartButton.Enabled        := TRUE;
        StopButton.Enabled         := FALSE;
        AnyServerCheckBox.Checked  := (Trim(SenderEdit.Text) = '0.0.0.0');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile   : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteString(SectionData, KeySender,    SenderEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.StartButtonClick(Sender: TObject);
begin
    FSenderAddr               := WSocketResolveHost(SenderEdit.Text);
    if FSenderAddr.S_addr = htonl(INADDR_LOOPBACK) then begin
        { Replace loopback address by real localhost IP addr }
        FSenderAddr           := WSocketResolveHost(LocalHostName);
    end;
    WSocket.Proto             := 'udp';
    WSocket.Addr              := '0.0.0.0';
    WSocket.Port              := PortEdit.Text;
    WSocket.Listen;
    PortEdit.Enabled          := FALSE;
    SenderEdit.Enabled        := FALSE;
    AnyServerCheckBox.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(value : string) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] in ['0'..'9']) do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : array [0..1023] of char;
    Len    : Integer;
    Src    : TSockAddrIn;
    SrcLen : Integer;
begin
    SrcLen := SizeOf(Src);
    Len    := WSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
    if Len >= 0 then begin
        if (FSenderAddr.S_addr = INADDR_ANY) or
           (FSenderAddr.S_addr = Src.Sin_addr.S_addr) then begin
            Buffer[Len] := #0;
            DataAvailableLabel.Caption := IntToStr(atoi(DataAvailableLabel.caption) + 1) +
                                          '  ' + StrPas(inet_ntoa(Src.sin_addr)) +
                                          ':'  + IntToStr(ntohs(Src.sin_port)) +
                                          '--> ' + StrPas(Buffer);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketSessionConnected(Sender: TObject;
  Error: Word);
begin
    StartButton.Enabled := FALSE;
    StopButton.Enabled  := TRUE;
    InfoLabel.Caption   := 'Listenning';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.StopButtonClick(Sender: TObject);
begin
    StartButton.Enabled       := TRUE;
    StopButton.Enabled        := FALSE;
    PortEdit.Enabled          := TRUE;
    SenderEdit.Enabled        := TRUE;
    AnyServerCheckBox.Enabled := TRUE;
    WSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    StartButton.Enabled        := TRUE;
    StopButton.Enabled         := FALSE;
    PortEdit.Enabled           := TRUE;
    SenderEdit.Enabled         := TRUE;
    AnyServerCheckBox.Enabled  := TRUE;
    InfoLabel.Caption          := 'Disconnected';
    DataAvailableLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.AnyServerCheckBoxClick(Sender: TObject);
begin
    if AnyServerCheckBox.Checked then
        SenderEdit.Text := '0.0.0.0'
    else begin
        ActiveControl := SenderEdit;
        SenderEdit.SelectAll;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.SenderEditChange(Sender: TObject);
begin
    AnyServerCheckBox.Checked := (Trim(SenderEdit.Text) = '0.0.0.0');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.SenderEditExit(Sender: TObject);
begin
    AnyServerCheckBox.Checked := (Trim(SenderEdit.Text) = '0.0.0.0');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

