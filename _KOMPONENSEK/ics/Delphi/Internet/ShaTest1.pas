{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jul 23, 2004
Description:  SHA1 test drive (Adapted from C-code shown in RFC-3174)
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2005 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit ShaTest1;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  IniFiles, StdCtrls, IcsSHA1, ExtCtrls;

type
  TSHA1TestForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SHA1TestForm: TSHA1TestForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSHA1TestForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSHA1TestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSHA1TestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSHA1TestForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSHA1TestForm.Button1Click(Sender: TObject);
const
    TEST1   = 'abc';
    TEST2a  = 'abcdbcdecdefdefgefghfghighijhi';
    TEST2b  = 'jkijkljklmklmnlmnomnopnopq';
    TEST2   = TEST2a + TEST2b;
    TEST3   = 'a';
    TEST4a  = '01234567012345670123456701234567';
    TEST4b  = '01234567012345670123456701234567';
    // an exact multiple of 512 bits
    TEST4   = TEST4a + TEST4b;
    TestArray   : array [0..3] of String  = (TEST1, TEST2, TEST3, TEST4);
    RepeatCount : array [0..3] of Integer = (1, 1, 1000000, 10 );
    ResultArray : array [0..3] of String  = (
             'A9 99 3E 36 47 06 81 6A BA 3E 25 71 78 50 C2 6C 9C D0 D8 9D',
             '84 98 3E 44 1C 3B D2 6E BA AE 4A A1 F9 51 29 E5 E5 46 70 F1',
             '34 AA 97 3C D4 C4 DA A4 F6 1E EB 2B DB AD 27 31 65 34 01 6F',
             'DE A3 56 A2 CD DD 90 C7 A7 EC ED C5 EB B5 63 93 4F 46 04 52');
type
    PSHA1Context = ^SHA1Context;
var
    Sha           : SHA1Context;
    I, J, Err     : Integer;
    MessageDigest : SHA1Digest;
    S             : String;
    Failed        : Boolean;
begin
    DisplayMemo.Clear;
    Failed := FALSE;
    for J := 0 to 3 do begin
        Display(Format('Test %d: %d, "%s"',
                       [J + 1, RepeatCount[J], TestArray[J]]));

        Err := SHA1Reset(Sha);
        if Err <> 0 then begin
            Display(Format('SHA1Reset Error %d.', [Err]));
            break;    // out of for J loop
        end;

        for I := 0 to RepeatCount[J] - 1 do begin
            Err := SHA1Input(Sha, @TestArray[J][1], Length(TestArray[J]));
            if Err <> 0 then begin
               Display(Format('SHA1Input Error %d.', [Err]));
               break;    // out of for I loop
            end;
        end;

        Err := SHA1Result(Sha, MessageDigest);
        if Err <> 0 then begin
            Display(Format(
            'SHA1Result Error %d, could not compute message digest.', [Err]));
            Failed := TRUE;
        end
        else begin
              S := '';
              for I := Low(MessageDigest) to High(MessageDigest) do
                  S := S + Format('%02.2X ', [Ord(MessageDigest[I])]);
              S := Trim(S);
              if CompareText(S, ResultArray[J]) <> 0 then begin
                  Failed := TRUE;
                  Display('**** FAILED');
              end;
              Display('Result: ' + S);
        end;

        Display('Wanted: ' + ResultArray[J]);
    end;

    // Test some error returns
    Display('Test 5: SHAInput state error');
    Err := SHA1Input(Sha, @TestArray[1][1], 1);
    if Err <> shaStateError then begin
        Display(Format('Error %d. Should be %d.', [Err, shaStateError]));
        Failed := TRUE;
    end
    else begin
        Display(Format('Result: %d', [Err]));
        Display(Format('Wanted: %d', [shaStateError]));
    end;
    Display('Test 6: SHA1Reset nil reference');
    Err := SHA1Reset(PSHA1Context(nil)^);
    if Err <> shaNull then begin
        Display(Format('Error %d. Should be %d.', [Err, shaNull]));
        Failed := TRUE;
    end
    else begin
        Display(Format('Result: %d', [Err]));
        Display(Format('Wanted: %d', [shaNull]));
    end;

    Display('');
    if Failed then
        Display('***** SHA1 TEST FAILED *****')
    else
        Display('===== SUCCESS =====');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
