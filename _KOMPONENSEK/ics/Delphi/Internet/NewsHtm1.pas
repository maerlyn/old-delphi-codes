{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     May 02, 2003
Description:  Simple program showing how to send a HTML news message using
              THtmlSmtCli component (ICS component family). The message can
              have embedded images or other type of documents or attached files.
Version:      1.00
EMail:        http://www.overbyte.be        http://www.rtfm.be/fpiette
              francois.piette@overbyte.be   francois.piette@rtfm.be
                                            francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2005 by François PIETTE
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
unit NewsHtm1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, NntpCli, MimeUtil;

type
  THtmlNewsForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    ServerEdit: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    GroupEdit: TEdit;
    PostButton: TButton;
    SubjectEdit: TEdit;
    Label2: TLabel;
    HtmlNntpClient: THtmlNntpCli;
    Label3: TLabel;
    PortEdit: TEdit;
    Panel1: TPanel;
    PlainTextMemo: TMemo;
    ImageFilesMemo: TMemo;
    HtmlTextMemo: TMemo;
    Label5: TLabel;
    FromEdit: TEdit;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HtmlNntpClientRequestDone(Sender: TObject;
      RqType: TNntpRequest; ErrorCode: Word);
    procedure PostButtonClick(Sender: TObject);
    procedure HtmlNntpClientDisplay(Sender: TObject; MsgBuf: Pointer;
      MsgLen: Integer);
    procedure HtmlNntpClientSessionConnected(Sender: TObject;
      ErrCode: Word);
    procedure Button1Click(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FRunning     : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HtmlNewsForm: THtmlNewsForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyServer          = 'Server';
    KeyPort            = 'Port';
    KeyFrom            = 'From';
    KeyGroup           = 'Group';
    KeySubject         = 'Subject';
    SectionHtmlText    = 'HtmlText';
    KeyHtmlText        = 'Html';
    SectionPlainText   = 'PlainText';
    KeyPlainText       = 'Plain';
    SectionImageFiles  = 'ImageFiles';
    KeyImageFiles      = 'File';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure SaveStringsToIniFile(
    const IniFileName : String;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings);
var
    IniFile : TIniFile;
    nItem   : Integer;
begin
    if (IniFileName = '') or (IniSection = '') or (IniKey = '') or
       (not Assigned(Strings)) then
        Exit;
    IniFile := TIniFile.Create(IniFileName);
    IniFile.EraseSection(IniSection);
    if Strings.Count <= 0 then
        IniFile.WriteString(IniSection, IniKey + 'EmptyFlag', 'Empty')
    else
        for nItem := 0 to Strings.Count - 1 do
            IniFile.WriteString(IniSection,
                                IniKey + IntToStr(nItem),
                                Strings.Strings[nItem]);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Return FALSE if non existant in IniFile                                   }
function LoadStringsFromIniFile(
    const IniFileName : String;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings) : Boolean;
var
    IniFile : TIniFile;
    nItem   : Integer;
    I       : Integer;
    Buf     : String;
begin
    Result := TRUE;
    if (IniFileName = '') or (IniSection = '') or (IniKey = '') or
       (not Assigned(Strings)) then
        Exit;
    Strings.Clear;
    IniFile := TIniFile.Create(IniFileName);
    try
        if IniFile.ReadString(IniSection, IniKey + 'EmptyFlag', '') <> '' then
             Exit;
        IniFile.ReadSectionValues(IniSection, Strings);
    finally
        IniFile.Free;
    end;
    nItem := Strings.Count - 1;
    while nItem >= 0 do begin
        Buf := Strings.Strings[nItem];
        if CompareText(IniKey, Copy(Buf, 1, Length(IniKey))) <> 0 then
            Strings.Delete(nItem)
        else begin
            if not (Buf[Length(IniKey) + 1] in ['0'..'9']) then
                Strings.Delete(nItem)
            else begin
                I := Pos('=', Buf);
                Strings.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
            end;
        end;
        Dec(nItem);
    end;
    Result := (Strings.Count <> 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.FormShow(Sender: TObject);
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
        ServerEdit.Text  := IniFile.ReadString(SectionData, KeyServer,
                                               'news.vienneinfo.org');
        PortEdit.Text    := IniFile.ReadString(SectionData, KeyPort,
                                               'nntp');
        GroupEdit.Text   := IniFile.ReadString(SectionData, KeyGroup,
                                               'vi.test');
        FromEdit.Text    := IniFile.ReadString(SectionData, KeyFrom,
                                               'first.last@company.com');
        SubjectEdit.Text := IniFile.ReadString(SectionData, KeySubject,
                                               'ICS html NNTP component');
        if not LoadStringsFromIniFile(IniFileName, SectionImageFiles,
                                      KeyImageFiles, ImageFilesMemo.Lines) then
        ImageFilesMemo.Text := 'ics_logo.gif' + #13#10 + 'fp_small.gif';
        if not LoadStringsFromIniFile(IniFileName, SectionPlainText,
                                      KeyPlainText, PlainTextMemo.Lines) then
            PlainTextMemo.Text :=
            'This is a HTML mail message sent using ICS.' + #13#10 +
            'Internet Component Suite is freeware.' + #13#10 +
            '<<IMAGE1>>' + #13#10 +
            'You can download ICS full source code from ' + #13#10 +
            'Overbyte website <http://www.overbyte.be>.' + #13#10 +
            #13#10 +
            'Need to secure your applications using ICS ?' + #13#10 +
            'Need to access secure web pages using HTTPS ?' + #13#10 +
            'Think about contributing to the ICS-SSL effort !' + #13#10 +
            'Visit the ICS-SSL website ' +
            '<http://overbyte.delphicenter.com/eng/ssl.html>.' + #13#10 +
            #13#10 +
            'Need high performance multi-tier applications ?' + #13#10 +
            'Then you need MidWare. It''s a full featured toolkit' + #13#10 +
            'to build powerful N-tier applications with Delphi.' + #13#10 +
            'It work equally well across the Internet or just on' + #13#10 +
            'your LAN/WAN. MidWare include two sets of components:' + #13#10 +
            'one to build your own application servers and one to' + #13#10 +
            'build your thin custom clients. MidWare can use any' + #13#10 +
            'database or even no database at all. Download full ' + #13#10 +
            'source code from http://www.overbyte.be' + #13#10 + #13#10 +
            'ICS and MidWare are creations of François Piette.' + #13#10 +
            '<<IMAGE2>>' + #13#10 +
            '--' + #13#10 +
            'francois.piette@overbyte.be' + #13#10;
        if not LoadStringsFromIniFile(IniFileName, SectionHtmlText,
                                      KeyHtmlText, HtmlTextMemo.Lines) then
            HtmlTextMemo.Text := '<HTML><BODY>' + #13#10 +
            'This is a HTML mail message sent using <B>ICS</B>.<BR>' + #13#10 +
            '<B>I</B>nternet <B>C</B>omponent ' +
            '<B>S</B>uite is <U>freeware</U>.<BR><BR>' + #13#10 +
            '<A HREF="http://www.overbyte.be">' +
            '<IMG SRC="cid:IMAGE1" WIDTH=148 HEIGHT=105 BORDER=0></A><BR><BR>' + #13#10 +
            'You can download ICS full source code from' + #13#10 +
            '<A HREF="http://www.overbyte.be">' +
            'Overbyte website</A>.<BR>' + #13#10 +
            'ICS is a PostcardWare: you must send a picture postcard ' +
            'to the author if you are using the code. You can find the ' +
            'details in the readme.txt file.<BR><BR>' + #13#10 +
            'Need to <U>secure your applications</U> using ICS ?<BR>' + #13#10 +
            'Need to access secure web pages using <B>HTTPS</B> ?<BR>' + #13#10 +
            'Think about contributing to the ICS-SSL effort !<BR>' + #13#10 +
            'Visit the <A HREF="http://overbyte.delphicenter.com/eng/ssl.html">' +
            'ICS-SSL website</A>.<BR><BR>' + #13#10 +
            'Need high performance multi-tier applications ?<BR>' + #13#10 +
            'Then you need <A HREF="http://www.overbyte.be">MidWare</A>. ' +
            'It''s a full featured toolkit '+
            'to build powerful N-tier applications with Delphi. It work ' +
            'equally well across the Internet or just on your LAN/WAN. ' +
            'MidWare include two sets of components: one to build your own ' +
            'application servers and one to build your thin custom clients. ' +
            'MidWare can use any database or even no database ' +
            'at all. Download full source code from ' +
            '<A HREF="http://www.overbyte.be">here</A><BR><BR>' + #13#10 +
            'ICS and MidWare are creations of François Piette<BR>' + #13#10 +
            '<A HREF="mailto:francois.piette@overbyte.be?' +
            'subject=ICS%26MIDWARE">' +
            '<IMG SRC="cid:IMAGE2" BORDER=0 WIDTH=92 HEIGHT=121></A>' +
            '<BR>--<BR>' + #13#10 +
            '<A HREF="mailto:francois.piette@overbyte.be?' +
            'subject=ICS%26MIDWARE">francois.piette@overbyte.be</A>' + #13#10 +
            '</BODY>' + #13#10 +
            '</HTML>' + #13#10;
        IniFile.Destroy;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyServer,  ServerEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,    PortEdit.Text);
    IniFile.WriteString(SectionData, KeyGroup,   GroupEdit.Text);
    IniFile.WriteString(SectionData, KeyFrom,    FromEdit.Text);
    IniFile.WriteString(SectionData, KeySubject, SubjectEdit.Text);
    SaveStringsToIniFile(IniFileName, SectionImageFiles,
                         KeyImageFiles, ImageFilesMemo.Lines);
    SaveStringsToIniFile(IniFileName, SectionPlainText,
                         KeyPlainText, PlainTextMemo.Lines);
    SaveStringsToIniFile(IniFileName, SectionHtmlText,
                         KeyHtmlText, HtmlTextMemo.Lines);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.Display(Msg : String);
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
procedure THtmlNewsForm.HtmlNntpClientRequestDone(
    Sender    : TObject;
    RqType    : TNntpRequest;
    ErrorCode : Word);
begin
     { For every operation, we display the status }
     if (ErrorCode > 0) and  (ErrorCode < 10000) then
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                ' Error='+ HtmlNntpClient.ErrorMessage)
     else
        Display('RequestDone Rq=' + IntToStr(Ord(RqType)) +
                ' Error='+ IntToStr(ErrorCode));
    if not FRunning then
        Exit;
    { Start next operation, but first check if previous one was OK }
    if ErrorCode <> 0 then begin
        FRunning := FALSE;   { Terminate All-In-One demo }
        Display('Error, stop.');
        Exit;
    end;
    case RqType of
    nntpConnect:  HtmlNntpClient.Group(GroupEdit.Text);
    nntpGroup:    HtmlNntpClient.Post(nil);
    nntpPost:     HtmlNntpClient.Quit;
    nntpQuit:     begin
                      Display('Done !');
                      FRunning := FALSE;
                  end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.PostButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    HtmlNntpClient.PlainText     := PlainTextMemo.Lines;
    HtmlNntpClient.HtmlText      := HtmlTextMemo.Lines;
    HtmlNntpClient.AttachedFiles := ImageFilesMemo.Lines;
    HtmlNntpClient.Host          := ServerEdit.Text;
    HtmlNntpClient.Port          := PortEdit.Text;
    HtmlNntpClient.HdrSubject    := SubjectEdit.Text;
    HtmlNntpClient.HdrFrom       := FromEdit.Text;
    HtmlNntpClient.HdrGroup      := GroupEdit.Text;
    FRunning                     := TRUE;
    HtmlNntpClient.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.HtmlNntpClientDisplay(
    Sender : TObject;
    MsgBuf : Pointer;
    MsgLen : Integer);
begin
    Display('> ' + StrPas(MsgBuf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THtmlNewsForm.HtmlNntpClientSessionConnected(
    Sender  : TObject;
    ErrCode : Word);
begin
    Display('Session connected');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure THtmlNewsForm.Button1Click(Sender: TObject);
begin
    Display(SplitQuotedPrintableString(EncodeQuotedPrintable(
'12345678901234567890123456789012345678901234567890123456789012345678901234567890' +
'12345678901234567890123456789012345678901234567890123456789012345é78901234567890')));

    Display(DecodeQuotedPrintable('123456=' + #13#10 + '7890=E9t=E9'));
end;

end.
