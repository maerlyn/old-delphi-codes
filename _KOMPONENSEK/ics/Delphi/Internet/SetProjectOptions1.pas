{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Apr 27, 2003
Description:
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2003 by François PIETTE
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
unit SetProjectOptions1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls;

type
  TAppBaseForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    FileListBox: TListBox;
    FileNameEdit: TEdit;
    AddButton: TButton;
    DeleteFileButton: TButton;
    ProcessFilesButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteFileButtonClick(Sender: TObject);
    procedure ProcessFilesButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  AppBaseForm: TAppBaseForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


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
    for nItem := 0 to Strings.Count - 1 do
        IniFile.WriteString(IniSection,
                            IniKey + IntToStr(nItem),
                            Strings.Strings[nItem]);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadFromIniFile(
    const IniFileName : String;
    const IniSection  : String;
    const IniKey      : String;
    Strings           : TStrings);
var
    IniFile : TIniFile;
    nItem   : Integer;
    I       : Integer;
    Buf     : String;
begin
    if (IniFileName = '') or (IniSection = '') or (IniKey = '') or
       (not Assigned(Strings)) then
        Exit;
    Strings.Clear;
    IniFile := TIniFile.Create(IniFileName);
    IniFile.ReadSectionValues(IniSection, Strings);
    IniFile.Free;
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
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormCreate(Sender: TObject);
begin
    FIniFileName := ChangeFileExt(Application.ExeName, '.ini');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
    Temp    : TStringList;
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
        LoadFromIniFile(FIniFileName, 'Files', 'File', FileListBox.Items);
        Temp    := TStringList.Create;
        Temp.Assign(FileListBox.Items);
        Temp.Sorted := TRUE;
        FileListBox.Items.Assign(Temp);
        Temp.Free;
        FileNameEdit.Clear;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.Destroy;
    SaveStringsToIniFile(FIniFileName, 'Files', 'File', FileListBox.Items);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.Display(Msg : String);
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
procedure TAppBaseForm.AddButtonClick(Sender: TObject);
var
    Buf : String;
    I   : Integer;
    Temp : TStringList;
begin
    Buf := Trim(FileNameEdit.Text);
    if Buf <> '' then begin
        I := FileListBox.Items.Count - 1;
        while I >= 0 do begin
            if CompareText(Buf, FileListBox.Items.Strings[I]) = 0 then
                break;   // Already exists, do not add again
            Dec(I);
        end;
        if I < 0 then begin
            FileListBox.Items.Add(Buf);
            Temp    := TStringList.Create;
            Temp.Assign(FileListBox.Items);
            Temp.Sorted := TRUE;
            FileListBox.Items.Assign(Temp);
            Temp.Free;
        end;
    end;
    FileNameEdit.SetFocus;
    FileNameEdit.SelectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.DeleteFileButtonClick(Sender: TObject);
var
    Selected : Integer;
begin
    Selected := FileListBox.ItemIndex;
    if Selected >= 0 then begin
        FileNameEdit.Text := FileListBox.Items[Selected];
        FileListBox.DeleteSelected;
        if Selected < FileListBox.Count then
            FileListBox.ItemIndex := Selected
        else
            FileListBox.ItemIndex := Selected - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure ProcessFile(const FileName : String);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FileName);
    IniFile.WriteInteger('Compiler', 'SymbolDeprecated', 0);
    IniFile.WriteInteger('Compiler', 'SymbolLibrary', 0);
    IniFile.WriteInteger('Compiler', 'SymbolPlatform', 0);
    IniFile.WriteInteger('Compiler', 'UnsafeType', 0);
    IniFile.WriteInteger('Compiler', 'UnsafeCode', 0);
    IniFile.WriteInteger('Compiler', 'UnsafeCast', 0);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TAppBaseForm.ProcessFilesButtonClick(Sender: TObject);
var
    I       : Integer;
    Buf     : String;
begin
    DisplayMemo.Clear;
    for I := 0 to FileListBox.Count - 1 do begin
        Buf := '.\' + FileListBox.Items.Strings[I];
        if FileExists(Buf) then begin
            Display('Processing ' + Buf);
            ProcessFile(Buf);
        end
        else
            Display(Buf + ' Doesn''t exist.');
    end;
    Display('Done.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
