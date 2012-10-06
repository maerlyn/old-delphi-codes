{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmdEditor.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmdEditor.pas file from the
mwEdit component suite by Martin Waldenburg and other developers.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynEditKeyCmdEditor.pas,v 1.2 2001/05/31 12:07:07 claplace Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.


The Original Code is based on , part of the mwEdit component
suite.
All Rights Reserved.

Contributors:

  For a list of the contributors to the mwEdit project see the
  accompanying Contributors.mwEdit.txt file.

Last Modified: 2000-04-07
Current Version: 1.00

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKeyCmdEditor;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_KYLIX}
  Qt, QGraphics, QMenus, QControls, QForms, QDialogs,
  QStdCtrls, QComCtrls,
  {$ELSE}
  Windows, Messages, Graphics, Menus, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls,
  {$ENDIF}
  SynEditKeyCmds;


type
  TSynEditKeystrokeEditorForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cmbCommand: TComboBox;
    {*****************}
    {$IFNDEF SYN_KYLIX}
    hkKeystroke: THotKey;
    {$ENDIF}
    btnOK: TButton;
    btnCancel: TButton;
    bntClearKey: TButton;
    Label4: TLabel;
    {*****************}
    {$IFNDEF SYN_KYLIX}
    hkKeystroke2: THotKey;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure bntClearKeyClick(Sender: TObject);
  private
    procedure SetCommand(const Value: TSynEditorCommand);
    procedure SetKeystroke(const Value: TShortcut);
    procedure AddEditorCommand(const S: string);
    function GetCommand: TSynEditorCommand;
    function GetKeystroke: TShortcut;
    function GetKeystroke2: TShortcut;
    procedure SetKeystroke2(const Value: TShortcut);
  public
    property Command: TSynEditorCommand read GetCommand write SetCommand;
    property Keystroke: TShortcut read GetKeystroke write SetKeystroke;
    property Keystroke2: TShortcut read GetKeystroke2 write SetKeystroke2;
  end;

var
  SynEditKeystrokeEditorForm: TSynEditKeystrokeEditorForm;

implementation

{$R *.dfm}

{ TSynEditKeystrokeEditorForm }

procedure TSynEditKeystrokeEditorForm.SetCommand(const Value: TSynEditorCommand);
begin
  cmbCommand.Text := EditorCommandToCodeString(Value);
end;

procedure TSynEditKeystrokeEditorForm.SetKeystroke(const Value: TShortcut);
begin
  {*****************}
  {$IFNDEF SYN_KYLIX}
  hkKeystroke.Hotkey := Value;
  {$ENDIF}
end;

procedure TSynEditKeystrokeEditorForm.FormCreate(Sender: TObject);
begin
  GetEditorCommandValues(AddEditorCommand);
end;

procedure TSynEditKeystrokeEditorForm.AddEditorCommand(const S: string);
begin
  cmbCommand.Items.Add(S);
end;

function TSynEditKeystrokeEditorForm.GetCommand: TSynEditorCommand;
var
  NewCmd: longint;
begin
  if not IdentToEditorCommand(cmbCommand.Text, NewCmd) then
  begin
     try
       NewCmd := StrToInt(cmbCommand.Text);
     except
       NewCmd := ecNone;
     end;
  end;
  Result := NewCmd;
end;

function TSynEditKeystrokeEditorForm.GetKeystroke: TShortcut;
begin
 {*****************}
 {$IFNDEF SYN_KYLIX}
  Result := hkKeystroke.HotKey;
 {$ENDIF}
end;

procedure TSynEditKeystrokeEditorForm.FormKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  // THotKey uses backspace to remove the current keystroke.  That would prevent
  // us from assigning backspace to anything.  We have to handle it here.
  {*****************}
  {$IFNDEF SYN_KYLIX}
  if (Key = VK_BACK) and
     (hkKeystroke.Focused) then
  begin
    hkKeystroke.HotKey := Menus.ShortCut(Key, Shift);
    Key := 0;  // Eat the key so THotKey doesn't get it.
  end;
  if (Key = VK_BACK) and (hkKeystroke2.Focused) then
  begin
    hkKeystroke2.HotKey := Menus.ShortCut(Key, Shift);
    Key := 0;  // Eat the key so THotKey doesn't get it.
  end;
  {$ENDIF}
end;

procedure TSynEditKeystrokeEditorForm.bntClearKeyClick(Sender: TObject);
begin
 {*****************}
 {$IFNDEF SYN_KYLIX}
  hkKeystroke.HotKey := 0;
  hkKeystroke2.HotKey := 0;
  {$ENDIF}
end;

function TSynEditKeystrokeEditorForm.GetKeystroke2: TShortcut;
begin
 {*****************}
 {$IFNDEF SYN_KYLIX}
  Result := hkKeystroke2.HotKey;
 {$ENDIF}
end;

procedure TSynEditKeystrokeEditorForm.SetKeystroke2(const Value: TShortcut);
begin
 {*****************}
 {$IFNDEF SYN_KYLIX}
  hkKeystroke2.Hotkey := Value;
 {$ENDIF}
end;

end.



