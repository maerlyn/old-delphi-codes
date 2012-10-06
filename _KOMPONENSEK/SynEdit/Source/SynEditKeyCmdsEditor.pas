{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditKeyCmdsEditor.pas, released 2000-04-07.
The Original Code is based on the mwKeyCmdsEditor.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is Brad Stowers.
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

$Id: SynEditKeyCmdsEditor.pas,v 1.2 2001/05/31 12:07:07 claplace Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditKeyCmdsEditor;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_KYLIX}
  Qt, QGraphics, QControls, QForms, QDialogs,
  QComCtrls, QMenus, QStdCtrls,
  {$ELSE}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Menus, StdCtrls,
  {$ENDIF}
  SynEditKeyCmds;

type
  TSynEditKeystrokesEditorForm = class(TForm)
    KeyCmdList: TListView;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    btnClear: TButton;
    btnReset: TButton;
    procedure FormResize(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    FKeystrokes: TSynEditKeystrokes;
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure UpdateKeystrokesList;
    {**************}
    {$IFNDEF SYN_KYLIX}
    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
      message WM_GETMINMAXINFO;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Keystrokes: TSynEditKeyStrokes read FKeystrokes write SetKeystrokes;
  end;

implementation

{$R *.dfm}

uses
  SynEditKeyCmdEditor, SynEditStrConst;

{ TSynEditKeystrokesEditorForm }

constructor TSynEditKeystrokesEditorForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FKeystrokes := NIL;
end;

destructor TSynEditKeystrokesEditorForm.Destroy;
begin
  FKeystrokes.Free;
  inherited Destroy;
end;

procedure TSynEditKeystrokesEditorForm.SetKeystrokes(const Value:
  TSynEditKeyStrokes);
begin
  if FKeystrokes = NIL then
    FKeystrokes := TSynEditKeyStrokes.Create(Self);
  FKeystrokes.Assign(Value);
  UpdateKeystrokesList;
end;

procedure TSynEditKeystrokesEditorForm.UpdateKeystrokesList;
var
  x: integer;
begin
  KeyCmdList.Items.BeginUpdate;
  try
    KeyCmdList.Items.Clear;
    for x := 0 to FKeystrokes.Count-1 do
    begin
      with KeyCmdList.Items.Add do
      begin
        Caption := EditorCommandToCodeString(FKeystrokes[x].Command);
        if FKeystrokes[x].ShortCut = 0 then
          SubItems.Add(SYNS_ShortCutNone)
        else
          if FKeystrokes[x].ShortCut2 = 0 then
            {$IFDEF SYN_KYLIX}
            SubItems.Add(QMenus.ShortCutToText(FKeystrokes[x].ShortCut))
            {$ELSE}
            SubItems.Add(Menus.ShortCutToText(FKeystrokes[x].ShortCut))
            {$ENDIF}
          else
            {$IFDEF SYN_KYLIX}
            SubItems.Add(QMenus.ShortCutToText(FKeystrokes[x].ShortCut)+ ' '+
              QMenus.ShortCutToText(FKeystrokes[x].ShortCut2));
            {$ELSE}
            SubItems.Add(Menus.ShortCutToText(FKeystrokes[x].ShortCut)+ ' '+
              Menus.ShortCutToText(FKeystrokes[x].ShortCut2));
            {$ENDIF}
      end;
    end;
  finally
    KeyCmdList.Items.EndUpdate;
  end;
end;

procedure TSynEditKeystrokesEditorForm.FormResize(Sender: TObject);
var
  x: integer;
begin
  for x := 0 to ControlCount-1 do
    if Controls[x] is TButton then
    begin
      Controls[x].Left := ClientWidth - Controls[x].Width - 7;
      if Controls[x] = btnOK then
        Controls[x].Top := ClientHeight - (Controls[x].Height * 2) - 10;
      if Controls[x] = btnCancel then
        Controls[x].Top := ClientHeight - Controls[x].Height - 3;
    end else if Controls[x] is TListView then
    begin
      Controls[x].Width := ClientWidth - 96;
      Controls[x].Height := ClientHeight - 8;
    end;
end;

{***************}
{$IFNDEF SYN_KYLIX}
procedure TSynEditKeystrokesEditorForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  Msg.MinMaxInfo.ptMinTrackSize := Point(300, 225);
end;
{$ENDIF}

procedure TSynEditKeystrokesEditorForm.btnAddClick(Sender: TObject);
var
  NewStroke: TSynEditKeyStroke;
begin
  with TSynEditKeystrokeEditorForm.Create(Self) do
    try
      Command := ecNone;
      Keystroke := 0;
      Keystroke2 := 0;
      if ShowModal = mrOK then
      begin
        NewStroke := FKeystrokes.Add;
        NewStroke.Command := Command;
        try
          NewStroke.ShortCut := Keystroke;
          NewStroke.ShortCut2 := Keystroke2;
        except
          on ESynKeyError do
            begin
              // Shortcut already exists in the collection!
              {$IFDEF KYLIX}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg,
                [QMenus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {ELSE}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg,
                [Menus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {$ENDIF}
              NewStroke.Free;
              exit;
            end;
          // Some other kind of exception, we don't deal with it...
        end;

        with KeyCmdList.Items.Add do
        begin
          Caption := EditorCommandToCodeString(NewStroke.Command);
          if NewStroke.ShortCut = 0 then
            SubItems.Add(SYNS_ShortcutNone)
          else
          if NewStroke.ShortCut2 = 0 then
            {$IFDEF SYN_KYLIX}
            SubItems.Add(QMenus.ShortCutToText(NewStroke.ShortCut))
            {$ELSE}
            SubItems.Add(Menus.ShortCutToText(NewStroke.ShortCut))
            {$ENDIF}
          else
            {$IFDEF SYN_KYLIX}
            SubItems.Add(QMenus.ShortCutToText(NewStroke.ShortCut)+ ' '+
              QMenus.ShortCutToText(NewStroke.ShortCut2));
            {$ELSE}
            SubItems.Add(Menus.ShortCutToText(NewStroke.ShortCut)+ ' '+
              Menus.ShortCutToText(NewStroke.ShortCut2));
            {$ENDIF}
        end;
      end;
    finally
      Free;
    end;
end;

procedure TSynEditKeystrokesEditorForm.btnEditClick(Sender: TObject);
var
  SelItem: TListItem;
  OldShortcut: TShortcut;
  OldShortcut2: TShortcut;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = NIL then
  begin
    {$IFDEF SYN_KYLIX}
    QControls.Beep;
    {$ELSE}
    MessageBeep(1);
    {$ENDIF}
    exit;
  end;
  with TSynEditKeystrokeEditorForm.Create(Self) do
    try
      Command := FKeystrokes[SelItem.Index].Command;
      Keystroke := FKeystrokes[SelItem.Index].Shortcut;
      Keystroke2 := FKeystrokes[SelItem.Index].Shortcut2;
      if ShowModal = mrOK then
      begin
        FKeystrokes[SelItem.Index].Command := Command;
        OldShortCut := FKeystrokes[SelItem.Index].ShortCut;
        OldShortCut2 := FKeystrokes[SelItem.Index].ShortCut2;
        try
          FKeystrokes[SelItem.Index].ShortCut := Keystroke;
          FKeystrokes[SelItem.Index].ShortCut2 := Keystroke2;
        except
          on ESynKeyError do
            begin
              // Shortcut already exists in the collection!
              {$IFDEF SYN_KYLIX}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg2,
                [QMenus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {$ELSE}
              MessageDlg(Format(SYNS_DuplicateShortcutMsg2,
                [Menus.ShortCutToText(Keystroke)]), mtError, [mbOK], 0);
              {$ENDIF}
              FKeystrokes[SelItem.Index].ShortCut := OldShortCut;
              FKeystrokes[SelItem.Index].ShortCut2 := OldShortCut2;
            end;
          // Some other kind of exception, we don't deal with it...
        end;

        KeyCmdList.Items.BeginUpdate;
        try
          with SelItem do
          begin
            Caption := EditorCommandToCodeString(FKeystrokes[Index].Command);
            if FKeystrokes[Index].ShortCut = 0 then
              SubItems[0] := SYNS_ShortcutNone
            else
              if FKeystrokes[Index].ShortCut2 = 0 then
                {$IFDEF SYN_KYLIX}
                SubItems[0] := QMenus.ShortCutToText(FKeystrokes[Index].ShortCut)
                {$ELSE}
                SubItems[0] := Menus.ShortCutToText(FKeystrokes[Index].ShortCut)
                {$ENDIF}
              else
                {$IFDEF SYN_KYLIX}
                SubItems[0] := QMenus.ShortCutToText(FKeystrokes[Index].ShortCut)
                  + ' ' + QMenus.ShortCutToText(FKeystrokes[Index].ShortCut2);
                {$ELSE}
                SubItems[0] := Menus.ShortCutToText(FKeystrokes[Index].ShortCut)
                  + ' ' + Menus.ShortCutToText(FKeystrokes[Index].ShortCut2);
                {$ENDIF}
          end;
        finally
          KeyCmdList.Items.EndUpdate;
        end;
      end;
    finally
      Free;
    end;
end;

procedure TSynEditKeystrokesEditorForm.btnDeleteClick(Sender: TObject);
var
  SelItem: TListItem;
begin
  SelItem := KeyCmdList.Selected;
  if SelItem = NIL then
  begin
    {$IFDEF SYN_KYLIX}
    QControls.Beep;
    {$ELSE}
    MessageBeep(1);
    {$ENDIF}
    exit;
  end;
  FKeystrokes[SelItem.Index].Free;
  KeyCmdList.Items.Delete(SelItem.Index);
end;

procedure TSynEditKeystrokesEditorForm.btnClearClick(Sender: TObject);
begin
  FKeystrokes.Clear;
  KeyCmdList.Items.Clear;
end;

procedure TSynEditKeystrokesEditorForm.btnResetClick(Sender: TObject);
begin
  FKeystrokes.ResetDefaults;
  UpdateKeystrokesList;
end;

procedure TSynEditKeystrokesEditorForm.FormCreate(Sender: TObject);
begin
  {$IFDEF SYN_COMPILER_3_UP}
  KeyCmdList.RowSelect := TRUE;
  {$ENDIF}
end;

end.

