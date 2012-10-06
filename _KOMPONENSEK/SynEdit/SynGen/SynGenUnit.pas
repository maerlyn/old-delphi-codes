{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynGenUnit.pas, released 2000-04-19.
Description: Generator for skeletons of HighLighters to use in SynEdit,
drived by a simple grammar.

The Original Code is based on SynGenU.pas by Martin Waldenburg, part of
the mwEdit component suite.
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

$Id: SynGenUnit.pas,v 1.1.1.1 2000/07/08 15:54:05 mghie Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Todo:
  - Remember the last opened MSG file
  - Use TRegIniFile instead of TIniFile
  - Double-click a MSG file opens SynGen
  - SynGen should not halt when TOpenFileDialog is cancelled
  - Add user-defined default attributes to TSynXXXSyn.Create
  - SynEdit to edit the MSG file (needs highlighter for MSG files)
  - Store language names list and attribute names list in INI file
  - SynEdit with Pascal highlighter to preview the created highlighter source

Known Issues:
-------------------------------------------------------------------------------}

unit SynGenUnit;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GenLex, ComCtrls, CheckLst, Menus;

var
  mKeyHashTable: array[#0..#255] of Integer;
  mSKeyHashTable: array[#0..#255] of Integer;

type
  TLexKeys = Class
  public
    KeyName: String;
    Key: Integer;
  end;

  TLexCharsets = Class
  public
    SetName: String;
    Charset: String;
    ProcData: String;
    FuncData: String;
  end;

  TFrmMain = class(TForm)
    BtnStart: TButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    TabLanguage: TTabSheet;
    LblFilter: TLabel;
    CboFilter: TComboBox;
    LblLangName: TLabel;
    CboLangName: TComboBox;
    TabAttributes: TTabSheet;
    GrpAttrNames: TGroupBox;
    LblIdentifier: TLabel;
    LblReservedWord: TLabel;
    CboAttrIdentifier: TComboBox;
    CboAttrReservedWord: TComboBox;
    LblUnknownTokenAttr: TLabel;
    CboUnknownTokenAttr: TComboBox;
    TabFields: TTabSheet;
    BtnAdd: TButton;
    BtnDelete: TButton;
    EditAddField: TEdit;
    ListBoxFields: TListBox;
    MainMenu: TMainMenu;
    MnuFile: TMenuItem;
    MnuOpen: TMenuItem;
    MnuExit: TMenuItem;
    TabHighlighter: TTabSheet;
    LblAuthor: TLabel;
    LblDescription: TLabel;
    LblVersion: TLabel;
    EditAuthor: TEdit;
    EditDescription: TEdit;
    EditVersion: TEdit;
    MnuStart: TMenuItem;
    ChkGetKeyWords: TCheckBox;
    procedure BtnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CboLangNameChange(Sender: TObject);
    procedure ListBoxFieldsClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure EditAddFieldChange(Sender: TObject);
    procedure EditAddFieldKeyPress(Sender: TObject; var Key: Char);
    procedure MnuExitClick(Sender: TObject);
    procedure MnuOpenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    LexName: String;
    IdentPre: String;
    IdentStart: String;
    IdentContent: String;
    FFileName: string;
    IniFile: string;
    OutFile: TextFile;
    Sensitivity: Boolean;
    Stream: TMemoryStream;
    Lex: TGenLex;
    KeyList: TList;
    SetList: TList;
    IdentList: TStringList;
    procedure MakeHashTable;
    procedure MakeSensitiveHashTable;
    procedure FillKeyList;
    procedure OutFileCreate(InName: String);
    procedure ParseCharsets;
    procedure RetriveCharset;
    procedure WriteSettings;
    function PerformFileOpen: Boolean;
    function KeyHash(ToHash: String): Integer;
    function SensKeyHash(ToHash: String): Integer;
    procedure WriteRest;
  public
  end;

var
  FrmMain: TFrmMain;

implementation

uses IniFiles;

{$R *.DFM}

function CompareKeys(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if TLexKeys(Item1).Key < TLexKeys(Item2).Key then
    Result := -1
  else
    if TLexKeys(Item1).Key > TLexKeys(Item2).Key then Result := 1;
end;

function CompareSets(Item1, Item2: Pointer): Integer;
begin
  Result := 0;
  if TLexCharsets(Item1).SetName < TLexCharsets(Item2).SetName then
    Result := -1
  else
    if TLexCharsets(Item1).SetName > TLexCharsets(Item2).SetName then Result := 1;
end;

procedure TFrmMain.MakeSensitiveHashTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then mSKeyHashTable[I] := Ord(I) - 64 else
            if (I > #96) then mSKeyHashTable[I] := Ord(I) - 95;
        end;
    else mSKeyHashTable[I] := 0;
    end;
  end;
end;

procedure TFrmMain.MakeHashTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    J := UpperCase(I)[1];
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mKeyHashTable[I] := Ord(J) - 64;
    else mKeyHashTable[I] := 0;
    end;
  end;
end;

function TFrmMain.SensKeyHash(ToHash: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(ToHash) do
    inc(Result, mSKeyHashTable[ToHash[I]]);
end;

function TFrmMain.KeyHash(ToHash: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(ToHash) do
    inc(Result, mKeyHashTable[ToHash[I]]);
end;

procedure TFrmMain.WriteSettings;
begin
  with TIniFile.Create(IniFile) do
  try
    WriteString('General', 'OpenDir', OpenDialog.InitialDir);
    WriteBool(FFileName, 'GetKeyWords', ChkGetKeyWords.Checked);
    WriteString(FFileName, 'Author', EditAuthor.Text);
    WriteString(FFileName, 'Description', EditDescription.Text);
    WriteString(FFileName, 'Version', EditVersion.Text);
    WriteString(FFileName, 'Filter', CboFilter.Text);
    WriteString(FFileName, 'Language', CboLangName.Text);
    WriteString(FFileName, 'AttrIdentifier', CboAttrIdentifier.Text);
    WriteString(FFileName, 'AttrReservedWord', CboAttrReservedWord.Text);
    WriteString(FFileName, 'UnknownTokenAttr', CboUnknownTokenAttr.Text);
    WriteString(FFileName, 'Fields', ListBoxFields.Items.CommaText);
  finally
    Free;
  end;
end;

function TFrmMain.PerformFileOpen: Boolean;
var
  UserName: PChar;
{$IFDEF SYN_COMPILER_5}
  Count: Cardinal;
{$ELSE}
  Count: Integer;
{$ENDIF}
begin
  if OpenDialog.Execute then
  begin
    Count := 0;
    Result := True;
    FFileName := ExtractFileName(OpenDialog.FileName);
    Caption := 'SynEdit Highlighter Generator - ' + FFileName;
    OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
    GetUserName(nil, Count); // retrieve the required size of the user name buffer
    UserName := StrAlloc(Count); // allocate memory for the user name
    GetUserName(UserName, Count); // retrieve the user name
    with TIniFile.Create(IniFile) do
    try
      EditAuthor.Text := ReadString(FFileName, 'Author', StrPas(UserName));
      EditDescription.Text := ReadString(FFileName, 'Description', 'Syntax Parser/Highlighter');
      EditVersion.Text := ReadString(FFileName, 'Version', '0.1');
      CboFilter.Text := ReadString(FFileName, 'Filter', 'All files (*.*)|*.*');
      CboLangName.Text := ReadString(FFileName, 'Language', '');
      ChkGetKeyWords.Checked := ReadBool(FFileName, 'GetKeyWords', True);
      CboAttrIdentifier.ItemIndex := CboAttrIdentifier.Items.IndexOf
        (ReadString(FFileName, 'AttrIdentifier', 'SYNS_AttrIdentifier'));
      CboAttrReservedWord.ItemIndex := CboAttrReservedWord.Items.IndexOf
        (ReadString(FFileName, 'AttrReservedWord', 'SYNS_AttrReservedWord'));
      CboUnknownTokenAttr.ItemIndex := CboUnknownTokenAttr.Items.IndexOf
        (ReadString(FFileName, 'UnknownTokenAttr', 'Identifier'));
      ListBoxFields.Items.CommaText := ReadString(FFileName, 'Fields', '');
    finally
      Free;
    end;
    StrDispose(UserName);
    CboLangNameChange(Self);    
  end else Result := False;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var i: Integer;
begin
  for i := FrmMain.ComponentCount - 1 downto 0 do
    if FrmMain.Components[i] is TComboBox then
      if TComboBox(FrmMain.Components[i]).Parent = GrpAttrNames then
      begin
        TComboBox(FrmMain.Components[i]).Items.Clear;
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrAsm');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrAsmComment');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrAsmKey');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrASP');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrAssembler');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrBlock');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrBrackets');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrCharacter');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrClass');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrComment');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrCondition');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrDir');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrDirective');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrDocumentation');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrEmbedSQL');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrEmbedText');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrEscapeAmpersand');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrForm');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrFunction');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrIcon');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrIdentifier');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrIllegalChar');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrIndirect');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrInvalidSymbol');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrInternalFunction');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrKey');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrLabel');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrMacro');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrMarker');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrMessage');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrMiscellaneous');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrNull');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrNumber');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrOperator');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrPragma');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrPreprocessor');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrQualifier');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrRegister');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrReservedWord');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrRpl');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrRplKey');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrRplComment');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSASM');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSASMComment');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSASMKey');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSecondReservedWord');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSection');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSpace');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSpecialVariable');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrString');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSymbol');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSyntaxError');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSystem');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrSystemValue');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrText');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrUnknownWord');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrUser');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrUserFunction');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrValue');
        TComboBox(FrmMain.Components[i]).Items.Add('SYNS_AttrVariable');
      end;
  PageControl.ActivePage := PageControl.Pages[0];
  Stream := TMemoryStream.Create;
  Lex := TGenLex.Create;
  KeyList := TList.Create;
  SetList := TList.Create;
  IdentList := TStringList.Create;
  // read ini file
  IniFile := Copy(ExtractFileName(Application.ExeName), 0,
             Length(ExtractFileName(Application.ExeName)) -
             Length(ExtractFileExt(Application.ExeName))) + '.ini';
  with TIniFile.Create(IniFile) do
  try
    OpenDialog.InitialDir := ReadString('General', 'OpenDir',
                             ExtractFilePath(Application.ExeName));
  finally
    Free;
  end;
  if PerformFileOpen then
  begin
    MakeHashTable;
    MakeSensitiveHashTable;
  end else Halt;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
var
  I: Integer;
begin
  Lex.Free;
  Stream.Free;
  IdentList.Free;
  for I := 0 to KeyList.Count - 1 do TObject(KeyList[I]).Free;
  KeyList.Free;
  for I := 0 to SetList.Count - 1 do TObject(SetList[I]).Free;
  SetList.Free;
end;

procedure TFrmMain.BtnStartClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  Stream.Clear;
  Stream.LoadFromFile(OpenDialog.FileName);
  Lex.Origin := Stream.Memory;
  Lex.Tokenize;
  while Lex.RunId <> IDIdentifier do Lex.Next;
  LexName := Lex.RunToken;
  Lex.Next;
  while Lex.RunId <> IDIdentifier do Lex.Next;
  IdentPre := Lex.RunToken;
  OutFileCreate(OpenDialog.FileName);
  while not (Lex.RunId in [IdSensitive, IdIdentStart]) do Lex.Next;
  if Lex.RunId = IdSensitive then Sensitivity := True else Sensitivity := False;
  Lex.Next;
  while Lex.RunId <> IDCharSet do Lex.Next;
  IdentStart := Lex.RunToken;
  Lex.Next;
  while Lex.RunId <> IDCharSet do Lex.Next;
  IdentContent := Lex.RunToken;
  while Lex.RunId <> IDKeys do Lex.Next;
  FillKeyList;
  while Lex.RunId <> IDChars do Lex.Next;
  ParseCharsets;
  WriteRest;
  while Lex.RunId <> IdNull do
  begin
    Lex.Next;
  end;
  CloseFile(OutFile);
  Screen.Cursor := crDefault;
  MessageDlg(LexName + ' created on ' + DateTimeToStr(Now), mtInformation, [mbOk], 0);
end;

procedure TFrmMain.FillKeyList;
var
  aLexKey: TLexKeys;
  aString: String;
begin
  Lex.Next;
  while Lex.RunId <> IdStop do
  begin
    while Lex.RunId in [IdSpace, IdBraceOpen, IdCRLF] do Lex.Next;
    if Lex.RunId <> IdStop then
    begin
      aString:= '';
      while not (Lex.RunId in [IdSpace, IdBraceOpen, IdCRLF]) do
      begin
        aString:= aString + Lex.RunToken;
        Lex.Next;
      end;
      aLexKey := TLexKeys.Create;
      aLexKey.KeyName := aString;
      if Sensitivity then aLexKey.Key := SensKeyHash(aLexKey.KeyName) else
        aLexKey.Key := KeyHash(aLexKey.KeyName);
      KeyList.Add(aLexKey);
    end else break;
    Lex.Next;
  end;
  Lex.Next;
  while Lex.RunId <> IdStop do
  begin
    while Lex.RunId in [IdSpace, IdBraceOpen, IdCRLF, IDUnknown] do Lex.Next;
    if Lex.RunId <> IdStop then IdentList.Add(IdentPre + Lex.RunToken) else break;
    Lex.Next;
  end;
  KeyList.Sort(CompareKeys);
end;

procedure TFrmMain.OutFileCreate(InName: String);
var
  OutName, UName: String;
  sysTime: TSystemTime;
  ISODate: string;
begin
  OutName := ExtractFileName(InName);
  Delete(OutName, Length(OutName) - 3, 4);
  Uname := OutName;
  OutName := OutName + '.pas';
  AssignFile(OutFile, OutName);
  rewrite(OutFile);
  GetSystemTime(sysTime);
  ISODate := Format('%.4d-%.2d-%.2d', [sysTime.wYear, sysTime.wMonth, sysTime.wDay]);
  Writeln(OutFile, '{+-----------------------------------------------------------------------------+');
  Writeln(OutFile, ' | Class:       ' + LexName);
  Writeln(OutFile, ' | Created:     ' + ISODate);
  Writeln(OutFile, ' | Last change: ' + ISODate);
  Writeln(OutFile, ' | Author:      ' + EditAuthor.Text);
  Writeln(OutFile, ' | Description: ' + EditDescription.Text);
  Writeln(OutFile, ' | Version:     ' + EditVersion.Text);
  Writeln(OutFile, ' |');
  Writeln(OutFile, ' | Copyright (c) ' + Format('%d', [sysTime.wYear]) + #32 +
    EditAuthor.Text + '. All rights reserved.');
  Writeln(OutFile, ' |');
  Writeln(OutFile, ' | Generated with SynGen.');
  Writeln(OutFile, ' +----------------------------------------------------------------------------+}');
  Writeln(OutFile, '');
  Writeln(OutFile, 'unit ' + Uname + ';' + #13#10);
  Writeln(OutFile, '{$I SynEdit.inc}'#13#10);
  Writeln(OutFile, 'interface' + #13#10);
  Writeln(OutFile, 'uses');
  Writeln(OutFile, '  SysUtils, Windows, Messages, Classes, Controls, Graphics, Registry,');
  Writeln(OutFile, '  SynEditTypes, SynEditHighlighter;' + #13#10);
  Writeln(OutFile, 'Type');
  Writeln(OutFile, '  T' + IdentPre + 'TokenKind = (');
end;

procedure TFrmMain.ParseCharsets;
begin
  Lex.Next;
  while Lex.RunId <> IdStop do
  begin
    Case Lex.RunId of
      IdCharset: RetriveCharset;
    else Lex.Next;
    end;
  end;
end;

procedure TFrmMain.RetriveCharset;
var
  aSet: TLexCharsets;
begin
  aSet := TLexCharsets.Create;
  aSet.Charset := Lex.RunToken;
  while Lex.RunId <> IDIdentifier do Lex.Next;
  aSet.SetName := Lex.RunToken;
  while Lex.RunId <> IDBeginProc do Lex.Next;
  Lex.Next;
  while Lex.RunId in [IdCRLF, IdSpace]do Lex.Next;
  while not(Lex.RunId=IdEndProc)do
  begin
    aSet.ProcData:=aSet.ProcData+Lex.RunToken;
    Lex.Next;
  end;
  SetList.Add(aSet);
  Lex.Next;
end;

procedure TFrmMain.WriteRest;
var
  I, LineLength                                                  : Integer;
  KeyString, NameString, AttrName, FilterName, LangName, AttrTemp: string;
  TempStringList                                                 : TStringList;
begin
  IdentList.Sort;
  SetList.Sort(CompareSets);
  I := 0;
  while I < IdentList.Count - 1 do
  begin
    Writeln(OutFile, '    ' + IdentList[I] + ',');
    inc(I);
  end;
  Writeln(OutFile, '    ' + IdentList[I] + ');');
  Writeln(OutFile, '');
  Writeln(OutFile, '  TProcTableProc = procedure of object;');
  Writeln(OutFile, '');
  Writeln(OutFile, '  PIdentFuncTableFunc = ^TIdentFuncTableFunc;');
  Writeln(OutFile, '  TIdentFuncTableFunc = function: T' + IdentPre + 'TokenKind of object;');
  Writeln(OutFile, '');
  Writeln(OutFile, 'type');
  Writeln(OutFile, '  ' + LexName + ' = class(TSynCustomHighlighter)');
  Writeln(OutFile, '  private');
  Writeln(OutFile, '    fLine: PChar;');
  Writeln(OutFile, '    fLineNumber: Integer;');
  Writeln(OutFile, '    fProcTable: array[#0..#255] of TProcTableProc;');
  Writeln(OutFile, '    Run: LongInt;');

  if ListBoxFields.Items.Count > 0 then
    for i := 0 to ListBoxFields.Items.Count - 1 do
      Writeln(OutFile, '    ' + ListBoxFields.Items[i] + ';');

  Writeln(OutFile, '    fStringLen: Integer;');
  Writeln(OutFile, '    fToIdent: PChar;');
  Writeln(OutFile, '    fTokenPos: Integer;');
  Writeln(OutFile, '    fTokenID: TtkTokenKind;');
  KeyString := IntToStr(TLexKeys(KeyList[KeyList.Count - 1]).Key);
  Writeln(OutFile, '    fIdentFuncTable: array[0..' + KeyString + '] of TIdentFuncTableFunc;');

  I := 0;
  while I < IdentList.Count do
  begin
    if (IdentList[I] <> IdentPre + 'Null') and (IdentList[I] <> IdentPre + 'Unknown') then
      Writeln(OutFile, '    f' + Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I])) + 'Attri: TSynHighlighterAttributes;');
    inc(I);
  end;

  Writeln(OutFile, '    function KeyHash(ToHash: PChar): Integer;');
  Writeln(OutFile, '    function KeyComp(const aKey: string): Boolean;');

  I := 0;
  while I < KeyList.Count do
  begin
    if I = 0 then
      Writeln(OutFile, '    function Func' + IntToStr(TLexKeys(KeyList[I]).Key) + ': T' + IdentPre + 'TokenKind;') else
      if (TLexKeys(KeyList[I - 1]).Key <> TLexKeys(KeyList[I]).Key) then
        Writeln(OutFile, '    function Func' + IntToStr(TLexKeys(KeyList[I]).Key) + ': T' + IdentPre + 'TokenKind;');
    inc(I);
  end;

  I := 0;
  while I < SetList.Count do
  begin
    Writeln(OutFile, '    procedure ' + TLexCharsets(SetList[I]).SetName + 'Proc;');
    inc(I);
  end;

  Writeln(OutFile, '    procedure UnknownProc;');
  Writeln(OutFile, '    function AltFunc: T' + IdentPre + 'TokenKind;');
  Writeln(OutFile, '    procedure InitIdent;');
  Writeln(OutFile, '    function IdentKind(MayBe: PChar): T' + IdentPre + 'TokenKind;');
  Writeln(OutFile, '    procedure MakeMethodTables;');
  Writeln(OutFile, '  protected');
  Writeln(OutFile, '    function GetIdentChars: TSynIdentChars; override;');
  Writeln(OutFile, '  public');
  Writeln(OutFile, '    class function GetLanguageName: string; override;');
  Writeln(OutFile, '  public');
  Writeln(OutFile, '    constructor Create(AOwner: TComponent); override;');
  Writeln(OutFile, '    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;');
  Writeln(OutFile, '      override;');
  Writeln(OutFile, '    function GetEOL: Boolean; override;');
  if ChkGetKeyWords.Checked then
    Writeln(OutFile, '    function GetKeyWords: string;');
  Writeln(OutFile, '    function GetTokenID: TtkTokenKind;');
  Writeln(OutFile, '    procedure SetLine(NewValue: String; LineNumber: Integer); override;');
  Writeln(OutFile, '    function GetToken: String; override;');
  Writeln(OutFile, '    function GetTokenAttribute: TSynHighlighterAttributes; override;');
  Writeln(OutFile, '    function GetTokenKind: integer; override;');
  Writeln(OutFile, '    function GetTokenPos: Integer; override;');
  Writeln(OutFile, '    procedure Next; override;');
  Writeln(OutFile, '  published');

  I := 0;
  while I < IdentList.Count do
  begin
    if (IdentList[I] <> IdentPre + 'Null') and (IdentList[I] <> IdentPre + 'Unknown') then
      Writeln(OutFile, '    property ' + Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I]))
      + 'Attri: TSynHighlighterAttributes read f' + Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I])) +
      'Attri write f' + Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I]))+ 'Attri;');
    inc(I);
  end;

  Writeln(OutFile, '  end;');
  Writeln(OutFile, '');
  Writeln(OutFile, 'implementation');
  Writeln(OutFile, '');
  Writeln(OutFile, 'uses');
  Writeln(OutFile, '  SynEditStrConst;');
  Writeln(OutFile, '');
  Writeln(OutFile, 'var');
  Writeln(OutFile, '  Identifiers: array[#0..#255] of ByteBool;');
  Writeln(OutFile, '  mHashTable : array[#0..#255] of Integer;'#13#10);
  if Sensitivity then
  begin
    Writeln(OutFile, 'procedure MakeIdentTable;');
    Writeln(OutFile, 'var');
    Writeln(OutFile, '  I: Char;');
    Writeln(OutFile, 'begin');
    Writeln(OutFile, '  for I := #0 to #255 do');
    Writeln(OutFile, '  begin');
    Writeln(OutFile, '    Case I of');
    Writeln(OutFile, '      ' + IdentContent + ': Identifiers[I] := True;');
    Writeln(OutFile, '      else Identifiers[I] := False;');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '    Case I in [''_'', ''A''..''Z'', ''a''..''z''] of');
    Writeln(OutFile, '      True:');
    Writeln(OutFile, '        begin');
    Writeln(OutFile, '          if (I > #64) and (I < #91) then mHashTable[I] := Ord(I) - 64 else');
    Writeln(OutFile, '            if (I > #96) then mHashTable[I] := Ord(I) - 95;');
    Writeln(OutFile, '        end;');
    Writeln(OutFile, '      else mHashTable[I] := 0;');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '  end;');
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
  end else
  begin
    Writeln(OutFile, 'procedure MakeIdentTable;');
    Writeln(OutFile, 'var');
    Writeln(OutFile, '  I, J: Char;');
    Writeln(OutFile, 'begin');
    Writeln(OutFile, '  for I := #0 to #255 do');
    Writeln(OutFile, '  begin');
    Writeln(OutFile, '    Case I of');
    Writeln(OutFile, '      ' + IdentContent + ': Identifiers[I] := True;');
    Writeln(OutFile, '      else Identifiers[I] := False;');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '    J := UpCase(I);');
    Writeln(OutFile, '    Case I in [''_'', ''A''..''Z'', ''a''..''z''] of');
    Writeln(OutFile, '      True: mHashTable[I] := Ord(J) - 64');
    Writeln(OutFile, '      else mHashTable[I] := 0;');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '  end;');
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
  end;

  Writeln(OutFile, 'procedure ' + LexName + '.InitIdent;');
  Writeln(OutFile, 'var');
  Writeln(OutFile, '  I: Integer;');
  Writeln(OutFile, '  pF: PIdentFuncTableFunc;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  pF := PIdentFuncTableFunc(@fIdentFuncTable);');
  Writeln(OutFile, '  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin');
  Writeln(OutFile, '    pF^ := AltFunc;');
  Writeln(OutFile, '    Inc(pF);');
  Writeln(OutFile, '  end;');

  I := 0;
  while I < KeyList.Count do
  begin
    if I < KeyList.Count - 1 then
      while TLexKeys(KeyList[I]).Key = TLexKeys(KeyList[I + 1]).Key do
      begin
        inc(I);
        if I >= KeyList.Count - 1 then break;
      end;
    KeyString := IntToStr(TLexKeys(KeyList[I]).Key);
    Writeln(OutFile, '  fIdentFuncTable[' + KeyString + '] := Func' + KeyString + ';');
    inc(I);
  end;

  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.KeyHash(ToHash: PChar): Integer;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := 0;');
  Writeln(OutFile, '  while ToHash^ in [' + IdentContent + '] do');
  Writeln(OutFile, '  begin');
  Writeln(OutFile, '    inc(Result, mHashTable[ToHash^]);');
  Writeln(OutFile, '    inc(ToHash);');
  Writeln(OutFile, '  end;');
  Writeln(OutFile, '  fStringLen := ToHash - fToIdent;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  if Sensitivity then
  begin
    Writeln(OutFile, 'function ' + LexName + '.KeyComp(const aKey: String): Boolean;');
    Writeln(OutFile, 'var');
    Writeln(OutFile, '  I: Integer;');
    Writeln(OutFile, '  Temp: PChar;');
    Writeln(OutFile, 'begin');
    Writeln(OutFile, '  Temp := fToIdent;');
    Writeln(OutFile, '  if Length(aKey) = fStringLen then');
    Writeln(OutFile, '  begin');
    Writeln(OutFile, '    Result := True;');
    Writeln(OutFile, '    for i := 1 to fStringLen do');
    Writeln(OutFile, '    begin');
    Writeln(OutFile, '      if Temp^ <> aKey[i] then');
    Writeln(OutFile, '      begin');
    Writeln(OutFile, '        Result := False;');
    Writeln(OutFile, '        break;');
    Writeln(OutFile, '      end;');
    Writeln(OutFile, '      inc(Temp);');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '  end else Result := False;');
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
  end else
  begin
    Writeln(OutFile, 'function ' + LexName + '.KeyComp(const aKey: String): Boolean;');
    Writeln(OutFile, 'var');
    Writeln(OutFile, '  I: Integer;');
    Writeln(OutFile, '  Temp: PChar;');
    Writeln(OutFile, 'begin');
    Writeln(OutFile, '  Temp := fToIdent;');
    Writeln(OutFile, '  if Length(aKey) = fStringLen then');
    Writeln(OutFile, '  begin');
    Writeln(OutFile, '    Result := True;');
    Writeln(OutFile, '    for i := 1 to fStringLen do');
    Writeln(OutFile, '    begin');
    Writeln(OutFile, '      if mHashTable[Temp^] <> mHashTable[aKey[i]] then');
    Writeln(OutFile, '      begin');
    Writeln(OutFile, '        Result := False;');
    Writeln(OutFile, '        break;');
    Writeln(OutFile, '      end;');
    Writeln(OutFile, '      inc(Temp);');
    Writeln(OutFile, '    end;');
    Writeln(OutFile, '  end else Result := False;');
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
  end;

  I := 0;
  while I < KeyList.Count do
  begin
    KeyString := IntToStr(TLexKeys(KeyList[I]).Key);
    Writeln(OutFile, 'function ' + LexName + '.Func' + KeyString + ': T' + IdentPre + 'TokenKind;');
    Writeln(OutFile, 'begin');
    KeyString := '';
    if I < KeyList.Count - 1 then
      while TLexKeys(KeyList[I]).Key = TLexKeys(KeyList[I + 1]).Key do
      begin
        NameString := TLexKeys(KeyList[I]).KeyName;
        Writeln(OutFile, KeyString + '  if KeyComp(' + #39 + NameString + #39 + ') then Result := ' + IdentPre + 'Key' + ' else');
        inc(I);
        KeyString := KeyString + '  ';
        if I >= KeyList.Count - 1 then break;
      end;
    NameString := TLexKeys(KeyList[I]).KeyName;
    Writeln(OutFile, KeyString + '  if KeyComp(' + #39 + NameString + #39 + ') then Result := ' + IdentPre + 'Key' + ' else Result := ' + IdentPre + 'Identifier;');
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
    inc(I);
  end;

  Writeln(OutFile, 'function ' + LexName + '.AltFunc: T' + IdentPre + 'TokenKind;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := ' + IdentPre + 'Identifier;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  KeyString := IntToStr(TLexKeys(KeyList[KeyList.Count - 1]).Key + 1);

  Writeln(OutFile, 'function ' + LexName + '.IdentKind(MayBe: PChar): T' + IdentPre + 'TokenKind;');
  Writeln(OutFile, 'var');
  Writeln(OutFile, '  HashKey: Integer;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  fToIdent := MayBe;');
  Writeln(OutFile, '  HashKey := KeyHash(MayBe);');
  Writeln(OutFile, '  if HashKey < ' + KeyString + ' then Result := fIdentFuncTable[HashKey] else Result := ' + IdentPre + 'Identifier;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'procedure ' + LexName + '.MakeMethodTables;');
  Writeln(OutFile, 'var');
  Writeln(OutFile, '  I: Char;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  for I := #0 to #255 do');
  Writeln(OutFile, '    case I of');

  I := 0;
  while I < SetList.Count do
  begin
    Writeln(OutFile, '      ' + TLexCharsets(SetList[I]).Charset + ': fProcTable[I] := '+
     TLexCharsets(SetList[I]).SetName+'Proc;');
    inc(I);
  end;

  Writeln(OutFile, '      else fProcTable[I] := UnknownProc;');
  Writeln(OutFile, '    end;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'constructor ' + LexName + '.Create(AOwner: TComponent);');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  inherited Create(AOwner);');
  Writeln(OutFile, '// YOU SHOULD ADD AT LEAST ONE OR TWO DIFFERENT DEFAULT ATTRIBUTES');
  I := 0;
  while I < IdentList.Count do
  begin
    AttrTemp := Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I]));
    if AttrTemp = 'Key' then
      AttrName := CboAttrReservedWord.Text
    else if AttrTemp = 'Identifier' then
      AttrName := CboAttrIdentifier.Text
    else
      AttrName := 'SYNS_Attr' + AttrTemp;
    if (IdentList[I] <> IdentPre + 'Null') and
       (IdentList[I] <> IdentPre + 'Unknown')
    then begin
      AttrTemp := 'f' + AttrTemp + 'Attri';
      Writeln(OutFile, '  ' + AttrTemp + ' := TSynHighLighterAttributes.Create(' + AttrName + ');');
      Writeln(OutFile, '  AddAttribute(' + AttrTemp + ');');
    end;
    Inc(I);
  end;

  Writeln(OutFile, '  SetAttributesOnChange(DefHighlightChange);');
  Writeln(OutFile, '  InitIdent;');
  Writeln(OutFile, '  MakeMethodTables;');
  case CboFilter.ItemIndex of
    -1: FilterName := #39 + CboFilter.Text + #39;
    0 : FilterName := 'SYNS_FilterPascal';
    1 : FilterName := 'SYNS_FilterHP48';
    2 : FilterName := 'SYNS_FilterCAClipper';
    3 : FilterName := 'SYNS_FilterCPP';
    4 : FilterName := 'SYNS_FilterJava';
    5 : FilterName := 'SYNS_FilterPerl';
    6 : FilterName := 'SYNS_FilterAWK';
    7 : FilterName := 'SYNS_FilterHTML';
    8 : FilterName := 'SYNS_FilterVBScript';
    9 : FilterName := 'SYNS_FilterGalaxy';
    10: FilterName := 'SYNS_FilterPython';
    11: FilterName := 'SYNS_FilterSQL';
    12: FilterName := 'SYNS_FilterHP';
    13: FilterName := 'SYNS_FilterTclTk';
    14: FilterName := 'SYNS_FilterRTF';
    15: FilterName := 'SYNS_FilterBatch';
    16: FilterName := 'SYNS_FilterDFM';
    17: FilterName := 'SYNS_FilterX86Asm';
    18: FilterName := 'SYNS_FilterGembase';
    19: FilterName := 'SYNS_FilterINI';
    20: FilterName := 'SYNS_FilterML';
    21: FilterName := 'SYNS_FilterVisualBASIC';
    22: FilterName := 'SYNS_FilterADSP21xx';
    23: FilterName := 'SYNS_FilterPHP';
    24: FilterName := 'SYNS_FilterCache';
    25: FilterName := 'SYNS_FilterCSS';
    26: FilterName := 'SYNS_FilterJScript';
    27: FilterName := 'SYNS_FilterKIX';
    28: FilterName := 'SYNS_FilterBaan';
    29: FilterName := 'SYNS_FilterFoxpro';
    30: FilterName := 'SYNS_FilterFortran';
    31: FilterName := 'SYNS_FilterAsm68HC11';
  end;
  Writeln(OutFile, '  fDefaultFilter := ' + FilterName + ';');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'procedure ' + LexName + '.SetLine(NewValue: String; LineNumber: Integer);');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  fLine := PChar(NewValue);');
  Writeln(OutFile, '  Run := 0;');
  Writeln(OutFile, '  fLineNumber := LineNumber;');
  Writeln(OutFile, '  Next;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  I := 0;
  while I < SetList.Count do
  begin
    Writeln(OutFile, 'procedure '+LexName+'.'+TLexCharsets(SetList[I]).SetName+'Proc;');
    Writeln(OutFile, 'begin');
    Write(OutFile, '  '+TLexCharsets(SetList[I]).ProcData);
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
    inc(I);
  end;

  Writeln(OutFile, 'procedure ' + LexName + '.UnknownProc;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  inc(Run);');
  Writeln(OutFile, '  fTokenID := ' + IdentPre + 'Unknown;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'procedure ' + LexName + '.Next;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  fTokenPos := Run;');
  Writeln(OutFile, '  fProcTable[fLine[Run]];');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  case Index of');
  Writeln(OutFile, '    SYN_ATTR_COMMENT   : Result := fCommentAttri;');
  Writeln(OutFile, '    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;');
  Writeln(OutFile, '    SYN_ATTR_KEYWORD   : Result := fKeyAttri;');
  Writeln(OutFile, '    SYN_ATTR_STRING    : Result := fStringAttri;');
  Writeln(OutFile, '    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;');
  Writeln(OutFile, '    else Result := nil;');
  Writeln(OutFile, '  end;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetEOL: Boolean;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := fTokenID = tkNull;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  if ChkGetKeyWords.Checked then
  begin
    Writeln(OutFile, 'function ' + LexName + '.GetKeyWords: string;');
    Writeln(OutFile, 'begin');
    TempStringList := TStringList.Create;
    try
      TempStringList.Sorted := True;
      for I := 0 to KeyList.Count - 1 do
        TempStringList.Add(TLexKeys(KeyList[I]).KeyName);
      if TempStringList.Count > 0 then
      begin
        Writeln(OutFile, '  Result := ');
        for I := 0 to Trunc(Int(Length(TempStringList.CommaText) div 70)) - 1 do
        begin
          if I = 0 then LineLength := 69 else LineLength := 70;
          Writeln(OutFile, '    ' + #39 + Copy(TempStringList.CommaText,
            I * 70, LineLength) + #39 + #32 + #43);
        end;
        I := Trunc(Int(Length(TempStringList.CommaText) div 70));
        Writeln(OutFile, '    ' + #39 + Copy(TempStringList.CommaText,
           I * 70, Length(TempStringList.CommaText)) + #39 + ';')
      end else
        Writeln(OutFile, '  Result := ' + #39 + #39 + ';');
    finally
      TempStringList.Free;
    end;
    Writeln(OutFile, 'end;');
    Writeln(OutFile, '');
  end;

  Writeln(OutFile, 'function ' + LexName + '.GetToken: String;');
  Writeln(OutFile, 'var');
  Writeln(OutFile, '  Len: LongInt;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Len := Run - fTokenPos;');
  Writeln(OutFile, '  SetString(Result, (FLine + fTokenPos), Len);');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetTokenID: TtkTokenKind;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := fTokenId;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetTokenAttribute: TSynHighLighterAttributes;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  case GetTokenID of');

  I := 0;
  while I < IdentList.Count do
  begin
    if (IdentList[I] <> IdentPre + 'Null') and (IdentList[I] <> IdentPre + 'Unknown') then
      Writeln(OutFile, '    ' + IdentList[I] + ': Result := f' +
      Copy(IdentList[I], Length(IdentPre) + 1, Length(IdentList[I])) + 'Attri;');
    inc(I);
  end;
  Writeln(OutFile, '    ' + IdentPre + 'Unknown: Result := f' + CboUnknownTokenAttr.Text + 'Attri;');

  Writeln(OutFile, '    else Result := nil;');
  Writeln(OutFile, '  end;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetTokenKind: integer;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := Ord(fTokenId);');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetTokenPos: Integer;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := fTokenPos;');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'function ' + LexName + '.GetIdentChars: TSynIdentChars;');
  Writeln(OutFile, 'begin');
  Writeln(OutFile, '  Result := [' + IdentContent + '];');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'class function ' + LexName + '.GetLanguageName: string;');
  Writeln(OutFile, 'begin');
  case CboLangName.ItemIndex of
    -1: LangName := #39 + CboLangName.Text + #39;
    0 : LangName := 'SYNS_LangHP48';
    1 : LangName := 'SYNS_LangCAClipper';
    2 : LangName := 'SYNS_LangCPP';
    3 : LangName := 'SYNS_LangJava';
    4 : LangName := 'SYNS_LangPerl';
    5 : LangName := 'SYNS_LangBatch';
    6 : LangName := 'SYNS_LangDfm';
    7 : LangName := 'SYNS_LangAWK';
    8 : LangName := 'SYNS_LangHTML';
    9 : LangName := 'SYNS_LangVBSScript';
    10 : LangName := 'SYNS_LangGalaxy';
    11 : LangName := 'SYNS_LangGeneral';
    12 : LangName := 'SYNS_LangPascal';
    13 : LangName := 'SYNS_LangX86Asm';
    14 : LangName := 'SYNS_LangPython';
    15 : LangName := 'SYNS_LangTclTk';
    16 : LangName := 'SYNS_LangSQL';
    17 : LangName := 'SYNS_LangGembase';
    18 : LangName := 'SYNS_LangINI';
    19 : LangName := 'SYNS_LangML';
    20 : LangName := 'SYNS_LangVisualBASIC';
    21 : LangName := 'SYNS_LangADSP21xx';
    22 : LangName := 'SYNS_LangPHP';
    23 : LangName := 'SYNS_LangSybaseSQL';
    24 : LangName := 'SYNS_LangGeneralMulti';
    25 : LangName := 'SYNS_LangCache';
    26 : LangName := 'SYNS_LangCSS';
    27 : LangName := 'SYNS_LangJScript';
    28 : LangName := 'SYNS_LangKIX';
    29 : LangName := 'SYNS_LangBaan';
    30 : LangName := 'SYNS_LangFoxpro';
    31 : LangName := 'SYNS_LangFortran';
    32 : LangName := 'SYNS_Lang68HC11';
  end;
  Writeln(OutFile, '  Result := ' + LangName + ';');
  Writeln(OutFile, 'end;');
  Writeln(OutFile, '');

  Writeln(OutFile, 'initialization');
  Writeln(OutFile, '  MakeIdentTable;');
  Writeln(OutFile, '  RegisterPlaceableHighlighter(' + LexName + ');');
  Writeln(OutFile, 'end.');
end;

procedure TFrmMain.CboLangNameChange(Sender: TObject);
begin
  if (CboLangName.Text <> '') and (CboFilter.Text <> '') then
    BtnStart.Enabled := True
  else
    BtnStart.Enabled := False;
end;

procedure TFrmMain.ListBoxFieldsClick(Sender: TObject);
begin
  BtnDelete.Enabled := True;
end;

procedure TFrmMain.BtnAddClick(Sender: TObject);
begin
  ListBoxFields.Items.Add(EditAddField.Text);
  EditAddField.Clear;
end;

procedure TFrmMain.BtnDeleteClick(Sender: TObject);
begin
  BtnDelete.Enabled := False;
  ListBoxFields.Items.Delete(ListBoxFields.ItemIndex);
end;

procedure TFrmMain.EditAddFieldChange(Sender: TObject);
begin
  BtnAdd.Enabled := EditAddField.Text <> '';
end;

procedure TFrmMain.EditAddFieldKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = ';') or (Key = #32) then Key := #0;
end;

procedure TFrmMain.MnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.MnuOpenClick(Sender: TObject);
begin
  WriteSettings;
  PerformFileOpen;
end;

procedure TFrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteSettings;
end;

end.
