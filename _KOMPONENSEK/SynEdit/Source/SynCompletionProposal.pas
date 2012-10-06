{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynCompletionProposal.pas, released 2000-04-11.
The Original Code is based on mwCompletionProposal.pas by Cyrille de Brebisson,
part of the mwEdit component suite.
Portions created by Cyrille de Brebisson are Copyright (C) 1999
Cyrille de Brebisson. All Rights Reserved.

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

$Id: SynCompletionProposal.pas,v 1.8 2001/08/07 13:20:05 jjans Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynCompletionProposal;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
{$IFDEF SYN_KYLIX}
  Qt, Types, QControls, QGraphics, QForms, QStdCtrls, QMenus,
{$ELSE}
  Windows, Messages, Graphics, Forms, Controls, StdCtrls, Menus,
{$ENDIF}
 SynEditTypes, SynEditKeyCmds, SynEditHighlighter, SynEditKbdHandler, SynEdit;

type
  TSynBaseCompletionProposalPaintItem = function(AKey: string; ACanvas: TCanvas;
    X, Y: integer): boolean of object;
  TCodeCompletionEvent = procedure(var Value: string; Shift: TShiftState)
    of object;
  TValidateEvent = procedure(Sender: TObject; Shift: TShiftState) of object;

  TSynBaseCompletionProposalForm = class(TForm)
  protected
    FCurrentString: string;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDelete: TNotifyEvent;
    FOnPaintItem: TSynBaseCompletionProposalPaintItem;
    FItemList: TStrings;
    fAssignedList: TStrings;
    FPosition: Integer;
    FNbLinesInWindow: Integer;
    FFontHeight: integer;
    Scroll: TScrollBar;
    FOnValidate: TValidateEvent;
    FOnCancel: TNotifyEvent;
    FClSelect: TColor;
    fClText: TColor;
    fClSelectText: TColor;
    fClBackGround: TColor;
    FAnsi: boolean;
    fCase: boolean;
    fShrink: Boolean;
    procedure SetCurrentString(const Value: string);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure Paint; override;
    procedure ScrollGetFocus(Sender: TObject);
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MoveLine (cnt: Integer);
    procedure ScrollChange(Sender: TObject);
    procedure SetItemList(const Value: TStrings);
    procedure SetPosition(const Value: Integer);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure StringListChange(Sender: TObject);
    function intLowerCase (s: string): string;
  private
    Bitmap: TBitmap; // used for drawing
    fCurrentEditor: TComponent;
    procedure SetShrink(const Value: Boolean);
  public
    constructor Create(AOwner: Tcomponent); override;
    destructor destroy; override;
  published
    property CurrentString: string read FCurrentString write SetCurrentString;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyDelete: TNotifyEvent read FOnKeyDelete write FOnKeyDelete;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem read FOnPaintItem
      write FOnPaintItem;
    property OnValidate: TValidateEvent read FOnValidate write FOnValidate;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;
    property ItemList: TStrings read FItemList write SetItemList;
    property Position: Integer read FPosition write SetPosition;
    property NbLinesInWindow: Integer read FNbLinesInWindow
      write SetNbLinesInWindow;
    property ClSelect: TColor read FClSelect write FClSelect;
    property ClText: TColor read fClText write fClText;
    property ClSelectedText: TColor read fClSelectText write fClSelectText;
    property ClBackground: TColor read fClBackGround write fClBackGround;
    property AnsiStrings: boolean read fansi write fansi;
    property CaseSensitive: Boolean read fCase write fCase;
    property ShrinkList: Boolean read fShrink write SetShrink;
    property CurrentEditor: tComponent read fCurrentEditor write fCurrentEditor;
  end;

  TSynBaseCompletionProposal = class(TComponent)
  private
    Form: TSynBaseCompletionProposalForm;
    FOnExecute: TNotifyEvent;
    FWidth: Integer;
    function GetClSelect: TColor;
    procedure SetClSelect(const Value: TColor);
    function GetCurrentString: string;
    function GetItemList: TStrings;
    function GetNbLinesInWindow: Integer;
    function GetOnCancel: TNotifyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
    function GetOnValidate: TValidateEvent;
    function GetPosition: Integer;
    procedure SetCurrentString(const Value: string);
    procedure SetItemList(const Value: TStrings);
    procedure SetNbLinesInWindow(const Value: Integer);
    procedure SetOnCancel(const Value: TNotifyEvent);
    procedure SetOnKeyPress(const Value: TKeyPressEvent);
    procedure SetOnPaintItem(const Value: TSynBaseCompletionProposalPaintItem);
    procedure SetPosition(const Value: Integer);
    procedure SetOnValidate(const Value: TValidateEvent);
    function GetOnKeyDelete: TNotifyEvent;
    procedure SetOnKeyDelete(const Value: TNotifyEvent);
    procedure RFAnsi(const Value: boolean);
    function SFAnsi: boolean;
    procedure SetWidth(Value: Integer);
    function GetCase: boolean;
    procedure SetCase(const Value: boolean);
    function GetClBack: TColor;
    procedure SetClBack(const Value: TColor);
    function GetClText: TColor;
    procedure SetClText(const Value: TColor);
    function GetClSelectedText: TColor;
    procedure SetClSelectedText(const Value: TColor);
    function GetShrink: Boolean;
    procedure SetShrink(const Value: Boolean);
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(s: string; x, y: integer);
    procedure Activate;
    procedure Deactivate;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyDelete: TNotifyEvent read GetOnKeyDelete write SetOnKeyDelete;
    property OnValidate: TValidateEvent read GetOnValidate write SetOnValidate;
    property OnCancel: TNotifyEvent read GetOnCancel write SetOnCancel;
    property CurrentString: string read GetCurrentString write SetCurrentString;
  published
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnPaintItem: TSynBaseCompletionProposalPaintItem
      read GetOnPaintItem write SetOnPaintItem;
    property ItemList: TStrings read GetItemList write SetItemList;
    property Position: Integer read GetPosition write SetPosition;
    property NbLinesInWindow: Integer read GetNbLinesInWindow
      write SetNbLinesInWindow;
    property ClSelect: TColor read GetClSelect write SetClSelect;
    property ClText: TColor read GetClText write SetClText;
    property ClSelectedText: TColor read GetClSelectedText write SetClSelectedText;
    property ClBackground: TColor read GetClBack write SetClBack;
    property AnsiStrings: boolean read SFAnsi write RFAnsi;
    property CaseSensitive: boolean read GetCase write SetCase;
    property ShrinkList: Boolean read GetShrink write SetShrink;
    property Width: Integer read FWidth write SetWidth;
  end;

  TSynCompletionProposal = class(TSynBaseCompletionProposal)
  private
    FShortCut: TShortCut;
    fEditor: TCustomSynEdit;
    fKeyDownProc: TKeyDownProc;
    fKeyPressProc: TKeyPressProc;
    FEndOfTokenChr: string;
    fNoNextKey: Boolean;
    FOnCodeCompletion: TCodeCompletionEvent;
    procedure SetEditor(const Value: TCustomSynEdit);
    procedure backspace(Senter: TObject);
    procedure Cancel(Senter: TObject);
    procedure Validate(Senter: TObject; Shift: TShiftState);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyPress(Sender: TObject; var Key: char);
    function GetPreviousToken(FEditor: TCustomSynEdit): string;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure SetShortCut(Value: TShortCut);
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    function RemoveEditor(Editor: TCustomSynEdit): boolean;
  published
    property ShortCut: TShortCut read FShortCut write SetShortCut;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property OnCodeCompletion: TCodeCompletionEvent
      read FOnCodeCompletion write FOnCodeCompletion;
  end;

  TSynAutoComplete = class(TComponent)
  private
    FShortCut: TShortCut;
    fEditor: TCustomSynEdit;
    fAutoCompleteList: TStrings;
    fKeyDownProc : TKeyDownProc;
    fKeyPressProc : TKeyPressProc;
    fNoNextKey : Boolean;
    FEndOfTokenChr: string;
    procedure SetAutoCompleteList(List: TStrings);
    procedure SetEditor(const Value: TCustomSynEdit);
  protected
    procedure SetShortCut(Value: TShortCut);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      virtual;
    procedure EditorKeyPress(Sender: TObject; var Key: char); virtual;
    function GetPreviousToken(Editor: TCustomSynEdit): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure Execute(token: string; Editor: TCustomSynEdit);
    function RemoveEditor(Editor: TCustomSynEdit): boolean;
    function GetTokenList: string;
    function GetTokenValue(Token: string): string;
  published
    property AutoCompleteList: TStrings read fAutoCompleteList
      write SetAutoCompleteList;
    property EndOfTokenChr: string read FEndOfTokenChr write FEndOfTokenChr;
    property Editor: TCustomSynEdit read fEditor write SetEditor;
    property ShortCut: TShortCut read FShortCut write SetShortCut;
  end;

implementation

uses
  SynEditKeyConst, SynEditStrConst;

{ TSynBaseCompletionProposalForm }

constructor TSynBaseCompletionProposalForm.Create(AOwner: TComponent);
begin
{$IFDEF SYN_CPPB_1}
  CreateNew(AOwner, 0);
{$ELSE}
  CreateNew(AOwner);
{$ENDIF}
  FItemList := TStringList.Create;
  fAssignedList := TStringList.Create;

  {$IFDEF SYN_KYLIX}
  BorderStyle := fbsNone;
  {$ELSE}
  BorderStyle := bsNone;
  {$ENDIF}
  Scroll := TScrollBar.Create(self);
  Scroll.Kind := sbVertical;
  {$IFNDEF SYN_KYLIX}
  Scroll.ParentCtl3D := False;
  {$ENDIF}
  Scroll.OnChange := ScrollChange;
  Scroll.Parent := self;
  Scroll.OnEnter := ScrollGetFocus;
  Visible := false;
  FFontHeight := Canvas.TextHeight('Cyrille de Brebisson');
  ClSelect := clHighlight;
  ClSelectedText := clHighlightText;
  ClBackground := clWindow;
  ClText := clBlack;
  CaseSensitive := false;
  ShrinkList := true;
  TStringList(FItemList).OnChange := StringListChange;
  bitmap := TBitmap.Create;
  NbLinesInWindow := 6;
end;

procedure TSynBaseCompletionProposalForm.Activate;
begin
  Visible := True;
  TCustomSynEdit (CurrentEditor).AddFocusControl(self);
end;

procedure TSynBaseCompletionProposalForm.Deactivate;
begin
  TCustomSynEdit (CurrentEditor).RemoveFocusControl(self);
  Visible := False;
end;

destructor TSynBaseCompletionProposalForm.destroy;
begin
  bitmap.free;
  Scroll.Free;
  FItemList.Free;
  fAssignedList.Free;
  inherited destroy;
end;

procedure TSynBaseCompletionProposalForm.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    SYNEDIT_RETURN :
      if Assigned(OnValidate) then
        OnValidate(Self, Shift);
    SYNEDIT_ESCAPE :
      if Assigned(OnCancel) then
        OnCancel(Self);
    SYNEDIT_PRIOR :
      MoveLine (NbLinesInWindow * -1);
    SYNEDIT_NEXT :
      MoveLine (NbLinesInWindow);
    SYNEDIT_END :
      Position := ItemList.count - 1;
    SYNEDIT_HOME :
      Position := 0;
    SYNEDIT_UP :
      if ssCtrl in Shift then Position := 0
        else MoveLine (-1);
    SYNEDIT_DOWN :
      if ssCtrl in Shift then Position := ItemList.count - 1
        else MoveLine (1);
    SYNEDIT_BACK :
      if (Shift = []) and (Length(CurrentString) > 0) then begin
        CurrentString := Copy(CurrentString, 1, Length(CurrentString) - 1);
        if Assigned (OnKeyDelete) then
          OnKeyDelete(Self);
      end;
  end;
  Paint;
end;

procedure TSynBaseCompletionProposalForm.KeyPress(var Key: char);
begin
  case key of //
    #32..'z': begin
        CurrentString := CurrentString + key;
        if Assigned(OnKeyPress) then
          OnKeyPress(self, Key);
      end;
    #8: begin
        if Assigned(OnKeyPress) then
          OnKeyPress(self, Key);
      end;
  else if Assigned(OnCancel) then
    OnCancel(Self);
  end; // case
  paint;
end;

procedure TSynBaseCompletionProposalForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  y := (y - 1) div FFontHeight;
  Position := Scroll.Position + y;
end;

procedure TSynBaseCompletionProposalForm.Paint;
var
  i: integer;
function Min(a, b: integer): integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;
begin
  // update scrool bar
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Position := Position;
  Scroll.LargeChange := NbLinesInWindow;

  // draw a rectangle around the window
  Canvas.Pen.Color := ClBlack;
  Canvas.Moveto(0, 0);
  Canvas.LineTo(Width - 1, 0);
  Canvas.LineTo(Width - 1, Height - 1);
  Canvas.LineTo(0, Height - 1);
  Canvas.LineTo(0, 0);

  with bitmap do begin
    canvas.pen.color := fClBackGround;
    canvas.brush.color := fClBackGround;
    canvas.Rectangle(0, 0, Width, Height);
    for i := 0 to min(NbLinesInWindow - 1, ItemList.Count - 1) do begin
      if i + Scroll.Position = Position then begin
        Canvas.Brush.Color := fClSelect;
        Canvas.Pen.Color := fClSelect;
        Canvas.Rectangle(0, FFontHeight * i, width, FFontHeight * (i + 1));
        Canvas.Font.Color := fClSelectText;
      end else begin
        Canvas.Brush.Color := fClBackGround;
        Canvas.Pen.Color := fClBackGround;
        Canvas.Font.Color := fClText;
      end;

      if not Assigned(OnPaintItem)
        or not OnPaintItem(ItemList[Scroll.Position + i], Canvas, 0, FFontHeight * i)
      then
        Canvas.TextOut(2, FFontHeight * i, ItemList[Scroll.Position + i]);
    end;
  end;
  canvas.Draw(1, 1, bitmap);
end;

procedure TSynBaseCompletionProposalForm.ScrollChange(Sender: TObject);
begin
  if Position < Scroll.Position then
    Position := Scroll.Position
  else if Position > Scroll.Position + NbLinesInWindow - 1 then
    Position := Scroll.Position + NbLinesInWindow - 1;
  Paint;
end;

procedure TSynBaseCompletionProposalForm.ScrollGetFocus(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TSynBaseCompletionProposalForm.MoveLine (cnt: Integer);
begin
  if (cnt > 0) then begin
    if (Position < (ItemList.Count - cnt)) then
      Position := Position + cnt
    else
      Position := ItemList.Count - 1;
  end else begin
    if (Position + cnt) > 0 then
      Position := Position + cnt
    else
      Position := 0;
  end;
end;

procedure TSynBaseCompletionProposalForm.SetCurrentString(const Value: string);
var
  i: integer;
  cs: string;
function MatchItem (item: string): Boolean;
  var
    ci: string;
  begin
    ci := intLowerCase (Copy (item,1,Length (Value)));
    if fAnsi then
      Result := (AnsiCompareText (ci,cs) = 0)
    else
      Result := (CompareText (ci,cs) = 0);
  end;
procedure RecalcList;
  var
    idx: Integer;
  begin
    with fAssignedList do
      if (FItemList.Count > Count) then
        Assign (FItemList);
    ItemList.Clear;
    for idx := 0 to fAssignedList.Count - 1 do begin
      if MatchItem (fAssignedList[idx]) then
        ItemList.Add(fAssignedList[idx]);
    end;
  end;
begin
  FCurrentString := Value;
  i:= 0;
  cs := intLowerCase (Value);
  if fShrink then
    RecalcList;
  while (i <= ItemList.count-1) and not MatchItem (ItemList[i]) do
    Inc (i);
  if (i <= ItemList.Count-1) then Position:= i
    else Position := 0;
end;

procedure TSynBaseCompletionProposalForm.SetItemList(const Value: TStrings);
begin
  FItemList.Assign(Value);
  fAssignedList.Assign(Value);
  CurrentString := CurrentString;
end;

procedure TSynBaseCompletionProposalForm.SetNbLinesInWindow(
  const Value: Integer);
begin
  FNbLinesInWindow := Value;
  Height := fFontHeight * NbLinesInWindow + 2;
  Scroll.Top := 1;
  Scroll.Left := ClientWidth - Scroll.Width - 1;
  Scroll.Height := Height - 2;
  Bitmap.Width := Scroll.Left;
  Bitmap.Height := Height - 2;
end;

procedure TSynBaseCompletionProposalForm.SetPosition(const Value: Integer);
begin
  if Value <= ItemList.Count - 1 then begin
    if FPosition <> Value then begin
      FPosition := Value;
      if Position < Scroll.Position then
        Scroll.Position := Position
      else if Scroll.Position < Position - NbLinesInWindow + 1 then
        Scroll.Position := Position - NbLinesInWindow + 1;
      invalidate;
    end;
  end;
end;

procedure TSynBaseCompletionProposalForm.StringListChange(Sender: TObject);
begin
  if ItemList.Count - NbLinesInWindow < 0 then
    Scroll.Max := 0
  else
    Scroll.Max := ItemList.Count - NbLinesInWindow;
  Position := Position;
end;

function TSynBaseCompletionProposalForm.intLowerCase (s: string): string;
begin
  if fCase then Result := s
    else Result := LowerCase (s);
end;

procedure TSynBaseCompletionProposalForm.SetShrink(const Value: Boolean);
begin
  fShrink := Value;
  with FItemList do
    if (Count < fAssignedList.Count) then
      Assign(fAssignedList);
end;

{ TSynBaseCompletionProposal }

constructor TSynBaseCompletionProposal.Create(Aowner: TComponent);
begin
  FWidth := 262;
  inherited Create(AOwner);
  Form := TSynBaseCompletionProposalForm.Create(Self);
  Form.Width := FWidth;
end;

destructor TSynBaseCompletionProposal.Destroy;
begin
  form.Free;
  inherited Destroy;
end;

procedure TSynBaseCompletionProposal.Execute(s: string; x, y: integer);
begin
  form.top := y;
  form.left := x;
  CurrentString := s;
  if assigned(OnExecute) then
    OnExecute(Self);
  form.Show;
end;

function TSynBaseCompletionProposal.GetCurrentString: string;
begin
  result := Form.CurrentString;
end;

function TSynBaseCompletionProposal.GetItemList: TStrings;
begin
  result := Form.ItemList;
end;

function TSynBaseCompletionProposal.GetNbLinesInWindow: Integer;
begin
  Result := Form.NbLinesInWindow;
end;

function TSynBaseCompletionProposal.GetOnCancel: TNotifyEvent;
begin
  Result := Form.OnCancel;
end;

function TSynBaseCompletionProposal.GetOnKeyPress: TKeyPressEvent;
begin
  Result := Form.OnKeyPress;
end;

function TSynBaseCompletionProposal.GetOnPaintItem: TSynBaseCompletionProposalPaintItem;
begin
  Result := Form.OnPaintItem;
end;

function TSynBaseCompletionProposal.GetOnValidate: TValidateEvent;
begin
  Result := Form.OnValidate;
end;

function TSynBaseCompletionProposal.GetPosition: Integer;
begin
  Result := Form.Position;
end;

procedure TSynBaseCompletionProposal.SetCurrentString(const Value: string);
begin
  form.CurrentString := Value;
end;

procedure TSynBaseCompletionProposal.SetItemList(const Value: TStrings);
begin
  form.ItemList := Value;
end;

procedure TSynBaseCompletionProposal.SetNbLinesInWindow(const Value: Integer);
begin
  form.NbLinesInWindow := Value;
end;

procedure TSynBaseCompletionProposal.SetOnCancel(const Value: TNotifyEvent);
begin
  form.OnCancel := Value;
end;

procedure TSynBaseCompletionProposal.SetOnKeyPress(const Value: TKeyPressEvent);
begin
  form.OnKeyPress := Value;
end;

procedure TSynBaseCompletionProposal.SetOnPaintItem(const Value:
  TSynBaseCompletionProposalPaintItem);
begin
  form.OnPaintItem := Value;
end;

procedure TSynBaseCompletionProposal.SetPosition(const Value: Integer);
begin
  form.Position := Value;
end;

procedure TSynBaseCompletionProposal.SetOnValidate(const Value: TValidateEvent);
begin
  form.OnValidate := Value;
end;

function TSynBaseCompletionProposal.GetClSelect: TColor;
begin
  Result := Form.ClSelect;
end;

procedure TSynBaseCompletionProposal.SetClSelect(const Value: TColor);
begin
  Form.ClSelect := Value;
end;

function TSynBaseCompletionProposal.GetOnKeyDelete: TNotifyEvent;
begin
  result := Form.OnKeyDelete;
end;

procedure TSynBaseCompletionProposal.SetOnKeyDelete(const Value: TNotifyEvent);
begin
  form.OnKeyDelete := Value;
end;

procedure TSynBaseCompletionProposal.RFAnsi(const Value: boolean);
begin
  form.AnsiStrings := value;
end;

function TSynBaseCompletionProposal.SFAnsi: boolean;
begin
  result := form.AnsiStrings;
end;

function TSynBaseCompletionProposal.GetCase: boolean;
begin
  result := form.CaseSensitive;
end;

procedure TSynBaseCompletionProposal.SetCase(const Value: boolean);
begin
  form.CaseSensitive := Value;
end;

procedure TSynBaseCompletionProposal.SetWidth(Value: Integer);
begin
  FWidth := Value;
  Form.Width := FWidth;
  Form.SetNbLinesInWindow(Form.FNbLinesInWindow);
end;

procedure TSynBaseCompletionProposal.Activate;
begin
  if Assigned(Form) then Form.Activate;
end;

procedure TSynBaseCompletionProposal.Deactivate;
begin
  if Assigned(Form) then Form.Deactivate;
end;

function TSynBaseCompletionProposal.GetClBack: TColor;
begin
  Result := form.ClBackground;
end;

procedure TSynBaseCompletionProposal.SetClBack(const Value: TColor);
begin
  form.ClBackground := Value
end;

function TSynBaseCompletionProposal.GetClText: TColor;
begin
  Result := form.ClText;
end;

procedure TSynBaseCompletionProposal.SetClText(const Value: TColor);
begin
  form.ClText := Value;
end;

function TSynBaseCompletionProposal.GetClSelectedText: TColor;
begin
  Result := form.ClSelectedText;
end;

procedure TSynBaseCompletionProposal.SetClSelectedText(const Value: TColor);
begin
  form.ClSelectedText := Value;
end;

function TSynBaseCompletionProposal.GetShrink: Boolean;
begin
  Result := form.ShrinkList;
end;

procedure TSynBaseCompletionProposal.SetShrink(const Value: Boolean);
begin
  form.ShrinkList := Value;
end;

{ TSynCompletionProposal }

procedure TSynCompletionProposal.backspace(Senter: TObject);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Senter as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then begin
    (F.CurrentEditor as TCustomSynEdit).CommandProcessor(ecDeleteLastChar, #0,
      nil);
  end;
end;

procedure TSynCompletionProposal.Cancel(Senter: TObject);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Senter as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then begin
    if (F.CurrentEditor as TCustomSynEdit).Owner is TWinControl then
      TWinControl((F.CurrentEditor as TCustomSynEdit).Owner).SetFocus;
    (F.CurrentEditor as TCustomSynEdit).SetFocus;
  end;
end;

procedure TSynCompletionProposal.Validate(Senter: TObject; Shift: TShiftState);
var
  F: TSynBaseCompletionProposalForm;
  Value: string;
  Pos: TPoint;
begin
  F := Senter as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then
    with F.CurrentEditor as TCustomSynEdit do begin
      BlockBegin := Point(CaretX - length(CurrentString), CaretY);
      BlockEnd := Point(CaretX, CaretY);
      if Assigned(FOnCodeCompletion) then begin
        Value := ItemList[position];
        FOnCodeCompletion(Value, Shift);
        SelText := Value;
      end else
        SelText := ItemList[position];
      with Editor do begin
        Pos.x := CaretX;
        Pos.y := CaretY;
        {*****************}
        {$IFNDEF SYN_KYLIX}
        Perform(WM_MBUTTONDOWN, 0, 0);
        {$ENDIF}
        Application.ProcessMessages;
        CaretX := Pos.x;
        CaretY := Pos.y;
      end;
      SetFocus;
    end;
end;

procedure TSynCompletionProposal.KeyPress(Sender: TObject; var Key: Char);
var
  F: TSynBaseCompletionProposalForm;
begin
  F := Sender as TSynBaseCompletionProposalForm;
  if F.CurrentEditor <> nil then begin
    with F.CurrentEditor as TCustomSynEdit do
      CommandProcessor(ecChar, Key, nil);
  end;
end;

procedure TSynCompletionProposal.SetEditor(const Value: TCustomSynEdit);
begin
  if (fEditor <> nil) then begin
    RemoveEditor (fEditor);
    fEditor := nil;
  end;
  fEditor := Value;
  if (fEditor <> nil) then
    with fEditor do begin
      AddKeyDownHandler (fKeyDownProc);
      AddKeyPressHandler (fKeyPressProc);
    end;
end;

procedure TSynCompletionProposal.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TCustomSynEdit) then
    RemoveEditor(AComponent as TCustomSynEdit);
end;

constructor TSynCompletionProposal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Form.OnKeyPress := KeyPress;
  Form.OnKeyDelete := backspace;
  Form.OnValidate := validate;
  Form.OnCancel := Cancel;
  FEndOfTokenChr := '()[].';
  fKeyDownProc := TKeyDownProc.Create (EditorKeyDown);
  fKeyPressProc := TKeyPressProc.Create (EditorKeyPress);
  fEditor := nil;
  fNoNextKey := false;
  {$IFDEF SYN_KYLIX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssCtrl]);
  {$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssCtrl]);
  {$ENDIF}
end;

procedure TSynCompletionProposal.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

procedure TSynCompletionProposal.EditorKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  p             : TPoint;
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  with sender as TCustomSynEdit do begin
    if not ReadOnly and (Shift = ShortCutShift) and (Key = ShortCutKey) then begin
      p := ClientToScreen(Point(CaretXPix, CaretYPix+LineHeight));
      Form.CurrentEditor:= Sender as TCustomSynEdit;
      Execute (GetPreviousToken (Sender as TCustomSynEdit),p.x,p.y);
      fNoNextKey := true;
      Key := 0;
    end;
  end;
end;

function TSynCompletionProposal.GetPreviousToken(FEditor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if FEditor <> nil then begin
    s := FEditor.LineText;
    i := FEditor.CaretX - 1;
    if i <= length(s) then begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        dec(i);
      result := copy(s, i + 1, FEditor.CaretX - i - 1);
    end;
  end;
end;

procedure TSynCompletionProposal.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if fNoNextKey then begin
    fNoNextKey := false;
    Key := #0;
  end;
end;

destructor TSynCompletionProposal.destroy;
begin
  // necessary to get Notification called before fEditors is freed
  Form.Free;
  Form := nil;

  RemoveEditor(fEditor);
  fKeyDownProc.Free;
  fKeyPressProc.Free;

  inherited;
end;

function TSynCompletionProposal.RemoveEditor(Editor: TCustomSynEdit): boolean;
begin
  Result := Assigned (Editor);
  if Result then begin
    Editor.RemoveKeyDownHandler (fKeyDownProc);
    Editor.RemoveKeyPressHandler (fKeyPressProc);
  end;
end;

{ TSynAutoComplete }

constructor TSynAutoComplete.Create(AOwner: TComponent);
begin
  inherited;

  FEndOfTokenChr := '()[].';
  fAutoCompleteList := TStringList.Create;
  fKeyDownProc := TKeyDownProc.Create (EditorKeyDown);
  fKeyPressProc := TKeyPressProc.Create (EditorKeyPress);
  fNoNextKey := false;
  {$IFDEF SYN_KYLIX}
  fShortCut := QMenus.ShortCut(Ord(' '), [ssShift]);
  {$ELSE}
  fShortCut := Menus.ShortCut(Ord(' '), [ssShift]);
  {$ENDIF}
end;

procedure TSynAutoComplete.SetShortCut(Value: TShortCut);
begin
  FShortCut := Value;
end;

destructor TSynAutoComplete.destroy;
begin
  RemoveEditor (fEditor);
  fKeyDownProc.Free;
  fKeyPressProc.Free;
  fAutoCompleteList.free;
  inherited;
end;

procedure TSynAutoComplete.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  ShortCutKey   : Word;
  ShortCutShift : TShiftState;
begin
  ShortCutToKey (fShortCut,ShortCutKey,ShortCutShift);
  if not (Sender as TCustomSynEdit).ReadOnly and
    (Shift = ShortCutShift) and (Key = ShortCutKey) then begin
    Execute (GetPreviousToken (Sender as TCustomSynEdit),Sender as TCustomSynEdit);
    fNoNextKey := true;
    Key := 0;
  end;
end;

procedure TSynAutoComplete.EditorKeyPress(Sender: TObject; var Key: char);
begin
  if fNoNextKey then begin
    fNoNextKey := false;
    Key := #0;
  end;
end;

procedure TSynAutoComplete.Execute(token: string; Editor: TCustomSynEdit);
var
  Temp: string;
  i, j, prevspace: integer;
  StartOfBlock: tpoint;
begin
  i := AutoCompleteList.IndexOf(token);
  if (i <> -1) then begin
    fNoNextKey := true;
    for j := 1 to length(token) do
      Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    inc(i);
    StartOfBlock := Point(-1, -1);
    PrevSpace := 0;
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      for j := 0 to PrevSpace - 1 do
        Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
      Temp := AutoCompleteList[i];
      PrevSpace := 0;
      while (length(temp) >= PrevSpace + 2) and (temp[PrevSpace + 2] <= ' ') do
        inc(PrevSpace);
      for j := 2 to length(Temp) do begin
        Editor.CommandProcessor(ecChar, Temp[j], nil);
        if (Temp[j] = '|') then
          StartOfBlock := Editor.CaretXY
      end;
      inc(i);
      if (i < AutoCompleteList.Count) and
        (length(AutoCompleteList[i]) > 0) and
        (AutoCompleteList[i][1] = '=') then
        Editor.CommandProcessor(ecLineBreak, ' ', nil);
    end;
    if (StartOfBlock.x <> -1) and (StartOfBlock.y <> -1) then begin
      Editor.CaretXY := StartOfBlock;
      Editor.CommandProcessor(ecDeleteLastChar, ' ', nil);
    end;
  end;
end;

function TSynAutoComplete.GetPreviousToken(Editor: TCustomSynEdit): string;
var
  s: string;
  i: integer;
begin
  Result := '';
  if Editor <> nil then begin
    s := Editor.LineText;
    i := Editor.CaretX - 1;
    if i <= Length (s) then begin
      while (i > 0) and (s[i] > ' ') and (pos(s[i], FEndOfTokenChr) = 0) do
        Dec(i);
      Result := copy(s, i + 1, Editor.CaretX - i - 1);
    end;
  end
end;

procedure TSynAutoComplete.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent is TCustomSynEdit) then
    RemoveEditor(AComponent as TCustomSynEdit);
end;

function TSynAutoComplete.RemoveEditor(Editor: TCustomSynEdit): boolean;
begin
  Result := Assigned (Editor);
  if Result then begin
    Editor.RemoveKeyDownHandler (fKeyDownProc);
    Editor.RemoveKeyPressHandler (fKeyPressProc);
  end;
end;

procedure TSynAutoComplete.SetAutoCompleteList(List: TStrings);
begin
  fAutoCompleteList.Assign(List);
end;

procedure TSynAutoComplete.SetEditor(const Value: TCustomSynEdit);
begin
  if (fEditor <> nil) then begin
    RemoveEditor (fEditor);
    fEditor := nil;
  end;
  fEditor := Value;
  if (fEditor <> nil) then
    with fEditor do begin
      AddKeyDownHandler (fKeyDownProc);
      AddKeyPressHandler (fKeyPressProc);
    end;
end;

function TSynAutoComplete.GetTokenList: string;
var
  List: TStringList;
  i: integer;
begin
  Result := '';
  if AutoCompleteList.Count < 1 then Exit;
  List := TStringList.Create;
  i := 0;
  while (i < AutoCompleteList.Count) do begin
    if (length(AutoCompleteList[i]) > 0) and (AutoCompleteList[i][1] <> '=') then
      List.Add(Trim(AutoCompleteList[i]));
    inc(i);
  end;
  Result := List.Text;
  List.Free;
end;

function TSynAutoComplete.GetTokenValue(Token: string): string;
var
  i: integer;
  List: TStringList;
begin
  Result := '';
  i := AutoCompleteList.IndexOf(Token);
  if i <> -1 then begin
    List := TStringList.Create;
    Inc(i);
    while (i < AutoCompleteList.Count) and
      (length(AutoCompleteList[i]) > 0) and
      (AutoCompleteList[i][1] = '=') do begin
      if Length(AutoCompleteList[i]) = 1 then
        List.Add('')
      else
        List.Add(Copy(AutoCompleteList[i], 2, Length(AutoCompleteList[i])));
      inc(i);
    end;
    Result := List.Text;
    List.Free;
  end;
end;

end.

