{*********************************************************}
{*                   OVCPLB.PAS 4.06                    *}
{*********************************************************}

{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Orpheus                                    *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C)1995-2002    *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

(*Changes)

  10/20/01- Hdc changed to TOvcHdc for BCB Compatibility
*)

{$I OVC.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{.W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

unit ovcplb;
  {-Picture label component}

interface

uses
  Windows, Classes, Controls, Forms, Graphics, Menus, Messages, SysUtils,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  OvcData, OvcIntl, OvcRLbl, OvcStr, OvcUser, OvcDate, OvcMisc;

type
  TPictureLabelDataType =
    (plNone, plBoolean, plYesNo, plDate, plFloat, plInteger,
     plStDate, plStTime, plString, plTime);

type
  TOvcCustomPictureLabel = class(TOvcCustomRotatedLabel)
  {.Z+}
  protected {private}
    FData           : string[255];
    FDataType       : TPictureLabelDataType;
    FIntlSupport    : TOvcIntlSup;  {international support object}
    FPictureMask    : string;
    FPictureFlags   : array[1..255] of Byte;
    FUseIntlMask    : Boolean;
    FUserData       : TOvcUserData;

    {property methods}
    procedure SetAsBoolean(Value : Boolean);
      {-set the field value to a Boolean value}
    procedure SetAsDate(Value : TDateTime);
      {-set the field value to a Delphi date value}
    procedure SetAsFloat(Value : Extended);
      {-set the field value to a Double value}
    procedure SetAsInteger(Value : Longint);
      {-set the field value to a LongInt value}
    procedure SetAsStDate(Value : TStDate);
      {-set the field value to an Orpheus date value}
    procedure SetAsStTime(Value : TStTime);
      {-set the field value to an Orpheus time value}
    procedure SetAsString(const Value : string);
      {-set the field value to a string value}
    procedure SetAsTime(Value : TDateTime);
      {-set the field value to a Delphi time value}
    procedure SetAsVariant(Value : Variant);
      {-sets the field value to a Variant value}
    procedure SetAsYesNo(Value : Boolean);
      {-set the field value to a Boolean value}
    procedure SetIntlSupport(Value : TOvcIntlSup);
      {-set the international support object to use}
    procedure SetPictureMask(const Value : string);
      {-set the picture mask}
    procedure SetUseIntlMask(Value : Boolean);
      {-set the use of the international picture masks}
    procedure SetUserData(Value : TOvcUserData);
      {-set user-defined mask data object}

    {internal methods}
    procedure plCalcWidthAndPlaces(var Width, Places : Word);
      {-calculate width and decimal places for a numeric value}
    procedure plFixCase(PicChar : AnsiChar; var Ch : AnsiChar; PrevCh : AnsiChar);
      {-fix the case of Ch based on PicChar}
    procedure plFixDecimalPoint(var S : string);
      {-fix decimal points for real numbers before merging}
    function plGetDisplayString : string;
      {-return the display string}
    procedure plInitPictureFlags;
      {-initialize flags for the picture mask}
    function plIsLiteral(N : Word) : Boolean;
      {-return True if the N'th mask character is a literal. N is 1-based}
    function plIsSemiLiteral(N : Word) : Boolean;
      {-return True if the N'th mask character is a semi-literal. N is 1-based}
    function plMergePicture(const Src : string) : string;
      {-merge Src with picture and return result}
    procedure plReadParentFont(Reader : TReader);

  protected
    procedure DefineProperties(Filer : TFiler);
      override;
    function plGetSampleDisplayData : string;
      dynamic;
      {-return the text to display while in design mode}
  {.Z-}

    {properties}
    property PictureMask : string
      read FPictureMask
      write SetPictureMask;

    property UseIntlMask : Boolean
      read FUseIntlMask
      write SetUseIntlMask;

  public
  {.Z+}
    constructor Create(AOwner : TComponent);
      override;
  {.Z-}

{ - Hdc changed to TOvcHdc for BCB Compatibility }
    procedure PaintTo(DC : TOvcHdc{hDC}; X, Y : Integer);
      virtual;

    procedure Clear;
      {-clear the field contents and picture mask}
    function GetDisplayString : string;
      {-return the display string}
      virtual;

    {public properties}
    property AsBoolean : Boolean
      write SetAsBoolean;

    property AsDate : TDateTime
      write SetAsDate;

    property AsFloat : Extended
      write SetAsFloat;

    property AsInteger : Longint
      write SetAsInteger;

    property AsString : string
      write SetAsString;

    property AsTime : TDateTime
      write SetAsTime;

    property AsVariant : Variant
      write SetAsVariant;

    property AsStDate : TStDate
      write SetAsStDate;

    property AsStTime : TStTime
      write SetAsStTime;

    property AsYesNo : Boolean
      write SetAsYesNo;

    property Canvas;

    property IntlSupport : TOvcIntlSup
      read FIntlSupport
      write SetIntlSupport;

    property Text : string
      read GetDisplayString
      write SetAsString;

    property UserData :  TOvcUserData
      read FUserData
      write SetUserData;
  end;

  TOvcPictureLabel = class(TOvcCustomPictureLabel)
  published
    {$IFDEF VERSION4}
    property Anchors;
    property Constraints;
    property DragKind;
    {$ENDIF}
    property Align;
    property Alignment default taLeftJustify;
    property AutoSize;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property FontAngle default 0;
    property OriginX default 0;
    property OriginY default 0;
    property ParentColor;
    property ParentShowHint;
    property PictureMask;
    property PopupMenu;
    property ShadowColor default clBtnShadow;
    property ShadowedText default False;
    property ShowHint;
    property Transparent default False;
    property UseIntlMask;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{*** TOvcCustomPictureLabel ***}

procedure TOvcCustomPictureLabel.Clear;
  {-clear the field contents and picture mask}
begin
  FillChar(FData, SizeOf(FData), #0);
  FDataType := plNone;
  Caption := GetDisplayString;
end;

constructor TOvcCustomPictureLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {set default values for inherited properties}
  ParentColor     := True;

  {set default property values}
  FIntlSupport    := OvcIntlSup;
  FPictureMask    := 'XXXXXXXXXX';
  FUseIntlMask    := False;

  {initialize picture mask flags}
  plInitPictureFlags;

  {assign default user data object}
  FUserData := OvcUserData;
end;

procedure TOvcCustomPictureLabel.DefineProperties(Filer : TFiler);
begin
  inherited DefineProperties(Filer);

  {define a ParentFont property for compatibility with eariler versions}
  Filer.DefineProperty('ParentFont', plReadParentFont, nil, False);
end;

function TOvcCustomPictureLabel.GetDisplayString : string;
{-return the display string}
begin
  Result := Caption;
  if csDesigning in ComponentState then
    Result := plGetSampleDisplayData
  else
    Result := plGetDisplayString;
end;

{ - Hdc changed to TOvcHdc for BCB Compatibility }
procedure TOvcCustomPictureLabel.PaintTo(DC : TOvcHdc{hDC}; X, Y : Integer);
var
  R : TRect;
begin
  R.Left := X;
  R.Top := Y;
  R.Right := X + ClientWidth;
  R.Bottom := Y + ClientHeight;

  Canvas.Handle := DC;
  try
    if not Transparent then begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Brush.Style := bsSolid;
      Canvas.FillRect(R);
    end;
    Canvas.Brush.Style := bsClear;
    lblDrawText(R, DT_LEFT)
  finally
    Canvas.Handle := 0;
  end;
end;

procedure TOvcCustomPictureLabel.plCalcWidthAndPlaces(var Width, Places : Word);
  {-calculate width and decimal places for a numeric value}
var
  I      : Word;
  DotPos : Word;
begin
  Places := 0;
  {find position of period and calculate decimal places}
  DotPos := Pos(pmDecimalPt, FPictureMask);
  if DotPos > 0 then begin
    I := DotPos+1;
    while (FPictureFlags[I] = pflagFormat) do begin
      Inc(Places);
      Inc(I);
    end;
  end;

  {calculate width}
  I := 1;
  Width := 0;
  while plIsLiteral(I) or plIsSemiLiteral(I) do
    Inc(I);
  while (FPictureFlags[I] = pflagFormat) or
        (FPictureMask[I] = pmComma) do begin
    Inc(Width, Ord(FPictureFlags[I] = pflagFormat));
    Inc(I);
  end;

  {add decimal places and period}
  if (DotPos <> 0) and (Places <> 0) then
    Inc(Width, Places + 1);
end;

procedure TOvcCustomPictureLabel.plFixCase(PicChar : AnsiChar;
  var Ch : AnsiChar; PrevCh : AnsiChar);
  {-fix the case of Ch based on PicChar}
begin
  case PicChar of
    pmMonthNameU, pmForceUpper, pmUpperAlpha, pmTrueFalse,
    pmYesNo, pmScientific, pmHexadecimal :
      Ch := UpCaseChar(Ch);
    pmForceLower, pmLowerAlpha :
      Ch := LoCaseChar(Ch);
    pmForceMixed :
      case PrevCh of
        ' ', '-' :
          Ch := UpCaseChar(Ch);
      end;
    pmAmPm : ;
    pmUser1..pmUser8 :
      case UserData.ForceCase[PicChar] of
        mcUpperCase :
          Ch := UpCaseChar(Ch);
        mcLowerCase :
          Ch := LoCaseChar(Ch);
        mcMixedCase :
          case PrevCh of
            ' ', '-' :
              Ch := UpCaseChar(Ch);
          end;
      end;
  end;
end;

procedure TOvcCustomPictureLabel.plFixDecimalPoint(var S : string);
  {-fix decimal points for real numbers before merging}
var
  P : Integer;
begin
  P := Pos(pmDecimalPt, S);
  if P > 0 then
    S[P] := IntlSupport.DecimalChar;
end;

function TOvcCustomPictureLabel.plGetDisplayString : string;
{-return the display string}

  function MergeBoolean(var Data) : string;
  var
    B : Boolean absolute Data;
  begin
    if B then
      Result := IntlSupport.TrueChar
    else
      Result := IntlSupport.FalseChar;
    Result := plMergePicture(Result);
  end;

  function MergeDate(var Data) : string;
  var
    DT : TDateTime absolute Data;
    FC : Boolean;
    S  : string;
  begin
    if FUseIntlMask then begin
      FC := Pos('yyyy', ShortDateFormat) > 0;
      S := FIntlSupport.InternationalDate(FC)
    end else
      S := FPictureMask;
    Result := FIntlSupport.DateToDateString(S, DateTimeToStDate(DT), False);
  end;

  function MergeFloat(var Data) : string;
  var
    E      : Extended absolute Data;
    Width  : Word;
    Places : Word;
    L      : Integer;
  begin
    plCalcWidthAndPlaces(Width, Places);
    L := Length(FPictureMask);
    Str(E:0:Places, Result);
    Result := Trim(Result);

    {does it fit?}
    if (Length(Result) > L) or (Pos('E', Result) > 0) then begin
      {won't fit--use scientific notation}
      if (Places > 0) and (9+Places < L) then
        Str(E:9+Places, Result)
      else
        Str(E:L, Result);
      Result := Trim(Result);
      Result := TrimEmbeddedZeros(Result);
      {convert decimal point}
      L := Pos(pmDecimalPt, Result);
      if L > 0 then
        Result[L] := IntlSupport.DecimalChar;
    end else
      Result := plMergePicture(Result);
  end;

  function MergeInteger(var Data) : string;
  var
    D   : LongInt absolute Data;
    L   : Integer;
    Buf : array[0..MaxEditLen] of Char;
  begin
    L := Length(FPictureMask);
    if Pos(pmHexadecimal, FPictureMask) > 0 then begin
      HexLPChar(Buf, D);
      if L < 8 then
        StrStDeletePrim(Buf, 0, 8-L);
      Result := StrPas(Buf);
    end else if Pos(pmOctal, FPictureMask) > 0 then begin
      OctalLPChar(Buf, D);
      if L < 12 then
        StrStDeletePrim(Buf, 0, 12-L);
      Result := StrPas(Buf);
    end else if Pos(pmBinary, FPictureMask) > 0 then begin
      BinaryLPChar(Buf, D);
      if L < 32 then
        StrStDeletePrim(Buf, 0, 32-L);
      Result := StrPas(Buf);
    end else
      Str(D, Result);
    Result := plMergePicture(Result);
  end;

  function MergeStDate(var Data) : string;
  var
    D  : TStDate absolute Data;
    FC : Boolean;
    S  : string;
  begin
    if FUseIntlMask then begin
      FC := Pos('yyyy', ShortDateFormat) > 0;
      S := FIntlSupport.InternationalDate(FC)
    end else
      S := FPictureMask;
    Result := FIntlSupport.DateToDateString(S, D, False);
  end;

  function MergeStTime(var Data) : string;
  var
    T : TStTime absolute Data;
    S : string;
  begin
    if FUseIntlMask then
      S := FIntlSupport.InternationalTime(False)
    else
      S := FPictureMask;
    Result := FIntlSupport.TimeToTimeString(S, T, False);
  end;

  function MergeString(var Data) : string;
  var
    S : string[255] absolute Data;
  begin
    Result := plMergePicture(S);
  end;

  function MergeTime(var Data) : string;
  var
    DT : TDateTime absolute Data;
    S  : string;
  begin
    if FUseIntlMask then
      S := FIntlSupport.InternationalTime(False)
    else
      S := FPictureMask;
    Result := FIntlSupport.TimeToTimeString(S, DateTimeToStTime(DT), False);
  end;

  function MergeYesNo(var Data) : string;
  var
    B : Boolean absolute Data;
  begin
    if B then
      Result := IntlSupport.YesChar
    else
      Result := IntlSupport.NoChar;
    Result := plMergePicture(Result);
  end;

begin
  case FDataType of
    plNone    : Caption := '';
    plBoolean : Caption := MergeBoolean(FData);
    plDate    : Caption := MergeDate(FData);
    plFloat   : Caption := MergeFloat(FData);
    plInteger : Caption := MergeInteger(FData);
    plStDate  : Caption := MergeStDate(FData);
    plStTime  : Caption := MergeStTime(FData);
    plString  : Caption := MergeString(FData);
    plTime    : Caption := MergeTime(FData);
    plYesNo   : Caption := MergeYesNo(FData);
  end;
  Result := Caption;
end;

function TOvcCustomPictureLabel.plGetSampleDisplayData : string;
  {-return the text to display while in design mode}
begin
  Result := PictureMask;
end;

procedure TOvcCustomPictureLabel.plInitPictureFlags;
var
  I : Word;
begin
  FillChar(FPictureFlags, SizeOf(FPictureFlags), pflagLiteral);
  for I := 1 to Length(FPictureMask) do begin
    if FPictureMask[I] in PictureChars then
      FPictureFlags[I] := pflagFormat
    else
      case FPictureMask[I] of
        pmFloatDollar,
        pmComma        : FPictureFlags[I] := pflagSemiLit;
      end;
  end;
end;

function TOvcCustomPictureLabel.plIsLiteral(N : Word) : Boolean;
  {-return True if the N'th mask character is a literal. N is 1-based}
begin
  Result := FPictureFlags[N] = pflagLiteral;
end;

function TOvcCustomPictureLabel.plIsSemiLiteral(N : Word) : Boolean;
  {-return True if the N'th mask character is a semi-literal. N is 1-based}
begin
  Result := FPictureFlags[N] = pflagSemiLit;
end;

function TOvcCustomPictureLabel.plMergePicture(const Src : string) : string;
  {-merge Src with picture and return result}
var
  SrcLen     : Integer;
  ResultLen  : Integer;
  DotPosP    : Integer;
  DotPosS    : Integer;
  FloatPos   : Integer;
  I, J, K, N : Integer;
  CurLeftMax : Byte;
  CurLeftLen : Byte;
  NeedFloat  : Boolean;
  IsNum      : Boolean;
  PicChar    : AnsiChar;
  NeedMinus  : Boolean;
  NotDecimal : Boolean;
  CopyOfSrc  : string;
  Buf        : array[0..255] of Char;

  procedure HandleOtherCases;
  begin
    if NeedFloat then begin
      Dec(CurLeftLen);
      Result[I] := IntlSupport.CurrencyLtStr[CurLeftLen+1];
      NeedFloat := CurLeftLen <> 0;
    end else if NeedMinus then begin
      Result[I] := '-';
      NeedMinus := False;
    end else if NotDecimal then
      Result[I] := '0'
    else
      Result[I] := ' ';
  end;

begin
  {get initial size and copy of Src}
  SrcLen := Length(Src);
  CopyOfSrc := Src;

  {copy picture mask into result}
  Result := FPictureMask;
  ResultLen := Length(Result);

  {set hexadecimal/octal/binary flag}
  NotDecimal := (Pos(pmHexadecimal, FPictureMask) > 0) or
                (Pos(pmOctal, FPictureMask) > 0) or
                (Pos(pmBinary, FPictureMask) > 0);

  {get position of decimal point}
  DotPosP := Pos(pmDecimalPt, FPictureMask);

  {is it a numeric string?}
  IsNum := FDataType in [plFloat, plInteger];

  {take care of left currency strings}
  I := Pos(pmCurrencyLt, FPictureMask);
  if I > 0 then begin
    K := I;
    while (K+1 < ResultLen) and (FPictureMask[K+1] = pmCurrencyLt) do
      Inc(K); {K marks end of CurrencyLt mask sequence}

    StrPCopy(Buf, IntlSupport.CurrencyLtStr);
    J := StrLen(Buf);
    for N := K downto I do
      if J > 0 then begin
        Dec(J);
        Result[N] := Buf[J];
      end else
        Result[N] := ' ';
  end;

  {take care of right currency strings}
  I := Pos(pmCurrencyRt, FPictureMask);
  if I > 0 then begin
    J := 0;
    StrPCopy(Buf, IntlSupport.CurrencyRtStr);
    K := StrLen(Buf);
    while (I <= ResultLen) and (FPictureMask[I] = pmCurrencyRt) do begin
      if J < K then begin
        Result[I] := Buf[J];
        Inc(J);
      end else
        Result[I] := ' ';
      Inc(I);
    end;
  end;

  if IsNum then begin
    {we need to fill in the FloatDollar positions too, if any}
    FloatPos := Pos(pmFloatDollar, FPictureMask);
    if FloatPos > 0 then begin
      CurLeftLen := Length(IntlSupport.CurrencyLtStr);
      CurLeftMax := 1;
      while FPictureMask[FloatPos+1] = pmFloatDollar do begin
        Inc(FloatPos);
        Inc(CurLeftMax);
      end;
      if CurLeftMax < CurLeftLen then
        CurLeftLen := CurLeftMax;
    end else begin
      CurLeftLen := 0;
      FloatPos := -1;
    end;

    {trim leading and trailing blanks}
    CopyOfSrc := Trim(CopyOfSrc);

    {check for a minus sign}
    NeedMinus := (CopyOfSrc[1] = '-');
    if NeedMinus then
      Delete(CopyOfSrc, 1, 1);

    {it's a numeric field--align the decimal points}
    DotPosS := Pos(pmDecimalPt, CopyOfSrc);

    {see if we need a floating dollar sign}
    SrcLen := Length(CopyOfSrc);
    NeedFloat := (SrcLen <> 0) and (CurLeftLen <> 0);

    {if there's no tail, pretend there's a dot beyond the end of CopyOfSrc}
    if DotPosS = 0 then
      K := SrcLen
    else
      K := DotPosS;

    {copy the tail of the string}
    if DotPosP = 0 then
      I := ResultLen
    else
      I := DotPosP+1;
    J := K+1;
    while (J <= SrcLen) and (I <= ResultLen) and not plIsLiteral(I) do begin
      Result[I] := CopyOfSrc[J];
      Inc(I);
      Inc(J);
    end;

    {pad to end with 0's}
    while (I <= ResultLen) and not plIsLiteral(I) do begin
      Result[I] := '0';
      Inc(I);
    end;

    {merge the head of the string}
    if DotPosP = 0 then
      J := ResultLen
    else
      J := DotPosP;
    if DotPosS > 0 then
      SrcLen := DotPosS-1;
    for I := J downto 1 do begin
      PicChar := FPictureMask[I];
      case FPictureFlags[I] of
        pflagFormat,
        pflagSemiLit :
          if PicChar = pmComma then begin
            if (SrcLen > 0) then begin
              if IntlSupport.CommaChar <> #0 then
                Result[I] := IntlSupport.CommaChar
              else
                System.Delete(Result, I, 1);
            end else
              HandleOtherCases;
          end else if (SrcLen > 0) and (I > FloatPos) then begin
            Result[I] := CopyOfSrc[SrcLen];
            Dec(SrcLen);
          end else
            HandleOtherCases;
        pflagLiteral :
          case PicChar of
            Subst1..Subst8 :
              Result[I] := UserData.SubstChars[PicChar];
            pmDecimalPt :
              Result[I] := IntlSupport.DecimalChar;
          end;
      end;
    end;

    {put in a 0 before the dot if necessary}
    if (DotPosP > 1) and (Result[DotPosP-1] = ' ') then
      Result[DotPosP-1] := '0';
  end else begin
    {fill in the characters from CopyOfSrc}
    J := 1;
    for I := 1 to ResultLen do begin
      PicChar := FPictureMask[I];
      case FPictureFlags[I] of
        pflagLiteral :
          case PicChar of
            Subst1..Subst8 :
              Result[I] := UserData.SubstChars[PicChar];
          end;
      else
        if (J <= SrcLen) then begin
          Result[I] := CopyOfSrc[J];
          plFixCase(PicChar, Result[I], #255);
          Inc(J);
        end else
          Result[I] := ' ';
      end;
    end;

    if FDataType = plFloat then
      plFixDecimalPoint(Result);
  end;
end;

procedure TOvcCustomPictureLabel.plReadParentFont(Reader : TReader);
begin
  {discard value}
  Reader.ReadBoolean;
end;

procedure TOvcCustomPictureLabel.SetAsBoolean(Value : Boolean);
  {-set the field value to a Boolean value}
begin
  Move(Value, FData, SizeOf(Boolean));
  FDataType := plBoolean;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsDate(Value : TDateTime);
  {-set the field value to a Delphi date value}
begin
  Move(Value, FData, SizeOf(TDateTime));
  FDataType := plDate;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsFloat(Value : Extended);
  {-set the field value to a Double value}
begin
  Move(Value, FData, SizeOf(Extended));
  FDataType := plFloat;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsInteger(Value : Longint);
  {-set the field value to a LongInt value}
begin
  Move(Value, FData, SizeOf(LongInt));
  FDataType := plInteger;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsStDate(Value : TStDate);
  {-set the field value to a Orpheus date value}
begin
  Move(Value, FData, SizeOf(TStDate));
  FDataType := plStDate;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsStTime(Value : TStTime);
  {-set the field value to a Orpheus time value}
begin
  Move(Value, FData, SizeOf(TStTime));
  FDataType := plStTime;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsString(const Value : string);
  {-set the field value to a string value}
begin
  FData := Value;
  FDataType := plString;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsTime(Value : TDateTime);
  {-set the field value to a Delphi time value}
begin
  Move(Value, FData, SizeOf(TDateTime));
  FDataType := plTime;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsYesNo(Value : Boolean);
  {-set the field value to a Boolean value}
begin
  Move(Value, FData, SizeOf(Boolean));
  FDataType := plYesNo;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetAsVariant(Value : Variant);
  {-sets the field value to a Variant value}
begin
  case VarType(Value) of
    varSmallInt,
    varInteger  : SetAsInteger(Value);
    varSingle,
    varDouble,
    varCurrency : SetAsFloat(Value);
    varDate     : SetAsDate(Value);
    varBoolean  : SetAsBoolean(Value);
    varString   : SetAsString(Value);
  end;
end;

procedure TOvcCustomPictureLabel.SetIntlSupport(Value : TOvcIntlSup);
  {-set the international support object this field will use}
begin
  if Assigned(Value) then
    FIntlSupport := Value
  else
    FIntlSupport := OvcIntlSup;
end;

procedure TOvcCustomPictureLabel.SetPictureMask(const Value : string);
  {-set the picture mask}
begin
  if (FPictureMask <> Value) and (Value <> '') then begin
    FPictureMask := Value;
    plInitPictureFlags;
    Caption := GetDisplayString;
  end;
end;

procedure TOvcCustomPictureLabel.SetUserData(Value : TOvcUserData);
  {-set pointer to user-defined mask data object}
begin
  if Assigned(Value) then
    FUserData := Value
  else
    FUserData := OvcUserData;
  Caption := GetDisplayString;
end;

procedure TOvcCustomPictureLabel.SetUseIntlMask(Value : Boolean);
  {-set the use of the international picture masks}
begin
  if Value <> FUseIntlMask then begin
    FUseIntlMask := Value;
    Caption := GetDisplayString;
  end;
end;


end.
