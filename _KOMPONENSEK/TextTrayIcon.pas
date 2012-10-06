{*****************************************************************}
{ This is a component for placing icons in the notification area  }
{ of the Windows taskbar (aka. the traybar).                      }
{                                                                 }
{ It is an expanded version of my CoolTrayIcon component, which   }
{ you will need to make this work. The expanded features allow    }
{ you to easily draw text in the tray icon.                       }
{                                                                 }
{ The component is freeware. Feel free to use and improve it.     }
{ I would be pleased to hear what you think.                      }
{                                                                 }
{ Troels Jakobsen - delphiuser@get2net.dk                         }
{ Copyright (c) 2001                                              }
{*****************************************************************}

unit TextTrayIcon;

interface

uses
  CoolTrayIcon, Graphics, Classes, Controls;

type
  TOffsetOptions = class(TPersistent)
  private
    FOffsetX,
    FOffsetY,
    FLineDistance: Integer;
    FOnChange: TNotifyEvent;           // Procedure var.
    procedure SetOffsetX(Value: Integer);
    procedure SetOffsetY(Value: Integer);
    procedure SetLineDistance(Value: Integer);
  protected
    procedure Changed; dynamic;
  published
    property OffsetX: Integer read FOffsetX write SetOffsetX;
    property OffsetY: Integer read FOffsetY write SetOffsetY;
    property LineDistance: Integer read FLineDistance write SetLineDistance;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextTrayIcon = class(TCoolTrayIcon)
  private
    FFont: TFont;
    FColor: TColor;
    FBorder: Boolean;
    FBorderColor: TColor;
    FText: String;
    FTextBitmap: TBitmap;
    FOffsetOptions: TOffsetOptions;
    FShowTextIcon: Boolean;
    procedure FontChanged(Sender: TObject);
    procedure SplitText(const Strings: TList);
    procedure OffsetOptionsChanged(OffsetOptions: TObject);
  protected
    procedure Paint; virtual;
    procedure SetShowTextIcon(Value: Boolean);
    procedure SetText(Value: String);
    procedure SetTextBitmap(Value: TBitmap);
    procedure SetFont(Value: TFont);
    procedure SetColor(Value: TColor);
    procedure SetBorder(Value: Boolean);
    procedure SetBorderColor(Value: TColor);
    procedure SetOffsetOptions(Value: TOffsetOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Draw;
  published
    property ShowTextIcon: Boolean read FShowTextIcon write SetShowTextIcon
      default true;
    property Text: String read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Border: Boolean read FBorder write SetBorder;
    property BorderColor: TColor read FBorderColor write SetBorderColor
      default clBlack;
    property Options: TOffsetOptions read FOffsetOptions write SetOffsetOptions;
  end;

procedure Register;

implementation

uses
  SysUtils;

{------------------- TTextTrayIcon --------------------}

procedure TOffsetOptions.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;


procedure TOffsetOptions.SetOffsetX(Value: Integer);
begin
  if Value <> FOffsetX then
  begin
    FOffsetX := Value;
    Changed;
  end;
end;


procedure TOffsetOptions.SetOffsetY(Value: Integer);
begin
  if Value <> FOffsetY then
  begin
    FOffsetY := Value;
    Changed;
  end;
end;


procedure TOffsetOptions.SetLineDistance(Value: Integer);
begin
  if Value <> FLineDistance then
  begin
    FLineDistance := Value;
    Changed;
  end;
end;

{------------------- TTextTrayIcon --------------------}

constructor TTextTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowTextIcon := true;
  FTextBitmap := TBitmap.Create;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FColor := clBtnFace;
  FBorderColor := clBlack;
  FOffsetOptions := TOffsetOptions.Create;
  FOffsetOptions.OnChange := OffsetOptionsChanged;
end;


destructor TTextTrayIcon.Destroy;
begin
  FFont.Free;
  FTextBitmap.Free;
  FOffsetOptions.Free;
  inherited Destroy;
end;


procedure TTextTrayIcon.FontChanged(Sender: TObject);
{ This method is invoked when user assigns to Font (but not when
  Font is set directly to another TFont var.) }
begin
  Draw;
end;


procedure TTextTrayIcon.SetShowTextIcon(Value: Boolean);
begin
  FShowTextIcon := Value;
  Draw;
end;


procedure TTextTrayIcon.SetText(Value: String);
begin
  FText := Value;
  Draw;
end;


procedure TTextTrayIcon.SetTextBitmap(Value: TBitmap);
begin
  FTextBitmap := Value;       // Assign?
  Draw;
end;


procedure TTextTrayIcon.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  Draw;
end;


procedure TTextTrayIcon.SetColor(Value: TColor);
begin
  FColor := Value;
  Draw;
end;


procedure TTextTrayIcon.SetBorder(Value: Boolean);
begin
  FBorder := Value;
  Draw;
end;


procedure TTextTrayIcon.SetBorderColor(Value: TColor);
begin
  FBorderColor := Value;
  Draw;
end;


procedure TTextTrayIcon.SetOffsetOptions(Value: TOffsetOptions);
{ This method will only be invoked if the user creates a new
  TOffsetOptions object. User will probably just set the values
  of the existing TOffsetOptions object. }
begin
  FOffsetOptions.Assign(Value);
  Draw;
end;


procedure TTextTrayIcon.OffsetOptionsChanged(OffsetOptions: TObject);
{ This method will be invoked when the user changes the values
  of the existing TOffsetOptions object. }
begin
  Draw;
end;


procedure TTextTrayIcon.Draw;
var
  Ico: TIcon;
begin
  if FShowTextIcon then      // Only redraw if in text mode!
  begin
    CycleIcons := False;     // We cannot cycle and draw at the same time
    Paint;                   // Render FTextBitmap
    Ico := TIcon.Create;
    if BitmapToIcon(FTextBitmap, Ico, clNone) then
    begin
      Icon.Assign(Ico);
      Refresh;
      Ico.Free;
    end;
  end;
end;


procedure TTextTrayIcon.Paint;
var
  Bitmap: TBitmap;
  Left, Top, LinesTop, LineHeight: Integer;
  Substr: PChar;
  Strings: TList;
  I: Integer;
begin
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Width := 16;
    Bitmap.Height := 16;
//    Bitmap.Canvas.TextFlags := 2;      // ETO_OPAQUE

    // Render background rectangle
    Bitmap.Canvas.Brush.Color := FColor;
    Bitmap.Canvas.FillRect(Rect(0, 0, 16, 16));

    // Render text; check for line breaks
    Bitmap.Canvas.Font.Assign(FFont);
    Substr := StrPos(PChar(FText), #13);
    if Substr = nil then
    begin
      // No line breaks
      Left := (15 - Bitmap.Canvas.TextWidth(FText)) div 2;
      if FOffsetOptions <> nil then
        Left := Left + FOffsetOptions.OffsetX;
      Top := (15 - Bitmap.Canvas.TextHeight(FText)) div 2;
      if FOffsetOptions <> nil then
        Top := Top + FOffsetOptions.OffsetY;
      Bitmap.Canvas.TextOut(Left, Top, FText);
    end
    else
    begin
      // Line breaks
      Strings := TList.Create;
      SplitText(Strings);
      LineHeight := Bitmap.Canvas.TextHeight(Substr);
      if FOffsetOptions <> nil then
        LineHeight := LineHeight + FOffsetOptions.LineDistance;
      LinesTop := (15 - (LineHeight * Strings.Count)) div 2;
      if FOffsetOptions <> nil then
        LinesTop := LinesTop + FOffsetOptions.OffsetY;
      for I := 0 to Strings.Count -1 do
      begin
        Substr := Strings[I];
        Left := (15 - Bitmap.Canvas.TextWidth(Substr)) div 2;
        if FOffsetOptions <> nil then
          Left := Left + FOffsetOptions.OffsetX;
        Top := LinesTop + (LineHeight * I);
        Bitmap.Canvas.TextOut(Left, Top, Substr);
      end;
      for I := 0 to Strings.Count -1 do
        StrDispose(Strings[I]);
      Strings.Free;
    end;

    // Render border
    if FBorder then
    begin
      Bitmap.Canvas.Brush.Color := FBorderColor;
      Bitmap.Canvas.FrameRect(Rect(0, 0, 16, 16));
    end;

    // Assign the final bitmap
    FTextBitmap.Assign(Bitmap);
    
  finally
    Bitmap.Free;
  end;
end;


procedure TTextTrayIcon.SplitText(const Strings: TList);

  function PeekedString(S: String): String;
  var
    P: Integer;
  begin
    P := Pos(#13, S);
    if P = 0 then
      Result := S
    else
      Result := Copy(S, 1, P-1);
  end;

var
  Substr: String;
  P: Integer;
  S: PChar;
begin
  Strings.Clear;
  Substr := FText;
  repeat
    P := Pos(#13, Substr);
    if P = 0 then
    begin
      S := StrNew(PChar(Substr));
      Strings.Add(S);
    end
    else
    begin
      S := StrNew(PChar(PeekedString(Substr)));
      Strings.Add(S);
      Delete(Substr, 1, P);
    end;
  until P = 0;
end;


procedure Register;
begin
  RegisterComponents('Custom', [TTextTrayIcon]);
end;

end.

