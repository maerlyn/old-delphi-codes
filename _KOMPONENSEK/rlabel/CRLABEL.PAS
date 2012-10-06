{----------------------------------------------------------}
{      CRLABEL.PAS - Running label                         }
{    by Michael Kochiashvili   Kochini@iberiapac.ge        }
{----------------------------------------------------------}
{ This unit is public domain. You can add/rewrite code, but}
{if you do it, please, let me know. If you have any        }
{suggestions for improvements, or if you find any bugs,    }
{please notify the author.                                 }
{----------------------------------------------------------}
unit CRLabel;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

{$R *.RES}

{ TRunString - base class for running tools ------------------------------}
type
  TRLabelStyle = ( rlNormal, rlRaised, rlLowered);
  TRDirection  = ( rdStatic, rdRightToLeft, rdLeftToRight,
                             rdTopToBottom, rdBottomToTop);

type
  TRunString = class(TGraphicControl)
  private
    FLabelStyle : TRLabelStyle;  { label style }
    FRunDirection : TRDirection; { running direction}
    FRText : string;             { text to run}
    FSteps : integer;            { steps}
    FSpeed : integer;            { speed}
    FTimer : TTimer;
    FColor : TColor;             { background color}
    FFont : TFont;               { text font}
    FDepth : integer;            { depth of raised or lowered label}
    FStepToView : integer;       { Designing time only}
    FOnBegin, FOnStep, FOnEnd : TNotifyEvent;

    CurrentStep : integer;       { step number when running}
    RTWidth, RTHeight : integer; { text width & height}
    CnX, CnY : integer;          { }

    procedure LabelChanged(Sender: TObject);
    procedure SetLabelStyle( Value : TRLabelStyle);
    procedure SetDirection( Value : TRDirection);
    procedure SetRText( Value : string);
    procedure SetSteps( Value : integer); virtual;
    procedure SetSpeed( Value : integer);
    procedure SetColor( Value : TColor);
    procedure SetFont( Value : TFont);
    procedure SetDepth( Value : integer);
    procedure SetStepToView( Value : integer);

    procedure DoTextOut( ACanvas : TCanvas; x, y : integer; AText : string);

  protected
    procedure Paint; override;
    procedure TimerTick(Sender: TObject);

    property Depth : integer read FDepth write SetDepth default 1;
    property Direction : TRDirection read FRunDirection
                                     write SetDirection
                                     default rdRightToLeft;
    property RText : string read FRText write SetRText;
    { Designing time only. To view label at given step}
    property StepToView : integer read FStepToView write SetStepToView;

    { Events}
    property OnBegin : TNotifyEvent read FOnBegin write FOnBegin;
    property OnStep : TNotifyEvent read FOnStep write FOnStep;
    property OnEnd : TNotifyEvent read FOnEnd write FOnEnd;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Start runing. SStep is starting step number.
        if 0<SStep<Steps then new starting step is assigned
        else step number is not changed
    }
    procedure RLStart( SStep : integer);

    { Stop runing. Returns current step number}
    function RLStop : integer;

    { Reverse running direction}
    procedure RLReverse;

    { Get current step number}
    function GetCurrentStep : integer;

  published

    property LabelStyle : TRLabelStyle read FLabelStyle
                                       write SetLabelStyle
                                       default rlNormal;
    property Steps : integer read FSteps
                             write SetSteps
                             default 66;
    property Speed : integer read FSpeed
                             write SetSpeed
                             default 200;
    property Color : TColor read FColor write SetColor default clBtnFace;
    property RFont : TFont read FFont write SetFont;
  end;

{ TRunLabel --------------------------------------------------------------}
type
  TRunLabel = class( TRunString)
  public
  published
    property Align;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    { Propertis and events from TRunString}
    property LabelStyle;
    property Direction;
    property RText;
    property Steps;
    property Speed;
    property Color;
    property RFont;
    property StepToView;
    property OnBegin;
    property OnStep;
    property OnEnd;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TRunLabel]);
end;

{ TRunString --------------------------------------------------------------}
constructor TRunString.Create(AOwner: TComponent);
begin
  inherited Create( AOwner);
  ControlStyle := ControlStyle - [csOpaque];

  Width := 105;
  Height := 20;
  FColor := clBtnFace;
  FSteps := 66;
  FStepToView := FSteps DIV 2;
  CurrentStep := 0;
  FDepth := 1;
  FRunDirection := rdRightToLeft;
  FLabelStyle := rlNormal;
  FRText := 'Running label';

  FFont := TFont.Create;
  with FFont do begin
    Name := 'System';
    Size := 10;
    Color := clBlack;
    end;
  FFont.OnChange := LabelChanged;

  { Init timer}
  FTimer := TTimer.Create(Self);
  FSpeed := 200;
  with FTimer do begin
    Enabled := False;
    OnTimer := TimerTick;
    INterval := FSpeed;
    end;

end;

destructor TRunString.Destroy;
begin
  FTimer.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TRunString.DoTextOut( ACanvas : TCanvas; x, y : integer; AText : string);
begin
  with ACanvas do begin
    Font := FFont;
    Brush.Style := bsClear;
    { Draw text}
    case FLabelStyle of
      rlRaised : begin
        Font.Color := clBtnHighlight;
        TextOut( x, y, AText);
        Font.Color := clBtnShadow;
        TextOut( x + 2*FDepth, y + 2*FDepth, AText);
        end;
      rlLowered : begin
        Font.Color := clBtnShadow;
        TextOut( x, y, AText);
        Font.Color := clBtnHighlight;
        TextOut( x + 2*FDepth, y + 2*FDepth, AText);
        end;
      end;
    Font.Color := FFont.Color;
    TextOut( x + FDepth, y + FDepth, AText);
    end;
end;

procedure TRunString.Paint;
var
  TmpBmp : TBitMap;
  TmpRect : TRect;
  StX, StY, CurStep : integer;
  PctDone : real;
  DC : HDC;

begin
  TmpBmp := TBitMap.Create;
  try
    TmpBmp.Width := Width;
    TmpBmp.Height := Height;
    with TmpBmp.Canvas do begin
      Font := FFont;
      RTWidth := TextWidth( FRText) + 2 * FDepth;
      RTHeight := TextHeight( FRText) + 2 * FDepth;
      Brush.Color := FColor;
      Brush.Style := {bsClear} bsSolid;
      FillRect( ClipRect);
      end;
    { Calc center points}
    if RTWidth >= Width then CnX := 0 else  CnX := ( Width - RTWidth) DIV 2;
    if RTHeight >= Height then CnY := 0 else  CnY := ( Height - RTHeight) DIV 2;

(*
    { Copy background in to the TmpBmp}
    TmpBmp.Canvas.CopyRect( TmpBmp.Canvas.ClipRect,
                            Canvas, ClientRect);
*)

    {Calculate percentages & starting points}

    if csDesigning in ComponentState then begin
      PctDone := FStepToView / FSteps;
      end
    else begin
      PctDone := CurrentStep / FSteps;
      end;

    case FRunDirection of
      rdRightToLeft : begin
        StY := CnY;
        StX := -RTWidth + round(( RTWidth + Width) * ( 1 - PctDone));
        end;
      rdLeftToRight : begin
        StY := CnY;
        StX := -RTWidth + round(( RTWidth + Width) * ( PctDone));
        end;
      rdBottomToTop : begin
        StX := CnX;
        StY := -RTHeight + round( ( RTHeight + Height) * ( PctDone));
        end;
      rdTopToBottom : begin
        StX := CnX;
        StY := -RTHeight + round( ( RTHeight + Height) * ( 1 - PctDone));
        end;
      else begin { static label}
        StX := CnX; StY := CnY;
        end
      end;
    DoTextOut( TmpBmp.Canvas, StX, StY, FRText);
    Canvas.Draw( 0, 0, TmpBmp);
  finally
    TmpBmp.Free;
    end;
end;

procedure TRunString.LabelChanged(Sender: TObject);
begin
  with Canvas do begin
    Font := FFont;
    RTWidth := TextWidth( FRText) + 2 * FDepth;
    RTHeight := TextHeight( FRText) + 2 * FDepth;
    end;

  { Calc center points}
  if RTWidth >= Width then CnX := 0 else  CnX := ( Width - RTWidth) DIV 2;
  if RTHeight >= Height then CnY := 0 else  CnY := ( Height - RTHeight) DIV 2;

  Invalidate;
end;

procedure TRunString.SetLabelStyle( Value : TRLabelStyle);
begin
  if FLabelStyle <> Value then begin
    FLabelStyle := Value;
    LabelChanged( Self);
    end;
end;

procedure TRunString.SetDirection( Value : TRDirection);
begin
  if FRunDirection <> Value then begin
    FRunDirection := Value;
    end;
end;

procedure TRunString.SetRText( Value : string);
begin
  if FRText <> Value then begin
    FRText := Value;
    LabelChanged( Self);
    end;
end;

procedure TRunString.SetSteps( Value : integer);
begin
  if FSteps <> Value then begin
    FSteps := Value;
    if ( csDesigning in ComponentState) then begin
      FStepToView := FSteps DIV 2;
      Invalidate;
      end;
    end;
end;

procedure TRunString.SetSpeed( Value : integer);
begin
  if FSpeed <> Value then begin
    FSpeed := Value;
    if Value > 1000 then FSpeed := 1000;
    if Value < 1 then FSpeed := 1;
    {Change the timer interval}
    if FTimer <> Nil then FTimer.Interval := FSpeed;
    end;
end;

procedure TRunString.SetColor( Value : TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    LabelChanged( Self);
    end;
end;

procedure TRunString.SetFont( Value : TFont);
begin
  FFont.Assign( Value);
end;

procedure TRunString.SetDepth( Value : integer);
begin
  if FDepth <> Value then begin
    FDepth := Value;
    LabelChanged( Self);
    end;
end;

procedure TRunString.SetStepToView( Value : integer);
begin
  if ( csDesigning in ComponentState) AND
     ( FStepToView <> Value) AND
     ( Value <= FSteps) AND ( Value >= 0) then begin
    FStepToView := Value;
    LabelChanged( Self);
    end;
end;

procedure TRunString.RLStart( SStep : integer);
begin
  if FTimer.Enabled then exit;
  if ( SStep >= 0) AND ( SStep <= FSteps) then CurrentStep := SStep;
  FTimer.Enabled := true;
end;

function TRunString.RLStop : integer;
begin
  FTimer.Enabled := false;
  Result := CurrentStep;
end;

procedure TRunString.RLReverse;
begin
  if FRunDirection = rdStatic then exit;
  CurrentStep := FSteps - CurrentStep;
  case FRunDirection of
    rdLeftToRight : FRunDirection := rdRightToLeft;
    rdRightToLeft : FRunDirection := rdLeftToRight;
    rdTopToBottom : FRunDirection := rdBottomToTop;
    rdBottomToTop : FRunDirection := rdTopToBottom;
    end;
end;

function TRunString.GetCurrentStep : integer;
begin
  Result := CurrentStep;
end;

procedure TRunString.TimerTick(Sender: TObject);
begin
  if NOT FTimer.Enabled then exit;
  if ( CurrentStep = 0) AND Assigned( FOnBegin) then FOnBegin( Self);
  inc( CurrentStep);
  Paint;
  if Assigned( FOnStep) then FOnStep( Self);
  if CurrentStep >= FSteps then begin
    FTimer.Enabled := false;
    if Assigned(FOnEnd) then FOnEnd( Self);
    CurrentStep := 0;
    end;
end;


end.
