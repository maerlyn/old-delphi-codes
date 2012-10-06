unit Rltestu;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, CRLabel, ExtCtrls, Tabs, Spin;

type
  TColorArray = array[ 0..6] of TColor;
const
  TxtColors : TColorArray =
      ( clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal);
  BkgColors : TColorArray =
      ( clSilver, clRed, clLime, clBlue, clFuchsia, clAqua, clWhite);

type
  TRLDemo = class(TForm)
    RLTPanel: TPanel;
    RLTTabSet: TTabSet;
    RLTNotebook: TNotebook;
    RLPanel1: TPanel;
    IconPanel: TPanel;
    IconIm: TImage;
    RLPanel2: TPanel;
    RLabelMain: TRunLabel;
    { Direction properties group}
    DirectionGB: TGroupBox;
    StRB: TRadioButton;
    RtLRB: TRadioButton;
    LtRRB: TRadioButton;
    TtBRB: TRadioButton;
    BtTRB: TRadioButton;
    { LabelStyle properties group}
    LabelStyleGB: TGroupBox;
    NlRB: TRadioButton;
    LlRB: TRadioButton;
    RlRB: TRadioButton;
    RunButton1: TButton;
    FontButton: TButton;
    FontDialog1: TFontDialog;
    RLText: TEdit;
    Label1: TLabel;
    StepsSE: TSpinEdit;
    Label2: TLabel;
    SpeedSE: TSpinEdit;
    Label3: TLabel;
    { Events groups}
    EvOnBeginGB: TGroupBox;
    EvOnEndGB: TGroupBox;
    EvOnStepGB: TGroupBox;
    EvChColor: TRadioButton;
    EvCycle: TRadioButton;
    EvNone1: TRadioButton;
    EvNone2: TRadioButton;
    EvReverse: TRadioButton;
    EvNone3: TRadioButton;
    EvStopAt: TRadioButton;
    StopOnSE: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Label6: TLabel;
    Label7: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure RLTTabSetClick(Sender: TObject);
    procedure ChDirection( Sender : TObject);
    procedure ChLStyle(Sender: TObject);
    procedure RunButton1Click(Sender: TObject);
    procedure FontButtonClick(Sender: TObject);
    procedure RLTextOnKD(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RLTextOnEx(Sender: TObject);
    procedure StepsSEOnEx(Sender: TObject);
    procedure SpeedSEOnEx(Sender: TObject);
    procedure RLEventsRBClick(Sender: TObject);

  private
    ColorCounter : integer;

    PrFontColor, PrColor : TColor;
    RLOldText : string;

    procedure ViewMainLabel; { Centers main label}
    { Event procedures}
    procedure ChangeColor(Sender: TObject);
    procedure CycleRuning(Sender: TObject);
    procedure ReverseRuning(Sender: TObject);
    procedure StopOnStep(Sender: TObject);

  public
    { Public declarations }
  end;

var
  RLDemo: TRLDemo;

implementation

{$R *.DFM}


procedure TRLDemo.FormCreate(Sender: TObject);
begin

  RLTTabSet.Tabs := RLTNotebook.Pages;
  RLTTabSet.TabIndex := 0;
  RLTNotebook.PageIndex := RLTTabSet.TabIndex;
  { Load main icon into image}
  IconIm.Picture.Icon.Handle := LoadIcon( HInstance, 'MAINICON');
  StepsSE.Value := RLabelMain.Steps;
  SpeedSE.Value := RLabelMain.Speed;

  ColorCounter := 0;
  with RLabelMain do begin
    PrColor := Color;
    PrFontColor := RFont.Color;
    RText := 'Running label';
    RLOldText := RText;
    RLText.Text := RText;
    end;

  ViewMainLabel;
end;

procedure TRLDemo.RLTTabSetClick(Sender: TObject);
begin
  RLTNotebook.PageIndex := RLTTabSet.TabIndex;
  if RLTNotebook.PageIndex <> 2 then RLabelMain.RText := RLOldText;
  case RLTNotebook.PageIndex of
    0 : begin { Properties page}
      with RLabelMain do begin
        {Color := PrColor;}
        RFont.Color := PrFontColor;
        end;
      ViewMainLabel;
      end;
    1 : begin { Event page}
      StopOnSE.MaxValue := RLabelMain.Steps;
      StopOnSE.Value := RLabelMain.Steps DIV 2 + RLabelMain.Steps DIV 4;
      end;
    2 : begin { About page}
      RLOldText := RLabelMain.RText;
      RLabelMain.RText := 'Thank you!';
      RLabelMain.RLStart( 0);
      end;
    end;
end;

procedure TRLDemo.ViewMainLabel;
var
  MidStep : integer;
begin
  with RLabelMain do begin
    RLStop;
    MidStep := Steps DIV 2;
    RLStart( MidStep);
    RLStop;
    end;
end;

procedure TRLDemo.ChDirection( Sender : TObject);
begin
  with RLabelMain do begin
    RLStop;
    case TRadioButton(Sender).Tag of
      101 : Direction := rdStatic;
      102 : Direction := rdRightToLeft;
      103 : Direction := rdLeftToRight;
      104 : Direction := rdTopToBottom;
      105 : Direction := rdBottomToTop;
      end;
    end;
  ViewMainLabel;
end;


procedure TRLDemo.ChLStyle(Sender: TObject);
begin
  with RLabelMain do begin
    RLStop;
    case TRadioButton(Sender).Tag of
      201 : LabelStyle := rlNormal;
      202 : LabelStyle := rlLowered;
      203 : LabelStyle := rlRaised;
      end;
    end;
  ViewMainLabel;
end;

procedure TRLDemo.RunButton1Click(Sender: TObject);
begin
  RLabelMain.RLStart( 0);
end;

procedure TRLDemo.FontButtonClick(Sender: TObject);
begin
  with FontDialog1 do
    if Execute then begin
      RLabelMain.RFont := Font;
      PrFontColor := Font.Color;
      ViewMainLabel;
      end;
end;

procedure TRLDemo.RLTextOnKD(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    RLabelMain.RLStop;
    RLabelMain.RText := RLText.Text;
    RLOldText := RLText.Text;
    ViewMainLabel;
    end;
end;

procedure TRLDemo.RLTextOnEx(Sender: TObject);
begin
    RLabelMain.RLStop;
    RLabelMain.RText := RLText.Text;
    RLOldText := RLText.Text;
    ViewMainLabel;
end;

procedure TRLDemo.StepsSEOnEx(Sender: TObject);
begin
  RLabelMain.RLStop;
  RLabelMain.Steps := StepsSE.Value;
  ViewMainLabel;
end;

procedure TRLDemo.SpeedSEOnEx(Sender: TObject);
begin
  RLabelMain.RLStop;
  RLabelMain.Speed := SpeedSE.Value;
  ViewMainLabel;
end;

procedure TRLDemo.RLEventsRBClick(Sender: TObject);
begin
  with RLabelMain do begin
    RLStop;
    case TRadioButton(Sender).Tag of
      301 : OnBegin := NIL;
      311 : OnEnd   := NIL;
      302 : OnBegin := ChangeColor;
      303 : OnEnd   := CycleRuning;
      304 : OnEnd   := ReverseRuning;
      321 : begin
        OnStep  := NIL;
        EvCycle.Enabled := true;
        EvReverse.Enabled := true;
        end;
      305 : begin
        OnStep  := StopOnStep;
        EvCycle.Enabled := false;
        EvReverse.Enabled := false;
        end;
      end;
    RLStart( 0);
    end;
end;

procedure TRLDemo.ChangeColor(Sender: TObject);
begin
  if ColorCounter > 6 then ColorCounter := 0
  else                     inc( ColorCounter);
  with RLabelMain do begin
    RFont.Color := TxtColors[ ColorCounter];
    {Color       := BkgColors[ ColorCounter];}
    end;
end;

procedure TRLDemo.CycleRuning(Sender: TObject);
begin
  RLabelMain.RLStart( -1);
end;

procedure TRLDemo.ReverseRuning(Sender: TObject);
begin
  RLabelMain.RLReverse;
  RLabelMain.RLStart( 0);
end;

procedure TRLDemo.StopOnStep(Sender: TObject);
begin
  with RLabelMain do begin
    if GetCurrentStep >= StopOnSE.Value then begin
      {RLStop;}
      RLabelMain.RLReverse;
      if EvChColor.Checked then ChangeColor( Self);
      end;
    end;
end;

end.
