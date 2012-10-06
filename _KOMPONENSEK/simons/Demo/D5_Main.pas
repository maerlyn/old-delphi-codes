unit D5_Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RackCtls, StdCtrls, Spin, ExtCtrls, ComCtrls, ColorGrd, Buttons, SRWave,
  EnhEdit, OvalBtn, SRCal, Grids, SRColBtn, SRGrad, SRLabel, SRDlgs,
  SRChkBox;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TSLEDDisplay: TTabSheet;
    ScrewPanel1: TScrewPanel;
    LEDDisplay1: TLEDDisplay;
    LEDDisplay2: TLEDDisplay;
    Label1: TLabel;
    EditValue: TEdit;
    Label2: TLabel;
    SpinEditNumDigits: TSpinEdit;
    Label3: TLabel;
    SpinEditFractionDigits: TSpinEdit;
    CBLeadingZeros: TCheckBox;
    Label4: TLabel;
    ComboBoxSS: TComboBox;
    Label5: TLabel;
    ComboBoxBevel: TComboBox;
    Label6: TLabel;
    ComboBoxDecSeperator: TComboBox;
    TSLEDButton: TTabSheet;
    LEDButton1: TLEDButton;
    ButtonPanel2: TButtonPanel;
    ButtonPanel3: TButtonPanel;
    LEDButton2: TLEDButton;
    ButtonPanel4: TButtonPanel;
    ButtonPanel1: TButtonPanel;
    TestButton: TLEDButton;
    ColorGrid: TColorGrid;
    Label7: TLabel;
    ComboBoxBD: TComboBox;
    Label8: TLabel;
    CBSwitching: TCheckBox;
    Label9: TLabel;
    ComboBoxTP: TComboBox;
    CBBeveled: TCheckBox;
    CBShowLED: TCheckBox;
    LabelClick: TLabel;
    Bevel1: TBevel;
    TSLEDMeter: TTabSheet;
    LabelPosition: TLabel;
    Label11: TLabel;
    TBPosition: TTrackBar;
    LEDMeter1: TLEDMeter;
    RGDirection: TRadioGroup;
    LEDMeter2: TLEDMeter;
    SpinEditStartColor2: TSpinEdit;
    Label10: TLabel;
    Label12: TLabel;
    SpinEditStartColor3: TSpinEdit;
    TSEnhancedEdit: TTabSheet;
    CBSingleLED: TCheckBox;
    TSOvalButton: TTabSheet;
    TSSRGradient: TTabSheet;
    TSSRCalendar: TTabSheet;
    TSSRClock: TTabSheet;
    SRWavePlayer: TSRWavePlayer;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    EnhEdit: TEnhancedEdit;
    RGEnhAlignment: TRadioGroup;
    SpinEnhDigits: TSpinEdit;
    EditEnhValueInt: TEdit;
    EditEnhValue: TEdit;
    CBEnhEnabled: TCheckBox;
    CBEnhGrayDisabled: TCheckBox;
    ComboEnhFormat: TComboBox;
    Hintergrund: TImage;
    OvalButton1: TOvalButton;
    OvalButton2: TOvalButton;
    OvalButton3: TOvalButton;
    OvalButton4: TOvalButton;
    OvalButton5: TOvalButton;
    OvalButton6: TOvalButton;
    OvalButton7: TOvalButton;
    OvalButton8: TOvalButton;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Uhr: TSRClock;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    EditZeit: TEdit;
    ComboClockStyle: TComboBox;
    ComboClockKind: TComboBox;
    CBAutoUpdate: TCheckBox;
    SBStart: TSpeedButton;
    SBStop: TSpeedButton;
    SBReset: TSpeedButton;
    SRGradient1: TSRGradient;
    SRGradient2: TSRGradient;
    SRGradient3: TSRGradient;
    SRGradient4: TSRGradient;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    LblDatum: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    LblFeiertag: TLabel;
    LblSternzeichen: TLabel;
    Label32: TLabel;
    LblTagImJahr: TLabel;
    LblWocheImJahr: TLabel;
    LblMonat: TLabel;
    LblJahr: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    ComboBundesland: TComboBox;
    SpinMonat: TSpinButton;
    SpinJahr: TSpinButton;
    CBAstroDaten: TCheckBox;
    ComboStartTag: TComboBox;
    Kalender: TSRCalendar;
    CBDeleteMarks: TCheckBox;
    CBShowMarks: TCheckBox;
    CBShowHolidays: TCheckBox;
    ComboAstroData: TComboBox;
    TSExtDialogs: TTabSheet;
    SBCSVOpenDlg: TSpeedButton;
    SBCSVSaveDlg: TSpeedButton;
    SBExtSaveDlg: TSpeedButton;
    SBExtOpenDlg: TSpeedButton;
    SBSliderSaveDlg: TSpeedButton;
    SBSliderOpenDlg: TSpeedButton;
    ExtOpenDlg: TExtOpenDialog;
    ExtSaveDlg: TExtSaveDialog;
    CSVSaveDlg: TCSVSaveDialog;
    SliderOpenDlg: TSliderOpenDialog;
    SliderSaveDlg: TSliderSaveDialog;
    CBDlgShowHelp: TCheckBox;
    CBDlgHideReadOnly: TCheckBox;
    Label37: TLabel;
    SpinLEDDisplayContrast: TSpinEdit;
    Label38: TLabel;
    SpinLEDMeterContrast: TSpinEdit;
    Label39: TLabel;
    SpinClockContrast: TSpinEdit;
    CSVOpenDlg: TCSVOpenDialog;
    TSSRWavePlayer: TTabSheet;
    SBWaveStart: TSpeedButton;
    SBWaveStop: TSpeedButton;
    CBWaveAsync: TCheckBox;
    CBWaveLoop: TCheckBox;
    TSSRColorButton: TTabSheet;
    SRColorButton2: TSRColorButton;
    SRColorButton1: TSRColorButton;
    SpinContrastShadow: TSpinEdit;
    SpinContrastHighlight: TSpinEdit;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    SpinBevelWidth: TSpinEdit;
    TSSRLabel: TTabSheet;
    SRLabel1: TSRLabel;
    Label35: TLabel;
    Label36: TLabel;
    CBLblShowHighlight: TCheckBox;
    SpinLblHighlightOffset: TSpinEdit;
    CBLblShowShadow: TCheckBox;
    SpinLblShadowOffset: TSpinEdit;
    RGHighlightPos: TRadioGroup;
    SRLabel2: TSRLabel;
    Bevel2: TBevel;
    CBLblLinkActive: TCheckBox;
    CBLblHighlightOnEnter: TCheckBox;
    CBLblUnderlineOnEnter: TCheckBox;
    CBLblUnderlined: TCheckBox;
    CBLblEnabled: TCheckBox;
    Bevel3: TBevel;
    SRLabel3: TSRLabel;
    RGLblLayout: TRadioGroup;
    RGLblAlignment: TRadioGroup;
    TSInfo: TTabSheet;
    LblTitel: TSRLabel;
    Bevel4: TBevel;
    LblAutor: TSRLabel;
    LbleMail: TSRLabel;
    LblInternet: TSRLabel;
    LblInfo: TSRLabel;
    Label43: TLabel;
    ComboDrawStyle: TComboBox;
    SRColorButton3: TSRColorButton;
    SRColorButton4: TSRColorButton;
    Label44: TLabel;
    Label45: TLabel;
    PanelVersion: TPanel;
    Bevel5: TBevel;
    Shape: TShape;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    SRCheckBox: TSRCheckBox;
    CBAllowGrayed: TCheckBox;
    ComboStyle: TComboBox;
    SpinCheckSize: TSpinEdit;
    CBAutoSize: TCheckBox;
    CBTransparent: TCheckBox;
    SpinSpacing: TSpinEdit;
    CBWordWrap: TCheckBox;
    CBEnhAcceptChars: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure EditValueChange(Sender: TObject);
    procedure SpinEditNumDigitsChange(Sender: TObject);
    procedure SpinEditFractionDigitsChange(Sender: TObject);
    procedure ComboBoxSSChange(Sender: TObject);
    procedure ComboBoxBevelChange(Sender: TObject);
    procedure CBLeadingZerosClick(Sender: TObject);
    procedure ComboBoxDecSeperatorChange(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure ColorGridChange(Sender: TObject);
    procedure ComboBoxBDChange(Sender: TObject);
    procedure ComboBoxTPChange(Sender: TObject);
    procedure CBShowLEDClick(Sender: TObject);
    procedure CBBeveledClick(Sender: TObject);
    procedure CBSwitchingClick(Sender: TObject);
    procedure LEDMeter1Change(Sender: TObject);
    procedure TBPositionChange(Sender: TObject);
    procedure RGDirectionClick(Sender: TObject);
    procedure SpinEditStartColor2Change(Sender: TObject);
    procedure SpinEditStartColor3Change(Sender: TObject);
    procedure CBSingleLEDClick(Sender: TObject);
    procedure SBWaveStartClick(Sender: TObject);
    procedure SBWaveStopClick(Sender: TObject);
    procedure SRWavePlayerAfterPlay(Sender: TObject);
    procedure CBWaveAsyncClick(Sender: TObject);
    procedure CBWaveLoopClick(Sender: TObject);
    procedure SRWavePlayerBeforePlay(Sender: TObject);
    procedure EnhEditKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CBEnhEnabledClick(Sender: TObject);
    procedure CBEnhGrayDisabledClick(Sender: TObject);
    procedure RGEnhAlignmentClick(Sender: TObject);
    procedure EditEnhValueKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditEnhValueIntKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEnhDigitsChange(Sender: TObject);
    procedure ComboEnhFormatChange(Sender: TObject);
    procedure OvalButton5Click(Sender: TObject);
    procedure EditZeitChange(Sender: TObject);
    procedure CBAutoUpdateClick(Sender: TObject);
    procedure ComboClockStyleChange(Sender: TObject);
    procedure ComboClockKindChange(Sender: TObject);
    procedure SBStartClick(Sender: TObject);
    procedure SBStopClick(Sender: TObject);
    procedure SBResetClick(Sender: TObject);
    procedure SpinMonatDownClick(Sender: TObject);
    procedure SpinMonatUpClick(Sender: TObject);
    procedure SpinJahrDownClick(Sender: TObject);
    procedure SpinJahrUpClick(Sender: TObject);
    procedure ComboBundeslandChange(Sender: TObject);
    procedure ComboStartTagChange(Sender: TObject);
    procedure CBShowHolidaysClick(Sender: TObject);
    procedure CBShowMarksClick(Sender: TObject);
    procedure CBDeleteMarksClick(Sender: TObject);
    procedure CBAstroDatenClick(Sender: TObject);
    procedure KalenderDblClick(Sender: TObject);
    procedure KalenderChange(Sender: TObject);
    procedure CBLblShowHighlightClick(Sender: TObject);
    procedure CBLblShowShadowClick(Sender: TObject);
    procedure SpinLblHighlightOffsetChange(Sender: TObject);
    procedure SpinLblShadowOffsetChange(Sender: TObject);
    procedure RGHighlightPosClick(Sender: TObject);
    procedure SBCSVOpenDlgClick(Sender: TObject);
    procedure SBCSVSaveDlgClick(Sender: TObject);
    procedure SBExtOpenDlgClick(Sender: TObject);
    procedure SBExtSaveDlgClick(Sender: TObject);
    procedure SBSliderOpenDlgClick(Sender: TObject);
    procedure SBSliderSaveDlgClick(Sender: TObject);
    procedure CBDlgShowHelpClick(Sender: TObject);
    procedure CBDlgHideReadOnlyClick(Sender: TObject);
    procedure ScrBLEDContrastChange(Sender: TObject);
    procedure SpinLEDDisplayContrastChange(Sender: TObject);
    procedure SpinLEDMeterContrastChange(Sender: TObject);
    procedure SpinClockContrastChange(Sender: TObject);
    procedure SpinContrastHighlightChange(Sender: TObject);
    procedure SpinContrastShadowChange(Sender: TObject);
    procedure SpinBevelWidthChange(Sender: TObject);
    procedure CBLblLinkActiveClick(Sender: TObject);
    procedure CBLblHighlightOnEnterClick(Sender: TObject);
    procedure CBLblUnderlineOnEnterClick(Sender: TObject);
    procedure CBLblUnderlinedClick(Sender: TObject);
    procedure CBLblEnabledClick(Sender: TObject);
    procedure RGLblAlignmentClick(Sender: TObject);
    procedure RGLblLayoutClick(Sender: TObject);
    procedure ComboDrawStyleChange(Sender: TObject);
    procedure SRColorButton3Click(Sender: TObject);
    procedure CBAllowGrayedClick(Sender: TObject);
    procedure CBTransparentClick(Sender: TObject);
    procedure CBAutoSizeClick(Sender: TObject);
    procedure CBWordWrapClick(Sender: TObject);
    procedure ComboStyleChange(Sender: TObject);
    procedure SpinCheckSizeChange(Sender: TObject);
    procedure SpinSpacingChange(Sender: TObject);
    procedure SRCheckBoxClick(Sender: TObject);
    procedure CBEnhAcceptCharsClick(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure SetCheckBoxSize;
  public
    { Public-Deklarationen }
  end;

var
  MainForm : TMainForm;

implementation

{$R *.DFM}

function GetPackageVersionNr:string;
var AText : TStringList;
    ALine,
    APath : string;
    i,P   : integer;
begin
  Result:='Unbekannt';
  AText:=TStringList.Create;
  try
    APath:=ExtractFileDir(Application.ExeName);
    P:=LastDelimiter('\', APath);
    if P>0 then begin
      delete(APath, P+1, length(APath)-P);
      APath:=APath+'Liesmich.txt';
      if FileExists(APath) then begin
        AText.LoadFromFile(APath);
        i:=0;
        while (i<AText.Count) and (Pos('Version', AText[i])=0) do
          inc(i);
        if Pos('Version', AText[i])>0 then begin
          ALine:=AText[i];
          P:=Pos(':', ALine);
          if P>0 then
            delete(ALine, 1, P);
          ALine:=Trim(ALine);
          if ALine<>'' then
            Result:=ALine;
        end;
      end;
    end;
  finally
    AText.Free;
  end;
end;

procedure TMainForm.SetCheckBoxSize;
begin
  if not CBAutoSize.Checked then begin
    if CBWordWrap.Checked then
      SRCheckBox.SetBounds(112, 8, 120, 70)
    else
      SRCheckBox.SetBounds(112, 8, 240, 70);
  end
  else begin
    SRCheckBox.AutoSize:=false;
    SRCheckBox.Width:=200;
    SRCheckBox.AutoSize:=true;
  end;
end;

{ TEnhancedEdit }
procedure TMainForm.EnhEditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EditEnhValue.Text:=FloatToStr(EnhEdit.Value);
    EditEnhValueInt.Text:=IntToStr(EnhEdit.ValueInt);
  except
  end;
end;

procedure TMainForm.CBEnhEnabledClick(Sender: TObject);
begin
  EnhEdit.Enabled:=CBEnhEnabled.Checked;
end;

procedure TMainForm.CBEnhGrayDisabledClick(Sender: TObject);
begin
  EnhEdit.GrayDisabled:=CBEnhGrayDisabled.Checked;
end;

procedure TMainForm.RGEnhAlignmentClick(Sender: TObject);
begin
  case RGEnhAlignment.ItemIndex of
    0 : EnhEdit.Alignment:=taLeftJustify;
    1 : EnhEdit.Alignment:=taCenter;
    2 : EnhEdit.Alignment:=taRightJustify;
  end;
end;

procedure TMainForm.EditEnhValueKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EnhEdit.Value:=StrToFloat(EditEnhValue.Text);
    EditEnhValueInt.Text:=IntToStr(EnhEdit.ValueInt);
  except
  end;
end;

procedure TMainForm.EditEnhValueIntKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  try
    EnhEdit.ValueInt:=StrToInt(EditEnhValueInt.Text);
    EditEnhValue.Text:=FloatToStr(EnhEdit.Value);
  except
  end;
end;

procedure TMainForm.SpinEnhDigitsChange(Sender: TObject);
begin
  EnhEdit.Digits:=SpinEnhDigits.Value;
end;

procedure TMainForm.CBEnhAcceptCharsClick(Sender: TObject);
begin
  EnhEdit.AcceptChars:=CBEnhAcceptChars.Checked;
end;

procedure TMainForm.ComboEnhFormatChange(Sender: TObject);
begin
  case ComboEnhFormat.ItemIndex of
    0 : EnhEdit.Format:=ffCurrency;
    1 : EnhEdit.Format:=ffExponent;
    2 : EnhEdit.Format:=ffFixed;
    3 : EnhEdit.Format:=ffGeneral;
    4 : EnhEdit.Format:=ffNumber;
  end;
end;

{ TSRLabel }
procedure TMainForm.CBLblShowHighlightClick(Sender: TObject);
begin
  SRLabel1.ShowHighlight:=CBLblShowHighlight.Checked;
end;

procedure TMainForm.CBLblShowShadowClick(Sender: TObject);
begin
  SRLabel1.ShowShadow:=CBLblShowShadow.Checked;
end;

procedure TMainForm.SpinLblHighlightOffsetChange(Sender: TObject);
begin
  SRLabel1.HighlightOffset:=SpinLblHighlightOffset.Value;
end;

procedure TMainForm.SpinLblShadowOffsetChange(Sender: TObject);
begin
  SRLabel1.ShadowOffset:=SpinLblShadowOffset.Value;
end;

procedure TMainForm.RGHighlightPosClick(Sender: TObject);
begin
  if RGHighlightPos.ItemIndex=0 then
    SRLabel1.HighlightPos:=hpTopLeft
  else
    SRLabel1.HighlightPos:=hpTopRight;
end;

procedure TMainForm.CBLblEnabledClick(Sender: TObject);
begin
  SRLabel1.Enabled:=CBLblEnabled.Checked;
end;

procedure TMainForm.CBLblLinkActiveClick(Sender: TObject);
begin
  SRLabel2.LinkActive:=CBLblLinkActive.Checked;
end;

procedure TMainForm.CBLblHighlightOnEnterClick(Sender: TObject);
begin
  SRLabel2.HighlightOnEnter:=CBLblHighlightOnEnter.Checked;
end;

procedure TMainForm.CBLblUnderlineOnEnterClick(Sender: TObject);
begin
  SRLabel2.UnderlineOnEnter:=CBLblUnderlineOnEnter.Checked;
end;

procedure TMainForm.CBLblUnderlinedClick(Sender: TObject);
begin
  if CBLblUnderlined.Checked then
    SRLabel2.Font.Style:=[fsBold,fsUnderline]
  else
    SRLabel2.Font.Style:=[fsBold];
end;

procedure TMainForm.RGLblAlignmentClick(Sender: TObject);
begin
  case RGLblAlignment.ItemIndex of
    0 : SRLabel3.Alignment:=taLeftJustify;
    1 : SRLabel3.Alignment:=taCenter;
    2 : SRLabel3.Alignment:=taRightJustify;
  end;
end;

procedure TMainForm.RGLblLayoutClick(Sender: TObject);
begin
  case RGLblLayout.ItemIndex of
    0 : SRLabel3.Layout:=tlTop;
    1 : SRLabel3.Layout:=tlCenter;
    2 : SRLabel3.Layout:=tlBottom;
  end;
end;

{ TLEDButton }
procedure TMainForm.TestButtonClick(Sender: TObject);
begin
  LabelClick.Visible:=not LabelClick.Visible;
end;

procedure TMainForm.ColorGridChange(Sender: TObject);
begin
  TestButton.Color:=ColorGrid.ForegroundColor;
end;

procedure TMainForm.ComboBoxBDChange(Sender: TObject);
begin
  case ComboBoxBD.ItemIndex of
    0 : TestButton.ButtonDirection:=bdBottomUp;
    1 : TestButton.ButtonDirection:=bdLeftUp;
    2 : TestButton.ButtonDirection:=bdNone;
    3 : TestButton.ButtonDirection:=bdRightUp;
    4 : TestButton.ButtonDirection:=bdTopUp;
  end;
end;

procedure TMainForm.ComboBoxTPChange(Sender: TObject);
begin
  case ComboBoxTP.ItemIndex of
    0 : TestButton.TextPosition:=tpAbove;
    1 : TestButton.TextPosition:=tpBelow;
    2 : TestButton.TextPosition:=tpNone;
    3 : TestButton.TextPosition:=tpOnButton;
  end;
end;

procedure TMainForm.CBShowLEDClick(Sender: TObject);
begin
  TestButton.ShowLED:=CBShowLED.Checked;
end;

procedure TMainForm.CBBeveledClick(Sender: TObject);
begin
  TestButton.Beveled:=CBBeveled.Checked;
end;

procedure TMainForm.CBSwitchingClick(Sender: TObject);
begin
  TestButton.Switching:=CBSwitching.Checked;
end;

{ TLEDDisplay }
procedure TMainForm.EditValueChange(Sender: TObject);
begin
  try
    LEDDisplay1.Value:=StrToFloat(EditValue.Text);
    LEDDisplay2.Value:=StrToFloat(EditValue.Text);
  except
  end;
end;

procedure TMainForm.SpinEditNumDigitsChange(Sender: TObject);
begin
  LEDDisplay1.NumDigits:=SpinEditNumDigits.Value;
  LEDDisplay2.NumDigits:=SpinEditNumDigits.Value;
end;

procedure TMainForm.SpinEditFractionDigitsChange(Sender: TObject);
begin
  LEDDisplay1.FractionDigits:=SpinEditFractionDigits.Value;
  LEDDisplay2.FractionDigits:=SpinEditFractionDigits.Value;
end;

procedure TMainForm.CBLeadingZerosClick(Sender: TObject);
begin
  LEDDisplay1.LeadingZeros:=CBLeadingZeros.Checked;
  LEDDisplay2.LeadingZeros:=CBLeadingZeros.Checked;
end;

procedure TMainForm.ComboBoxSSChange(Sender: TObject);
begin
  if ComboBoxSS.ItemIndex=0 then begin
    LEDDisplay1.SegmentStyle:=ssBeveled;
    LEDDisplay2.SegmentStyle:=ssBeveled;
  end
  else begin
    LEDDisplay1.SegmentStyle:=ssRectangular;
    LEDDisplay2.SegmentStyle:=ssRectangular;
  end;
end;

procedure TMainForm.ComboBoxBevelChange(Sender: TObject);
begin
  case ComboBoxBevel.ItemIndex of
    0 : begin
          LEDDisplay1.BevelStyle:=bvLowered;
          LEDDisplay2.BevelStyle:=bvLowered;
        end;
    1 : begin
          LEDDisplay1.BevelStyle:=bvNone;
          LEDDisplay2.BevelStyle:=bvNone;
        end;
    2 : begin
          LEDDisplay1.BevelStyle:=bvRaised;
          LEDDisplay2.BevelStyle:=bvRaised;
        end;
  end;
end;

procedure TMainForm.ComboBoxDecSeperatorChange(Sender: TObject);
begin
  case ComboBoxDecSeperator.ItemIndex of
    0 : begin
          LEDDisplay1.DecSeperator:=dsComma;
          LEDDisplay2.DecSeperator:=dsComma;
        end;
    1 : begin
          LEDDisplay1.DecSeperator:=dsDoublePoint;
          LEDDisplay2.DecSeperator:=dsDoublePoint;
        end;
    2 : begin
          LEDDisplay1.DecSeperator:=dsMinus;
          LEDDisplay2.DecSeperator:=dsMinus;
        end;
    3 : begin
          LEDDisplay1.DecSeperator:=dsPoint;
          LEDDisplay2.DecSeperator:=dsPoint;
        end;
  end;
end;

procedure TMainForm.SpinLEDDisplayContrastChange(Sender: TObject);
begin
  LEDDisplay1.LEDContrast:=SpinLEDDisplayContrast.Value;
  LEDDisplay2.LEDContrast:=SpinLEDDisplayContrast.Value;
end;

procedure TMainForm.ScrBLEDContrastChange(Sender: TObject);
begin
end;

{ TLEDMeter }
procedure TMainForm.LEDMeter1Change(Sender: TObject);
begin
  LabelPosition.Caption:=IntToStr(LEDMeter1.Position);
end;

procedure TMainForm.TBPositionChange(Sender: TObject);
begin
  LEDMeter1.Position:=TBPosition.Position;
  LEDMeter2.Position:=TBPosition.Position;
end;

procedure TMainForm.RGDirectionClick(Sender: TObject);
begin
  if RGDirection.ItemIndex=0 then begin
    LEDMeter1.Direction:=mdRight;
    LEDMeter2.Direction:=mdUp;
  end
  else begin
    LEDMeter1.Direction:=mdLeft;
    LEDMeter2.Direction:=mdDown;
  end;
end;

procedure TMainForm.CBSingleLEDClick(Sender: TObject);
begin
  LEDMeter1.SingleLED:=CBSingleLED.Checked;
  LEDMeter2.SingleLED:=CBSingleLED.Checked;
end;

procedure TMainForm.SpinLEDMeterContrastChange(Sender: TObject);
begin
  LEDMeter1.LEDContrast:=SpinLEDMeterContrast.Value;
  LEDMeter2.LEDContrast:=SpinLEDMeterContrast.Value;
end;

procedure TMainForm.SpinEditStartColor2Change(Sender: TObject);
begin
  LEDMeter1.StartColor2:=SpinEditStartColor2.Value;
  LEDMeter2.StartColor2:=SpinEditStartColor2.Value;
  SpinEditStartColor2.Value:=LEDMeter1.StartColor2;
end;

procedure TMainForm.SpinEditStartColor3Change(Sender: TObject);
begin
  LEDMeter1.StartColor3:=SpinEditStartColor3.Value;
  LEDMeter2.StartColor3:=SpinEditStartColor3.Value;
  SpinEditStartColor3.Value:=LEDMeter1.StartColor3;
end;

{ TSRWavePlayer }
procedure TMainForm.SBWaveStartClick(Sender: TObject);
begin
  SRWavePlayer.Play;
end;

procedure TMainForm.SBWaveStopClick(Sender: TObject);
begin
  SRWavePlayer.Stop;
  CBWaveLoop.Enabled:=true;
  CBWaveAsync.Enabled:=true;
end;

procedure TMainForm.SRWavePlayerAfterPlay(Sender: TObject);
begin
  SBWaveStart.Down:=false;
  SBWaveStop.Down:=true;
  SBWaveStop.Enabled:=true;
  if not SRWavePlayer.Loop then begin
    CBWaveAsync.Enabled:=true;
    CBWaveLoop.Enabled:=true;
  end;
end;

procedure TMainForm.SRWavePlayerBeforePlay(Sender: TObject);
begin
  CBWaveAsync.Enabled:=false;
  CBWaveLoop.Enabled:=false;
  SBWaveStop.Enabled:=false;
end;

procedure TMainForm.CBWaveAsyncClick(Sender: TObject);
begin
  CBWaveLoop.Enabled:=CBWaveAsync.Checked;
  SRWavePlayer.Async:=CBWaveAsync.Checked;
end;

procedure TMainForm.CBWaveLoopClick(Sender: TObject);
begin
  SRWavePlayer.Loop:=CBWaveLoop.Checked;
end;

{ TSRCheckBox }
procedure TMainForm.SRCheckBoxClick(Sender: TObject);
begin
  MessageBeep(0);
end;

procedure TMainForm.CBAllowGrayedClick(Sender: TObject);
begin
  SRCheckBox.AllowGrayed:=CBAllowGrayed.Checked;
end;

procedure TMainForm.CBTransparentClick(Sender: TObject);
begin
  SRCheckBox.Transparent:=CBTransparent.Checked;
  Shape.Visible:=CBTransparent.Checked;
end;

procedure TMainForm.CBAutoSizeClick(Sender: TObject);
begin
  SRCheckBox.AutoSize:=CBAutoSize.Checked;
  SetCheckBoxSize;
end;

procedure TMainForm.CBWordWrapClick(Sender: TObject);
begin
  SRCheckBox.WordWrap:=CBWordWrap.Checked;
  SetCheckBoxSize;
end;

procedure TMainForm.ComboStyleChange(Sender: TObject);
begin
  SRCheckBox.Style:=TCheckStyle(ComboStyle.ItemIndex);
end;

procedure TMainForm.SpinCheckSizeChange(Sender: TObject);
begin
  SRCheckBox.CheckSize:=SpinCheckSize.Value;
end;

procedure TMainForm.SpinSpacingChange(Sender: TObject);
begin
  SRCheckBox.Spacing:=SpinSpacing.Value;
end;

{ TOvalButton }
procedure TMainForm.OvalButton5Click(Sender: TObject);
begin
  MessageBeep(0);
end;

{ TSRColorButton }
procedure TMainForm.SpinContrastHighlightChange(Sender: TObject);
begin
  SRColorButton1.ContrastHighlight:=SpinContrastHighlight.Value;
  SRColorButton2.ContrastHighlight:=SpinContrastHighlight.Value;
end;

procedure TMainForm.SpinContrastShadowChange(Sender: TObject);
begin
  SRColorButton1.ContrastShadow:=SpinContrastShadow.Value;
  SRColorButton2.ContrastShadow:=SpinContrastShadow.Value;
end;

procedure TMainForm.SpinBevelWidthChange(Sender: TObject);
begin
  SRColorButton1.BevelWidth:=SpinBevelWidth.Value;
  SRColorButton2.BevelWidth:=SpinBevelWidth.Value;
end;

procedure TMainForm.SRColorButton3Click(Sender: TObject);
begin
  MessageBeep(0);
end;

{ TSRCalendar }
procedure TMainForm.SpinMonatDownClick(Sender: TObject);
begin
  Kalender.PrevMonth;
end;

procedure TMainForm.SpinMonatUpClick(Sender: TObject);
begin
  Kalender.NextMonth;
end;

procedure TMainForm.SpinJahrDownClick(Sender: TObject);
begin
  Kalender.PrevYear;
end;

procedure TMainForm.SpinJahrUpClick(Sender: TObject);
begin
  Kalender.NextYear;
end;

procedure TMainForm.ComboBundeslandChange(Sender: TObject);
begin
  case ComboBundesland.ItemIndex of
    0 : Kalender.Bundesland:=Baden_Wuerttemberg;
    1 : Kalender.Bundesland:=Bayern;
    2 : Kalender.Bundesland:=Berlin;
    3 : Kalender.Bundesland:=Brandenburg;
    4 : Kalender.Bundesland:=Bremen;
    5 : Kalender.Bundesland:=Hamburg;
    6 : Kalender.Bundesland:=Hessen;
    7 : Kalender.Bundesland:=Mecklenburg_Vorpommern;
    8 : Kalender.Bundesland:=Niedersachsen;
    9 : Kalender.Bundesland:=Nordrhein_Westfalen;
   10 : Kalender.Bundesland:=Rheinland_Pfalz;
   11 : Kalender.Bundesland:=Saarland;
   12 : Kalender.Bundesland:=Sachsen;
   13 : Kalender.Bundesland:=Sachsen_Anhalt;
   14 : Kalender.Bundesland:=Schleswig_Holstein;
   15 : Kalender.Bundesland:=Thueringen;
  end;
end;

procedure TMainForm.ComboStartTagChange(Sender: TObject);
begin
  Kalender.StartOfWeek:=ComboStartTag.ItemIndex;
end;

procedure TMainForm.ComboDrawStyleChange(Sender: TObject);
begin
  Kalender.DrawStyle:=TCalendarDrawStyle(ComboDrawStyle.ItemIndex);
end;

procedure TMainForm.CBShowHolidaysClick(Sender: TObject);
begin
  Kalender.ShowHolidays:=CBShowHolidays.Checked;
  LblFeiertag.Caption:=Kalender.Holiday;
end;

procedure TMainForm.CBShowMarksClick(Sender: TObject);
begin
  Kalender.ShowMarks:=CBShowMarks.Checked;
end;

procedure TMainForm.CBDeleteMarksClick(Sender: TObject);
begin
  Kalender.AutoDeleteMarks:=CBDeleteMarks.Checked;
end;

procedure TMainForm.CBAstroDatenClick(Sender: TObject);
begin
  Kalender.AstronomicalData:=CBAstroDaten.Checked;
  ComboAstroData.Visible:=CBAstroDaten.Checked;
end;

procedure TMainForm.KalenderDblClick(Sender: TObject);
begin
  Kalender.Marked[Kalender.Day]:=not Kalender.Marked[Kalender.Day];
end;

procedure TMainForm.KalenderChange(Sender: TObject);
begin
  LblMonat.Caption:=LongMonthNames[Kalender.Month];
  LblJahr.Caption:=IntToStr(Kalender.Year);
  LblDatum.Caption:=FormatDateTime('dddd, dd. mmmm yyyy',Kalender.Date);
  LblTagImJahr.Caption:=IntToStr(Kalender.DayOfYear)+'. Tag im Jahr';
  LblWocheImJahr.Caption:=IntToStr(Kalender.WeekOfYear)+'. Woche im Jahr';
  LblFeiertag.Caption:=Kalender.Holiday;
  LblSternzeichen.Caption:='Sternzeichen: '+Kalender.Sternzeichen;
  with ComboAstroData.Items do begin
    Clear;
    Add('Mondaufgang: '+TimeToStr(Kalender.MoonRise)+' h');
    Add('Mondhöchststand: '+TimeToStr(Kalender.MoonTransit)+' h');
    Add('Monduntergang: '+TimeToStr(Kalender.MoonSet)+' h');
    Add('Entfernung zum Mond: '+FloatToStrF(Kalender.MoonDistance,ffNumber,8,0)+' km');
    case Kalender.MoonPhase of
      Neumond   : Add('Neumond');
      Vollmond  : Add('Vollmond');
      abnehmend : Add('Abnehmeder Mond');
      zunehmend : Add('Zunehmeder Mond');
    end;
    Add('Sonnenaufgang: '+TimeToStr(Kalender.SunRise)+' h');
    Add('Sonnenhöchststand: '+TimeToStr(Kalender.SunTransit)+' h');
    Add('Sonnenuntergang: '+TimeToStr(Kalender.SunSet)+' h');
    Add('Entfernung zur Sonne: '+FloatToStrF(Kalender.SunDistance/1000000,ffNumber,8,2)+' mio km');
  end;
  if ComboAstroData.Items.Count>0 then
    ComboAstroData.ItemIndex:=0;
end;

{ TSRClock }
procedure TMainForm.EditZeitChange(Sender: TObject);
begin
  try
    Uhr.Time:=StrToFloat(EditZeit.Text)/24;
  except
  end;
end;

procedure TMainForm.CBAutoUpdateClick(Sender: TObject);
begin
  Uhr.AutoUpdate:=CBAutoUpdate.Checked;
  EditZeit.Enabled:=not CBAutoUpdate.Checked;
end;

procedure TMainForm.ComboClockStyleChange(Sender: TObject);
begin
  case ComboClockStyle.ItemIndex of
    0 : Uhr.Style:=csClassic;
    1 : Uhr.Style:=csDigital;
    2 : Uhr.Style:=csMovingPoints;
    3 : Uhr.Style:=csPieSlice;
  end;
  if Uhr.Style=csDigital then begin
    Uhr.Height:=50;
    SpinClockContrast.Visible:=true;
    Label39.Visible:=true;
  end
  else begin
    Uhr.Height:=165;
    SpinClockContrast.Visible:=false;
    Label39.Visible:=false;
  end;
end;

procedure TMainForm.ComboClockKindChange(Sender: TObject);
begin
  case ComboClockKind.ItemIndex of
    0 : Uhr.Kind:=ckRealTime;
    1 : Uhr.Kind:=ckStopWatch;
  end;
  SBStart.Visible:=ComboClockKind.ItemIndex=1;
  SBStop.Visible:=ComboClockKind.ItemIndex=1;
  SBReset.Visible:=ComboClockKind.ItemIndex=1;
end;

procedure TMainForm.SpinClockContrastChange(Sender: TObject);
begin
  Uhr.LEDContrast:=SpinClockContrast.Value;
  Uhr.LEDContrast:=SpinClockContrast.Value;
end;

procedure TMainForm.SBStartClick(Sender: TObject);
begin
  Uhr.Start;
end;

procedure TMainForm.SBStopClick(Sender: TObject);
begin
  Uhr.Stop;
end;

procedure TMainForm.SBResetClick(Sender: TObject);
begin
  Uhr.Reset;
end;

procedure TMainForm.SBCSVOpenDlgClick(Sender: TObject);
begin
  CSVOpenDlg.Execute;
end;

procedure TMainForm.SBCSVSaveDlgClick(Sender: TObject);
begin
  CSVSaveDlg.Execute;
end;

procedure TMainForm.SBExtOpenDlgClick(Sender: TObject);
begin
  ExtOpenDlg.Execute;
end;

procedure TMainForm.SBExtSaveDlgClick(Sender: TObject);
begin
  ExtSaveDlg.Execute;
end;

procedure TMainForm.SBSliderOpenDlgClick(Sender: TObject);
begin
  SliderOpenDlg.Execute;
end;

procedure TMainForm.SBSliderSaveDlgClick(Sender: TObject);
begin
  SliderSaveDlg.Execute;
end;

procedure TMainForm.CBDlgShowHelpClick(Sender: TObject);
var Opt : TOpenOptions;
begin
  if CBDlgShowHelp.Checked then
    Opt:=CSVOpenDlg.Options+[ofShowHelp]
  else
    Opt:=CSVOpenDlg.Options-[ofShowHelp];
  CSVOpenDlg.Options:=Opt;
  CSVSaveDlg.Options:=Opt;
  ExtOpenDlg.Options:=Opt;
  ExtSaveDlg.Options:=Opt;
  SliderOpenDlg.Options:=Opt;
  SliderSaveDlg.Options:=Opt;
end;

procedure TMainForm.CBDlgHideReadOnlyClick(Sender: TObject);
var Opt : TOpenOptions;
begin
  if CBDlgShowHelp.Checked then
    Opt:=CSVOpenDlg.Options+[ofHideReadOnly]
  else
    Opt:=CSVOpenDlg.Options-[ofHideReadOnly];
  CSVOpenDlg.Options:=Opt;
  CSVSaveDlg.Options:=Opt;
  ExtOpenDlg.Options:=Opt;
  ExtSaveDlg.Options:=Opt;
  SliderOpenDlg.Options:=Opt;
  SliderSaveDlg.Options:=Opt;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var i : byte;
begin
  ComboEnhFormat.ItemIndex:=4;
  ComboBoxBD.ItemIndex:=0;
  ComboBoxTP.ItemIndex:=1;
  ComboBoxSS.ItemIndex:=0;
  ComboBoxBevel.ItemIndex:=0;
  ComboBoxDecSeperator.ItemIndex:=0;
  SRWavePlayer.WaveName:=ExtractFilePath(Application.ExeName)+'Maus.wav';
  ComboStyle.ItemIndex:=ord(SRCheckBox.Style);
  for i:=0 to 15 do
    ComboBundesland.Items.Add(Bundeslaender[i]);
  ComboBundesland.ItemIndex:=ord(Kalender.Bundesland);
  for i:=1 to 7 do
    ComboStartTag.Items.Add(LongDayNames[i]);
  ComboStartTag.ItemIndex:=Kalender.StartOfWeek;
  ComboDrawStyle.ItemIndex:=ord(Kalender.DrawStyle);
  Kalender.Date:=Date;
  ComboClockStyle.ItemIndex:=0;
  ComboClockKind.ItemIndex:=0;
  SRLabel3.Caption:='SRLabel3:'+#13#10+'Beispieltext'+#13#10+'mit Zeilenumbrüchen';
  PanelVersion.Caption:='Version '+GetPackageVersionNr;
  LblInfo.Caption:='(C)opyright 2001'+#13#10#13#10+
                   'Die Komponenten sind Public Domain'+#13#10+
                   'das Urheberrecht liegt aber beim Autor.';
end;

end.
