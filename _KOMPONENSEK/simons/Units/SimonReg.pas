unit Simonreg;

interface

{$I SRDefine.inc}

procedure Register;

implementation

uses EnhEdit, OvalBtn, RackCtls, SRCal, SRColBtn, SRLabel, SRGrad, SRWave,
     {$IFDEF SR_Delphi2_Up}SRChkBox, {$ENDIF}
     {$IFDEF SR_Delphi3_Up}SRDlgs, {$ENDIF} Classes;

procedure Register;
begin
  RegisterComponents('Simon', [TButtonPanel, TScrewPanel, TLEDButton, TOvalButton,
                               TSRColorButton, TEnhancedEdit, TLEDDisplay, TSRLabel,
                               TLEDMeter, TSRCalendar, TSRGradient, TSRWavePlayer]);
  {$IFDEF SR_Delphi2_Up}
  RegisterComponents('Simon', [TSRClock, TSRCheckBox]);
  {$ENDIF}
  {$IFDEF SR_Delphi3_Up}
  RegisterComponents('Simon', [TCSVOpenDialog, TCSVSaveDialog,
                               TExtOpenDialog, TExtSaveDialog,
                               TSliderOpenDialog, TSliderSaveDialog]);
  {$ENDIF}
end;

end.
