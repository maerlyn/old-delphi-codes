{------------------------------------------------------------------------------}
{ Copyright 2001 by EuroSoft Software Development                              }
{ designl@worldnet.net                                                         }
{                                                                              }
{ This software is provided 'as-is', without any express or implied warranty.  }
{ In no event will the author be held liable for any  damages arising from     }
{ the use of this software.                                                    }
{                                                                              }
{ No part of this Unit may be copied in any way without a written permission.  }
{------------------------------------------------------------------------------}

unit fglibreg;

interface

uses WinProcs, Sysutils, Classes, Dsgnintf, forms, Controls,
     Typefglib, FgLib, AnimeDlg, OpenBmp, OpenWav, CurDlg,
     AlarmDlg, StrDlg, PerpCal;

type
  None = class(TClassProperty)
  end;

  TImageListProperty = class(TPropertyEditor)
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
  end;

  TWaveFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TBitmapFilenameProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TCursorsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TAlarmProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  THintStringProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TCalendarProperty = class(TStringProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure Register;

implementation
{-----------------------------------------------------------------------------}
{  TImageListProperty                                                         }
{-----------------------------------------------------------------------------}
function TImageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TImageListProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetPropType^.Name]);
end;

procedure TImageListProperty.Edit;
var
  PropertyDlg: TImageListDialog;
  BmpAnime : TBmpAnime;
begin
  BmpAnime := GetComponent(0) as TBmpAnime;
  PropertyDlg := TImageListDialog.Create(Application);
  try
    PropertyDlg.Timer.interval := BmpAnime.FTimer.interval;
    PropertyDlg.SetPageList(TStringList(GetOrdValue));
    PropertyDlg.ShowModal;
    if PropertyDlg.Modified then Designer.Modified;
  finally
    PropertyDlg.Free;
  end;
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  TWaveFilenameProperty                                                      }
{-----------------------------------------------------------------------------}
procedure TWaveFilenameProperty.Edit;
var
  FileOpen: TOpenWave;
begin
  FileOpen := TOpenWave.create(Application);
  FileOpen.Language := LanguageBuff;
  FileOpen.Filename := GetValue;
  try
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TWaveFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  TBitmapFilenameProperty                                                    }
{-----------------------------------------------------------------------------}
procedure TBitmapFilenameProperty.Edit;
var
  FileOpen: TOpenBitmap;
begin
  FileOpen := TOpenBitmap.create(Application);
  FileOpen.Language := LanguageBuff;
  FileOpen.Filename := GetValue;
  try
    if FileOpen.Execute then SetValue(FileOpen.Filename);
  finally
    FileOpen.Free;
  end;
end;

function TBitmapFilenameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  TCursorsProperty                                                           }
{-----------------------------------------------------------------------------}
procedure TCursorsProperty.Edit;
var
  Cursordlg: TCursorsDlg;
begin
  Cursordlg := TCursorsDlg.create(Application);
  Cursordlg.NewCursor := TCursor(GetOrdValue);
  try
    if Cursordlg.ShowModal=mrOk then setordvalue(longint(Cursordlg.NewCursor));
  finally
    Cursordlg.Free;
  end;
end;

function TCursorsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  TAlarmProperty                                                             }
{-----------------------------------------------------------------------------}
procedure TAlarmProperty.Edit;
var
  PropertyDlg : TAlarmSetup;
begin
  PropertyDlg := TAlarmSetup.create(Application);
  PropertyDlg.AlarmData := TAlarmData(getordvalue);
  if PropertyDlg.ShowModal=mrOk then setordvalue(longint(PropertyDlg.AlarmData));
  PropertyDlg.Free;
end;

function TAlarmProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  Component : THintStringProperty                                            }
{-----------------------------------------------------------------------------}
procedure THintStringProperty.Edit;
var
  HintString : TStringDlg;
begin
  HintString := TStringDlg.create(Application);
  HintString.Strings.Text := GetValue;
  try
    if HintString.Showmodal=mrOk then SetValue(HintString.Strings.Text);
  finally
    HintString.Free;
  end;
end;

function THintStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------}
{  Dialogbox : TCalendarProperty                                              }
{-----------------------------------------------------------------------------}
procedure TCalendarProperty.Edit;
var
  PerpetualCalendarDlg : TPerpetualCalendarDlg;
begin
  PerpetualCalendarDlg := TPerpetualCalendarDlg.Create(Application);
  PerpetualCalendarDlg.Position := poScreenCenter;
  PerpetualCalendarDlg.Language := laDefault;
  PerpetualCalendarDlg.date := GetValue;
  try
    PerpetualCalendarDlg.showmodal;
    SetValue(PerpetualCalendarDlg.date);
  finally
    PerpetualCalendarDlg.Free;
  end;
end;

function TCalendarProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;
{-----------------------------------------------------------------------------}

procedure Register;
begin
  RegisterComponents('FGLib',[TNotePad]);
  RegisterComponents('FGLib',[TBmpMosaic]);
  RegisterComponents('FGLib',[TBmpAnime]);
  RegisterComponents('FGLib',[TBmpTransparent]);
  RegisterComponents('FGLib',[TIntensityFilter]);
  RegisterComponents('FGLib',[TShadow]);
  RegisterComponents('FGLib',[TPad3D]);
  RegisterComponents('FGLib',[TOpenBitmap]);
  RegisterComponents('FGLib',[TOpenWave]);
  RegisterComponents('FGLib',[TPlayWave]);
  RegisterComponents('FGLib',[TCursorsDialog]);
  RegisterComponents('FGLib',[TTrash]);
  RegisterComponents('FGLib',[TMagicLabel]);
  RegisterComponents('FGLib',[THintDesign]);
  RegisterComponents('FGLib',[TDigitalClock]);
  RegisterComponents('FGLib',[TAnalogicClock]);
  RegisterComponents('FGLib',[TAlarm]);
  RegisterComponents('FGLib',[TCalendarPad]);
  RegisterComponents('FGLib',[TMiniCalendar]);
  RegisterComponents('FGLib',[TPerpetualCalendar]);
  RegisterPropertyEditor(TypeInfo(TStrings), TBmpAnime, 'ImageList', TImageListProperty);
  RegisterPropertyEditor(TypeInfo(TWaveFileName), nil, '', TWaveFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TWaveFileName), nil, '', TWaveFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TPlayWave, 'Filename', TWaveFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TOpenWave, 'Filename', TWaveFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TFileName), TOpenBitmap, 'Filename', TBitmapFilenameProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), nil, 'Cursor', TCursorsProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TTrash, 'Cursor', None);
  RegisterPropertyEditor(TypeInfo(string), TTrash, 'Hint', None);
  RegisterPropertyEditor(TypeInfo(string), nil, 'Hint', THintStringProperty);
  RegisterPropertyEditor(typeinfo(TAlarmData),TAlarm,'AlarmSet',TAlarmProperty);
  RegisterPropertyEditor(TypeInfo(string), TPerpetualCalendar, 'Date', TCalendarProperty);
  RegisterPropertyEditor(TypeInfo(string), TCalendarPad, 'Date', TCalendarProperty);
  RegisterPropertyEditor(TypeInfo(TCursor), TCalendarPad, 'Cursor', None);
  RegisterPropertyEditor(TypeInfo(string), TCalendarPad, 'Hint', None);
end;

end.


