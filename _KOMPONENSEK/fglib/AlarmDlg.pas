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

unit AlarmDlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  Typefglib, fgLib, DT;

type
  TAlarmSetup = class(TForm)
    Panel1: TPanel;
    GroupBox3: TGroupBox;
    DigitalClock1: TDigitalClock;
    time: TEdit;
    GroupBox2: TGroupBox;
    text: TMemo;
    GroupBox1: TGroupBox;
    RepeatAlarm: TRadioButton;
    Frequency: TComboBox;
    DateAlarm: TRadioButton;
    Date: TEdit;
    GroupBox4: TGroupBox;
    play: TBitBtn;
    Panel2: TPanel;
    ok1: TBitBtn;
    BitBtn1: TBitBtn;
    clear: TBitBtn;
    Sound: TEdit;
    open: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    procedure ok1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RepeatAlarmClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure timeExit(Sender: TObject);
    procedure DateExit(Sender: TObject);
    procedure openClick(Sender: TObject);
    procedure clearClick(Sender: TObject);
    procedure playClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
  private
    procedure SoundState;
    procedure RepeatState;
  public
    AlarmData : TAlarmData;
  end;

var
  AlarmSetup: TAlarmSetup;

implementation

{$R *.DFM}


procedure TAlarmSetup.FormCreate(Sender: TObject);
begin
  bordericons := [];
  frequency.itemindex := 0;
end;

procedure TAlarmSetup.FormShow(Sender: TObject);
begin
  time.text := timetostr(AlarmData.time);
  date.text := datetostr(AlarmData.date);
  text.text := AlarmData.text;
  if AlarmData.RepeatAlarm then RepeatAlarm.checked := true else DateAlarm.checked := true;
  Sound.text := ExtractFileName(LowerCase(AlarmData.SoundFileName));
  frequency.itemindex := AlarmData.frequency;
  SoundState;
end;

procedure TAlarmSetup.timeExit(Sender: TObject);
begin
  if not CheckTime(strtotime(time.text+':00')) then
  begin
    messagedlg(time.text+' is not a valid time',mterror,[mbok],0);
    time.text := GetTime(tiShort);
    time.setfocus;
  end;
end;

procedure TAlarmSetup.DateExit(Sender: TObject);
begin
  if not CheckDate(strtodate(date.text)) then
  begin
    messagedlg(date.text+' is not a valid date',mterror,[mbok],0);
    date.text := GetDate(daShort,GetCurrentLanguage);
    date.setfocus;
  end;
end;

procedure TAlarmSetup.RepeatState;
begin
  if RepeatAlarm.checked then
  begin
    DateAlarm.checked := false;
    frequency.enabled := true;
    date.enabled := false;
  end else
  begin
    DateAlarm.checked := true;
    frequency.enabled := false;
    date.enabled := true;
  end;
end;

procedure TAlarmSetup.RepeatAlarmClick(Sender: TObject);
begin
  RepeatState;
end;

procedure TAlarmSetup.openClick(Sender: TObject);
begin
  OpenDialog1.filename := AlarmData.SoundFileName;
  if OpenDialog1.execute then
  begin
    Sound.text := ExtractFileName(LowerCase(OpenDialog1.filename));
    AlarmData.SoundFileName := LowerCase(OpenDialog1.filename);
  end;
  SoundState;
end;

procedure TAlarmSetup.clearClick(Sender: TObject);
begin
  AlarmData.SoundFileName := '';
  Sound.text := '';
  SoundState;
end;

procedure TAlarmSetup.playClick(Sender: TObject);
begin
  PlayWaveFile(AlarmData.SoundFileName,1);
end;

procedure TAlarmSetup.SoundState;
begin
  if ((Sound.text='') or not FileExists(Sound.text)) then
  begin
    clear.enabled := false;
    play.enabled := false;
  end else
  begin
    clear.enabled := true;
    play.enabled := true;
  end;
end;

procedure TAlarmSetup.ok1Click(Sender: TObject);
begin
  AlarmData.time := strtotime(time.text);
  AlarmData.date := strtodate(date.text);
  AlarmData.text := text.text;
  AlarmData.RepeatAlarm := RepeatAlarm.checked;
  AlarmData.frequency := frequency.itemindex;
  modalresult := mrOk;
end;

procedure TAlarmSetup.BitBtn1Click(Sender: TObject);
begin
  modalresult := mrCancel;
end;

end.
