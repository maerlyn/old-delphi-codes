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

unit Perpcal;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Grids, Calendar, Spin,
  TypefgLib, DT;

type
  TPerpetualCalendarDlg = class(TForm)
    Panel1: TPanel;
    Calendar: TCalendar;
    Month: TComboBox;
    Year: TSpinEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MonthChange(Sender: TObject);
    procedure YearChange(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    Sep : char;
  public
    Date : string[10];
    Language : TLanguage;
  end;

var
  PerpetualCalendarDlg: TPerpetualCalendarDlg;

implementation

{$R *.DFM}

{-----------------------------------------------------------------------------}
{  Dialogbox : TPerpetualCalendarDlg                                          }
{-----------------------------------------------------------------------------}
procedure TPerpetualCalendarDlg.FormCreate(Sender: TObject);
begin
  bordericons := [];
  Sep := DateSeparator;
end;

procedure TPerpetualCalendarDlg.FormShow(Sender: TObject);
var i : integer;
begin
  if Language=laDefault then Language := GetCurrentLanguage;
  ResetDayMonth(Language);
  calendar.refresh;
  Month.items.clear;
  for i:= 1 to 12 do Month.items.add(MonthStr(i,Language));
  calendar.day := strtoint(DateExtract(date,extDay));
  calendar.month := strtoint(DateExtract(date,extMonth));
  calendar.year := strtoint(DateExtract(date,extYear));
  Month.itemindex := calendar.month-1;
  year.value := calendar.year;
  case Language of
    laEnglish : BitBtn2.Caption := '&Cancel';
    laGerman  : BitBtn2.Caption := '&Annulieren';
    laFrench  : BitBtn2.Caption := '&Annuler';
    laItalian : BitBtn2.Caption := '&Annullare';
    laSpanish : BitBtn2.Caption := '&Cancelar';
  end;
end;

procedure TPerpetualCalendarDlg.YearChange(Sender: TObject);
begin
  if calendar.day>MonthLength(calendar.month,calendar.year) then
  calendar.day := MonthLength(calendar.month,calendar.year);
  calendar.year := year.value;
end;

procedure TPerpetualCalendarDlg.MonthChange(Sender: TObject);
begin
  if calendar.day>MonthLength(Month.itemindex+1,calendar.year) then
  calendar.day := MonthLength(Month.itemindex+1,calendar.year);
  calendar.month := Month.itemindex+1;
end;

procedure TPerpetualCalendarDlg.BitBtn1Click(Sender: TObject);
begin
  date := datetostr(EncodeDate(calendar.year, calendar.month, calendar.day));
  ModalResult := mrok;
end;

procedure TPerpetualCalendarDlg.BitBtn2Click(Sender: TObject);
begin
  Close;
end;
{-----------------------------------------------------------------------------}

end.
