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

unit Dt;

interface

uses SysUtils, WinProcs, Classes, Controls, TypefgLib;

const
  ShortDayNamesEnglish : array[1..7] of string[3]=('Sun','Mon','Tue','Wed','Thu','Fri','Sat');
  ShortDayNamesGerman : array[1..7] of string[3]=('Son','Mon','Die','Mit','Don','Fre','Sam');
  ShortDayNamesFrench : array[1..7] of string[3]=('Dim','Lun','Mar','Mer','Jeu','Ven','Sam');
  ShortDayNamesItalian : array[1..7] of string[3]=('Dom','Lun','Mar','Mer','Gio','Ven','Sab');
  ShortDayNamesSpanish : array[1..7] of string[3]=('Dom','Lun','Mar','Mié','Jue','Vie','Sál');
  LongDayNamesEnglish : array[1..7] of string[15]=('Sunday','Monday','Tuesday','Wednesday','Thursday','Friday','Saturday');
  LongDayNamesGerman : array[1..7] of string[15]=('Sonntag','Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag');
  LongDayNamesFrench : array[1..7] of string[15]=('Dimanche','Lundi','Mardi','Mercredi','Jeudi','Vendredi','Samedi');
  LongDayNamesItalian : array[1..7] of string[15]=('Domenica','Lunedi','Martedi','Mercoledi','Giovedi','Venerdi','Sabato');
  LongDayNamesSpanish : array[1..7] of string[15]=('Domingo','Lunes','Martes','Miércoles','Jueves','Viernes','Sálbado');
  ShortMonthNamesEnglish : array[1..12] of string[3]=('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
  ShortMonthNamesGerman : array[1..12] of string[3]=('Jan','Feb','Mär','Apr','Mai','Jun','Jul','Aug','Sep','Okt','Nov','Dez');
  ShortMonthNamesFrench : array[1..12] of string[3]=('Jan','Fév','Mar','Avr','Mai','Jui','Jui','Aoû','Sep','Oct','Nov','Déc');
  ShortMonthNamesItalian : array[1..12] of string[3]=('Gen','Feb','Mar','Apr','Mag','Giu','Lug','Ago','Set','Ott','Nov','Dic');
  ShortMonthNamesSpanish : array[1..12] of string[3]=('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic');
  LongMonthNamesEnglish : array[1..12] of string[15]=('January','February','March','April','May','June','July','August',
  'September','October','November','December');
  LongMonthNamesGerman : array[1..12] of string[15]=('Januar','Februar','März','April','Mai','Juni','Juli','August',
  'September','Oktober','November','Dezember');
  LongMonthNamesFrench : array[1..12] of string[15]=('Janvier','Février','Mars','Avril','Mai','Juin','Juillet','Août',
  'Septembre','Octobre','Novembre','Décembre');
  LongMonthNamesItalian : array[1..12] of string[15]=('Gennaio','Febbraio','Marzo','Aprile','Maggio','Giugno','Luglio',
  'Agosto','Settembre','Ottobre','Novembre','Dicembre');
  LongMonthNamesSpanish : array[1..12] of string[15]=('Enero','Febrero','Marzo','Abril','Mayo','Junio','Julio','Agosto',
  'Septiembre','Octubre','Noviembre','Diciembre');


type
  TExtMode = (extDay, extMonth, extYear, extHour, extMinute, extSecond);
  TDateFormat = (daLong, daMedium, daShort, daDay, daDayOfWeek, daMonth, daYear);
  TTimeFormat = (tiLong,tiShort, tiHour, tiMinute, tiSecond);

function GetDate(Format : TDateFormat; Language : TLanguage) : string;
function DayStr(Day : integer; Language : TLanguage) : string;
function MonthStr(Month : integer; Language : TLanguage) : string;
function MonthLength(Month,Year : word) : integer;
function AsLeapYear(Year : word) : boolean;
function DateComp(Date1,Date2 : string) : boolean;
function DateExtract(Date_ : string; Mode : TExtMode) : string;
function DateInc(Date_ : string; Value : integer) : string;
function DateDec(Date_ : string; Value : integer) : string;
function DayCount(date_ : string) : integer;
function WeekCount(date_ : string) : integer;
function CheckDate(Date_ : TDate) : Boolean;
procedure AssigncurrentDayMonth;
procedure ResetDayMonth(Language : TLanguage);
function GetTime(Format : TTimeFormat) : string;
function TimeComp(Time1,Time2 : string) : boolean;
function TimeInc(h : string; w : INTEGER) : string;
function TimeDec(h : string; w : INTEGER) : string;
function TimeExtract(Time_ : TTime; Mode : TExtMode) : string;
function CheckTime(Time_ : TTime) : Boolean;

var
  FYear,FMonth,FDay,FDayOfWeek : WORD;

implementation


{-----------------------------------------------------------------------------}
{  Date Procedures Library                                                    }
{-----------------------------------------------------------------------------}
function GetDate(Format : TDateFormat; Language : TLanguage) : string;
var
  StrYear,StrMonth,StrDay,StrDayOfWeek : string;
BEGIN
  if Language=laDefault then language := GetCurrentLanguage;
  DecodeDate(date,FYear,FMonth,FDay);
  FDayOfWeek := DayOfWeek(date);
  StrDayOfWeek := DayStr(FDayOfWeek,Language);
  StrDay := inttostr(FDay);
  StrMonth := MonthStr(FMonth,Language);
  StrYear := inttostr(FYear);
  case Format of
    daLong : result := StrDayOfWeek+' '+StrDay+' '+StrMonth+' '+StrYear;
    daMedium : result := StrDay+' '+StrMonth+' '+StrYear;
    daShort : result := datetostr(date);
    daDay : result := inttostrform(FDay,2);
    daDayOfWeek : result := StrDayOfWeek;
    daMonth : result := StrMonth;
    daYear : result := StrYear;
  end;
end;

function DayStr(Day : integer; Language : TLanguage) : string;
begin
  if Language=laDefault then language := GetCurrentLanguage;
  case Language of
    laEnglish : result := LongDayNamesEnglish[Day];
    laGerman : result := LongDayNamesGerman[Day];
    laFrench : result := LongDayNamesFrench[Day];
    laItalian : result := LongDayNamesItalian[Day];
    laSpanish : result := LongDayNamesSpanish[Day];
  end;
end;

function MonthStr(Month : integer; Language : TLanguage) : string;
begin
  if Language=laDefault then language := GetCurrentLanguage;
  case Language of
    laEnglish : result := LongMonthNamesEnglish[Month];
    laGerman : result := LongMonthNamesGerman[Month];
    laFrench : result := LongMonthNamesFrench[Month];
    laItalian : result := LongMonthNamesItalian[Month];
    laSpanish : result := LongMonthNamesSpanish[Month];
  end;
end;

function MonthLength(Month,Year : word) : integer;
BEGIN
  case Month of
    1 : result := 31;
    2 : if AsLeapYear(year) then result := 29 else result := 28;
    3 : result := 31;
    4 : result := 30;
    5 : result := 31;
    6 : result := 30;
    7 : result := 31;
    8 : result := 31;
    9 : result := 30;
    10: result := 31;
    11: result := 30;
    12: result := 31;
  end;
end;

function AsLeapYear(Year : word) : boolean;
begin
  if frac(Year/4)=0 THEN result := true ELSE result := false;
end;

function DateComp(Date1,Date2 : string) : boolean;
var
  Day1,Month1,Year1,Day2,Month2,Year2 : word;
begin
  DecodeDate(strtodate(Date1),Year1,Month1,Day1);
  DecodeDate(strtodate(Date2),Year2,Month2,Day2);
  IF ((Year1>Year2) or
     ((Year1=Year2) and (Month1>Month2)) or
     ((Year1=Year2) and (Month1=Month2) and (Day1>=Day2))) then result:=true else result:=false;
END;

function DateExtract(Date_ : string; Mode : TExtMode) : string;
var
  Day_,Month_,Year_ : word;
begin
  DecodeDate(strtodate(Date_),Year_,Month_,Day_);
  case mode of
    extDay  : result := inttostrform(Day_,2);
    extMonth: result := inttostrform(Month_,2);
    extYear : result := inttostrform(Year_,2);
  end;
end;

function DateInc(Date_ : string; Value : integer) : string;
var
  Day_,Month_,Year_ : Word;
begin
  DecodeDate(strtodate(Date_),Year_,Month_,Day_);
  repeat
    dec(value);
    inc(Day_);
    if Day_>Monthlength(Month_,Year_) then
    begin
      Day_ := 1;
      if Month_=12 then
      begin
        Month_ := 1;
        inc(Year_);
      end else inc(Month_);
    end;
  until value=0;
  result := datetostr(EncodeDate(Year_, Month_, Day_));
end;

function DateDec(Date_ : string; Value : integer) : string;
var
  Day_,Month_,Year_ : word;
begin
  DecodeDate(strtodate(Date_),Year_,Month_,Day_);
  repeat
    dec(value);
    dec(Day_);
    if Day_<1 then
    begin
      if Month_=1 then
      begin
        Month_ := 12;
        dec(Year_);
      end else dec(Month_);
      Day_ := Monthlength(Month_,Year_);
    end;
  until value=0;
  result := datetostr(EncodeDate(Year_, Month_, Day_));
end;

function GetDayOfWeek(Date_ : string) : integer;
begin
  result := DayOfWeek(strtodate(date_));
end;

function DayCount(date_ : string) : integer;
var
  Day_,Month_,Year_ : Word;
  i,count : integer;
begin
  DecodeDate(strtodate(Date_),Year_,Month_,Day_);
  count := 0;
  for i := 1 to Month_-1 do count := count+MonthLength(i,Year_);
  result := count+Day_;
end;

function WeekCount(date_ : string) : integer;
var
  i,count,nweek : integer;
  FirstDate,LastDate : string[10];
  Year_ : integer;
  DW : integer;
begin
  FirstDate := datetostr(EncodeDate(1979,12,31));
  LastDate := date_;
  DW := DayOfWeek(strtodate(date_));
  if DW=1 then DW := 7 else dec(DW);
  if DW>1 then LastDate := Datedec(LastDate,DW-1);
  count := 1;
  i := 0;
  repeat
    FirstDate := DateInc(FirstDate,7);
    inc(count);
    Year_ := strtoint(DateExtract(FirstDate,extYear));
    if ((Year_=1980) or
        (Year_=1986) or
        (Year_=1992) or
        (Year_=1997) or
        (Year_=2003) or
        (Year_=2008) or
        (Year_=2014) or
        (Year_=2020) or
        (Year_=2025) or
        (Year_=2031) or
        (Year_=2036) or
        (Year_=2042) or
        (Year_=2048) or
        (Year_=2053) or
        (Year_=2059) or
        (Year_=2064) or
        (Year_=2070) or
        (Year_=2076) or
        (Year_=2081) or
        (Year_=2087) or
        (Year_=2092) or
        (Year_=2098)) then nweek := 53 else nweek := 52;
    if count>nweek then count := 1;
  until DateComp(FirstDate,LastDate);
  result := count;
end;

function CheckDate(Date_ : TDate) : Boolean;
var
  Day_,Month_,Year_ : Word;
  error : integer;
begin
  AssigncurrentDayMonth;
  DecodeDate(Date_,Year_,Month_,Day_);
  if ((Day_>=1) and (Day_<=MonthLength(Month_,Year_)) and
      (Month_>=1) and (Month_<=12) and
      (Year_>=1900) and (Year_<=2099)) then result := true else result := False;
end;

procedure ResetDayMonth(Language : TLanguage);
var
  i : integer;
begin
  if Language=laDefault then Language := GetCurrentLanguage;
  for i := 1 to 7 do
  begin
    case Language of
      laEnglish :
      begin
        ShortDayNames[i] := ShortDayNamesEnglish[i];
        LongDayNames[i] := LongDayNamesEnglish[i];
      end;
      laGerman :
      begin
        ShortDayNames[i] := ShortDayNamesGerman[i];
        LongDayNames[i] := LongDayNamesGerman[i];
      end;
      laFrench :
      begin
        ShortDayNames[i] := ShortDayNamesFrench[i];
        LongDayNames[i] := LongDayNamesFrench[i];
      end;
      laItalian :
      begin
        ShortDayNames[i] := ShortDayNamesItalian[i];
        LongDayNames[i] := LongDayNamesItalian[i];
      end;
      laSpanish :
      begin
        ShortDayNames[i] := ShortDayNamesSpanish[i];
        LongDayNames[i] := LongDayNamesSpanish[i];
      end;
    end;
  end;
  for i  := 1 to 12 do
  begin
    case Language of
      laEnglish :
      begin
        ShortMonthNames[i] := ShortMonthNamesEnglish[i];
        LongMonthNames[i] := LongMonthNamesEnglish[i];
      end;
      laGerman :
      begin
        ShortMonthNames[i] := ShortMonthNamesGerman[i];
        LongMonthNames[i] := LongMonthNamesGerman[i];
      end;
      laFrench :
      begin
        ShortMonthNames[i] := ShortMonthNamesFrench[i];
        LongMonthNames[i] := LongMonthNamesFrench[i];
      end;
      laItalian :
      begin
        ShortMonthNames[i] := ShortMonthNamesItalian[i];
      LongMonthNames[i] := LongMonthNamesItalian[i];
      end;
      laSpanish :
      begin
        ShortMonthNames[i] := ShortMonthNamesSpanish[i];
        LongMonthNames[i] := LongMonthNamesSpanish[i];
      end;
    end;
  end;
end;

procedure AssigncurrentDayMonth;
var
  CountryChar : array[0..10] of char;
  CountryStr : string;
  i : integer;
begin
  GetProfileString('Intl', 'sLanguage', '', CountryChar, 10);
  CountryStr := copy(UpperCase(strpas(CountryChar)),1,2);
  for i := 1 to 7 do
  begin
    if CountryStr='DE' then
    begin
      ShortDayNames[i] := ShortDayNamesGerman[i];
      LongDayNames[i] := LongDayNamesGerman[i];
    end else
    if CountryStr='ES' then
    begin
      ShortDayNamesSpanish[i] := ShortDayNamesSpanish[i];
      LongDayNamesSpanish[i] := LongDayNamesSpanish[i];
    end else
    if CountryStr='FR' then
    begin
      ShortDayNamesFrench[i] := ShortDayNamesFrench[i];
      LongDayNamesFrench[i] := LongDayNamesFrench[i];
    end else
    if CountryStr='IT' then
    begin
      ShortDayNamesItalian[i] := ShortDayNamesItalian[i];
      LongDayNamesItalian[i] := LongDayNamesItalian[i];
    end else
    begin
      ShortDayNamesEnglish[i] := ShortDayNamesEnglish[i];
      LongDayNamesEnglish[i] := LongDayNamesEnglish[i];
    end;
  end;
  for i  := 1 to 12 do
  begin
    if CountryStr='DE' then
    begin
      ShortMonthNames[i] := ShortMonthNamesGerman[i];
      LongMonthNames[i] := LongMonthNamesGerman[i];
    end else
    if CountryStr='ES' then
    begin
      ShortMonthNamesSpanish[i] := ShortMonthNamesSpanish[i];
      LongMonthNamesSpanish[i] := LongMonthNamesSpanish[i];
    end else
    if CountryStr='FR' then
    begin
      ShortMonthNamesFrench[i] := ShortMonthNamesFrench[i];
      LongMonthNamesFrench[i] := LongMonthNamesFrench[i];
    end else
    if CountryStr='IT' then
    begin
      ShortMonthNamesItalian[i] := ShortMonthNamesItalian[i];
      LongMonthNamesItalian[i] := LongMonthNamesItalian[i];
    end else
    begin
      ShortMonthNamesEnglish[i] := ShortMonthNamesEnglish[i];
      LongMonthNamesEnglish[i] := LongMonthNamesEnglish[i];
    end;
  end;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Time Procedures Library                                                    }
{-----------------------------------------------------------------------------}
function TimeComp(Time1,Time2 : string) : boolean;
var
  h1,m1,h2,m2,ii : INTEGER;
begin
  val(copy(Time1,1,2),h1,ii);
  val(copy(Time1,4,2),m1,ii);
  val(copy(Time2,1,2),h2,ii);
  val(copy(Time2,4,2),m2,ii);
  if ((h1>h2) or ((h1=h2) and (m1>m2))) then  result := true else result := false;
END;

function TimeInc(h : string; w : INTEGER) : string;
var
  heure_tmp,minute_tmp : INTEGER;
  compteur,i : INTEGER;
begin
  heure_tmp := strtoint(copy(h,1,2));
  minute_tmp := strtoint(copy(h,4,2));  
  if w>0 then
  begin
    compteur := 0;
    repeat
      inc(compteur);
      inc(minute_tmp);
      if minute_tmp>59 then
      begin
        minute_tmp := 0;
        inc(heure_tmp);
        if heure_tmp=24 then heure_tmp := 0;
      end;
    until compteur=w;
  end;
  TimeInc := inttostrform(heure_tmp,2)+':'+inttostrform(minute_tmp,2);
end;

function TimeDec(h : string; w : INTEGER) : string;
var
  heure_tmp,minute_tmp : INTEGER;
  compteur,i : INTEGER;
begin
  heure_tmp := strtoint(copy(h,1,2));
  minute_tmp := strtoint(copy(h,4,2));
  if w>0 then
  begin
    compteur := 0;
    repeat
      inc(compteur);
      dec(minute_tmp);
      if minute_tmp<0 then
      begin
        minute_tmp := 59;
        dec(heure_tmp);
        if heure_tmp=-1 then heure_tmp := 23;
      end;
    until compteur=w;
  end;
  TimeDec := inttostrform(heure_tmp,2)+':'+inttostrform(minute_tmp,2);
end;

function GetTime(Format : TTimeFormat) : string;
var
  Sep : char;
  StrHour,StrMinute,StrSecond : string[2];
  FHour,FMinute,FSecond,FMSecond : Word;
begin
  Sep := TimeSeparator;
  DecodeTime(Time,FHour,FMinute,FSecond,FMSecond);
  StrHour := inttostrform(FHour,2);
  StrMinute := inttostrform(FMinute,2);
  StrSecond := inttostrform(FSecond,2);
  case Format of
    tiLong : result := StrHour+Sep+StrMinute+Sep+StrSecond;
    tiShort : result := StrHour+Sep+StrMinute;
    tiHour : result := StrHour;
    tiMinute : result := StrMinute;
    tiSecond : result := StrSecond;
  end;
end;

function TimeExtract(Time_ : TTime; Mode : TExtMode) : string;
var
  Hour_, Min_, Sec_, MSec_ : Word;
begin
  DecodeTime(Time_, Hour_, Min_, Sec_, MSec_);
  case mode of
    extHour : result := inttostr(Hour_);
    extMinute : result := inttostr(Min_);
    extSecond : result := inttostr(Sec_);
  end;
end;

function CheckTime(Time_ : TTime) : Boolean;
var
  Hour_, Min_, Sec_, MSec_ : Word;
begin
  DecodeTime(Time_, Hour_, Min_, Sec_, MSec_);
  if ((Hour_>=0) and (Hour_<=24) and
      (Min_>=0) and (Min_<=59) and
      (Sec_>=0) and (Sec_<=59)) then result := true else result := False;
end;
{-----------------------------------------------------------------------------}
END.









