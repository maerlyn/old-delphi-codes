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

unit typefglib;

{$R fglib.res}

interface

uses SysUtils, WinProcs, WinTypes, Forms, graphics, Classes, ExtCtrls,
     mmsystem, controls, Dialogs;


type
  TCursorEntry = record
    Value: TCursor;
    Name: PChar;
  end;

const
  crTopHand = -1018;
  crRightHand = -1019;
  crLeftHand = -1020;

const
  Cursors: array[0..15] of TCursorEntry = (
    (Value: crDefault;      Name: 'crDefault'),
    (Value: crArrow;        Name: 'crArrow'),
    (Value: crCross;        Name: 'crCross'),
    (Value: crIBeam;        Name: 'crIBeam'),
    (Value: crSize;         Name: 'crSize'),
    (Value: crSizeNESW;     Name: 'crSizeNESW'),
    (Value: crSizeNS;       Name: 'crSizeNS'),
    (Value: crSizeNWSE;     Name: 'crSizeNWSE'),
    (Value: crSizeWE;       Name: 'crSizeWE'),
    (Value: crUpArrow;      Name: 'crUpArrow'),
    (Value: crHourGlass;    Name: 'crHourGlass'),
    (Value: crDrag;         Name: 'crDrag'),
    (Value: crHSplit;       Name: 'crHSplit'),
    (Value: crVSplit;       Name: 'crVSplit'),
    (Value: crMultiDrag;    Name: 'crMultiDrag'),
    (Value: crSQLWait;      Name: 'crSQLWait'));

type
  TQuality=(quHi,quLow);
  TShadowPos = (shTopLeft, shTopRight, shBottomLeft, shBottomRight);
  TBackground = (bgMosaic,bgPicture);
  TBorderPos = (boVertical, boHorizontal);
  TLanguage = (laDefault, laEnglish, laGerman, laFrench, laItalian, laSpanish);
  THintPosition=(hiTopRight,hiTopLeft,hiBottomRight,hiBottomLeft);
  THintStyle=(hiRectangle,hiRoundrect,hiBubble,hiImage, hiTexture, hiText);
  TLinkStyle=(liNone,liArrow,liBubble);
  TFontData = record
    Color : TColor;
    Size : integer;
    Style : TFontStyles;
    Name : TFontName;
  end;

type
  TWaveFileName = TFileName;
  TBmpFileName = TFileName;

type
  AboutFreeLib = class
  end;


var
  LanguageBuff : TLanguage;

procedure PlayWaveFile(filename : string; flag : WORD);
function GetCurrentLanguage : TLanguage;
function IntToStrForm(value,length:integer) : string;
function MouseIn(xm,ym : integer; R : TRect) : boolean;

implementation

procedure PlayWaveFile(filename : string; flag : WORD);
var
  text : array[0..80] OF CHAR;
begin
  strpcopy(text,filename);
  SndPlaySound(text,flag);
end;

function GetCurrentLanguage : TLanguage;
var
  CountryChar : array[0..10] of char;
  CountryStr : string;
begin
  GetProfileString('Intl', 'sLanguage', '', CountryChar, 10);
  CountryStr := copy(UpperCase(strpas(CountryChar)),1,2);
  if CountryStr='DE' then result := laGerman else
  if CountryStr='ES' then result := laSpanish else
  if CountryStr='FR' then result := laFrench else
  if CountryStr='IT' then result := laItalian else
  if CountryStr='EN' then result := laEnglish else result := laEnglish;
end;

function IntToStrForm(value,length:integer) : string;
var
  str : string;
begin
  str := inttostr(value);
  if ((value<10) and (length>=2)) then insert('0',str,1);
  if ((value<100) and (length>=3)) then insert('0',str,1);
  if ((value<1000) and (length>=4)) then insert('0',str,1);
  if ((value<10000) and (length>=5)) then insert('0',str,1);
  if ((value<100000) and (length>=6)) then insert('0',str,1);
  if ((value<1000000) and (length>=7)) then insert('0',str,1);
  if ((value<10000000) and (length>=8)) then insert('0',str,1);
  result := str;
end;

function MouseIn(xm,ym : integer; R : TRect) : boolean;
begin
  if ((xm>=R.Left) and (xm<=R.Right) and (ym>=R.Top) and (ym<=R.Bottom)) then result := true else result := false;
end;

begin
  screen.cursors[crRightHand] := loadcursor(HInstance,'RightHand_D');
  screen.cursors[crLeftHand] := loadcursor(HInstance,'LeftHand_D');
  screen.cursors[crTopHand] := loadcursor(HInstance,'TopHand_D');
end.
