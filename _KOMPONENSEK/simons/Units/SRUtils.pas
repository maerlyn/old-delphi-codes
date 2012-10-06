unit SRUtils;

{----------------------------------------------------------------------}
{  Version  : 1.30                                                     }
{  Autor    : Simon Reinhardt                                          }
{  eMail    : reinhardt@picsoft.de                                     }
{  Internet : http://www.picsoft.de                                    }
{                                                                      }
{  Hilfreiche Prozeduren und Funktionen, die die Borland-Programmierer }
{  offensichtlich vergessen haben.                                     }
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{ Version 1.30:                                                        }
{ Neu: GetShiftState                                                   }
{                                                                      }
{ Version 1.29:                                                        }
{ Neu: ExtractFileDir, LastDelimiter                                   }
{ Geändert: GetExeForProtocol, FindAssociatedProgram                   }
{                                                                      }
{ Version 1.28:                                                        }
{ Geändert: DateTimeToStrDef, DateToStrDef                             }
{ Neu: Konstante PeriodNames                                           }
{                                                                      }
{ Version 1.27:                                                        }
{ Neu: GetWindowState, GetSystemWorkArea                               }
{                                                                      }
{ Version 1.26:                                                        }
{ Neu: GetFirstDayOfWeek                                               }
{ Geändert: IsSummerTime,                                              }
{ Initialisierung von FirstWeekDay und FirstWeekDate in Delphi 1       }
{                                                                      }
{ Version 1.25:                                                        }
{ Neu: GetHourFromTime, GetMinuteFromTime, GetSecondFromTime           }
{ Geändert: GetDayFromDate, GetMonthFromDate, GetYearFromDate          }
{                                                                      }
{ Version 1.24:                                                        }
{ Geändert: Konstanten ShortForbiddenChars und LongForbiddenChars      }
{                                                                      }
{ Version 1.23:                                                        }
{ Geändert: GetWeekOfYear, GetWeeksPerYear                             }
{                                                                      }
{ Version 1.22:                                                        }
{ Neu: DateToStrDef, StrToDateDef, GetWeeksPerYear                     }
{ Geändert: GetFirstPartOfString, AddBackSlash                         }
{                                                                      }
{----------------------------------------------------------------------}

interface

{$I SRDefine.inc}

{$IFDEF SR_Delphi1}
uses WinTypes, WinProcs, Graphics, Classes;
{$ELSE}
uses Windows, Graphics, Classes;
{$ENDIF}


const
  { Standard Encarta & FlatStyle Color Constants     }
  { Diese konstanten hat maik Porkert am 31.10.2000  }
  { in de.comp.lang.delphi.non-tech gepostet.        }
  { Ich stelle Sie hier zur Verfügung:               }

  ecDarkBlue = TColor($00996633);
  ecBlue = TColor($00CF9030);
  ecLightBlue = TColor($00CFB78F);

  ecDarkRed = TColor($00302794);
  ecRed = TColor($005F58B0);
  ecLightRed = TColor($006963B6);

  ecDarkGreen = TColor($00385937);
  ecGreen = TColor($00518150);
  ecLightGreen = TColor($0093CAB1);

  ecDarkYellow = TColor($004EB6CF);
  ecYellow = TColor($0057D1FF);
  ecLightYellow = TColor($00B3F8FF);

  ecDarkBrown = TColor($00394D4D);
  ecBrown = TColor($00555E66);
  ecLightBrown = TColor($00829AA2);

  ecDarkKaki = TColor($00D3D3D3);
  ecKaki = TColor($00C8D7D7);
  ecLightKaki = TColor($00E0E9EF);

  {$IFDEF SR_Delphi1}
  Max_Path = 255;
  {$ENDIF}
  { Ungültige Zeichen fuer 8.3-Dateinamen im DOS-Format: }
  ShortForbiddenChars :
    set of char = [':','?','*',';','=','+','<','>','|','"','[',']',' ','\',#39];
  { Ungültige Zeichen fuer lange Dateinamen im Win9x-Format: }
  LongForbiddenChars :
    set of char = ['\','/',':','*','?','"','<','>','|'];
  { Bezeichner für relative Datumsangaben in DateTimeToStrDef und DateToStrDef: }
  PeriodNames :
    array [0..4] of string = ('Übermorgen', 'Morgen', 'Heute', 'Gestern', 'Vorgestern');

type
  TFileSizeFormat = (fsByte, fsKilobyte, fsMegabyte);
  { Rückgabe-Formate für die Funktion GetFileSize }

{---------------------------------------}
{ Funktionen für alle Delphi-Versionen: }
{---------------------------------------}

function AddBackslash(FileName:string):string;
 { erweitert den Dateinamen mit einem abschließenden Backslash }
function CutBackSlash(FileName:string):string;
 { entfernt den abschließenden Backslash aus dem Dateinamen }
function CutRootDirectory(FName:string):string;
 { entfernt das Stammverzeichnis aus einem Dateinamen }
function DateTimeToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
 { Umwandlung DateTime->String mit Rückgabe eines Default-Wertes bei Fehlern }
function DateToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
 { Umwandlung Date->String mit Rückgabe eines Default-Wertes bei Fehlern }
function ExecAndWait(const Filename,Params:string;WindowState:word):boolean;
 { Startet ein Programm und wartet auf dessen Ende }
function ExpandString(S:string;AChar:char;ALength:word):string;
 { Erweitert einen String mit dem Zeichen "AChar" auf die Länge ALength }
function ExtractRawFileName(DName:string):string;
 { Gibt von einem vollen Dateinamen mit Pfad nur den Dateinamen ohne Erweiterung zurück }
function GetBuildInfo(const AFilename:String; var V1,V2,V3,V4:Word):Boolean;
 { Ermittelt die vier Versionsnummern einer Exe- oder Dll-Datei }
function GetDayFromDate(ADate:TDateTime):word;
 { Gibt den Tag im Monat aus einem Datums-Wert zurück }
function GetDayOfYear(ADate:TDateTime):word;
 { Gibt den Tag im Jahr aus einem Datums-Wert zurück }
function GetDaysPerMonth(AYear,AMonth:integer):integer;
 { Gibt die Anzahl Tage in einem Monat zurück }
function GetFileSize(FileName:string;AFormat:TFileSizeFormat):integer;
 { Ermittelt die Größe der Datei "FileName" im Format "AFormat" }
function GetFirstDayOfWeek(ADate:TDateTime):TDateTime;
 { Gibt den ersten Tag der Woche zurück, in dem das Datum ADate liegt }
function GetFirstPartOfString(var AText:string;Delimiter:char;IncludeDelimiter:boolean):string;
 { Extrahiert aus einem String den ersten Teil bis zum Zeichen "Delimiter" und entfernt
   diesen Teil aus dem String "AText" }
function GetHourFromTime(ATime:TDateTime):byte;
 { Gibt die Stunde aus einem Zeit-Wert zurück }
function GetMinuteFromTime(ATime:TDateTime):byte;
 { Gibt die Minute aus einem Zeit-Wert zurück }
function GetMonthFromDate(ADate:TDateTime):word;
 { Gibt den Monat aus einem Datums-Wert zurück }
function GetSecondFromTime(ATime:TDateTime):byte;
 { Gibt die Sekunde aus einem Zeit-Wert zurück }
function GetShiftState:TShiftState;
 { Ermittelt den Zustand der Shift-, Alt- und Ctrl-Tasten }
function GetSystemDir:string;
 { Ermittelt das Windows-System-Verzeichnis }
function GetVersionNr(ExeName:string;BuildNr:boolean):string;
 { Generiert einen Versionsnummern-string zu einer Exe- oder Dll-Datei }
function GetWeekOfYear(ADate:TDateTime):byte;
 { Gibt die Woche im Jahr aus einem Datums-Wert zurück }
function GetWeeksPerYear(AYear:word):byte;
 { Gibt die Wochenzahl der letzten Woche im Jahr "AYear" zurück }
function GetWindowsDir:string;
 { Ermittelt das Windows-Verzeichnis }
function GetYearFromDate(ADate:TDateTime):word;
 { Gibt das Jahr aus einem Datums-Wert zurück }
function IntToStrFixed(IntValue:integer;OutDigits:byte):string;
 { Umwandlung Int->String mit fester Stellenzahl und führenden Nullen }
function IsSummertime(ADate:TDateTime):boolean;
 { Ermmittelt, ob ein Datum in der Sommerzeit liegt }
function ReverseString(AText:string):string;
 { Spiegelt einen String, die Buchstabenfolge wird umgedreht }
function RGBToStr(RGBColor:integer):string;
 { Umwandlung Windows-RGB-Wert -> HTML-RGB-Wert }
function StripForbiddenChars(AText:string):string;
 { Entfernt für Dateinamen nicht erlaubte Zeichen aus einem String }
function StrToDateDef(S:string;Def:TDateTime):TDateTime;
 { Umwandlung String->Date mit Rückgabe eines Default-Wertes bei Fehlern }
function StrToDateTimeDef(S:string;Def:TDateTime):TDateTime;
 { Umwandlung String->DateTime mit Rückgabe eines Default-Wertes bei Fehlern }
function StrToFloatDef(S:string;Def:extended):extended;
 { Umwandlung String->Extended mit Rückgabe eines Default-Wertes bei Fehlern }
function ValidFileName(DName:string):boolean;
 { Ermittelt, ob es sich um einen gültigen Dateinamen handelt }


{---------------------------------------}
{ Funktionen nur für Delphi 1:          }
{---------------------------------------}

{$IFDEF SR_Delphi1}
procedure DrawEdge(ACanvas:TCanvas;ARect:TRect;Raised:boolean);
 { Zeichnet einen 3D-Rahmen auf der Zeichenfläche ACanvas }
procedure SetFileDate(FName:string;FDate:LongInt);
 { Setzt das Erstellungs-Datum einer Datei }
function Trim(const S:string):string;
 { Entfernt führende und abschließende Leerzeichen aus einem String }
{$ENDIF}


{---------------------------------------}
{ Funktionen nur für alle 32Bit-        }
{ Delphi-Versionen                      }
{---------------------------------------}

{$IFDEF SR_Delphi2_Up}
function ConvertStrToDateTime(s:String):TDateTime;
 { Versucht, einen String in einen Datumswert zu wandeln
   (zuvor muß InitLocale aufgerufen werden) }
function FindAssociatedProgram(DateiName:String):String;
 { Ermittelt das mit einer Dateierweiterung verknüpfte Programm }
function GetExeForProtocol(URL:string):string;
 { Ermittelt das mit einem Übertragungs-Protokoll verknüpfte Programm }
function GetFocussedControl:HWnd;
 { Ermittelt das Fensterelement mit dem Eingabefokus }
function GetLongPathName(APath:String):String;
 { Wandelt einen verkürzten DOS-Dateinamen in einen langen Windows9x-Dateinamen }
function GetSystemFileDescription(FileName:string):string;
 { Liefert die in Windows registrierte Dateibeschreibung zu einem Dateinamen zurück }
function GetSystemWorkArea:TRect;
 { Gibt das Windows-Desktop-Rechteck ohne die Taskbar zurück }
function GetWindowState(WHandle:HWnd):integer;
 { Gibt den Anzeige-Zustand des Fenster mit dem Handle "WHandle" zurück }
function GetWinUsername:string;
 { Ermittelt den aktuell angemeldeten Windows-Benutzer }
procedure InitLocale;
 { Ermittelt die aktuellen Lokalisierungseinstellungen
   (muß vor  ConvertStrToDateTime aufgerufen werden) }
function IsWindowsNT:boolean;
 { Ermittelt ob es sich bei dem Betriebssystem um eine Windows-NT-Version handelt }
procedure SendKeys(AText:string);
 { Sendet einen String als Folge von Tastendrücken an ein Fensterelement }
function SetFileDate(FName:string;FDate:Integer):boolean;
 { Setzt das Erstellungs-Datum einer Datei }
procedure SimulateKeyDown(Key : byte);
 { Sendet eine KeyDown-Nachricht an ein Fensterelement }
procedure SimulateKeystroke(Key:byte; extra:DWORD);
 { Sendet einen vollständigen Tatendruck (KeyDown+KeyUp) an ein Fensterelement }
procedure SimulateKeyUp(Key : byte);
 { Sendet eine KeyUp-Nachricht an ein Fensterelement }
{$ENDIF}


{---------------------------------------}
{ Funktionen nur für bestimmte          }
{ Delphi-Versionen                      }
{---------------------------------------}

{$IFNDEF SR_Delphi4_Up}
procedure FreeAndNil(var Obj);
 { Gibt ein Objekt frei und setzt den Objektzeiger auf NIL (Delphi 1..3) }
{$ENDIF}
{$IFNDEF SR_Delphi3_Up}
function ExtractFileDir(APath:string):string;
 { Gibt wie ExtractFilePath den Pfad eines Dateinamens zurück,
  aber ohne abschließenden Backslash }
function IsLeapYear(AYear: Integer):boolean;
 { Ermittelt, ob ein Jahr ein Schaltjahr ist (Delphi 1..2) }
function LastDelimiter(AChar:char;AText:string):integer;
 { Ermittelt die letzte Position des Zeichens AChar im string AText (Delphi 1..2) }
{$ENDIF}

implementation

uses SysUtils {$IFDEF SR_Delphi2_Up}, Registry, ShellAPI {$ELSE}, Forms, Ver {$ENDIF};

var
  {$IFDEF SR_Delphi2_Up}
  FirstWeekDay  : Integer = 2;  { Wochentag, mit dem die Woche beginnt
                                  (siehe Delphi-Wochentage)
                                  2 : Montag (nach DIN 1355) }
  FirstWeekDate : Integer = 4;  { 1 : Beginnt am ersten Januar
                                  4 : Erste-4 Tage-Woche (nach DIN 1355)
                                  7 : Erste volle Woche }
  {$ELSE}
  FirstWeekDay  : Integer;
  FirstWeekDate : Integer;
  {$ENDIF}
  LocaleIDate,
  LocaleILDate,
  CurrentYear2Digit,
  CurrentCentury    : Integer;

function AddBackslash(Filename:string):string;
begin
  if (length(Filename)>0) and (Filename[length(Filename)]<>'\') then
    Result:=Filename+'\'
  else
    Result:=Filename;
end; {AddBackslash}

{$IFDEF SR_Delphi2_Up}
function ConvertStrToDateTime(s:String):TDateTime;
var
  p,p2     : PChar;
  i1,i2,i3 : Integer;    { einzelne Datumsangaben }
  Mode     : Integer;    { Reihenfolge beim Datum }
  t1,t2    : String;     { Separator }
  t        : String;     { Zeit }

  function GetNumber:Integer;
  var s : String;
  begin
    p:=p2;
    while p2^ in ['0'..'9'] do
      Inc(p2);
    SetString(s,p,p2-p);
    Result:=StrToIntDef(s,-1);
  end; {GetNumber}

  function GetSeparator:String;
  begin
    p:=p2;
    while Not (p2^ in ['0'..'9',#0]) do
      Inc(p2);
    SetString(Result,p,p2-p);
    Result:=Trim(Result);
  end; {GetSeparator}

  procedure ConvertTo4Digit(var AYear:Integer);
  begin
    if AYear in [0..100] then begin
      if AYear>CurrentYear2Digit then
        Dec(AYear,100);
      Inc(AYear,CurrentCentury);
    end;
  end; {ConvertTo4Digit}

begin
  Result:=0;
  p:=Pointer(s);
  if p=Nil then
    Exit;
  p2:=p;
  i1:=GetNumber;
  t1:=GetSeparator;
  i2:=GetNumber;
  t2:=GetSeparator;
  i3:=GetNumber;
  SetString(t,p2,StrLen(p2));
  t:=Trim(t);
  Mode:=-1;
  if (i1<1) or (i1>31) then           { y/m/d }
    Mode:=2
  else begin
    if (i3<1) or (i3>31) then begin   { x/x/y }
      if Not (i1 in [1..31]) then     { m/d/y }
        Mode:=0
      else
        if Not (i2 in [1..31]) then   { d/m/y }
          Mode:=1;
    end
    else
      if i1=i2 then                   { Tag=Monat, Format egal }
        Mode:=1;
  end;
  if Mode<0 then begin                { Format nicht auswertbar }
    if LocaleIDate in [0..1] then
      Mode:=LocaleIDate               { Reihenfolge kurzes Datum }
    else begin
      if LocaleILDate in [0..1] then
        Mode:=LocaleILDate            { Reihenfolge langes Datum }
      else                            // evtl. User befragen
        Mode:=1;
    end;
  end;
  // Jahr auf vierstellig bringen
  case Mode of
    0..1 : ConvertTo4Digit(i3);
    2    : ConvertTo4Digit(i1);
  end;
  // Datum konvertieren
  case Mode of
    0 : Result:=EncodeDate(i3,i1,i2);
    1 : Result:=EncodeDate(i3,i2,i1);
    2 : Result:=EncodeDate(i1,i2,i3);
  end;
  if Length(t)>0 then
    Result:=Result+StrToTime(t);
end; {ConvertStrToDateTime}
{$ENDIF}

function CutBackSlash(FileName:string):string;
begin
  if (length(FileName)>0) and (FileName[length(FileName)]='\') then
    Result:=copy(FileName,1,length(FileName)-1)
  else
    Result:=FileName;
end; {CutBackSlash}

function CutRootDirectory(FName:string):string;
var P : integer;
begin
  P:=Pos(':',FName);
  if (P>0) and (P<length(FName)) then
    delete(FName,1,P+1);
  Result:=FName;
end; {CutRootDirectory}

function DateTimeToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
var DayDiff : integer;
begin
  try
    Result:='';
    if CompareToday then begin
      DayDiff:=trunc(Date)-trunc(ADate);
      if (abs(DayDiff))<=2 then
        Result:=PeriodNames[DayDiff+2]+', '+TimeToStr(frac(ADate));
    end;
    if Result='' then
      Result:=DateTimeToStr(ADate);
  except
    Result:=Default;
  end;
end; {DateTimeToStrDef}

function DateToStrDef(ADate:TDateTime;Default:string;CompareToday:boolean):string;
var DayDiff : integer;
begin
  try
    Result:='';
    if CompareToday then begin
      DayDiff:=trunc(Date)-trunc(ADate);
      if (abs(DayDiff))<=2 then
        Result:=PeriodNames[DayDiff+2];
    end;
    if Result='' then
      Result:=DateToStr(ADate);
  except
    Result:=Default;
  end;
end; {DateToStrDef}

{$IFDEF SR_Delphi1}
procedure DrawEdge(ACanvas:TCanvas;ARect:TRect;Raised:boolean);
begin
  with ACanvas do begin
    if Raised then
      Pen.Color:=clBtnHighlight
    else
      Pen.Color:=clBtnShadow;
    MoveTo(ARect.Right-1,ARect.Top);
    LineTo(ARect.Left,ARect.Top);
    LineTo(ARect.Left,ARect.Bottom-2);
    if Raised then
      Pen.Color:=clBtnShadow
    else
      Pen.Color:=clBtnHighlight;
    MoveTo(ARect.Left,ARect.Bottom-2);
    LineTo(ARect.Right-1,ARect.Bottom-2);
    LineTo(ARect.Right-1,ARect.Top);
    Pen.Color:=clWindowFrame;
    MoveTo(ARect.Left,ARect.Bottom-1);
    LineTo(ARect.Right,ARect.Bottom-1);
    LineTo(ARect.Right,ARect.Top);
  end;
end; {DrawEdge}
{$ENDIF}

function ExecAndWait(const Filename, Params: string;
                     WindowState: word): boolean;
{$IFDEF SR_Delphi2_Up}
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := '"' + Filename + '" ' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(NIL, PChar(CmdLine), NIL, NIL, FALSE, 
                          CREATE_NEW_CONSOLE or 
                          NORMAL_PRIORITY_CLASS, NIL, 
                          PChar(ExtractFilePath(Filename)), 
                          SUInfo, ProcInfo);
  { Wait for it to finish. }
  if Result then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
 
{$ELSE}
var
  InstanceID : THandle;
  Buff: array[0..255] of char;
begin
  StrPCopy(Buff, Filename + ' ' + Params);
  InstanceID := WinExec(Buff, WindowState);
  if InstanceID < 32 then
  { a value less than 32 indicates an Exec error }
    Result := FALSE
  else begin
    Result := TRUE;
    repeat
      Application.ProcessMessages;
    until Application.Terminated or
          (GetModuleUsage(InstanceID) = 0);
  end;
{$ENDIF}
end;

function ExpandString(S:string;AChar:char;ALength:word):string;
begin
  while length(S)<ALength do
    S:=AChar+S;
  Result:=S;
end; {ExpandString}

{$IFNDEF SR_Delphi3_Up}
function ExtractFileDir(APath:string):string;
begin
  Result:=CutBackslash(ExtractFilePath(APath));
end; {ExtractFileDir}
{$ENDIF}

function ExtractRawFileName(DName:string):string;
begin
  Result:=ChangeFileExt(ExtractFileName(DName),'');
end; {ExtractRawFileName}

{$IFDEF SR_Delphi2_Up}
function FindAssociatedProgram(DateiName:String):String;
var Reg  : TRegistry;
    Res  : boolean;
    AKey : string;
    i    : integer;
begin
  Result:='';
  {$IFDEF SR_Delphi4_Up}
  Reg := TRegistry.Create(Key_Read);
  {$ELSE}
  Reg := TRegistry.Create;
  {$ENDIF}
  try
    Reg.RootKey := HKEY_CLASSES_ROOT;
    Res:=Reg.OpenKey(ExtractFileExt(DateiName), false);
    if Res then begin
      AKey:=Reg.ReadString('');
      Reg.CloseKey;
      if AKey<>'' then begin
        Res:=Reg.OpenKey(AKey+'\shell\open\command', false);
        if Res then begin
          Result:=Reg.ReadString('');
          for i:=length(Result) downto 1 do
            if Result[i]='"' then
              delete(Result, i, 1);
          i:=Pos(LowerCase('.exe'), LowerCase(Result));
          if i>0 then
            delete(Result, i+4, length(Result)-i-3);
          Result:=GetLongPathName(Result);
          Reg.CloseKey;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end; {FindAssociatedProgram}
{$ENDIF}

{$IFNDEF SR_Delphi4_Up}
procedure FreeAndNil(var Obj);
var P : TObject;
begin
  P:=TObject(Obj);
  TObject(Obj):=nil;
  P.Free;
end; {FreeAndNil}
{$ENDIF}

function GetBuildInfo(const AFilename:String; var V1,V2,V3,V4:Word):Boolean;
var
  VerInfoSize  : Integer;
  {$IFDEF SR_Delphi2_Up}
  VerValueSize : DWord;
  Dummy        : DWord;
  VerValue     : PVSFixedFileInfo;
  {$ELSE}
  VerValueSize : Word;
  Dummy        : LongInt;
  VerValue     : ^TVS_FixedFileInfo;
  {$ENDIF}
  VerInfo      : Pointer;
  FName        : PChar;
begin
  FName:=StrAlloc(Max_Path);
  try
    StrPCopy(FName,AFileName);
    VerInfoSize:=GetFileVersionInfoSize(FName,Dummy);
    Result:=False;
    if VerInfoSize>0 then begin
      GetMem(VerInfo,VerInfoSize);
      try
        if GetFileVersionInfo(FName,0,VerInfoSize,VerInfo) then begin
          if VerQueryValue(VerInfo,'\',Pointer(VerValue),VerValueSize) then
           with VerValue^ do begin
            V1:=dwFileVersionMS shr 16;
            V2:=dwFileVersionMS and $FFFF;
            V3:=dwFileVersionLS shr 16;
            V4:=dwFileVersionLS and $FFFF;
          end;
          Result:=True;
        end;
      finally
        FreeMem(VerInfo,VerInfoSize);
      end;
    end;
  finally
    StrDispose(FName);
  end;
end; {GetBuildInfo}

function GetDayFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate, Y, M, D);
  except
    D:=0;
  end;
  Result:=D;
end; {GetDayFromDate}

function GetDayOfYear(ADate:TDateTime):word;
{ liefert den Tag im Jahr }
var T,M,J  : word;
    Erster : TDateTime;
begin
  try
    DecodeDate(ADate,J,M,T);
    Erster:=EncodeDate(J,1,1);
    Result:=trunc(ADate-Erster+1);
  except
    Result:=0;
  end;
end; {GetDayOfYear}

function GetDaysPerMonth(AYear,AMonth:integer):integer;
const
  DaysInMonth: array [1..12] of Integer =
   (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
begin
  Result:=DaysInMonth[AMonth];
  if (AMonth=2) and IsLeapYear(AYear) then
    Inc(Result);
end; {GetDaysPerMonth}

{$IFDEF SR_Delphi2_Up}
function GetExeForProtocol(URL:string):string;
var Reg  : TRegistry;
    Res  : boolean;
    Temp : string;
    P    : integer;
begin
  Result:='';
  P:=Pos(':', URL);
  if P>1 then
    delete(URL, P, length(URL)-P+1);
  {$IFDEF SR_Delphi4_Up}
  Reg := TRegistry.Create(Key_Read);
  {$ELSE}
  Reg := TRegistry.Create;
  {$ENDIF}
  try
    Reg.Rootkey:=HKEY_CLASSES_ROOT;
    Res:=Reg.OpenKey(URL+'\shell\open\command', false);
    if Res then begin
      Temp:=Reg.ReadString('');
      while (length(Temp)>0) and ((Temp[1]='"') or (Temp[1]=' ')) do
        delete(Temp, 1, 1);
      P:=Pos('"', Temp);
      if P>0 then
        delete(Temp, P, length(Temp)-P+1);
      Result:=Temp;
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end; {GetExeForProtocol}
{$ENDIF}

function GetFileSize(FileName:string;AFormat:TFileSizeFormat):integer;
var SR : TSearchRec;
begin
  if FindFirst(FileName, faAnyFile, SR)=0 then begin
    Result:=SR.Size;
    if AFormat=fsKilobyte then
      Result:=Result div 1024;
    if AFormat=fsMegabyte then
      Result:=Result div (1024*1024);
    FindClose(SR);
  end
  else
    Result:=-1;
end; {GetFileSize}

function GetFirstDayOfWeek(ADate:TDateTime):TDateTime;
begin
  while DayOfWeek(ADate)<>1 do
    ADate:=ADate-1;
  Result:=ADate;
end; {GetFirstDayOfWeek}

function GetFirstPartOfString(var AText:string;Delimiter:char;IncludeDelimiter:boolean):string;
var P : integer;
begin
  P:=Pos(Delimiter,AText);
  if P>0 then begin
    if IncludeDelimiter then
      Result:=copy(AText,1,P)
    else
      Result:=copy(AText,1,P-1);
    delete(AText,1,P);
  end
  else
    Result:=AText;
end; {GetFirstPartOfString}

function GetHourFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    H:=0;
  end;
  Result:=H;
end; {GetHourFromTime}

function GetMinuteFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    M:=0;
  end;
  Result:=M;
end; {GetMinuteFromTime}

{$IFDEF SR_Delphi2_Up}
function GetFocussedControl:HWnd;
var OtherThreadID,
    Buffer        : DWord;
    ParentWnd     : HWnd;
begin
  Result:=0;
  ParentWnd:=GetForegroundWindow;
  if ParentWnd<>0 then begin
    OtherThreadID:=GetWindowThreadProcessID(ParentWnd, @Buffer);
    if AttachThreadInput(GetCurrentThreadID, OtherThreadID, true) then begin
      Result:=GetFocus;
      AttachThreadInput(GetCurrentThreadID, OtherThreadID, false);
    end;
  end;
end; {GetFocussedControl}

function GetLongPathName(APath:String):String;
var
  i : Integer;
  h : THandle;
  Data : TWin32FindData;
  IsBackSlash : Boolean;
begin
  APath:=ExpandFileName(APath);
  i:=Pos('\',APath);
  Result:=Copy(APath,1,i);
  Delete(APath,1,i);
  repeat
    i:=Pos('\',APath);
    IsBackSlash:=i>0;
    if Not IsBackSlash then
      i:=Length(APath)+1;
    h:=FindFirstFile(PChar(Result+Copy(APath,1,i-1)),Data);
    if h<>INVALID_HANDLE_VALUE then begin
      try
        Result:=Result+Data.cFileName;
        if IsBackSlash then
          Result:=Result+'\';
      finally
        Windows.FindClose(h);
      end;
    end
    else begin
      Result:=Result+APath;
      Exit;
    end;
    Delete(APath,1,i);
  until Length(APath)=0;
end; {GetLongPathName}
{$ENDIF}

function GetMonthFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate,Y,M,D);
  except
    M:=0;
  end;
  Result:=M;
end; {GetMonthFromDate}

function GetSecondFromTime(ATime:TDateTime):byte;
var H,M,S,MS : word;
begin
  try
    DecodeTime(ATime, H, M, S, MS);
  except
    S:=0;
  end;
  Result:=S;
end; {GetSecondFromTime}

function GetShiftState:TShiftState;
const Key_Pressed = 65535;
{$IFDEF SR_Delphi1}
var AState : integer;
{$ELSE}
var AState : short;
{$ENDIF}
begin
  Result:=[];
  AState:=GetAsyncKeyState(VK_Shift);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssShift);
  AState:=GetAsyncKeyState(VK_Menu);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssAlt);
  AState:=GetAsyncKeyState(VK_Control);
  if (AState and Key_Pressed)>0 then
    Include(Result, ssCtrl);
end; {GetShiftState}

function GetSystemDir:string;
var SysDir : array [0..Max_Path] of char;
begin
  GetSystemDirectory(SysDir,Max_Path);
  Result:=AddBackSlash(String(SysDir));
end; {GetSystemDir}

{$IFDEF SR_Delphi2_Up}
function GetSystemFileDescription(FileName:string):string;
var
 SysIL : UInt;
 Info  : TSHFileInfo;
begin
  SysIL:=SHGetFileInfo(PChar(FileName), 0, Info, SizeOf(TSHFileInfo), SHGFI_TYPENAME);
  if SysIL<>0 then
    Result:=Info.szTypeName
  else
    Result:='';
end; {GetSystemFileDescr}

function GetSystemWorkArea:TRect;
var PRect : ^TRect;
begin
  Result:=Rect(0, 0, 0, 0);
  GetMem(PRect, SizeOf(TRect));
  try
    if SystemParametersInfo(SPI_GetWorkArea, 0, PRect, 0) then begin
      Result:=PRect^;
    end;
  finally
    FreeMem(PRect);
  end;
end; {GetSystemWorkArea}
{$ENDIF}

function GetVersionNr(ExeName:string;BuildNr:boolean):string;
var V1,V2,V3,V4 : Word;
begin
  if GetBuildInfo(ExeName, V1, V2, V3, V4) then begin
    if BuildNr then
      Result:=IntToStr(V1)+'.'+IntToStr(V2)+IntToStr(V3)+' (Build '+IntToStr(V4)+')'
    else
      Result:=IntToStr(V1)+'.'+IntToStr(V2)+IntToStr(V3);
  end
  else
    Result:='';
end; {GetVersionNr}

function GetWeekOfYear(ADate:TDateTime):byte;
var Year,Month,Day : Word;
begin
  ADate:=ADate-((DayOfWeek(ADate)-FirstWeekDay+7) mod 7)+ 7-FirstWeekDate;
  DecodeDate(ADate, Year, Month, Day);
  Result:=(Trunc(ADate-EncodeDate(Year,1,1)) div 7)+1;
end; {GetWeekOfYear}

function GetWeeksPerYear(AYear:word):byte;
var AWeek : byte;
begin
  AWeek:=GetWeekOfYear(EncodeDate(AYear,12,31));
  if AWeek=1 then
    Result:=52
  else
    Result:=AWeek;
end; {GetWeeksPerYear}

function GetWindowsDir:string;
var WinDir : array [0..Max_Path] of char;
begin
  GetWindowsDirectory(WinDir,Max_Path);
  Result:=AddBackSlash(String(WinDir));
end; {GetWindowsDir}

{$IFDEF SR_Delphi2_Up}
function GetWindowState(WHandle:HWnd):integer;
{$IFNDEF SR_Delphi4_Up}
var PPlcmnt : TWindowPlacement;
{$ELSE}
var PPlcmnt : WindowPlacement;
{$ENDIF}

  { Die Rückgabewerte der Funktion entsprechen folgenden Konstanten
    aus der Unit Windows.pas:

    SW_HIDE = 0;
    SW_SHOWNORMAL = 1;
    SW_SHOWMINIMIZED = 2;
    SW_SHOWMAXIMIZED = 3;
    SW_SHOWNOACTIVATE = 4;
    SW_SHOW = 5;
    SW_MINIMIZE = 6;
    SW_SHOWMINNOACTIVE = 7;
    SW_SHOWNA = 8;
    SW_RESTORE = 9;
    SW_SHOWDEFAULT = 10; }

begin
  {$IFNDEF SR_Delphi4_Up}
  PPlcmnt.Length:=SizeOf(TWindowPlacement);
  {$ELSE}
  PPlcmnt.Length:=SizeOf(WindowPlacement);
  {$ENDIF}
  if GetWindowPlacement(WHandle, @PPlcmnt) then
    Result:=PPlcmnt.ShowCmd
  else
    Result:=-1;
end; {GetWindowState}

function GetWinUsername:string;
var UName : PChar;
    USize : DWord;
begin
  USize:=Max_Path;
  UName:=StrAlloc(USize);
  try
    GetUserName(UName,USize);
    Result:=string(UName);
  finally
    StrDispose(UName);
  end;
end; {GetWinUsername}
{$ENDIF}

function GetYearFromDate(ADate:TDateTime):word;
var Y,M,D : word;
begin
  try
    Decodedate(ADate, Y, M, D);
  except
    Y:=0;
  end;
  Result:=Y;
end; {GetYearFromDate}

{$IFDEF SR_Delphi2_Up}
procedure InitLocale;
var SystemTime: TSystemTime;

  function GetLocaleInt(AInfo:LCTYPE):Integer;
  var
    Buffer: array[0..1] of Char;
  begin
    if GetLocaleInfo(GetThreadLocale, AInfo, Buffer, 2) > 0 then
      Result:=Ord(Buffer[0])-Ord('0')
    else
      Result:=-1;
  end; {GetLocaleInt}

begin
  LocaleIDate :=GetLocaleInt(LOCALE_IDATE);
  LocaleILDate:=GetLocaleInt(LOCALE_ILDATE);
  GetLocalTime(SystemTime);
  CurrentYear2Digit:=SystemTime.wYear mod 100;
  CurrentCentury:=SystemTime.wYear-CurrentYear2Digit;
end; {InitLocale}
{$ENDIF}

function IntToStrFixed(IntValue:integer;OutDigits:byte):string;
begin
  try
    Result:=IntToStr(Abs(IntValue));
    while (length(Result)<OutDigits) do
      Result:='0'+Result;
    if (IntValue<0) then
      Result:='-'+Result;
  except
    Result:='';
  end;
end; {IntToStrFixed}

{$IFNDEF SR_Delphi3_Up}
function IsLeapYear(AYear:integer):boolean;
begin
  Result:=(AYear mod 4=0) and ((AYear mod 100<>0) or (AYear mod 400=0));
end; {IsLeapYear}
{$ENDIF}

function IsSummertime(ADate:TDateTime):boolean;
{ Ermmittelt, ob ein Datum in der Sommerzeit liegt }
var AYear,
    AMonth,
    ADay   : word;
    Beginn,
    Ende   : TDateTime;
begin
  try
    DecodeDate(ADate, AYear, AMonth, ADay);
    if AYear<1980 then
      { Keine Sommerzeit vor 1980 }
      Result:=false
    else begin
      { Beginn der Sommerzeit: }
      Beginn:=EncodeDate(AYear, 3, 31);
      while DayOfWeek(Beginn)<>1 do
        Beginn:=Beginn-1;
      { Ende der Sommerzeit: }
      if AYear<=1995 then
        { bis 1995: letzter So im September }
        Ende:=EncodeDate(AYear, 9, 30)
      else
        { ab 1996: letzter So im Oktober }
        Ende:=EncodeDate(AYear, 10, 31);
      while DayOfWeek(Ende)<>1 do
        Ende:=Ende-1;
      Result:=(ADate>=Beginn) and (ADate<Ende);
    end;
  except
    Result:=false;
  end;
end; {IsSummertime}

{$IFDEF SR_Delphi2_Up}
function IsWindowsNT:boolean;
var OsVinfo : TOSVERSIONINFO;
begin
  ZeroMemory(@OsVinfo,sizeOf(OsVinfo));
  OsVinfo.dwOSVersionInfoSize := sizeof(TOSVERSIONINFO);
  if GetVersionEx(OsVinfo) then
    Result:=OsVinfo.dwPlatformId = VER_PLATFORM_WIN32_NT
  else
    Result:=false;
end; {IsWindowsNT}
{$ENDIF}

{$IFNDEF SR_Delphi3_Up}
function LastDelimiter(AChar:char;AText:string):integer;
var i : integer;
begin
  Result:=0;
  if length(AText)=0 then
    Exit;
  for i:=length(AText) downto 1 do begin
    if AText[i]=AChar then begin
      Result:=i;
      Exit;
    end;
  end;
end;
{$ENDIF}

function ReverseString(AText:string):string;
var i : byte;
begin
  Result:='';
  for i:=length(AText) downto 1 do
    Result:=Result+AText[i];
end; {ReverseString}

function RGBToStr(RGBColor:integer):string;
var ColText : string;
begin
  ColText:=IntToHex(RGBColor,6);
  Result:=copy(ColText,5,2)+copy(ColText,3,2)+copy(ColText,1,2);
end; {RGBToStr}

{$IFDEF SR_Delphi2_Up}
procedure SendKeys(AText:string);
var i : integer;
    w : word;
begin
  for i:=1 to Length(AText) do begin
    w:=VkKeyScan(AText[i]);
    if ((HiByte(w)<>$FF) and (LoByte(w)<>$FF)) then begin
      {If the key requires the shift key down - hold it down}
      if HiByte(w) and 1 = 1 then
        SimulateKeyDown(VK_SHIFT);
      {Send the VK_KEY}
      SimulateKeystroke(LoByte(w), 0);
      {If the key required the shift key down - release it}
      if HiByte(w) and 1 = 1 then
        SimulateKeyUp(VK_SHIFT);
    end;
  end;
end; {SendKeys}
{$ENDIF}

{$IFDEF SR_Delphi1}
procedure SetFileDate(FName:string;FDate:LongInt);
var F : TFileStream;
begin
  try
    F:=TFileStream.Create(FName,fmOpenWrite);
    FileSetDate(F.Handle,FDate);
  finally
    F.Free;
  end;
end;
{$ELSE}
function SetFileDate(FName:string;FDate:Integer):boolean;
var F : TFileStream;
begin
  try
    F:=TFileStream.Create(FName,fmOpenWrite);
    Result:=(FileSetDate(F.Handle,FDate)=0);
    F.Free;
  except
    Result:=false;
  end;
end; {SetFileDate}
{$ENDIF}

{$IFDEF SR_Delphi2_Up}
procedure SimulateKeyDown(Key : byte);
begin
  Keybd_Event(Key, 0, 0, 0);
end; {SimulateKeyDown}

procedure SimulateKeystroke(Key:byte; extra:DWORD);
begin
  Keybd_Event(Key, extra, 0, 0);
  Keybd_Event(Key, extra, KEYEVENTF_KEYUP, 0);
end; {SimulateKeystroke}

procedure SimulateKeyUp(Key : byte);
begin
  Keybd_Event(Key, 0, KEYEVENTF_KEYUP, 0);
end; {SimulateKeyUp}
{$ENDIF}

function StripForbiddenChars(AText:string):string;
var i : integer;
begin
  if length(AText)>0 then
    for i:=length(AText) downto 0 do
      {$IFDEF SR_Delphi1}
      if AText[i] in ShortForbiddenChars then
        delete(AText,i,1);
      {$ELSE}
      if AText[i] in LongForbiddenChars then
        delete(AText,i,1);
      {$ENDIF}
  Result:=AText;
end; {StripForbiddenChars}

function StrToDateDef(S:string;Def:TDateTime):TDateTime;
begin
  try
    Result:=StrToDate(S);
  except
    Result:=Def;
  end;
end; {StrToDateDef}

function StrToDateTimeDef(S:string;Def:TDateTime):TDateTime;
begin
  try
    Result:=StrToDateTime(S);
  except
    Result:=Def;
  end;
end; {StrToDateTimeDef}

function StrToFloatDef(S:string;Def:extended):extended;
begin
  try
    Result:=StrToFloat(S);
  except
    Result:=Def;
  end;
end; {StrToFloatDef}

{$IFDEF SR_Delphi1}
function Trim(const S:string):string;
var i,L: Integer;
begin
  L:=length(S);
  i:=1;
  while (i<=L) and (S[i]<=' ') do
    inc(i);
  if i>L then
    Result:=''
  else begin
    while S[L]<=' ' do
      dec(L);
    Result:=Copy(S, i, L-i+1);
  end;
end; {Trim}
{$ENDIF}

function ValidFileName(DName:string):boolean;
var i : integer;
begin
  Result:=true;
  for i:=1 to length(DName) do
    {$IFDEF SR_Delphi1}
    if DName[i] in ShortForbiddenChars then
      Result:=false;
    {$ELSE}
    if DName[i] in LongForbiddenChars then
      Result:=false;
    {$ENDIF}
end; {ValidFileName}

{$IFDEF SR_Delphi1}
initialization

FirstWeekDay  := 2;  { Wochentag, mit dem die Woche beginnt
                       (siehe Delphi-Wochentage)
                       2 : Montag (nach DIN 1355) }
FirstWeekDate := 4;  { 1 : Beginnt am ersten Januar
                       4 : Erste-4 Tage-Woche (nach DIN 1355)
                       7 : Erste volle Woche }
{$ENDIF}

end.
