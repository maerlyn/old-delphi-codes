unit SRCal;

{ TSRCalendar (C)opyright 2000 Version 1.36
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TSRCalendar ist eine Weiterentwicklung der
  TCalendar-Komponente aus den Beispielkomponenten der Delphi-VCL.
  Sie enthält viele Zusatzinformationen, wie Feiertage,
  Sternzeichen und verschiedene astronomische Daten.

  Die Routinen aus der Unit TimeFunc stammen aus der TMoon-Komponente
  von Andreas Hörstemeier : http://www.westend.de/~hoerstemeier/index_d.htm
  Andreas hat die Routinen aus dem Buch "Astronomical Algorithms" von Jean Meeus.

  Die GetWeekOfYear-Funktion, die die Wochennummer nach DIN 1355 ermittelt,
  stammt von Christoph Kremer, Aachen.

  Vielen Dank auch an:
  - Edmund Matzke <edmund_matzke@gmx.de> für die Korrektur der
    Schleswig-Holsteinischen Feiertage,
  - Matthias Frey <info@Matthias-Frey.de> für die Korrektur der
    Advents-Berechnung.
  - Robert Rossmair für seine rrColors-Unit!

  TSRClock ist die zum Kalender passende Uhr..

  Diese Komponenten sind Public Domain, das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

uses {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, Menus, {$ENDIF}
  Classes, Controls, Messages, Forms, Graphics, StdCtrls, Grids, SysUtils;

const
  Feiertage : array [1..19] of string[25] =
   ('Neujahr','Maifeiertag','Tag der deutschen Einheit','Allerheiligen',
    'Totensonntag','Volkstrauertag','1. Weihnachtstag','2. Weihnachtstag',
    'Karfreitag','Ostersonntag','Ostermontag','Christi Himmelfahrt',
    'Pfingstsonntag','Pfingstmontag','Fronleichnam','Heilige 3 Könige',
    'Mariä Himmelfahrt','Reformationstag','Buß- und Bettag');
  Sondertage : array [1..24] of string[25] =
   ('Mariä Lichtmeß','Valentinstag','Weiberfastnacht','Rosenmontag','Fastnacht',
    'Aschermittwoch','Mariä Verkündigung','Palmsonntag','Gründonnerstag','Muttertag',
    'Peter und Paul','Mariä Geburt','Erntedankfest','Mariä Empfängnis','Silvester',
    '1. Advent','2. Advent','3. Advent','4. Advent','Heiligabend','Frühlingsanfang',
    'Sommmeranfang','Herbstanfang','Winteranfang');
  SternzNamen : array [0..11] of string[10] =
   ('Wassermann','Fische','Widder','Stier','Zwilling','Krebs','Löwe','Jungfrau',
    'Waage','Skorpion','Schütze','Steinbock');
  Bundeslaender : array [0..15] of string[25] =
   ('Baden-Württemberg','Bayern','Berlin','Brandenburg','Bremen','Hamburg',
    'Hessen','Mecklenburg-Vorpommern','Niedersachsen','Nordrhein-Westfalen',
    'Rheinland-Pfalz','Saarland','Sachsen','Sachsen-Anhalt','Schleswig-Holstein',
    'Thüringen');
  Laenge : array [0..15] of extended =
   (-9,-11.5,-13.4,-13.4,-8.8,-10,-8.7,-12.2,-8.8,-7.5,-7.3,-7,-14,-11.7,-10.2,-11);
  Breite : array [0..15] of extended =
   (48.6,48.8,52.5,52.5,53.1,53.5,50.5,53.7,53.1,51.6,50.2,49.2,51,52,54.3,51);

type
  TBundesland =
   (Baden_Wuerttemberg,Bayern,Berlin,Brandenburg,Bremen,Hamburg,
    Hessen,Mecklenburg_Vorpommern,Niedersachsen,Nordrhein_Westfalen,
    Rheinland_Pfalz,Saarland,Sachsen,Sachsen_Anhalt,Schleswig_Holstein,
    Thueringen);
  {$IFDEF SR_Delphi1}
  TCalendarDrawStyle = (cdsColorGrid, cdsMonoGrid);
  {$ELSE}
  TCalendarDrawStyle = (cdsColorGrid, cdsMonoGrid, cdsButtons);
  {$ENDIF}
  TDayOfWeek = 0..6;
  THolidays = array [1..31] of integer;
  TMarked = array [1..31] of boolean;
  TMoonPhase = (Neumond,zunehmend,Vollmond,abnehmend);

{$IFDEF SR_Delphi2_Up}
  TClockStyle = (csClassic, csDigital, csMovingPoints, csPieSlice);
  TClockKind = (ckRealTime, ckStopWatch);
  TContrast = 0..9;
  TNumbers = (snAll, snNone, snQuarters);
  TTime = TDateTime;

  TThreadTimer = class;

  TTimerThread = class(TThread)
    OwnerTimer: TThreadTimer;
    procedure Execute; override;
  end;

  TThreadTimer = class(TComponent)
  private
    FEnabled        : boolean;
    FInterval       : word;
    FOnTimer        : TNotifyEvent;
    FTimerThread    : TTimerThread;
    FThreadPriority : TThreadPriority;

    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: word);
    procedure SetThreadPriority(Value: TThreadPriority);
    procedure Timer; dynamic;

  protected
    procedure UpdateTimer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property TimerThread: TTimerThread read FTimerThread write FTimerThread;

  published
    property Enabled: boolean read FEnabled write SetEnabled default True;
    property Interval: word read FInterval write SetInterval default 250;
    property Priority: TThreadPriority read FThreadPriority write SetThreadPriority default tpNormal;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;
{$ENDIF}

  TSRCalendar = class(TCustomGrid)
  private
    FAstronomicalData    : boolean;
    FAutoDeleteMarks     : boolean;
    FBGColorHoliday      : TColor;
    FBGColorMarked       : TColor;
    FBGColorSelected     : TColor;
    FBGColorToday        : TColor;
    FBGColorWeekend      : TColor;
    FBundesland          : TBundesland;
    FDate                : TDateTime;
    FDaysThisMonth       : integer;
    FDrawStyle           : TCalendarDrawStyle;
    FFrameColorSelected  : TColor;
    FHoliday             : string;
    FHolidayNr           : integer;
    FHolidays            : THolidays;
    FMarked              : TMarked;
    FMonthOffset         : Integer;
    FMoonDistance        : extended;
    FMoonPhase           : TMoonPhase;
    FMoonRise            : TDateTime;
    FMoonSet             : TDateTime;
    FMoonTransit         : TDateTime;
    FOnBeforeChange      : TNotifyEvent;
    FOnChange            : TNotifyEvent;
    FOnMonthChange       : TNotifyEvent;
    FReadOnly            : Boolean;
    FShowHolidays        : boolean;
    FShowMarks           : boolean;
    FStartOfWeek         : TDayOfWeek;
    FSternzeichen        : string;
    FSternzeichenNr      : integer;
    FSunDistance         : extended;
    FSunRise             : TDateTime;
    FSunSet              : TDateTime;
    FSunTransit          : TDateTime;
    FTextColorHoliday    : TColor;
    FTextColorMarked     : TColor;
    FTextColorStandard   : TColor;
    FTextColorToday      : TColor;
    FTextColorWeekend    : TColor;
    FUpdating            : Boolean;
    FUseCurrentDate      : Boolean;
    FWeekOfYear          : integer;
    FDayOfYear           : integer;

    function GetCellText(ACol, ARow: Integer): string;
    function GetDateElement(Index: Integer): Integer;
    function GetHolidays(Index: integer): integer;
    function GetMarked(Index: integer): boolean;
    procedure GetMoonData(Dat:TDateTime);
    function GetSternzeichenNr(Dat:TDateTime):integer;
    procedure GetSunData(Dat:TDateTime);
    procedure SetBGColorHoliday(newColor: TColor);
    procedure SetBGColorMarked(newColor: TColor);
    procedure SetBGColorSelected(newColor: TColor);
    procedure SetBGColorToday(newColor: TColor);
    procedure SetBGColorWeekend(newColor: TColor);
    procedure SetBundesland(NewValue: TBundesland);
    procedure SetDate(Value: TDateTime);
    procedure SetDateElement(Index: Integer; Value: Integer);
    procedure SetDrawStyle(newValue: TCalendarDrawStyle);
    procedure SetFrameColorSelected(newColor: TColor);
    procedure SetHolidays(Index: integer; newValue: integer);
    procedure SetMarked(Index: integer; newValue: boolean);
    procedure SetShowHolidays(newValue: boolean);
    procedure SetShowMarks(newValue: boolean);
    procedure SetStartOfWeek(Value: TDayOfWeek);
    procedure SetTextColorHoliday(newColor: TColor);
    procedure SetTextColorMarked(newColor: TColor);
    procedure SetTextColorStandard(newColor: TColor);
    procedure SetTextColorToday(newColor: TColor);
    procedure SetTextColorWeekend(newColor: TColor);
    procedure SetUseCurrentDate(Value: Boolean);
    function StoreDate: Boolean;

  protected
    procedure BeforeChange; dynamic;
    procedure Change; dynamic;
    procedure ChangeMonth(Delta: Integer);
    procedure Click; override;
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    function GetDaysThisMonth: Integer; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MonthChange; dynamic;
    function SelectCell(ACol, ARow: Longint): Boolean; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

  public
    constructor Create(AOwner: TComponent); override;
    property CellText[ACol, ARow: Integer]: string read GetCellText;
    property Date: TDateTime  read FDate write SetDate stored StoreDate;
    property DayOfYear: integer read FDayOfYear;
    property DaysThisMonth: integer read FDaysThisMonth;
    function GetHoliday(WhatDate:TDateTime;Land:integer):integer;
    property Holiday: string read FHoliday;
    property HolidayNr: integer read FHolidayNr;
    property Holidays[Index: integer]: integer read GetHolidays write SetHolidays;
    property Marked[Index: integer]: boolean read GetMarked write SetMarked;
    property MoonDistance: extended read FMoonDistance;
    property MoonPhase: TMoonPhase read FMoonPhase;
    property MoonRise: TDateTime read FMoonRise;
    property MoonSet: TDateTime read FMoonSet;
    property MoonTransit: TDateTime read FMoonTransit;
    procedure MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
    function MouseToDate(X, Y: Integer):TDateTime;
    procedure NextMonth;
    procedure NextYear;
    procedure PrevMonth;
    procedure PrevYear;
    property SunDistance: extended read FSunDistance;
    property SunRise: TDateTime read FSunRise;
    property SunSet: TDateTime read FSunSet;
    property SunTransit: TDateTime read FSunTransit;
    property Sternzeichen: string read FSternzeichen;
    property SternzeichenNr: integer read FSternzeichenNr;
    procedure UpdateCalendar; virtual;
    property WeekOfYear: integer read FWeekOfYear;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AstronomicalData: boolean read FAstronomicalData write FAstronomicalData;
    property AutoDeleteMarks: boolean read FAutoDeleteMarks write FAutoDeleteMarks;
    property BGColorHoliday: TColor read FBGColorHoliday write SetBGColorHoliday;
    property BGColorMarked: TColor read FBGColorMarked write SetBGColorMarked;
    property BGColorSelected: TColor read FBGColorSelected write SetBGColorSelected;
    property BGColorToday: TColor read FBGColorToday write SetBGColorToday;
    property BGColorWeekend: TColor read FBGColorWeekend write SetBGColorWeekend;
    property BorderStyle;
    property Bundesland: TBundesland read FBundesland write SetBundesland;
    property Color;
    property Ctl3D;
    property Day: Integer index 3  read GetDateElement write SetDateElement stored False;
    property DrawStyle: TCalendarDrawStyle read FDrawStyle write SetDrawStyle;
    property Enabled;
    property Font;
    property FrameColorSelected: TColor read FFrameColorSelected write SetFrameColorSelected;
    property GridLineWidth;
    property Month: Integer index 2  read GetDateElement write SetDateElement stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ShowHint;
    property ShowHolidays: boolean read FShowHolidays write SetShowHolidays;
    property ShowMarks: boolean read FShowMarks write SetShowMarks;
    property StartOfWeek: TDayOfWeek read FStartOfWeek write SetStartOfWeek;
    property TabOrder;
    property TabStop;
    property TextColorHoliday: TColor read FTextColorHoliday write SetTextColorHoliday;
    property TextColorMarked: TColor read FTextColorMarked write SetTextColorMarked;
    property TextColorStandard: TColor read FTextColorStandard write SetTextColorStandard;
    property TextColorToday: TColor read FTextColorToday write SetTextColorToday;
    property TextColorWeekend: TColor read FTextColorWeekend write SetTextColorWeekend;
    property UseCurrentDate: Boolean read FUseCurrentDate write SetUseCurrentDate default True;
    property Visible;
    property Year: Integer index 1  read GetDateElement write SetDateElement stored False;
    property OnBeforeChange: TNotifyEvent read FOnBeforeChange write FOnBeforeChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    {$IFDEF SR_Delphi5_Up}
    property OnEndDock;
    {$ENDIF}
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMonthChange: TNotifyEvent read FOnMonthChange write FOnMonthChange;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF SR_Delphi5_Up}
    property OnStartDock;
    {$ENDIF}
    {$IFDEF SR_Delphi2_Up}
    property OnStartDrag;
    {$ENDIF}
  end;

{$IFDEF SR_Delphi2_Up}
  TSRClock = class(TGraphicControl)
  private
    FAutoUpdate           : boolean;
    FBorderWidth          : integer;
    FColorBackground,
    FColorBorder,
    FColorHands,
    FColorNumbers,
    FColorSegments        : TColor;
    FDigit                : array [0..9] of TBitmap;
    FFadingColor          : boolean;
    FHour,FMinute,FSecond : word;
    FKind                 : TClockKind;
    FLEDContrast          : TContrast;
    FLineWidth            : integer;
    FPriority             : TThreadPriority;
    FOldWidth,FOldHeight  : integer;
    FRunning              : boolean;
    FSegCl                : array [0..9, 1..7] of TColor;
    FShowNumbers          : TNumbers;
    FShowSeconds,
    FShowTicks            : boolean;
    FSummertime           : boolean;
    FStyle                : TClockStyle;
    FTime                 : TTime;
    FTimeOffset           : double;
    FUpdateInterval       : word;

    FOnMouseEnter,
    FOnMouseExit,
    FOnTimer              : TNotifyEvent;

    Timer                 : TThreadTimer;
    Buffer                : TBitmap;

    function  GetPriority: TThreadPriority;
    procedure SetAutoUpdate(Value: boolean);
    procedure SetBorderWidth(Value: integer);
    procedure SetColorBackground(Value: TColor);
    procedure SetColorBorder(Value: TColor);
    procedure SetColorNumbers(Value: TColor);
    procedure SetColorHands(Value: TColor);
    procedure SetFadingColor(Value: boolean);
    procedure SetKind(Value: TClockKind);
    procedure SetLEDContrast(Value : TContrast);
    procedure SetLineWidth (Value: integer);
    procedure SetPriority(Value: TThreadPriority);
    procedure SetShowNumbers(Value: TNumbers);
    procedure SetShowSeconds(Value: boolean);
    procedure SetShowTicks(Value: boolean);
    procedure SetStyle(Value: TClockStyle);
    procedure SetTime(Value: TTime);
    procedure SetUpdateInterval(Value: word);

    procedure AssignColors (seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);
    procedure GenerateBitMaps(AWidth, AHeight: integer);

  protected
    procedure Paint;  override;
    procedure Loaded; override;
    procedure AutoUpdateClock(Sender: TObject);
    procedure CmEnabledChanged(var Message: TWmNoParams); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CmVisibleChanged(var Message: TWmNoParams); message CM_VISIBLECHANGED;

  public
    property Hour: word read FHour;
    property Minute: word read FMinute;
    property Second: word read FSecond;
    property Time: TTime read FTime write SetTime;
    property Summertime: boolean read FSummertime;

    procedure Reset;
    procedure Start;
    procedure Stop;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Align;
    {$IFDEF SR_Delphi5_Up}
    property Anchors;
    {$ENDIF}
    property AutoUpdate: boolean read FAutoUpdate write SetAutoUpdate;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property ColorBackground: TColor read FColorBackground write SetColorBackground;
    property ColorBorder: TColor read FColorBorder write SetColorBorder;
    property ColorNumbers: TColor read FColorNumbers write SetColorNumbers;
    property ColorHands: TColor read FColorHands write SetColorHands;
    property DigitLineWidth: integer read FLineWidth write setLineWidth;
    property Enabled;
    property FadingColor: boolean read FFadingColor write SetFadingColor;
    property Font;
    property Kind: TClockKind read FKind write SetKind;
    property LEDContrast: TContrast read FLEDContrast write SetLEDContrast;
    property Priority: TThreadPriority read GetPriority write SetPriority default tpNormal;
    property ShowNumbers: TNumbers read FShowNumbers write SetShowNumbers;
    property ShowSeconds: boolean read FShowSeconds write SetShowSeconds;
    property ShowTicks: boolean read FShowTicks write SetShowTicks;
    property Style: TClockStyle read FStyle write SetStyle;
    property UpdateInterval: word read FUpdateInterval write SetUpdateInterval;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
    property OnStartDrag;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
uses SRUtils, rrColors, TimeFunc;
{$ELSE}
{$R *.D16}
uses SRUtils, TimeFunc;
{$ENDIF}

const
  DefaultWidth  = 192;
  DefaultHeight = 115;
  AU            = 149597869;

function XKoord(XMittel,XRadius,Grad:word):word;
begin
  Result:=round(XMittel-(sin(Grad*Pi/180)*XRadius));
end; {XKoord}

function YKoord(YMittel,YRadius,Grad:word):word;
begin
  Result:=round(YMittel-(cos(Grad*Pi/180)*YRadius));
end; {YKoord}

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  { Rot }
  Red:=GetRValue(RGBColor);
  NewR:=Red+(Percentage*Red div 100);
  if NewR>255 then begin
    Overflow:=NewR-255;
    NewG:=Overflow;
    NewB:=Overflow;
  end
  else begin
    NewG:=0;
    NewB:=0;
  end;
  { Grün }
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  { Blau }
  Blue:=GetBValue(RGBColor);
  NewB:=NewB+Blue+(Percentage*Blue div 100);
  if NewB>255 then begin
    Overflow:=NewB-255;
    if NewG<=255 then
      NewR:=NewR+Overflow;
  end;
  if NewR>255 then
    NewR:=255;
  if NewG>255 then
    NewG:=255;
  if NewB>255 then
    NewB:=255;
  if NewR<0 then
    NewR:=0;
  if NewG<0 then
    NewG:=0;
  if NewB<0 then
    NewB:=0;
  Result:=NewR+(NewG shl 8)+(NewB shl 16);
end;
{$ENDIF}

function CalcShadowedColors(AColor:TColor;AContrast:integer):TColor;
var Dummy : TColor;
begin
  {$IFDEF SR_Delphi1}
  Result:=ChangeBrightness(AColor,-100 div 10*AContrast);
  {$ELSE}
  Get3DColors(AColor,Dummy,REsult,(10-AContrast)/10,(10-AContrast)/10);
  {$ENDIF}
end;


{ Komponente TSRCalendar }
constructor TSRCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { defaults }
  ColCount := 7;
  DefaultDrawing := true;
  FAutoDeleteMarks := true;
  FBGColorHoliday := clWindow;
  FBGColorMarked := clAqua;
  FBGColorSelected := clWindow;
  FBGColorToday := clWindow;
  FBGColorWeekend := clWindow;
  FBundesland := Nordrhein_Westfalen;
  FDrawStyle := cdsColorGrid;
  FFrameColorSelected:=clHighlight;
  FixedCols := 0;
  FixedRows := 1;
  FShowHolidays := true;
  FShowMarks := true;
  FTextColorStandard := clWindowText;
  FTextColorMarked := clWindowText;
  FTextColorHoliday := clRed;
  FTextColorToday := clBlue;
  FTextColorWeekend:=clRed;
  FUseCurrentDate := True;
  Height := DefaultHeight;
  Options := Options - [goRangeSelect] + [goDrawFocusSelected];
  RowCount := 7;
  ScrollBars := ssNone;
  Width := DefaultWidth;
  FDate := Now;
  UpdateCalendar;
end;

procedure TSRCalendar.BeforeChange;
begin
  if Assigned(FOnBeforeChange) then
    FOnBeforeChange(Self);
end;

procedure TSRCalendar.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSRCalendar.ChangeMonth(Delta: Integer);
var
  AYear,
  AMonth,
  ADay    : Word;
  CurDay  : Integer;
  NewDate : TDateTime;
begin
  BeforeChange;
  DecodeDate(FDate, AYear, AMonth, ADay);
  CurDay := ADay;
  if Delta > 0 then
    ADay := GetDaysPerMonth(AYear, AMonth)
  else
    ADay := 1;
  NewDate := EncodeDate(AYear, AMonth, ADay);
  NewDate := NewDate + Delta;
  DecodeDate(NewDate, AYear, AMonth, ADay);
  if GetDaysPerMonth(AYear, AMonth) > CurDay then
    ADay := CurDay
  else
    ADay := GetDaysPerMonth(AYear, AMonth);
  Date := EncodeDate(AYear, AMonth, ADay);
  MonthChange;
end;

procedure TSRCalendar.Click;
var
  TheCellText: string;
begin
  inherited Click;
  TheCellText := CellText[Col, Row];
  if TheCellText <> '' then begin
    try
      Day := StrToInt(TheCellText);
    except
    end;
  end;
end;

procedure TSRCalendar.DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState);
var
  TheText  : string;
  CellDay  : integer;
  CellDate : TDateTime;
  {$IFDEF SR_Delphi1}
  PText    : array [0..2] of char;
  {$ENDIF}
begin
  TheText := CellText[ACol, ARow];
  with ARect, Canvas do begin
    if DrawStyle<>cdsMonoGrid then begin
      try
        {$IFDEF SR_Delphi2_Up}
        if DrawStyle=cdsButtons then begin
          Brush.Color:=clBtnFace;
          Font.Color:=clBtnText;
        end;
        {$ENDIF}
        if (TheText<>'') and (ARow>0) then begin
          CellDay:=StrToInt(TheText);
          CellDate := EncodeDate(Year, Month, CellDay);
          Font.Color:=FTextColorStandard;
          Font.Style:=[];
          if DayOfWeek(CellDate)=1 then begin
            Font.Color:=FTextColorWeekend;
            Brush.Color:=FBGColorWeekend;
          end;
          if trunc(CellDate)=trunc(Now) then begin
            Font.Color:=FTextColorToday;
            Brush.Color:=FBGColorToday;
            Font.Style:=[fsBold];
          end;
          if FShowHolidays then
            FHolidays[CellDay]:=GetHoliday(CellDate, ord(FBundesland));
          if FHolidays[CellDay]>0 then begin
            Font.Color:=FTextColorHoliday;
            Brush.Color:=FBGColorHoliday;
            Font.Style:=[fsBold];
          end;
          if FShowMarks then
            if (CellDay>0) and FMarked[Cellday] then begin
              Font.Color:=FTextColorMarked;
              Brush.Color:=FBGColorMarked;
            end;
          if (ACol=Col) and (ARow=Row) then begin
            if not FMarked[Cellday] or not FShowMarks then
              Brush.Color:=FBGColorSelected;
            Pen.Color:=FFrameColorSelected;
            if DrawStyle=cdsColorGrid then begin
              InflateRect(ARect, -1, -1);
              Rectangle(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom);
              InflateRect(ARect, -1, -1);
            end;
          end;
        end;
        {$IFDEF SR_Delphi2_Up}
        if DrawStyle=cdsButtons then begin
          FillRect(ARect);
          if ARow=0 then
            Font.Style:=[]
          else
            ARect.Bottom:=ARect.Bottom+1;
          ARect.Right:=ARect.Right+1;
          if (ACol=Col) and (ARow=Row) then
            DrawFrameControl(Handle,
                             ARect,
                             DFC_Button,
                             DFCS_ButtonPush or DFCS_Pushed)
          else
            DrawFrameControl(Handle,
                             ARect,
                             DFC_Button,
                             DFCS_ButtonPush);
        end;
        {$ENDIF}
      except
      end;
      {$IFDEF SR_Delphi1}
      if Brush.Color<>Color then
        FillRect(ARect);
      {$ELSE}
      if (DrawStyle<>cdsButtons) and (Brush.Color<>Color) then
        FillRect(ARect);
      {$ENDIF}
    end;
    Brush.Style:=bsClear;
    {$IFDEF SR_Delphi1}
    StrPCopy(PText,TheText);
    DrawText(Handle,
             PText,
             length(TheText),
             ARect,
             DT_SingleLine or DT_NoPrefix or DT_Center or DT_VCenter);
    {$ELSE}
    DrawText(Handle,
             PChar(TheText),
             length(TheText),
             ARect,
             DT_SingleLine or DT_NoPrefix or DT_Center or DT_VCenter);
    Brush.Style:=bsSolid;
    {$ENDIF}
  end;
end;

function TSRCalendar.GetDaysThisMonth: Integer;
begin
  Result := GetDaysPerMonth(Year, Month);
end;

function TSRCalendar.GetCellText(ACol, ARow: Integer): string;
var DayNum: Integer;
begin
  if ARow = 0 then  { day names at tops of columns }
    Result := ShortDayNames[(StartOfWeek + ACol) mod 7 + 1]
  else begin
    DayNum := FMonthOffset + ACol + (ARow - 1) * 7;
    if (DayNum < 1) or (DayNum > GetDaysThisMonth) then
      Result := ''
    else begin
      try
        Result := IntToStr(DayNum);
      except
        Result:='';
      end;
    end;
  end;
end;

function TSRCalendar.GetDateElement(Index: Integer): Integer;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(FDate, AYear, AMonth, ADay);
  case Index of
    1: Result := AYear;
    2: Result := AMonth;
    3: Result := ADay;
    else Result := -1;
  end;
end;

function TSRCalendar.GetHoliday(WhatDate:TDateTime;Land:integer):integer;
var DoY,Y,M    : word;
    D,dw,OM,aw : word;
    Dat        : TDateTime;
    Ostern     : TDateTime;
    Weihnacht  : TDateTime;

  function EasterSunday(Y:word):TDateTime;
  var a,b,c,d,e,
      Tag,Monat : integer;
  begin
    a:=y mod 19 ;
    b:=y mod 4;
    c:=y mod 7;
    d:=(19*a+24) mod 30;
    e:=(2*b+4*c+6*d+5) mod 7;
    Tag:=22+d+e;
    Monat:=3;
    if Tag>31 then begin
      Tag:=d+e-9;
      Monat:=4;
    end;
    if (Tag=26) and (Monat=4) then
      Tag:=19;
    if (Tag=25) and (Monat=4) and (d=28) and (e=6) and (a>10) then
      Tag:=18;
    try
      Result:= EncodeDate(y,Monat,Tag);
    except
      Result:=0;
    end;
  end; { EasterSunday }

begin
  DecodeDate(WhatDate,Y,M,D);
  DoY:=GetDayOfYear(WhatDate);
  Result:=0;
  Ostern:=EasterSunday(Y);
  try
    DecodeDate(Ostern,Y,OM,D);
  except
    OM:=4;
  end;
  try
    Weihnacht:=EncodeDate(Y,12,25);
    if (DayOfWeek(Weihnacht)-1)=0 then
      dw:=7
    else
      dw:=DayOfWeek(Weihnacht)-1;
  except
    Weihnacht:=-1;
    dw:=0;
  end;
  { Mariä Lichtmeß }                     { Sondertage }
  Dat:=EncodeDate(Y,2,2);
  if DoY=GetDayOfYear(Dat) then
    Result:=-1;
  { Valentinstag }
  Dat:=Encodedate(Y,2,14);
  if DoY=GetDayOfYear(Dat) then
    Result:=-2;
  { Weiberfastnacht }
  Dat:=Ostern-45;
  while DayOfWeek(Dat)<>2 do
    Dat:=Dat-1;
  if DoY=GetDayOfYear(Dat-4) then
    Result:=-3;
  { Rosenmontag }
  if DoY=GetDayOfYear(Dat) then
    Result:=-4;
  { Fastnacht }
  if DoY=GetDayOfYear(Dat+1) then
    Result:=-5;
  { Aschermittwoch }
  if DoY=GetDayOfYear(Dat+2) then
    Result:=-6;
  { Mariä Verkündigung }
  Dat:=Encodedate(Y,3,25);
  if DoY=GetDayOfYear(Dat) then
    Result:=-7;
  { Palmsonntag }
  if DoY=GetDayOfYear(Ostern-7) then
    Result:=-8;
  { Gründonnerstag }
  if DoY=GetDayOfYear(Ostern-3) then
    Result:=-9;
  { Muttertag }
  Dat:=EncodeDate(y,4,30);
  aw:=DayOfWeek(Dat)-1;
  Dat:=Dat-aw+14;
  if Dat=(Ostern+49) then
    Dat:=Dat-7;
  if DoY=GetDayOfYear(Dat) then
    Result:=-10;
  { Peter und Paul }
  Dat:=Encodedate(Y,6,29);
  if DoY=GetDayOfYear(Dat) then
    Result:=-11;
  { Mariä Geburt }
  Dat:=Encodedate(Y,9,8);
  if DoY=GetDayOfYear(Dat) then
    Result:=-12;
  { Erntedankfest }
  Dat:=Encodedate(Y,10,1);
  while DayOfWeek(Dat)<>1 do
    Dat:=Dat+1;
  if DoY=GetDayOfYear(Dat) then
    Result:=-13;
  { Mariä Empfängnis }
  Dat:=Encodedate(Y,12,8);
  if DoY=GetDayOfYear(Dat) then
    Result:=-14;
  { Silvester }
  Dat:=Encodedate(Y,12,31);
  if DoY=GetDayOfYear(Dat) then
    Result:=-15;
  { 1. Advent }
  Dat:=Weihnacht-1;
  while DayOfWeek(Dat)<>1 do
    Dat:=Dat-1;
  if DoY=GetDayOfYear(Dat-21) then
    Result:=-16;
  { 2. Advent }
  if DoY=GetDayOfYear(Dat-14) then
    Result:=-17;
  { 3. Advent }
  if DoY=GetDayOfYear(Dat-7) then
    Result:=-18;
  { 4. Advent }
  if DoY=GetDayOfYear(Dat) then
    Result:=-19;
  { Heiligabend }
  if DoY=GetDayOfYear(Weihnacht-1) then
    Result:=-20;
  { Frühlingsanfang }
  Dat:=StartSeason(Y,Spring);
  if DoY=GetDayOfYear(Dat) then
    Result:=-21;
  { Sommmeranfang }
  Dat:=StartSeason(Y,Summer);
  if DoY=GetDayOfYear(Dat) then
    Result:=-22;
  { Herbstanfang }
  Dat:=StartSeason(Y,Autumn);
  if DoY=GetDayOfYear(Dat) then
    Result:=-23;
  { Winteranfang }
  Dat:=StartSeason(Y,Winter);
  if DoY=GetDayOfYear(Dat) then
    Result:=-24;
  { Neujahr }                     { Holidays }
  if DoY=1 then
    Result:=1;
  { MaiHoliday }
  Dat:=EncodeDate(Y,5,1);
  if DoY=GetDayOfYear(Dat) then
    Result:=2;
  { Tag der deutschen Einheit }
  Dat:=EncodeDate(Y,10,3);
  if DoY=GetDayOfYear(Dat) then
    Result:=3;
  { Allerheiligen }
  if Land<>14 then begin
    Dat:=EncodeDate(Y,11,1);
    if DoY=GetDayOfYear(Dat) then
      Result:=4;
  end;
  { Totensonntag }
  if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht-dw-28)) then
    Result:=5;
  { Volkstrauertag }
  if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht-dw-35)) then
    Result:=6;
  { 1. Weihnachtstag }
  if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht)) then
    Result:=7;
  { 2. Weihnachtstag }
  if (Weihnacht>=0) and (DoY=GetDayOfYear(Weihnacht+1)) then
    Result:=8;
  { Karfreitag }
  if DoY=GetDayOfYear(Ostern-2) then
    Result:=9;
  { Ostersonntag }
  if DoY=GetDayOfYear(Ostern) then
    Result:=10;
  { Ostermontag }
  if DoY=GetDayOfYear(Ostern+1) then
    Result:=11;
  { Christi Himmelfahrt }
  if DoY=GetDayOfYear(Ostern+39) then
    Result:=12;
  { Pfingstsonntag }
  if DoY=GetDayOfYear(Ostern+49) then
    Result:=13;
  { Pfingstmontag }
  if DoY=GetDayOfYear(Ostern+50) then
    Result:=14;
  { Fronleichnam }
  if (Land<2) or ((Land>=9) and (Land<=12)) or (Land=15) then
    if DoY=GetDayOfYear(Ostern+60) then
      Result:=15;
  { Heilige 3 Könige }
  if (Land=0) or (Land=1) or (Land=13) then
    if DoY=6 then
      Result:=16;
  { Mariä Himmelfahrt }
  if (Land=1) or (Land=11) then begin
    Dat:=EncodeDate(Y,8,15);
    if DoY=GetDayOfYear(Dat) then
      Result:=17;
  end;
  { Reformationstag }
  if (Land=3) or (Land=7) or (Land=12) or (Land=13) or (Land=15) then begin
    Dat:=Encodedate(Y,10,31);
    if DoY=GetDayOfYear(Dat) then
      Result:=18;
  end;
  { Buß- und Bettag }
  if (Weihnacht>=0) and (Land=12) and (DoY=GetDayOfYear(Weihnacht-dw-32)) then
    Result:=19;
end;

function TSRCalendar.GetHolidays(Index: integer):integer;
begin
  Result:=FHolidays[Index];
end;

function TSRCalendar.GetMarked(Index: integer):boolean;
begin
  Result:=FMarked[Index];
end;

procedure TSRCalendar.GetMoonData(Dat:TDateTime);
var TimeDiff         : extended;

  function LowestPhase(Dat:TDateTime):extended;
  var Phase   : extended;
      Std     : byte;
  begin
    Result:=Current_Phase(trunc(Dat));
    for Std:=1 to 23 do begin
      Phase:=Current_Phase(trunc(Dat)+Std/24);
      if Phase<Result then
        Result:=Phase;
    end;
  end; { LowestPhase }

begin
  FMoonDistance:=Moon_Distance(Dat);
  if LowestPhase(Dat-1)>LowestPhase(Dat) then begin
    if LowestPhase(Dat+1)>LowestPhase(Dat) then
      FMoonPhase:=Neumond
    else
      FMoonPhase:=abnehmend;
  end
  else begin
    if LowestPhase(Dat+1)<LowestPhase(Dat) then
      FMoonPhase:=Vollmond
    else
      FMoonPhase:=zunehmend;
  end;
  TimeDiff:=1/24;
  if IsSummertime(Dat) then
    TimeDiff:=TimeDiff+1/24;
  FMoonRise:=Moon_Rise(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
  FMoonSet:=Moon_Set(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
  FMoonTransit:=Moon_Transit(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
end;

function TSRCalendar.GetSternzeichenNr(Dat:TDateTime):integer;
var TiJ : word;
begin
  Result:=0;
  TiJ:=GetDayOfYear(Dat);
  if (TiJ>=21) and (TiJ<=49) then
    Result:=0;
  if (TiJ>=50) and (TiJ<=79) then
    Result:=1;
  if (TiJ>=80) and (TiJ<=111) then
    Result:=2;
  if (TiJ>=112) and (TiJ<=141) then
    Result:=3;
  if (TiJ>=142) and (TiJ<=172) then
    Result:=4;
  if (TiJ>=173) and (TiJ<=204) then
    Result:=5;
  if (TiJ>=205) and (TiJ<=235) then
    Result:=6;
  if (TiJ>=236) and (TiJ<=266) then
    Result:=7;
  if (TiJ>=267) and (TiJ<=296) then
    Result:=8;
  if (TiJ>=297) and (TiJ<=326) then
    Result:=9;
  if (TiJ>=327) and (TiJ<=355) then
    Result:=10;
  if (TiJ>=355) or (TiJ<=20) then
    Result:=11;
end;

procedure TSRCalendar.GetSunData(Dat:TDateTime);
var TimeDiff         : extended;
begin
  FSunDistance:=Sun_Distance(Dat)*au;
  TimeDiff:=1/24;
  if IsSummertime(Dat) then
    TimeDiff:=TimeDiff+1/24;
  FSunRise:=Sun_Rise(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
  FSunSet:=Sun_Set(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
  FSunTransit:=Sun_Transit(Dat,Breite[ord(FBundesland)],Laenge[ord(FBundesland)])+TimeDiff;
end;

procedure TSRCalendar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key=VK_Left then begin
    if Day=1 then begin
      ChangeMonth(-1);
      Day:=GetDaysThisMonth;
      Key:=0;
    end
    else
      if (Col=0) and (Row>1) then begin
        Day:=Day-1;
        Key:=0;
      end;
  end;
  if Key=VK_Right then begin
    if Day=GetDaysThisMonth then begin
      Day:=1;
      ChangeMonth(1);
      Key:=0;
    end
    else
      if (Col=6) and (Row<6) then begin
        Day:=Day+1;
        Key:=0;
      end;
  end;
  if (Key=VK_Up) and (Row=1) then begin
    ChangeMonth(-1);
    Day:=GetDaysThisMonth;
    Key:=0;
  end;
  if (Key=VK_Down) and (Row=6) then begin
    ChangeMonth(1);
    Day:=1;
    Key:=0;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TSRCalendar.MonthChange;
var i : integer;
begin
  if FShowHolidays then
    for i:=1 to 31 do
      FHolidays[i]:=0;
  if FAutoDeleteMarks then
    for i:=1 to 31 do
      FMarked[i]:=false;
  FDaysThisMonth:=GetDaysThisMonth;
  if Assigned(FOnMonthChange) then
    FOnMonthChange(Self);
end;

procedure TSRCalendar.MouseToCell(X, Y: Integer; var ACol, ARow: Longint);
var Coord : TGridCoord;
begin
  Coord := MouseCoord(X, Y);
  ACol := Coord.X;
  ARow := Coord.Y;
end;

function TSRCalendar.MouseToDate(X, Y: Integer):TDateTime;
var ACol, ARow : longint;
    ADay       : word;
begin
  MouseToCell(X, Y, ACol, ARow);
  try
    ADay := StrToInt(CellText[ACol, ARow]);
    Result:=EncodeDate(Year, Month, ADay);
  except
    Result:=-1;
  end;
end;

procedure TSRCalendar.NextMonth;
begin
  ChangeMonth(1);
end;

procedure TSRCalendar.NextYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year + 1;
end;

procedure TSRCalendar.PrevMonth;
begin
  ChangeMonth(-1);
end;

procedure TSRCalendar.PrevYear;
begin
  if IsLeapYear(Year) and (Month = 2) and (Day = 29) then
    Day := 28;
  Year := Year - 1;
end;

function TSRCalendar.SelectCell(ACol, ARow: Longint): Boolean;
begin
  if ((not FUpdating) and FReadOnly) or (CellText[ACol, ARow] = '') then
    Result := False
  else
    Result := inherited SelectCell(ACol, ARow);
end;

procedure TSRCalendar.SetBGColorHoliday(newColor: TColor);
begin
  if FBGColorHoliday<>newColor then begin
    FBGColorHoliday:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetBGColorMarked(newColor: TColor);
begin
  if FBGColorMarked<>newColor then begin
    FBGColorMarked:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetBGColorSelected(newColor: TColor);
begin
  if FBGColorSelected<>newColor then begin
    FBGColorSelected:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetBGColorToday(newColor: TColor);
begin
  if FBGColorToday<>newColor then begin
    FBGColorToday:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetBGColorWeekend(newColor: TColor);
begin
  if FBGColorWeekend<>newColor then begin
    FBGColorWeekend:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetBundesland(newValue: TBundesland);
begin
  if FBundesland<>newValue then begin
    BeforeChange;
    FBundesland:=newValue;
    UpdateCalendar;
    Change;
  end;
end;

procedure TSRCalendar.SetDate(Value: TDateTime);
var AYear,
    AMonth,
    ADay    : Word;
    MChange : boolean;
begin
  BeforeChange;
  try
    DecodeDate(FDate,AYear, AMonth, ADay);
    MChange:=AMonth<>Month;
  except
    MChange:=false;
  end;
  FDate:=Value;
  UpdateCalendar;
  Change;
  if MChange then
    MonthChange;
end;

procedure TSRCalendar.SetDateElement(Index: Integer; Value: Integer);
var
  AYear, AMonth, ADay: Word;
  MChange            : boolean;
begin
  if Value>0 then begin
    BeforeChange;
    DecodeDate(FDate, AYear, AMonth, ADay);
    MChange:=false;
    case Index of
      1: if AYear <> Value then begin
           AYear := Value;
           MChange:=true;
         end
         else
           Exit;
      2: if (Value <= 12) and (Value <> AMonth) then begin
           AMonth := Value;
           MChange:=true;
         end
         else
           Exit;
      3: if (Value <= GetDaysThisMonth) and (Value <> ADay) then
           ADay := Value
         else
           Exit;
      else Exit;
    end;
    FDate := EncodeDate(AYear, AMonth, ADay)+Time;
    FUseCurrentDate := False;
    UpdateCalendar;
    Change;
    if MChange then
      MonthChange;
  end;
end;

procedure TSRCalendar.SetDrawStyle(newValue: TCalendarDrawStyle);
begin
  if newValue<>FDrawStyle then begin
    FDrawStyle:=newValue;
    {$IFDEF SR_Delphi2_Up}
    DefaultDrawing:=FDrawStyle<>cdsButtons;
    {$ENDIF}
    Invalidate;
  end;
end;

procedure TSRCalendar.SetFrameColorSelected(newColor: TColor);
begin
  if FFrameColorSelected<>newColor then begin
    FFrameColorSelected:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetHolidays(Index: integer; newValue: integer);
begin
  FHolidays[Index]:=newValue;
  Invalidate;
end;

procedure TSRCalendar.SetMarked(Index: integer; newValue: boolean);
begin
  FMarked[Index]:=newValue;
  Invalidate;
end;

procedure TSRCalendar.SetShowHolidays(newValue: boolean);
begin
  if FShowHolidays<>newValue then begin
    FShowHolidays:=newValue;
    UpdateCalendar;
  end;
end;

procedure TSRCalendar.SetShowMarks(newValue: boolean);
begin
  if FShowMarks<>newValue then begin
    FShowMarks:=newValue;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetStartOfWeek(Value: TDayOfWeek);
begin
  if Value <> FStartOfWeek then begin
    FStartOfWeek := Value;
    UpdateCalendar;
  end;
end;

procedure TSRCalendar.SetTextColorHoliday(newColor: TColor);
begin
  if FTextColorHoliday<>newColor then begin
    FTextColorHoliday:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetTextColorMarked(newColor: TColor);
begin
  if FTextColorMarked<>newColor then begin
    FTextColorMarked:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetTextColorToday(newColor: TColor);
begin
  if FTextColorToday<>newColor then begin
    FTextColorToday:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetTextColorStandard(newColor: TColor);
begin
  if FTextColorStandard<>newColor then begin
    FTextColorStandard:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetTextColorWeekend(newColor: TColor);
begin
  if FTextColorWeekend<>newColor then begin
    FTextColorWeekend:=newColor;
    Invalidate;
  end;
end;

procedure TSRCalendar.SetUseCurrentDate(Value: Boolean);
begin
  if Value <> FUseCurrentDate then begin
    FUseCurrentDate := Value;
    if Value then begin
      FDate := Now; { use the current date, then }
      UpdateCalendar;
    end;
  end;
end;

function TSRCalendar.StoreDate: Boolean;
begin
  Result := not FUseCurrentDate;
end;

procedure TSRCalendar.UpdateCalendar;
var
  AYear,
  AMonth,
  ADay      : Word;
  FirstDate : TDateTime;
begin
  FUpdating := True;
  try
    DecodeDate(FDate, AYear, AMonth, ADay);
    FDayOfYear := GetDayOfYear(FDate);
    FWeekOfYear := GetWeekOfYear(FDate);
    FDaysThisMonth := GetDaysThisMonth;
    if FShowHolidays then begin
      FHolidayNr := GetHoliday(FDate, ord(FBundesland));
      if FHolidayNr=0 then
        FHoliday := '';
      if FHolidayNr>0 then
        FHoliday := Feiertage[FHolidayNr];
      if FHolidayNr<0 then
        FHoliday := Sondertage[abs(FHolidayNr)];
    end
    else
      FHoliday := '';
    FSternzeichenNr := GetSternzeichenNr(FDate);
    FSternzeichen := SternzNamen[FSternzeichenNr];
    if FAstronomicalData then begin
      GetMoonData(FDate);
      GetSunData(FDate);
    end;
    FirstDate := EncodeDate(AYear, AMonth, 1);
    FMonthOffset := 2 - ((DayOfWeek(FirstDate) - StartOfWeek + 7) mod 7); {  day of week for 1st of month  }
    if FMonthOffset = 2 then
      FMonthOffset := -5;
    MoveColRow((ADay - FMonthOffset) mod 7, (ADay - FMonthOffset) div 7 + 1,
      False, False);
    Invalidate;
  finally
    FUpdating := False;
  end;
end;

procedure TSRCalendar.WMSize(var Message: TWMSize);
var
  GridLines: Integer;
begin
  GridLines := 6 * GridLineWidth;
  DefaultColWidth := (Message.Width - GridLines) div 7;
  DefaultRowHeight := (Message.Height - GridLines) div 7;
end;

{$IFDEF SR_Delphi2_Up}
{ Klasse TThreadTimer (nicht in Delphi 1) }
procedure TTimerThread.Execute;
begin
  Priority := OwnerTimer.Priority;
  repeat
    SleepEx(OwnerTimer.Interval, False);
    Synchronize(OwnerTimer.Timer);
  until Terminated;
end;

procedure TThreadTimer.UpdateTimer;
begin
  if not TimerThread.Suspended then
    TimerThread.Suspend;
  if (FInterval <> 0) and FEnabled then
    if TimerThread.Suspended then
      TimerThread.Resume;
end;

procedure TThreadTimer.SetEnabled(Value: boolean);
begin
  if Value<>FEnabled then begin
    FEnabled:=Value;
    UpdateTimer;
  end;
end;

procedure TThreadTimer.SetInterval(Value: Word);
begin
  if Value<>FInterval then begin
    FInterval:=Value;
    UpdateTimer;
  end;
end;

procedure TThreadTimer.SetThreadPriority(Value: TThreadPriority);
begin
  if Value<>FThreadPriority then begin
    FThreadPriority:=Value;
    UpdateTimer;
  end;
end;

procedure TThreadTimer.Timer;
begin
  if Assigned(FOntimer) then
    FOnTimer(Self);
end;

constructor TThreadTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FInterval := 250;
  FThreadPriority := tpNormal;
  FTimerThread := TTimerThread.Create(False);
  FTimerThread.OwnerTimer := Self;
end;

destructor TThreadTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;
  FTimerThread.Free;
  inherited Destroy;
end;

{ Komponente TSRClock (nicht in Delphi 1) }
procedure TSRClock.AssignColors (seg: integer; s1,s2,s3,s4,s5,s6,s7: Boolean);
begin
  if s1 then
    FSegCl[seg, 1] := FColorNumbers
  else
    FSegCl[seg, 1] := FColorSegments;
  if s2 then
    FSegCl[seg, 2] := FColorNumbers
  else
    FSegCl[seg, 2] := FColorSegments;
  if s3 then
    FSegCl[seg, 3] := FColorNumbers
  else
    FSegCl[seg, 3] := FColorSegments;
  if s4 then
    FSegCl[seg, 4] := FColorNumbers
  else
    FSegCl[seg, 4] := FColorSegments;
  if s5 then
    FSegCl[seg, 5] := FColorNumbers
  else
    FSegCl[seg, 5] := FColorSegments;
  if s6 then
    FSegCl[seg, 6] := FColorNumbers
  else
    FSegCl[seg, 6] := FColorSegments;
  if s7 then
    FSegCl[seg, 7] := FColorNumbers
  else
    FSegCl[seg, 7] := FColorSegments;
end;

procedure TSRClock.GenerateBitMaps(AWidth, AHeight: integer);
var
  TL, TR, TBL, TBR,
  ML, MTL, MTR, MR,
  MBL, MBR, BL, BTL,
  BTR, BR            : TPoint;
  c, wAlt, LineW,
  DigitW             : integer;
begin
  LineW:=FLineWidth+2;
  DigitW:=round((AWidth-12)/8);
  wAlt := AHeight-4;
  { Polygonpunkte zuweisen }
  TL.x := 0;
  TL.y := 0;
  TR.x := DigitW-1;
  TR.y := 0;
  TBL.x := LineW - 1;
  TBL.y := LineW -1;
  TBR.x := DigitW - LineW;
  TBR.y := TBL.y;
  ML.x := 0;
  ML.y := wAlt div 2;
  MTL.x := TBL.x;
  MTL.y := ML.y - (LineW div 2);
  MTR.x := TBR.x;
  MTR.y := MTL.y;
  MR.x := TR.x;
  MR.y := ML.y;
  MBL.x := TBL.x;
  MBL.y := ML.y + (LineW div 2);
  MBR.x := MTR.x; MBR.y := MBL.y;
  BL.x := 0;
  BL.y := wAlt - 1;
  BR.x := TR.x;
  BR.y := BL.y;
  BTL.x := TBL.x;
  BTL.y := wAlt - LineW;
  BTR.x := TBR.x;
  BTR.y := BTL.y;

  { Segmentfarben zuweisen }
  AssignColors (0,true,true,true,false,true,true,true);
  AssignColors (1,false,false,true,false,false,true,false);
  AssignColors (2,true,false,true,true,true,false,true);
  AssignColors (3,true,false,true,true,false,true,true);
  AssignColors (4,false,true,true,true,false,true,false);
  AssignColors (5,true,true,false,true,false,true,true);
  AssignColors (6,false,true,false,true,true,true,true);
  AssignColors (7,true,false,true,false,false,true,false);
  AssignColors (8,true,true,true,true,true,true,true);
  AssignColors (9,true,true,true,true,false,true,true);

  { Bitmap erstellen }
  for c := 0 to 9 do begin
    FDigit[c].free;
    FDigit[c] := TBitmap.create;
    FDigit[c].width := DigitW;
    FDigit[c].height := wAlt;
    with FDigit[c].canvas do begin
      Pen.Color := ColorBorder;
      Brush.Color := FColorBackGround;
      Brush.style := bsSolid;
      Pen.Width := 1;
      Rectangle (TL.x, TL.y, BR.x+1, BR.y+1);
      { Segment 1 }
      Brush.Color := FSegCl[c, 1];
      Polygon ([TL, TR, TBR, TBL]);
      { Segment 2 }
      Brush.Color := FSegCl[c, 2];
      Polygon ([TL, TBL, MTL, ML]);
      { Segment 3 }
      Brush.Color := FSegCl[c, 3];
      Polygon ([TR, MR, MTR, TBR]);
      { Segment 4 }
      Brush.Color := FSegCl[c, 4];
      Polygon ([ML, MTL, MTR, MR, MBR, MBL]);
      { Segment 5 }
      Brush.Color := FSegCl[c, 5];
      Polygon ([ML, MBL, BTL, BL]);
      { Segment 6 }
      Brush.Color := FSegCl[c, 6];
      Polygon ([MR, BR, BTR, MBR]);
      { Segment 7 }
      Brush.Color := FSegCl[c, 7];
      Polygon ([BL, BTL, BTR, BR]);
    end;
  end;
end;

constructor TSRClock.Create(AOwner: TComponent);
var msec : word;
begin
  inherited Create(AOwner);
  {  defaults  }
  Buffer := TBitmap.Create;

  FUpdateInterval:=1000;
  Timer := TThreadTimer.Create(self);
  Timer.Interval := FUpdateInterval;
  Timer.OnTimer := AutoUpdateClock;

  FTime:=Now;
  try
    DecodeTime(FTime,FHour,FMinute,FSecond,msec);
  except
  end;

  FAutoUpdate:=false;
  FBorderWidth:=2;
  FColorBackGround:=clWindow;
  FColorBorder:=clWindowFrame;
  FColorNumbers:=clBlue;
  FLEDContrast:=6;
  FColorSegments:=CalcShadowedColors(FColorNumbers, FLEDContrast);
  FColorHands:=clNavy;
  FLineWidth:= 3;
  FPriority := tpNormal;
  FRunning:=false;
  FShowNumbers:=snQuarters;
  FShowSeconds:=true;
  FShowTicks:=true;
  FSummertime:=IsSummertime(Now);
  FStyle:=csClassic;

  SetBounds(0,0,80,80);

  FOldWidth:=Self.Width;
  FOldHeight:=Self.Height;
  if FStyle=csDigital then
    GenerateBitMaps(Self.Width, Self.Height);
end;

destructor TSRClock.Destroy;
begin
  Buffer.Free;
  Timer.Free;
  inherited Destroy;
end;

procedure TSRClock.Loaded;
begin
  inherited Loaded;
  Buffer.Width := Self.ClientWidth;
  Buffer.Height := Self.ClientHeight;
  Buffer.Canvas.Brush.Color := Color;
end;

procedure TSRClock.CmEnabledChanged(var Message: TWmNoParams);
begin
  inherited;
  Timer.Enabled := Self.Enabled;
  Invalidate;
end;

procedure TSRClock.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TSRClock.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TSRClock.CmVisibleChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

function TSRClock.GetPriority: TThreadPriority;
begin
  Result := Timer.Priority;
end;

procedure TSRClock.SetAutoUpdate(Value: boolean);
begin
  if (FAutoUpdate<>Value) and (FKind=ckRealTime) then begin
    FAutoUpdate:=Value;
    Timer.Enabled := FAutoUpdate;
  end;
end;

procedure TSRClock.SetBorderWidth(Value: integer);
begin
  if Value<>FBorderWidth then begin
    FBorderWidth:=Value;
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetColorBackground(Value: TColor);
begin
  if Value<>FColorBackground then begin
    FColorBackground:=Value;
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetColorBorder(Value: TColor);
begin
  if Value<>FColorBorder then begin
    FColorBorder:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetColorNumbers(Value: TColor);
begin
  if Value<>FColorNumbers then begin
    FColorNumbers:=Value;
    FColorSegments:=CalcShadowedColors(FColorNumbers, FLEDContrast);
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetColorHands(Value: TColor);
begin
  if Value<>FColorHands then begin
    FColorHands:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetFadingColor(Value: boolean);
begin
  if Value<>FFadingColor then begin
    FFadingColor:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetKind(Value: TClockKind);
begin
  if Value<>FKind then begin
    FKind:=Value;
    if FKind=ckRealTime then
      FTime:=Now
    else begin
      FRunning:=false;
      FTimeOffset:=Now;
      FTime:=0;
    end;
    Invalidate;
  end;
end;

procedure TSRClock.SetLEDContrast(Value: TContrast);
begin
  if (FLEDContrast<>Value) and (Value>=0) and (Value<10) then begin
    FLEDContrast:=Value;
    FColorSegments:=CalcShadowedColors(FColorNumbers, FLEDContrast);
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetLineWidth (Value: integer);
begin
  if FLineWidth<>Value then begin
    FLineWidth:=Value;
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetPriority(Value: TThreadPriority);
begin
  if Value<>FPriority then begin
    FPriority:=Value;
    Timer.Priority := FPriority;
  end;
end;

procedure TSRClock.SetShowNumbers(Value: TNumbers);
begin
  if Value<>FShowNumbers then begin
    FShowNumbers:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetShowSeconds(Value: boolean);
begin
  if Value<>FShowSeconds then begin
    FShowSeconds:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetShowTicks(Value: boolean);
begin
  if Value<>FShowTicks then begin
    FShowTicks:=Value;
    Invalidate;
  end;
end;

procedure TSRClock.SetStyle(Value: TClockStyle);
begin
  if Value<>FStyle then begin
    FStyle:=Value;
    if FStyle=csDigital then
      GenerateBitMaps(Self.Width, Self.Height);
    Invalidate;
  end;
end;

procedure TSRClock.SetTime(Value: TTime);
var msec : word;
begin
  if Value<>FTime then begin
    FTime:=Value;
    try
      DecodeTime(FTime,FHour,FMinute,FSecond,msec);
    except
      FHour:=0;
      FMinute:=0;
      FSecond:=0;
    end;
    Paint;
  end;
end;

procedure TSRClock.SetUpdateInterval(Value: word);
begin
  if Value<>FUpdateInterval then begin
    FUpdateInterval:=Value;
    Timer.Interval:=FUpdateInterval;
    Invalidate;
  end;
end;

procedure TSRClock.AutoUpdateClock(Sender: TObject);
begin
  if ((Kind=ckRealTime) and FAutoUpdate) or ((Kind=ckStopWatch) and FRunning) then begin
    if Kind=ckStopWatch then
      SetTime(Now-FTimeOffset)
    else
      SetTime(Now);
    if Assigned(FOnTimer) then
      FOnTimer(Self);
  end;
end;

procedure TSRClock.Reset;
begin
  FTimeOffset:=Now;
  FTime:=0;
  Invalidate;
end;

procedure TSRClock.Start;
begin
  FTimeOffset:=Now-FTime;
  FRunning:=true;
end;

procedure TSRClock.Stop;
begin
  FRunning:=false;
end;

procedure TSRClock.Paint;
var ARect       : TRect;
    Center,
    ElCenter    : TPoint;
    i           : byte;
    XRadius,
    YRadius,
    ElXRadius,
    ElYRadius,
    Grad        : word;
    anchoPosi,
    posiLeft,
    PosiTop, c,
    SepPosition : integer;
    outText     : string;
    ElXAbstand,
    ElYAbstand  : double;

  procedure AlTextOut(X,Y:integer;Text:string;HAlign,VAlign:TAlignment);
  var LeftOut,TopOut : integer;
  begin
    with Buffer.Canvas do begin
      LeftOut:=X;
      if HAlign=taRightJustify then
        LeftOut:=X-TextWidth(Text);
      if HAlign=taCenter then
        LeftOut:=X-(TextWidth(Text) div 2);
      TopOut:=Y;
      if VAlign=taRightJustify then
        TopOut:=Y-TextHeight(Text);
      if VAlign=taCenter then
        TopOut:=Y-(TextHeight(Text) div 2);
      TextOut(LeftOut,TopOut,Text);
    end;
  end; { AlTextOut }

begin
  Buffer.Width := Self.Width;
  Buffer.Height := Self.Height;
  ARect:=GetClientRect;
  Center.X:=(ARect.Right-ARect.Left) div 2;
  Center.Y:=(ARect.Bottom-ARect.Top) div 2;
  with Buffer.Canvas do begin
    Font.Assign(Self.Font);
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Color := Self.Color;
    Rectangle(0, 0, Width, Height);

    if Style=csDigital then begin
      if (FOldWidth<>Self.Width) or (FOldHeight<>Self.Height) then
        GenerateBitmaps(Self.Width, Self.Height);
      Brush.Color := ColorBackground;
      Pen.Color := ColorBorder;
      Rectangle(0, 0, Width, Height);
      try
        outText:=FormatDateTime('hh:mm:ss', FTime);
      except
        outText:='';
      end;
      anchoPosi := round((Self.Width-4)/8);
      PosiTop := (Self.Height - (Self.Height-4)) div 2;
      posiLeft := ((anchoPosi - round((Self.Width)/8)) div 2)+3;
      Brush.Color := FColorNumbers;
      Pen.Color := FColorNumbers;
      { Bitmaps und DecSeperator zeichnen }
      for c := 1 to 8 do begin
        { nachfolgende Nullen müssen gezeichnet werden! }
        if outText[c]=':' then begin
          Pen.Width:=1;
          Ellipse(posiLeft+round((Width-12)/16), posiTop+((Height-4) div 3)-2,
                  posiLeft+FLineWidth+round((Width-12)/16), posiTop+((Height-4) div 3)-2+FLineWidth);
          Ellipse(posiLeft+round((Width-12)/16), posiTop+((Height-4)*2 div 3)-2,
                  posiLeft+FLineWidth+round((Width-12)/16), posiTop+((Height-4)*2 div 3)-2+FLineWidth);
        end
        else
          Draw (posiLeft, posiTop, FDigit[strToInt(outText[c])]);
        inc (posiLeft, anchoPosi);
      end;
    end
    else begin
      { Rahmen und Hintergrund: }
      Pen.Width:=FBorderWidth;
      Pen.Color:=FColorBorder;
      Brush.Color:=FColorBackground;
      Brush.Style:=bsSolid;
      InflateRect(ARect, -FBorderWidth div 2, -FBorderWidth div 2);
      Ellipse(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
      Pen.Width:=1;
    end;
    XRadius:=(ARect.Right-ARect.Left) div 2;
    YRadius:=(ARect.Bottom-ARect.Top) div 2;
    if Style=csClassic then begin
      { Markierungen: }
      if FShowTicks then begin
        for i:=1 to 12 do begin
          MoveTo(XKoord(Center.X-1, XRadius-1, 360-(i*30)),
                 YKoord(Center.Y-1, YRadius-1, 360-(i*30)));
          LineTo(XKoord(Center.X-1, XRadius-5, 360-(i*30)),
                 YKoord(Center.Y-1, YRadius-5, 360-(i*30)));
        end;
      end;

      { Ziffern: }
      if FShowNumbers<>snNone then begin
        Brush.Style:=bsClear;
        Font.Color:=FColorNumbers;
        for i:=1 to 12 do
          if (FShowNumbers=snAll) or ((FShowNumbers=snQuarters) and ((i mod 3)=0)) then
            AlTextOut(XKoord(Center.X, XRadius-TextWidth('3')-2, 360-(i*30)),
                      YKoord(Center.Y, YRadius-(TextHeight('1') div 2)-4, 360-(i*30)),
                      IntToStr(i), taCenter, taCenter);
      end;

      { Zeiger: }
      Pen.Color:=FColorBorder;
      Brush.Color:=FColorBorder;
      Brush.Style:=bsSolid;
      Ellipse(Center.X-(XRadius div 10), Center.Y-(YRadius div 10),
              Center.X+(XRadius div 10), Center.Y+(YRadius div 10));
      Pen.Color:=FColorHands;
      { Stunden }
      Pen.Width:=4;
      Grad:=360-((FHour mod 12)*30);
      Grad:=Grad-round(30*(FMinute/60));
      MoveTo(Center.X, Center.Y);
      LineTo(XKoord(Center.X,XRadius div 2,Grad),
             YKoord(Center.Y,YRadius div 2,Grad));
      { Minuten }
      Pen.Width:=2;
      MoveTo(Center.X, Center.Y);
      LineTo(XKoord(Center.X,XRadius-4,360-(FMinute*6)),
             YKoord(Center.Y,YRadius-4,360-(FMinute*6)));
      { Sekunden }
      if FShowSeconds then begin
        Pen.Width:=1;
        Pen.Color:=FColorNumbers;
        MoveTo(XKoord(Center.X,5,180-(FSecond*6)),
               YKoord(Center.Y,5,180-(FSecond*6)));
        LineTo(XKoord(Center.X,XRadius-4,360-(FSecond*6)),
               YKoord(Center.Y,YRadius-4,360-(FSecond*6)));
      end;
    end;
    if Style=csMovingPoints then begin
      Brush.Color:=FColorBorder;
      Brush.Style:=bsSolid;
      ElXRadius:=((XRadius-(XRadius div 5)) div 2)-2;
      ElYRadius:=((YRadius-(YRadius div 5)) div 2)-2;
      Ellipse(Center.X-ElXRadius, Center.Y-ElYRadius,
              Center.X+ElXRadius, Center.Y+ElYRadius);
      { Stunden und Minuten }
      if (FMinute=0) or not FFadingColor then
        Brush.Color:=FColorHands
      else
        Brush.Color:=CalcShadowedColors(FColorHands, round(7-(7/(60-FMinute))));
      Pen.Color:=Brush.Color;
      Grad:=360-((FHour mod 12)*30);
      Grad:=Grad-round(30*(FMinute/60));
      ElXRadius:=XRadius div 5;
      ElYRadius:=YRadius div 5;
      ElXAbstand:=(XRadius-ElXRadius)/120;
      ElYAbstand:=(YRadius-ElYRadius)/120;
      if FMinute=0 then begin
        ElCenter.X:=XKoord(Center.X, XRadius-2, Grad);
        ElCenter.Y:=YKoord(Center.Y, YRadius-2, Grad);
      end
      else begin
        ElCenter.X:=XKoord(Center.X, XRadius-2-round((60-FMinute)*ElXAbstand), Grad);
        ElCenter.Y:=YKoord(Center.Y, YRadius-2-round((60-FMinute)*ElYAbstand), Grad);
      end;
      Pie(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
          ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius,
          XKoord(ElCenter.X, ElXRadius, Grad+135), YKoord(ElCenter.Y, ElYRadius, Grad+135),
          XKoord(ElCenter.X, ElXRadius, Grad-135), YKoord(ElCenter.Y, ElYRadius, Grad-135));
      { Sekunden }
      if FShowSeconds then begin
        Brush.Color:=FColorNumbers;
        Pen.Color:=Brush.Color;
        ElXRadius:=ElXRadius div 3;
        ElYRadius:=ElYRadius div 3;
        ElCenter.X:=XKoord(Center.X, (XRadius div 3), 360-(FSecond*6));
        ElCenter.Y:=YKoord(Center.Y, (YRadius div 3), 360-(FSecond*6));
        Ellipse(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
                ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius);
      end;
    end;
    if Style=csPieSlice then begin
      if (FMinute=0) or not FFadingColor then
        Brush.Color:=FColorHands
      else
        Brush.Color:=CalcShadowedColors(FColorHands, round(7-(7/(60-FMinute))));
      Pen.Color:=Brush.Color;
      { Stunden und Minuten }
      ElXAbstand:=(XRadius-(XRadius div 3)-4)/60;
      ElYAbstand:=(YRadius-(YRadius div 3)-4)/60;
      if FMinute=0 then begin
        ElXRadius:=(XRadius div 3)+round(ElXAbstand*60);
        ElYRadius:=(YRadius div 3)+round(ElYAbstand*60);
      end
      else begin
        ElXRadius:=(XRadius div 3)+round(ElXAbstand*FMinute);
        ElYRadius:=(YRadius div 3)+round(ElYAbstand*FMinute);
      end;
      Grad:=360-((FHour mod 12)*30);
      Grad:=Grad-round(30*(FMinute/60));
      Pie(Center.X-ElXRadius, Center.Y-ElYRadius,
          Center.X+ElXRadius, Center.Y+ElYRadius,
          XKoord(Center.X, ElXRadius, Grad), YKoord(Center.Y, ElYRadius, Grad),
          XKoord(Center.X, ElXRadius, 0), YKoord(Center.Y, ElYRadius, 0));
      Brush.Color:=FColorBorder;
      Brush.Style:=bsSolid;
      Pen.Color:=Brush.Color;
      Ellipse(Center.X-(XRadius div 3), Center.Y-(YRadius div 3),
              Center.X+(XRadius div 3), Center.Y+(YRadius div 3));
      { Sekunden }
      if FShowSeconds then begin
        Brush.Color:=FColorNumbers;
        Pen.Color:=Brush.Color;
        ElXRadius:=XRadius div 10;
        ElYRadius:=YRadius div 10;
        ElCenter.X:=XKoord(Center.X, (XRadius div 3), 360-(FSecond*6));
        ElCenter.Y:=YKoord(Center.Y, (YRadius div 3), 360-(FSecond*6));
        Ellipse(ElCenter.X-ElXRadius, ElCenter.Y-ElYRadius,
                ElCenter.X+ElXRadius, ElCenter.Y+ElYRadius);
      end;
    end;
    if (FOldWidth<>Self.Width) or (FOldHeight<>Self.Height) then begin
      FOldWidth:=Self.Width;
      FOldHeight:=Self.Height;
    end;
  end;
  Canvas.Draw(0,0,Buffer);
end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Simon', [TSRCalendar]);
  {$IFDEF SR_Delphi2_Up}
  RegisterComponents('Simon', [TSRClock]);
  {$ENDIF}
end;

end.
