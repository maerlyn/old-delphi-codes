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

unit fglib;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Menus,
  Forms, Dialogs, ExtCtrls, StdCtrls, DBTables, DB, MMsystem, StrDlg,
  OpenBmp, OpenWav, CurDlg, FgDrawLib, DT, PerpCal, Typefglib;


Const
  EnglishEmpty       = 'Empty';
  EnglishFull        = 'Click to delete';
  EnglishConfimation = 'Confirmation(Right) - Cancel(Left)';
  GermanEmpty        = 'leer';
  GermanFull         = 'abschaffen';
  GermanConfimation  = 'Bestätigung(recht) - Annulieren(Link)';
  FrenchEmpty        = 'Vide';
  FrenchFull         = 'Cliquez pour supprimer';
  FrenchConfimation  = 'Confirmer(Droit) - Annuler(Gauche)';
  ItalianEmpty       = 'Vuoto';
  ItalianFull        = 'Devastare';
  ItalianConfimation = 'Conferma(destro) - Annulare(Sinistra)';
  SpanishEmpty       = 'Vacio';
  SpanishFull        = 'Destruir';
  SpanishConfimation = 'Confirmacion(derecho) - Cancelar(izquierdo)';
  EnglishDay         = 'Day';
  EnglishWeek        = 'Week';
  EnglishPrior       = 'prior';
  EnglishNext        = 'next';
  EnglishGo          = 'go';
  GermanDay          = 'Tag';
  GermanWeek         = 'Woche';
  GermanPrior        = 'Folgendtag';
  GermanNext         = 'Vortag';
  GermanGo           = 'Gehen';
  FrenchDay          = 'Jour';
  FrenchWeek         = 'Semaine';
  FrenchPrior        = 'Jour précédent';
  FrenchNext         = 'Jour suivant';
  FrenchGo           = 'Aller à';
  ItalianDay         = 'Giorno';
  ItalianWeek        = 'Settimana';
  ItalianPrior       = 'anteriore';
  ItalianNext        = 'prossimo';
  ItalianGo          = 'andare';
  SpanishDay         = 'Diario';
  SpanishWeek        = 'Semana';
  SpanishPrior       = 'anterior';
  SpanishNext        = 'proximo';
  SpanishGo          = 'ir';

Const
  TrashTopHand = -1001;
  TrashHand = -1002;
  TrashMatch = -1003;
  TrashForbid = -1004;

type
  TTrashStyle = (tsStandard, tsEngine);

type
  TNotePad = class(TCustomControl)
  private
    FBackground : TPicture;
    Note : TMemo;
    FLines : TStringlist;
    FReadOnly : boolean;
    procedure SetLines(Value: TStringlist);
    procedure SetReadOnly(Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;
    procedure clear;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function GetTextLen: Integer;
    procedure SetTextBuf(Buffer: PChar);
    procedure SetBackground(Value: TPicture);
  published
    property Lines : TStringlist read FLines write SetLines;
    property ReadOnly : boolean read FReadOnly write SetReadOnly;
    property Background : TPicture read FBackground write SetBackground;
    property Font;
    property Visible;
  end;


TImageCtrl = class(TControl)
  private
    FCaption : string;
    FBmp : TBitmap;
  protected
    procedure ReadState(Reader: TReader); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption : string read FCaption write FCaption;
    property Bmp : TBitmap read FBmp write FBmp;
  end;

  TBmpAnime =class(TCustomControl)
  private
    FList: TList;
    FAccess: TStrings;
    FAnimecount : integer;
    FAutoSize : Boolean;
    FCenter : Boolean;
    FStretch : Boolean;
    procedure SetImageList(Value: TStrings);
    procedure SetAutoSize(Value: Boolean);
    procedure SetCenter(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    function GetActive : boolean;
    procedure SetActive(value : boolean);  
    procedure SetAnimationSpeed(value : integer);
    function GetAnimationSpeed : integer;
    procedure TimerPulse(Sender: TObject);
  protected
    procedure paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ReadState(Reader: TReader); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    FTimer : Ttimer;
    procedure AddImage(Name : String; Bmp : TBitmap);
    procedure DeleteImage(Index : Integer);
    constructor create(aowner : tcomponent); override;
    destructor Destroy; override;
  published
    property Active : boolean read GetActive write SetActive;
    property AnimationSpeed : integer read GetAnimationSpeed write SetAnimationSpeed;
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    property Center: Boolean read FCenter write SetCenter;
    property ImageList : TStrings read FAccess write SetImageList stored False;
    property Stretch: Boolean read FStretch write SetStretch;
    property Align;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
  end;

  TBmpMosaic = class(TGraphicControl)
  private
    CanvasMem : TBitmap;
    FPicture : TPicture;
    FPictureMos : TPicture;
    procedure SetPicture(Value: TPicture);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawMosaicPicture;
  published
    property Align;
    property Picture : TPicture read FPicture write SetPicture;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TBmpTransparent = class(TgraphicControl)
  private
    FPicture : TPicture;
    FAuto : boolean;
    FTransparentColor : TColor;
    procedure SetPicture(Value: TPicture);
    procedure SetAuto(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture : TPicture read FPicture write SetPicture;
    property Auto : boolean read FAuto write SetAuto;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor;
    property Align;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TIntensityFilter = class(TGraphicControl)
  private
    FIntensity : integer;
    FQuality : TQuality;
    FBorder : boolean;
    FBorderWidth : integer;
    FBorderIntensity : integer;   
    procedure SetIntensity(Value: integer);
    procedure SetQuality(Value: TQuality);
    procedure SetBorder(Value: boolean);
    procedure SetBorderWidth(Value: integer);
    procedure SetBorderIntensity(Value: integer);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Intensity : integer read FIntensity write SetIntensity;
    property Quality : TQuality read FQuality write SetQuality;
    property Border : boolean read FBorder write SetBorder;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth;
    property BorderIntensity : integer read FBorderIntensity write SetBorderIntensity;
    property visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TShadow = class(TGraphicControl)
  private
    FShadowIntensity : integer;
    FShadowQuality : TQuality;
    FShadowPosition : TShadowPos;
    FShadowWidth : integer;          
    procedure SetShadowIntensity(Value: integer);
    procedure SetShadowQuality(Value: TQuality);
    procedure SetShadowPosition(Value: TShadowPos);
    procedure SetShadowWidth(Value: integer);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property ShadowIntensity : integer read FShadowIntensity write SetShadowIntensity;
    property ShadowQuality : TQuality read FShadowQuality write SetShadowQuality;
    property ShadowPosition : TShadowPos read FShadowPosition write SetShadowPosition;
    property ShadowWidth : integer read FShadowWidth write SetShadowWidth;
    property visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  TPad3D = class(TGraphicControl)
  private
    FAlignment: TAlignment;
    FShadowIntensity : integer;
    FShadowQuality : TQuality;
    FShadowPosition : TShadowPos;
    FShadowWidth : integer;
    FBorderQuality : TQuality;
    FBorderWidth : integer;
    FBorderIntensity : integer;
    FPicture : TPicture;
    FBackground : TBackground;
    FCaption : string;           
    procedure SetShadowIntensity(Value: integer);
    procedure SetShadowQuality(Value: TQuality);
    procedure SetShadowPosition(Value: TShadowPos);
    procedure SetShadowWidth(Value: integer);
    procedure SetBorderQuality(Value: TQuality);
    procedure SetBorderWidth(Value: integer);
    procedure SetBorderIntensity(Value: integer);
    procedure SetPicture(Value: TPicture);
    procedure SetAlignment(Value: TAlignment);
    procedure SetBackground(Value: TBackground);
    procedure SetCaption(Value: string);
    procedure DoDrawText(var Rect: TRect; Flags: Word);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment;
    property Font;
    property Caption : String read FCaption write SetCaption;
    property Picture : TPicture read FPicture write SetPicture;
    property ShadowIntensity : integer read FShadowIntensity write SetShadowIntensity;
    property ShadowQuality : TQuality read FShadowQuality write SetShadowQuality;
    property ShadowPosition : TShadowPos read FShadowPosition write SetShadowPosition;
    property ShadowWidth : integer read FShadowWidth write SetShadowWidth;
    property BorderQuality : TQuality read FBorderQuality write SetBorderQuality;
    property BorderWidth : integer read FBorderWidth write SetBorderWidth;
    property BorderIntensity : integer read FBorderIntensity write SetBorderIntensity;
    property Background : TBackground read FBackground write SetBackground;
    property visible;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TOpenBitmap = class(TCommonDialog)
  private
    FFileName: TBmpFileName;
    FInitialDir: String;
    FTitle: String;
    FLanguage : TLanguage;
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property FileName: TBmpFileName read FFileName write FFileName;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

  TOpenWave = class(TCommonDialog)
  private
    FFileName: TWaveFileName;
    FInitialDir: String;
    FTitle: String;
    FLanguage : TLanguage;
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property FileName: TWaveFileName read FFileName write FFileName;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

  TCursorsDialog = class(TCommonDialog)
  private
    FCursor : TCursor;
    FTitle: String;
    FLanguage : TLanguage;
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Cursor: TCursor read FCursor write FCursor;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

  TTrashOnDeleteEvent = procedure(Sender: TObject; var AllowDelete: Boolean) of object;
  TTrashAsDeleteEvent = procedure(Sender: TObject) of object;

  TTrash = class(TGraphicControl)
  private
    FTrashBmp : TPicture;
    FPicture : TPicture;
    FTimer : Ttimer;
    FFull : boolean;
    FTrashStyle : TTrashStyle;
    FAnimationSpeed : integer;
    FSoundEffect : boolean;
    FWarningSoundFilename : TWaveFilename;
    FDestroySoundFilename : TWaveFilename;
    FCancelSoundFilename : TWaveFilename;
    FHintEmpty : string;
    FHintFull : string;
    FHintConfimation : string;
    FLanguage : TLanguage;
    Directory : string;
    Animationcount : integer;
    TranparentColor : TColor;
    FDataSource : TDataSource;
    FOnDelete: TTrashOnDeleteEvent;
    FAsDelete: TTrashAsDeleteEvent;
    procedure FTimerTimer(Sender: TObject);
    procedure SetFull(value : boolean);
    procedure SetTrashStyle(value : TTrashStyle);
    procedure SetAnimationSpeed(value : integer);
    procedure SetLanguage(value : TLanguage);
    procedure SetDataSource(value : TDataSource);
    procedure SetHint(Text: string);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure paint; override;
  public
    progress : integer;
    delete : boolean;
    constructor create(aowner : tcomponent); override;
    destructor Destroy; override;
  published
    property Full : boolean read FFull write SetFull;
    property TrashStyle : TTrashStyle read FTrashStyle write SetTrashStyle;
    property AnimationSpeed : integer read FAnimationSpeed write SetAnimationSpeed;
    property HintEmpty : string read FHintEmpty write FHintEmpty;
    property HintFull : string read FHintFull write FHintFull;
    property HintConfimation : string read FHintConfimation write FHintConfimation;
    property Language : TLanguage read FLanguage write SetLanguage;
    property SoundEffect : boolean read FSoundEffect write FSoundEffect;
    property WarningSoundFilename : TWaveFilename read FWarningSoundFilename write FWarningSoundFilename;
    property DestroySoundFilename : TWaveFilename read FDestroySoundFilename write FDestroySoundFilename;
    property CancelSoundFilename : TWaveFilename read FCancelSoundFilename write FCancelSoundFilename;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property Enabled;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnDelete: TTrashOnDeleteEvent read FOnDelete write FOnDelete;
    property AsDelete: TTrashAsDeleteEvent read FAsDelete write FAsDelete;
  end;

  TPlayWaveMode = (waCanStop, waNoStop, waLoop);

  TPlayWaveEvent = procedure(Sender: TObject;
    var AllowPlay : Boolean) of object;

  TPlayWave = class(TComponent)
  private
    FFilename : TFileName;
    FOnPlay: TPlayWaveEvent;
    FWaveMode : TPlayWaveMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure play;
    procedure stop;
  published
    property Filename : TFileName read FFilename write FFilename;
    property Mode : TPlayWaveMode read FWaveMode write FWaveMode;
    property OnPlay: TPlayWaveEvent read FOnPlay write FOnPlay;
  end;

  THintDesign = class(TComponent)
  private
    FPicture : TPicture;
    FBorder : Boolean;
    FBorderColor : TColor;
    FColor : TColor;
    FFont : TFont;
    FPosition : THintPosition;
    FShadow : boolean;
    FShadowQuality : TQuality;
    FShadowIntensity : integer;
    FShadowWidth : integer;
    FDelay : Integer;
    FHintStyle : THintStyle;
    FlinkStyle : TLinkStyle;
    procedure SetFont(Value:TFont);
    procedure SetDelay(Value:Integer);
    procedure SetPicture(Value: TPicture);
    procedure SetShadowIntensity(Value: integer);
    procedure SetShadowWidth(Value: integer);
    procedure CMFontChanged(var Message:TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure Loaded;override;
    procedure Reset;
  published
    property Picture : TPicture read FPicture write SetPicture;
    property Font : TFont read FFont write SetFont;
    property Color : TColor read FColor write FColor;
    property Position : THintPosition read FPosition write FPosition;
    property Shadow : boolean read FShadow write FShadow;
    property ShadowQuality : TQuality read FShadowQuality write FShadowQuality;
    property ShadowIntensity : integer read FShadowIntensity write SetShadowIntensity;
    property ShadowWidth : integer read FShadowWidth write SetShadowWidth;
    property Delay : Integer read FDelay write SetDelay;
    property HintStyle : THintStyle read FHintStyle write FHintStyle;
    property linkStyle : TLinkStyle read FlinkStyle write FlinkStyle;
    property Border : Boolean read FBorder write FBorder;
    property BorderColor : TColor read FBorderColor write FBorderColor;
  end;

  TNewDesign = class(THintWindow)
  private
    LinkSize : integer;
    RealTextHeight : integer;
    FlinkStyle : TLinkStyle;
    FHintDesign : THintDesign;
    FPosition : THintPosition;
    FShadow : Boolean;
    Left,Top,Width,Height : integer;
    CanvasTmp : TBitmap;
    function CheckLeft : boolean;
    function CheckRight : boolean;
    function CheckTop : boolean;
    function CheckBottom : boolean;
    procedure IsTopRightValid;
    procedure IsBottomRightValid;
    procedure IsTopLeftValid;
    procedure IsBottomLeftValid;
    function Search : THintDesign;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;
    procedure ActivateHint(Rect: TRect; const AHint: string);override;
  end;

  TMagicLabel = class(TCustomLabel)
  private
    FTimer : Ttimer;
    FCaption : TCaption;
    BlinkState : boolean;
    FBlinkTransparent : boolean;
    FBlinkColor : TColor;
    FBlinkTopColor : TColor;
    FBlinkBottomColor : TColor;
    FRaisedTop : boolean;
    FRaisedBottom : boolean;
    FRaisedTopColor : TColor;
    FRaisedBottomColor : TColor;
    FRaisedTopDistance : integer;
    FRaisedBottomDistance : integer;
    FShowAccelChar: Boolean;
    procedure SetBlink(value: boolean);
    procedure SetBlinkSpeed(Value: Word);
    function GetBlink : boolean;
    function GetBlinkSpeed : Word;
    procedure SetRaisedTop(Value: boolean);
    procedure SetRaisedBottom(Value: boolean);
    procedure SetRaisedTopColor(Value: TColor);
    procedure SetRaisedBottomColor(Value: TColor);
    procedure SetRaisedTopDistance(Value: integer);
    procedure SetRaisedBottomDistance(Value: integer);
    procedure DoDrawText(var Rect: TRect; colortext:TColor; Flags: Word);
    procedure TimerTimer(Sender: TObject);
    procedure AdjustBounds;
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Blink : boolean read GetBlink write SetBlink;
    property BlinkSpeed : Word read GetBlinkSpeed write SetBlinkSpeed;
    property BlinkColor : TColor read FBlinkColor write FBlinkColor;
    property BlinkTopColor : TColor read FBlinkTopColor write FBlinkTopColor;
    property BlinkBottomColor : TColor read FBlinkBottomColor write FBlinkBottomColor;
    property BlinkTransparent : boolean read FBlinkTransparent write FBlinkTransparent;
    property RaisedTop : boolean read FRaisedTop write SetRaisedTop;
    property RaisedBottom : boolean read FRaisedBottom write SetRaisedBottom;
    property RaisedTopColor : TColor read FRaisedTopColor write SetRaisedTopColor;
    property RaisedBottomColor : TColor read FRaisedBottomColor write SetRaisedBottomColor;
    property RaisedTopDistance : integer read FRaisedTopDistance write SetRaisedTopDistance;
    property RaisedBottomDistance : integer read FRaisedBottomDistance write SetRaisedBottomDistance;
    property Align;
    property Alignment;
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

TDateChangeEvent = procedure(Sender: TObject; NewDate: string;
    var AllowChange: Boolean) of object;


  TMiniCalendar = class(TGraphicControl)
  private
    FPicture : TPicture;
    FDayFontName : TFontName;
    FDayFontColor : TColor;
    FMonthFontName : TFontName;
    FMonthFontColor : TColor;
    FLanguage : TLanguage;
    procedure SetDayFontName(value : TFontName);
    procedure SetDayFontColor(value : TColor);
    procedure SetMonthFontName(value : TFontName);
    procedure SetMonthFontColor(value : TColor);
    procedure SetLanguage(value : TLanguage);
  protected
    procedure paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DayFontName : TFontName read FDayFontName write SetDayFontName;
    property DayFontColor : TColor read FDayFontColor write SetDayFontColor;
    property MonthFontName : TFontName read FMonthFontName write SetMonthFontName;
    property MonthFontColor : TColor read FMonthFontColor write SetMonthFontColor;
    property Language : TLanguage read FLanguage write SetLanguage;
    property Align;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TPerpetualCalendar = class(TComponent)
  private
    FLanguage : TLanguage;
    FTitle : string;
    FDate : string;
    FOnChange: TDateChangeEvent;
    FPosition : TPosition;
    FLeftPos : integer;
    FTopPos : integer;
    FForceToday : boolean;
    procedure SetFDate(value : string);
    procedure SetForceToday(value : boolean);
    procedure SetLanguage(value : TLanguage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property Language : TLanguage read FLanguage write SetLanguage;
    property Title : string read FTitle write FTitle;
    property Date : string read FDate write SetFDate;
    property Position : TPosition read FPosition write FPosition;
    property LeftPos : integer read FLeftPos write FLeftPos;
    property TopPos : integer read FTopPos write FTopPos;
    property ForceToday : boolean read FForceToday write SetForceToday;
    property OnChange: TDateChangeEvent read FOnChange write FOnChange;
  end;

  TCalendarPad = class(TCustomControl)
  private
    FBackground : TPicture;
    Previous,Next,go : boolean;
    Note : TMemo;
    FTimer : TTimer;
    TimeField : array[1..20] of TEdit;
    FDate : string;
    FHint : string;
    FLines1 : TStringlist;
    FLines2 : TStringlist;
    FMaxLength : integer;
    FReadOnly : boolean;
    FYearColor : TColor;
    FMonthColor : TColor;
    FDateColor : TColor;
    FDayColor : TColor;
    FDateBackgroundColor : TColor;
    FDayWeekColor : TColor;
    FTimeColor : TColor;
    FTimeFieldColor : TColor;
    FCurrentTimeColor : TColor;
    FStartTime : integer;
    FLanguage : TLanguage;
    FOnChange: TDateChangeEvent;
    FSoundEffect : boolean;
    FSoundFileName : string;
    FExtensionFile : string;
    FDirectory : string;
    FDestroyDelay : integer;
    StrYear,StrMonth,StrDay,StrDayOfWeek : string;
    FStrDay,FStrWeek,FStrPrior,FStrNext,FStrGo : string[15];
    procedure SetDate(value : string);
    procedure SetReadOnly(Value: boolean);
    procedure SetYearColor(Value: TColor);
    procedure SetMonthColor(Value: TColor);
    procedure SetDateColor(Value: TColor);
    procedure SetDayColor(Value: TColor);
    procedure SetDateBackgroundColor(Value: TColor);
    procedure SetDayWeekColor(Value: TColor);
    procedure SetTimeColor(Value: TColor);
    procedure SetTimeFieldColor(Value: TColor);
    procedure SetCurrentTimeColor(Value: TColor);
    procedure SetStartTime(Value: integer);
    procedure SetLanguage(value : TLanguage);
    procedure SetExtensionFile(Value: string);
    procedure SetDestroyDelay(Value: integer);
    procedure SetBackground(Value: TPicture);
    function formatdatetofile(value:string) : string;
    procedure RefreshDate;
    procedure RefreshLanguage;
    procedure SetHint(Text: string);
  protected
    procedure paint; override;
    procedure DblClick; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Mouseup(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);  override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer);  override;
    procedure pulse(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure clear; 
    procedure LoadDay;
    procedure SaveDay;  
    procedure DeleteDays;
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    function GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
    function GetTextLen: Integer;
    procedure SetTextBuf(Buffer: PChar);
  published
    property Background : TPicture read FBackground write SetBackground;
    property YearColor : TColor read FYearColor write SetYearColor;
    property MonthColor : TColor read FMonthColor write SetMonthColor;
    property DateColor : TColor read FDateColor write SetDateColor;
    property DayColor : TColor read FDayColor write SetDayColor;
    property DateBackgroundColor : TColor read FDateBackgroundColor write SetDateBackgroundColor;
    property DayWeekColor : TColor read FDayWeekColor write SetDayWeekColor;
    property TimeColor : TColor read FTimeColor write SetTimeColor;
    property TimeFieldColor : TColor read FTimeFieldColor write SetTimeFieldColor;
    property CurrentTimeColor : TColor read FCurrentTimeColor write SetCurrentTimeColor;
    property Language : TLanguage read FLanguage write SetLanguage;
    property Date : string read FDate write SetDate;
    property ExtensionFile : string read FExtensionFile write SetExtensionFile;
    property StartTime : Integer read FStartTime write SetStartTime;
    property DestroyDelay : Integer read FDestroyDelay write SetDestroyDelay;
    property Font;
    property ShowHint;
    property Visible;
    property Directory : string read FDirectory write FDirectory;
    property SoundFileName : string read FSoundFileName write FSoundFileName;
    property SoundEffect : boolean read FSoundEffect write FSoundEffect;
    property OnChange: TDateChangeEvent read FOnChange write FOnChange;
  end;

  TClockMark = (maNone, maNumber, maPoint, maSquare);

  TDigitalClock = class(TMagicLabel)
  private
    FTimer : TTimer;
    FDelayRefresh : longint;
    FSecond : boolean;         
    procedure SetDelayRefresh(Value: longint);
    procedure SetSecond(Value: boolean);
  protected
    procedure pulse(Sender: TObject);
  public
    constructor create(aowner : tcomponent); override;
    destructor Destroy; override;
  published
    property DelayRefresh : longint read FDelayRefresh write SetDelayRefresh;
    property Second : boolean read FSecond write SetSecond;
  end;

  TAnalogicClock = class(TGraphicControl)
  private
    CanvasMem : TBitmap;
    FTimer : TTimer;
    FPicture : TPicture;
    FTransparentPicture : boolean;
    FTransparentColor : Tcolor;
    FDelayRefresh : longint;
    FSecond : boolean;
    FMinuteSize : integer;
    FSecondSize : integer;
    FHourSize : integer;
    FMinuteWidth : integer;
    FSecondWidth : integer;
    FHourWidth : integer;
    FMinuteColor : TColor;
    FSecondColor : TColor;
    FHourColor : TColor;
    FMark : TClockMark;
    FMarkColor : TColor;
    FMarkWidth : integer; 
    FMarkDistance : integer;     
    procedure SetPicture(Value: TPicture);
    procedure SetDelayRefresh(Value: longint);
    procedure SetSecond(Value: boolean);
    procedure SetMinuteSize(Value: integer);
    procedure SetSecondSize(Value: integer);
    procedure SetHourSize(Value: integer);
    procedure SetMinuteWidth(Value: integer);
    procedure SetSecondWidth(Value: integer);
    procedure SetHourWidth(Value: integer);
    procedure SetMinuteColor(Value: TColor);
    procedure SetSecondColor(Value: TColor);
    procedure SetHourColor(Value: TColor);
    procedure SetMark(Value: TClockMark);
    procedure SetMarkColor(Value: TColor);
    procedure SetMarkWidth(Value: integer);
    procedure SetMarkDistance(Value: integer);
    procedure SetTransparentPicture(Value: boolean);
    procedure SetTransparentColor(Value: TColor);
  protected
    procedure paint; override;
    procedure pulse(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Picture : TPicture read FPicture write SetPicture;
    property TransparentPicture : boolean read FTransparentPicture write SetTransparentPicture;
    property TransparentColor : Tcolor read FTransparentColor write SetTransparentColor;
    property DelayRefresh : longint read FDelayRefresh write SetDelayRefresh;
    property Second : boolean read FSecond write SetSecond;
    property MinuteSize : integer read FMinuteSize write SetMinuteSize;
    property SecondSize : integer read FSecondSize write SetSecondSize;
    property HourSize : integer read FHourSize write SetHourSize;
    property MinuteWidth : integer read FMinuteWidth write SetMinuteWidth;
    property SecondWidth : integer read FSecondWidth write SetSecondWidth;
    property HourWidth : integer read FHourWidth write SetHourWidth;
    property MinuteColor : Tcolor read FMinuteColor write SetMinuteColor;
    property SecondColor : Tcolor read FSecondColor write SetSecondColor;
    property HourColor : Tcolor read FHourColor write SetHourColor;
    property Mark : TClockMark read FMark write SetMark;
    property MarkColor : TColor read FMarkColor write SetMarkColor;
    property MarkWidth : integer read FMarkWidth write SetMarkWidth;
    property MarkDistance : integer read FMarkDistance write SetMarkDistance;
    property Align;
    property Enabled;
    property Font;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TAlarmEvent = procedure(Sender: TObject; Text: string; var PlaySound: Boolean) of object;

  TAlarmData = class(TPersistent)
  private
    FTime : TTime;
    FDate : TDate;
    FText : String;
    FRepeatAlarm : Boolean;
    FFrequency : Word;
    FSoundFileName : TFileName;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Time : TTime read FTime write FTime;
    property Date : TDate read FDate write FDate;
    property Text : String read FText write FText;
    property RepeatAlarm : Boolean read FRepeatAlarm write FRepeatAlarm;
    property Frequency : Word read FFrequency write FFrequency;
    property SoundFileName : TFileName read FSoundFileName write FSoundFileName;
  end;

  TAlarm = class(TComponent)
  private
    FTimer : TTimer;
    FAlarmSet : TAlarmData;
    FOnAlarm : TAlarmEvent; 
    function GetEnabled : Boolean;
    procedure SetEnabled(Value: Boolean);
  protected
    procedure pulse(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property AlarmSet: TAlarmData read FAlarmSet write FAlarmSet;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property OnAlarm: TAlarmEvent read FOnAlarm write FOnAlarm;
  end;


implementation


{-----------------------------------------------------------------------------}
{  Component : TNotePad                                                       }
{-----------------------------------------------------------------------------}
constructor TNotePad.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  height := 278;
  width := 387;
  FBackground := TPicture.Create;
  FBackground.bitmap.handle := loadbitmap(hinstance,'notepad');
  Note := TMemo.create(self);
  with Note do
  begin
    parent := self;
    Left := 26;
    Top := 56;
    Width := 333;
    Height := 203;
    BorderStyle := bsNone;
    Color := clWhite;
    Ctl3D := False;
    Font := self.font;
    MaxLength := 0;
    ParentCtl3D := False;
    ParentFont := true;
    TabOrder := 0;
    ParentShowHint := true;
  end;
  FLines := TStringlist.Create;
end;

destructor TNotePad.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TNotePad.paint;
var
  Dest: TRect;
begin
  inherited paint;
  if ((FBackground.bitmap.Width<=0) and (FBackground.bitmap.Height<=0)) then
  FBackground.bitmap.handle := loadbitmap(hinstance,'notepad');
  Dest := Rect(0, 0, FBackground.Graphic.Width, FBackground.Graphic.Height);
  SetBounds(left, top, 387, 278);
  with inherited Canvas do StretchDraw(Dest, FBackground.Graphic);
  Note.lines := FLines;
  Note.ReadOnly := ReadOnly;
end;

procedure TNotePad.SetLines(Value: TStringlist);
begin
  FLines.assign(value);
  refresh;
end;

procedure TNotePad.SetReadOnly(Value: boolean);
begin
  FReadOnly := value;
  Note.ReadOnly := value;
end;

function TNotePad.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  result := Note.GetTextBuf(Buffer,BufSize);
end;

function TNotePad.GetTextLen: Integer;
begin
  result := Note.GetTextLen;
end;

procedure TNotePad.SetTextBuf(Buffer: PChar);
begin
  Note.SetTextBuf(Buffer);
end;

procedure TNotePad.SetBackground(Value: TPicture);
begin
  FBackground.Assign(Value);
  refresh;
end;

procedure TNotePad.LoadFromFile(const FileName: string);
begin
  FLines.LoadFromFile(FileName);
  Note.lines := FLines;
end;

procedure TNotePad.SaveToFile(const FileName: string);
begin
  FLines.assign(Note.lines);
  FLines.SaveToFile(FileName);
end;

procedure TNotePad.clear;
begin
  FLines.clear;
  Note.lines := FLines;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  TImageCtrl                                                                       }
{-----------------------------------------------------------------------------}
constructor TImageCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Bmp := TBitmap.Create;
end;

destructor TImageCtrl.Destroy;
begin
  Bmp.Free;
  inherited Destroy;
end;

procedure TImageCtrl.ReadState(Reader: TReader);
var
  OldOwner: TComponent;
begin
  if Reader.Parent is TBmpAnime then
  TBmpAnime(Reader.Parent).FList.Add(Self);
  OldOwner := Reader.Owner;
  Reader.Owner := Reader.Root;
  try
    inherited ReadState(Reader);
  finally
    Reader.Owner := OldOwner;
  end;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  TImageListAccess                                                                 }
{-----------------------------------------------------------------------------}
type
  TImageListAccess = class(TStrings)
  private
    FList: TList;
    FTabsDesign: TBmpAnime;
  protected
    function GetCount: Integer; override;
    function Get(Index: Integer): string; override;
    procedure Put(Index: Integer; const S: string); override;
    function GetObject(Index: Integer): TObject; override;
  public
    constructor Create(AList: TList; ATabsDesign: TBmpAnime);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

constructor TImageListAccess.Create(AList: TList; ATabsDesign: TBmpAnime);
begin
  inherited Create;
  FList := AList;
  FTabsDesign := ATabsDesign;
end;

function TImageListAccess.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TImageListAccess.Get(Index: Integer): string;
begin
  result := TImageCtrl(FList[Index]).Caption;
end;

procedure TImageListAccess.Put(Index: Integer; const S: string);
begin
  TImageCtrl(FList[Index]).Caption := S;
end;

function TImageListAccess.GetObject(Index: Integer): TObject;
begin
  Result := FList[Index];
end;

procedure TImageListAccess.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do TImageCtrl(FList[I]).Free;
  FList.Clear;
end;

procedure TImageListAccess.Delete(Index: Integer);
var
  Form: TCustomForm;
begin
  TImageCtrl(FList[Index]).Free;
  FList.Delete(Index);
  if csDesigning in FTabsDesign.ComponentState then
  begin
    Form := GetParentForm(FTabsDesign);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
end;

procedure TImageListAccess.Insert(Index: Integer; const S: string);
var
  Tab: TImageCtrl;
  Form: TCustomForm;
begin
  Tab := TImageCtrl.Create(FTabsDesign);
  with Tab do
  begin
    Parent := FTabsDesign;
    Caption := S;
  end;
  FList.Insert(Index, Tab);


  if csDesigning in FTabsDesign.ComponentState then
  begin
    Form := GetParentForm(FTabsDesign);
    if (Form <> nil) and (Form.Designer <> nil) then
      Form.Designer.Modified;
  end;
end;

procedure TImageListAccess.Move(CurIndex, NewIndex: Integer);
var
  AObject: TObject;
begin
  if CurIndex <> NewIndex then
  begin
    AObject := FList[CurIndex];
    FList[CurIndex] := FList[NewIndex];
    FList[NewIndex] := AObject;
  end;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TBmpAnime                                                      }
{-----------------------------------------------------------------------------}
constructor TBmpAnime.create(aowner : tcomponent);
const
  Registered: Boolean = False;
begin
  inherited create(aowner);    
  height := 150;
  width := 150;
  Brush.Style := bsClear;
  FList := TList.Create;
  FAccess := TImageListAccess.Create(FList, Self);
  FAnimecount := 0;
  FAutoSize := false;
  FCenter := false;
  FStretch := false;
  FTimer := ttimer.create(self);
  FTimer.interval := 80;
  FTimer.enabled := true;
  FTimer.ontimer := TimerPulse;
  if not Registered then
  begin
    Classes.RegisterClasses([TImageCtrl]);
    Registered := True;
  end;
end;

destructor TBmpAnime.Destroy;
begin
  FAccess.Free;
  FList.Free;
  inherited Destroy;
end;

procedure TBmpAnime.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do Style := Style or WS_CLIPCHILDREN;
end;

procedure TBmpAnime.ReadState(Reader: TReader);
var
  OldOwner: TComponent;
begin
  ImageList.Clear;
  OldOwner := Reader.Owner;
  Reader.Owner := Self;
  try
    inherited ReadState(Reader);
  finally
    Reader.Owner := OldOwner;
  end;
end;

procedure TBmpAnime.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    Proc(FList[I]);
end;

procedure TBmpAnime.paint;
var
  BmpRect : TRect;
begin
  if FList.count>0 then
  begin
  brush.style := bsclear;
  if FAutoSize then SetBounds(Left, Top, TImageCtrl(FList[FAnimecount]).Bmp.Width, TImageCtrl(FList[FAnimecount]).Bmp.Height);
  if Stretch then BmpRect := ClientRect
  else if Center then
    BmpRect := Bounds((Width - TImageCtrl(FList[FAnimecount]).Bmp.Width) div 2,
                      (Height - TImageCtrl(FList[FAnimecount]).Bmp.Height) div 2,
                      TImageCtrl(FList[FAnimecount]).Bmp.Width,
                      TImageCtrl(FList[FAnimecount]).Bmp.Height)
  else BmpRect := Rect(0, 0, TImageCtrl(FList[FAnimecount]).Bmp.Width, TImageCtrl(FList[FAnimecount]).Bmp.Height);
  Canvas.StretchDraw(BmpRect,TImageCtrl(FList[FAnimecount]).Bmp);
  end else Canvas.rectangle(0,0,width,height);
end;

procedure TBmpAnime.SetImageList(Value: TStrings);
begin
  FAccess.Assign(Value);
  refresh;
end;

procedure TBmpAnime.TimerPulse(Sender: TObject);
begin
  if FAnimecount<FList.count-1 then inc(FAnimecount) else FAnimecount := 0;
  Refresh;
end;

function TBmpAnime.GetActive : boolean;
begin
  result := FTimer.enabled;
  FAnimecount := 0;
end;

procedure TBmpAnime.SetActive(value : boolean);
begin
  FTimer.enabled := value;
  FAnimecount := 0;
end;

procedure TBmpAnime.SetAnimationSpeed(value : integer);
begin
  FTimer.interval := value;
end;

function TBmpAnime.GetAnimationSpeed : integer;
begin
  result := FTimer.interval;
end;

procedure TBmpAnime.SetAutoSize(Value: Boolean);
begin
  FAutoSize := Value;
  Refresh;
end;

procedure TBmpAnime.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    Refresh;
  end;
end;

procedure TBmpAnime.SetStretch(Value: Boolean);
begin
  FStretch := Value;
  Refresh;
end;

procedure TBmpAnime.AddImage(Name : String; Bmp : TBitmap);
begin
  FAccess.Add(Name);
  TImageCtrl(FList[FList.count-1]).Bmp.assign(Bmp);
end;

procedure TBmpAnime.DeleteImage(Index : Integer);
begin
  if FAnimecount<=FList.count-1 then FAccess.Delete(Index);
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TBmpMosaic                                                     }
{-----------------------------------------------------------------------------}
constructor TBmpMosaic.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  Height := 105;
  Width := 105;
  FPicture := TPicture.Create;
  FPictureMos := TPicture.Create;
  CanvasMem := TBitmap.Create;
  CanvasMem.assign(nil);
end;

destructor TBmpMosaic.Destroy;
begin
  FPicture.Free;
  FPictureMos.Free;
  CanvasMem.free;
  inherited Destroy;
end;   

procedure TBmpMosaic.DrawMosaicPicture;
begin
  CanvasMem.assign(nil);
  CanvasMem.releasehandle;
  CanvasMem.Width := Width;
  CanvasMem.height := height;
  DrawMosaic(CanvasMem.Canvas.handle,0,0,Width,Height,FPicture.bitmap.handle);
end;

procedure TBmpMosaic.paint;
var
  BmpTmp : TBitmap;
begin
  if FPicture.width=0 then
  with inherited Canvas do
  begin
    Pen.Style := psDash;
    Brush.Style := bsClear;
    Rectangle(0, 0, Width, Height);
  end;
  if CanvasMem.Width<>width then DrawMosaicPicture;
  Canvas.CopyRect(ClientRect,CanvasMem.Canvas,ClientRect);
end;

procedure TBmpMosaic.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  DrawMosaicPicture;
  paint;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TBmpTransparent                                                }
{-----------------------------------------------------------------------------}
constructor TBmpTransparent.create(aowner : tcomponent);
begin
  inherited Create(AOwner);   
  Height := 105;
  Width := 105;
  FPicture := TPicture.create;
  FAuto := false;
  FTransparentColor := clOlive;
end;

destructor TBmpTransparent.Destroy;
begin
  FPicture.free;
  inherited Destroy;
end; 

procedure TBmpTransparent.paint;
var
  CanvasMem : TBitmap;
  BmpTmp : TBitmap;
  TC : tcolor;
  IT : integer;
  x,y : integer;
  sh : boolean;
  TextColor : TColor;
begin
  inherited paint;
  if FPicture.width>0 then
  begin
    x := trunc((width-FPicture.width)/2);
    y := trunc((height-FPicture.height)/2);
    BmpTmp := TBitmap.create;
    BmpTmp.assign(FPicture.bitmap);
    if FAuto then TC := getpixel(BmpTmp.Canvas.Handle,0,0) else TC := FTransparentColor;
    TransparentBitmap(canvas.handle,x,y,BmpTmp.handle,TC,0);
    BmpTmp.free;
  end else
  if csDesigning in ComponentState then
  with inherited Canvas do
  begin
    Pen.Style := psDash;
    Brush.Style := bsClear;
    Rectangle(0, 0, Width, Height);
  end;
end;

procedure TBmpTransparent.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  Refresh;
end;

procedure TBmpTransparent.SetAuto(Value: boolean);
begin
  FAuto := Value;
  Refresh;
end;

procedure TBmpTransparent.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := Value;
  Refresh;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TIntensityFilter                                                         }
{-----------------------------------------------------------------------------}
constructor TIntensityFilter.create(aowner : tcomponent);
begin
  inherited Create(AOwner); 
  Height := 105;
  Width := 105;
  FIntensity := -60;
  FBorder := false;
  FBorderWidth := 5;
  FBorderIntensity := 40;
  FQuality := quLow;
end;

destructor TIntensityFilter.Destroy;
begin
  inherited Destroy;
end;       

procedure TIntensityFilter.paint;
var
  CanvasMem : TBitmap;
  BW,BI : integer;
  FilterRect : TRect;
begin
  CanvasMem := TBitmap.Create;
  CanvasMem.Width := Width;
  CanvasMem.height := height;
  CanvasMem.Canvas.CopyRect(ClientRect,Canvas,ClientRect);
  if FBorder then
  begin
    BW := FBorderWidth;
    BI := FBorderIntensity;
    DrawBorderRect(CanvasMem.Canvas.handle, 0, 0, width, height, BW, BI, 0, 1, FQuality);
    FilterRect := rect(BW+1, BW+1, width-BW-1, height-BW-1);
  end else FilterRect := rect(0, 0, width, height);
  DrawFilter(CanvasMem.Canvas.handle,
             FilterRect.left,
             FilterRect.top,
             FilterRect.right,
             FilterRect.bottom,
             FIntensity, FQuality);
  inherited canvas.CopyRect(ClientRect,CanvasMem.Canvas,ClientRect);
  CanvasMem.free;
end;

procedure TIntensityFilter.SetIntensity(Value: integer);
begin
  FIntensity := Value;
  Refresh;
end;

procedure TIntensityFilter.SetQuality(Value: TQuality);
begin
  FQuality := Value;
  Refresh;
end;

procedure TIntensityFilter.SetBorder(Value: boolean);
begin
  FBorder := Value;
  Refresh;
end;

procedure TIntensityFilter.SetBorderWidth(Value: integer);
begin
  FBorderWidth := Value;
  Refresh;
end;

procedure TIntensityFilter.SetBorderIntensity(Value: integer);
begin
  FBorderIntensity := Value;
  Refresh;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TShadow                                                        }
{-----------------------------------------------------------------------------}
constructor TShadow.create(aowner : tcomponent);
begin
  inherited Create(AOwner); 
  Height := 105;
  Width := 105;
  FShadowIntensity := 60;
  FShadowQuality := quLow;
  FShadowPosition := shBottomRight;
  FShadowWidth := 4;
end;

destructor TShadow.Destroy;
begin
  inherited Destroy;
end;      

procedure TShadow.paint;
var
  CanvasMem : TBitmap;
  BW,BI : integer;
begin
  CanvasMem := TBitmap.Create;
  CanvasMem.Width := Width;
  CanvasMem.height := height;
  CanvasMem.Canvas.CopyRect(ClientRect,Canvas,ClientRect);
  DrawShadow(CanvasMem.Canvas.handle, 0, 0, width, height, FShadowWidth, -FShadowIntensity, FShadowPosition, FShadowQuality);
  inherited canvas.CopyRect(ClientRect,CanvasMem.Canvas,ClientRect);
  CanvasMem.free;
end;

procedure TShadow.SetShadowIntensity(Value: integer);
begin
  FShadowIntensity := Value;
  Refresh;
end;

procedure TShadow.SetShadowQuality(Value: TQuality);
begin
  FShadowQuality := Value;
  Refresh;
end;

procedure TShadow.SetShadowPosition(Value: TShadowPos);
begin
  FShadowPosition := Value;
  Refresh;
end;

procedure TShadow.SetShadowWidth(Value: integer);
begin
  FShadowWidth := Value;
  Refresh;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TPad3D                                                           }
{-----------------------------------------------------------------------------}
constructor TPad3D.create(aowner : tcomponent);
begin
  inherited Create(AOwner);     
  Height := 50;
  Width := 200;
  FPicture := TPicture.create;
  FPicture.bitmap.handle := loadbitmap(hinstance,'Background');
  FShadowIntensity := 60;
  FShadowQuality := quLow;
  FShadowPosition := shBottomRight;
  FShadowWidth := 4;
  FBorderIntensity := 40;
  FBorderQuality := quLow;
  FBorderWidth := 3;
  FAlignment := taCenter;
  FBackground := bgMosaic;
  FCaption := '';
end;

destructor TPad3D.Destroy;
begin
  FPicture.free;
  inherited Destroy;
end;        

procedure TPad3D.DoDrawText(var Rect: TRect; Flags: Word);
var
  Text: array[0..255] of Char;
begin
  GetTextBuf(Text, SizeOf(Text));
  StrPCopy(Text,FCaption);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) and
    (Text[0] = '&') and (Text[1] = #0)) then StrCopy(Text, ' ');
  DrawText(Canvas.Handle, Text, StrLen(Text), Rect, Flags);
end;

procedure TPad3D.paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  CanvasMem : TBitmap;
  TextRect: TRect;
  TextTop,TextHeight : integer;
begin
  inherited paint;
  if FBackground=bgPicture then
  begin
    width := FPicture.width+FShadowWidth;
    height := FPicture.height+FShadowWidth;
  end;
  CanvasMem := TBitmap.Create;
  CanvasMem.Width := Width;
  CanvasMem.height := height;
  CanvasMem.Canvas.CopyRect(ClientRect,Canvas,ClientRect);
  Canvas.Font := Font;
  TextHeight := Canvas.TextHeight(Caption);
  TextTop := trunc((height-FShadowWidth-TextHeight)/2);
  case FShadowPosition of
  shBottomRight:
  begin
  TextRect := Rect(0, TextTop, width-FShadowWidth, TextTop+TextHeight);
  if FPicture.bitmap.width>0 then
  begin
  if FBackground=bgMosaic then
  DrawMosaic(CanvasMem.Canvas.handle,0,0,Width-FShadowWidth,Height-FShadowWidth,FPicture.bitmap.handle)
  else CanvasMem.Canvas.Draw(0,0,FPicture.bitmap);
  end;
  DrawBorderRect(CanvasMem.Canvas.handle, 0, 0, Width-FShadowWidth-1, Height-FShadowWidth-1,
                 FBorderWidth, FBorderIntensity, 0, 1, FBorderQuality);
  end;
  shBottomLeft:
  begin
  TextRect := Rect(FShadowWidth, TextTop, width, TextTop+TextHeight);
  if FPicture.bitmap.width>0 then
  begin
  if FBackground=bgMosaic then
  DrawMosaic(CanvasMem.Canvas.handle,FShadowWidth+1,0,Width-FShadowWidth,Height-FShadowWidth,FPicture.bitmap.handle)
  else CanvasMem.Canvas.Draw(FShadowWidth+1,0,FPicture.bitmap);
  end;
  DrawBorderRect(CanvasMem.Canvas.handle, FShadowWidth+1, 0, Width, Height-FShadowWidth-1,
                 FBorderWidth, FBorderIntensity, 0, 1, FBorderQuality);
  end;
  shTopLeft:
  begin
  TextRect := Rect(FShadowWidth, TextTop+FShadowWidth, Width, TextTop+FShadowWidth+TextHeight);    
  if FPicture.bitmap.width>0 then
  begin
  if FBackground=bgMosaic then
  DrawMosaic(CanvasMem.Canvas.handle,FShadowWidth+1,FShadowWidth+1,
             Width-FShadowWidth,Height-FShadowWidth,FPicture.bitmap.handle)
  else CanvasMem.Canvas.Draw(FShadowWidth+1,FShadowWidth+1,FPicture.bitmap);
  end;
  DrawBorderRect(CanvasMem.Canvas.handle, FShadowWidth+1, FShadowWidth+1, Width, Height,
                 FBorderWidth, FBorderIntensity, 0, 1, FBorderQuality);
  end;
  shTopRight:
  begin
  TextRect := Rect(0, TextTop+FShadowWidth, Width-FShadowWidth, TextTop+FShadowWidth+TextHeight);  
  if FPicture.bitmap.width>0 then
  begin
  if FBackground=bgMosaic then
  DrawMosaic(CanvasMem.Canvas.handle,0,FShadowWidth+1,Width-FShadowWidth,Height-FShadowWidth,FPicture.bitmap.handle)
  else CanvasMem.Canvas.Draw(0,FShadowWidth+1,FPicture.bitmap);
  end;
  DrawBorderRect(CanvasMem.Canvas.handle, 0, FShadowWidth+1, Width-FShadowWidth-1, Height,
                 FBorderWidth, FBorderIntensity, 0, 1, FBorderQuality);
  end;
  end;
  DrawShadow(CanvasMem.Canvas.handle, 0, 0, width, height, FShadowWidth, -FShadowIntensity, FShadowPosition, FShadowQuality);
  with canvas do
  begin
    SetBkMode(Handle,TRANSPARENT);
    CopyRect(ClientRect,CanvasMem.Canvas,ClientRect);
  end;
  DoDrawText(TextRect, (DT_EXPANDTABS or DT_WORDBREAK) or Alignments[Alignment]);
  CanvasMem.free;
end;

procedure TPad3D.SetShadowIntensity(Value: integer);
begin
  FShadowIntensity := Value;
  Refresh;
end;

procedure TPad3D.SetShadowQuality(Value: TQuality);
begin
  FShadowQuality := Value;
  Refresh;
end;

procedure TPad3D.SetShadowPosition(Value: TShadowPos);
begin
  FShadowPosition := Value;
  Refresh;
end;

procedure TPad3D.SetShadowWidth(Value: integer);
begin
  FShadowWidth := Value;
  Refresh;
end;

procedure TPad3D.SetBorderQuality(Value: TQuality);
begin
  FBorderQuality := Value;
  Refresh;
end;

procedure TPad3D.SetBorderWidth(Value: integer);
begin
  FBorderWidth := Value;
  Refresh;
end;

procedure TPad3D.SetBorderIntensity(Value: integer);
begin
  FBorderIntensity := Value;
  Refresh;
end;

procedure TPad3D.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  Refresh;
end;

procedure TPad3D.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TPad3D.SetBackground(Value: TBackground);
begin
  FBackground := Value;
  Refresh;
end;

procedure TPad3D.SetCaption(Value: string);
begin
  FCaption := Value;
  Refresh;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TOpenBitmap                                                    }
{-----------------------------------------------------------------------------}
constructor TOpenBitmap.create(aowner : tcomponent);
begin
  inherited Create(AOwner); 
  FFileName := '';
  FInitialDir := '';
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TOpenBitmap.Destroy;
begin
  inherited Destroy;
end;    

function TOpenBitmap.Execute: Boolean;
var
  OpenBmpDlg : TOpenBmpDlg;
begin
  OpenBmpDlg := TOpenBmpDlg.create(application);
  OpenBmpDlg.FileName := FFileName;
  OpenBmpDlg.InitialDir := FInitialDir;
  OpenBmpDlg.Title := FTitle;
  OpenBmpDlg.Language := FLanguage;
  OpenBmpDlg.Ctl3D := Ctl3D;
  if OpenBmpDlg.showmodal=mrok then
  begin
    FFileName := OpenBmpDlg.FileName;
    FInitialDir := OpenBmpDlg.InitialDir;
    FTitle := OpenBmpDlg.Title;
    result := true;
  end else
  begin
    FFileName := '';
    FInitialDir := '';
    FTitle := '';
    result := false;
  end;
  OpenBmpDlg.free;
end;

procedure TOpenBitmap.SetLanguage(value : TLanguage);
begin
  FLanguage := value;
  LanguageBuff := FLanguage;
end;

function TOpenBitmap.GetLanguage : TLanguage;
begin
  result := FLanguage;
  LanguageBuff := FLanguage;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TOpenWave                                                      }
{-----------------------------------------------------------------------------}
constructor TOpenWave.create(aowner : tcomponent);
begin
  inherited Create(AOwner);  
  FFileName := '';
  FInitialDir := '';
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TOpenWave.Destroy;
begin
  inherited Destroy;
end;    

function TOpenWave.Execute: Boolean;
var
  OpenWavDlg : TOpenWavDlg;
begin
  OpenWavDlg := TOpenWavDlg.create(application);
  OpenWavDlg.FileName := FFileName;
  OpenWavDlg.InitialDir := FInitialDir;
  OpenWavDlg.Title := FTitle;
  OpenWavDlg.Language := FLanguage;
  OpenWavDlg.Ctl3D := Ctl3D;
  if OpenWavDlg.showmodal=mrok then
  begin
    FFileName := OpenWavDlg.FileName;
    FInitialDir := OpenWavDlg.InitialDir;
    FTitle := OpenWavDlg.Title;
    result := true;
  end else
  begin
    FFileName := '';
    FInitialDir := '';
    FTitle := '';
    result := false;
  end;
  OpenWavDlg.free;
end;

procedure TOpenWave.SetLanguage(value : TLanguage);
begin
  FLanguage := value;
  LanguageBuff := FLanguage;
end;

function TOpenWave.GetLanguage : TLanguage;
begin
  result := FLanguage;
  LanguageBuff := FLanguage;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TCursorsDialog                                                 }
{-----------------------------------------------------------------------------}
constructor TCursorsDialog.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  FCursor := crDefault;
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TCursorsDialog.Destroy;
begin
  inherited Destroy;
end;   

function TCursorsDialog.Execute: Boolean;
var
  CursorsDlg : TCursorsDlg;
begin
  CursorsDlg := TCursorsDlg.create(application);
  CursorsDlg.NewCursor := FCursor;
  CursorsDlg.Title := FTitle;
  CursorsDlg.Language := FLanguage;
  CursorsDlg.Ctl3D := Ctl3D;
  if CursorsDlg.showmodal=mrok then
  begin
    FCursor := CursorsDlg.NewCursor;
    result := true;
  end else
  begin
    result := false;
  end;
  CursorsDlg.free;
end;

procedure TCursorsDialog.SetLanguage(value : TLanguage);
begin
  FLanguage := value;
  LanguageBuff := FLanguage;
end;

function TCursorsDialog.GetLanguage : TLanguage;
begin
  result := FLanguage;
  LanguageBuff := FLanguage;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TTrash                                                         }
{-----------------------------------------------------------------------------}
constructor TTrash.create(aowner : tcomponent);
var
  Dir : string;
begin
  inherited create(aowner);
  getdir(0,Dir);
  height := 49;
  width := 40;
  showhint := true;
  FTrashBmp := TPicture.create;
  FPicture := TPicture.Create;
  FTimer := ttimer.create(self);
  FTimer.enabled := false;
  FTimer.ontimer := FTimertimer;
  FFull := false;
  FAnimationSpeed := 80;
  FSoundEffect := true;
  FWarningSoundFilename := lowercase(Dir)+'\match.wav';
  FDestroySoundFilename := lowercase(Dir)+'\fire.wav';
  FCancelSoundFilename := lowercase(Dir)+'\cancel.wav';
  HintEmpty := EnglishEmpty;
  HintFull := EnglishFull;
  HintConfimation := EnglishConfimation;
  FLanguage := laEnglish;
  FTrashStyle := tsStandard;
  TranparentColor := clSilver;
  Animationcount := 0;
  case FTrashStyle of
    tsStandard:FPicture.bitmap.handle := loadbitmap(hinstance,'T0112');
    tsEngine:FPicture.bitmap.handle := loadbitmap(hinstance,'T0212');
  end;
  hint := HintEmpty;
  screen.cursors[TrashMatch] := loadcursor(HInstance,'TrashMatch');
  screen.cursors[TrashForbid] := loadcursor(HInstance,'TrashForbid');
  screen.cursors[TrashTopHand] := loadcursor(HInstance,'TrashTopHand');
  screen.cursors[TrashHand] := loadcursor(HInstance,'TrashHand');
  cursor := TrashForbid;
end;

destructor TTrash.Destroy;
begin
  FPicture.Free;
  FTrashBmp.Free;
  inherited Destroy;
end;

procedure TTrash.paint;
begin
  TransparentBitmap(canvas.handle,0,0,FPicture.bitmap.handle,TranparentColor,0);
end;

procedure TTrash.FTimerTimer(Sender: TObject);
begin
  with inherited Canvas do
  begin
    inc(Animationcount);
    case FTrashStyle of
      tsStandard:
      begin
        case Animationcount of
          1  : Fpicture.bitmap.handle := loadbitmap(hinstance,'T0101');
          2  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0102');
          3  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0103');
          4  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0104');
          5  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0105');
          6  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0106');
          7  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0107');
          8  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0108');
          9  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0109');
          10 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0110');
          11 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0111');
          12 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0112');
          13 : begin
                 FPicture.bitmap.handle := loadbitmap(hinstance,'T0113');
                 FTimer.enabled := false;
                 Animationcount := 0;
                 if Assigned(FAsDelete) then FAsDelete(Self);
               end;
        end;
      end;
      tsEngine:
      begin
        case Animationcount of
          1  : Fpicture.bitmap.handle := loadbitmap(hinstance,'T0201');
          2  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0202');
          3  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0203');
          4  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0204');
          5  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0205');
          6  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0206');
          7  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0207');
          8  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0208');
          9  : FPicture.bitmap.handle := loadbitmap(hinstance,'T0209');
          10 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0210');
          11 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0211');
          12 : FPicture.bitmap.handle := loadbitmap(hinstance,'T0212');
          13 : begin
                 FPicture.bitmap.handle := loadbitmap(hinstance,'T0213');
                 FTimer.enabled := false;
                 Animationcount := 0;
                 if Assigned(FAsDelete) then FAsDelete(Self);
               end;
        end;
      end;
    end;
  end;
  Refresh;
end;

procedure TTrash.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AllowDelete: Boolean;
begin
  if full then
  begin
    if ((button=mbleft) and (progress=0)) then
    begin
      progress := 1;
      PlayWaveFile(FWarningSoundFilename,1);
    end else
    if ((button=mbleft) and (progress=1)) then
    begin
      progress := 0;
      PlayWaveFile(FCancelSoundFilename,1);
    end else
    if ((button=mbright) and (progress=1)) then
    begin
      progress := 0;
      delete := true;
      AllowDelete := True;
      if Assigned(FOnDelete) then FOnDelete(Self,AllowDelete);
      if AllowDelete then
      begin
        Ftimer.enabled := true;
        PlayWaveFile(FDestroySoundFilename,1);
        if ((FDataSource<>nil) and FDataSource.DataSet.active) then FDataSource.DataSet.delete;
      end else
      begin
        progress := 0;
        delete := false;
        PlayWaveFile(FCancelSoundFilename,1);
      end;
    end;
    if progress=0 then
    begin
      SetHint(HintFull);
      cursor := TrashTopHand;
    end;
    if progress=1 then
    begin
      SetHint(HintConfimation);
      case FTrashStyle of
        tsStandard:cursor := TrashMatch;
        tsEngine:cursor := TrashHand;
      end;
    end;
  end;
end;

procedure TTrash.SetFull(value : boolean);
begin
  FFull := value;
  if FFull then
  begin
    case FTrashStyle of
      tsStandard:
      begin
        FPicture.bitmap.handle := loadbitmap(hinstance,'T0113');
        cursor := TrashTopHand;
      end;
      tsEngine:
      begin
        FPicture.bitmap.handle := loadbitmap(hinstance,'T0213');
        cursor := TrashTopHand;
      end;
    end;
    SetHint(HintFull);
  end else
  begin
    case FTrashStyle of
      tsStandard:
      begin
        FPicture.bitmap.handle := loadbitmap(hinstance,'T0112');
        cursor := TrashForbid;
      end;
      tsEngine:
      begin
        FPicture.bitmap.handle := loadbitmap(hinstance,'T0212');
        cursor := TrashForbid;
      end;
    end;
    SetHint(HintEmpty);
  end;
  Refresh;
end;

procedure TTrash.SetAnimationSpeed(value : integer);
begin
  FAnimationSpeed := value;
  FTimer.interval := FAnimationSpeed;
end;

procedure TTrash.SetLanguage(value : TLanguage);
begin
  if value=laDefault then FLanguage := GetCurrentLanguage else FLanguage := value;
  case FLanguage of
    laEnglish : begin
                  HintEmpty := EnglishEmpty;
                  HintFull := EnglishFull;
                  HintConfimation := EnglishConfimation;
                end;
    laGerman  : begin
                  HintEmpty := GermanEmpty;
                  HintFull := GermanFull;
                  HintConfimation := GermanConfimation;
                end;
    laFrench  : begin
                  HintEmpty := FrenchEmpty;
                  HintFull := FrenchFull;
                  HintConfimation := FrenchConfimation;
                end;
    laItalian : begin
                  HintEmpty := ItalianEmpty;
                  HintFull := ItalianFull;
                  HintConfimation := ItalianConfimation;
                end;
    laSpanish : begin
                  HintEmpty := SpanishEmpty;
                  HintFull := SpanishFull;
                  HintConfimation := SpanishConfimation;
                end;
  end;
  if full then SetHint(HintFull) else SetHint(HintEmpty);
end;

procedure TTrash.SetDataSource(value : TDataSource);
begin
  FDataSource := Value;
end;

procedure TTrash.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataSource) then FDataSource := nil;
end;

procedure TTrash.SetHint(Text: string);
begin
  if Text<>Hint then
  begin
    hint := Text;
    Application.hintpause := Application.hintpause;
  end;
end;

procedure TTrash.SetTrashStyle(value : TTrashStyle);
var
  Dir : string;
begin
  getdir(0,Dir);
  FTrashStyle := value;
  case FTrashStyle of
    tsStandard:
    begin
      TranparentColor := clSilver;
      FWarningSoundFilename := lowercase(Dir)+'\match.wav';
      FDestroySoundFilename := lowercase(Dir)+'\fire.wav';
      FCancelSoundFilename := lowercase(Dir)+'\cancel.wav';
    end;
    tsEngine:
    begin
      TranparentColor := clWhite; 
      FWarningSoundFilename := lowercase(Dir)+'\active.wav';
      FDestroySoundFilename := lowercase(Dir)+'\press.wav';
      FCancelSoundFilename := lowercase(Dir)+'\cancel.wav';
    end;
  end;
  SetFull(Full);
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TPlayWave                                                      }
{-----------------------------------------------------------------------------}
constructor TPlayWave.create(aowner : tcomponent);
begin
  inherited Create(AOwner); 
end;

destructor TPlayWave.Destroy;
begin
  inherited Destroy;
end;  

procedure TPlayWave.play;
var
  AllowPlay : Boolean;
  flag : word;
begin
  if Assigned(FOnPlay) then
  begin
    AllowPlay := True;
    FOnPlay(Self,AllowPlay);
    if not AllowPlay then Exit;
  end;
  case FWaveMode of
    waCanStop : flag := 1;
    waNoStop : flag := 0;
    waLoop : flag := 9;
  end;
  PlayWaveFile(FFilename,flag);
end;

procedure TPlayWave.stop;
begin
  SndPlaySound(nil,0);
end;


{-----------------------------------------------------------------------------}
{  Component : THintDesign                                                    }
{-----------------------------------------------------------------------------}
constructor THintDesign.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FHintStyle := hiBubble;
  FLinkStyle := liArrow;
  FPosition := hiTopRight;
  FPicture := TPicture.Create;
  FFont:=TFont.Create;
  FFont.Name:='Arial';
  FFont.Size:=10;
  FFont.Color:=clBlack;
  FFont.Style:=[];
  FColor:=clwhite;
  FBorder := true;
  FBorderColor := clBlack;
  FShadow := true;
  FShadowQuality := quLow;
  FShadowIntensity := 70;
  FShadowWidth := 4;
  FDelay := 500;
  Application.HintPause:=FDelay;
  Reset;
end;

destructor THintDesign.Destroy;
begin
   FPicture.free;
   FFont.free;
   inherited Destroy;
end;

procedure THintDesign.Reset;
var
  i : integer;
begin
  if not (csDesigning in ComponentState) then
  begin
    Application.ShowHint:=not Application.ShowHint;
    for i := 0 to 10 do Application.processmessages;
    Application.ShowHint:=not Application.ShowHint;
    for i := 0 to 10 do Application.processmessages;
    for i := 0 to Application.ComponentCount-1 do
    if Application.Components[I] is TNewDesign then
    begin
      TNewDesign(Application.Components[i]).Canvas.Font.Assign(FFont);
      Exit;
    end;
  end;
end;

procedure THintDesign.Loaded;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
    HintWindowClass:=TNewDesign;
  Reset;
end;

procedure THintDesign.CMFontChanged(var Message:TMessage);
begin
  inherited;
  Reset;
end;

procedure THintDesign.SetFont(Value:TFont);
begin
  FFont.Assign(Value);
  Reset;
end;

procedure THintDesign.SetDelay(Value:Integer);
begin
  FDelay := Value;
  Application.HintPause := Value;
end;

procedure THintDesign.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure THintDesign.SetShadowIntensity(Value: integer);
begin
  if ((value>=0) and (value<=255)) then FShadowIntensity := Value;
end;

procedure THintDesign.SetShadowWidth(Value: integer);
begin
  if ((value>=0) and (value<=10)) then FShadowWidth := Value;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TNewDesign                                                     }
{-----------------------------------------------------------------------------}
constructor TNewDesign.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  brush.style := bsClear;
  ctl3d := false;
  borderwidth := 0;
end;

destructor TNewDesign.Destroy;
begin
  inherited Destroy;
end;

procedure TNewDesign.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := WS_POPUP;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TNewDesign.WMNCPaint(var Message: TMessage);
begin
end;

procedure TNewDesign.Paint;
var
 CanvasTmp : TBitmap;
 HintFont : TFontData;
begin
  CanvasTmp := TBitmap.Create;
  CanvasTmp.Width := Width;
  CanvasTmp.height := height;
  BitBlt(CanvasTmp.Canvas.Handle,0,0,Width,height,CreateDC('DISPLAY',nil,nil,nil),left,top,SRCCOPY);
  CanvasTmp.Canvas.Font.Assign(FHintDesign.Font);
  with HintFont do
  begin
    Color := FHintDesign.Font.Color;
    size := FHintDesign.Font.size;
    Style:= FHintDesign.Font.Style;
    Name:= FHintDesign.Font.Name;
  end;
  DrawHint(CanvasTmp.canvas.handle,
            HintFont,
            left,
            top,
            Width,
            Height,
            RealTextHeight,
            FHintDesign.Color,
            Caption,
            FPosition,
            FHintDesign.HintStyle,
            FLinkStyle,
            LinkSize,
            FHintDesign.FShadow,
            FHintDesign.FShadowQuality,
            FHintDesign.ShadowWidth,
            FHintDesign.ShadowIntensity,
            FHintDesign.Picture.bitmap.handle,
            FHintDesign.Border,
            FHintDesign.BorderColor);
  with inherited Canvas do
  begin
    CopyMode:=cmSrcCopy;
    CopyRect(ClientRect,CanvasTmp.Canvas,ClientRect);
  end;
  CanvasTmp.Free;
end;

function TNewDesign.CheckLeft : boolean;
begin
  if Left-Width>0 then result := true else result := false;
end;

function TNewDesign.CheckRight : boolean;
begin
  if Left+Width<Screen.Width then result := true else result := false;
end;

function TNewDesign.CheckTop : boolean;
begin
  if Top-Height-10>0 then result := true else result := false;
end;

function TNewDesign.CheckBottom : boolean;
begin
  if Top+Height+10<Screen.Height then result := true else result := false;
end;

procedure TNewDesign.IsTopRightValid;
begin
  if not CheckRight and CheckLeft then
  begin
    IsTopLeftValid;
    exit;
  end;
  if  not CheckTop and CheckBottom then
  begin
    IsBottomRightValid;
    exit;
  end;
  if not CheckRight and not CheckLeft then Left := Screen.Width-Width;
  dec(Top,Height+10);
  FPosition := hiTopRight;
end;

procedure TNewDesign.IsBottomRightValid;
begin
  if  not CheckRight and CheckLeft then
  begin
    IsBottomLeftValid;
    exit;
  end;
  if  not CheckBottom and CheckTop then
  begin
    IsTopRightValid;
    exit;
  end;
  if not CheckRight and not CheckLeft then Left := Screen.Width-Width;
  inc(Top,10);
  FPosition := hiBottomRight;
end;

procedure TNewDesign.IsTopLeftValid;
begin
  if  not CheckLeft and CheckRight then
  begin
    IsTopRightValid;
    exit;
  end;
  if  not CheckTop and CheckBottom then
  begin
    IsBottomLeftValid;
    exit;
  end;
  if not CheckRight and not CheckLeft then Left := 0 else dec(Left,Width);
  dec(Top,Height+10);
  FPosition := hiTopLeft;
end;

procedure TNewDesign.IsBottomLeftValid;
begin
  if  not CheckLeft and CheckRight then
  begin
    IsBottomRightValid;
    exit;
  end;
  if  not CheckBottom and CheckTop then
  begin
    IsTopLeftValid;
    exit;
  end;
  if not CheckRight and not CheckLeft then Left := 0 else dec(Left,Width);
  inc(Top,10);
  FPosition := hiBottomLeft;
end;

function TNewDesign.Search : THintDesign;
var
  i:Integer;
begin
  Result:=nil;
  for I:=0 to Application.MainForm.ComponentCount-1 do
  if Application.MainForm.Components[I] is THintDesign then
  begin
    Result:=THintDesign(Application.MainForm.Components[I]);
    Exit;
  end;
end;

procedure TNewDesign.ActivateHint(Rect: TRect; const AHint: string);
var
  MousePos : TPoint;
  i : integer;
begin
  SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE);
  for i := 0 to 10 do application.processmessages;
  brush.style := bsClear;
  ctl3d := false;
  borderwidth := 0;
  GetCursorPos(MousePos);
  Caption := AHint;
  FHintDesign := Search;
  FShadow := FHintDesign.Shadow;
  FPosition := FHintDesign.Position;
  FLinkStyle := FHintDesign.LinkStyle;
  Left := MousePos.x;
  Top := MousePos.y;
  Width := Rect.Right-Rect.Left;
  Height := Rect.Bottom-Rect.Top;
  RealTextHeight := Rect.Bottom-Rect.Top;
  case FHintDesign.HintStyle of
    hiBubble:
    begin
      Width := trunc(Width+Height);
      Height := trunc(Height*2);
      if trunc(Width/Height)>4 then Height := trunc(Width/4);
    end;
    hiImage:
    begin
      Width := FHintDesign.Picture.width;
      Height := FHintDesign.Picture.height;
      if FHintDesign.Border then
      begin
        inc(Width,2);
        inc(Height,2);
      end;
      FLinkStyle := liNone;
    end;
    hiTexture:
    begin
      FLinkStyle := liNone;
    end;
    hiText:
    begin
      FLinkStyle := liNone;
      FShadow := false;
    end;
  end;
  if FLinkStyle<>liNone then
  begin
    if FHintDesign.LinkStyle=liArrow then LinkSize := 15 else LinkSize := 20;
    if width<35 then width := 35;
    inc(Height,LinkSize);
  end else
  begin
    LinkSize := 10;
    inc(Height,LinkSize);
  end;
  if FShadow then
  begin
    inc(Width,FHintDesign.ShadowWidth);
    inc(Height,FHintDesign.ShadowWidth);
  end;
  case FPosition of
    hiTopRight : IsTopRightValid;
    hiBottomRight : IsBottomRightValid;
    hiTopLeft : IsTopLeftValid;
    hiBottomLeft : IsBottomLeftValid;
  end;
  brush.style := bsClear;
  ctl3d := false;
  borderwidth := 0;
  SetWindowPos(Handle, HWND_TOPMOST, Left, Top, Width, height, SWP_SHOWWINDOW or SWP_NOACTIVATE);
end;


{-----------------------------------------------------------------------------}
{  Component : TMagicLabel                                                    }
{-----------------------------------------------------------------------------}
constructor TMagicLabel.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  FCaption := '';
  FBlinkTransparent := true;
  FBlinkColor := clRed;
  FBlinkTopColor := clwhite;
  FBlinkBottomColor := clgray;
  FRaisedTop := true;
  FRaisedBottom := true;
  FRaisedTopColor := clwhite;
  FRaisedBottomColor := clgray;
  FRaisedTopDistance := 1;
  FRaisedBottomDistance := 1;
  FShowAccelChar := True;
  FTimer := ttimer.create(self);
  FTimer.interval := 50;
  FTimer.enabled := false;
  FTimer.ontimer := timertimer;
  Font.Color := clsilver;
  BlinkState := false;
  AutoSize := false;
  width := 95;
  height := 20;
end;

destructor TMagicLabel.Destroy;
begin
  inherited Destroy;
end;

procedure TMagicLabel.TimerTimer(Sender: TObject);
begin
  if BlinkState then BlinkState := false else BlinkState := true;
  Refresh;
end;

procedure TMagicLabel.DoDrawText(var Rect: TRect; colortext:TColor; Flags: Word);
var
  Text: array[0..255] of Char;
begin 
  StrPCopy(Text,FCaption);
  if (Flags and DT_CALCRECT <> 0) and ((Text[0] = #0) or ShowAccelChar and
    (Text[0] = '&') and (Text[1] = #0)) then StrCopy(Text, ' ');
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  Canvas.Font.color := colortext;
  if not Enabled then Canvas.Font.Color := clGrayText;
  DrawText(Canvas.Handle, Text, StrLen(Text), Rect, Flags);
end;

procedure TMagicLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  space : integer;
begin
  if (not (csReading in ComponentState) and AutoSize and (align=alNone) and (FCaption<>'')) then
  begin
    space := 0;
    if FRaisedTop then space := FRaisedTopDistance;
    if FRaisedBottom then space := space+FRaisedBottomDistance;
    height := canvas.textheight(FCaption)+space;
    width := canvas.textwidth(FCaption)+space;
  end;
end;

procedure TMagicLabel.paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  TopRect,BottomRect,Rect: TRect;
  TextColor,TopColor,BottomColor : TColor;
  TopDistance,BottomDistance : integer;
begin
  if (visible or (csDesigning in ComponentState)) then
  begin
    if BlinkState then
    begin
      if BlinkTransparent then FCaption := '';
      TextColor := BlinkColor;
      TopColor := BlinkTopColor;
      BottomColor := BlinkBottomColor;
    end else
    begin
      if Caption<>'' then FCaption := Caption;
      TextColor := self.font.Color;
      TopColor := RaisedTopColor;
      BottomColor := RaisedBottomColor;
    end;
    with Canvas do
    begin
      if not Transparent then
      begin
        Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        FillRect(ClientRect);
      end;
      Font := self.font;
      Brush.Style := bsClear;
      AdjustBounds;
      Rect := ClientRect;
      if FRaisedBottom then BottomDistance := FRaisedBottomDistance else BottomDistance := 0;
      if FRaisedTop then TopDistance := FRaisedTopDistance else TopDistance := 0;
      if RaisedBottom then
      begin
        BottomRect := Rect;
        BottomRect.Left   := Rect.Left + TopDistance + BottomDistance;
        BottomRect.Top    := Rect.Top + TopDistance + BottomDistance;
        BottomRect.Right  := Width;
        BottomRect.Bottom := Height;
        DoDrawText(BottomRect, BottomColor, (DT_EXPANDTABS or DT_WORDBREAK) or Alignments[Alignment]);
      end;
      if RaisedTop then
      begin
        TopRect := Rect;
        TopRect.Left   := Rect.Left;
        TopRect.Top    := Rect.Top;
        TopRect.Right  := Width - TopDistance - BottomDistance;
        TopRect.Bottom := Height - TopDistance  - BottomDistance;
        DoDrawText(TopRect, TopColor, (DT_EXPANDTABS or DT_WORDBREAK) or Alignments[Alignment]);
      end;
      inc(Rect.Left,TopDistance);
      inc(Rect.Top,TopDistance);
      Rect.Right := Width-BottomDistance;
      Rect.Bottom := Height-BottomDistance;
      DoDrawText(Rect, TextColor, (DT_EXPANDTABS or DT_WORDBREAK) or Alignments[Alignment]);
    end;
  end;
end;

procedure TMagicLabel.SetRaisedTop(Value: boolean);
begin
  FRaisedTop := Value;
  Refresh;
end;

procedure TMagicLabel.SetRaisedBottom(Value: boolean);
begin
  FRaisedBottom := Value;
  Refresh;
end;

procedure TMagicLabel.SetRaisedTopColor(Value: TColor);
begin
  FRaisedTopColor := Value;
  Refresh;
end;

procedure TMagicLabel.SetRaisedBottomColor(Value: TColor);
begin
  FRaisedBottomColor := Value;
  Refresh;
end;

procedure TMagicLabel.SetRaisedTopDistance(Value: integer);
begin
  FRaisedTopDistance := Value;
  Refresh;
end;

procedure TMagicLabel.SetRaisedBottomDistance(Value: integer);
begin
  FRaisedBottomDistance := Value;
  Refresh;
end;

procedure TMagicLabel.SetBlink(value: boolean);
begin
  FTimer.enabled := value;
  BlinkState := false;
  Refresh;
end;

procedure TMagicLabel.SetBlinkSpeed(Value: Word);
begin
  FTimer.interval := value;
end;

function TMagicLabel.GetBlink : boolean;
begin
  result := FTimer.enabled;
  BlinkState := false;
end;

function TMagicLabel.GetBlinkSpeed : Word;
begin
  result := FTimer.interval;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TMiniCalendar                                                  }
{-----------------------------------------------------------------------------}
constructor TMiniCalendar.create(aowner : tcomponent);
begin
  inherited create(aowner);
  height := 43;
  width := 29;
  FPicture := TPicture.Create;
  FDayFontName := 'Arial';
  FDayFontColor := clred;
  FMonthFontName := 'Arial';
  FMonthFontColor := clblack;
  FLanguage := laEnglish;
end;

destructor TMiniCalendar.Destroy;
begin
  FPicture.Free;
  inherited Destroy;
end;

procedure TMiniCalendar.paint;
begin
  FPicture.bitmap.handle := loadbitmap(hinstance,'MiniCalendarBackGround_');
  with inherited Canvas do
  begin
    settextalign(handle,ta_center);
    TransparentBitmap(handle,0,0,FPicture.bitmap.handle,clSilver,0);
    font.color := DayFontColor;
    font.name := DayFontName;
    font.size := trunc(FPicture.height*0.38);
    font.style := [fsbold];
    textout(trunc(FPicture.width/2),trunc(FPicture.height*0.16),GetDate(daDay,FLanguage));
    font.color := MonthFontColor;
    font.name := MonthFontName;
    font.size := trunc(font.size/2);
    font.style := [];
    textout(trunc(FPicture.width/2),FPicture.height-font.size*2,copy(GetDate(daMonth,FLanguage),1,3));
  end;
end;

procedure TMiniCalendar.SetDayFontName(value : TFontName);
begin
  FDayFontName := Value;
  paint;
end;

procedure TMiniCalendar.SetDayFontColor(value : TColor);
begin
  FDayFontColor := Value;
  paint;
end;

procedure TMiniCalendar.SetMonthFontName(value : TFontName);
begin
  FMonthFontName := Value;
  paint;
end;

procedure TMiniCalendar.SetMonthFontColor(value : TColor);
begin
  FMonthFontColor := Value;
  paint;
end;

procedure TMiniCalendar.SetLanguage(value : TLanguage);
begin
  FLanguage := value;
  paint;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TPerpetualCalendar                                             }
{-----------------------------------------------------------------------------}
constructor TPerpetualCalendar.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  FLanguage := laEnglish;
  LanguageBuff := FLanguage;
  FTitle := 'Calendar';
  FDate := GetDate(daShort,FLanguage);
  FPosition := poScreenCenter;
  FLeftPos := 100;
  FTopPos := 100;
end;

destructor TPerpetualCalendar.Destroy;
begin
  inherited Destroy;
end;

function TPerpetualCalendar.Execute: Boolean;
var
  PerpetualCalendarDlg : TPerpetualCalendarDlg;
begin
  PerpetualCalendarDlg := TPerpetualCalendarDlg.create(application);
  PerpetualCalendarDlg.Caption := FTitle;
  PerpetualCalendarDlg.Language := Language;
  PerpetualCalendarDlg.position := position;
  PerpetualCalendarDlg.left := LeftPos;
  PerpetualCalendarDlg.top := TopPos;
  if FForceToday then date := GetDate(daShort,FLanguage);
  PerpetualCalendarDlg.date := Date;
  if PerpetualCalendarDlg.showmodal=mrok then
  begin
    date := PerpetualCalendarDlg.date;
    result := true;
  end else result := false;
  PerpetualCalendarDlg.free;
end;

procedure TPerpetualCalendar.SetFDate(value : string);
var
  AllowChange: Boolean;
begin
  if Assigned(FOnChange) then
  begin
    AllowChange := True;
    FOnChange(Self, Value, AllowChange);
    if not AllowChange then Exit;
  end;
  FDate := value;
end;

procedure TPerpetualCalendar.SetForceToday(value : boolean);
begin
  FForceToday := value;
  if value then date := GetDate(daShort,FLanguage);
end;

procedure TPerpetualCalendar.SetLanguage(value : TLanguage);
begin
  FLanguage := value;
  LanguageBuff := value;
  case value of
    laEnglish:FTitle := 'Calendar';
    laGerman:FTitle := 'Kalender';
    laFrench:FTitle := 'Calendrier';
    laItalian:FTitle := 'Calendario';
    laSpanish:FTitle := 'Calendario';
  end;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TCalendarPad                                                   }
{-----------------------------------------------------------------------------}
constructor TCalendarPad.create(aowner : tcomponent);
var
  i : integer;
  Dir : string;
begin
  inherited Create(AOwner);
  ShortDateFormat := 'dd/mm/yyyy';
  DateSeparator := '/';
  ControlStyle := ControlStyle+[csOpaque];
  getdir(0,Dir);
  height := 278;
  width := 387;
  Font.Color := clBlack;
  Font.size := 8;
  Font.Name := 'Arial';
  Font.Style := [];
  FTimer := ttimer.create(self);
  FTimer.ontimer := pulse;
  FTimer.interval := 60000;
  FTimer.enabled := true;
  FBackground := TPicture.Create;
  FBackground.bitmap.handle := loadbitmap(hinstance,'CalendarPad_');
  Note := TMemo.create(self);
  with Note do
  begin
    parent := self;
    Left := 18;
    Top := 85;
    Width := 149;
    Height := 160;
    BorderStyle := bsNone;
    Color := clWhite;
    Ctl3D := False;
    Font := self.font;
    MaxLength := 32000;
    ParentCtl3D := False;
    ParentFont := true;
    TabOrder := 0;
    ParentShowHint := false;
  end;
  for i := 1 to 17 do
  begin
    Timefield[i] := TEdit.create(self);
    with Timefield[i] do
    begin
      parent := self;
      Left := 246;
      Top := 20+((i-1)*14);
      Width := 115;
      Height := 16;
      BorderStyle := bsNone;
      Ctl3D := False;
      Font.Color := clBlack;
      Font.size := 7;
      Font.Name := 'Arial';
      Font.Style := [];
      MaxLength := 255;
      ParentCtl3D := False;
      ParentFont := False;
      TabOrder := i;
      Text := '';
      ParentShowHint := false;
    end;
  end;
  FLines1 := TStringlist.Create;
  FLines2 := TStringlist.Create;
  FYearColor := clBlack;
  FMonthColor := clBlack;
  FDateColor := clWhite;
  FDayColor := clWhite;
  FDateBackgroundColor := clRed;
  FDayWeekColor := clBlack;
  FTimeColor := clBlack;
  FTimeFieldColor := clBlack;
  FCurrentTimeColor := clRed;
  FLanguage := laDefault;
  FDate := GetDate(daShort,FLanguage);
  FStartTime := 8;
  FDirectory := lowercase(Dir);
  FSoundEffect := true;
  FSoundFileName := lowercase(Dir)+'\paper.wav';;
  FExtensionFile := '000';
  FDestroyDelay := 10;
end;

destructor TCalendarPad.Destroy;
begin
  FBackground.Free;
  inherited Destroy;
end;

procedure TCalendarPad.paint;
var
  Dest: TRect;
  i,ii : integer;
procedure line(x1,y1,x2,y2 : integer);
begin
  Canvas.moveto(x1,y1);
  Canvas.lineto(x2,y2);
end;
begin
  inherited paint;
  RefreshDate;
  RefreshLanguage;
  if ((FBackground.bitmap.Width<=0) and (FBackground.bitmap.Height<=0)) then
  FBackground.bitmap.handle := loadbitmap(hinstance,'CalendarPad_');
  Dest := Rect(0, 0, FBackground.Graphic.Width, FBackground.Graphic.Height);
  SetBounds(left, top, 387, 278);
  with inherited Canvas do
  begin
    StretchDraw(Dest, FBackground.Graphic);
    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Pen.Width := 1;
    Brush.Style := bsSolid;
    Brush.Color := FDateBackgroundColor;
    Rectangle(100, 18, 164, 76);
    settextalign(handle,ta_center);
    font.Color := FDateColor;
    font.style := [fsBold];
    font.name := 'Arial';
    font.size := 28;
    textout(133,20,StrDay);
    font.Color := FDayColor;
    font.style := [];
    font.size := 8;
    textout(133,60,StrDayOfWeek);
    font.Color := FMonthColor;
    font.style := [fsBold];
    font.size := 11;
    Brush.Color := clWhite;
    textout(60,55,StrMonth);
    font.Color := FYearColor;
    font.size := 20;
    textout(60,20,StrYear);
    font.Color := FDayWeekColor;
    font.style := [];
    font.size := 8;
    settextalign(handle,ta_right);
    textout(165,250,FStrDay+' '+inttostr(DayCount(FDate))+' - '+FStrWeek+' '+inttostr(WeekCount(FDate)));
    font.size := 8;
    settextalign(handle,ta_left);
    ii := FStartTime;
    for i := 1 to 17 do
    begin
      if copy(timetostr(time),1,2)=inttostrform(ii,2) then
      begin
        font.Color := FCurrentTimeColor;
        Timefield[i].Font.Color := FCurrentTimeColor;
      end else
      begin
        font.Color := FTimeColor;
        Timefield[i].Font.Color := FTimeFieldColor;
      end;
      if ((Language=laEnglish) and (ii>12)) then ii := ii-12 else
      if ii>23 then ii := 0;
      textout(218,19+((i-1)*14),inttostrform(ii,2)+':00');
      inc(ii);
    end;
    Pen.Color := clBlack;
    line(20,80,165,80);
    line(20,250,165,250);
  end;
  LoadDay;
end;

procedure TCalendarPad.pulse(Sender: TObject);
var
  i,ii : integer;
begin
  with inherited Canvas do
  begin
    font.name := 'Arial';
    font.style := [];
    font.size := 8;
    settextalign(handle,ta_left);
    ii := FStartTime;
    for i := 1 to 17 do
    begin
      if copy(timetostr(time),1,2)=inttostrform(ii,2) then
      begin
        font.Color := FCurrentTimeColor;
        Timefield[i].Font.Color := FCurrentTimeColor;
      end else
      begin
        font.Color := FTimeColor;
        Timefield[i].Font.Color := FTimeFieldColor;
      end;
      if ((Language=laEnglish) and (ii>12)) then ii := ii-12 else
      if ii>23 then ii := 0;
      textout(218,19+((i-1)*14),inttostrform(ii,2)+':00');
      inc(ii);
    end;
  end;
end;

procedure TCalendarPad.DblClick;
var
  PerpetualCalendarDlg : TPerpetualCalendarDlg;
  point : TPoint;
begin
  if go then
  begin
    SaveDay;
    PerpetualCalendarDlg := TPerpetualCalendarDlg.create(application);
    PerpetualCalendarDlg.Language := Language;
    GetCursorPos(Point);
    PerpetualCalendarDlg.left := point.x;
    PerpetualCalendarDlg.top := point.y;
    PerpetualCalendarDlg.position := poDesigned;
    PerpetualCalendarDlg.date := FDate;
    PerpetualCalendarDlg.showmodal;
    date := PerpetualCalendarDlg.date;
    PerpetualCalendarDlg.free;
    LoadDay;
    note.setfocus;
  end;
end;

procedure TCalendarPad.Mouseup(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  date_ : string;
begin
  date_ := FDate;
  if previous then
  begin
    SaveDay; 
    date := datedec(Fdate,1);
    if FSoundEffect then PlayWaveFile(FSoundFileName,1);
  end else
  if next then
  begin
    SaveDay;
    date := dateinc(Fdate,1);
    if FSoundEffect then PlayWaveFile(FSoundFileName,1);
  end;
end;

procedure TCalendarPad.MouseMove(Shift: TShiftState; X,Y: Integer);
begin
  FHint := Hint;
  Previous := false;
  Next := false;
  go := false;
  if Mousein(X,Y,Rect(1,249,36,269)) then
  begin
    SetHint(FStrPrior);
    cursor := crlefthand;
    Previous := true;
  end else
  if Mousein(X,Y,Rect(349,249,384,269)) then
  begin
    SetHint(FStrNext);
    cursor := crrighthand;
    Next := true;
  end else
  if Mousein(X,Y,Rect(105,20,165,75)) then
  begin
    SetHint(FStrGo);
    cursor := crtophand;
    go := true;
  end else
  begin
    SetHint('');
    cursor := crDefault;
  end;
  inherited MouseMove(Shift,X,Y);
end;

procedure TCalendarPad.SetDate(value : string);
var
  AllowChange: Boolean;
begin
  if Assigned(FOnChange) then
  begin
    AllowChange := True;
    FOnChange(Self, Value, AllowChange);
    if not AllowChange then Exit;
  end;
  FDate := value;
  refresh;
end;

procedure TCalendarPad.SetReadOnly(Value: boolean);    
var
  i : integer;
begin
  FReadOnly := value;
  Note.ReadOnly := value;
  for i := 1 to 17 do Timefield[i].ReadOnly := value;
end;

function TCalendarPad.GetTextBuf(Buffer: PChar; BufSize: Integer): Integer;
begin
  result := Note.GetTextBuf(Buffer,BufSize);
end;

function TCalendarPad.GetTextLen: Integer;
begin
  result := Note.GetTextLen;
end;

procedure TCalendarPad.SetTextBuf(Buffer: PChar);
begin
  Note.SetTextBuf(Buffer);
end;

procedure TCalendarPad.SetBackground(Value: TPicture);
begin
  FBackground.Assign(Value);
  refresh;
end;

procedure TCalendarPad.LoadFromFile(const FileName: string);
begin
  FLines1.LoadFromFile(FileName);
  Note.lines := FLines1;
end;

procedure TCalendarPad.SaveToFile(const FileName: string);
begin
  FLines1.assign(Note.lines);
  FLines1.SaveToFile(FileName);
end;

procedure TCalendarPad.clear;
var
  i : integer;
begin
  FLines1.clear;
  FLines2.clear;
  Note.lines := FLines1;
  for i := 1 to 17 do Timefield[i].text := '';
end;

procedure TCalendarPad.SetYearColor(Value: TColor);
begin
  FYearColor := Value;
  refresh;
end;

procedure TCalendarPad.SetMonthColor(Value: TColor);
begin
  FMonthColor := Value;
  refresh;
end;

procedure TCalendarPad.SetDateColor(Value: TColor);
begin
  FDateColor := Value;
  refresh;
end;

procedure TCalendarPad.SetDayColor(Value: TColor);
begin
  FDayColor := Value;
  refresh;
end;

procedure TCalendarPad.SetDateBackgroundColor(Value: TColor);
begin
  FDateBackgroundColor := Value;
  refresh;
end;

procedure TCalendarPad.SetDayWeekColor(Value: TColor);
begin
  FDayWeekColor := Value;
  refresh;
end;

procedure TCalendarPad.SetTimeColor(Value: TColor);
begin
  FTimeColor := Value;
  refresh;
end;

procedure TCalendarPad.SetTimeFieldColor(Value: TColor);
var
  i : integer;
begin
  FTimeFieldColor := Value;
  for i := 1 to 17 do Timefield[i].Font.Color := Value;
end;

procedure TCalendarPad.SetCurrentTimeColor(Value: TColor);
begin
  FCurrentTimeColor := Value;
  refresh;
end;

procedure TCalendarPad.SetStartTime(Value: integer);
begin
  if ((value>=0) and (value<=23)) then FStartTime := Value;
  refresh;
end;

procedure TCalendarPad.SetDestroyDelay(Value: integer);
begin
  if ((value>=1) and (value<=365)) then FDestroyDelay := Value;
  refresh;
end;

procedure TCalendarPad.SetLanguage(value : TLanguage);
begin
  FLanguage := Value;
  LanguageBuff := value;
  refresh;
end;

procedure TCalendarPad.SetExtensionFile(Value: string);
begin
  if length(value)<=3 then FExtensionFile := Value;
end;

procedure TCalendarPad.RefreshDate;
begin
  StrDayOfWeek := DayStr(DayOfWeek(strtodate(FDate)),FLanguage);
  StrDay := DateExtract(FDate,extDay);
  StrMonth := MonthStr(strtoint(DateExtract(FDate,extMonth)),FLanguage);
  StrYear := DateExtract(FDate,extYear);
end;

procedure TCalendarPad.LoadDay;
var
  i : integer;
begin                       
  Note.clear;
  if FileExists(FDirectory+'\'+formatdatetofile(Fdate)+'N.'+FExtensionFile) then
  loadfromfile(FDirectory+'\'+formatdatetofile(Fdate)+'N.'+FExtensionFile);
  FLines2.Clear;
  if FileExists(FDirectory+'\'+formatdatetofile(Fdate)+'T.'+FExtensionFile) then
  begin
    FLines2.LoadFromFile(FDirectory+'\'+formatdatetofile(Fdate)+'T.'+FExtensionFile);
    for i := 1 to 17 do Timefield[i].text := FLines2.strings[i-1];
  end else for i := 1 to 17 do Timefield[i].text := '';
end;

procedure TCalendarPad.SaveDay;
var
  i : integer;
begin
  savetofile(FDirectory+'\'+formatdatetofile(Fdate)+'N.'+FExtensionFile);
  FLines2.Clear;
  for i := 1 to 17 do FLines2.add(Timefield[i].text);
  FLines2.SaveToFile(FDirectory+'\'+formatdatetofile(Fdate)+'T.'+FExtensionFile);
end;

function TCalendarPad.formatdatetofile(value:string) : string;
begin
  result :=
  DateExtract(value,extDay)+
  DateExtract(value,extMonth)+
  copy(DateExtract(value,extYear),3,2);
end;

procedure TCalendarPad.DoEnter;
begin
  LoadDay;
end;

procedure TCalendarPad.DoExit;
begin
  SaveDay;
end;

procedure TCalendarPad.DeleteDays;
var
  date_ : string;
  ch : array[0..255] of char;
begin
  date_ := datedec(GetDate(daShort,FLanguage),365);
  repeat
    strpcopy(ch,FDirectory+'\'+formatdatetofile(date_)+'t.'+FExtensionFile);
    if FileExists(FDirectory+'\'+formatdatetofile(date_)+'t.'+FExtensionFile) then DeleteFile(ch);
    strpcopy(ch,FDirectory+'\'+formatdatetofile(date_)+'n.'+FExtensionFile);
    if FileExists(FDirectory+'\'+formatdatetofile(date_)+'n.'+FExtensionFile) then DeleteFile(ch);
    date_ := DateInc(date_,1);
  until date_=datedec(GetDate(daShort,FLanguage),FDestroyDelay);
end;

procedure TCalendarPad.RefreshLanguage;
var
  LanguageTmp : TLanguage;
begin
  if FLanguage=laDefault then LanguageTmp := GetCurrentLanguage else LanguageTmp := FLanguage;
  case LanguageTmp of
    laEnglish :
    begin
      FStrDay   := EnglishDay;
      FStrWeek  := EnglishWeek;
      FStrPrior := EnglishPrior;
      FStrNext  := EnglishNext;
      FStrGo    := EnglishGo;
    end;
    laGerman  :
    begin
      FStrDay   := GermanDay;
      FStrWeek  := GermanWeek;
      FStrPrior := GermanPrior;
      FStrNext  := GermanNext;
      FStrGo    := GermanGo;
    end;
    laFrench  :
    begin
      FStrDay   := FrenchDay;
      FStrWeek  := FrenchWeek;
      FStrPrior := FrenchPrior;
      FStrNext  := FrenchNext;
      FStrGo    := FrenchGo;
    end;
    laItalian :
    begin
      FStrDay   := ItalianDay;
      FStrWeek  := ItalianWeek;
      FStrPrior := ItalianPrior;
      FStrNext  := ItalianNext;
      FStrGo    := ItalianGo;
    end;
    laSpanish :
    begin
      FStrDay   := SpanishDay;
      FStrWeek  := SpanishWeek;
      FStrPrior := SpanishPrior;
      FStrNext  := SpanishNext;
      FStrGo    := SpanishGo;
    end;
  end;
end;

procedure TCalendarPad.SetHint(Text: string);
begin
  if Text<>Hint then
  begin
    hint := Text;
    Application.hintpause := Application.hintpause;
  end;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TDigitalClock                                                  }
{-----------------------------------------------------------------------------}
constructor TDigitalClock.create(aowner : tcomponent);
begin
  inherited create(aowner);
  FDelayRefresh := 1000;
  FSecond := true;
  FTimer := ttimer.create(self);
  FTimer.ontimer := pulse;
  FTimer.interval := FDelayRefresh;
  FTimer.enabled := true;
  Caption := timetostr(time);
end;

destructor TDigitalClock.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end; 

procedure TDigitalClock.pulse(Sender: TObject);
begin
  if Fsecond then Text := timetostr(time) else Text := copy(timetostr(time),1,5);
end;

procedure TDigitalClock.SetDelayRefresh(Value: longint);
begin
  FDelayRefresh := value;
  FTimer.interval := FDelayRefresh;
  pulse(self);
end;

procedure TDigitalClock.SetSecond(Value: boolean);
begin
  FSecond := value;
  pulse(self);
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TAnalogicClock                                                 }
{-----------------------------------------------------------------------------}
constructor TAnalogicClock.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  height := 180;
  width := 180;
  FPicture := TPicture.Create;
  FDelayRefresh := 1000;
  FTimer := ttimer.create(self);
  FTimer.ontimer := pulse;
  FTimer.interval := FDelayRefresh;
  FTimer.enabled := true;
  FSecond := true;
  FMinuteSize := 70;
  FSecondSize := 70;
  FHourSize := 50;
  FMinuteWidth := 1;
  FSecondWidth := 1;
  FHourWidth := 1;
  FMinuteColor := clBlack;
  FSecondColor := clBlack;
  FHourColor := clBlack;
  FMark := maNumber;
  FMarkColor := clBlack;
  FMarkWidth := 3;
  FMarkDistance := 80;
  FTransparentPicture := false;
  FTransparentColor := clWhite;
  CanvasMem := TBitmap.Create;
  CanvasMem.assign(nil);
end;

destructor TAnalogicClock.Destroy;
begin
  FPicture.Free;
  FTimer.Free;
  CanvasMem.free;
  inherited Destroy;
end;

procedure TAnalogicClock.paint;
var
  Dest : TRect;
  i, ii, x, y, e : integer;
  Hour, Minute, Second : real;
function Radians(W: Double): Double; export;
begin
  Radians := Abs(Round(W) mod 360)*Pi/180.0;
end;  
procedure ClockWise(Angle,Size,Width : INTEGER; Color : TColor);
var
  x,y : integer;
begin
  angle := angle+270;
  canvas.pen.Width := Width;
  canvas.pen.Color := Color;
  x := trunc(self.width/2)+Round(Size*Cos(radians(angle)));
  y := trunc(self.height/2)+Round(Size*Sin(radians(angle)));
  canvas.moveto(trunc(self.width/2),trunc(self.height/2));
  canvas.lineto(x,y);
  x := trunc(self.width/2)+Round(trunc(Size/10)*Cos(radians(angle-180)));
  y := trunc(self.height/2)+Round(trunc(Size/10)*Sin(radians(angle-180)));
  canvas.moveto(trunc(self.width/2),trunc(self.height/2));
  canvas.lineto(x,y);
end;
procedure ShowMark(angle,size,width : INTEGER; color : Tcolor);
var
  x,y : integer;
  Text: array[0..255] of Char;
begin
  strpcopy(text,inttostr(trunc(angle/30)));
  angle := angle+270;
  x := trunc(self.width/2)+Round(size*Cos(radians(angle)));
  y := trunc(self.height/2)+Round(size*Sin(radians(angle)));
  with inherited Canvas do
  begin
    brush.color := color;
    brush.style := bssolid;
    pen.color := color;
    pen.style := pssolid;
    pen.width := 1;
    setbkmode(Handle,1);
    case FMark of
      maNumber :
      begin
        Dest := Rect(x-trunc(font.size/1),y-trunc(font.size/1),x+trunc(font.size/1),y+trunc(font.size/1));
        DrawText(Handle,text, StrLen(text), Dest,DT_EXPANDTABS or DT_WORDBREAK or DT_CENTER);
      end;
      maPoint  :
      begin
        Pie(x-width,y-width,x+width,y+width,0,0,0,0);
      end;
      maSquare :
      begin
        Rectangle(x-width,y-width,x+width,y+width);
      end;
    end;
  end;
end;
begin
  canvas.font := font;
  val(copy(timetostr(time),1,2),Hour,e);
  val(copy(timetostr(time),4,2),Minute,e);
  val(copy(timetostr(time),7,2),Second,e);
  if Picture.bitmap.width<>0 then with inherited Canvas do
  begin
    ControlStyle := ControlStyle + [csOpaque];
    Dest := Rect(0, 0, Picture.Width, Picture.Height);
    SetBounds(Left, Top, Picture.Width, Picture.Height);
    if FTransparentPicture then TransparentBitmap(Canvas.handle,Dest.left,Dest.Top,Picture.bitmap.handle,FTransparentColor,0)
    else Canvas.StretchDraw(Dest, Picture.Graphic);
  end else ControlStyle := ControlStyle - [csOpaque];
  if Fsecond then ClockWise(trunc(Second)*6,FSecondSize,FSecondWidth,FSecondColor);
  ClockWise(trunc(Minute)*6,FMinuteSize,FMinuteWidth,FMinuteColor);
  ClockWise(trunc(Hour)*30+trunc(Minute*6/12),FHourSize,FHourWidth,FHourColor);
  if FMark<>maNone then for i := 1 to 12 do ShowMark(i*30,FMarkDistance,FMarkWidth,FMarkColor);
end;

procedure TAnalogicClock.pulse(Sender: TObject);
begin
  refresh;
end;

procedure TAnalogicClock.SetPicture(Value: TPicture);
begin
  FPicture.Assign(Value);
  refresh;
end;

procedure TAnalogicClock.SetDelayRefresh(Value: longint);
begin
  FDelayRefresh := value;
  FTimer.interval := FDelayRefresh;
  refresh;
end;

procedure TAnalogicClock.SetSecond(Value: boolean);
begin
  FSecond := value;
  refresh;
end;

procedure TAnalogicClock.SetMinuteSize(Value: integer);
begin
  FMinuteSize := Value;
  refresh;
end;

procedure TAnalogicClock.SetSecondSize(Value: integer);
begin
  FSecondSize := Value;
  refresh;
end;

procedure TAnalogicClock.SetHourSize(Value: integer);
begin
  FHourSize := Value;
  refresh;
end;

procedure TAnalogicClock.SetMinuteWidth(Value: integer);
begin
  FMinuteWidth := Value;
  refresh;
end;

procedure TAnalogicClock.SetSecondWidth(Value: integer);
begin
  FSecondWidth := Value;
  refresh;
end;

procedure TAnalogicClock.SetHourWidth(Value: integer);
begin
  FHourWidth := Value;
  refresh;
end;

procedure TAnalogicClock.SetMinuteColor(Value: TColor);
begin
  FMinuteColor := Value;
  refresh;
end;

procedure TAnalogicClock.SetSecondColor(Value: TColor);
begin
  FSecondColor := Value;
  refresh;
end;

procedure TAnalogicClock.SetHourColor(Value: TColor);
begin
  FHourColor := Value;
  refresh;
end;

procedure TAnalogicClock.SetMark(Value: TClockMark);
begin
  FMark := Value;
  refresh;
end;

procedure TAnalogicClock.SetMarkColor(Value: TColor);
begin
  FMarkColor := Value;
  refresh;
end;

procedure TAnalogicClock.SetMarkWidth(Value: integer);
begin
  FMarkWidth := Value;
  refresh;
end;

procedure TAnalogicClock.SetMarkDistance(Value: integer);
begin
  FMarkDistance := Value;
  refresh;
end;

procedure TAnalogicClock.SetTransparentPicture(Value: boolean);
begin
  FTransparentPicture := value;
  refresh;
end;

procedure TAnalogicClock.SetTransparentColor(Value: TColor);
begin
  FTransparentColor := value;
  refresh;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  TAlarmData                                                              }
{-----------------------------------------------------------------------------}
constructor TAlarmData.Create;
begin
  inherited Create;
end;

destructor TAlarmData.Destroy;
begin
  inherited Destroy;
end;
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------}
{  Component : TAlarm                                                         }
{-----------------------------------------------------------------------------}
constructor TAlarm.create(aowner : tcomponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.create(self);
  FTimer.ontimer := pulse;
  FTimer.interval := 60000;
  FTimer.enabled := true;
  FAlarmSet := TAlarmData.Create;
  FAlarmSet.Time := strtotime('00:00');
  FAlarmSet.Date := Date;//GetDate(daShort,GetCurrentLanguage);
  FAlarmSet.Text := '';
  FAlarmSet.SoundFileName := '';
  FAlarmSet.RepeatAlarm := true;
  FAlarmSet.Frequency := 0;
end;

destructor TAlarm.Destroy;
begin
  inherited Destroy;
end;       

procedure TAlarm.pulse(Sender: TObject);
var
  PlaySound : Boolean;
begin
  if time=FAlarmSet.Time then
  begin
    if ((FAlarmSet.RepeatAlarm and ((FAlarmSet.Frequency=0) or (FAlarmSet.Frequency=DayOfWeek(date)))) or
        (not FAlarmSet.RepeatAlarm and (date=FAlarmSet.Date))) then
    begin
      if Assigned(FOnAlarm) then
      begin
        PlaySound := True;
        FOnAlarm(Self, FAlarmSet.Text, PlaySound);
        if not PlaySound then Exit;
      end;
      if ((FAlarmSet.SoundFileName<>'') and FileExists(FAlarmSet.SoundFileName)) then PlayWaveFile(FAlarmSet.SoundFileName,1);
    end;
  end;
end;

function TAlarm.GetEnabled : Boolean;
begin
  result := FTimer.enabled;
end;

procedure TAlarm.SetEnabled(Value: Boolean);
begin
  FTimer.enabled := value;
end;
{-----------------------------------------------------------------------------}
end.
