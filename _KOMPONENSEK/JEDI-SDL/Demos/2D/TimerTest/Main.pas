unit Main;

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

interface

uses
  SysUtils,
  Classes,
{$IFDEF VCL}
  Forms,
  StdCtrls,
  Controls,
{$ENDIF}
{$IFDEF CLX}
  QControls,
  QForms,
  QStdCtrls,
{$ENDIF}
  SDL, Logger;
  
type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Timer1: PSDL_TimerID;
  Timer2: PSDL_TimerID;
  Param1: Integer;
  Param2: Integer;
  retval: Integer;

function CallTimer1(interval: UInt32; param: Pointer): UInt32; cdecl;
function CallTimer2(interval: UInt32; param: Pointer): UInt32; cdecl;

implementation

{$IFDEF WIN32}
{$R *.dfm}
{$ENDIF}

{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

function CallTimer1(interval: UInt32; param: Pointer): UInt32; cdecl;
var
  pParam: PInteger;
begin
  pParam := PInteger(param);
  Form1.Memo1.Lines.Add('Parameter = ' + IntToStr(pParam^) + ' in CallTimer1');
  Result := interval;
end;

function CallTimer2(interval: UInt32; param: Pointer): UInt32; cdecl;
var
  pParam: PInteger;
begin
  pParam := PInteger(param);
  Form1.Memo1.Lines.Add('Parameter = ' + IntToStr(pParam^) + ' in CallTimer2');
  Result := interval;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  retval := SDL_Init(SDL_INIT_TIMER);
  if retval <> 0 then
    log.LogError('Cannot initalize SDL', 'Form1.FormShow');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Button1.Caption = 'Start' then
  begin
    Param1 := 12;
    Param2 := 15;
    Timer1 := SDL_AddTimer(500, CallTimer1, @Param1);
    Timer2 := SDL_AddTimer(1200, CallTimer2, @Param2);
    Button1.Caption := 'Stop';
  end
  else
  begin
    SDL_RemoveTimer(Timer1);
    SDL_RemoveTimer(Timer2);
    Button1.Caption := 'Start';
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SDL_Quit;
end;

end.
