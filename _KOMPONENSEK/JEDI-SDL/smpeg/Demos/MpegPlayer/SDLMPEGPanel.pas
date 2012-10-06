unit SDLMPEGPanel;

interface

{$IFDEF LINUX}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  SysUtils,
  Classes,
{$IFDEF VCL}
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
{$ENDIF}
{$IFDEF CLX}
  Types,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QExtCtrls,
{$ENDIF}
  SDL,
  smpeg;

type
  TSDLMPEGPanel = class( TPanel )
  private
    { Private declarations }
    FSurface : PSDL_Surface;
    FSMPEGHandle : PSMPEG;
    FMPEGFile : TFileName;
    FMPEGInfo : TSMPEG_Info;
    FPlaying : Boolean;
    FSound : Integer;
    function GetSound : Boolean;
    procedure SetSound( const Value : Boolean );
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Play;
    procedure Stop;
    procedure Pause;
  published
    { Published declarations }
    property MPEGFile : TFileName read FMPEGFile write FMPEGFile;
    property Sound : Boolean read GetSound write SetSound;
  end;

procedure Register;

implementation

{$IFDEF CLX}
uses
  Libc,
  Qt;
{$ENDIF}

procedure TSDLMPEGPanel.Play;
var
  EnvVal : string;
begin
  if FPlaying then
    Exit;
  FPlaying := True;

  if SDL_getenv( 'SDL_WINDOWID' ) = '' then
  begin
    {$IFDEF VCL}
    SDL_putenv( 'SDL_VIDEODRIVER=windib' );
    EnvVal := 'SDL_WINDOWID=' + inttostr( Integer( Handle ) );
    {$ENDIF}

    {$IFDEF CLX}
    EnvVal := 'SDL_WINDOWID=' + inttostr( QWidget_WinId( ChildHandle ) );
    {$ENDIF}

    SDL_putenv( PChar( EnvVal ) );
  end;

  if SDL_getenv( 'SDL_VIDEO_YUV_HWACCEL' ) = '' then
    SDL_putenv( 'SDL_VIDEO_YUV_HWACCEL=0' );

  if SDL_Init( SDL_INIT_VIDEO or SDL_INIT_NOPARACHUTE ) < 0 then
    raise Exception.Create( 'SDL_Init failed' );
  {$IFDEF VCL}
  FSurface := SDL_SetVideoMode( Width, Height, 16{QPixmap_depth( Bitmap.Handle )}, SDL_SWSURFACE );
  {$ENDIF}
  {$IFDEF CLX}
  FSurface := SDL_SetVideoMode( Width, Height, QPixmap_depth( Bitmap.Handle ), SDL_SWSURFACE );
  {$ENDIF}
  if FSurface = nil then
    raise Exception.Create( 'SDL_SetVideoMode failed' );

  FSMPEGHandle := SMPEG_new( PChar( FMPEGFile ), @FMPEGInfo, FSound );
  if FSMPEGHandle = nil then
    raise Exception.Create( 'Cannot create MPEG stream' );

  if FSound = 0 then
    SMPEG_enableaudio( FSMPEGHandle, 0 );

  SMPEG_scaleXY( FSMPEGHandle, Width, Height );
  SMPEG_setdisplay( FSMPEGHandle, FSurface, nil, nil );

  SMPEG_play( FSMPEGHandle );
end;

procedure TSDLMPEGPanel.Stop;
begin
  if not FPlaying then
    Exit;
  FPlaying := False;
  SMPEG_stop( FSMPEGHandle );
  SMPEG_delete( FSMPEGHandle );
  SDL_Quit;
end;

procedure TSDLMPEGPanel.Pause;
begin
  if FPlaying then
    SMPEG_pause( FSMPEGHandle );
end;

function TSDLMPEGPanel.GetSound : Boolean;
begin
  Result := Boolean( FSound );
end;

procedure TSDLMPEGPanel.SetSound( const Value : Boolean );
begin
  FSound := Integer( Value );
end;

procedure Register;
begin
  RegisterComponents( 'SDL', [ TSDLMPEGPanel ] );
end;

end.


