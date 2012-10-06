unit DTT_Sound;

// Gestion du son
// --------------
//  Utilise FMod (http://www.fmod.org)

interface

uses
  fmod;

type
  TMusic = class
  private
    Module: PFMusicModule;
  public
    constructor Create(FileName: String);
    procedure Play;
    procedure Pause;
    procedure Stop;
    destructor Destroy; override;
  end;

  TSample = class
  private
    Sample: PFSoundSample;
  public
    constructor Create(FileName: String);
    procedure Play;
    procedure Pause( Channel : Integer = 0 );
    destructor Destroy; override;
  end;

  TSoundStream = class
  private
    Stream: PFSoundStream;
  public
    constructor Create(FileName: String);
    procedure Play;
    procedure Stop;
    destructor Destroy; override;
  end;

function SoundInit: Boolean;
procedure SoundClose;

implementation

const
  SOUND_DIR = 'sound/';

function SoundInit: Boolean;
begin
{$IFDEF LINUX}
  FSOUND_SetOutput(FSOUND_OUTPUT_OSS);
{$ELSE}
  FSOUND_SetOutput(FSOUND_OUTPUT_DSOUND);
{$ENDIF}
  FSOUND_SetDriver(0);
  FSOUND_SetMixer(FSOUND_MIXER_QUALITY_AUTODETECT);
  Result := FSOUND_Init(44100, 64, 0);
end;

constructor TMusic.Create(FileName: String);
begin
  inherited Create;
  Module := FMUSIC_LoadSong(PChar(SOUND_DIR + FileName));
  FMUSIC_SetPanSeperation(Module, 0.15);
  if Module = nil then
    Halt;
end;

procedure TMusic.Play;
begin
  FMUSIC_PlaySong(Module);
end;

procedure TMusic.Pause;
begin
  FMUSIC_SetPaused(Module, not FMUSIC_GetPaused(Module));
end;

procedure TMusic.Stop;
begin
  FMUSIC_StopSong(Module);
end;

destructor TMusic.Destroy;
begin
  FMUSIC_FreeSong(Module);
  inherited Destroy;
end;

constructor TSample.Create(FileName: String);
begin
  inherited Create;
  Sample := FSOUND_Sample_Load(FSOUND_FREE, PChar(SOUND_DIR + FileName),
                               FSOUND_2D, 0);
end;

procedure TSample.Play;
begin
  FSOUND_PlaySound(FSOUND_FREE, Sample);
end;

destructor TSample.Destroy;
begin
  FSOUND_Sample_Free(Sample);
  inherited Destroy;
end;

constructor TSoundStream.Create(FileName: String);
begin
  Stream := FSOUND_Stream_OpenFile(PChar(SOUND_DIR + FileName),
                                   FSOUND_LOOP_NORMAL or FSOUND_NORMAL, 0);
end;

procedure TSoundStream.Play;
begin
  FSOUND_Stream_Play(FSOUND_FREE, Stream);
end;

procedure TSoundStream.Stop;
begin
  FSOUND_Stream_Stop(Stream);
end;

destructor TSoundStream.Destroy;
begin
  FSOUND_Stream_Close(Stream);
  inherited Destroy;
end;

procedure SoundClose;
begin
  FSOUND_Close;
end;

procedure TSample.Pause( Channel : Integer );
begin
  FSOUND_SetPaused( Channel, not FSOUND_GetPaused( Channel ) );
end;

end.

