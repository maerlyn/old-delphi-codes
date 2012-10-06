unit Main;

interface

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  {$IFDEF VCL}
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Buttons,
  ExtDlgs,
  Spin,
  {$ELSE}
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
  QComCtrls,
  QButtons,
  Types,
  {$ENDIF}
  SysUtils,
  SDL,
  SDL_Mixer;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button4: TButton;
    gbOptions: TGroupBox;
    cbLoops: TCheckBox;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    Memo: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog: TOpenDialog;
    Label1: TLabel;
    seAudioRate: TSpinEdit;
    seAudioBuffers: TSpinEdit;
    seAudioChannels: TSpinEdit;
    seAudioFormat: TSpinEdit;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    bAudioOpen: Boolean;
    wave: PMix_Chunk;
    procedure Initialize;
    procedure CleanUp;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF VCL}
{$R *.DFM}
{$ELSE}
{$R *.xfm}
{$ENDIF}

procedure TForm1.Initialize;
var
  audio_rate: integer;
  audio_format: Uint16;
  audio_channels: integer;
  audio_buffers : integer;
  loops: integer;
begin
  loops := 0;
  if ((SDL_Init(SDL_INIT_AUDIO) < 0)) then
  begin
    {$IFDEF VCL}
    MessageBox(0, 'Could not initialize SDL Audio', 'ERROR', MB_OK );
    {$ELSE}
    Application.MessageBox( 'Could not initialize SDL Audio','ERROR', [smbOK]);
    {$ENDIF}
    SDL_Quit;
    exit;
  end
  else
  begin
    audio_rate := StrToInt(seAudioRate.Text);
    audio_format := StrToInt(seAudioFormat.Text);
    audio_channels := StrToInt(seAudioChannels.Text);
    audio_buffers := StrToInt(seAudioBuffers.Text);
    // Open the audio device
    if ( Mix_OpenAudio( audio_rate, audio_format, audio_channels, audio_buffers ) < 0) then
    begin
      Memo.Lines.Add(Format('Could not open audio: %s', [SDL_GetError]));
      exit;
    end
    else
    begin
      Mix_QuerySpec( audio_rate, audio_format, audio_channels);
      if (audio_channels > 1) then
        Memo.Lines.Add(Format('Opened audio at %d Hz %d bit %s', [audio_rate,
          (audio_format and $FF), 'stereo']))
      else
        Memo.Lines.Add(Format('Opened audio at %d Hz %d bit %s', [audio_rate,
          (audio_format and $FF), 'mono']));

      if (cbLoops.Checked) then
      begin
        Memo.Lines.Add('( looping )');
        loops := -1;
      end
      else
        Memo.Lines.Add('( not looping ) ');

      bAudioOpen := true;

      // Load the requested wave file * /
      wave := Mix_LoadWAV(PChar(String(Edit1.Text)));
      if (wave = nil) then
      begin
        Memo.Lines.Add(Format('Could not load %s: %s', [Edit1.Text, SDL_GetError()]));
        exit;
      end;

      // Play and then exit * /
      Mix_PlayChannel(0, wave, loops);
      while ( Mix_Playing(0) = 0 ) do
        SDL_Delay(100);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CleanUp;
  gbOptions.Enabled := True;
  Button4.Enabled := True;
  Button2.Enabled := False;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // set up variables
  try
    gbOptions.Enabled := False;
    Button4.Enabled := False;
    Button2.Enabled := True;
    Initialize;
  finally
  end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Edit1.Text := OpenDialog.FileName;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  seAudioRate.Value := MIX_DEFAULT_FREQUENCY;
  seAudioFormat.Value := MIX_DEFAULT_FORMAT;
  seAudioChannels.Value := MIX_DEFAULT_CHANNELS;
  seAudioBuffers.Value := 4096;
  wave := nil;
end;

procedure TForm1.CleanUp;
begin
  if ( wave <> nil ) then
  begin
    Mix_FreeChunk(wave);
    wave := nil;
  end;
  if ( bAudioOpen ) then
  begin
    Mix_CloseAudio;
    bAudioOpen := false;
  end;
  SDL_Quit;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CleanUp;
end;

end.
