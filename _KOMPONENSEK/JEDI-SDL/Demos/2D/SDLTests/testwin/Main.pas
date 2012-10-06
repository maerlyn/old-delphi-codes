unit Main;

interface

{$IFDEF VER140}
{$DEFINE CLX}
{$ELSE}
{$DEFINE VCL}
{$ENDIF}

uses
  Classes,
{$IFDEF VCL}
  Windows,
  Messages,
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
{$ENDIF}
{$IFDEF CLX}
  QT,
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
  SDL;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button4: TButton;
    GroupBox1: TGroupBox;
    cbspeedy: TCheckBox;
    cbfade: TCheckBox;
    cbwarp: TCheckBox;
    cbhw: TCheckBox;
    cbflip: TCheckBox;
    cbfullscreen: TCheckBox;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    cbbenchmark: TCheckBox;
    Memo: TMemo;
    cbSCREENSHOT: TCheckBox;
    seDelay: TSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    seheight: TSpinEdit;
    Label4: TLabel;
    seBPP: TSpinEdit;
    Panel1: TPanel;
    seWidth: TSpinEdit;
    OpenDialog: TOpenDialog;
    cbToPanel: TCheckBox;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbToPanelClick(Sender: TObject);
  private
    { Private declarations }
    video_flags: DWord;
    screen_: PSDL_Surface;
    procedure DrawPict(screen_: PSDL_Surface; bmpfile: string);
    procedure Initialize;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IFDEF Win32}
{$R *.dfm}
{$ENDIF}

{$IFDEF Linux}
{$R *.xfm}
{$ENDIF}

procedure TForm1.Initialize;
var
  memory: string;
begin
  if ( SDL_Init( SDL_INIT_VIDEO ) < 0 ) then
  begin
    exit;
  end
  else
  begin
    // Set the window manager title bar
    SDL_WM_SetCaption('JEDI-SDL Test Window', nil);

    // Initialize the display
    if cbwarp.checked then
      video_flags := video_flags or SDL_HWPALETTE;
    if cbhw.checked then
      video_flags := video_flags or SDL_HWSURFACE;
    if cbflip.checked then
      video_flags := video_flags or SDL_DOUBLEBUF;
    if cbfullscreen.checked then
      video_flags := video_flags or SDL_FULLSCREEN;

    screen_ := SDL_SetVideoMode(seWidth.Value, seHeight.Value, seBPP.Value,
      video_flags);

    if (screen_ = nil) then
    begin
      //Application.MessageBox( 'Could not set video mode!', 'Error', MB_OK );
      exit;
    end
    else
    begin
      if ((screen_.flags and SDL_HWSURFACE) = 1) then
        memory := 'video'
      else
        memory := 'system';
      if ((screen_.flags and SDL_DOUBLEBUF) = 1) then
      begin
        //Application.MessageBox( 'Double-buffering enabled', 'Message', MB_OK );
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button4.Enabled := True;
  Button2.Enabled := False;
  SDL_QuitSubSystem( SDL_INIT_VIDEO );
end;

procedure TForm1.DrawPict(screen_: PSDL_Surface; bmpfile: string);
type
  TCDist = record
    r, g, b: Sint16;
  end;
var
  picture: PSDL_Surface;
  dest, update: TSDL_Rect;
  i, centered: integer;
  ncolors: integer;
  colors, cmap: array of TSDL_Color;
  //colors, cmap : PSDL_ColorArray;
  r, g, b: integer;
  black: Uint32;
  pixels: PUint8;
  displayfmt: PSDL_Surface;

  maxstep: integer;
  final: TSDL_Color;
  palcolors: array[0..255] of TSDL_Color;
  cdist: array[0..255] of TCdist;

  c: integer;
begin
  picture := nil; // to keep the compiler happy

  // Load the image into a surface
  if (bmpfile = '') then
    bmpfile := 'sample.bmp'; // Sample image

  try
    Memo.Lines.Add(Format('Loading picture : %s', [bmpfile]));
    picture := SDL_LoadBMP(PChar(bmpfile));
    if (picture = nil) then
    begin
      raise ERangeError.CreateFmt('Could not load %s: %s', [bmpfile,
        SDL_GetError]);
    end;

    // set the display colors - -on a hicolor display this is a no - op
    if (picture.format.palette <> nil) then
    begin
      ncolors := picture.format.palette.ncolors;
      SetLength(colors, ncolors * SizeOf(TSDL_Color));
      SetLength(cmap, ncolors * SizeOf(TSDL_Color));

      Move(picture.format.palette.colors^, colors[0], ncolors *
        sizeof(TSDL_Color));
    end
    else
    begin
      // Allocate 256 color palette
      ncolors := 256;
      SetLength(colors, ncolors * SizeOf(TSDL_Color));
      SetLength(cmap, ncolors * SizeOf(TSDL_Color));

      // Set a 3,3,2 color cube
      for r := 0 to 7 do
      begin
        for g := 0 to 7 do
        begin
          for b := 0 to 3 do
          begin
            i := ((r shl 5) or (g shl 2) or b);
            colors[i].r := r shl 5;
            colors[i].g := g shl 5;
            colors[i].b := b shl 6;
          end;
        end;
      end;

    end;

    Memo.Lines.Add('setting colors');
    if (SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, ncolors) = 0) and
      (screen_.format.palette <> nil) then
    begin
      Memo.Lines.Add('Warning: Could not set all of the colors, but SDL will map the image');
    end;

    // set the screen to black( not really necessary )
    if (SDL_LockSurface(screen_) = 0) then
    begin
      black := SDL_MapRGB(screen_.format, 0, 0, 0);
      pixels := PUint8(screen_.pixels);
      for i := 0 to screen_.h - 1 do
      begin
        FillChar(pixels^, screen_.w * screen_.format.BytesPerPixel, black);
        inc(pixels, screen_.pitch);
      end;
      SDL_UnlockSurface(screen_);
      SDL_UpdateRect(screen_, 0, 0, 0, 0);
    end;

    // Display the picture
    if (cbspeedy.Checked) then
    begin
      Memo.Lines.Add('Converting picture');
      displayfmt := SDL_DisplayFormat(picture);
      if (displayfmt = nil) then
      begin
        raise ERangeError.CreateFmt('Could not convert image: %s',
          [SDL_GetError]);
      end;

      SDL_FreeSurface(picture);
      picture := displayfmt;
    end;

    if (picture.flags and SDL_HWSURFACE) = 1 then
      Memo.Lines.Add('( image surface located in video memory )')
    else
      Memo.Lines.Add('( image surface located in system memory )');

    centered := (screen_.w - picture.w) div 2;
    if (centered < 0) then
    begin
      centered := 0;
    end;

    dest.y := (screen_.h - picture.h) div 2;
    dest.w := picture.w;
    dest.h := picture.h;
    Memo.Lines.Add('moving image');
    for i := 0 to centered do
    begin
      dest.x := i;
      update := dest;
      if (SDL_BlitSurface(picture, nil, screen_, @update) < 0) then
      begin
        Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
        break;
      end;

      if (cbflip.checked) then
      begin
        SDL_Flip(screen_);
      end
      else
      begin
        SDL_UpdateRects(screen_, 1, @update);
      end;
    end;

    if cbSCREENSHOT.Checked then
      if (SDL_SaveBMP(screen_, 'screen.bmp') < 0) then
        Memo.Lines.Add(Format('Could not save screen: %s', [SDL_GetError]));

    if not cbBenchmark.Checked then
      // Let it sit there for a while * /
      SDL_Delay(5 * 1000);

    // Fade the colormap * /
    if (cbfade.checked) then
    begin
      Memo.Lines.Add('fading out...');
      Move(colors[0], cmap[0], ncolors * sizeof(TSDL_Color));
      maxstep := 32 - 1;
      final.r := $FF;
      final.g := $00;
      final.b := $00;
      Move(colors[0], palcolors[0], ncolors * sizeof(TSDL_Color));
      for i := 0 to ncolors - 1 do
      begin
        cdist[i].r := final.r - palcolors[i].r;
        cdist[i].g := final.g - palcolors[i].g;
        cdist[i].b := final.b - palcolors[i].b;
      end;

      for i := 0 to (maxstep div 2) do // halfway fade */
      begin
        for c := 0 to ncolors - 1 do
        begin
          colors[c].r := palcolors[c].r + ((cdist[c].r * i)) div maxstep;
          colors[c].g := palcolors[c].g + ((cdist[c].g * i)) div maxstep;
          colors[c].b := palcolors[c].b + ((cdist[c].b * i)) div maxstep;
        end;

        SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, ncolors);

        SDL_Delay(1);
      end;

      final.r := $00;
      final.g := $00;
      final.b := $00;
      Move(colors[0], palcolors[0], ncolors * sizeof(TSDL_Color));
      for i := 0 to ncolors - 1 do
      begin
        cdist[i].r := final.r - palcolors[i].r;
        cdist[i].g := final.g - palcolors[i].g;
        cdist[i].b := final.b - palcolors[i].b;
      end;

      maxstep := maxstep div 2;
      for i := 0 to maxstep do // finish fade out */
      begin
        for c := 0 to ncolors - 1 do
        begin
          colors[c].r := palcolors[c].r + ((cdist[c].r * i)) div maxstep;
          colors[c].g := palcolors[c].g + ((cdist[c].g * i)) div maxstep;
          colors[c].b := palcolors[c].b + ((cdist[c].b * i)) div maxstep;
        end;
        SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, ncolors);
        SDL_Delay(1);
      end;

      for i := 0 to ncolors - 1 do
      begin
        colors[i].r := final.r;
        colors[i].g := final.g;
        colors[i].b := final.b;
      end;

      SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, ncolors);
      Memo.Lines.Add('fading in ...');
      Move(colors[0], palcolors[0], ncolors * sizeof(TSDL_Color));
      for i := 0 to ncolors - 1 do
      begin
        cdist[i].r := cmap[i].r - palcolors[i].r;
        cdist[i].g := cmap[i].g - palcolors[i].g;
        cdist[i].b := cmap[i].b - palcolors[i].b;
      end;

      for i := 0 to maxstep do // 32 step fade in */
      begin
        for c := 0 to ncolors - 1 do
        begin
          colors[c].r := palcolors[c].r + ((cdist[c].r * i)) div maxstep;
          colors[c].g := palcolors[c].g + ((cdist[c].g * i)) div maxstep;
          colors[c].b := palcolors[c].b + ((cdist[c].b * i)) div maxstep;
        end;
        SDL_SetColors(screen_, PSDL_Color(@colors[0]), 0, ncolors);
        SDL_Delay(1);
      end;
      Memo.Lines.Add('fading over');
    end;
  finally
    begin
      // Free the picture and return
      SDL_FreeSurface(picture);
      SetLength(colors, 0);
      SetLength(cmap, 0);
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  then_, now_: Cardinal;
  EnvVal : string;
begin
  // set up variables
  try
    Button4.Enabled := False;
    Button2.Enabled := True;

    if cbToPanel.Checked then
    begin
      {$IFDEF VCL}
      SDL_putenv( 'SDL_VIDEODRIVER=windib' );
      EnvVal := 'SDL_WINDOWID=' + inttostr(Integer(Panel1.Handle));
      {$ENDIF}

      {$IFDEF CLX}
      EnvVal := 'SDL_WINDOWID=' + inttostr(QWidget_WinId(Panel1.Handle));
      {$ENDIF}

      SDL_putenv( PChar( EnvVal ) );
    end;

    Initialize;

    if cbbenchmark.checked then
    begin
      then_ := SDL_GetTicks;
      DrawPict(screen_, Edit1.Text);
      now_ := SDL_GetTicks;
      Memo.Lines.Add(Format('Benchmark Time: %d milliseconds', [now_ - then_]));
    end
    else
    begin
      DrawPict(screen_, Edit1.Text);
      SDL_Delay(seDelay.Value * 1000);
    end;
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
  Edit1.Text := ExtractFilePath(Application.ExeName) + 'sample.bmp';
end;

procedure TForm1.cbToPanelClick(Sender: TObject);
begin
  SeWidth.Value := Panel1.Width;
  SeHeight.Value := Panel1.Height;
end;

end.

