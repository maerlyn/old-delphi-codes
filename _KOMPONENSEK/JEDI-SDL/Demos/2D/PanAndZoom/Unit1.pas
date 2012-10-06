unit Unit1;

interface

{$IFDEF VER140}
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
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Forms,
  Dialogs,
  {$ENDIF}
  {$IFDEF CLX}
  Types,
  //Variant,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QExtCtrls,
  QStdCtrls,
  QT,
  QComCtrls,
  QButtons,
  {$ENDIF}
  SDL;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    cbhw: TCheckBox;
    Memo: TMemo;
    Edit1: TEdit;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBar2Change(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
  private
    { Private-Deklarationen }
    video_flags: LongWord;
    screen_: PSDL_Surface;
    procedure Initialize;
    procedure DrawPict(screen_: PSDL_Surface; bmpfile: string);
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  picture, Raster, zoom0, zoom: PSDL_Surface;
  upd, src: TSDL_Rect;
  gray, black, key: Uint32;
  xxx: integer;

implementation

{$IFDEF VCL}
{$R *.dfm}
{$ENDIF}
{$IFDEF CLX}
{$R *.xfm}
{$ENDIF}

procedure TForm1.Initialize;
var
  memory: string;
begin
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    exit;
  end
  else
  begin
    // Set the window manager title bar
    SDL_WM_SetCaption('JEDI-SDL Pan and Zoom Demo', nil);

    // Initialize the display
    if cbhw.checked then
      video_flags := video_flags or SDL_HWSURFACE;
    screen_ := SDL_SetVideoMode(Panel1.Width, Panel1.Height, 16,
      video_flags);
    if (screen_ = nil) then
    begin
      //Application.MessageBox( 'Could not set video mode!', 'Error', MB_OK );
      exit;
    end
    else
    begin
      Edit1.Enabled := true;
      TrackBar1.Enabled := true;
      TrackBar2.Enabled := true;
      TrackBar3.Enabled := true;
      TrackBar4.Enabled := true;
      Memo.Enabled := true;
      cbhw.Enabled := true;
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

procedure TForm1.DrawPict(screen_: PSDL_Surface; bmpfile: string);
type
  TCDist = record
    r, g, b: Sint16;
  end;
var
  dest: TSDL_Rect;
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

    // set the screen to gray( not really necessary )
    if (SDL_LockSurface(screen_) = 0) then
    begin
      gray := SDL_MapRGB(screen_.format, 127, 127, 127);
      dest.x := 0;
      dest.y := 0;
      dest.w := screen_.w;
      dest.h := screen_.h;
      SDL_FillRect(screen_, @dest, gray);

      SDL_UnlockSurface(screen_);
      SDL_UpdateRect(screen_, 0, 0, 0, 0);
    end;

    // Display the picture
    if (picture.flags and SDL_HWSURFACE) = 1 then
      Memo.Lines.Add('( image surface located in video memory )')
    else
      Memo.Lines.Add('( image surface located in system memory )');

    Memo.Lines.Add(inttostr(picture.format.BytesPerPixel) + ' Bytes Per Pixel');

    if (SDL_BlitSurface(picture, nil, zoom0, @dest) < 0) then
    begin
      Memo.Lines.Add(Format('Blit picture -> zoom failed: %s', [SDL_GetError]));
    end;

    src.x := 0;
    src.y := 0;
    src.w := picture.w;
    src.h := picture.h;

    if (SDL_BlitSurface(zoom0, @src, zoom, @dest) < 0) then
    begin
      Memo.Lines.Add(Format('Blit picture -> zoom failed: %s', [SDL_GetError]));
    end;

    TrackBar1.Max := screen_.w - picture.w;
    TrackBar1.Position := TrackBar1.Max div 2;
    dest.x := TrackBar1.Position;
    TrackBar2.Max := screen_.h - picture.h;
    TrackBar2.Position := TrackBar2.Max div 2;
    dest.y := TrackBar2.Position;
    dest.w := picture.w;
    dest.h := picture.h;
    Memo.Lines.Add('displaying image');

    Application.ProcessMessages;

    upd := dest;

    if (SDL_BlitSurface(zoom, @src, screen_, @upd) < 0) then
    begin
      Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
    end;

    dest.x := 0;
    dest.y := 0;
    if (SDL_BlitSurface(raster, nil, screen_, @dest) < 0) then
    begin
      Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
    end;

    SDL_UpdateRects(screen_, 1, @dest);

  finally
    begin
      // Free the picture and return
    end;
  end;

end;

function inizoom(screen_: PSDL_Surface): PSDL_Surface;
var
  dest: TSDL_Rect;
begin

  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;

  Result :=
    SDL_CreateRGBSurface(0, screen_.w, screen_.h, 16,
    screen_.format.RMask, screen_.format.GMask, screen_.format.BMask,
    screen_.format.AMask);

end;

function iniraster(screen_: PSDL_Surface): PSDL_Surface;
var
  dest: TSDL_Rect;
  x, y: integer;
begin

  Result :=
    SDL_CreateRGBSurface(SDL_SRCCOLORKEY, screen_.w, screen_.h, 16,
    screen_.format.RMask, screen_.format.GMask, screen_.format.BMask,
    screen_.format.AMask);

  key := SDL_MapRGB(screen_.format, 127, 63, 32);
  black := SDL_MapRGB(screen_.format, 0, 0, 0);
  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;

  SDL_SetColorKey(Result, SDL_SRCCOLORKEY, key);

  SDL_FillRect(Result, @dest, key);
  dest.w := 1;
  for x := 0 to (screen_.w - 1) div 10 do
  begin
    dest.x := x * 10;
    SDL_FillRect(Result, @dest, black);
  end;
  dest.w := screen_.w;
  dest.h := 1;
  dest.x := 0;
  for y := 0 to (screen_.h - 1) div 10 do
  begin
    dest.y := y * 10;
    SDL_FillRect(Result, @dest, black);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  EnvVal: string;
begin
  try
    Button1.Enabled := False;
    begin
      {$IFDEF WIN32}
      SDL_putenv('SDL_VIDEODRIVER=windib');
      {$ENDIF}

      {$IFDEF VCL}
      EnvVal := 'SDL_WINDOWID=' + inttostr(Integer(Panel1.Handle));
      {$ENDIF}

      {$IFDEF CLX}
      EnvVal := 'SDL_WINDOWID=' + inttostr(QWidget_WinId(Panel1.Handle));
      {$ENDIF}

      SDL_putenv(PChar(EnvVal));
    end;

    Initialize;

    raster := iniraster(screen_);
    zoom0 := inizoom(screen_);
    zoom := inizoom(screen_);

    Memo.Lines.Add('Zoom ' + IntToStr(zoom.format.BytesPerPixel) +
      ' Bytes per Pixel');

    DrawPict(screen_, Edit1.Text);
  finally
    Button1.Enabled := True;

  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Edit1.Text := ExtractFilePath(Application.ExeName) + 'sample.bmp';
  Edit1.Enabled := false;
  TrackBar1.Enabled := false;
  TrackBar2.Enabled := false;
  TrackBar3.Enabled := false;
  TrackBar4.Enabled := false;
  Memo.Enabled := false;
  cbhw.Enabled := false;
  xxx := 0;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SDL_FreeSurface(picture);
  SDL_QuitSubSystem( SDL_INIT_VIDEO );
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var
  dest: TSDL_Rect;
begin
  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;
  SDL_FillRect(screen_, @dest, gray);

  upd.x := TrackBar1.Position;
  if (SDL_BlitSurface(zoom, @src, screen_, @upd) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  if (SDL_BlitSurface(raster, nil, screen_, @dest) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  SDL_UpdateRects(screen_, 1, @dest);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
var
  dest: TSDL_Rect;
begin
  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;
  SDL_FillRect(screen_, @dest, gray);

  upd.y := TrackBar2.Position;
  if (SDL_BlitSurface(zoom, @src, screen_, @upd) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  if (SDL_BlitSurface(raster, nil, screen_, @dest) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  SDL_UpdateRects(screen_, 1, @dest);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
var
  x, y, xmax, ymax, i, ii, f, ys: integer;
  ps, pd: ^Word;
  dest: TSDL_Rect;
begin
  i := TrackBar4.Position + 10;

  src.x := 0;
  src.y := 0;
  src.w := (picture.w * i) div 10;
  src.h := (picture.h * i) div 10;
  if src.w > screen_.w then
    src.w := screen_.w;
  if src.h > screen_.h then
    src.h := screen_.h;
  f := 100 div i;

  ymax := src.h;
  xmax := src.w;
  ys := 0;
  ys := 0;
  for y := 0 to ymax - 1 do
  begin
    ys := (y * f) div 10;
    ii := Uint32(zoom0.pixels);
    inc(ii, ys * zoom0.pitch);
    i := Uint32(zoom.pixels);
    inc(i, y * zoom.pitch);
    pd := pointer(i);
    for x := 0 to xmax - 1 do
    begin
      i := (x * f) div 10;
      i := ii + 2 * i;
      ps := pointer(i);
      pd^ := ps^;
      inc(pd);
    end;
  end;

  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;
  SDL_FillRect(screen_, @dest, gray);

  if (SDL_BlitSurface(zoom, @src, screen_, @upd) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  if (SDL_BlitSurface(raster, nil, screen_, @dest) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  SDL_UpdateRects(screen_, 1, @dest);
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
type
  TByteArray = array[0..2] of Byte;
  PByteArray = ^TByteArray;
var
  bpp, x, y, i: integer;
  ps, pd: ^Word;
  dest: TSDL_Rect;

begin
  bpp := zoom.format.BytesPerPixel;
    // Here p is the address to the pixel we want to set
  ps := Pointer(Uint32(picture.pixels));
  pd := Pointer(Uint32(zoom.pixels));

  xxx := TrackBar3.Position * ((1 shl zoom.format.Rshift) + (1 shl
    zoom.format.Gshift) + (1 shl zoom.format.Bshift));

  for y := 0 to picture.h - 1 do
  begin
    i := Uint32(zoom0.pixels);
    inc(i, y * zoom0.pitch);
    ps := pointer(i);
    i := Uint32(zoom.pixels);
    inc(i, y * zoom.pitch);
    pd := pointer(i);
    for x := 0 to picture.w - 1 do
    begin
      pd^ := ps^ + xxx;
      inc(pd);
      inc(ps);
    end
  end;

  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;

  if (SDL_BlitSurface(zoom, @src, screen_, @upd) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  if (SDL_BlitSurface(raster, nil, screen_, @dest) < 0) then
  begin
    Memo.Lines.Add(Format('Blit failed: %s', [SDL_GetError]));
  end;

  SDL_UpdateRects(screen_, 1, @dest);

end;

procedure TForm1.FormPaint(Sender: TObject);
var
  dest: TSDL_Rect;
begin
  if not assigned(screen_) then
    exit;
  dest.x := 0;
  dest.y := 0;
  dest.w := screen_.w;
  dest.h := screen_.h;
  SDL_UpdateRects(screen_, 1, @dest);
end;

end.

