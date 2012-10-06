{: GLScreen<p>

   Routines to interact with the screen/desktop.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>08/02/00 - Egg - TLowResMode & TVideoMode packed (wins 5 kb)
      <li>06/02/00 - Egg - Javadocisation, added "default"s to properties
   </ul></font>
}
unit GLScreen;

// GLScreen    - This units contains routines to interact with the screen/desktop.
// Version     - 0.0.8
// Last Change - 30. September 1998
// for more information see help file

interface

uses Windows, Classes, Geometry, Graphics;

const
   MaxVideoModes = 200;

type

   TResolution = 0..MaxVideoModes;

   // window attributes
   TWindowAttribute  = (woDesktop, woStayOnTop, woTransparent);
   TWindowAttributes = set of TWindowAttribute;

   // window-to-screen fitting
   TWindowFitting = (wfDefault, wfFitWindowToScreen, wfFitScreenToWindow);

   // TDisplayOptions
   //
   TDisplayOptions = class(TPersistent)
      private
         FFullScreen       : Boolean;
         FScreenResolution : TResolution;
         FWindowAttributes : TWindowAttributes;
         FWindowFitting    : TWindowFitting;
      public
         procedure Assign(Source: TPersistent); override;
      published
         property FullScreen: Boolean read FFullScreen write FFullScreen default False;
         property ScreenResolution: TResolution read FScreenResolution write FScreenResolution default 0;
         property WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes default [];
         property WindowFitting: TWindowFitting read FWindowFitting write FWindowFitting default wfDefault;
   end;

     TVideoMode = packed record
                    Width : Word;
                    Height : Word;
                    ColorDepth  : Byte;
                    Description : String;
                  end;

function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;
function SetFullscreenMode(ModeIndex: Integer): Boolean;

procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer; SrcRect: TRectangle);
procedure RestoreDefaultMode;

var VideoModes        : array[0..MaxVideoModes] of TVideoMode;
    NumberVideomodes  : Integer = 1;
    CurrentVideoMode  : Integer = 0;

//------------------------------------------------------------------------------

implementation

uses Forms, GLScene, SysUtils;

type TLowResMode = packed record
                     Width : Word;
                     Height : Word;
                     ColorDepth : Byte;
                   end;

const NumberLowResModes = 60;
      LowResModes       : array[0..NumberLowResModes-1] of TLowResMode =
      ((Width:320;Height:200;ColorDepth: 8),(Width:320;Height:200;ColorDepth:15),(Width:320;Height:200;ColorDepth:16),
       (Width:320;Height:200;ColorDepth:24),(Width:320;Height:200;ColorDepth:32),
       (Width:320;Height:240;ColorDepth: 8),(Width:320;Height:240;ColorDepth:15),(Width:320;Height:240;ColorDepth:16),
       (Width:320;Height:240;ColorDepth:24),(Width:320;Height:240;ColorDepth:32),
       (Width:320;Height:350;ColorDepth: 8),(Width:320;Height:350;ColorDepth:15),(Width:320;Height:350;ColorDepth:16),
       (Width:320;Height:350;ColorDepth:24),(Width:320;Height:350;ColorDepth:32),
       (Width:320;Height:400;ColorDepth: 8),(Width:320;Height:400;ColorDepth:15),(Width:320;Height:400;ColorDepth:16),
       (Width:320;Height:400;ColorDepth:24),(Width:320;Height:400;ColorDepth:32),
       (Width:320;Height:480;ColorDepth: 8),(Width:320;Height:480;ColorDepth:15),(Width:320;Height:480;ColorDepth:16),
       (Width:320;Height:480;ColorDepth:24),(Width:320;Height:480;ColorDepth:32),
       (Width:360;Height:200;ColorDepth: 8),(Width:360;Height:200;ColorDepth:15),(Width:360;Height:200;ColorDepth:16),
       (Width:360;Height:200;ColorDepth:24),(Width:360;Height:200;ColorDepth:32),
       (Width:360;Height:240;ColorDepth: 8),(Width:360;Height:240;ColorDepth:15),(Width:360;Height:240;ColorDepth:16),
       (Width:360;Height:240;ColorDepth:24),(Width:360;Height:240;ColorDepth:32),
       (Width:360;Height:350;ColorDepth: 8),(Width:360;Height:350;ColorDepth:15),(Width:360;Height:350;ColorDepth:16),
       (Width:360;Height:350;ColorDepth:24),(Width:360;Height:350;ColorDepth:32),
       (Width:360;Height:400;ColorDepth: 8),(Width:360;Height:400;ColorDepth:15),(Width:360;Height:400;ColorDepth:16),
       (Width:360;Height:400;ColorDepth:24),(Width:360;Height:400;ColorDepth:32),
       (Width:360;Height:480;ColorDepth: 8),(Width:360;Height:480;ColorDepth:15),(Width:360;Height:480;ColorDepth:16),
       (Width:360;Height:480;ColorDepth:24),(Width:360;Height:480;ColorDepth:32),
       (Width:400;Height:300;ColorDepth: 8),(Width:400;Height:300;ColorDepth:15),(Width:400;Height:300;ColorDepth:16),
       (Width:400;Height:300;ColorDepth:24),(Width:400;Height:300;ColorDepth:32),
       (Width:512;Height:384;ColorDepth: 8),(Width:512;Height:384;ColorDepth:15),(Width:512;Height:384;ColorDepth:16),
       (Width:512;Height:384;ColorDepth:24),(Width:512;Height:384;ColorDepth:32)
      );

//------------------------------------------------------------------------------

procedure TDisplayOptions.Assign(Source: TPersistent);

begin
  if Source is TDisplayOptions then
  begin
    FFullScreen       :=TDisplayOptions(Source).FFullScreen;
    FScreenResolution :=TDisplayOptions(Source).FScreenResolution;
    FWindowAttributes :=TDisplayOptions(Source).FWindowAttributes;
    FWindowFitting    :=TDisplayOptions(Source).FWindowFitting;
  end
  else inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TryToAddToList(DeviceMode: TDevMode);

// Adds a video mode to the list if it's not a duplicate and can actually be set.

var I : Integer;

begin
  // See if this is a duplicate mode (can happen because of refresh
  // rates, or because we explicitly try all the low-res modes)
  for I:=1 to NumberVideomodes-1 do
    with DeviceMode do
      if ((dmBitsPerPel = VideoModes[I].ColorDepth) and
          (dmPelsWidth  = VideoModes[I].Width)      and
          (dmPelsHeight = VideoModes[I].Height))    then Exit; // it's a duplicate mode

  // do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
  if ChangeDisplaySettings(DeviceMode,CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then Exit;

  // it's a new, valid mode, so add this to the list
  with DeviceMode do
  begin
    VideoModes[NumberVideomodes].ColorDepth:=dmBitsPerPel;
    VideoModes[NumberVideomodes].Width:=dmPelsWidth;
    VideoModes[NumberVideomodes].Height:=dmPelsHeight;
    VideoModes[NumberVideomodes].Description:=Format('%d x %d, %d bpp',[dmPelsWidth,dmPelsHeight,dmBitsPerPel]);
  end;
  Inc(NumberVideomodes);
end;


//------------------------------------------------------------------------------

procedure ReadVideoModes;

var I, ModeNumber : Integer;
    done          : Boolean;
    DeviceMode    : TDevMode;
    DeskDC        : HDC;

begin
  // prepare 'default' entry
  DeskDC:=GetDC(0);
  with VideoModes[0] do
  try
    ColorDepth:=GetDeviceCaps(DeskDC,BITSPIXEL)*GetDeviceCaps(DeskDC,PLANES);
    Width:=Screen.Width;
    Height:=Screen.Height;
    Description:='default';
  finally
    ReleaseDC(0,DeskDC);
  end;

  // enumerate all available video modes
  ModeNumber:=0;
  repeat
    done:=not EnumDisplaySettings(nil,ModeNumber,DeviceMode);
    TryToAddToList(DeviceMode);
    Inc(ModeNumber);
  until (done or (NumberVideomodes >= MaxVideoModes));

  // low-res modes don't always enumerate, ask about them explicitly
  with DeviceMode do
  begin
    dmBitsPerPel:=8;
    dmPelsWidth:=42;
    dmPelsHeight:=37;
    dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    // make sure the driver doesn't just answer yes to all tests
    if ChangeDisplaySettings(DeviceMode,CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
    begin
      I:=0;
      while (I < NumberLowResModes-1) and (NumberVideoModes < MaxVideoModes) do
      begin
        dmSize:=Sizeof(DeviceMode);
        with LowResModes[I] do begin
           dmBitsPerPel:=ColorDepth;
           dmPelsWidth:=Width;
           dmPelsHeight:=Height;
        end;
        dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
        TryToAddToList(DeviceMode);
        Inc(I);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function GetIndexFromResolution(XRes,YRes,BPP: Integer): TResolution;

// Determines the index of a screen resolution nearest to the
// given values. The returned screen resolution is always greater
// or equal than XRes and YRes or, in case the resolution isn't
// supported, the value 0, which indicates the default mode.

var I : Integer;
    XDiff, YDiff, CDiff : Integer;

begin
  // prepare result in case we don't find a valid mode
  Result:=0;
  // set differences to maximum
  XDiff:=9999; YDiff:=9999; CDiff:=99;
  for I:=1 to NumberVideomodes-1 do
  with VideoModes[I] do
    if (Width  >= XRes) and ((Width-XRes)  <= XDiff) and
       (Height >= YRes) and ((Height-YRes) <= YDiff) and
       (ColorDepth >= BPP) and ((ColorDepth-BPP) <= CDiff)
    then
    begin
      XDiff:=Width-XRes;
      YDiff:=Height-YRes;
      CDiff:=ColorDepth-BPP;
      Result:=I;
    end;
end;

//------------------------------------------------------------------------------

function SetFullscreenMode(ModeIndex: Integer) : Boolean;

// changes to the video mode given by 'Index'

var DeviceMode : TDevMode;

begin
  with DeviceMode do
  begin
    dmSize:=SizeOf(DeviceMode);
    dmBitsPerPel:=VideoModes[ModeIndex].ColorDepth;
    dmPelsWidth:=VideoModes[ModeIndex].Width;
    dmPelsHeight:=VideoModes[ModeIndex].Height;
    dmFields:=DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
    // if mode set failed, we'll just run in windowed mode
    Result:=ChangeDisplaySettings(DeviceMode,CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
    if Result then CurrentVideoMode:=ModeIndex;
  end;
end;

//------------------------------------------------------------------------------

procedure ReadScreenImage(Dest: HDC; DestLeft, DestTop: Integer; SrcRect: TRectangle);

var ScreenDC : HDC;

begin
  ScreenDC:=GetDC(0);
  try
    GDIFlush;
    BitBlt(Dest,DestLeft,DestTop,SrcRect.Width,SrcRect.Height,ScreenDC,SrcRect.Left,SrcRect.Top,SRCCOPY);
  finally
    ReleaseDC(0,ScreenDC);
  end;
end;

//------------------------------------------------------------------------------

procedure RestoreDefaultMode;

// restores default desktop video mode

var T : TDevMode absolute 0; // a little trick to create a nil pointer

begin
  // Since the first parameter must be a var, we cannot use nil directly. Instead
  //  we use a variable with an absolute address of 0.
  ChangeDisplaySettings(T,CDS_FULLSCREEN);
end;

//------------------------------------------------------------------------------

initialization
  ReadVideoModes;
finalization
  if CurrentVideoMode <> 0 then RestoreDefaultMode;  // set default video mode
end.
