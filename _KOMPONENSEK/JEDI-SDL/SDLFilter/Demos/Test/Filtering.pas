unit Filtering;
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                SDL Image Filtering and effects functions                                         }
{                                                                              }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Jason Farmer <jason@cerebral-bicycle.co.uk>                                  }
{                                                                              }
{ Portions created by Jason Farmer are                                         }
{ Copyright (C) 2000 - 2001 Jason Farmer.                                         }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{                                                                              }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   Helper functions...                                                        }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL.so on Linux platform                                                }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   Sept   30   2001 - JF : First written                                      }
{   Oct    01   2001 - DL : Made Kylix friendly                                }
{******************************************************************************}
interface

uses
  SysUtils,
  Classes,
{$IFDEF WIN32}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
{$ENDIF}
{$IFDEF Linux}
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
{$ENDIF}
  SDL,
  SDLUtils,
  Logger,
  SDL_Image,
  SDLFilter;

type
  TForm1 = class(TForm)
    btnLoadImage: TButton;
    btnApplyFilter: TButton;
    btnQuit: TButton;
    dlgOpenFile: TOpenDialog;
    rdgSelectFilter: TRadioGroup;
    btnOutlineImage: TButton;
    chkTransparent: TCheckBox;
    chkCombine: TCheckBox;
    rgpOutputTypes: TRadioGroup;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    eR: TEdit;
    eG: TEdit;
    eB: TEdit;
    procedure btnLoadImageClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure rdgSelectFilterClick(Sender: TObject);
    procedure DrawRects();
    procedure btnOutlineImageClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ImageLoaded: Boolean;
  WorkSurface: PSDL_Surface;
  ResultsSurface: PSDL_Surface;
  Screen_: PSDL_Surface;
  dstrect: TSDL_Rect;
  MyFilter: T3x3Kernel;
  OutlineColour: Cardinal;

implementation

{$IFDEF WIN32}
{$R *.DFM}
{$ENDIF}

{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

procedure TForm1.btnLoadImageClick(Sender: TObject);
var
  imagebmp: PSDL_Surface;
begin
  ImageLoaded := false;
  with dlgOpenFile do
  begin
    if Execute() = True then
    begin
      {if (imagebmp <> nil) then
      begin
        SDL_FreeSurface(imagebmp);
      end; }

      imagebmp := IMG_Load(pchar(dlgopenfile.FileName));

      if (imagebmp <> nil) then
      begin
        // Set the video colormap
        if (imagebmp.format.palette <> nil) and (Screen_.Format.palette <> nil
          ) then
        begin

        end;
        imagebmp.flags := imagebmp.flags or SDL_SRCCOLORKEY;
        imagebmp.format.colorkey := 0;

        WorkSurface := SDL_DisplayFormat(imagebmp);

        if chkTransparent.Checked = true then
        begin
          SDL_SetColorKey(WorkSurface, SDL_SRCCOLORKEY, SDL_GetPixel(
            WorkSurface, 0, 0));
        end;

        SDL_FreeSurface(imagebmp);

        ImageLoaded := true;

      end;

      imagebmp := IMG_Load(pchar(dlgopenfile.FileName));

      if not (imagebmp = nil) then
      begin
        // Set the video colormap
        if (imagebmp.format.palette <> nil) and (Screen_.Format.palette <> nil
          ) then
        begin

        end;
        imagebmp.flags := imagebmp.flags or SDL_SRCCOLORKEY;
        imagebmp.format.colorkey := 0;

        ResultsSurface := SDL_DisplayFormat(imagebmp);

        if chkTransparent.Checked = true then
        begin
          SDL_SetColorKey(ResultsSurface, SDL_SRCCOLORKEY, SDL_GetPixel(
            ResultsSurface, 0, 0));
        end;

        SDL_FreeSurface(imagebmp);

        ImageLoaded := true;

      end;

    end;
  end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Tidy up SDL
  SDL_Quit;
end;

procedure TForm1.btnApplyFilterClick(Sender: TObject);
begin
  if ImageLoaded = true then
  begin
    if rdgSelectFilter.ItemIndex >= 0 then
    begin
      // Apply the selected filter to the images
      ApplyFilter(WorkSurface, nil, ResultsSurface, nil, @MyFilter);
      // Repaint the screen
      DrawRects;
    end
    else
      ShowMessage( 'Please Select a Filter to Apply' );
  end
  else
    ShowMessage( 'Please Load an Image' );
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dlgOpenFile.InitialDir := ExtractFileDir(ParamSTr(0)) + '/images';
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
  begin
    Log.LogError(Format('Couldn''t initialize SDL : %s', [SDL_GetError]
      ), 'btnInitialiseClick');
    SDL_Quit;
    halt(1);
  end; // Initialize the display in a 640x480 32 bit mode

  // Set the title bar in environments that support it */
  SDL_WM_SetCaption('Main Filter Viewing Window', nil);

  screen_ := SDL_SetVideoMode(640, 480, 32, SDL_SWSURFACE);
  if (screen_ = nil) then
  begin
    Log.LogError(Format('Couldn''t set 640x480x32 video mode : %s',
      [SDL_GetError]), 'btnInitialiseClick');
    SDL_Quit;
    halt(1);
  end;
end;

procedure TForm1.btnQuitClick(Sender: TObject);
begin
  Form1.close;
end;

procedure TForm1.rdgSelectFilterClick(Sender: TObject);
var
  KernelToUse: TKernelTypes;
begin

  case rdgSelectFilter.ItemIndex of
    0: KernelToUse := LowPassUniform;
    1: KernelToUse := LowPassPeaked;
    2: KernelToUse := LowPassStronglyPeaked;
    3: KernelToUse := HighPassVeryWeak;
    4: KernelToUse := HighPassWeak;
    5: KernelToUse := HighPassStrong;
    6: KernelToUse := HighPassVeryStrong;
    7: KernelToUse := PrewittEdge_NW_SE;
    8: KernelToUse := PrewittEdge_N_S;
    9: KernelToUse := PrewittEdge_NE_SW;
    10: KernelToUse := PrewittEdge_E_W;
    11: KernelToUse := PrewittEdge_SE_NW;
    12: KernelToUse := PrewittEdge_S_N;
    13: KernelToUse := PrewittEdge_SW_NE;
    14: KernelToUse := PrewittEdge_W_E;
    15: KernelToUse := LapiacianEdgeWeak;
    16: KernelToUse := LapiacianEdgeStrong;
    17: KernelToUse := LapiacianEdgeVeryStrong;

  end;
  // Build the selected 3x3 Kernel
  Build3x3Kernel(KernelToUse, @MyFilter);

end;

procedure TForm1.DrawRects;
var
  ImageRect: SDL_Rect;
begin
  // Routine to refresh the display

  // Clear the screen
  with ImageRect do
  begin
    x := 0;
    y := 0;
    w := Screen_.w;
    h := Screen_.h
  end;

  SDL_FillRect(screen_, @imagerect, 0);

  // Draw the original image
  with ImageRect do
  begin
    x := 0;
    y := 0;
    w := worksurface.w;
    h := worksurface.h
  end;
  SDL_BlitSurface(worksurface, nil, Screen_, @ImageRect);
  // Draw the results
  with ImageRect do
  begin
    x := 0;
    y := worksurface.h;
    w := worksurface.w;
    h := worksurface.h;
  end;

  SDL_BlitSurface(ResultsSurface, nil, Screen_, @ImageRect);

  // Combine the images
  if chkCombine.Checked = true then
  begin
    with ImageRect do
    begin
      x := 0;
      y := worksurface.h + ResultsSurface.h;
      w := worksurface.w;
      h := worksurface.h;
    end;
    if rgpOutputTypes.ItemIndex = 0 then
    begin
      SDL_BlitSurface(worksurface, nil, Screen_, @ImageRect);
      SDL_BlitSurface(ResultsSurface, nil, Screen_, @ImageRect);
    end;

    if rgpOutputTypes.ItemIndex = 1 then
    begin
      SDL_BlitSurface(worksurface, nil, Screen_, @ImageRect);
      SDL_AddSurface(ResultsSurface, nil, Screen_, @ImageRect);
    end;

    if rgpOutputTypes.ItemIndex = 2 then
    begin
      SDL_BlitSurface(worksurface, nil, Screen_, @ImageRect);
      SDL_SubSurface(ResultsSurface, nil, Screen_, @ImageRect);
    end;
  end;

  // Update the screen
  dstrect.x := 0;
  dstrect.y := 0;
  dstrect.w := screen_.w;
  dstrect.h := screen_.h;
  SDL_UpdateRects(screen_, 1, @dstrect);

end;

procedure TForm1.btnOutlineImageClick(Sender: TObject);
var
  Red, Green, Blue: integer;
  R, G, B: Byte;
begin
  if ImageLoaded = true then
  begin
    if eR.text = '' then
      eR.text := '0';
    if eG.text = '' then
      eG.text := '0';
    if eB.text = '' then
      eB.text := '0';

    red := StrToInt(eR.Text);
    green := StrToInt(eG.Text);
    blue := StrToInt(eB.Text);

    if red > 255 then
      red := 255;
    if red < 0 then
      red := 0;

    if Green > 255 then
      Green := 255;
    if Green < 0 then
      Green := 0;

    if Blue > 255 then
      Blue := 255;
    if Blue < 0 then
      Blue := 0;

    r := Red;
    g := Green;
    b := Blue;

    // Make the results image the outline of the work image
    OutlineColour := SDL_MapRGB(WorkSurface.format, R, G, B);
    ApplyImageOutline(WorkSurface, nil, ResultsSurface, nil, OutlineColour);
    // Repaint the screen
    DrawRects;

  end;
end;

end.

