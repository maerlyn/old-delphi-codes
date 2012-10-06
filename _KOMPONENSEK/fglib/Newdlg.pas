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

unit Newdlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls, Menus,
  Forms, Dialogs, ExtCtrls, StdCtrls, DsgnIntf,
  DesiType, OpenBmp, OpenWav, CurDlg;

type
  TOpenBitmap = class(TCommonDialog)
  private
    FAbout : AboutDesignLib;
    FFileName: TBmpFileName;
    FInitialDir: String;
    FTitle: String;
    FLanguage : TLanguage;
    procedure OnSharewareTimer(Sender: TObject);
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property About : AboutDesignLib read FAbout write FAbout;
    property FileName: TBmpFileName read FFileName write FFileName;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

  TOpenWave = class(TCommonDialog)
  private
    FAbout : AboutDesignLib;
    FFileName: TWaveFileName;
    FInitialDir: String;
    FTitle: String;
    FLanguage : TLanguage;
    procedure OnSharewareTimer(Sender: TObject);
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property About : AboutDesignLib read FAbout write FAbout;
    property FileName: TWaveFileName read FFileName write FFileName;
    property InitialDir: string read FInitialDir write FInitialDir;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

  TCursorsDialog = class(TCommonDialog)
  private
    FAbout : AboutDesignLib;
    FCursor : TCursor;
    FTitle: String;
    FLanguage : TLanguage;
    procedure OnSharewareTimer(Sender: TObject);
    procedure SetLanguage(value : TLanguage);
    function GetLanguage : TLanguage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
  published
    property About : AboutDesignLib read FAbout write FAbout;
    property Cursor: TCursor read FCursor write FCursor;
    property Title: string read FTitle write FTitle;
    property Language : TLanguage read GetLanguage write SetLanguage;
  end;

implementation

{-----------------------------------------------------------------------------}
{  Component : TOpenBitmap                                                    }
{-----------------------------------------------------------------------------}
constructor TOpenBitmap.create(aowner : tcomponent);
begin
  inherited Create(AOwner); 
  SharewareTimer.enabled := shareware;
  SharewareTimer.ontimer := OnSharewareTimer;
  FFileName := '';
  FInitialDir := '';
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TOpenBitmap.Destroy;
begin
  inherited Destroy;
end;    

procedure TOpenBitmap.OnSharewareTimer(Sender: TObject);
begin
  SharewareMessage;
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
  SharewareTimer.enabled := shareware;
  SharewareTimer.ontimer := OnSharewareTimer;
  FFileName := '';
  FInitialDir := '';
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TOpenWave.Destroy;
begin
  inherited Destroy;
end;    

procedure TOpenWave.OnSharewareTimer(Sender: TObject);
begin
  SharewareMessage;
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
  SharewareTimer.enabled := shareware;
  SharewareTimer.ontimer := OnSharewareTimer;
  FCursor := crDefault;
  FTitle := '';
  FLanguage := GetCurrentLanguage;
end;

destructor TCursorsDialog.Destroy;
begin
  inherited Destroy;
end;   

procedure TCursorsDialog.OnSharewareTimer(Sender: TObject);
begin
  SharewareMessage;
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
end.
