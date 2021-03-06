unit TFlatSoundUnit;

{***************************************************************}
{  TFlatSound                                                   }
{  Copyright �1999 Lloyd Kinsella.                              }
{                                                               }
{  FlatStyle is Copyright �1998-99 Maik Porkert.                }
{***************************************************************}

interface

{$I Version.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  MMSystem;

type
 TSoundEvent = (seBtnClick, seMenu, seMenuClick, seMoveIntoBtn, sePanelExpand);

type
  TFlatSound = class(TComponent)
  private
    FEvent: TSoundEvent;
    procedure SetEvent(Value: TSoundEvent);
  public
    procedure Play;
    procedure PlayThis(ThisEvent: TSoundEvent);
    constructor Create(AOwner: TComponent); override;
  published
    property Event: TSoundEvent read FEvent write SetEvent;
  end;

{$R FLATSOUND.RES}

const
  Flags = SND_RESOURCE or SND_SYNC;

implementation

constructor TFlatSound.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Event := seBtnClick;
end;

procedure TFlatSound.Play;
begin
  case FEvent of
    seBtnClick:    PlaySound('ENC_001',0,Flags);
    seMenu:        PlaySound('ENC_001',0,Flags);
    seMenuClick:   PlaySound('ENC_001',0,Flags);
    seMoveIntoBtn: PlaySound('ENC_001',0,Flags);
   sePanelExpand: PlaySound('ENC_001',0,Flags);
  end;
end;

procedure TFlatSound.PlayThis(ThisEvent: TSoundEvent);
begin
  case ThisEvent of
    seBtnClick:    PlaySound('ENC_001',0,Flags);
    seMenu:        PlaySound('ENC_001',0,Flags);
    seMenuClick:   PlaySound('ENC_001',0,Flags);
    seMoveIntoBtn: PlaySound('ENC_001',0,Flags);
    sePanelExpand: PlaySound('ENC_001',0,Flags);
  end;
end;

procedure TFlatSound.SetEvent(Value: TSoundEvent);
begin
  if Value <> FEvent then
  begin
    FEvent := Value;
  end;
end;

end.
