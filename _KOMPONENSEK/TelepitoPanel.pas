unit TelepitoPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TTelepitoPanel = class(TPanel)
  private
    fColorActive: TColor;
    fFontActive: TColor;
    fOrigColor: TColor;
    fOrigFont: TColor;
  protected
    procedure SetColorActive(Value: TColor);
    procedure SetFontActive(Value: TColor);
    procedure MouseEnter(var Msg: TMessage);message cm_MouseEnter;
    procedure MouseLeave(var Msg: TMessage);message cm_MouseLeave;
  public
    constructor Create(AOwner: TComponent);override;
  published
    property ColorActive: TColor read fColorActive write SetColorActive default clBlue;
    property FontActive: TColor read fFontActive write SetFontActive;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Md', [TTelepitoPanel]);
end;

{ TTelepitoPanel }

constructor TTelepitoPanel.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);

 Self.Font.Name := 'Arial Black';
 Self.Font.Size := 7;

 Self.BevelOuter := bvNone;
end;

procedure TTelepitoPanel.MouseEnter(var Msg: TMessage);
begin
 fOrigColor := Self.Color;
 Self.Color := fColorActive;
 fOrigFont := Self.Font.Color;
 Self.Font.Color := fFontActive;
end;

procedure TTelepitoPanel.MouseLeave(var Msg: TMessage);
begin
 Self.Font.Color := fOrigFont;
 Self.Color := fOrigColor;
end;

procedure TTelepitoPanel.SetColorActive(Value: TColor);
begin
 if fColorActive <> Value then
  fColorActive := Value;

 Invalidate;
end;

procedure TTelepitoPanel.SetFontActive(Value: TColor);
begin
 if fFontActive <> Value then
  fFontActive := Value;

 Invalidate;
end;

end.

