unit ActiveBtnColor;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TActiveBtnColor = class(TBitBtn)
  private
    fOrigColor: TColor;
  protected
    procedure MouseEnter(var Msg: TMessage);message cm_mouseEnter;
    procedure MouseLeave(var Msg: TMessage);message cm_mouseLeave;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

var EredetiSzin: TColor;

implementation

procedure Register;
begin
  RegisterComponents('Md', [TActiveBtnColor]);
end;

{ TActiveBtnColor }

procedure TActiveBtnColor.MouseEnter(var Msg: TMessage);
begin
 Font.Style := Font.Style + [fsBold];
 EredetiSzin := Font.Color;
end;

procedure TActiveBtnColor.MouseLeave(var Msg: TMessage);
begin
 Font.Style := Font.Style - [fsBold];
 Font.Color := EredetiSzin;
 Top := Top + 5;
 Left := Left + 5;
 Width := Width - 10;
 Height := Height -10;
end;

end.
