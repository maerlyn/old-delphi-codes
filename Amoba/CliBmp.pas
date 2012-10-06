unit CliBmp;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TFormBmp = class(TForm)
    Image1: TImage;
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormBmp: TFormBmp;

implementation

{$R *.DFM}

procedure TFormBmp.FormResize(Sender: TObject);
begin
 Image1.Left := 0;
 Image1.Top := 0;
 Image1.Width := FormBmp.ClientWidth;
 Image1.Height := FormBmp.ClientHeight;
end;

end.
