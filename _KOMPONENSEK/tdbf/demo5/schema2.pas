unit Schema2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TSchema2Form = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Schema2Form: TSchema2Form;

implementation

{$R *.DFM}

procedure TSchema2Form.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

