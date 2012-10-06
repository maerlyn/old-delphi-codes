unit Schema;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TSchema1Form = class(TForm)
    Image1: TImage;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Schema1Form: TSchema1Form;

implementation

{$R *.DFM}

procedure TSchema1Form.Button1Click(Sender: TObject);
begin
  Close;
end;

end.

