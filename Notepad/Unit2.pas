unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmNevjegy = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    cmdBezaras: TButton;
    procedure cmdBezarasClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmNevjegy: TfrmNevjegy;

implementation

{$R *.DFM}

procedure TfrmNevjegy.cmdBezarasClick(Sender: TObject);
begin
 frmNevjegy.Hide;
end;

end.
