unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TfrmModositas = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    txtNev: TEdit;
    txtCim: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmModositas: TfrmModositas;

implementation

{$R *.DFM}

end.

