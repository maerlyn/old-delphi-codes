unit MAIN2;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, fhd, fglib;

type
  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    HintDesign1: THintDesign;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Déclarations private }
  public
    { Déclarations public }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  HintSetting.ShowModal;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  close;
end;

end.
