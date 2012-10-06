unit Unit1;

interface

uses Forms, Graphics, Classes, Controls, StdCtrls, ButtonWithColor, Dialogs;

type
  TForm1 = class(TForm)
    BitBtnWithColor1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    Button3: TBitBtnWithColor;
    ColorDialog: TColorDialog;
    procedure Button2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button2MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button2.Color := clLime;
  Button2.Font.Style := [fsBold];
  Button2.Caption := 'Pressed...';
end;

procedure TForm1.Button2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Button2.Color := clBtnFace;
  Button2.Font.Style := [];
  Button2.Caption := 'Press me !!!';  
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ColorDialog.Color := Button3.Color;
  if ColorDialog.Execute then
   Button3.Color := ColorDialog.Color;
end;

end.
