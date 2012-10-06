unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, G32, G32_Image, G32_RotLayer, G32_Transforms, G32_RangeBars;

type
  TForm1 = class(TForm)
    ImgView: TImgView32;
    GaugeBar1: TGaugeBar;
    Label1: TLabel;
    GaugeBar2: TGaugeBar;
    Label2: TLabel;
    GaugeBar3: TGaugeBar;
    Label3: TLabel;
    GaugeBar4: TGaugeBar;
    Label4: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GaugeBar1Change(Sender: TObject);
    procedure GaugeBar2Change(Sender: TObject);
    procedure GaugeBar4Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    L: TRotLayer;
    procedure FillBitmap(B: TBitmap32; N: Integer);
  end;

var
  Form1: TForm1;

implementation

uses Math;

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ImgView.Bitmap.SetSize(200, 200);
  FillBitmap(ImgView.Bitmap, 0);
  L := TRotLayer.Create(ImgView.Layers);
  L.Bitmap := TBitmap32.Create;
  with L.Bitmap do
  begin
    BeginUpdate;
    SetSize(100, 60);
    L.BitmapCenter := FloatPoint(Width / 2, Height / 2);
    FillBitmap(L.Bitmap, 127);
    SetBorderTransparent(L.Bitmap, Rect(0, 0, Width - 1, Height - 1));
    DrawMode := dmBlend;
    StretchFilter := sfLinear;
    EndUpdate;
    Changed;
  end;
  L.Scaled := True;
  L.Position := FloatPoint(100, 100);
end;

procedure TForm1.GaugeBar1Change(Sender: TObject);
begin
  L.Angle := GaugeBar1.Position;
end;

procedure TForm1.GaugeBar2Change(Sender: TObject);
var
  P: TFloatPoint;
begin
  P := L.Position;
  P.X := GaugeBar2.Position;
  P.Y := GaugeBar3.Position;
  L.Position := P;
end;

procedure TForm1.GaugeBar4Change(Sender: TObject);
begin
  ImgView.Scale := Power(10, GaugeBar4.Position / 100);
  ImgView.Update;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  L.Scaled := not L.Scaled;
end;

procedure TForm1.FillBitmap(B: TBitmap32; N: Integer);
var
  X, Y: Integer;
begin
  for Y := 0 to B.Height - 1 do
    for X := 0 to B.Width - 1 do
      B[X, Y] := Color32(Random(127) + N, Random(127) + N, Random(127) + N, Random(127) + N);
end;

end.
