unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Button1: TButton;
    SpinEdit1: TSpinEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Vonal(x1,y1,x2,y2: integer);
    procedure Sarok(x1,y1,x2,y2,x3,y3, step: integer);
  end;

var
  Form1: TForm1;
  t: integer;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
 if (spinedit1.value<1) then
  abort;

 image1.canvas.pen.color := clwhite;
 image1.canvas.brush.color := clwhite;
 image1.canvas.pen.style := psSolid;
 image1.canvas.brush.style := bssolid;
 image1.canvas.FillRect(rect(0,0,image1.width,image1.height));

 image1.canvas.pen.color := clblack;
 image1.canvas.brush.color := clblack;

 vonal(10,10,510,10);
 vonal(10,10,10,510);

 t := 0;

 while t<500 do
 begin

  inc(t,spinedit1.value);
  vonal(10,510-t,10+t,10);

 end;
end;

procedure TForm1.Sarok(x1, y1, x2, y2, x3, y3, step: integer);
var t: integer;
begin
 // _|  bal felso sarok

 if (y1=y2)and(x2=x3)and(x1<x2)and(y2>y3) then
 begin
  Vonal(x1,y1,x2,y2);
  Vonal(x2,y2,x3,y3);
  t := 0;
  while t < (x2-x1) do
  begin
   inc(t,step);
   Vonal(x1+t,y1,x3,y2-t);
  end;
 end;

 // |_  jobb felso sarok

 if (x1=x2)and(y2=y3)and(y1<y2)and(x3>x2) then
 begin
  Vonal(x1,y1,x2,y2);
  Vonal(x2,y2,x3,y3);
  t := 0;
  while t < (y2-y1) do
  begin
   inc(t,step);
   Vonal(x1,y1+t,x2+t,y2);
  end;
 end;

 // -|  bal also sarok

 if (y1=y2)and(x2=x3)and(x1<x2)and(y2<y3) then
 begin
  Vonal(x1,y1,x2,y2);
  Vonal(x2,y2,x3,y3);
  t := 0;
  while t < (x2-x1) do
  begin
   inc(t,step);
   Vonal(x1+t,y1,x2,y2+t);
  end;
 end;

 // |-  jobb also sarok

 if (x1=x2)and(y2=y3)and(y1>y2)and(x2<x3) then
 begin
  Vonal(x1,y1,x2,y2);
  Vonal(x2,y2,x3,y3);
  t := 0;
  while t < (x3-x2) do
  begin
   inc(t,step);
   Vonal(x1,y1-t,x2+t,y2);
  end;
 end;

end;

procedure TForm1.Vonal(x1, y1, x2, y2: integer);
begin
 Image1.Canvas.MoveTo(x1,y1);
 Image1.Canvas.LineTo(x2,y2);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 image1.canvas.pen.color := clwhite;
 image1.canvas.brush.color := clwhite;
 image1.canvas.pen.style := psSolid;
 image1.canvas.brush.style := bssolid;
 image1.canvas.FillRect(rect(0,0,image1.width,image1.height));

 image1.canvas.pen.color := clblack;
 image1.canvas.brush.color := clblack;

 Sarok(10,110,10,60,60,60,spinedit1.value);
 Sarok(10,60,60,60,60,10,spinedit1.value);
 Sarok(60,60,60,10,110,10,spinedit1.value);
 Sarok(110,10,160,10,160,60,spinedit1.value);
 Sarok(160,10,160,60,210,60,spinedit1.value);
 Sarok(160,60,210,60,210,110,spinedit1.value);
 Sarok(160,160,210,160,210,110,spinedit1.value);
 Sarok(160,210,160,160,210,160,spinedit1.value);
 Sarok(110,210,160,210,160,160,spinedit1.value);
 Sarok(60,160,60,210,110,210,spinedit1.value);
 Sarok(10,160,60,160,60,210,spinedit1.value);
 Sarok(10,110,10,160,60,160,spinedit1.value);
end;

end.
