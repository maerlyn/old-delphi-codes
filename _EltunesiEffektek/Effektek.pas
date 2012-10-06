unit Effektek;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure RemoveWindow_Shrink(Handle: HWND; Obj: TForm);
    procedure RemoveWindow_Clock(Handle: HWND; Obj: TForm);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  ps_o:array[0..3]of tpoint;
  fh_o:thandle;

const
  kulombseg: integer = 25;

implementation

{$R *.DFM}

procedure TForm1.RemoveWindow_Shrink(Handle: HWND; Obj: TForm);
var
  lepeskoz    : integer;
  varakozas   : integer;
  oldalarany  : real;
  y_up        : integer;
  y_down      : integer;
  x_left      : integer;
  x_right     : integer;
  rgn         : HRGN;
  rgnpoints   : array[1..22] of tpoint;
  rgnpcount   : integer;
begin
  rgnpcount:=4;

  y_up       :=0;
  y_down     :=obj.height;
  oldalarany :=obj.width/obj.height;
  lepeskoz   :=10;
  varakozas  :=10;
  randomize;
  //ELTUNTETES
  while (y_up<y_down) do
  begin
    inc(y_up,   lepeskoz);
    dec(y_down, lepeskoz);
    x_left  :=  round(y_up  *  oldalarany);
    x_right :=  round(y_down * oldalarany);
    while y_up>y_down+1 do
    begin
      dec(y_up);
      inc(y_down);
    end;
    rgnpoints[1].x:=x_right;  rgnpoints[1].y:=y_down;
    rgnpoints[2].x:=x_left;   rgnpoints[2].y:=y_down;
    rgnpoints[3].x:=x_left;   rgnpoints[3].y:=y_up;
    rgnpoints[4].x:=x_right;  rgnpoints[4].y:=y_up;
    Rgn := CreatePolygonRgn(RgnPoints, rgnpcount, ALTERNATE);
    SetWindowRgn(Handle, Rgn, True);
    sleep(varakozas);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 ps_o[0] := point(0,0);
 ps_o[1] := point(width,0);
 ps_o[2] := point(width,height);
 ps_o[3] := point(0,height);
 fh_o := CreatePolygonRgn(ps_o,4,1);
 RemoveWindow_Clock(Form1.Handle,Form1);
 Sleep(500);
 SetWindowRgn(Form1.Handle,fh_o,true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 ps_o[0] := point(0,0);
 ps_o[1] := point(width,0);
 ps_o[2] := point(width,height);
 ps_o[3] := point(0,height);
 fh_o := CreatePolygonRgn(ps_o,4,1);
 RemoveWindow_Shrink(Form1.Handle,Form1);
 Sleep(500);
 SetWindowRgn(Form1.Handle,fh_o,true);
end;

procedure TForm1.RemoveWindow_Clock(Handle: HWND; Obj: TForm);
var xm, ym: integer;
    ps: array[0..6] of TPoint;
    fh: THandle;
    x, y: integer;
begin
 xm := Obj.Width div 2;
 ym := Obj.Height div 2;

 x := xm;
 while x <= Obj.Width do
 begin
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(x,0);
  ps[4] := point(Obj.width,0);
  ps[5] := point(Obj.width,Obj.height);
  ps[6] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,7,1);
  SetWindowRgn(Handle,fh,true);
  Sleep(1);
  x := x + 25;
 end;
 y := 0;
 while y <= height do
 begin
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(Obj.width,y);
  ps[4] := point(Obj.width,Obj.height);
  ps[5] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,6,1);
  SetWindowRgn(Handle,fh,true);
  Sleep(1);
  y := y + 25;
 end;
 x := width;
 while x >= 0 do
 begin
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(x,Obj.height);
  ps[4] := point(0,Obj.height);
  fh := CreatePolygonRgn(ps,5,1);
  SetWindowRgn(Handle,fh,true);
  Sleep(1);
  x:=x - 25;
 end;
 y := height;
 while y >= 0 do
 begin
  ps[0] := point(0,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  ps[3] := point(0,y);
  fh := CreatePolygonRgn(ps,4,1);
  SetWindowRgn(Handle,fh,true);
  Sleep(1);
  y := y - 25;
 end;
 x := 0;
 while x <= xm do
 begin
  ps[0] := point(x,0);
  ps[1] := point(xm,0);
  ps[2] := point(xm,ym);
  fh := CreatePolygonRgn(ps,3,1);
  SetWindowRgn(Handle,fh,true);
  Sleep(1);
  x := x + 25;
 end;
end;

end.

