unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Kerdes, ExtCtrls;

type
  TKerdes = record
   Esemeny: string;
   Evszam: string;
  end;

  TfrmMainForm = class(TForm)
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure BillentyuNyomas(var Msg: TWmKeyDown);message wm_KeyDown;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  end;

var
  frmMainForm: TfrmMainForm;
  AktualisMenupont: integer;

const
  Szovegek: array[0..3] of string = ('Gyakorlás','Teszt','','Kilépés');

implementation

uses Gyakorlas, Teszt;

{$R *.DFM}

procedure TfrmMainForm.BillentyuNyomas(var Msg: TWmKeyDown);
var i: integer;
begin
 case Msg.CharCode of
  38: begin                            //fel
       Dec(AktualisMenupont);
       if AktualisMenupont = -1 then AktualisMenupont := 3;
       if AktualisMenupont = 2 then AktualisMenupont := 1;
      end;
  40: begin                            //le
       Inc(AktualisMenupont);
       if AktualisMenupont = 4 then AktualisMenupont := 0;
       if AktualisMenupont = 2 then AktualisMenupont := 3;
      end;
  13: begin                            //enter
       case AktualisMenupont of
        0: Button1Click(Application);
        1: Button2Click(Application);
        2: Exit;
        3: Button3Click(Application);
       end;
      end;
 end;

 PaintBox1.Canvas.Font.Color := clBlack;
 for i := 0 to 3 do
 begin
  PaintBox1.Canvas.TextOut(5,i * 50,Szovegek[i]);
  if i = AktualisMenupont then
  begin
   PaintBox1.Canvas.Font.Color := RGB(7,223,248);
   PaintBox1.Canvas.TextOut(5,i * 50,Szovegek[i]);
   PaintBox1.Canvas.Font.Color := clBlack;
  end;
 end;
end;

procedure TfrmMainForm.Button1Click(Sender: TObject);
var temp: integer;
begin
 temp := frmKerdes.ShowModal;
 if temp = mrOk then
 begin
  frmGyakorlas.UjKerdes;
  frmGyakorlas.Show;
 end;
end;

procedure TfrmMainForm.Button2Click(Sender: TObject);
var temp: integer;
begin
 temp := frmKerdes.ShowModal;
 if temp = mrOk then
 begin
  frmTeszt.UjTeszt;
  frmTeszt.Show;
 end;
end;

procedure TfrmMainForm.Button3Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TfrmMainForm.FormCreate(Sender: TObject);
var temp: TWmKeyDown;
begin
 AktualisMenupont := -1;
 PaintBox1.Font.Name := 'Arial';
 PaintBox1.Font.Size := 20;
 PaintBox1.Canvas.Font.Name := 'Arial';
 PaintBox1.Canvas.Font.Size := 20;

 temp.CharCode := 40;
 BillentyuNyomas(temp);
end;

procedure TfrmMainForm.FormPaint(Sender: TObject);
var i: integer;
    Kocka: TRect;
begin
 PaintBox1.Canvas.Pen.Color := clBtnFace;
 PaintBox1.Canvas.Brush.Color := clBtnFace;
 PaintBox1.Canvas.Brush.Style := bsSolid;
 PaintBox1.Canvas.Font.Color := clBlack;
 with Kocka do
 begin
  Left := 0;
  Top := 0;
  Bottom := PaintBox1.Height;
  Right := PaintBox1.Width;
 end;
 PaintBox1.Canvas.FillRect(Kocka);

 for i := 0 to 3 do
 begin
  PaintBox1.Canvas.TextOut(5,i * 50,Szovegek[i]);
  if i = AktualisMenupont then
  begin
   PaintBox1.Canvas.Font.Color := RGB(7,223,248);
   PaintBox1.Canvas.TextOut(5,i * 50,Szovegek[i]);
   PaintBox1.Canvas.Font.Color := clBlack;
  end;
 end;
end;

end.
