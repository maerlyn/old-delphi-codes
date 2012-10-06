unit About;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TfrmAbout = class(TForm)
    PaintBox1: TPaintBox;
    procedure ShowAboutText;
    procedure Delay(Seconds, MilliSec: Word);
    procedure CreateParams(var Params: TCreateParams);override;
    procedure HitTest(var Msg: TWmNCHitTest);message wm_NcHitTest;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;
  Vege: boolean = false;
  vg: integer = 5;

implementation

uses MainForm;

{$R *.DFM}

procedure TfrmAbout.ShowAboutText;
var i, LineH: integer;
    FestendoNegyzet: TRect;
    Kep: TBitmap;
begin
 Vege := false;
 vg := 5;
 FestendoNegyzet.Left := 0;
 FestendoNegyzet.Top := 0;
 FestendoNegyzet.Right := PaintBox1.Width;
 FestendoNegyzet.Bottom := PaintBox1.Height;
  PaintBox1.Canvas.Font.Name := 'Arial';
  PaintBox1.Canvas.Font.Size := 20;
 PaintBox1.Color := frmMainForm.HatterSzin;
 PaintBox1.Font.Color := frmMainForm.Betuk;
 PaintBox1.Canvas.Font.Color := frmMainForm.Betuk;
  LineH := PaintBox1.Canvas.TextHeight('0');
  PaintBox1.Canvas.Pen.Color := frmMainForm.HatterSzin;
  PaintBox1.Canvas.Brush.Color := frmMainForm.HatterSzin;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.FillRect(FestendoNegyzet);

  Kep := TBitmap.Create;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'logo.bmp') then
   Kep.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'logo.bmp')
  else
  begin
   Kep.Height := 1;
   Kep.Width := 1;
   Kep.Canvas.Pen.Color := frmMainForm.HatterSzin;
   Kep.Canvas.Brush.Color := frmMainForm.HatterSzin;
   Kep.Canvas.Brush.Style := bsSolid;
   Kep.Canvas.FillRect(Rect(1,1,1,1));
  end;

  for i := 0 to 350 + LineH * 13 do
   with PaintBox1.Canvas do
   begin
    Draw(40,350 - I - 10 - Kep.Height,Kep);
    TextOut(40,350 - I,'                                                           ');
    TextOut(40,350 + LineH * 1 - I,'CD-nyilvántartó                                ');
    TextOut(40,350 + LineH * 2 - I,'Programmed by: Putra  Ware                     ');
    TextOut(40,350 + LineH * 3 - I,'2001  -  2002                                  ');
    TextOut(40,350 + LineH * 4 - I,'                                               ');
    TextOut(40,350 + LineH * 5 - I,'for Boxer                                      ');
    TextOut(40,350 + LineH * 6 - I,'                                               ');
    TextOut(40,350 + LineH * 7 - I,'                                               ');
    TextOut(40,350 + LineH * 8 - I,'e-Mail: TeleVonZsinor@yahoo.com                ');
    TextOut(40,350 + LineH * 9 - I,'honlap: http://putraware.ini.hu                ');
    Delay(0,vg);
    if Vege then
    begin
     Exit;
     frmAbout.Close;
    end;
   end;
 frmAbout.Close;
end;

procedure TfrmAbout.Delay(Seconds, MilliSec: Word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

procedure TfrmAbout.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 Params.Style := (Params.Style or ws_Popup) and not ws_Caption;
end;

procedure TfrmAbout.HitTest(var Msg: TWmNCHitTest);
begin
 inherited;
 if (Msg.Result = htClient) and (Msg.YPos < PaintBox1.Height + Top + GetSystemMetrics(sm_cyFrame)) then Msg.Result := htCaption;
end;

procedure TfrmAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 27 then
  vg := 0;
//  Vege := true;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
 Self.Color := frmMainForm.HatterSzin;
 PaintBox1.Color := frmMainForm.HatterSzin;
 PaintBox1.Font.Color := frmMainForm.Betuk;
 PaintBox1.Canvas.Font.Color := frmMainForm.Betuk;
end;

end.
