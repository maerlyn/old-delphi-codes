unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtDlgs, ExtCtrls;

type
  TColorsType = record
    R,
    G,
    B: byte;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    OpenPictureDialog1: TOpenDialog;
    SavePictureDialog1: TSaveDialog;
    function RGBToColors(RGB: integer): TColorsType;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  sfn,
  dfn: string;

implementation

{$R *.DFM}

function TForm1.RGBToColors(RGB: integer): TColorsType;
var temp: integer;
begin
 temp := RGB;
 Result.B := temp div 65536;
 temp := temp - (Result.B*65536);
 Result.G := temp div 256;
 temp  := temp - (Result.G*256);
 Result.R := temp;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if not OpenPictureDialog1.Execute then
  Abort;

 sfn := OpenPictureDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 if not SavePictureDialog1.Execute then
  Abort;

 dfn := SavePictureDialog1.FileName;
end;

procedure TForm1.Button3Click(Sender: TObject);
var i, k: integer;
    tmp: TColorsType;
begin
 Screen.Cursor := crHourGlass;

 Image1.Picture.Bitmap.LoadFromFile(sfn);
// Image1.Picture.Bitmap.LoadFromFile(sfn);

 for i := 1 to Image1.Width do
  for k := 1 to Image1.Height do
  begin
   tmp := RGBToColors(Image1.Canvas.Pixels[i,k]);
   Image1.Canvas.Pixels[i,k] := RGB(tmp.B,
                                    tmp.G,
                                    tmp.R);
  end;

  Image1.Picture.Bitmap.SaveToFile(dfn);

  Screen.Cursor := crDefault;

  Application.MessageBox('Kész','',mb_OK);
end;

end.
