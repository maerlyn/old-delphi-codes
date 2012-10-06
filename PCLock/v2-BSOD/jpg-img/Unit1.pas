unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, JPEG, Gauges;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Image1: TImage;
    Memo1: TMemo;
    Gauge1: TGauge;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  a: TJpegImage;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var i, k: integer;
    f: textfile;
begin
 OpenDialog1.InitialDir := ExtractFilePath(ParamStr(0));

 if not OpenDialog1.Execute then
  Abort;

 SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
 if not SaveDialog1.Execute then
  Abort;

 a := TJPEGImage.Create;
 a.LoadFromFile(OpenDialog1.FileName);

 Image1.Picture.Bitmap.Assign(a);
// Image1.Picture.LoadFromFile(OpenDialog1.FileName);
// Memo1.Lines.Clear;

 AssignFile(f,savedialog1.FileName);
 ReWrite(f);
 writeln(f,inttostr(a.width));
 writeln(f,inttostr(a.height));

// Memo1.Lines.Add(IntToStr(a.Width));
// Memo1.Lines.Add(IntToStr(a.Height));

 Gauge1.MaxValue := a.Width * a.Height;

 for i := 1 to a.Width do
  for k := 1 to a.Height do
  begin
//   Memo1.Lines.Add(IntToStr(Image1.Canvas.Pixels[i,k]));
   writeln(f,inttostr(image1.canvas.pixels[i,k]));
   Gauge1.Progress := Gauge1.Progress + 1;
  end;

  CloseFIle(f);
// Memo1.Lines.SaveToFile(SaveDialog1.FileName);

 MessageDlg('Vége',mtError,[mbOK],0);

 Application.Terminate;
end;

end.
