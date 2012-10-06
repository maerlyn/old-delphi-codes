unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  G32, G32_Transforms, StdCtrls, AppEvnts, G32_Image, G32_Layers;

type
  TForm1 = class(TForm)
    Image32: TImage32;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    BitmapList: TBitmap32List;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    Velocities: TArrayOfFloatPoint;
    procedure IdleHandler(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  X: Integer;
  ALayer: TBitmapLayer;
  L: TFloatRect;
  I: Integer;
begin
  Image32.BeginUpdate;
  for X := 0 to 49 do
  begin
    // create a new layer...
    ALayer := TBitmapLayer.Create(Image32.Layers);
    with ALayer do
    begin
      Bitmap := BitmapList.Bitmaps[Random(BitmapList.Bitmaps.Count)].Bitmap;
      Bitmap.DrawMode := dmBlend;
      Bitmap.MasterAlpha := Random(255);

      // put it somethere
      L.Left := Random(Image32.Width);
      L.Top := Random(Image32.Height);
      L.Right := L.Left + Bitmap.Width;
      L.Bottom := L.Top + Bitmap.Height;
      ALayer.Location := L;

      I := Length(Velocities);
      SetLength(Velocities, I + 1);
      Velocities[I] := FloatPoint(Random - 0.5, Random - 0.5);
    end;
  end;
  Image32.EndUpdate;
  Image32.Changed;
  Edit1.Text := IntToStr(Image32.Layers.Count) + ' layers';
end;

procedure TForm1.IdleHandler(Sender: TObject; var Done: Boolean);
var
  I: Integer;
  R: TFloatRect;
begin
  if Image32.Layers.Count = 0 then Exit;
  Image32.BeginUpdate;
  for I := 0 to Image32.Layers.Count - 1 do
  begin
    with TBitmapLayer(Image32.Layers[I]) do
    begin
      Bitmap.MasterAlpha := (Bitmap.MasterAlpha + 1) mod 256;
      R := Location;
      with Velocities[I] do
      begin
        OffsetRect(R, X, Y);
        X := X + (Random - 0.5) * 0.1;
        Y := Y + (Random - 0.5) * 0.1;
        if (R.Left < 0) and (X < 0) then X := 1;
        if (R.Top < 0) and (Y < 0) then Y := 1;
        if (R.Right > Image32.Width) and (X > 0) then X := -1;
        if (R.Bottom > Image32.Height) and (Y > 0) then Y := -1;
      end;
      Location := R;
    end;
  end;
  Image32.EndUpdate;
  Image32.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image32.Layers.Clear;
  Velocities := nil;
  Edit1.Text := '0 layers';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnIdle := IdleHandler;
end;

end.
