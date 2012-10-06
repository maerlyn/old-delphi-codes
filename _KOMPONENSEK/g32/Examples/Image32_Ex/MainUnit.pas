unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, G32, G32_Image, G32_RangeBars;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image: TImage32;
    rgScaleMode: TRadioGroup;
    rgStretchFilter: TRadioGroup;
    rgBitmapAlign: TRadioGroup;
    StaticText1: TStaticText;
    sbScale: TGaugeBar;
    procedure rgBitmapAlignClick(Sender: TObject);
    procedure sbScaleChange(Sender: TObject);
    procedure rgScaleModeClick(Sender: TObject);
    procedure rgStretchFilterClick(Sender: TObject);
  public
    Time: Single;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.rgBitmapAlignClick(Sender: TObject);
const
  BA_CONSTS: array [0..2] of TBitmapAlign = (baTopLeft, baCenter, baTile);
begin
  Image.BitmapAlign := BA_CONSTS[rgBitmapAlign.ItemIndex];
end;

procedure TForm1.sbScaleChange(Sender: TObject);
begin
  sbScale.Update;
  Image.Scale := sbScale.Position / 100;
end;

procedure TForm1.rgScaleModeClick(Sender: TObject);
const
  SM_CONSTS: array [0..3] of TScaleMode = (smNormal, smStretch, smScale, smResize);
begin
  Image.ScaleMode := SM_CONSTS[rgScaleMode.ItemIndex];
  sbScale.Enabled := rgScaleMode.ItemIndex = 2;
  StaticText1.Enabled := rgScaleMode.ItemIndex = 2;
end;

procedure TForm1.rgStretchFilterClick(Sender: TObject);
const
  SF_CONSTS: array [0..3] of TStretchFilter =
    (sfNearest, sfLinear, sfLinear2, sfSpline);
begin
  Image.Bitmap.StretchFilter := SF_CONSTS[rgStretchFilter.ItemIndex];
end;

end.
