unit RGBALoaderUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, G32_Image, G32_Filters, ExtCtrls, ExtDlgs, Buttons;

type
  TRGBALoaderForm = class(TForm)
    Panel1: TPanel;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    ImgRGB: TImgView32;
    Button1: TButton;
    Label3: TLabel;
    ImgAlpha: TImgView32;
    Button2: TButton;
    Label4: TLabel;
    Button3: TButton;
    Button4: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Button5: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RGBALoaderForm: TRGBALoaderForm;

implementation

{$R *.DFM}

procedure TRGBALoaderForm.Button1Click(Sender: TObject);
begin
  with OpenPictureDialog do
    if Execute then ImgRGB.Bitmap.LoadFromFile(FileName);
end;

procedure TRGBALoaderForm.Button2Click(Sender: TObject);
begin
  with OpenPictureDialog, ImgAlpha do
    if Execute then
    begin
      Bitmap.LoadFromFile(FileName);
      ColorToGrayscale(Bitmap, Bitmap);
    end;
end;

procedure TRGBALoaderForm.SpeedButton1Click(Sender: TObject);
begin
  ImgRGB.Scale := ImgRGB.Scale * 1.5;
end;

procedure TRGBALoaderForm.SpeedButton2Click(Sender: TObject);
begin
  ImgRGB.Scale := ImgRGB.Scale / 1.5;
end;

procedure TRGBALoaderForm.SpeedButton3Click(Sender: TObject);
begin
  ImgAlpha.Scale := ImgAlpha.Scale * 1.5;
end;

procedure TRGBALoaderForm.SpeedButton4Click(Sender: TObject);
begin
  ImgAlpha.Scale := ImgAlpha.Scale / 1.5;
end;

procedure TRGBALoaderForm.Button5Click(Sender: TObject);
begin
  ImgRGB.Scale := 1;
  ImgAlpha.Scale := 1;
end;

end.
