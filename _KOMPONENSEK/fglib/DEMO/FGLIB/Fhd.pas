unit Fhd;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Spin, fglib, typefglib;

type
  THintSetting = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    BitBtn2: TBitBtn;
    GroupBox12: TGroupBox;
    BorderColor: TPanel;
    BitBtn12: TBitBtn;
    Border: TCheckBox;
    GroupBox2: TGroupBox;
    Memo1: TMemo;
    ColorDialog1: TColorDialog;
    Panel3: TPanel;
    TestBtn: TBitBtn;
    FontDialog1: TFontDialog;
    Image1: TImage;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Shadow: TCheckBox;
    Label2: TLabel;
    ShadowWidth: TSpinEdit;
    Label3: TLabel;
    ShadowIntensity: TSpinEdit;
    Label4: TLabel;
    ShadowQuality: TComboBox;
    Label6: TLabel;
    Color: TPanel;
    BitBtn3: TBitBtn;
    Panel13: TPanel;
    Picture: TImage;
    Label5: TLabel;
    BitBtn11: TBitBtn;
    HintStyle: TComboBox;
    LinkStyle: TComboBox;
    Position: TComboBox;
    BitBtn4: TBitBtn;
    font: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure Memo1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TestBtnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    CP : integer;
    procedure GetData;
    procedure SetData;
  end;

var
  HintSetting: THintSetting;

implementation

{$R *.DFM}


procedure THintSetting.FormShow(Sender: TObject);
var
  i : integer;
begin
  TestBtn.hint := memo1.text;
  for I:=0 to Application.MainForm.ComponentCount-1 do
  if Application.MainForm.Components[I] is THintDesign then CP := I;
  setdata;
end;

procedure THintSetting.Memo1Change(Sender: TObject);
begin
  TestBtn.hint := memo1.text;
end;

procedure THintSetting.GetData;
begin
  THintDesign(Application.MainForm.Components[CP]).Border := Border.checked;
  THintDesign(Application.MainForm.Components[CP]).BorderColor := BorderColor.color;
  THintDesign(Application.MainForm.Components[CP]).Shadow := Shadow.checked;
  THintDesign(Application.MainForm.Components[CP]).ShadowWidth := ShadowWidth.value;
  THintDesign(Application.MainForm.Components[CP]).ShadowIntensity := ShadowIntensity.value;
  case ShadowQuality.itemindex of
    0 : THintDesign(Application.MainForm.Components[CP]).ShadowQuality := quLow;
    1 : THintDesign(Application.MainForm.Components[CP]).ShadowQuality := quHi;
  end;
  THintDesign(Application.MainForm.Components[CP]).Color := Color.color;
  case HintStyle.itemindex of
    0 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiBubble;
    1 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiImage;
    2 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiRectangle;
    3 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiRoundrect;
    4 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiText;
    5 : THintDesign(Application.MainForm.Components[CP]).HintStyle := hiTexture;
  end;
  case LinkStyle.itemindex of
    0 : THintDesign(Application.MainForm.Components[CP]).LinkStyle := liArrow;
    1 : THintDesign(Application.MainForm.Components[CP]).LinkStyle := liBubble;
    2 : THintDesign(Application.MainForm.Components[CP]).LinkStyle := liNone;
  end;
  case Position.itemindex of
    0 : THintDesign(Application.MainForm.Components[CP]).Position := hiBottomLeft;
    1 : THintDesign(Application.MainForm.Components[CP]).Position := hiBottomRight;
    2 : THintDesign(Application.MainForm.Components[CP]).Position := hiTopLeft;
    3 : THintDesign(Application.MainForm.Components[CP]).Position := hiTopRight;
  end;
  THintDesign(Application.MainForm.Components[CP]).picture.bitmap.assign(Picture.Picture.bitmap);
  THintDesign(Application.MainForm.Components[CP]).font.assign(font.font);
end;

procedure THintSetting.SetData;
begin
  Border.checked := THintDesign(Application.MainForm.Components[CP]).Border;
  BorderColor.color := THintDesign(Application.MainForm.Components[CP]).BorderColor;
  Shadow.checked := THintDesign(Application.MainForm.Components[CP]).Shadow;
  ShadowWidth.value := THintDesign(Application.MainForm.Components[CP]).ShadowWidth;
  ShadowIntensity.value := THintDesign(Application.MainForm.Components[CP]).ShadowIntensity;
  case THintDesign(Application.MainForm.Components[CP]).ShadowQuality of
    quLow : ShadowQuality.itemindex := 0;
    quHi : ShadowQuality.itemindex := 1;
  end;
  Color.color := THintDesign(Application.MainForm.Components[CP]).Color;
  case THintDesign(Application.MainForm.Components[CP]).HintStyle of
    hiBubble    : HintStyle.itemindex := 0;
    hiImage     : HintStyle.itemindex := 1;
    hiRectangle : HintStyle.itemindex := 2;
    hiRoundrect : HintStyle.itemindex := 3;
    hiText      : HintStyle.itemindex := 4;
    hiTexture   : HintStyle.itemindex := 5;
  end;
  case THintDesign(Application.MainForm.Components[CP]).LinkStyle of
    liArrow  : LinkStyle.itemindex := 0;
    liBubble : LinkStyle.itemindex := 1;
    liNone   : LinkStyle.itemindex := 2;
  end;
  case THintDesign(Application.MainForm.Components[CP]).Position of
    hiBottomLeft  : Position.itemindex := 0;
    hiBottomRight : Position.itemindex := 1;
    hiTopLeft     : Position.itemindex := 2;
    hiTopRight    : Position.itemindex := 3;
  end;
  Picture.Picture.bitmap.assign(THintDesign(Application.MainForm.Components[CP]).picture.bitmap);
  font.font.assign(THintDesign(Application.MainForm.Components[CP]).font);
  BitBtn4.caption := font.font.name+', '+inttostr(font.font.size);
end;

procedure THintSetting.TestBtnMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  GetData;
end;

procedure THintSetting.BitBtn12Click(Sender: TObject);
begin
  ColorDialog1.color := BorderColor.color;
  if ColorDialog1.execute then BorderColor.color := ColorDialog1.color;
end;

procedure THintSetting.BitBtn3Click(Sender: TObject);
begin
  ColorDialog1.color := Color.color;
  if ColorDialog1.execute then Color.color := ColorDialog1.color;
end;

procedure THintSetting.BitBtn11Click(Sender: TObject);
begin
  if OpenDialog1.execute then Picture.picture.loadfromfile(OpenDialog1.filename);
end;

procedure THintSetting.BitBtn4Click(Sender: TObject);
begin
  FontDialog1.font := font.font;
  if FontDialog1.execute then font.font := FontDialog1.font;
  BitBtn4.caption := font.font.name+', '+inttostr(font.font.size);
  getdata;
  THintDesign(Application.MainForm.Components[CP]).reset;
end;

procedure THintSetting.BitBtn2Click(Sender: TObject);
begin
  GetData;
end;

end.
