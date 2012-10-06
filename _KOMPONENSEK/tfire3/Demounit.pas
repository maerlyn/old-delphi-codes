unit Demounit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, firewnd,ShellAPI;


type
  TForm1 = class(TForm)
    Bevel2: TBevel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Label8: TLabel;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Fire1: TFire;
    Image2: TImage;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label9: TLabel;
    Button1: TButton;
    CheckBox4: TCheckBox;
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
    procedure Fire1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Label9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Label9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    Bitmap:TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  MessageDlg('More properties avaliable.Each could be modified in designing time.',
    mtInformation,[mbOK],0);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  fire1.EditPalette;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
var s:string;i:Integer;
begin
  s:=InputBox ('Change MaxHeat','Input Integer value(between 0~400)', IntToStr(Fire1.MaxHeat));
  try
    i:=StrToInt(s);
  except
    ShowMessage(s+' is not a valid integer.');
    Exit;
  end;
  Fire1.MaxHeat:=i;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Fire1.Burning:=true;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Bitmap:=TBitmap.CReate;
  Bitmap.Assign(Fire1.Mask);
end;

procedure TForm1.RadioButton1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    Fire1.Mask:=Bitmap
  else
    fire1.Mask:=nil;
end;

procedure TForm1.Fire1Click(Sender: TObject);
begin
  Fire1.About;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if not CheckBox2.Checked then
    Fire1.MaskColor:=clWhite
  else
    Fire1.MaskColor:=clBlue;
    //note: Mask bitmap should NOT be monochrome,
    //if you want MaskColor take effect.


end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  Fire1.Transparent:=CheckBox3.Checked;
end;

procedure TForm1.Label9Click(Sender: TObject);
begin
  ShellExecute(0, Nil,'mailto:tqz@163.net', Nil, Nil, SW_NORMAL);

end;

procedure TForm1.Label9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Label9.Font.Color:=clRed;
end;

procedure TForm1.Label9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Label9.Font.Color:=clBlue;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  MessageDlg('You have pushed me!', mtInformation,[mbOK],0);
end;

end.
