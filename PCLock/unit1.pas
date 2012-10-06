unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Registry, ActiveButton, ActiveLabel, TBIcn;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    ActiveButton1: TActiveButton;
    ActiveButton5: TActiveButton;
    ActiveButton2: TActiveButton;
    procedure GroupBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure ActiveButton1Click(Sender: TObject);
    procedure ActiveButton3Click(Sender: TObject);
    procedure ActiveButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  drag:boolean;
  epos:TPoint;

implementation

uses Unit2;

{$R *.dfm}

procedure RegIras(kulcsnev:string;erteknev:string;ertek:string);
var reg:TRegistry;
begin
reg:=TRegistry.Create;
reg.RootKey:=HKEY_CURRENT_USER;
reg.OpenKey(kulcsnev,true);
reg.WriteString(erteknev,ertek);
reg.CloseKey;
end;

function RegOlvas(kulcsnev:string;erteknev:string):string;
var reg:TRegistry;
begin
reg:=TRegistry.Create;
reg.RootKey:=HKEY_CURRENT_USER;
reg.OpenKey(kulcsnev,false);
result:=reg.ReadString(erteknev);
reg.CloseKey;
end;

procedure RegTorol(kulcsnev:string);
var reg:TRegistry;
begin
reg:=TRegistry.Create;
reg.RootKey:=HKEY_CURRENT_USER;
reg.DeleteKey(kulcsnev);
end;

procedure TForm1.GroupBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 ReleaseCapture;
 SendMessage(Form1.Handle, wm_SysCommand,$f012,0);
end;

procedure TForm1.FormActivate(Sender: TObject);
var r1,r2:HRGN;
begin
r1:=CreateRectRgn(
form1.GroupBox1.Left,
form1.ActiveButton2.Top-3,
form1.GroupBox1.Left+form1.GroupBox1.Width,
form1.GroupBox1.Top+form1.GroupBox1.Height);

r2:=CreateRectRgn(
0,
0,
form1.ActiveButton1.Left,
form1.ActiveButton1.Height div 2-4);

CombineRgn(r1,r1,r2,RGN_DIFF);

r2:=CreateRectRgn(
form1.ActiveButton5.Left+form1.ActiveButton5.Width,
form1.ActiveButton5.Top,
form1.ActiveButton2.Left,
form1.ActiveButton2.Height div 2-4);

CombineRgn(r1,r1,r2,RGN_DIFF);

r2:=CreateRectRgn(
form1.ActiveButton2.Left+form1.ActiveButton2.Width,
0,
form1.Width,
form1.GroupBox1.Top+5);


CombineRgn(r1,r1,r2,RGN_DIFF);

SetWindowRgn(form1.Handle,r1,true);
end;

procedure Delay(ms : Integer);
 Var
  tmpTime : Integer;
Begin
 tmpTime := GetTickCount + ms;
  While tmpTime > GetTickCount Do
   Application.ProcessMessages;
End;


procedure TForm1.ActiveButton1Click(Sender: TObject);
begin
if (form1.Edit1.Text=form1.Edit2.Text) then
  begin
   RegIras('PCLOCK','jelszo',form1.Edit1.text);
   form1.Hide;
   Form2.Showmodal;
   Application.Terminate;
  end
   else
  begin
   Form1.ActiveButton1.Caption:='     Hiba! A jelszavak nem egyeznek!';
   Form1.ActiveButton1.TextColorOFF:=clRed;
   Form1.ActiveButton1.TextColorON:=clRed;
   Delay(1000);
   Form1.ActiveButton1.TextColorOFF:=clWhite;
   Form1.ActiveButton1.TextColorON:=clWhite;
   form1.ActiveButton1.Caption:='                       Blokkolás';
  end;
end;

procedure TForm1.ActiveButton3Click(Sender: TObject);
begin
form1.WindowState:=wsMinimized;
end;

procedure TForm1.ActiveButton2Click(Sender: TObject);
begin
form1.Close;
end;

end.
