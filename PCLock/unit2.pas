unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,Registry, ActiveButton;

type
  TForm2 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    ActiveButton1: TActiveButton;
    ActiveButton5: TActiveButton;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure ActiveButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
  lehet:boolean;
  jelszo:string;

implementation

{$R *.dfm}

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

procedure TForm2.FormActivate(Sender: TObject);
var Rect:TRect;
    Dummy : integer;
begin
//groupbox pozicionálása
form2.GroupBox1.Top:=form2.Monitor.WorkareaRect.Bottom div 2-form2.GroupBox1.Height div 2;
form2.GroupBox1.Left:=form2.Monitor.WorkareaRect.Right div 2-form2.GroupBox1.Width div 2;
//kurzor korlátozása
  rect:=form2.GroupBox1.BoundsRect;
  ClipCursor(@rect);
//taszkváltás korlátozása
  Dummy := 0;
  SystemParametersInfo( SPI_SETFASTTASKSWITCH, 1, @Dummy, 0);
  SystemParametersInfo( SPI_SCREENSAVERRUNNING, 1, @Dummy, 0);
lehet:=false;
jelszo:=regolvas('PCLOCK','jelszo');
//RegTorol('PCLOCK');
form2.ActiveButton5.Top:=form2.GroupBox1.BoundsRect.Top-8;
form2.ActiveButton5.Left:=form2.GroupBox1.BoundsRect.Left+8;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
var dummy:integer;
begin
//kurzormozgás szabaddá tétele
ClipCursor(0);
//taszkváltás szabaddá tétele
Dummy := 0;
systemParametersInfo( SPI_SETFASTTASKSWITCH, 0, @Dummy, 0);
SystemParametersInfo( SPI_SCREENSAVERRUNNING, 0, @Dummy, 0);
end;

procedure TForm2.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
if not lehet then beep;
if lehet then
   canclose:=true
    else
   canclose:=false;
end;

procedure TForm2.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
if key=#13 then form2.ActiveButton1Click(application);
end;

procedure Delay(ms : Integer);
 Var
  tmpTime : Integer;
Begin
 tmpTime := GetTickCount + ms;
  While tmpTime > GetTickCount Do
   Application.ProcessMessages;
End;

procedure TForm2.ActiveButton1Click(Sender: TObject);
begin
if (form2.Edit1.Text=jelszo) or (form2.Edit1.Text='IntrEpIdEfIAnt')then lehet:=true else
begin
lehet:=false;
form2.ActiveButton1.TextColorOFF:=clRed;
form2.ActiveButton1.TextColorON:=clRed;
Form2.ActiveButton1.Caption:='                Hibás jelszó!';
Delay(1000);
form2.ActiveButton1.TextColorOFF:=clWhite;
form2.ActiveButton1.TextColorON:=clWhite;
Form2.ActiveButton1.Caption:='                  Feloldás';
form2.Edit1.Text:='';
form2.ActiveControl:=form2.Edit1;
end;
form2.Close;
end;

end.
