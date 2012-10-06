unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, jpeg, ExtCtrls, Buttons, ShellAPI;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    Panel4: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Image2: TImage;
    Panel2: TPanel;
    Image1: TImage;
    Panel7: TPanel;
    Image4: TImage;
    SpeedButton1: TSpeedButton;
    Panel6: TPanel;
    ScrollBox1: TScrollBox;
    Label7: TLabel;
    Label5: TLabel;
    Image5: TImage;
    Label12: TLabel;
    Label11: TLabel;
    Label10: TLabel;
    Label9: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    Label14: TLabel;
    Image7: TImage;
    Label15: TLabel;
    Label16: TLabel;
    Panel8: TPanel;
    Timer1: TTimer;
    Panel3: TPanel;
    Panel9: TPanel;
    Label18: TLabel;
    Label19: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Panel7MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel7MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Panel7MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure Image7DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    fh:THandle;
    ps:array [0..5] of Tpoint;
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;
  startx,starty : integer;
  dragable : boolean;

implementation

{$R *.DFM}

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
 panel7.caption:='Névjegy - CD-Nyilvántartó v2.5';
 label2.caption:='v2.5'
end;

procedure TfrmAbout.Panel7MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 startx:=x;
 starty:=y;
 dragable:=true;
end;

procedure TfrmAbout.Panel7MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 dragable:=false;
end;

procedure TfrmAbout.Panel7MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
 if dragable then
 begin
  frmAbout.left:=mouse.cursorpos.x - startx;
  frmAbout.top:=mouse.cursorpos.y-starty;
 end;
end;

procedure TfrmAbout.SpeedButton1Click(Sender: TObject);
begin
 timer1.enabled:=false;
 close;
end;

procedure TfrmAbout.Panel3Click(Sender: TObject);
begin
 ShellExecute(Application.Handle,'open','http:\\putraware.ini.hu','','',SW_SHOWMAXIMIZED);
end;

procedure TfrmAbout.Image7DblClick(Sender: TObject);
begin
 panel7.Caption := 'Wake up Neo. The Matrix has you.';
end;

procedure TfrmAbout.Timer1Timer(Sender: TObject);
begin
 if panel8.Top < 232 then
  panel8.top := panel8.top+1
 else
  panel8.top := 8;
end;

procedure TfrmAbout.FormActivate(Sender: TObject);
begin
 timer1.enabled:=true;
end;

end.
