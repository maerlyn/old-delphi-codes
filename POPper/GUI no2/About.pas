unit about;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, jpeg, ExtCtrls, Buttons, ShellAPI;

type
  TfrmAbout = class(TForm)
    Panel1: TPanel;
    Panel4: TPanel;
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    startx,starty : integer;
    dragable : boolean;
  public
    fh:THandle;
    ps:array [0..5] of Tpoint;
  end;

  TLangInfoBuffer = array[1..4] of smallint;

var
  frmAbout: TfrmAbout;

implementation

{$R *.DFM}

procedure TfrmAbout.FormCreate(Sender: TObject);
var VInfoSize, DetSize: dword;
    pVInfo, pDetail: pointer;
    MajorVer, MinorVer: integer;
    FileDescription: string;
    pLangInfo: ^TLangInfoBuffer;
    strLangID: string;
begin
 MajorVer := 0;
 MinorVer := 0;

 VInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)),DetSize);
 if VInfoSize > 0 then
 begin
  GetMem(pVInfo,VInfoSize);
  try
   GetFileVersionInfo(PChar(ParamStr(0)),0,VInfoSize,pVInfo);
   VerQueryValue(PVInfo,'\',pDetail,DetSize);
   with TVSFixedFileInfo(pDetail^) do
   begin
    MajorVer := HiWord(dwFileVersionMS);
    MinorVer := LoWord(dwFileVersionMS);
    VerQueryValue(pVInfo,'\VarFileInfo\Translation',pointer(pLangInfo),DetSize);
    strLangID := IntToHex(smallint(pLangInfo^[1]),4) + IntToHex(smallint(pLangInfo^[2]),4);
    strLangID := 'StringFileInfo\' + strLangID;
    VerQueryValue(pVInfo,PChar(strLangID + '\FileDescription'),pDetail,DetSize);
    FileDescription := PChar(pDetail);
   end;
  finally
   FreeMem(PVInfo);
  end;
 end;

 panel7.caption:='Névjegy - Putra POPper v' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + #32 + FileDescription;
 label1.caption:='Putra POPper v' + IntToStr(MajorVer) + '.' + IntToStr(MinorVer) + #32 + FileDescription;
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
  Self.left:=mouse.cursorpos.x - startx;
  Self.top:=mouse.cursorpos.y-starty;
 end;
end;

procedure TfrmAbout.SpeedButton1Click(Sender: TObject);
begin
 timer1.enabled:=false;
 self.hide;
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

procedure TfrmAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

end.
