unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, Jpeg, ExeMod, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    OpenDialog1: TOpenDialog;
    Image1: TImage;
    SpeedButton3: TSpeedButton;
    Label1: TLabel;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     procedure AcceptFiles( var msg : TMessage ); 
              message WM_DROPFILES;
  end;

var
  Form1: TForm1;
  PicNumber: Integer;
  StoredPics: Integer;

implementation

{$R *.DFM}

uses
  ShellApi;

procedure TForm1.AcceptFiles( var msg : TMessage );
const
  cnMaxFileNameLen = 255;
var
  i,
  nCount     : integer;
  acFileName : array [0..cnMaxFileNameLen] of char;
begin
  nCount := DragQueryFile( msg.WParam,
                           $FFFFFFFF,
                           acFileName,
                           cnMaxFileNameLen );
  for i := 0 to nCount-1 do
  begin
      Label1.Visible := True;
      application.processmessages;
      DragQueryFile( msg.WParam, i, acFileName, cnMaxFileNameLen );
      AddFile2Exe('jpg'+IntToStr(PicNumber),acfilename);
      Inc(PicNumber);
      If PicNumber = 1001 then
      begin
        Showmessage('Limit of 1000 Images Has Been reached!'+chr(13)
        +'Please save your new image viewer Now');
        Label1.Visible := False;
        exit;
      end;
  end;
  DragFinish( msg.WParam );
  Label1.Visible := False;
  beep;
end;

procedure ShowPic(JpegNumber: Byte);
var temp: String;
PicScale,ScreenScale: Real;
MyStream: TMemoryStream;
MyPic: TJpegImage;
begin
ExtractFromExe('jpg'+IntToStr(PicNumber),Temp);
If Temp = '' Then Exit;
MyPic := TJpegImage.Create;
MyStream := TMemoryStream.Create;
try
String2Stream(temp,MyStream);
MyPic.LoadFromStream(MyStream);

Form1.Image1.Picture.Assign(MyPic);

PicScale := MyPic.Width/MyPic.Height;
ScreenScale := Screen.Width/Screen.Height;

Form1.Image1.Left := 0;
Form1.Image1.Top := 0;
Form1.Image1.Width := Screen.Width;
Form1.Image1.Height := Screen.Height;

if PicScale > ScreenScale  then
begin
  Form1.Image1.Width := Trunc((Screen.Width/MyPic.Width)*MyPic.Width);
  Form1.Image1.Height := Trunc((Screen.Width/MyPic.Width)*MyPic.Height);
  Form1.Image1.Top := (Screen.Height-Form1.Image1.Height)div 2;
end
else
begin
  Form1.Image1.Width := Trunc((Screen.Height/MyPic.Height)*MyPic.Width);
  Form1.Image1.Height := Trunc((Screen.Height/MyPic.Height)*MyPic.Height);
  Form1.Image1.Left := (Screen.Width-Form1.Image1.Width)div 2;
end;

finally
MyStream.Free;
MyPic.Free;
end;


end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
OpenDialog1.Execute;
If OpenDialog1.FileName <> '' then
begin
  AddFile2Exe('jpg'+IntToStr(PicNumber),OpenDialog1.FileName);
  Inc(PicNumber);
end;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
Var
Temp: String;
begin
If PicNumber = 1 Then Exit;
Temp := IntToStr(PicNumber-1);
Add2Exe('ImageCount',Temp);
AlterExe;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
Temp: String;
begin
PicNumber := 1;
DragAcceptFiles( Handle, True );
ExtractFromExe('ImageCount',Temp);
If Temp <> '' then
begin
  StoredPics := StrToInt(Temp);
  Form1.Color := clBlack;
  Form1.BorderStyle := bsNone;
  SpeedButton1.Visible := False;
  SpeedButton2.Visible := False;
  SpeedButton3.Visible := False;
  Form1.Left := 0;
  Form1.Top := 0;
  Form1.Width := Screen.Width;
  Form1.Height := Screen.Height;
  Image1.Top := 0;
  Image1.Left := 0;
  Image1.Width := Screen.Width;
  Image1.Height := Screen.Height;
  ShowPic(1);
end;
end;

procedure TForm1.Image1Click(Sender: TObject);
begin
  inc(PicNumber);
  if PicNumber <= StoredPics then
  begin
    ShowPic(PicNumber);
  end
  else
  begin
    PicNumber := 1;
    ShowPic(PicNumber);
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
If Key = chr(32) then
begin
  inc(PicNumber);
  if PicNumber <= StoredPics then
  begin
    ShowPic(PicNumber);
  end
  else
  begin
    PicNumber := 1;
    ShowPic(PicNumber);
  end;
end;
If Key = chr(8) then
begin
  dec(PicNumber);
  if PicNumber <> 0 then
  begin
    ShowPic(PicNumber);
  end
  else
  begin
    PicNumber := StoredPics;
    ShowPic(PicNumber);
  end;
end;
If Key = chr(27) then Application.Terminate;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
Temp: String;
begin
If PicNumber <> 1 then
begin
Label1.Visible := True;
application.processmessages;
Temp := IntToStr(PicNumber-1);
Add2Exe('ImageCount',Temp);
Exe2File('View.exe');
Exe := '';
PicNumber := 1;
Label1.Visible := False;
beep;
end;
end;

end.
