unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,Registry, ButtonWithColor, IniFiles, JPEG, ExtCtrls,
  MMSystem;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    ActiveButton1: TBitBtnWithColor;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure ActiveButton1Click(Sender: TObject);
    function JelszoOlvasasa: string;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  lehet:boolean;
  jelszo:string;
  uny: boolean;
  img: TJpegImage;
  begepelt: string = '';
  a: TResourceStream;
  rect: trect;

implementation

{$R *.dfm}
{$R 'images.res'}

procedure TForm1.FormActivate(Sender: TObject);
var Dummy : integer;
    i,k:integer;
begin
 Form1.Left := 0;
 Form1.Top := 0;
 Form1.Width := Screen.Width;
 Form1.Height := Screen.Height;
 Form1.GroupBox1.Top := Screen.Height div 2 - Form1.GroupBox1.Height div 2;
 Form1.GroupBox1.Left := Screen.Width div 2 - Form1.GroupBox1.Width div 2;
 rect := GroupBox1.BoundsRect;
// ClipCursor(@rect);
 Dummy := 0;
 SystemParametersInfo(SPI_SETFASTTASKSWITCH,1,@Dummy,0);
 SystemParametersInfo(SPI_SCREENSAVERRUNNING,1,@Dummy,0);
 lehet := false;
 jelszo := JelszoOlvasasa;

 GroupBox1.Visible := false;
 ActiveButton1.Visible := false;
 rect.Left := 1;
 rect.Top := 1;
 rect.Right := Screen.Width - 2;
 rect.Bottom := Screen.Height - 2;
 ClipCursor(@rect);
 img := TJpegImage.Create;
{ if Screen.Width = 800 then
  a := TResourceStream.Create(HInstance,'SVGA','CUSTOM')
 else
  a := TResourceStream.Create(HInstance,'XGA','CUSTOM');}

  img.LoadFromFile(ExtractFilePath(ParamStr(0)) + '\image.jpg');

 rect.TopLeft := point(0,0);
 rect.BottomRight := point(Screen.Width,Screen.Height);

 img.LoadFromStream(a);
 Form1.Canvas.StretchDraw(rect,img);

 Form1.Cursor := crNone;
 SetCursorPos(100,100);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var dummy: integer;
begin
//kurzormozgás engedélyezése
 ClipCursor(nil);
//taszkváltás engedélyezése
 Dummy := 0;
 systemParametersInfo(SPI_SETFASTTASKSWITCH,0,@Dummy,0);
 SystemParametersInfo(SPI_SCREENSAVERRUNNING,0,@Dummy,0);

 if FileExists(ExtractFilePath(ParamStr(0)) + 'sound.wav') then
 begin
  mciSendString(PChar('open '+ ExtractFilePath(ParamStr(0)) + 'sound.wav alias pclock'),nil,0,0);
  Application.Minimize;
  mciSendString('play pclock wait',nil,0,0);
  mciSendString('close pclock',nil,0,0);
 end;
 Application.Terminate;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if not lehet then beep;
 if lehet then
  canclose:=true
 else
  canclose:=false;
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
 if Key = #13 then
  ActiveButton1Click(Edit1);
end;

procedure Delay(ms : Integer);
var tmpTime: Integer;
Begin
 tmpTime := GetTickCount + ms;
 while tmpTime > GetTickCount do
  Application.ProcessMessages;
End;

procedure TForm1.ActiveButton1Click(Sender: TObject);
begin
 if (Edit1.Text = jelszo) or (Edit1.Text='ElfelejtettemAJelszot') then
  lehet:=true
 else
 begin
  lehet:=false;
  ActiveButton1.Font.Color := clRed;
  ActiveButton1.Caption:='Hibás jelszó!';
  Delay(1000);
  ActiveButton1.Font.Color := clLime;
  ActiveButton1.Caption:='Feloldás';
  Edit1.Text:='';
  Edit1.SetFocus;
 end;
 Form1.Close;
end;

function TForm1.JelszoOlvasasa: string;
var f: file of byte;
    buf: array[1..101] of byte;
    size: byte;
    filename: string;
    i: byte;
begin
 filename := ExtractFilePath(ParamStr(0)) + 'PCLock.ini';
 AssignFile(f,filename);
 try

  Reset(f);
  size := FileSize(f);
  BlockRead(f,buf,size);
  CloseFile(f);

  if buf[1] = 10 then
   uny := true
  else
   uny := false;

  jelszo := '';
  for i := 2 to size do
   jelszo := jelszo + Chr(buf[i]-1);

 except
  on E:Exception do
  begin
   Application.MessageBox('Hiba a konfigurációs file megnyitása során, alapállapotú üzem.','PCLock',mb_Ok + mb_IconError);
   jelszo := 'ElfelejtettemAJelszot';
   uny := true;
  end;
 end;

 Result := jelszo;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
var qqq: string;
    i: integer;
begin
 if (begepelt='')and(Key = jelszo[1])then
  begepelt := Key
 else
 if (pos(begepelt+key,jelszo)=1) then
  begepelt := begepelt + key;

 if begepelt = jelszo then
 begin
  lehet := true;
  Form1.Close;
 end;

 if Ord(Key) = VK_DELETE then
  begepelt := '';
 if Ord(Key) = VK_BACK then
 begin
  qqq := '';
  for i := 1 to length(begepelt) do
   qqq := qqq + begepelt[i];
  begepelt := qqq;
 end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
 Form1.Canvas.StretchDraw(rect,img);
end;

end.
