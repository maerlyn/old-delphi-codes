unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExeMod, Buttons, ShellAPI;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
var temp: string;
begin
If paramstr(1) = '' then
begin
ExtractFromExe('FirstDateTime',Temp);
If Temp = '' then
Add2Exe('FirstDateTime', DateToStr(Date) + ' ' + TimeToStr(Time));
Exe := Exe + '[' + DateToStr(Date) + ' ' + TimeToStr(Time) + ']'+chr(13)+chr(10);
AlterExe;
end
else if ParamStr(1) = 'deltemp' then WindowState := WsNormal;
ExtractFromExe('FirstDateTime',Temp);
Form1.Caption := 'First Run Time ' + Temp + ' ->This Run Time '
 + DateToStr(Date) + ' ' + TimeToStr(Time);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
ReadExe;
Exe :=  Copy(Exe,(Pos(UpperCase('eo!#firstdatetime'),Exe))+17,
    Length(Exe)-(Pos(UpperCase('eo!#firstdatetime'),Exe))+17);
Exe2File('runtimes.txt');
Exe := '';
ShellExecute(0, 'open', PChar('runtimes.txt'),nil,nil, SW_SHOW);
end;

end.
