unit Unit1;
{
This short program shows how easy it is to add password access to
a delphi app using ExeMod. The program will not allow access until
the user has created a pw. Once a pw has been created then the exe
cannot be run without the user entering the pw. The pw is stored at
the end of the exe... here is what is added to the exe if the user
selects 'qwerty' as her pw.  SO!#MYPWRD¶qwertyEO!#MYPWRD  of course
there is very little security here but you could encrypt the pw that
is added to the exe and prevent a user from simply looking at the end
of the exe to copy the cleartext pw.
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExeMod, ExtCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
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
Var Temp: String;
begin
ExtractFromExe('MyPwrd',Temp);
If Temp = '' then Button1.Caption :=
'You Must Create A Password For This Program. Enter It Below And Click Here'
else
begin
  Button1.Caption :=
  'Enter Your Password Below And Click Here To Access Program';
  Form1.Color := $00aec2ff;
end;
end;

procedure TForm1.Button1Click(Sender: TObject);
Var Temp: String;
begin
If Button1.Caption[1] = 'Y' then
begin
  If Edit1.Text = '' then Exit;
  Add2Exe('MyPwrd',Edit1.Text); //add a pw to the exe
  AlterExe; //alter and restart the exe
end
else
begin
  ExtractFromExe('MyPwrd',Temp);
  If Temp = Edit1.Text Then
  begin
    Form1.Height := 350;
    Edit1.Visible := False;
    Button1.Visible := False;
    Image1.Visible := True;
    Label1.Visible := True;
    Exe := ''; //set exe string to empty string
  end;
end;
end;

end.
 