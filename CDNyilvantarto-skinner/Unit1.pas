unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    fDir: string;
    fLoad: boolean;
  public
    property Dir: string read fDir write fDir;
    property Load: boolean read fLoad write fLoad default false;
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

procedure TForm1.Button3Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Text: string;
begin
  Text := 'C:\';
  if not SelectDirectory (Text, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
  begin
   Application.MessageBox('Ki kell választani egy könyvtárat!','CD-Ny skinner',mb_Ok + mb_IconInformation);
   Abort;
  end;

  if not DirectoryExists(Text) then
   ForceDirectories(Text);

  fDir := Text;
  fLoad := false;
  Form2.Show;
  Form1.Hide;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Text: string;
begin
  Text := 'C:\';
  if not SelectDirectory (Text, [sdPrompt], 0) then
  begin
   Application.MessageBox('Ki kell választani egy könyvtárat!','CD-Ny skinner',mb_Ok + mb_IconInformation);
   Abort;
  end;

 fLoad := true;
 fDir := Text;
 Form2.Show;
 Form1.Hide;
end;

end.
