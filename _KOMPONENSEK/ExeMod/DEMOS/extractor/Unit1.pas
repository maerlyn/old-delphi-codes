unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ExeMod, Buttons, ShellAPI;

type
  TForm1 = class(TForm)
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SpeedButton3: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  Count: Integer;  //this variable holds the number of files added to the exe

implementation

{$R *.DFM}

//This procedure handles adding a file to an exe
//==============================================================================
procedure TForm1.SpeedButton1Click(Sender: TObject); 
Var Temp: String;
begin

if Exe = '' then ReadExe;    //These 2 lines make sure that a new file cannot
if Exe[3] <> 'P' then Exit;  //be added to a finalized exe..

OpenDialog1.Execute;
If OpenDialog1.FileName <> '' then
begin
  Add2Exe('YourFileName'+IntToStr(Count),ExtractFileName(OpenDialog1.FileName));
  AddFile2Exe('YourFile'+IntToStr(Count),OpenDialog1.FileName);
  Inc(Count);
end;
end;
//==============================================================================



//This procedure rebuilds the exe at runtime by adding all the files.
//==============================================================================
procedure TForm1.SpeedButton2Click(Sender: TObject);
Var Temp: String;
begin
ExtractFromExe('YourFileName0',Temp);
If Temp = '' then Exit;
Exe[3] := Chr(Count);
AlterExe;
end;
//==============================================================================



//This procedure extracts a finished archive to disk and views it with browser..
//the first file you stuffed into the exe is what will be executed so make
//certain you put the main page as the first saved file... usually this is
//index.htm/l  ... but it could be anything at all... including another exe :-)
//==============================================================================
procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
Temp: String;
X,Y: Byte;
begin
ReadExe;
If Exe[3] = 'P' then Exit;
Y := ord(Exe[3])-1;
For X := 0 to Y do
begin
  ExtractFromExe('YourFileName'+IntToStr(X),Temp);
  Extract2File('YourFile'+IntToStr(X),Temp);
end;
ExtractFromExe('YourFileName0',Temp);
ShellExecute(Form1.Handle,nil,pchar(Temp),
nil,nil, SW_SHOWNORMAL);
end;
//==============================================================================



//This procedure returns the running exe to its original state .. to activate
//this just double click somewhere on the form of either a finalized program
//or do it if you make a mistake while building an exe and want to start over.
//==============================================================================
procedure TForm1.FormDblClick(Sender: TObject);
Var
Temp: String;
X,Y: Byte;
begin
Y := ord(Exe[3])-1;
For X := 0 to Y do
begin
  DelFromString('YourFileName'+IntToStr(X),Exe);
  DelFromString('YourFile'+IntToStr(X),Exe);
  Exe[3] := 'P';
end;
AlterExe;
end;
//==============================================================================



//This procedure always runs first and it checks to see if the exe is a
//finalized ebook... if it is it alters the appearance of the form to reflect
//that fact :-)
//==============================================================================
procedure TForm1.FormCreate(Sender: TObject);
begin
ReadExe;
if Exe[3] <> 'P' then
begin
  Form1.Height := 85;
  SpeedButton3.Top := 14;
  Form1.Color := $00BFDABA;
  SpeedButton3.Visible := True;
  SpeedButton1.Visible := False;
  SpeedButton2.Visible := False;
end;
end;
//==============================================================================

end.
