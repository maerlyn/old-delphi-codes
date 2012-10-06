unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  szum: string;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var f: TextFile;
    s: string;
begin
 if not opendialog1.execute then
  abort;

 szum := '';

 assignfile(f,opendialog1.filename);
 reset(f);
 while not eof(f) do
 begin
  Readln(f,s);
  szum := szum + trim(s) + ' ';
 end;

 closefile(f);

 while pos(#13,szum) > 0 do
  delete(szum,pos(#13,szum),1);
 while pos(#10,szum) > 0 do
  delete(szum,pos(#10,szum),1);
end;

procedure TForm1.Button2Click(Sender: TObject);
var f: textfile;
begin
 if not savedialog1.execute then
  abort;

 assignfile(f,savedialog1.filename);
 rewrite(f);
  writeln(f,szum);
 closefile(f);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 opendialog1.initialdir := extractfilepath(paramstr(0));
 savedialog1.initialdir := extractfilepath(paramstr(0));
end;

end.
