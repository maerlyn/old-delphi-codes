unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MMSystem;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
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

procedure TForm1.Button2Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm1.Button1Click(Sender: TObject);
var i: integer;
begin
 i := Application.MessageBox('A gép elejét kérem szabadon hagyni!','Süti',mb_IconWarning + mb_OkCancel);
 if i = id_Cancel then Exit;
 mciSendString('set cdaudio door open',nil,0,Handle);
 Application.MessageBox('Tessék!','Süti',mb_Ok);
 mciSendString('set cdaudio door closed',nil,0,Handle);
 Application.Terminate;
end;

end.

