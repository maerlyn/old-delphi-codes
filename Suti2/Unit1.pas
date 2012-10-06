unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, MMSystem;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
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
begin
 mciSendString('set cdaudio door open',nil,0,Handle);
 Application.MessageBox('Szólj, ha becsukhatom!','Süti',mb_Ok);
 mciSendString('set cdaudio door closed',nil,0,Handle);
 Application.Terminate;
end;

end.

