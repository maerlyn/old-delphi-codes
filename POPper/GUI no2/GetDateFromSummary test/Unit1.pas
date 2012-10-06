unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Egyenlo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
 Memo1.Lines.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'summary.txt');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 label1.caption := DateTimeToStr(GetDateFromSummary(Memo1.Lines.Text));
end;

end.
