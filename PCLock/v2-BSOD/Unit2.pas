unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ButtonWithColor;

type
  TForm2 = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Button1: TBitBtnWithColor;
    Button2: TBitBtnWithColor;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.FormCreate(Sender: TObject);
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
   CheckBox1.Checked := true;

  Edit1.Text := '';
  for i := 2 to size do
   Edit1.Text := Edit1.Text + Chr(buf[i]-1);

 except
  on E:Exception do
  begin
   Application.MessageBox('Hiba a konfigurációs file megnyitása során, alapállapot visszaállítása.','PCLock',mb_Ok + mb_IconError);
   Edit1.Text := 'ElfelejtettemAJelszot';
   CheckBox1.Checked := true;
  end;
 end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
 Application.Terminate;
end;

procedure TForm2.Button1Click(Sender: TObject);
var f: file of byte;
    size: byte;
    buf: array[1..101] of byte;
    i: byte;
    filename: string;
begin
 filename := ExtractFilePath(ParamStr(0)) + 'PCLock.ini';
 AssignFile(f,filename);
 try
  ReWrite(f);

  if CheckBox1.Checked then
   buf[1] := 10
  else
   buf[1] := 20;

  for i := 1 to Length(Edit1.Text) do
   buf[i+1] := Ord(Edit1.Text[i])+1;

  size := Length(Edit1.Text) + 1;

  BlockWrite(f,buf,size);

 except
  on E: Exception do
   Application.MessageBox('Hiba a beállítások mentése során, minden marad a régiben.','PCLock',mb_Ok + mb_IconError);
 end;
 Application.Terminate;
end;

end.
