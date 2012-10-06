unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Egyenlo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    function  Hex2Dec(Hex: string):integer;
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
var i: integer;
    s,ss: string;
    k: integer;
    h: string;
    sl: TStringList;
begin
 sl := TStringList.Create;
 sl.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'data.in');
 s := '';
 ss := '';

{ for i := 0 to sl.Count-1 do
 begin
  s := sl[i];
  if length(s) <> 0 then
   if s[length(s)] = '=' then
   begin
    delete(s,length(s),1);
    ss := ss + s
   end
   else
    ss := ss + s + #13#10
  else
   ss := ss + #13#10;
 end;

 while pos('=',ss) > 0 do
 begin
  k := pos('=',ss);
  h := copy(ss,k+1,2);
  ss := copy(ss,1,k-1) +
        chr(Hex2Dec(h)) +
        copy(ss,k+3,length(ss));
 end;}

 ss := EgyenlosegTorles(sl.Text);

 Memo1.Lines.Text := ss;
 Memo1.Lines.SaveToFile(ExtractFilePath(ParamStr(0)) + 'data.out');

 sl.Free;
end;

function TForm1.Hex2Dec(Hex: string): integer;
begin
 if length(Hex) <> 2 then
  Abort;

 Hex := UpperCase(Hex);

 if Hex[1] = 'F' then
  Result := 16*15
 else if Hex[1] = 'E' then
  Result := 16*14
 else if Hex[1] = 'D' then
  Result := 16*13
 else if Hex[1] = 'C' then
  Result := 16*12
 else if Hex[1] = 'B' then
  Result := 16*11
 else if Hex[1] = 'A' then
  Result := 16*10
 else
  Result := 16*StrToInt(Hex[1]);

 if Hex[2] = 'F' then
  inc(Result,15)
 else if Hex[2] = 'E' then
  inc(Result,14)
 else if Hex[2] = 'D' then
  inc(Result,13)
 else if Hex[2] = 'C' then
  inc(Result,12)
 else if Hex[2] = 'B' then
  inc(Result,11)
 else if Hex[2] = 'A' then
  inc(Result,10)
 else
  inc(Result,StrToInt(Hex[2]));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 Edit1.Text := TargyAtalakitas(trim(Edit1.Text));
end;

end.
