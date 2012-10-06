unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IniFiles, ShellAPI;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Label1: TLabel;
    Button14: TButton;
    ColorDialog1: TColorDialog;
    Button15: TButton;
    Button16: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    function Win: string;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TSzinek = packed record
    Hatter,
    Gombok,
    Fulek,
    Betuk: TColor;
  end;

var
  Form2: TForm2;
  Szinek: TSzinek;

implementation

uses Unit1;

{$R *.DFM}
{$R 'kepek.res'}

procedure TForm2.FormShow(Sender: TObject);
var Bmp: TBitmap;
    i: shortint;
    Cfg: TIniFile;
begin
 if Form1.Dir = '' then
 begin
  Application.MessageBox('Belsõ hiba!','CD-Ny skinner',mb_Ok + mb_IconAsterisk);
  Abort;
 end;

 if not Form1.Load then
 begin
  Bmp := TBitmap.Create;
  Bmp.LoadFromResourceName(hInstance,'bmp32x32');
  for i := 1 to 9 do
   Bmp.SaveToFile(Form1.Dir + '\' + IntToStr(i) + '.bmp');
  Bmp.SaveToFile(Form1.Dir + '\bg.bmp');
  Bmp.LoadFromResourceName(hInstance,'bmp16x16');
  Bmp.SaveToFile(Form1.Dir + '\kereses.bmp');
  Bmp.LoadFromResourceName(hInstance,'bmp18x18');
  Bmp.SaveToFile(Form1.Dir + '\ok.bmp');
  Bmp.SaveToFile(Form1.Dir + '\megse.bmp');
  Bmp.Free;
 end
 else
 begin
  Cfg := TIniFile.Create(Form1.Dir + '\skin.ini');
  Szinek.Betuk := Cfg.ReadInteger('Szinek','Betuk',0);
  Szinek.Fulek := Cfg.ReadInteger('Szinek','Fulek',0);
  Szinek.Gombok := Cfg.ReadInteger('Szinek','Gombok',0);
  Szinek.Hatter := Cfg.ReadInteger('Szinek','Hatter',0);
  Cfg.Free;
 end;
end;

procedure TForm2.Button14Click(Sender: TObject);
begin
 repeat
  Application.MessageBox('Kérem a háttér színét...','CD-Ny skinner',mb_Ok);
  ColorDialog1.Color := Szinek.Hatter;
 until ColorDialog1.Execute;
 Szinek.Hatter := ColorDialog1.Color;

 repeat
  Application.MessageBox('Kérem a gombok színét...','CD-Ny skinner',mb_Ok);
  ColorDialog1.Color := Szinek.Gombok;
 until ColorDialog1.Execute;
 Szinek.Gombok := ColorDialog1.Color;

 repeat
  Application.MessageBox('Kérem a fülek színét...','CD-Ny skinner',mb_Ok);
  ColorDialog1.Color := Szinek.Fulek;
 until ColorDialog1.Execute;
 Szinek.Fulek := ColorDialog1.COlor;

 repeat
  Application.MessageBox('És végül a betûk színét...','CD-Ny skinner',mb_Ok);
  ColorDialog1.Color := Szinek.Betuk;
 until ColorDialog1.Execute;
 Szinek.Betuk := ColorDialog1.Color;
end;

procedure TForm2.Button15Click(Sender: TObject);
var Cfg: TIniFile;
begin
 Cfg := TIniFile.Create(Form1.Dir + '\skin.ini');
 Cfg.WriteInteger('Szinek','Betuk',Szinek.Betuk);
 Cfg.WriteInteger('Szinek','Fulek',Szinek.Fulek);
 Cfg.WriteInteger('Szinek','Gombok',Szinek.Gombok);
 Cfg.WriteInteger('Szinek','Hatter',Szinek.Hatter);
 Cfg.Free;
end;

procedure TForm2.Button16Click(Sender: TObject);
begin
 Form1.Show;
 Form2.Hide;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
// ShellExecute(Application.Handle,'open',PChar(Form1.Dir + '\' + ((Sender as TButton).Caption)),'','',0);
 WinExec(PChar('C:\Program Files\Accessories\mspaint.exe '+Form1.Dir+'\'+((Sender as TButton).Caption)),sw_ShowMaximized);
end;

function TForm2.Win: string;
var strFolder: string;
    lngResult: longint;
    i: integer;
const MAX_PATH = 100;
begin
 strFolder := '';
 for i := 1 to MAX_PATH do strFolder := strFolder + #32;
 lngResult := GetWindowsDirectory(PChar(strFolder),MAX_PATH);
 if lngResult <> 0 then
  Result := trim(strFolder)
 else
  Result := '';
end;

end.
