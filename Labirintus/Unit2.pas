unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, Menus;

type
  TfrmPalyaszerkeszto = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    cmbPalya: TComboBox;
    MainMenu1: TMainMenu;
    mnuMentes: TMenuItem;
    mnuUjpalya: TMenuItem;
    mnuAktualisTorlese: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure cmbPalyaChange(Sender: TObject);
    procedure UjraRajzolas;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuMentesClick(Sender: TObject);
    procedure mnuUjpalyaClick(Sender: TObject);
    procedure mnuAktualisTorleseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPalyaszerkeszto: TfrmPalyaszerkeszto;
  Map: array[1..20,1..20] of char;

  kep_nothing, kep_wall, kep_manus, kep_finish, kep_manusacelban: TBitmap;
  kep_raketa_d, kep_raketa_l, kep_raketa_r, kep_raketa_u: TBitmap;

  VanManus, VanCel: boolean;

implementation

{$R *.DFM}
//{$R kepek.RES}

procedure TfrmPalyaszerkeszto.FormCreate(Sender: TObject);
var Cfg: TIniFile;
    i: integer;
begin
 Cfg := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Labirintus.dat');
 for i := 1 to Cfg.ReadInteger('General Info','NumberOfLevels',1) do
  if Cfg.SectionExists('Level' + IntToStr(i)) then
   cmbPalya.Items.Add('#' + IntToStr(i));
 Cfg.Free;

 kep_nothing := TBitmap.Create;
 kep_wall := TBitmap.Create;
 kep_manus := TBitmap.Create;
 kep_finish := TBitmap.Create;
 kep_manusacelban := TBitmap.Create;

 kep_nothing.LoadFromResourceName(hInstance, 'kep_nothing');
 kep_wall.LoadFromResourceName(hInstance, 'kep_wall');
 kep_manus.LoadFromResourceName(hInstance, 'kep_manus');
 kep_finish.LoadFromResourceName(hInstance, 'kep_finish');
 kep_manusacelban.LoadFromResourceName(hInstance, 'kep_manusacelban');

 cmbPalya.ItemIndex := 0;
 cmbPalyaChange(Sender);
end;

procedure TfrmPalyaszerkeszto.cmbPalyaChange(Sender: TObject);
var temp: string;
    Cfg: TIniFile;
    x, y: integer;
begin
 temp := cmbPalya.Items[cmbPalya.ItemIndex];
 temp := Copy(temp,2,Length(temp));
 Cfg := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Labirintus.dat');
 VanManus := false;
 VanCel := false;
 for x := 1 to 20 do
  for y := 1 to 20 do
  begin
   Map[x,y] := Cfg.ReadString('Level' + temp,'sor' + IntToStr(x),'--------------------')[y];
   if Map[x,y] = 'm' then
    VanManus := true;
   if Map[x,y] = 'f' then
    VanCel := true;
  end;
 Cfg.Free;
 UjraRajzolas;
end;

procedure TfrmPalyaszerkeszto.UjraRajzolas;
var x, y: integer;
begin
 for x := 1 to 20 do
  for y := 1 to 20 do
  begin
   if Map[x,y] = '-' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_nothing);
   if Map[x,y] = '+' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_wall);
   if Map[x,y] = 'm' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_manus);
   if Map[x,y] = 'f' then Image1.Canvas.Draw(y * 10 - 10, x * 10 - 10, kep_finish);
  end;
end;

procedure TfrmPalyaszerkeszto.Image1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var HanyadikX, HanyadikY: integer;
begin
 HanyadikX := trunc(Y / 10) + 1;
 HanyadikY := trunc(X / 10) + 1;
 case Map[HanyadikX,HanyadikY] of
  '-' : Map[HanyadikX,HanyadikY] := '+';
  '+' : begin
         if VanManus then
          if VanCel then
           Map[HanyadikX,HanyadikY] := '-'
          else
          begin
           Map[HanyadikX,HanyadikY] := 'f';
           VanCel := true;
          end
          else
          begin
           Map[HanyadikX,HanyadikY] := 'm';
           VanManus := true;
          end;
         end;
  'm' : if VanCel then
        begin
         Map[HanyadikX,HanyadikY] := '-';
         VanManus := false;
        end
        else
        begin
         Map[HanyadikX,HanyadikY] := 'f';
         VanCel := true;
        end;
  'f' : begin
         Map[HanyadikX,HanyadikY] := '-';
         VanCel := false;
        end;
 end;       

 UjraRajzolas;
end;

procedure TfrmPalyaszerkeszto.mnuMentesClick(Sender: TObject);
var x, y: integer;
    Cfg: TIniFile;
    temp: string;
    temp2: array[1..20] of string;
begin
 Cfg := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Labirintus.dat');
 temp := cmbPalya.Items[cmbPalya.ItemIndex];
 temp := Copy(temp,2,Length(temp));

 for x := 1 to 20 do
  for y := 1 to 20 do
   temp2[x] := temp2[x] + Map[x,y];

 for x := 1 to 20 do
  Cfg.WriteString('Level' + temp,'sor' + IntToStr(x),temp2[x]);

 Cfg.Free;
end;

procedure TfrmPalyaszerkeszto.mnuUjpalyaClick(Sender: TObject);
var i,x,y: integer;
    Cfg: TIniFile;
begin
 Cfg := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Labirintus.dat');
 i := Cfg.ReadInteger('General Info','NumberOfLevels',1);
 inc(i);
 Cfg.WriteInteger('General Info','NumberOfLevels',i);
 cmbPalya.Items.Add('#' + IntToStr(i));
 cmbPalya.ItemIndex := cmbPalya.Items.IndexOf('#' + IntToStr(i));
 Cfg.Free;

 for x := 1 to 20 do
  for y := 1 to 20 do
   Map[x,y] := '-';

 Map[1,1] := 'm';
 Map[20,20] := 'f';
 VanManus := true;
 VanCel := true;
 UjraRajzolas;
end;

procedure TfrmPalyaszerkeszto.mnuAktualisTorleseClick(Sender: TObject);
var i: integer;
    Cfg: TIniFile;
begin
 if Application.MessageBox(PCHar('Tényleg törlöd az aktuális (' + cmbPalya.Items[cmbPalya.ItemIndex] + ') pályát?'),'Labirintus',mb_YesNo + mb_IconQuestion) <> id_Yes then
  Abort;
 if Application.MessageBox(PChar('Igazán biztos vagy benne, hogy törlöd a ' + cmbPalya.Items[cmbPalya.ItemIndex] + '. pályát?'),'Labirintus',mb_YesNo + mb_IconQuestion) <> id_Yes then
  Abort;
 if Application.MessageBox('De ha törlöd, akkor nem tudsz majd annyit játszani a kevesebb pályával! Mégis törlöd?','Labirintus',mb_YesNo + mb_IconWarning) <> id_Yes then
  Abort;
 Application.MessageBox('Na jó, ha ennyire akarod... :(((((','Labirintus',mb_Ok);


 i := StrToInt(Copy(cmbPalya.Items[cmbPalya.ItemIndex],2,2000));
 Cfg := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'Labirintus.dat');
 Cfg.EraseSection('Level' + IntToStr(i));
 Cfg.Free;
end;

end.

