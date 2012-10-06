unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFoForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    Image15: TImage;
    Image16: TImage;
    Image17: TImage;
    Image18: TImage;
    Image19: TImage;
    Image20: TImage;
    Image21: TImage;
    Image22: TImage;
    Image23: TImage;
    Image24: TImage;
    cmbKorongokSzama: TComboBox;
    cmdUjjatek: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure UjJatek;
    procedure cmdUjjatekClick(Sender: TObject);
    procedure Kattintas(X, Y: integer);
    function Legfelso(X, Y: integer): boolean;
    procedure MapFeltoltes;
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure Image8Click(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure Image10Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure Image13Click(Sender: TObject);
    procedure Image14Click(Sender: TObject);
    procedure Image15Click(Sender: TObject);
    procedure Image16Click(Sender: TObject);
    procedure Image17Click(Sender: TObject);
    procedure Image18Click(Sender: TObject);
    procedure Image19Click(Sender: TObject);
    procedure Image20Click(Sender: TObject);
    procedure Image21Click(Sender: TObject);
    procedure Image22Click(Sender: TObject);
    procedure Image23Click(Sender: TObject);
    procedure Image24Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FoForm: TFoForm;
  ImageObjektumok: array[1..3,1..8] of TImage;
  Map: array[1..3,1..8] of string;
  Kepek: array[1..16] of TBitmap;
  Kijelolve: array[1..3,1..8] of boolean;
  VanKijelolt: boolean;
  MelyikVanKijelolve:  TPoint;

const
  korong1 = 11;
  korong2 = 12;
  korong3 = 13;
  korong4 = 14;
  korong5 = 15;
  korong6 = 16;
  korong7 = 17;

implementation

{$R *.DFM}
{$R kepek.res}

procedure TFoForm.FormCreate(Sender: TObject);
var i: integer;
begin
 ImageObjektumok[1,8] := Image1;
 ImageObjektumok[1,7] := Image2;
 ImageObjektumok[1,6] := Image3;
 ImageObjektumok[1,5] := Image4;
 ImageObjektumok[1,4] := Image5;
 ImageObjektumok[1,3] := Image6;
 ImageObjektumok[1,2] := Image7;
 ImageObjektumok[1,1] := Image8;
 ImageObjektumok[2,8] := Image9;
 ImageObjektumok[2,7] := Image10;
 ImageObjektumok[2,6] := Image11;
 ImageObjektumok[2,5] := Image12;
 ImageObjektumok[2,4] := Image13;
 ImageObjektumok[2,3] := Image14;
 ImageObjektumok[2,2] := Image15;
 ImageObjektumok[2,1] := Image16;
 ImageObjektumok[3,8] := Image17;
 ImageObjektumok[3,7] := Image18;
 ImageObjektumok[3,6] := Image19;
 ImageObjektumok[3,5] := Image20;
 ImageObjektumok[3,4] := Image21;
 ImageObjektumok[3,3] := Image22;
 ImageObjektumok[3,2] := Image23;
 ImageObjektumok[3,1] := Image24;
 for i := 1 to 16 do Kepek[i] := TBitmap.Create;
 for i := 1 to 7 do
 begin
  Kepek[i].LoadFromResourceName(HInstance,'disk' + IntToStr(i));
  Kepek[i + 7].LoadFromResourceName(HInstance,'disk' + IntToStr(i) + 'h');
 end;
 Kepek[15].LoadFromResourceName(HInstance,'post');
 Kepek[16].LoadFromResourceName(HInstance,'posttop');

 ImageObjektumok[1,8].Picture.Bitmap := Kepek[16];
 ImageObjektumok[2,8].Picture.Bitmap := Kepek[16];
 ImageObjektumok[3,8].Picture.Bitmap := Kepek[16];
 for i := 7 downto 1 do
 begin
  ImageObjektumok[1,i].Picture.Bitmap := Kepek[15];ImageObjektumok[1,i].Tag := 0;
  ImageObjektumok[2,i].Picture.Bitmap := Kepek[15];ImageObjektumok[2,i].Tag := 0;
  ImageObjektumok[3,i].Picture.Bitmap := Kepek[15];ImageObjektumok[3,i].Tag := 0;
 end;
 cmbKorongokSzama.ItemIndex := 0;

 ImageObjektumok[2,1].Picture.Bitmap := Kepek[5];ImageObjektumok[2,1].Tag := 5;
 ImageObjektumok[2,2].Picture.Bitmap := Kepek[6];ImageObjektumok[2,2].Tag := 6;
 ImageObjektumok[2,3].Picture.Bitmap := Kepek[7];ImageObjektumok[2,3].Tag := 7;
 MapFeltoltes;
end;

procedure TFoForm.UjJatek;
var i, j: integer;
begin
 ImageObjektumok[1,8].Picture.Bitmap := Kepek[16];
 ImageObjektumok[2,8].Picture.Bitmap := Kepek[16];
 ImageObjektumok[3,8].Picture.Bitmap := Kepek[16];
 for i := 7 downto 1 do
 begin
  ImageObjektumok[1,i].Picture.Bitmap := Kepek[15];
  ImageObjektumok[2,i].Picture.Bitmap := Kepek[15];
  ImageObjektumok[3,i].Picture.bitmap := Kepek[15];
 end;
 for i := 1 to 3 do for j := 1 to 8 do ImageObjektumok[i,j].Tag := 0;
 if cmbKorongokSzama.Text = '3' then begin
                                     ImageObjektumok[2,1].Picture.Bitmap := Kepek[5];ImageObjektumok[2,1].Tag := 5;
                                     ImageObjektumok[2,2].Picture.Bitmap := Kepek[6];ImageObjektumok[2,2].Tag := 6;
                                     ImageObjektumok[2,3].Picture.Bitmap := Kepek[7];ImageObjektumok[2,3].Tag := 7;
                                     end
 else if cmbKorongokSzama.Text = '4' then begin
                                          ImageObjektumok[2,1].Picture.Bitmap := Kepek[4];ImageObjektumok[2,1].Tag := 4;
                                          ImageObjektumok[2,2].Picture.Bitmap := Kepek[5];ImageObjektumok[2,2].Tag := 5;
                                          ImageObjektumok[2,3].Picture.Bitmap := Kepek[6];ImageObjektumok[2,3].Tag := 6;
                                          ImageObjektumok[2,4].Picture.Bitmap := Kepek[7];ImageObjektumok[2,4].Tag := 7;
                                          end
 else if cmbKorongokSzama.Text = '5' then begin
                                          ImageObjektumok[2,1].Picture.Bitmap := Kepek[3];ImageObjektumok[2,1].Tag := 3;
                                          ImageObjektumok[2,2].Picture.Bitmap := Kepek[4];ImageObjektumok[2,2].Tag := 4;
                                          ImageObjektumok[2,3].Picture.Bitmap := Kepek[5];ImageObjektumok[2,3].Tag := 5;
                                          ImageObjektumok[2,4].Picture.Bitmap := Kepek[6];ImageObjektumok[2,4].Tag := 6;
                                          ImageObjektumok[2,5].Picture.Bitmap := Kepek[7];ImageObjektumok[2,5].Tag := 7;
                                          end
 else if cmbKorongokSzama.Text = '6' then begin
                                          ImageObjektumok[2,1].Picture.Bitmap := Kepek[2];ImageObjektumok[2,1].Tag := 2;
                                          ImageObjektumok[2,2].Picture.Bitmap := Kepek[3];ImageObjektumok[2,2].Tag := 3;
                                          ImageObjektumok[2,3].Picture.Bitmap := Kepek[4];ImageObjektumok[2,3].Tag := 4;
                                          ImageObjektumok[2,4].Picture.Bitmap := Kepek[5];ImageObjektumok[2,4].Tag := 5;
                                          ImageObjektumok[2,5].Picture.Bitmap := Kepek[6];ImageObjektumok[2,5].Tag := 6;
                                          ImageObjektumok[2,6].Picture.Bitmap := Kepek[7];ImageObjektumok[2,6].Tag := 7;
                                          end
 else if cmbKorongokSzama.Text = '7' then begin
                                          ImageObjektumok[2,1].Picture.Bitmap := Kepek[1];ImageObjektumok[2,1].Tag := 1;
                                          ImageObjektumok[2,2].Picture.Bitmap := Kepek[2];ImageObjektumok[2,2].Tag := 2;
                                          ImageObjektumok[2,3].Picture.Bitmap := Kepek[3];ImageObjektumok[2,3].Tag := 3;
                                          ImageObjektumok[2,4].Picture.Bitmap := Kepek[4];ImageObjektumok[2,4].Tag := 4;
                                          ImageObjektumok[2,5].Picture.Bitmap := Kepek[5];ImageObjektumok[2,5].Tag := 5;
                                          ImageObjektumok[2,6].Picture.Bitmap := Kepek[6];ImageObjektumok[2,6].Tag := 6;
                                          ImageObjektumok[2,7].Picture.Bitmap := Kepek[7];ImageObjektumok[2,7].Tag := 7;
                                          end;
end;

procedure TFoForm.cmdUjjatekClick(Sender: TObject);
begin
 UjJatek;
end;

procedure TFoForm.Kattintas(X, Y: integer);
begin
 if ImageObjektumok[X,Y].Tag = 0 then Exit;
 if not Legfelso(X,Y) then Exit;
 Kijelolve[X,Y] := not Kijelolve[X,Y];
 if Kijelolve[X,Y] then
 begin
  if ImageObjektumok[X,Y].Tag = 1 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[8];
  if ImageObjektumok[X,Y].Tag = 2 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[9];
  if ImageObjektumok[X,Y].Tag = 3 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[10];
  if ImageObjektumok[X,Y].Tag = 4 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[11];
  if ImageObjektumok[X,Y].Tag = 5 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[12];
  if ImageObjektumok[X,Y].Tag = 6 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[13];
  if ImageObjektumok[X,Y].Tag = 7 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[14];
 end
 else
 begin
  if ImageObjektumok[X,Y].Tag = korong1 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[1];
  if ImageObjektumok[X,Y].Tag = korong2 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[2];
  if ImageObjektumok[X,Y].Tag = korong3 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[3];
  if ImageObjektumok[X,Y].Tag = korong4 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[4];
  if ImageObjektumok[X,Y].Tag = korong5 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[5];
  if ImageObjektumok[X,Y].Tag = korong6 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[6];
  if ImageObjektumok[X,Y].Tag = korong7 then ImageObjektumok[X,Y].Picture.Bitmap := Kepek[7];
 end;
end;

function TFoForm.Legfelso(X, Y: integer): boolean;
begin
  if ImageObjektumok[X,Y + 1].Tag = 0 then
   Legfelso := true
  else
   Legfelso := false;
end;

procedure TFoForm.MapFeltoltes;
var i, j: integer;
begin
 for i := 1 to 3 do
  for j := 1 to 8 do
  begin
   if ImageObjektumok[i,j].Tag = 0 then Map[i,j] := '-';
   if ImageObjektumok[i,j].Tag = 1 then Map[i,j] := 'korong1';
   if ImageObjektumok[i,j].Tag = 2 then Map[i,j] := 'korong2';
   if ImageObjektumok[i,j].Tag = 3 then Map[i,j] := 'korong3';
   if ImageObjektumok[i,j].Tag = 4 then Map[i,j] := 'korong4';
   if ImageObjektumok[i,j].Tag = 5 then Map[i,j] := 'korong5';
   if ImageObjektumok[i,j].Tag = 6 then Map[i,j] := 'korong6';
   if ImageObjektumok[i,j].Tag = 7 then Map[i,j] := 'korong7';
  end;
end;

procedure TFoForm.Image1Click(Sender: TObject);
begin
 Kattintas(1,8);
end;

procedure TFoForm.Image2Click(Sender: TObject);
begin
 Kattintas(1,7);
end;

procedure TFoForm.Image3Click(Sender: TObject);
begin
 Kattintas(1,6);
end;

procedure TFoForm.Image4Click(Sender: TObject);
begin
 Kattintas(1,5);
end;

procedure TFoForm.Image5Click(Sender: TObject);
begin
 Kattintas(1,4);
end;

procedure TFoForm.Image6Click(Sender: TObject);
begin
 Kattintas(1,3);
end;

procedure TFoForm.Image7Click(Sender: TObject);
begin
 Kattintas(1,2);
end;

procedure TFoForm.Image8Click(Sender: TObject);
begin
 Kattintas(1,1);
end;

procedure TFoForm.Image9Click(Sender: TObject);
begin
 Kattintas(2,8);
end;

procedure TFoForm.Image10Click(Sender: TObject);
begin
 Kattintas(2,7);
end;

procedure TFoForm.Image11Click(Sender: TObject);
begin
 Kattintas(2,6);
end;

procedure TFoForm.Image12Click(Sender: TObject);
begin
 Kattintas(2,5);
end;

procedure TFoForm.Image13Click(Sender: TObject);
begin
 Kattintas(2,4);
end;

procedure TFoForm.Image14Click(Sender: TObject);
begin
 Kattintas(2,3);
end;

procedure TFoForm.Image15Click(Sender: TObject);
begin
 Kattintas(2,2);
end;

procedure TFoForm.Image16Click(Sender: TObject);
begin
 Kattintas(2,1);
end;

procedure TFoForm.Image17Click(Sender: TObject);
begin
 Kattintas(3,8);
end;

procedure TFoForm.Image18Click(Sender: TObject);
begin
 Kattintas(3,7);
end;

procedure TFoForm.Image19Click(Sender: TObject);
begin
 Kattintas(3,6);
end;

procedure TFoForm.Image20Click(Sender: TObject);
begin
 Kattintas(3,5);
end;

procedure TFoForm.Image21Click(Sender: TObject);
begin
 Kattintas(3,4);
end;

procedure TFoForm.Image22Click(Sender: TObject);
begin
 Kattintas(3,3);
end;

procedure TFoForm.Image23Click(Sender: TObject);
begin
 Kattintas(3,2);
end;

procedure TFoForm.Image24Click(Sender: TObject);
begin
 Kattintas(3,1);
end;

end.


