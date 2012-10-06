unit Teszt;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Kerdes, MainForm, IniFiles, StdCtrls, Eredmeny;

type
  TfrmTeszt = class(TForm)
    Button1: TButton;
    txtEvszam: TEdit;
    txtEsemeny: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure UjTeszt;
    procedure UjKerdes;
    procedure Delay(Seconds, MilliSec: word);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TKerdes = record
    Letezik: boolean;
    Evszam: string;
    Esemeny: string;
  end;

var
  frmTeszt: TfrmTeszt;
  DBFile: TIniFile;
  KerdesSzama: integer;
  Kerdesek: array[1..65535] of TKerdes;
  KerdesekSzama: integer;
  LetezoKerdesekSzama: integer;
  HanyKerdesLegyen: integer;
  Pontok: integer;
  MarVoltKerdesek: string;
  HanyKerdesVolt: integer;
  Mode: integer;

implementation

{$R *.DFM}

{ TfrmTeszt }

procedure TfrmTeszt.Delay(Seconds, MilliSec: word);
var TimeOut: TDateTime;
begin
 TimeOut := Now + EncodeTime(0,Seconds div 60,Seconds mod 60,MilliSec);
 while Now < TimeOut do
  Application.ProcessMessages;
end;

procedure TfrmTeszt.UjKerdes;
begin
 Button1.Caption := '&OK';
 repeat
  KerdesSzama := trunc(Random(KerdesekSzama) + 1);
 until ((Kerdesek[KerdesSzama].Letezik = true)and(Pos(',' + IntToStr(KerdesSzama) + ',',MarVoltKerdesek) <= 0));
 MarVoltKerdesek := MarVoltKerdesek + ',' + IntToStr(KerdesSzama) + ',';
 if Mode = 1 then
 begin                                 //én:évsz.
  txtEvszam.Text := Kerdesek[KerdesSzama].Evszam;
  try
   txtEsemeny.Clear;
   txtEsemeny.SetFocus;
  except
  end;
 end
 else
 begin                                 //én:esemény
  txtEsemeny.Text := Kerdesek[KerdesSzama].Esemeny;
  try
   txtEvszam.Clear;
   txtEvszam.SetFocus;
  except
  end;
 end;
end;

procedure TfrmTeszt.UjTeszt;
var temp: string;
begin
 repeat
  temp := InputBox('Törigyakorló','Hány kérdés legyen?','');
  if temp = '' then Exit;
  try
   HanyKerdesLegyen := StrToInt(temp);
  except
   on E: Exception do
    Application.MessageBox('Ez nem egész szám!','Törigyakorló',mb_Ok + mb_IconExclamation);
  end;
 until HanyKerdesLegyen <= LetezoKerdesekSzama;
 Pontok := 0;
 MarVoltKerdesek := '';
 HanyKerdesVolt := 0;
end;

procedure TfrmTeszt.FormCreate(Sender: TObject);
var i: integer;
begin
 Mode := 1;
 if not FileExists(ExtractFilePath(Application.ExeName) + 'Events.dat') then
 begin
  Application.MessageBox('Nem találom az ''Events.dat'' filet!','Törigyakorló',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;
 DBFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'Events.dat');
 KerdesekSzama := DBFile.ReadInteger('General','NumberOfEvents',0);
 if KerdesekSzama = 0 then
 begin
  Application.MessageBox('Hibás/sérült ''Events.dat'' file!','Törigyakorló',mb_Ok + mb_IconHand);
  Application.Terminate;
 end;

 LetezoKerdesekSzama := 0;
 for i := 1 to KerdesekSzama do
 begin
  if DBFile.SectionExists('Event' + IntToStr(i)) then
  begin
   Inc(LetezoKerdesekSzama);
   Kerdesek[i].Letezik := true;
   Kerdesek[i].Esemeny := DBFile.ReadString('Event' + IntToStr(i),'Esemeny','');
   Kerdesek[i].Evszam := DBFile.ReadString('Event' + IntToStr(i),'Evszam','');
  end
  else
  begin
   Kerdesek[i].Letezik := false;
   Kerdesek[i].Esemeny := '';
   Kerdesek[i].Evszam := '';
  end;
 end;
 DBFile.Free;
 Randomize;

 if LetezoKerdesekSzama = 0 then
 begin
  Application.MessageBox('A file nem tartalmaz kérdéseket.','Törigyakorló',mb_Ok + mb_IconExclamation);
  Application.Terminate;
 end;
end;

procedure TfrmTeszt.FormShow(Sender: TObject);
begin
 Mode := frmKerdes.RadioGroup1.ItemIndex + 1;
 UjKerdes;
end;

procedure TfrmTeszt.Button1Click(Sender: TObject);
var EljarasMutato: TNotifyEvent;
    txtEsemenyText, KerdesEsemeny, txtEvszamText, KerdesEvszam: string;
begin
 txtEsemenyText := txtEsemeny.Text;
 while Pos(' ',txtEsemenyText) <> 0 do
  Delete(txtEsemenyText,Pos(' ',txtEsemenyText),1);

 txtEvszamText := txtEvszam.Text;
 while Pos(' ',txtEsemenyText) <> 0 do
  Delete(txtEvszamText,Pos(' ',txtEvszamText),1);

 KerdesEsemeny := Kerdesek[KerdesSzama].Esemeny;
 while Pos(' ',KerdesEsemeny) <> 0 do
  Delete(KerdesEsemeny,Pos(' ',KerdesEsemeny),1);

 KerdesEvszam := Kerdesek[KerdesSzama].Evszam;
 while Pos(' ',KerdesEvszam) <> 0 do
  Delete(KerdesEvszam,Pos(' ',KerdesEvszam),1);

 txtEsemenyText := LowerCase(txtEsemenyText);
 txtEvszamText := LowerCase(txtEvszamText);
 KerdesEsemeny := LowerCase(KerdesEsemeny);
 KerdesEvszam := lowerCase(KerdesEvszam);

 if Mode = 1 then
 begin
  if txtEsemenyText = KerdesEsemeny then
  begin
   Button1.Caption := 'Helyes!';
   EljarasMutato := Button1.OnClick;
   Button1.OnClick := nil;
   Delay(2,500);
   Button1.OnClick := EljarasMutato;
   Pontok := Pontok + 1;
   HanyKerdesVolt := HanyKerdesVolt + 1;
   if HanyKerdesVolt = HanyKerdesLegyen then
   begin
    frmEredmeny.lblPont.Caption := IntToStr(Pontok);
    frmEredmeny.lblOsszes.Caption := IntToStr(HanyKerdesLegyen);
    frmEredmeny.SzazalekEsJegyKalkulacio;
    frmTeszt.Hide;
    frmEredmeny.Show;
   end
   else
    UjKerdes;
  end
  else
  begin
   Button1.Caption := 'Nem jó! Helyesen: ' + Kerdesek[KerdesSzama].Esemeny;
   EljarasMutato := Button1.OnClick;
   Button1.OnClick := nil;
   Delay(2,500);
   Button1.OnClick := EljarasMutato;
   HanyKerdesVolt := HanyKerdesVolt + 1;
   if HanyKerdesVolt = HanyKerdesLegyen then
   begin
    frmEredmeny.lblPont.Caption := IntToStr(Pontok);
    frmEredmeny.lblOsszes.Caption := IntToStr(HanyKerdesLegyen);
    frmEredmeny.SzazalekEsJegyKalkulacio;
    frmTeszt.Hide;
    frmEredmeny.Show;
   end
   else
    UjKerdes;
  end;
 end
 else
 begin
  if txtEvszamText = KerdesEvszam then
  begin
   Button1.Caption := 'Helyes!';
   EljarasMutato := Button1.OnClick;
   Button1.OnClick := nil;
   Delay(2,500);
   Button1.OnClick := EljarasMutato;
   HanyKerdesVolt := HanyKerdesVolt + 1;
   Pontok := Pontok + 1;
   if HanyKerdesVolt = HanyKerdesLegyen then
   begin
    frmEredmeny.lblPont.Caption := IntToStr(Pontok);
    frmEredmeny.lblOsszes.Caption := IntToStr(HanyKerdesLegyen);
    frmEredmeny.SzazalekEsJegyKalkulacio;
    frmTeszt.Hide;
    frmEredmeny.Show;
   end
   else
    UjKerdes;
  end
  else
  begin
   Button1.Caption := 'Nem jó! Helyesen: ' + Kerdesek[KerdesSzama].Evszam;
   EljarasMutato := Button1.OnClick;
   Button1.OnClick := nil;
   Delay(2,500);
   Button1.OnClick := EljarasMutato;
   HanyKerdesVolt := HanyKerdesVolt + 1;
   if HanyKerdesVolt = HanyKerdesLegyen then
   begin
    frmEredmeny.lblPont.Caption := IntToStr(Pontok);
    frmEredmeny.lblOsszes.Caption := IntToStr(HanyKerdesLegyen);
    frmEredmeny.SzazalekEsJegyKalkulacio;
    frmTeszt.Hide;
    frmEredmeny.Show;
   end
   else
    UjKerdes;
  end;
 end;
end;

end.
